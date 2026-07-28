-- JDTLS recovery after macOS sleep.
--
-- • setup — register sleep-detection autocmds and attach recovery probing
--
-- Detection: wall-clock gap between BufEnter/FocusGained events. macOS sleep
-- does not fire VimResume; FocusGained is unreliable in tmux. CursorMoved /
-- InsertEnter normally only update the tick, but they also trigger a health
-- check after a clear suspend/long-idle gap so a wake-up in the same buffer is
-- not missed.
--
-- Recovery action on gap:
--   1. workspace probe (`java.project.getAll`) — if it times out, the jdtls
--      process / message loop is dead → restart.
--   2. workspace alive → probe raw `textDocument/completion` on the current
--      Java buffer, but only when that buffer already has an attached jdtls
--      client, so the server has received didOpen for the URI.
--   3. raw completion healthy → probe Blink's configured LSP provider and
--      request queue on the exact same real Java buffer and cursor context.
--   4. Blink failure → reset and re-probe Blink only. Empty raw completion is
--      inconclusive and never restarts jdtls.
--   5. workspace/raw completion error or timeout → restart jdtls. A hard
--      restart is used after a very long gap only when one of those server-side
--      health checks actually fails.
--
-- Why completion probing is guarded: sending completion to a URI that jdtls
-- has not yet received didOpen for can crash the jdtls message loop. The
-- automatic probe therefore only uses the current buffer after Neovim reports
-- an attached jdtls client for it. If focus returns to a non-Java buffer, the
-- gap is remembered and checked on the next Java BufEnter.
--
-- On restart: we call attach_fn for EVERY loaded Java buffer (not just one per
-- root). jdtls.start_or_attach deduplicates by root_dir, so the server is
-- only started once. `jdt://` library buffers are handled separately: they
-- cannot start a project client because they are virtual URIs, so after the
-- project clients are recreated we attach those buffers directly to the
-- matching new client.
--
-- Recovery actions are logged via utils.logging-util (file:
-- $stdpath('log')/jdtls-recovery.log) so intermittent failures can be
-- diagnosed after the fact.

local M = {}

local logger = require("utils.logging-util").new({
    name = "jdtls-recovery",
    filename = "jdtls-recovery.log",
})
local lsp_util = require("utils.lsp-util")

local SLEEP_THRESHOLD_MS = 10 * 60 * 1000
-- Above this gap, non-focus activity should also trigger recovery; otherwise
-- a wake-up in the same Java buffer can be missed until focus changes.
local LONG_SLEEP_MS = 60 * 60 * 1000
-- Above this gap, a required restart becomes hard: jdtls can come back with
-- completion or annotation-processing state still broken until its workspace
-- cache is rebuilt.
local HARD_RESTART_GAP_MS = 8 * 60 * 60 * 1000
local PROBE_TIMEOUT_MS = 3000
local BUF_PROBE_TIMEOUT_MS = 2000
local COMPLETION_PROBE_TIMEOUT_MS = 5000
local COMPLETION_EMPTY_RETRY_DELAY_MS = 250
local BLINK_REPROBE_DELAY_MS = 100
local ACTION_COOLDOWN_MS = 30 * 1000
local BUF_SOFT_COOLDOWN_MS = 10 * 1000
local RESTART_SHUTDOWN_TIMEOUT_MS = 10 * 1000
local RESTART_ATTACH_GRACE_MS = 1000
local RESTART_ATTACH_POLL_MS = 200
local JDT_URI_REATTACH_TIMEOUT_MS = 15 * 1000
local JDT_URI_REATTACH_POLL_MS = 500
local JDT_URI_PREFIX = "jdt://"
local JDT_URI_WITH_LEADING_SLASH_PREFIX = "/jdt://"

local state = {
    -- Wall-clock timestamp for sleep/idle detection. Do not use vim.uv.now()
    -- here: that is an event-loop/monotonic clock, not wall time.
    last_tick = nil,
    -- Monotonic timestamp used only to detect wall-vs-monotonic drift after
    -- real system suspend on platforms where monotonic time pauses.
    last_mono_tick = nil,
    attach_fn = nil,
    recovering = false,
    probing = false,
    -- Monotonically increasing operation token. Every asynchronous callback
    -- must still own this token before it mutates recovery state.
    operation_id = 0,
    -- Blink's normal context ids are small increasing integers. Negative ids
    -- keep diagnostic requests isolated from live completion contexts.
    next_blink_probe_context_id = -1,
    last_action_at = 0,
    -- per-buffer cooldown timestamps for soft recovery
    buf_last_soft = {},
    -- last real Java buffer entered by the user; used by manual probes when
    -- notifications or popups temporarily own the current window.
    last_java_buf = nil,
    -- gap health check deferred until the user enters a Java buffer
    pending_gap = nil,
    -- context captured by :JdtlsStop so :JdtlsStart can reattach the same
    -- project and virtual buffers later.
    stopped_ctx = nil,
    -- User explicitly stopped JDTLS; idle/focus recovery must not auto-start it
    -- until an explicit start/restart/toggle-on clears this flag.
    manual_stopped = false,
}

local function in_cooldown()
    return (vim.uv.now() - state.last_action_at) < ACTION_COOLDOWN_MS
end

local summarize_completion_result

local function mark_action()
    state.last_action_at = vim.uv.now()
end

--- Start a new asynchronous operation and invalidate callbacks from older work.
---@param clear_pending? boolean
---@return integer
local function begin_operation(clear_pending)
    state.operation_id = state.operation_id + 1
    state.probing = false
    if clear_pending then
        state.pending_gap = nil
    end
    return state.operation_id
end

--- Return whether an asynchronous callback still owns the active operation.
---@param operation_id integer
---@return boolean
local function operation_is_current(operation_id)
    return state.operation_id == operation_id
end

--- Return current wall-clock time in milliseconds.
local function wall_now_ms()
    return os.time() * 1000
end

--- Reset wall-clock and monotonic sleep-detection ticks to now.
local function set_tick()
    state.last_tick = wall_now_ms()
    state.last_mono_tick = vim.uv.now()
end

--- Finish the active lifecycle operation.
---@param operation_id integer
---@return boolean
local function finish_operation(operation_id)
    if not operation_is_current(operation_id) then
        return false
    end

    set_tick()
    state.recovering = false
    state.probing = false
    return true
end

--- Update sleep-detection ticks and return wall-clock/monotonic gaps.
local function update_tick_and_get_gaps()
    local now = wall_now_ms()
    local mono_now = vim.uv.now()
    local prev = state.last_tick
    local prev_mono = state.last_mono_tick

    state.last_tick = now
    state.last_mono_tick = mono_now

    if not prev then
        return nil, nil
    end

    return now - prev, prev_mono and (mono_now - prev_mono) or nil
end

--- Return whether a buffer name is a JDTLS virtual URI.
---@param name string
---@return boolean
local function is_jdt_uri_name(name)
    return vim.startswith(name, JDT_URI_PREFIX) or vim.startswith(name, JDT_URI_WITH_LEADING_SLASH_PREFIX)
end

--- Normalize a JDTLS virtual buffer name to a `jdt://` URI.
---@param name string
---@return string|nil
local function normalize_jdt_uri_name(name)
    if vim.startswith(name, JDT_URI_PREFIX) then
        return name
    end
    if vim.startswith(name, JDT_URI_WITH_LEADING_SLASH_PREFIX) then
        return name:sub(2)
    end
    return nil
end

local function java_buffers()
    local bufs = {}
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
        if vim.api.nvim_buf_is_loaded(buf) and vim.bo[buf].filetype == "java" then
            table.insert(bufs, buf)
        end
    end
    return bufs
end

--- Return whether a loaded buffer is a real Java project file.
---@param buf integer
---@return boolean
local function is_real_java_buffer(buf)
    return vim.api.nvim_buf_is_loaded(buf)
        and vim.bo[buf].filetype == "java"
        and not is_jdt_uri_name(vim.api.nvim_buf_get_name(buf))
end

--- Return all loaded real Java project buffers.
---@return integer[]
local function real_java_buffers()
    local bufs = {}
    for _, buf in ipairs(java_buffers()) do
        if is_real_java_buffer(buf) then
            table.insert(bufs, buf)
        end
    end
    return bufs
end

--- Return a Java buffer suitable for manual diagnostics.
local function manual_java_probe_buffer()
    local cur = vim.api.nvim_get_current_buf()
    if is_real_java_buffer(cur) and lsp_util.get_client_by_name("jdtls", { bufnr = cur }) then
        return cur
    end

    local last = state.last_java_buf
    if last and is_real_java_buffer(last) and lsp_util.get_client_by_name("jdtls", { bufnr = last }) then
        return last
    end

    for _, buf in ipairs(real_java_buffers()) do
        if lsp_util.get_client_by_name("jdtls", { bufnr = buf }) then
            return buf
        end
    end

    return nil
end

--- Extract the JDTLS project segment embedded in a `jdt://` URI.
---@param uri string|nil
---@return string|nil
local function jdt_uri_project_name(uri)
    return uri and uri:match("%?=([^/]+)") or nil
end

--- Return whether a root directory plausibly owns a JDTLS virtual URI project.
---@param root_dir string|nil
---@param project_name string|nil
---@return boolean
local function root_matches_jdt_project(root_dir, project_name)
    if not root_dir or not project_name then
        return false
    end

    local root_name = vim.fs.basename(root_dir)
    return root_name == project_name
        or (#root_name > #project_name and root_name:sub(-#project_name) == project_name)
        or root_name:find(project_name, 1, true) ~= nil
end

--- Return root dirs for JDTLS clients attached to a buffer.
---@param buf integer
---@return table<string, boolean>
local function jdtls_roots_for_buffer(buf)
    local roots = {}
    for _, client in ipairs(lsp_util.get_clients_by_name("jdtls", { bufnr = buf })) do
        local root_dir = client.config and client.config.root_dir
        if root_dir then
            roots[root_dir] = true
        end
    end
    return roots
end

--- Return JDTLS client configs attached to a buffer, keyed by root dir.
---@param buf integer
---@return table<string, vim.lsp.ClientConfig>
local function jdtls_configs_for_buffer(buf)
    local configs = {}
    for _, client in ipairs(lsp_util.get_clients_by_name("jdtls", { bufnr = buf })) do
        local root_dir = client.config and client.config.root_dir
        if root_dir then
            configs[root_dir] = client.config
        end
    end
    return configs
end

--- Return active JDTLS client configs keyed by root dir.
---@return table<string, vim.lsp.ClientConfig>
local function jdtls_configs_by_root()
    local configs = {}
    for _, client in ipairs(lsp_util.get_clients_by_name("jdtls")) do
        local root_dir = client.config and client.config.root_dir
        if root_dir then
            configs[root_dir] = client.config
        end
    end
    return configs
end

--- Build restart metadata for a loaded JDTLS virtual buffer.
---@param buf integer
---@param name? string
---@return table|nil
local function jdt_uri_target_from_buffer(buf, name)
    name = name or vim.api.nvim_buf_get_name(buf)
    if not is_jdt_uri_name(name) then
        return nil
    end

    local uri = normalize_jdt_uri_name(name)
    return {
        buf = buf,
        name = name,
        uri = uri,
        project_name = jdt_uri_project_name(uri),
        roots = jdtls_roots_for_buffer(buf),
        configs = jdtls_configs_for_buffer(buf),
        filetype = vim.bo[buf].filetype,
    }
end

--- Collect real Java buffers and JDTLS virtual buffers for a restart.
---@return {project_bufs: integer[], jdt_uri_bufs: table[], client_configs: table<string, vim.lsp.ClientConfig>}
local function collect_restart_context()
    local ctx = {
        project_bufs = {},
        jdt_uri_bufs = {},
        client_configs = jdtls_configs_by_root(),
    }

    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
        if vim.api.nvim_buf_is_loaded(buf) then
            local name = vim.api.nvim_buf_get_name(buf)
            if is_jdt_uri_name(name) then
                table.insert(ctx.jdt_uri_bufs, jdt_uri_target_from_buffer(buf, name))
            elseif vim.bo[buf].filetype == "java" then
                table.insert(ctx.project_bufs, buf)
            end
        end
    end

    return ctx
end

--- Merge saved and currently loaded restart contexts without duplicating buffers.
---@param first table
---@param second table
---@return table
local function merge_restart_contexts(first, second)
    local merged = {
        project_bufs = {},
        jdt_uri_bufs = {},
        client_configs = {},
    }
    local project_seen = {}
    local jdt_uri_seen = {}

    for _, ctx in ipairs({ first, second }) do
        for _, buf in ipairs(ctx.project_bufs or {}) do
            if not project_seen[buf] then
                project_seen[buf] = true
                table.insert(merged.project_bufs, buf)
            end
        end
        for _, target in ipairs(ctx.jdt_uri_bufs or {}) do
            if target and not jdt_uri_seen[target.buf] then
                jdt_uri_seen[target.buf] = true
                table.insert(merged.jdt_uri_bufs, target)
            end
        end
        for root_dir, config in pairs(ctx.client_configs or {}) do
            merged.client_configs[root_dir] = config
        end
    end

    return merged
end

--- Clear diagnostics published by the supplied JDTLS client namespaces.
---
--- Neovim keys each LSP client's diagnostics by client id
--- (`nvim.lsp.jdtls.<id>` for push, `nvim.lsp.jdtls.<id>.<pull_id>` for pull).
--- nvim-jdtls' `:JdtRestart` stops the old client and starts a new one with a
--- fresh id, so the old namespace lingers with stale diagnostics and each
--- restart stacks another duplicate. We reset the active clients' namespaces
--- and sweep every orphaned jdtls namespace so accumulated duplicates are
--- purged too.
---@param clients vim.lsp.Client[]
local function clear_jdtls_diagnostics(clients)
    local cleared = {}

    local function reset(namespace)
        if namespace and not cleared[namespace] then
            pcall(vim.diagnostic.reset, namespace)
            cleared[namespace] = true
        end
    end

    for _, client in ipairs(clients) do
        local ok, namespace = pcall(vim.lsp.diagnostic.get_namespace, client.id)
        if ok then
            reset(namespace)
        end
    end

    -- Sweep orphaned namespaces from previously-stopped clients. The pattern is
    -- unanchored at the tail so it also catches pull-diagnostic namespaces
    -- (`nvim.lsp.jdtls.<id>.<pull_id>`) should jdtls ever advertise them.
    for name, namespace in pairs(vim.api.nvim_get_namespaces()) do
        if name:match("^nvim%.lsp%.jdtls%.%d+") then
            reset(namespace)
        end
    end
end

--- Request graceful stop for every active JDTLS client.
local function request_stop_jdtls_clients()
    local clients = lsp_util.get_clients_by_name("jdtls")
    clear_jdtls_diagnostics(clients)
    for _, client in ipairs(clients) do
        local ok, err = pcall(function()
            if client.stop then
                client:stop(RESTART_SHUTDOWN_TIMEOUT_MS)
            else
                vim.lsp.stop_client(client.id, RESTART_SHUTDOWN_TIMEOUT_MS)
            end
        end)
        if not ok then
            logger.fmt_warn("failed to stop client %s: %s", tostring(client.id), tostring(err))
        end
    end
    return clients
end

--- Check whether all supplied clients are already closing their RPC streams.
local function clients_are_closing(clients)
    for _, client in ipairs(clients) do
        if client.rpc and not client.rpc.is_closing() then
            return false
        end
    end
    return true
end

--- Poll until supplied clients close or the restart timeout is reached.
---@param clients vim.lsp.Client[]
---@param operation_id integer
---@param done fun(closed: boolean)
local function wait_for_clients_to_close(clients, operation_id, done)
    local deadline = vim.uv.now() + RESTART_SHUTDOWN_TIMEOUT_MS + RESTART_ATTACH_GRACE_MS

    local function poll()
        if not operation_is_current(operation_id) then
            return
        end
        if clients_are_closing(clients) then
            done(true)
            return
        end
        if vim.uv.now() >= deadline then
            logger.fmt_warn("timed out waiting for %d jdtls client(s) to close; reattaching anyway", #clients)
            done(false)
            return
        end
        vim.defer_fn(poll, RESTART_ATTACH_POLL_MS)
    end

    poll()
end

--- Clear blink.cmp completion state and force its LSP provider to be recreated.
local function reset_blink_lsp_state(reason)
    local trigger_ok, trigger = pcall(require, "blink.cmp.completion.trigger")
    if trigger_ok then
        local hide_ok, hide_err = pcall(function()
            trigger.hide()
            trigger.context = nil
            if trigger.buffer_events then
                trigger.buffer_events.last_char = ""
                trigger.buffer_events.ignore_next_text_changed = false
                trigger.buffer_events.ignore_next_cursor_moved = false
            end
        end)
        if not hide_ok then
            logger.fmt_warn("blink trigger reset failed (%s): %s", reason, tostring(hide_err))
        end
    end

    local list_ok, list = pcall(require, "blink.cmp.completion.list")
    if list_ok then
        local hide_ok, hide_err = pcall(function()
            list.hide()
            list.context = nil
            list.items = {}
            list.selected_item_idx = nil
            list.preview_undo = nil
        end)
        if not hide_ok then
            logger.fmt_warn("blink list reset failed (%s): %s", reason, tostring(hide_err))
        end
    end

    local ok_sources, sources = pcall(require, "blink.cmp.sources.lib")
    if not ok_sources then
        return
    end

    local cancel_ok, cancel_err = pcall(function()
        sources.cancel_completions()
    end)
    if not cancel_ok then
        logger.fmt_warn("blink completion queue reset failed (%s): %s", reason, tostring(cancel_err))
    end

    local provider_ok, provider = pcall(function()
        return sources.providers and sources.providers.lsp or sources.get_provider_by_id("lsp")
    end)
    if provider_ok and provider then
        if provider.list then
            local list_ok, list_err = pcall(function()
                provider.list:destroy()
            end)
            if not list_ok then
                logger.fmt_warn("blink lsp list reset failed (%s): %s", reason, tostring(list_err))
            end
            provider.list = nil
        end
        provider.resolve_cache_context_id = nil
        provider.resolve_cache = {}
    end
    if sources.providers then
        sources.providers.lsp = nil
    end

    local ok_cache, cache = pcall(require, "blink.cmp.sources.lsp.cache")
    if ok_cache and type(cache) == "table" then
        cache.entries = {}
    end
end

--- Refresh blink.cmp after JDTLS clients have been recreated.
local function refresh_blink_lsp(reason)
    reset_blink_lsp_state(reason)

    local ok, cmp = pcall(require, "blink.cmp")
    if not ok then
        return
    end

    local reload_ok, reload_err = pcall(function()
        cmp.reload("lsp")
    end)
    if not reload_ok then
        logger.fmt_warn("blink lsp reload failed (%s): %s", reason, tostring(reload_err))
    end

    local subscribe_ok, subscribe_err = pcall(function()
        cmp.resubscribe()
    end)
    if not subscribe_ok then
        logger.fmt_warn("blink resubscribe failed (%s): %s", reason, tostring(subscribe_err))
    end
end

--- Refresh blink.cmp's LSP source after external JDTLS workspace events.
function M.refresh_blink_lsp(reason)
    refresh_blink_lsp(reason or "external")
end

--- Return whether the user explicitly stopped JDTLS.
---@return boolean
function M.is_manually_stopped()
    return state.manual_stopped
end

--- Probe Blink's configured LSP provider and request queue on one Java buffer.
---@param buf integer
---@param done fun(result: table)
local function probe_blink_lsp_completion(buf, done)
    if not is_real_java_buffer(buf) then
        done({ status = "skipped", reason = "no real Java buffer found" })
        return
    end
    local jdtls_client = lsp_util.get_client_by_name("jdtls", { bufnr = buf })
    if not jdtls_client then
        done({ status = "skipped", reason = "no jdtls client on probe buffer" })
        return
    end

    local context_ok, context_mod = pcall(require, "blink.cmp.completion.trigger.context")
    if not context_ok then
        done({ status = "error", error = context_mod })
        return
    end

    local sources_ok, sources = pcall(require, "blink.cmp.sources.lib")
    if not sources_ok then
        done({ status = "error", error = sources })
        return
    end

    local context_id = state.next_blink_probe_context_id
    state.next_blink_probe_context_id = state.next_blink_probe_context_id - 1

    local context
    local setup_ok, setup_err = pcall(function()
        vim.api.nvim_buf_call(buf, function()
            local provider = sources.get_provider_by_id("lsp")
            if not provider:enabled() then
                error("configured lsp provider is disabled")
            end

            context = context_mod.new({
                id = context_id,
                providers = { "lsp" },
                initial_trigger_kind = "manual",
                trigger_kind = "manual",
            })
        end)
    end)
    if not setup_ok then
        done({ status = "error", error = setup_err })
        return
    end

    local responded = false
    local start = vim.uv.now()
    local listener

    local function cleanup()
        if listener then
            sources.completions_emitter:off(listener)
        end
        if sources.completions_queue and sources.completions_queue.id == context_id then
            sources.cancel_completions()
        end
    end

    local function finish(result)
        if responded then
            return
        end
        responded = true
        cleanup()
        done(result)
    end

    listener = function(event)
        if not event.context or event.context.id ~= context_id then
            return
        end

        local elapsed = vim.uv.now() - start
        local lsp_items = event.items and event.items.lsp or {}
        local jdtls_items = vim.tbl_filter(function(item)
            return item.client_id == jdtls_client.id
        end, lsp_items)
        local count, preview = summarize_completion_result(jdtls_items)
        finish({
            status = count > 0 and "ok" or "empty",
            elapsed = elapsed,
            count = count,
            preview = preview,
        })
    end
    sources.completions_emitter:on(listener)

    local request_ok, request_err = pcall(function()
        sources.cancel_completions()
        vim.api.nvim_buf_call(buf, function()
            sources.request_completions(context)
        end)
    end)
    if not request_ok then
        finish({ status = "error", error = request_err })
        return
    end

    vim.defer_fn(function()
        if responded then
            return
        end
        if sources.completions_queue and sources.completions_queue.id ~= context_id then
            finish({ status = "skipped", reason = "superseded by live Blink completion" })
            return
        end
        finish({ status = "timeout", elapsed = COMPLETION_PROBE_TIMEOUT_MS })
    end, COMPLETION_PROBE_TIMEOUT_MS)
end

--- Return whether a JDTLS client is the new owner for a virtual URI buffer.
---@param client vim.lsp.Client
---@param target table
---@return boolean
local function client_matches_jdt_uri_buffer(client, target)
    local root_dir = client.config and client.config.root_dir
    if not root_dir then
        return false
    end

    if target.roots and target.roots[root_dir] then
        return true
    end

    return root_matches_jdt_project(root_dir, target.project_name)
end

--- Find the new JDTLS client that should own a virtual URI buffer.
---@param target table
---@return vim.lsp.Client|nil
local function find_jdt_uri_client(target)
    local clients = lsp_util.get_clients_by_name("jdtls")
    for _, client in ipairs(clients) do
        if client_matches_jdt_uri_buffer(client, target) then
            return client
        end
    end

    if #clients == 1 then
        return clients[1]
    end

    return nil
end

--- Normalize `/jdt://...` buffers back to `jdt://...` when possible.
---@param target table
---@return boolean
local function normalize_jdt_uri_buffer_name(target)
    if target.name == target.uri then
        return true
    end
    if not target.uri then
        return false
    end

    local existing = vim.fn.bufnr(target.uri)
    if existing ~= -1 and existing ~= target.buf then
        logger.fmt_warn(
            "cannot normalize jdt uri buffer %d (%s): target name already used by buf %d",
            target.buf,
            target.name,
            existing
        )
        return false
    end

    local ok, err = pcall(vim.api.nvim_buf_set_name, target.buf, target.uri)
    if ok then
        logger.fmt_info("normalized jdt uri buffer %d name from %s to %s", target.buf, target.name, target.uri)
        target.name = target.uri
        return true
    end

    logger.fmt_warn("failed to normalize jdt uri buffer %d (%s): %s", target.buf, target.name, tostring(err))
    return false
end

--- Find a saved JDTLS client config that can own a virtual URI buffer.
---@param target table
---@param ctx table
---@return vim.lsp.ClientConfig|nil
local function find_jdt_uri_config(target, ctx)
    for root_dir, config in pairs(target.configs or {}) do
        if target.roots and target.roots[root_dir] then
            return config
        end
    end

    for root_dir, config in pairs(ctx.client_configs or {}) do
        if target.roots and target.roots[root_dir] then
            return config
        end
    end

    for root_dir, config in pairs(target.configs or {}) do
        if root_matches_jdt_project(root_dir, target.project_name) then
            return config
        end
    end

    for root_dir, config in pairs(ctx.client_configs or {}) do
        if root_matches_jdt_project(root_dir, target.project_name) then
            return config
        end
    end

    if vim.tbl_count(ctx.client_configs or {}) == 1 then
        return select(2, next(ctx.client_configs))
    end

    return nil
end

--- Start a replacement JDTLS client from the old config for a virtual URI buffer.
---@param target table
---@param ctx table
---@param reason string
---@return vim.lsp.Client|nil
local function start_jdt_uri_client_from_config(target, ctx, reason)
    local config = find_jdt_uri_config(target, ctx)
    if not config then
        return nil
    end

    local ok, client_id = pcall(function()
        return vim.lsp.start(config, {
            bufnr = target.buf,
            reuse_client = function(client, candidate)
                return client.name == "jdtls" and client.config and client.config.root_dir == candidate.root_dir
            end,
        })
    end)
    if ok and client_id then
        logger.fmt_info(
            "started jdtls client %d from saved config for jdt uri buffer %d (%s)",
            client_id,
            target.buf,
            reason
        )
        return vim.lsp.get_client_by_id(client_id)
    end

    logger.fmt_warn(
        "failed to start jdtls from saved config for jdt uri buffer %d (%s): %s",
        target.buf,
        reason,
        tostring(client_id)
    )
    return nil
end

--- Attach one virtual URI buffer to a JDTLS client.
---@param target table
---@param client vim.lsp.Client
---@param reason string
---@return boolean
local function attach_jdt_uri_target_to_client(target, client, reason)
    if vim.lsp.buf_is_attached(target.buf, client.id) then
        return true
    end

    local ok, attached_ok = pcall(vim.lsp.buf_attach_client, target.buf, client.id)
    if ok and attached_ok then
        logger.fmt_info("attached jdt uri buffer %d to client %d (%s)", target.buf, client.id, reason)
        return true
    end

    local err = ok and "vim.lsp.buf_attach_client returned false" or attached_ok
    logger.fmt_warn("failed to attach jdt uri buffer %d (%s): %s", target.buf, reason, tostring(err))
    return false
end

--- Try to attach one virtual URI buffer without deleting it on failure.
---@param target table
---@param ctx table
---@param reason string
---@param allow_start boolean
---@return boolean attached
---@return string? failure_reason
local function try_reattach_jdt_uri_buffer(target, ctx, reason, allow_start)
    if not vim.api.nvim_buf_is_loaded(target.buf) then
        return true, "unloaded"
    end
    if not normalize_jdt_uri_buffer_name(target) then
        return false, "could not normalize jdt uri buffer name"
    end

    local client = find_jdt_uri_client(target)
    if not client and allow_start then
        client = start_jdt_uri_client_from_config(target, ctx, reason)
    end
    if not client then
        return false, "no matching jdtls client"
    end

    if attach_jdt_uri_target_to_client(target, client, reason) then
        return true, nil
    end

    return false, "attach failed"
end

--- Retry attaching existing `jdt://` buffers to recreated JDTLS clients.
---@param ctx table
---@param reason string
---@param operation_id integer
---@param done fun(attached: integer, kept: integer)
local function reattach_jdt_uri_buffers(ctx, reason, operation_id, done)
    local targets = ctx.jdt_uri_bufs or {}
    if #targets == 0 then
        done(0, 0)
        return
    end

    local pending = {}
    for _, target in ipairs(targets) do
        table.insert(pending, target)
    end
    local attached = 0
    local deadline = vim.uv.now() + JDT_URI_REATTACH_TIMEOUT_MS

    local function poll()
        if not operation_is_current(operation_id) then
            return
        end

        local allow_start = vim.uv.now() >= deadline
        local remaining = {}
        local last_reason = {}

        for _, target in ipairs(pending) do
            local ok, failure_reason = try_reattach_jdt_uri_buffer(target, ctx, reason, allow_start)
            if ok then
                if failure_reason ~= "unloaded" then
                    attached = attached + 1
                end
            else
                table.insert(remaining, target)
                last_reason[target.buf] = failure_reason
            end
        end

        pending = remaining
        if #pending == 0 then
            logger.fmt_info("jdt uri buffers after %s: attached=%d kept=0 total=%d", reason, attached, #targets)
            done(attached, 0)
            return
        end

        if not allow_start then
            vim.defer_fn(poll, JDT_URI_REATTACH_POLL_MS)
            return
        end

        for _, target in ipairs(pending) do
            logger.fmt_warn(
                "kept jdt uri buffer %d (%s) unattached after %s: %s",
                target.buf,
                target.name,
                reason,
                last_reason[target.buf] or "unknown"
            )
        end
        logger.fmt_info("jdt uri buffers after %s: attached=%d kept=%d total=%d", reason, attached, #pending, #targets)
        done(attached, #pending)
    end

    poll()
end

--- Attach a single `jdt://` buffer to its current JDTLS client.
---@param buf integer
---@param reason string
---@return boolean
local function attach_jdt_uri_buffer(buf, reason)
    if not vim.api.nvim_buf_is_loaded(buf) then
        return false
    end

    local target = jdt_uri_target_from_buffer(buf)
    if not target then
        return false
    end

    return try_reattach_jdt_uri_buffer(target, { client_configs = jdtls_configs_by_root() }, reason, false)
end

--- Reattach JDTLS to loaded Java project buffers and finish the recovery cycle.
---@param ctx table
---@param reason string
---@param operation_id integer
local function reattach_java_buffers(ctx, reason, operation_id)
    if not operation_is_current(operation_id) then
        return
    end

    pcall(function()
        require("utils.java.jdtls-workspace-watcher").mark_recovery_refresh(reason)
    end)

    local attached = 0
    for _, buf in ipairs(ctx.project_bufs) do
        if vim.api.nvim_buf_is_loaded(buf) then
            local ok, err = pcall(function()
                vim.api.nvim_buf_call(buf, function()
                    state.attach_fn(buf)
                end)
            end)
            if ok then
                attached = attached + 1
            else
                logger.fmt_warn("failed to reattach Java buffer %d after %s: %s", buf, reason, tostring(err))
            end
        end
    end

    vim.defer_fn(function()
        if not operation_is_current(operation_id) then
            return
        end

        reattach_jdt_uri_buffers(ctx, reason, operation_id, function(jdt_attached, jdt_kept)
            if not operation_is_current(operation_id) then
                return
            end

            refresh_blink_lsp(reason)
            if not finish_operation(operation_id) then
                return
            end

            local message = string.format("JDTLS recovered (%s, %d project buffers)", reason, attached)
            if jdt_attached > 0 or jdt_kept > 0 then
                message = string.format(
                    "JDTLS recovered (%s, %d project buffers, %d jdt:// reattached, %d jdt:// kept)",
                    reason,
                    attached,
                    jdt_attached,
                    jdt_kept
                )
            end
            vim.notify(message, jdt_kept > 0 and vim.log.levels.WARN or vim.log.levels.INFO)
        end)
    end, RESTART_ATTACH_GRACE_MS)
end

--- Extract JDTLS cache project names from client root directories.
local function project_names_from_clients(clients)
    local names = {}
    for _, client in ipairs(clients) do
        if client.config and client.config.root_dir then
            names[vim.fs.basename(client.config.root_dir)] = true
        end
    end
    return names
end

--- Add JDTLS cache project names from client configs keyed by root directory.
---@param names table<string, boolean>
---@param configs table<string, vim.lsp.ClientConfig>|nil
local function add_project_names_from_configs(names, configs)
    for root_dir in pairs(configs or {}) do
        names[vim.fs.basename(root_dir)] = true
    end
end

--- Derive a JDTLS cache project name from a real Java buffer.
---@param buf integer
---@return string|nil
local function project_name_from_buffer(buf)
    if not is_real_java_buffer(buf) then
        return nil
    end

    local jdtls_config = vim.lsp.config and vim.lsp.config.jdtls or nil
    local markers = jdtls_config and jdtls_config.root_markers
        or { ".git", "mvnw", "gradlew", "pom.xml", "build.gradle", "build.gradle.kts" }
    local root_dir = vim.fs.root(vim.api.nvim_buf_get_name(buf), markers)
    return root_dir and vim.fs.basename(root_dir) or nil
end

--- Resolve only the cache project names owned by a restart context.
---@param ctx table
---@param clients vim.lsp.Client[]
---@return table<string, boolean>
local function project_names_from_restart_context(ctx, clients)
    local names = project_names_from_clients(clients)
    add_project_names_from_configs(names, ctx.client_configs)

    if next(names) == nil then
        for _, buf in ipairs(ctx.project_bufs or {}) do
            local project_name = project_name_from_buffer(buf)
            if project_name then
                names[project_name] = true
            end
        end
    end

    return names
end

--- Wipe JDTLS cache directories for the supplied project names.
local function wipe_jdtls_cache(project_names)
    local cache_dir = vim.fn.stdpath("cache") .. "/jdtls"
    local wiped = {}

    for project_name in pairs(project_names) do
        local path = cache_dir .. "/" .. project_name
        if vim.fn.isdirectory(path) == 1 then
            vim.fn.delete(path, "rf")
            table.insert(wiped, project_name)
        end
    end

    return wiped
end

--- Stop JDTLS, wipe project cache, and reattach project plus virtual Java buffers.
local function hard_restart_all_jdtls(reason)
    if state.recovering then
        return
    end
    local ctx = collect_restart_context()
    if #ctx.project_bufs == 0 then
        if #ctx.jdt_uri_bufs > 0 then
            logger.fmt_warn(
                "hard restart (%s): no real Java project buffers; keeping %d jdt uri buffers",
                reason,
                #ctx.jdt_uri_bufs
            )
            vim.notify(
                "JDTLS hard restart: no real Java project buffer to restart from; kept jdt:// buffers",
                vim.log.levels.WARN
            )
        end
        return
    end

    local clients = lsp_util.get_clients_by_name("jdtls")
    local project_names = project_names_from_restart_context(ctx, clients)
    if next(ctx.client_configs) == nil and state.stopped_ctx then
        ctx.client_configs = vim.deepcopy(state.stopped_ctx.client_configs or {})
    end
    state.stopped_ctx = nil

    local operation_id = begin_operation(true)
    state.manual_stopped = false
    state.recovering = true
    mark_action()

    logger.fmt_info(
        "hard restart (%s): %d project java buffers, %d jdt uri buffers",
        reason,
        #ctx.project_bufs,
        #ctx.jdt_uri_bufs
    )

    wait_for_clients_to_close(request_stop_jdtls_clients(), operation_id, function()
        if not operation_is_current(operation_id) then
            return
        end

        local wiped = wipe_jdtls_cache(project_names)
        if #wiped > 0 then
            logger.fmt_info("hard restart (%s): wiped cache for %s", reason, table.concat(wiped, ", "))
            vim.notify("JDTLS hard restart: wiped cache for " .. table.concat(wiped, ", "), vim.log.levels.INFO)
        elseif next(project_names) == nil then
            logger.fmt_warn("hard restart (%s): project could not be identified; skipped cache deletion", reason)
            vim.notify("JDTLS hard restart: project unknown, cache deletion skipped", vim.log.levels.WARN)
        else
            logger.fmt_warn("hard restart (%s): no cache directories found to wipe", reason)
        end
        reattach_java_buffers(ctx, "hard restart: " .. reason, operation_id)
    end)
end

--- Stop JDTLS and reattach project plus virtual Java buffers without wiping cache.
local function restart_all_jdtls(reason)
    if state.recovering then
        return
    end
    local ctx = collect_restart_context()
    if #ctx.project_bufs == 0 then
        if #ctx.jdt_uri_bufs > 0 then
            logger.fmt_warn(
                "full restart (%s): no real Java project buffers; keeping %d jdt uri buffers",
                reason,
                #ctx.jdt_uri_bufs
            )
            vim.notify(
                "JDTLS restart: no real Java project buffer to restart from; kept jdt:// buffers",
                vim.log.levels.WARN
            )
        end
        return
    end

    if next(ctx.client_configs) == nil and state.stopped_ctx then
        ctx.client_configs = vim.deepcopy(state.stopped_ctx.client_configs or {})
    end
    state.stopped_ctx = nil

    local operation_id = begin_operation(true)
    state.manual_stopped = false
    state.recovering = true
    mark_action()

    logger.fmt_info(
        "full restart (%s): %d project java buffers, %d jdt uri buffers",
        reason,
        #ctx.project_bufs,
        #ctx.jdt_uri_bufs
    )

    wait_for_clients_to_close(request_stop_jdtls_clients(), operation_id, function()
        if not operation_is_current(operation_id) then
            return
        end
        reattach_java_buffers(ctx, reason, operation_id)
    end)
end

--- Stop all active JDTLS clients while preserving restart context for later start.
---@param reason string
local function stop_all_jdtls(reason)
    if state.recovering then
        vim.notify("JDTLS stop: recovery is already in progress", vim.log.levels.WARN)
        return
    end

    local clients = lsp_util.get_clients_by_name("jdtls")
    if #clients == 0 then
        begin_operation(true)
        state.manual_stopped = true
        state.stopped_ctx = state.stopped_ctx or collect_restart_context()
        logger.fmt_info("stop (%s): already stopped; no active jdtls clients", reason)
        vim.notify("JDTLS is already stopped", vim.log.levels.INFO)
        return
    end

    local ctx = collect_restart_context()
    state.stopped_ctx = ctx
    state.manual_stopped = true

    local operation_id = begin_operation(true)
    state.recovering = true
    mark_action()

    logger.fmt_info(
        "stop (%s): %d clients, %d project java buffers, %d jdt uri buffers",
        reason,
        #clients,
        #ctx.project_bufs,
        #ctx.jdt_uri_bufs
    )

    wait_for_clients_to_close(request_stop_jdtls_clients(), operation_id, function()
        if not operation_is_current(operation_id) then
            return
        end

        refresh_blink_lsp("stop: " .. reason)
        if not finish_operation(operation_id) then
            return
        end
        vim.notify(
            string.format("JDTLS stopped (%s, %d project buffers saved)", reason, #ctx.project_bufs),
            vim.log.levels.INFO
        )
    end)
end

--- Start JDTLS from the last stopped context or current loaded Java buffers.
---@param reason string
local function start_all_jdtls(reason)
    if state.recovering then
        vim.notify("JDTLS start: recovery is already in progress", vim.log.levels.WARN)
        return
    end

    local clients = lsp_util.get_clients_by_name("jdtls")
    if #clients > 0 then
        state.stopped_ctx = nil
        state.manual_stopped = false
        logger.fmt_info("start (%s): already running with %d active jdtls client(s)", reason, #clients)
        vim.notify(
            string.format("JDTLS is already running (%d client%s)", #clients, #clients == 1 and "" or "s"),
            vim.log.levels.INFO
        )
        return
    end

    local using_stopped_ctx = state.stopped_ctx ~= nil
    local current_ctx = collect_restart_context()
    local ctx = using_stopped_ctx and merge_restart_contexts(state.stopped_ctx, current_ctx) or current_ctx

    if #ctx.project_bufs == 0 then
        if #ctx.jdt_uri_bufs == 0 then
            logger.fmt_warn("start (%s): no Java buffers found", reason)
            vim.notify("JDTLS start: no Java buffers found", vim.log.levels.WARN)
            return
        end

        local operation_id = begin_operation(true)
        state.manual_stopped = false
        state.recovering = true
        mark_action()
        logger.fmt_warn(
            "start (%s): no real Java project buffers; trying saved config for %d jdt uri buffers",
            reason,
            #ctx.jdt_uri_bufs
        )
        reattach_jdt_uri_buffers(ctx, "start: " .. reason, operation_id, function(jdt_attached, jdt_kept)
            if not operation_is_current(operation_id) then
                return
            end

            refresh_blink_lsp("start: " .. reason)
            if not finish_operation(operation_id) then
                return
            end
            if jdt_attached > 0 and jdt_kept == 0 then
                state.stopped_ctx = nil
            elseif not using_stopped_ctx then
                state.stopped_ctx = nil
            end
            vim.notify(
                string.format("JDTLS started from jdt:// context (%d reattached, %d kept)", jdt_attached, jdt_kept),
                jdt_kept > 0 and vim.log.levels.WARN or vim.log.levels.INFO
            )
        end)
        return
    end

    local operation_id = begin_operation(true)
    state.manual_stopped = false
    state.recovering = true
    mark_action()

    logger.fmt_info(
        "start (%s): %d project java buffers, %d jdt uri buffers",
        reason,
        #ctx.project_bufs,
        #ctx.jdt_uri_bufs
    )

    state.stopped_ctx = nil
    reattach_java_buffers(ctx, "start: " .. reason, operation_id)
end

--- Restart all loaded JDTLS workspaces through the diagnostic-cleaning path.
---@param reason? string
function M.restart(reason)
    if state.recovering then
        vim.notify("JDTLS restart: recovery is already in progress", vim.log.levels.WARN)
        return
    end
    restart_all_jdtls(reason or "manual")
end

--- Hard-restart all loaded JDTLS workspaces through the diagnostic-cleaning path.
---@param reason? string
function M.hard_restart(reason)
    if state.recovering then
        vim.notify("JDTLS hard restart: recovery is already in progress", vim.log.levels.WARN)
        return
    end
    hard_restart_all_jdtls(reason or "manual")
end

--- Stop all active JDTLS clients and remember the current restart context.
---@param reason? string
function M.stop(reason)
    if state.recovering then
        vim.notify("JDTLS stop: recovery is already in progress", vim.log.levels.WARN)
        return
    end
    stop_all_jdtls(reason or "manual")
end

--- Start JDTLS from the last stopped context or currently loaded Java buffers.
---@param reason? string
function M.start(reason)
    if state.recovering then
        vim.notify("JDTLS start: recovery is already in progress", vim.log.levels.WARN)
        return
    end
    start_all_jdtls(reason or "manual")
end

--- Toggle JDTLS by stopping active clients or starting from saved/current context.
---@param reason? string
function M.toggle(reason)
    if state.recovering then
        vim.notify("JDTLS toggle: recovery is already in progress", vim.log.levels.WARN)
        return
    end

    if #lsp_util.get_clients_by_name("jdtls") > 0 then
        stop_all_jdtls(reason or "toggle")
        return
    end

    start_all_jdtls(reason or "toggle")
end

local function probe_client(client, done)
    local responded = false
    local ok, req_id = client:request("workspace/executeCommand", {
        command = "java.project.getAll",
    }, function()
        if responded then
            return
        end
        responded = true
        done(true)
    end)

    if not ok then
        done(false)
        return
    end

    vim.defer_fn(function()
        if responded then
            return
        end
        responded = true
        pcall(function()
            client:cancel_request(req_id)
        end)
        done(false)
    end, PROBE_TIMEOUT_MS)
end

local function probe_all_clients(clients, done)
    local pending = #clients
    if pending == 0 then
        done(false)
        return
    end
    local any_dead = false
    for _, client in ipairs(clients) do
        probe_client(client, function(alive)
            if not alive then
                any_dead = true
            end
            pending = pending - 1
            if pending == 0 then
                done(any_dead)
            end
        end)
    end
end

-- Cheap buffer-scoped probe. foldingRange is light, supported by jdtls, and
-- (unlike completion) does not crash the message loop if the buffer is
-- desynced — it just fails or returns nothing.
local function probe_buffer(client, buf, done)
    if not vim.api.nvim_buf_is_loaded(buf) then
        done(true)
        return
    end
    local uri = vim.uri_from_bufnr(buf)
    local responded = false
    local ok, req_id = client:request("textDocument/foldingRange", {
        textDocument = { uri = uri },
    }, function(err)
        if responded then
            return
        end
        responded = true
        -- An explicit error response still proves the message loop and
        -- per-buffer routing are alive. Only timeout = dead.
        done(true, err)
    end, buf)

    if not ok then
        done(false)
        return
    end

    vim.defer_fn(function()
        if responded then
            return
        end
        responded = true
        pcall(function()
            client:cancel_request(req_id)
        end)
        done(false)
    end, BUF_PROBE_TIMEOUT_MS)
end

--- Extract item count and preview labels from a completion response.
function summarize_completion_result(result)
    local items = result
    if type(result) == "table" and result.items then
        items = result.items
    end

    local count = type(items) == "table" and #items or 0
    local preview = {}
    for i = 1, math.min(5, count) do
        table.insert(preview, items[i].label or "?")
    end

    return count, preview
end

--- Return a real Java buffer and its attached JDTLS client for safe completion probing.
---@param buf? integer
---@return integer|nil, vim.lsp.Client|nil, string|nil
local function completion_probe_target(buf)
    buf = buf or vim.api.nvim_get_current_buf()
    if not is_real_java_buffer(buf) then
        return nil, nil, "current buffer is not a real Java project buffer"
    end

    local client = lsp_util.get_client_by_name("jdtls", { bufnr = buf })
    if not client then
        return nil, nil, "no jdtls client on current buffer"
    end
    if client.initialized == false then
        return nil, nil, "jdtls client is not initialized"
    end

    return buf, client, nil
end

--- Send a raw textDocument/completion request to an attached Java buffer.
local function probe_completion(client, buf, done)
    if not vim.api.nvim_buf_is_loaded(buf) then
        done({ status = "skipped", reason = "buffer is unloaded" })
        return
    end

    local cap = client.server_capabilities or {}
    local cp = cap.completionProvider
    if not cp then
        done({ status = "unsupported", reason = "client has no completionProvider" })
        return
    end

    local params_ok, params, probe_cursor, probe_changedtick = pcall(function()
        return vim.api.nvim_buf_call(buf, function()
            return vim.lsp.util.make_position_params(0, client.offset_encoding or "utf-16"),
                vim.api.nvim_win_get_cursor(0),
                vim.api.nvim_buf_get_changedtick(buf)
        end)
    end)
    if not params_ok then
        done({ status = "error", error = params })
        return
    end

    local trigger_kind = vim.lsp.protocol.CompletionTriggerKind or {}
    params.context = { triggerKind = trigger_kind.Invoked or 1 }

    local start = vim.uv.now()
    local responded = false
    local ok, req_id = client:request("textDocument/completion", params, function(err, result)
        if responded then
            return
        end
        responded = true

        local elapsed = vim.uv.now() - start
        if err then
            local error_codes = vim.lsp.protocol.ErrorCodes or {}
            if err.code == error_codes.RequestCancelled or err.code == error_codes.ContentModified then
                done({
                    status = "skipped",
                    elapsed = elapsed,
                    reason = string.format("completion request superseded (%s)", tostring(err.code)),
                })
                return
            end
            done({ status = "error", elapsed = elapsed, error = err })
            return
        end

        local count, preview = summarize_completion_result(result)
        done({
            status = count > 0 and "ok" or "empty",
            elapsed = elapsed,
            count = count,
            preview = preview,
            probe_cursor = probe_cursor,
            probe_changedtick = probe_changedtick,
        })
    end, buf)

    if not ok then
        done({ status = "request_failed", reason = "client:request returned false" })
        return
    end

    vim.defer_fn(function()
        if responded then
            return
        end
        responded = true
        pcall(function()
            client:cancel_request(req_id)
        end)
        done({ status = "timeout", elapsed = COMPLETION_PROBE_TIMEOUT_MS })
    end, COMPLETION_PROBE_TIMEOUT_MS)
end

--- Probe raw JDTLS completion on the current Java buffer when that is safe.
local function probe_current_completion(done)
    local buf, client, reason = completion_probe_target()
    if not buf or not client then
        done({ status = "skipped", reason = reason })
        return
    end

    probe_completion(client, buf, done)
end

--- Probe completion and retry once when JDTLS returns an empty list.
---@param client vim.lsp.Client
---@param buf integer
---@param done fun(result: table)
local function probe_completion_for_recovery(client, buf, done)
    probe_completion(client, buf, function(first)
        if first.status ~= "empty" then
            done(first)
            return
        end

        vim.defer_fn(function()
            probe_completion(client, buf, function(second)
                second.first_empty = first
                done(second)
            end)
        end, COMPLETION_EMPTY_RETRY_DELAY_MS)
    end)
end

--- Return whether a completion probe still describes the buffer's current text and cursor.
---@param buf integer
---@param result table
---@return boolean
local function completion_probe_context_is_current(buf, result)
    if not vim.api.nvim_buf_is_loaded(buf) or not result.probe_cursor or not result.probe_changedtick then
        return false
    end
    if vim.api.nvim_buf_get_changedtick(buf) ~= result.probe_changedtick then
        return false
    end

    local ok, cursor = pcall(function()
        return vim.api.nvim_buf_call(buf, function()
            return vim.api.nvim_win_get_cursor(0)
        end)
    end)
    return ok and cursor[1] == result.probe_cursor[1] and cursor[2] == result.probe_cursor[2]
end

--- Format a completion probe result for logs and notifications.
local function format_completion_probe_result(result)
    if result.status == "ok" or result.status == "empty" then
        local preview = result.preview and #result.preview > 0 and " — " .. table.concat(result.preview, ", ") or ""
        local retry = result.first_empty and " after empty retry" or ""
        return string.format("%d items in %dms%s%s", result.count or 0, result.elapsed or 0, preview, retry)
    end

    if result.status == "error" then
        return string.format(
            "ERROR%s: %s",
            result.elapsed and " after " .. result.elapsed .. "ms" or "",
            vim.inspect(result.error)
        )
    end

    if result.status == "timeout" then
        return string.format("TIMEOUT after %dms", result.elapsed or COMPLETION_PROBE_TIMEOUT_MS)
    end

    return result.reason or result.status
end

--- Return the notification severity for a completion probe result.
local function completion_probe_level(result)
    if result.status == "ok" then
        return vim.log.levels.INFO
    end
    if result.status == "empty" or result.status == "skipped" then
        return vim.log.levels.WARN
    end
    return vim.log.levels.ERROR
end

--- Return whether a completion probe failed rather than returning an inconclusive empty result.
---@param result table
---@return boolean
local function completion_probe_failed(result)
    return result.status ~= "ok" and result.status ~= "empty" and result.status ~= "skipped"
end

--- Remember a gap that should be health-checked on the next Java buffer.
local function defer_gap_until_java_buffer(gap, source, reason)
    state.pending_gap = {
        gap = gap,
        source = source,
        reason = reason,
        at = vim.uv.now(),
    }
    logger.fmt_info(
        "gap %ds/%s: completion probe skipped (%s) -> waiting for Java BufEnter",
        math.floor(gap / 1000),
        source,
        reason
    )
end

--- Restart JDTLS after a failed health check, using hard restart only after very long gaps.
local function restart_after_failed_health_check(gap, gap_label, reason)
    if gap >= HARD_RESTART_GAP_MS then
        logger.fmt_info("gap %s: %s -> hard restart", gap_label, reason)
        hard_restart_all_jdtls("gap " .. gap_label .. ", " .. reason)
        return
    end

    logger.fmt_info("gap %s: %s -> full restart", gap_label, reason)
    restart_all_jdtls("gap " .. gap_label .. ", " .. reason)
end

--- Soft-recover one buffer by detaching and reattaching its JDTLS client.
---
--- Cheaper than full restart (no JVM startup, no re-index). Used when the
--- workspace is healthy but the buffer-specific didOpen state has been lost
--- (silent post-sleep failure).
local function soft_recover_buffer(buf, reason)
    if state.recovering then
        vim.notify("JDTLS soft recovery: another recovery is already in progress", vim.log.levels.WARN)
        return
    end
    if not vim.api.nvim_buf_is_loaded(buf) then
        return
    end
    local now = vim.uv.now()
    if (now - (state.buf_last_soft[buf] or 0)) < BUF_SOFT_COOLDOWN_MS then
        return
    end
    state.buf_last_soft[buf] = now

    local operation_id = begin_operation(true)
    state.recovering = true

    for _, c in ipairs(lsp_util.get_clients_by_name("jdtls", { bufnr = buf })) do
        pcall(vim.lsp.buf_detach_client, buf, c.id)
    end
    vim.defer_fn(function()
        if not operation_is_current(operation_id) then
            return
        end

        local ok, err = pcall(function()
            if vim.api.nvim_buf_is_loaded(buf) then
                local name = vim.api.nvim_buf_get_name(buf)
                if is_jdt_uri_name(name) then
                    attach_jdt_uri_buffer(buf, "soft recover: " .. reason)
                else
                    vim.api.nvim_buf_call(buf, function()
                        state.attach_fn(buf)
                    end)
                end
                local buf_name = vim.api.nvim_buf_get_name(buf)
                local msg = string.format("soft-recover buf %d (%s): %s", buf, vim.fs.basename(buf_name), reason)
                logger.info(msg)
                -- vim.notify("JDTLS " .. msg, vim.log.levels.INFO)
            end
        end)
        if not ok then
            logger.fmt_warn("soft recovery failed for buffer %d (%s): %s", buf, reason, tostring(err))
        end
        refresh_blink_lsp("soft recovery: " .. reason)
        finish_operation(operation_id)
    end, 200)
end

--- Reset Blink after a provider-pipeline failure and verify the rebuilt provider once.
---@param buf integer
---@param operation_id integer
---@param gap_label string
---@param first_result table
local function reset_and_reprobe_blink(buf, operation_id, gap_label, first_result)
    local first_probe = format_completion_probe_result(first_result)
    logger.fmt_warn("gap %s: Blink LSP pipeline unhealthy (%s) -> resetting Blink only", gap_label, first_probe)
    refresh_blink_lsp("gap " .. gap_label .. ", Blink probe " .. first_probe)

    vim.defer_fn(function()
        if not operation_is_current(operation_id) then
            return
        end

        probe_blink_lsp_completion(buf, function(second_result)
            if not operation_is_current(operation_id) then
                return
            end

            state.probing = false
            local second_probe = format_completion_probe_result(second_result)
            if second_result.status == "ok" then
                logger.fmt_info("gap %s: Blink LSP pipeline recovered after reset (%s)", gap_label, second_probe)
                return
            end
            if second_result.status == "skipped" then
                logger.fmt_info("gap %s: Blink re-probe skipped after reset (%s)", gap_label, second_probe)
                return
            end

            logger.fmt_warn(
                "gap %s: Blink LSP pipeline still unhealthy after reset (%s); JDTLS remains running",
                gap_label,
                second_probe
            )
            vim.notify(
                "JDTLS is healthy, but Blink LSP completion did not recover: " .. second_probe,
                vim.log.levels.WARN
            )
        end)
    end, BLINK_REPROBE_DELAY_MS)
end

--- Recover JDTLS after a detected wall-clock gap.
local function recover_after_gap(gap, source)
    if gap <= SLEEP_THRESHOLD_MS then
        return
    end
    if state.recovering or state.probing or in_cooldown() then
        return
    end
    if #real_java_buffers() == 0 then
        return
    end

    local clients = lsp_util.get_clients_by_name("jdtls")
    local gap_label = string.format("%ds/%s", math.floor(gap / 1000), source)

    if #clients == 0 then
        if state.manual_stopped then
            logger.fmt_info("gap %s: jdtls manually stopped -> skipping auto restart", gap_label)
            return
        end
        restart_all_jdtls("gap " .. gap_label .. ", no client")
        return
    end

    local probe_buf = select(1, completion_probe_target())
    local operation_id = begin_operation(probe_buf ~= nil)
    state.probing = true
    probe_all_clients(clients, function(any_dead)
        if not operation_is_current(operation_id) then
            return
        end

        state.probing = false
        if any_dead then
            restart_after_failed_health_check(gap, gap_label, "workspace probe failed")
            return
        end

        if not probe_buf then
            defer_gap_until_java_buffer(gap, source, "current buffer is not a real attached Java project buffer")
            return
        end

        local target_buf, target_client, target_reason = completion_probe_target(probe_buf)
        if not target_buf or not target_client then
            defer_gap_until_java_buffer(gap, source, target_reason or "probe buffer lost its jdtls client")
            return
        end

        state.probing = true
        probe_completion_for_recovery(target_client, target_buf, function(result)
            if not operation_is_current(operation_id) then
                return
            end

            state.probing = false
            if result.status == "skipped" then
                defer_gap_until_java_buffer(gap, source, result.reason or result.status)
                return
            end

            local probe_result = format_completion_probe_result(result)
            if completion_probe_failed(result) then
                restart_after_failed_health_check(gap, gap_label, "completion probe " .. probe_result)
                return
            end

            if result.status == "empty" then
                logger.fmt_info(
                    "gap %s: workspace healthy; raw completion inconclusive (%s) -> no recovery action",
                    gap_label,
                    probe_result
                )
                return
            end

            if not completion_probe_context_is_current(target_buf, result) then
                logger.fmt_info(
                    "gap %s: raw completion succeeded, but the probe buffer changed -> no recovery action",
                    gap_label
                )
                return
            end

            logger.fmt_info(
                "gap %s: workspace and raw completion healthy (%s) -> probing configured Blink LSP pipeline",
                gap_label,
                probe_result
            )
            state.probing = true
            probe_blink_lsp_completion(target_buf, function(blink_result)
                if not operation_is_current(operation_id) then
                    return
                end

                state.probing = false
                local blink_probe_result = format_completion_probe_result(blink_result)
                if blink_result.status == "skipped" then
                    logger.fmt_info("gap %s: configured Blink LSP probe skipped (%s)", gap_label, blink_probe_result)
                    return
                end

                if blink_result.status ~= "ok" then
                    state.probing = true
                    reset_and_reprobe_blink(target_buf, operation_id, gap_label, blink_result)
                    return
                end

                logger.fmt_info(
                    "gap %s: workspace, raw completion, and configured Blink LSP healthy (%s; blink %s)"
                        .. " -> no recovery action",
                    gap_label,
                    probe_result,
                    blink_probe_result
                )
            end)
        end)
    end)
end

--- Replay a deferred gap health check once a Java buffer is current.
local function run_pending_gap_on_java_bufenter()
    local pending = state.pending_gap
    if not pending then
        return
    end

    state.pending_gap = nil
    local pending_age = vim.uv.now() - pending.at
    logger.fmt_info(
        "running pending gap %ds/%s after Java BufEnter (deferred %ds)",
        math.floor(pending.gap / 1000),
        pending.source,
        math.floor(pending_age / 1000)
    )

    vim.defer_fn(function()
        recover_after_gap(pending.gap, pending.source .. "+java")
    end, 200)
end

--- Refresh the idle tick during ordinary editing and recover on long gaps.
local function update_tick()
    local gap, mono_gap = update_tick_and_get_gaps()
    if not gap or gap < 0 then
        return
    end

    -- Normal "reading without moving the cursor" produces both a wall-clock
    -- gap and a monotonic gap. A real system suspend often produces wall time
    -- advancing much further than monotonic time. Also recover after very long
    -- inactivity even if the platform's monotonic clock includes sleep.
    local suspend_gap = mono_gap and (gap - mono_gap) or 0
    if suspend_gap >= SLEEP_THRESHOLD_MS or gap >= LONG_SLEEP_MS then
        recover_after_gap(gap, "activity")
    end
end

--- Check focus/buffer-entry events for an idle gap that needs recovery.
local function check_gap()
    local gap = update_tick_and_get_gaps()
    if not gap or gap < 0 then
        return
    end
    recover_after_gap(gap, "focus")
end

--- Attach missing JDTLS clients on Java BufEnter and replay pending gap checks.
local function on_java_bufenter()
    if state.recovering then
        return
    end
    local buf = vim.api.nvim_get_current_buf()
    if vim.bo[buf].filetype ~= "java" then
        return
    end
    local name = vim.api.nvim_buf_get_name(buf)
    if is_jdt_uri_name(name) then
        if not lsp_util.get_client_by_name("jdtls", { bufnr = buf }) and lsp_util.get_client_by_name("jdtls") then
            attach_jdt_uri_buffer(buf, "BufEnter")
        end
        return
    end
    state.last_java_buf = buf

    if state.manual_stopped then
        return
    end

    if not lsp_util.get_client_by_name("jdtls", { bufnr = buf }) then
        if lsp_util.get_client_by_name("jdtls") then
            state.attach_fn(buf)
            vim.defer_fn(run_pending_gap_on_java_bufenter, 200)
        else
            run_pending_gap_on_java_bufenter()
        end
        return
    end
    run_pending_gap_on_java_bufenter()
end

--- Register JDTLS sleep-recovery autocmds and diagnostic commands.
---@param attach_fn fun(buf: integer)
function M.setup(attach_fn)
    state.attach_fn = attach_fn
    set_tick()

    local group = vim.api.nvim_create_augroup("jdtls_sleep_recovery", { clear = true })

    vim.api.nvim_create_autocmd({ "BufEnter", "FocusGained" }, {
        group = group,
        callback = check_gap,
    })

    -- Keep the tick fresh during normal editing so ordinary idle time below the
    -- recovery threshold does not trigger a false gap detection.
    vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI", "InsertEnter" }, {
        group = group,
        callback = update_tick,
    })

    vim.api.nvim_create_autocmd("BufEnter", {
        group = group,
        callback = on_java_bufenter,
    })

    vim.api.nvim_create_user_command("JdtlsRestart", function()
        vim.notify("JDTLS: restarting...", vim.log.levels.INFO)
        M.restart("manual")
    end, { desc = "Force-restart JDTLS for all Java buffers" })

    vim.api.nvim_create_user_command("JdtlsStop", function()
        M.stop("manual")
    end, { desc = "Stop JDTLS and remember loaded Java buffers for :JdtlsStart" })

    vim.api.nvim_create_user_command("JdtlsStart", function()
        M.start("manual")
    end, { desc = "Start JDTLS for stopped/current Java buffers" })

    vim.api.nvim_create_user_command("JdtlsToggle", function()
        M.toggle("manual toggle")
    end, { desc = "Toggle JDTLS start/stop using the recovery context" })

    vim.api.nvim_create_user_command("JdtlsHealthCheck", function()
        local clients = lsp_util.get_clients_by_name("jdtls")
        if #clients == 0 then
            vim.notify("JDTLS: no clients attached", vim.log.levels.WARN)
            return
        end
        vim.notify("JDTLS: probing " .. #clients .. " client(s)...", vim.log.levels.INFO)
        for _, client in ipairs(clients) do
            probe_client(client, function(alive)
                vim.notify(
                    string.format(
                        "JDTLS client %d (workspace): %s",
                        client.id,
                        alive and "HEALTHY" or "BROKEN (probe timed out)"
                    ),
                    alive and vim.log.levels.INFO or vim.log.levels.ERROR
                )
            end)
        end

        -- Also probe the current buffer for the silent-desync case.
        local cur = vim.api.nvim_get_current_buf()
        if vim.bo[cur].filetype == "java" then
            local buf_clients = lsp_util.get_clients_by_name("jdtls", { bufnr = cur })
            if #buf_clients == 0 then
                vim.notify(
                    string.format("JDTLS current buf %d: NO CLIENT attached (run :JdtlsSoftRecover)", cur),
                    vim.log.levels.ERROR
                )
            else
                probe_buffer(buf_clients[1], cur, function(alive)
                    vim.notify(
                        string.format(
                            "JDTLS current buf %d: %s",
                            cur,
                            alive and "HEALTHY" or "ZOMBIE (run :JdtlsSoftRecover)"
                        ),
                        alive and vim.log.levels.INFO or vim.log.levels.ERROR
                    )
                end)
            end

            probe_current_completion(function(result)
                vim.notify(
                    "JDTLS current buf completion: " .. format_completion_probe_result(result),
                    completion_probe_level(result)
                )
            end)
        end
    end, { desc = "Probe JDTLS health (workspace + current buffer + completion) without restarting" })

    vim.api.nvim_create_user_command("JdtlsSoftRecover", function()
        local cur = vim.api.nvim_get_current_buf()
        if vim.bo[cur].filetype ~= "java" then
            vim.notify("JdtlsSoftRecover: current buffer is not Java", vim.log.levels.WARN)
            return
        end
        state.buf_last_soft[cur] = 0
        soft_recover_buffer(cur, "manual")
    end, { desc = "Detach + reattach JDTLS for the current buffer only (cheap recovery)" })

    vim.api.nvim_create_user_command("JdtlsBlinkReset", function()
        refresh_blink_lsp("manual blink reset")
        vim.notify("JDTLS: blink LSP completion state reset", vim.log.levels.INFO)
    end, { desc = "Reset blink.cmp LSP state without restarting JDTLS" })

    vim.api.nvim_create_user_command("JdtlsBlinkLspProbe", function()
        local buf = manual_java_probe_buffer()
        if not buf then
            vim.notify("JdtlsBlinkLspProbe: no attached real Java buffer found", vim.log.levels.WARN)
            return
        end

        probe_blink_lsp_completion(buf, function(result)
            vim.notify("JdtlsBlinkLspProbe: " .. format_completion_probe_result(result), completion_probe_level(result))
        end)
    end, { desc = "Probe Blink's configured LSP provider pipeline without opening the completion menu" })

    -- Manually send a real textDocument/completion request at the cursor and
    -- report the raw result. Use this when completion feels broken — it tells
    -- us whether jdtls is the problem (returns 0 / errors / times out) or the
    -- client (jdtls returns items but blink does not surface them).
    vim.api.nvim_create_user_command("JdtlsCompletionProbe", function()
        probe_current_completion(function(result)
            vim.notify(
                "JdtlsCompletionProbe: " .. format_completion_probe_result(result),
                completion_probe_level(result)
            )
        end)
    end, { desc = "Send a real textDocument/completion at cursor and report raw result" })

    -- Print detailed jdtls state to :messages.
    vim.api.nvim_create_user_command("JdtlsDiag", function()
        local lines = {}
        local cur = vim.api.nvim_get_current_buf()
        local cur_name = vim.api.nvim_buf_get_name(cur)
        table.insert(lines, "=== JDTLS Diag ===")
        table.insert(lines, "cwd: " .. vim.fn.getcwd())
        table.insert(lines, string.format("current buf: %d (%s, ft=%s)", cur, cur_name, vim.bo[cur].filetype))
        table.insert(
            lines,
            string.format(
                "recovery: operation=%d recovering=%s probing=%s manual_stopped=%s pending_gap=%s last_java_buf=%s",
                state.operation_id,
                tostring(state.recovering),
                tostring(state.probing),
                tostring(state.manual_stopped),
                state.pending_gap and "yes" or "no",
                tostring(state.last_java_buf)
            )
        )

        local cur_clients = lsp_util.get_clients_by_name("jdtls", { bufnr = cur })
        table.insert(lines, "jdtls clients on current buf: " .. #cur_clients)

        local all = lsp_util.get_clients_by_name("jdtls")
        table.insert(lines, "")
        table.insert(lines, "Total jdtls clients: " .. #all)
        for _, c in ipairs(all) do
            local cap = c.server_capabilities or {}
            local cp = cap.completionProvider
            local bufs = vim.lsp.get_buffers_by_client_id(c.id)
            table.insert(
                lines,
                string.format(
                    "  client %d  initialized=%s  root=%s  bufs=%d  completionProvider=%s",
                    c.id,
                    tostring(c.initialized),
                    tostring(c.config and c.config.root_dir or "?"),
                    #bufs,
                    cp and "yes" or "no"
                )
            )
            if cp and cp.triggerCharacters then
                table.insert(lines, "    triggerCharacters: " .. table.concat(cp.triggerCharacters, " "))
            end
        end

        table.insert(lines, "")
        table.insert(lines, "Loaded Java buffers:")
        for _, b in ipairs(java_buffers()) do
            local cs = lsp_util.get_clients_by_name("jdtls", { bufnr = b })
            table.insert(lines, string.format("  buf %d  jdtls=%d  %s", b, #cs, vim.api.nvim_buf_get_name(b)))
        end

        for _, l in ipairs(lines) do
            print(l)
        end
        vim.notify("JdtlsDiag printed to :messages", vim.log.levels.INFO)
    end, { desc = "Show JDTLS attachment / capability state in :messages" })

    -- Wipe the workspace cache and restart. Slower than :JdtlsRestart (full
    -- workspace re-index, ~30-90s) but bypasses any corruption in the cache
    -- that survives ordinary restarts.
    vim.api.nvim_create_user_command("JdtlsHardRestart", function()
        M.hard_restart("manual")
    end, { desc = "Wipe JDTLS workspace cache and restart (slow, fixes cache corruption)" })
end

return M
