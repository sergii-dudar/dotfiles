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
--   3. completion healthy → reset blink.cmp LSP state only; do not restart
--      jdtls. This handles stale client-side completion wiring cheaply.
--   4. completion empty/error/timeout → restart jdtls. After a very long gap,
--      always use a hard restart because stale workspace/APT/client state can
--      survive healthy-looking probes and ordinary restarts.
--
-- Why completion probing is guarded: sending completion to a URI that jdtls
-- has not yet received didOpen for can crash the jdtls message loop. The
-- automatic probe therefore only uses the current buffer after Neovim reports
-- an attached jdtls client for it. If focus returns to a non-Java buffer, the
-- gap is remembered and checked on the next Java BufEnter.
--
-- On restart: we call attach_fn for EVERY loaded Java buffer (not just one per
-- root). jdtls.start_or_attach deduplicates by root_dir, so the server is
-- only started once. Without this, buffers not in the initial call have no
-- JDTLS because FileType does not re-fire for already-open buffers.
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
local ACTION_COOLDOWN_MS = 30 * 1000
local BUF_SOFT_COOLDOWN_MS = 10 * 1000
local PENDING_GAP_TTL_MS = 10 * 60 * 1000
local RESTART_SHUTDOWN_TIMEOUT_MS = 10 * 1000
local RESTART_ATTACH_GRACE_MS = 1000
local RESTART_ATTACH_POLL_MS = 200

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
    last_action_at = 0,
    -- per-buffer cooldown timestamps for soft recovery
    buf_last_soft = {},
    -- gap health check deferred until the user enters a Java buffer
    pending_gap = nil,
}

local function in_cooldown()
    return (vim.uv.now() - state.last_action_at) < ACTION_COOLDOWN_MS
end

local function mark_action()
    state.last_action_at = vim.uv.now()
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

local function java_buffers()
    local bufs = {}
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
        if vim.api.nvim_buf_is_loaded(buf) and vim.bo[buf].filetype == "java" then
            table.insert(bufs, buf)
        end
    end
    return bufs
end

--- Request graceful stop for every active JDTLS client.
local function request_stop_jdtls_clients()
    local clients = lsp_util.get_clients_by_name("jdtls")
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
local function wait_for_clients_to_close(clients, done)
    local deadline = vim.uv.now() + RESTART_SHUTDOWN_TIMEOUT_MS + RESTART_ATTACH_GRACE_MS

    local function poll()
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

--- Clear blink.cmp's active LSP completion queue and cached LSP source items.
local function reset_blink_lsp_state(reason)
    local trigger_ok, trigger = pcall(require, "blink.cmp.completion.trigger")
    if trigger_ok then
        local hide_ok, hide_err = pcall(function()
            trigger.hide()
        end)
        if not hide_ok then
            logger.fmt_warn("blink trigger reset failed (%s): %s", reason, tostring(hide_err))
        end
    end

    local list_ok, list = pcall(require, "blink.cmp.completion.list")
    if list_ok then
        local hide_ok, hide_err = pcall(function()
            list.hide()
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

--- Reattach JDTLS to loaded Java buffers and finish the recovery cycle.
local function reattach_java_buffers(bufs, reason)
    pcall(function()
        require("utils.java.jdtls-workspace-watcher").mark_recovery_refresh(reason)
    end)

    local attached = 0
    for _, buf in ipairs(bufs) do
        if vim.api.nvim_buf_is_loaded(buf) then
            vim.api.nvim_buf_call(buf, function()
                state.attach_fn(buf)
            end)
            attached = attached + 1
        end
    end
    -- Reset the sleep detector after reattach so the next event does not reuse
    -- the stale pre-recovery timestamp.
    set_tick()
    state.recovering = false
    if attached > 0 then
        vim.defer_fn(function()
            refresh_blink_lsp(reason)
        end, RESTART_ATTACH_GRACE_MS)
        vim.notify(string.format("JDTLS recovered (%s, %d buffers)", reason, attached), vim.log.levels.INFO)
    end
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

    if #wiped == 0 and next(project_names) == nil and vim.fn.isdirectory(cache_dir) == 1 then
        vim.fn.delete(cache_dir, "rf")
        table.insert(wiped, "<all jdtls cache>")
    end

    return wiped
end

--- Stop JDTLS, wipe project cache, and reattach all loaded Java buffers.
local function hard_restart_all_jdtls(reason)
    if state.recovering then
        return
    end
    local bufs = java_buffers()
    if #bufs == 0 then
        return
    end

    local clients = lsp_util.get_clients_by_name("jdtls")
    local project_names = project_names_from_clients(clients)

    state.recovering = true
    mark_action()

    logger.fmt_info("hard restart (%s): %d java buffers", reason, #bufs)

    wait_for_clients_to_close(request_stop_jdtls_clients(), function()
        local wiped = wipe_jdtls_cache(project_names)
        if #wiped > 0 then
            logger.fmt_info("hard restart (%s): wiped cache for %s", reason, table.concat(wiped, ", "))
            vim.notify("JDTLS hard restart: wiped cache for " .. table.concat(wiped, ", "), vim.log.levels.INFO)
        else
            logger.fmt_warn("hard restart (%s): no cache directories found to wipe", reason)
        end
        reattach_java_buffers(bufs, "hard restart: " .. reason)
    end)
end

--- Stop JDTLS and reattach all loaded Java buffers without wiping cache.
local function restart_all_jdtls(reason)
    if state.recovering then
        return
    end
    local bufs = java_buffers()
    if #bufs == 0 then
        return
    end
    state.recovering = true
    mark_action()

    logger.fmt_info("full restart (%s): %d java buffers", reason, #bufs)

    wait_for_clients_to_close(request_stop_jdtls_clients(), function()
        reattach_java_buffers(bufs, reason)
    end)
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
local function summarize_completion_result(result)
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

--- Return the current Java buffer and attached JDTLS client for safe completion probing.
local function completion_probe_target()
    local buf = vim.api.nvim_get_current_buf()
    if vim.bo[buf].filetype ~= "java" then
        return nil, nil, "current buffer is not Java"
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

    local params_ok, params = pcall(function()
        return vim.api.nvim_buf_call(buf, function()
            return vim.lsp.util.make_position_params(0, client.offset_encoding or "utf-16")
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
            done({ status = "error", elapsed = elapsed, error = err })
            return
        end

        local count, preview = summarize_completion_result(result)
        done({
            status = count > 0 and "ok" or "empty",
            elapsed = elapsed,
            count = count,
            preview = preview,
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
local function probe_current_completion_for_recovery(done)
    probe_current_completion(function(first)
        if first.status ~= "empty" then
            done(first)
            return
        end

        vim.defer_fn(function()
            probe_current_completion(function(second)
                second.first_empty = first
                done(second)
            end)
        end, COMPLETION_EMPTY_RETRY_DELAY_MS)
    end)
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

--- Return whether a completion probe result proves JDTLS needs a restart.
local function completion_probe_failed(result)
    return result.status ~= "ok" and result.status ~= "skipped"
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

-- Soft recovery: detach + reattach a single buffer. Cheaper than full restart
-- (no JVM startup, no re-index). Used when the workspace is healthy but the
-- buffer-specific didOpen state has been lost (silent post-sleep failure).
local function soft_recover_buffer(buf, reason)
    if not vim.api.nvim_buf_is_loaded(buf) then
        return
    end
    local now = vim.uv.now()
    if (now - (state.buf_last_soft[buf] or 0)) < BUF_SOFT_COOLDOWN_MS then
        return
    end
    state.buf_last_soft[buf] = now

    for _, c in ipairs(lsp_util.get_clients_by_name("jdtls", { bufnr = buf })) do
        pcall(vim.lsp.buf_detach_client, buf, c.id)
    end
    vim.defer_fn(function()
        if vim.api.nvim_buf_is_loaded(buf) then
            vim.api.nvim_buf_call(buf, function()
                state.attach_fn(buf)
            end)
            local name = vim.api.nvim_buf_get_name(buf)
            local msg = string.format("soft-recover buf %d (%s): %s", buf, vim.fs.basename(name), reason)
            logger.info(msg)
            -- vim.notify("JDTLS " .. msg, vim.log.levels.INFO)
        end
    end, 200)
end

--- Recover JDTLS after a detected wall-clock gap.
local function recover_after_gap(gap, source)
    if gap <= SLEEP_THRESHOLD_MS then
        return
    end
    if state.recovering or state.probing or in_cooldown() then
        return
    end
    if #java_buffers() == 0 then
        return
    end

    local clients = lsp_util.get_clients_by_name("jdtls")
    local gap_label = string.format("%ds/%s", math.floor(gap / 1000), source)

    if #clients == 0 then
        restart_all_jdtls("gap " .. gap_label .. ", no client")
        return
    end

    state.probing = true
    probe_all_clients(clients, function(any_dead)
        state.probing = false
        if any_dead then
            restart_after_failed_health_check(gap, gap_label, "workspace probe failed")
            return
        end

        if gap >= HARD_RESTART_GAP_MS then
            logger.fmt_info("gap %s: workspace healthy but very long gap -> hard restart", gap_label)
            hard_restart_all_jdtls("gap " .. gap_label .. ", long idle workspace refresh")
            return
        end

        state.probing = true
        probe_current_completion_for_recovery(function(result)
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

            logger.fmt_info(
                "gap %s: workspace and completion healthy (%s) -> blink refresh only",
                gap_label,
                probe_result
            )
            refresh_blink_lsp("gap " .. gap_label .. ", completion healthy")
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
    if (vim.uv.now() - pending.at) > PENDING_GAP_TTL_MS then
        logger.fmt_info(
            "dropping pending gap %ds/%s after Java BufEnter: expired",
            math.floor(pending.gap / 1000),
            pending.source
        )
        return
    end

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

    -- Keep the tick fresh during normal editing so reading code for >2 min
    -- does not trigger a false gap detection.
    vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI", "InsertEnter" }, {
        group = group,
        callback = update_tick,
    })

    vim.api.nvim_create_autocmd("BufEnter", {
        group = group,
        pattern = "*.java",
        callback = on_java_bufenter,
    })

    vim.api.nvim_create_user_command("JdtlsRestart", function()
        state.recovering = false
        state.last_action_at = 0
        vim.notify("JDTLS: restarting...", vim.log.levels.INFO)
        restart_all_jdtls("manual")
    end, { desc = "Force-restart JDTLS for all Java buffers" })

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
        hard_restart_all_jdtls("manual")
    end, { desc = "Wipe JDTLS workspace cache and restart (slow, fixes cache corruption)" })
end

return M
