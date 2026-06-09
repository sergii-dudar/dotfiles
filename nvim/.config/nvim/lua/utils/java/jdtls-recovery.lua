-- JDTLS recovery after macOS sleep.
--
-- • setup — register sleep-detection autocmds and attach recovery probing
--
-- Detection: wall-clock gap between BufEnter/FocusGained events. macOS sleep
-- does not fire VimResume; FocusGained is unreliable in tmux. CursorMoved /
-- InsertEnter only update the tick (no gap check) so reading code for >2 min
-- does not look like sleep.
--
-- Three-tier action on gap:
--   1. workspace probe (`java.project.getAll`) — if it times out, the jdtls
--      process / message loop is dead → full restart.
--   2. gap >= 1 hour (LONG_SLEEP_MS) — jdtls's process-wide completion state
--      is empirically unsalvageable by soft-recover after long suspends:
--      even buffers opened fresh AFTER the wake have broken completion.
--      Skip the cheap path and full-restart immediately.
--   3. shorter gap, workspace alive → mark every loaded Java buffer as
--      "needs_resync" and soft-recover the current buffer immediately.
--      Other Java buffers are soft-recovered lazily on first BufEnter
--      (avoids reattaching buffers the user never visits again).
--
-- Why unconditional soft-recover on short gaps instead of probing the
-- buffer? jdtls's per-buffer completion state silently rots during sleep:
-- workspace requests still answer, foldingRange still answers, only
-- completion goes dead. There is no cheap, safe request that reliably
-- distinguishes "completion fine" from "completion silently dead", so we
-- always pay the soft-recover cost (~200ms, single buffer) after a sleep
-- gap. didClose + didOpen forces jdtls to rebuild the per-buffer state.
--
-- NOTE: we deliberately do NOT probe textDocument/completion. Sending
-- completion to a URI that jdtls has not yet received didOpen for causes a
-- NullPointerException inside lsp4j (RemoteEndpoint.handleRequest, future=null)
-- which crashes the jdtls message loop and triggers a restart cascade.
-- foldingRange is the lightest buffer-scoped request that does not have this
-- failure mode.
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

local SLEEP_THRESHOLD_MS = 2 * 60 * 1000
-- Above this gap, jdtls's process-wide state is empirically unsalvageable
-- by soft-recover alone — even buffers opened fresh AFTER the wake have
-- broken completion. Escalate straight to full restart for long suspends.
local LONG_SLEEP_MS = 60 * 60 * 1000
local PROBE_TIMEOUT_MS = 3000
local BUF_PROBE_TIMEOUT_MS = 2000
local ACTION_COOLDOWN_MS = 30 * 1000
local BUF_SOFT_COOLDOWN_MS = 10 * 1000

local state = {
    last_tick = nil,
    attach_fn = nil,
    recovering = false,
    probing = false,
    last_action_at = 0,
    -- per-buffer cooldown timestamps for soft recovery
    buf_last_soft = {},
    -- buffers flagged for soft-recover on next BufEnter (post-sleep lazy resync)
    needs_resync = {},
}

local function in_cooldown()
    return (vim.uv.now() - state.last_action_at) < ACTION_COOLDOWN_MS
end

local function mark_action()
    state.last_action_at = vim.uv.now()
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

    for _, client in ipairs(lsp_util.get_clients_by_name("jdtls")) do
        pcall(vim.lsp.stop_client, client.id, true)
    end

    vim.defer_fn(function()
        local attached = 0
        for _, buf in ipairs(bufs) do
            if vim.api.nvim_buf_is_loaded(buf) then
                vim.api.nvim_buf_call(buf, function()
                    state.attach_fn(buf)
                end)
                attached = attached + 1
            end
        end
        -- Reset tick to now so that the next gap check starts from this
        -- point, not from the pre-sleep time that triggered this restart.
        state.last_tick = vim.uv.now()
        state.recovering = false
        if attached > 0 then
            vim.notify(string.format("JDTLS recovered (%s, %d buffers)", reason, attached), vim.log.levels.INFO)
        end
    end, 500)
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

local function update_tick()
    state.last_tick = vim.uv.now()
end

local function check_gap()
    local now = vim.uv.now()
    local prev = state.last_tick
    state.last_tick = now
    if not prev then
        return
    end
    local gap = now - prev
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
    local gap_label = string.format("%ds", math.floor(gap / 1000))

    if #clients == 0 then
        restart_all_jdtls("gap " .. gap_label .. ", no client")
        return
    end

    state.probing = true
    probe_all_clients(clients, function(any_dead)
        state.probing = false
        if any_dead then
            logger.fmt_info("workspace probe failed (gap %s) -> full restart", gap_label)
            restart_all_jdtls("gap " .. gap_label .. ", probe failed")
            return
        end

        -- Long-suspend escalation: jdtls's process-wide completion state is
        -- empirically unsalvageable by soft-recover after multi-hour
        -- suspends — even freshly-opened buffers have broken completion.
        -- Skip the cheap path and go straight to full restart.
        if gap >= LONG_SLEEP_MS then
            logger.fmt_info("gap %s exceeds long-sleep threshold -> full restart", gap_label)
            restart_all_jdtls("gap " .. gap_label .. ", long sleep")
            return
        end

        -- Short suspend: workspace is alive, but jdtls's per-buffer
        -- completion state can silently rot. Cure is buffer-level
        -- didClose+didOpen (soft recover). Mark all loaded Java buffers as
        -- "needs_resync" and soft-recover the current buffer immediately;
        -- others get done lazily on first BufEnter.
        local java_bufs = java_buffers()
        for _, b in ipairs(java_bufs) do
            state.needs_resync[b] = true
        end
        logger.fmt_info("gap %s: workspace healthy, marked %d java buffers for resync", gap_label, #java_bufs)

        local cur = vim.api.nvim_get_current_buf()
        if vim.bo[cur].filetype == "java" and state.needs_resync[cur] then
            state.needs_resync[cur] = nil
            soft_recover_buffer(cur, "gap " .. gap_label .. ", post-sleep resync")
        end
    end)
end

-- BufEnter for java buffers: cover two cases.
--   1. Buffer has no jdtls client at all → attach.
--   2. Buffer is flagged for post-sleep resync → soft recover lazily.
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
            state.needs_resync[buf] = nil
            state.attach_fn(buf)
        end
        return
    end
    if state.needs_resync[buf] then
        state.needs_resync[buf] = nil
        soft_recover_buffer(buf, "post-sleep lazy resync")
    end
end

---@param attach_fn fun(buf: integer)
function M.setup(attach_fn)
    state.attach_fn = attach_fn
    state.last_tick = vim.uv.now()

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
        end
    end, { desc = "Probe JDTLS health (workspace + current buffer) without restarting" })

    vim.api.nvim_create_user_command("JdtlsSoftRecover", function()
        local cur = vim.api.nvim_get_current_buf()
        if vim.bo[cur].filetype ~= "java" then
            vim.notify("JdtlsSoftRecover: current buffer is not Java", vim.log.levels.WARN)
            return
        end
        state.buf_last_soft[cur] = 0
        soft_recover_buffer(cur, "manual")
    end, { desc = "Detach + reattach JDTLS for the current buffer only (cheap recovery)" })

    -- Manually send a real textDocument/completion request at the cursor and
    -- report the raw result. Use this when completion feels broken — it tells
    -- us whether jdtls is the problem (returns 0 / errors / times out) or the
    -- client (jdtls returns items but blink does not surface them).
    vim.api.nvim_create_user_command("JdtlsCompletionProbe", function()
        local cur = vim.api.nvim_get_current_buf()
        if vim.bo[cur].filetype ~= "java" then
            vim.notify("JdtlsCompletionProbe: current buffer is not Java", vim.log.levels.WARN)
            return
        end
        local client = lsp_util.get_client_by_name("jdtls", { bufnr = cur })
        if not client then
            vim.notify("JdtlsCompletionProbe: no jdtls client on buffer", vim.log.levels.ERROR)
            return
        end
        local cap = client.server_capabilities or {}
        local cp = cap.completionProvider
        if not cp then
            vim.notify(
                "JdtlsCompletionProbe: client advertises NO completionProvider capability!",
                vim.log.levels.ERROR
            )
            return
        end

        local params = vim.lsp.util.make_position_params(0, client.offset_encoding or "utf-16")
        local start = vim.uv.now()
        local responded = false
        local ok, req_id = client:request("textDocument/completion", params, function(err, result)
            if responded then
                return
            end
            responded = true
            local elapsed = vim.uv.now() - start
            if err then
                vim.notify(
                    string.format("JdtlsCompletionProbe: ERROR after %dms: %s", elapsed, vim.inspect(err)),
                    vim.log.levels.ERROR
                )
                return
            end
            local items = result
            if type(result) == "table" and result.items then
                items = result.items
            end
            local count = items and #items or 0
            local preview = {}
            for i = 1, math.min(5, count) do
                table.insert(preview, items[i].label or "?")
            end
            vim.notify(
                string.format(
                    "JdtlsCompletionProbe: %d items in %dms%s",
                    count,
                    elapsed,
                    #preview > 0 and " — " .. table.concat(preview, ", ") or ""
                ),
                count > 0 and vim.log.levels.INFO or vim.log.levels.WARN
            )
        end, cur)

        if not ok then
            vim.notify("JdtlsCompletionProbe: request() returned false", vim.log.levels.ERROR)
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
            vim.notify("JdtlsCompletionProbe: TIMEOUT after 5s", vim.log.levels.ERROR)
        end, 5000)
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
        local cache_dir = vim.fn.stdpath("cache") .. "/jdtls"

        -- Capture root_dirs BEFORE stopping clients (after stop, config is gone).
        local roots = {}
        for _, c in ipairs(lsp_util.get_clients_by_name("jdtls")) do
            if c.config and c.config.root_dir then
                local name = vim.fs.basename(c.config.root_dir)
                roots[name] = true
            end
        end

        -- Now stop clients so the cache is not in use.
        for _, client in ipairs(lsp_util.get_clients_by_name("jdtls")) do
            pcall(vim.lsp.stop_client, client.id, true)
        end

        vim.defer_fn(function()
            local wiped = {}
            for project_name in pairs(roots) do
                local p = cache_dir .. "/" .. project_name
                if vim.fn.isdirectory(p) == 1 then
                    vim.fn.delete(p, "rf")
                    table.insert(wiped, project_name)
                end
            end
            if #wiped == 0 then
                -- Fallback: nuke entire jdtls cache if we couldn't pin down a project.
                if vim.fn.isdirectory(cache_dir) == 1 then
                    vim.fn.delete(cache_dir, "rf")
                    table.insert(wiped, "<all jdtls cache>")
                end
            end
            vim.notify("JDTLS hard restart: wiped cache for " .. table.concat(wiped, ", "), vim.log.levels.INFO)
            state.recovering = false
            state.last_action_at = 0
            restart_all_jdtls("hard restart")
        end, 500)
    end, { desc = "Wipe JDTLS workspace cache and restart (slow, fixes cache corruption)" })
end

return M
