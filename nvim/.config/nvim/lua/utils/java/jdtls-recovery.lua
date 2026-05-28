-- JDTLS recovery after macOS sleep.
--
-- • setup — register sleep-detection autocmds and attach recovery probing
--
-- Detection: wall-clock gap between BufEnter/FocusGained events. macOS sleep
-- does not fire VimResume; FocusGained is unreliable in tmux. CursorMoved /
-- InsertEnter only update the tick (no gap check) so reading code for >2 min
-- does not look like sleep.
--
-- On gap: probe each jdtls client with `java.project.getAll`. Any response
-- (even an error) counts as alive — only timeout means dead.
--
-- NOTE: we deliberately do NOT probe textDocument/completion. Sending
-- completion to a URI that jdtls has not yet received didOpen for causes a
-- NullPointerException inside lsp4j (RemoteEndpoint.handleRequest, future=null)
-- which crashes the jdtls message loop and triggers a restart cascade.
--
-- On restart: we call attach_fn for EVERY loaded Java buffer (not just one per
-- root). jdtls.start_or_attach deduplicates by root_dir, so the server is
-- only started once. Without this, buffers not in the initial call have no
-- JDTLS because FileType does not re-fire for already-open buffers.

local M = {}

local SLEEP_THRESHOLD_MS = 2 * 60 * 1000
local PROBE_TIMEOUT_MS = 3000
local ACTION_COOLDOWN_MS = 30 * 1000

local state = {
    last_tick = nil,
    attach_fn = nil,
    recovering = false,
    probing = false,
    last_action_at = 0,
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

    for _, client in ipairs(vim.lsp.get_clients({ name = "jdtls" })) do
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
    if state.recovering or state.probing then
        return
    end
    if #java_buffers() == 0 then
        return
    end

    local clients = vim.lsp.get_clients({ name = "jdtls" })
    local gap_label = string.format("%ds", math.floor(gap / 1000))

    if #clients == 0 then
        restart_all_jdtls("gap " .. gap_label .. ", no client")
        return
    end

    state.probing = true
    probe_all_clients(clients, function(any_dead)
        state.probing = false
        if any_dead then
            restart_all_jdtls("gap " .. gap_label .. ", probe failed")
        end
    end)
end

-- When entering a Java buffer that has no jdtls client but one is already
-- running (for another buffer in the same project), attach it. This covers
-- buffers that were open during a restart but were not the first buffer
-- processed by restart_all_jdtls.
local function reattach_if_missing()
    if state.recovering then
        return
    end
    local buf = vim.api.nvim_get_current_buf()
    if vim.bo[buf].filetype ~= "java" then
        return
    end
    if #vim.lsp.get_clients({ bufnr = buf, name = "jdtls" }) > 0 then
        return
    end
    if #vim.lsp.get_clients({ name = "jdtls" }) == 0 then
        return
    end
    state.attach_fn(buf)
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
        callback = reattach_if_missing,
    })

    vim.api.nvim_create_user_command("JdtlsRestart", function()
        state.recovering = false
        state.last_action_at = 0
        vim.notify("JDTLS: restarting...", vim.log.levels.INFO)
        restart_all_jdtls("manual")
    end, { desc = "Force-restart JDTLS for all Java buffers" })

    vim.api.nvim_create_user_command("JdtlsHealthCheck", function()
        local clients = vim.lsp.get_clients({ name = "jdtls" })
        if #clients == 0 then
            vim.notify("JDTLS: no clients attached", vim.log.levels.WARN)
            return
        end
        vim.notify("JDTLS: probing " .. #clients .. " client(s)...", vim.log.levels.INFO)
        for _, client in ipairs(clients) do
            probe_client(client, function(alive)
                vim.notify(
                    string.format("JDTLS client %d: %s", client.id, alive and "HEALTHY" or "BROKEN (probe timed out)"),
                    alive and vim.log.levels.INFO or vim.log.levels.ERROR
                )
            end)
        end
    end, { desc = "Probe JDTLS health without restarting" })

    -- Print detailed jdtls state to :messages.
    vim.api.nvim_create_user_command("JdtlsDiag", function()
        local lines = {}
        local cur = vim.api.nvim_get_current_buf()
        local cur_name = vim.api.nvim_buf_get_name(cur)
        table.insert(lines, "=== JDTLS Diag ===")
        table.insert(lines, "cwd: " .. vim.fn.getcwd())
        table.insert(lines, string.format("current buf: %d (%s, ft=%s)", cur, cur_name, vim.bo[cur].filetype))

        local cur_clients = vim.lsp.get_clients({ bufnr = cur, name = "jdtls" })
        table.insert(lines, "jdtls clients on current buf: " .. #cur_clients)

        local all = vim.lsp.get_clients({ name = "jdtls" })
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
            local cs = vim.lsp.get_clients({ bufnr = b, name = "jdtls" })
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
        for _, c in ipairs(vim.lsp.get_clients({ name = "jdtls" })) do
            if c.config and c.config.root_dir then
                local name = vim.fs.basename(c.config.root_dir)
                roots[name] = true
            end
        end

        -- Now stop clients so the cache is not in use.
        for _, client in ipairs(vim.lsp.get_clients({ name = "jdtls" })) do
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
