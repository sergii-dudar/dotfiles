-- JDTLS recovery after macOS sleep.
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
end

return M
