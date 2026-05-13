-- Detect macOS sleep/wake by watching the wall-clock gap between cheap idle/focus
-- events. macOS sleep does not fire VimResume, and FocusGained is unreliable in
-- tmux/terminals, so we infer "machine just woke up" from a long delta between
-- events that *would* have been firing during normal use.
--
-- A long gap alone is NOT enough — the user may have just been at lunch with
-- jdtls perfectly fine. So when a gap is detected we *probe* each jdtls client
-- with a lightweight LSP request (`java.project.getAll`) and only restart the
-- ones that fail to reply within a short timeout. This avoids needless restarts
-- when jdtls is healthy.
--
-- Cost in steady state: two integer comparisons per BufEnter / CursorHold /
-- FocusGained. The probe only runs after a >2 minute gap.

local M = {}

-- A gap larger than this between events is treated as "machine slept".
-- 2 minutes is well above CursorHold (4s default) and any normal idle pattern.
local SLEEP_THRESHOLD_MS = 2 * 60 * 1000

-- How long to wait for a probe reply before declaring jdtls dead. Healthy jdtls
-- replies to java.project.getAll in well under a second; 3s is generous.
local PROBE_TIMEOUT_MS = 3000

-- Completion has more variance — first-shot completion on a cold buffer can
-- legitimately take a few seconds while the engine warms caches. Be lenient
-- here so we don't restart healthy jdtls just because completion was slow.
local COMPLETION_PROBE_TIMEOUT_MS = 6000

-- Refuse to do another auto-restart / auto-reattach within this window after
-- the previous one. Prevents a feedback loop when jdtls init fails: LspDetach
-- fires, we re-attach, init fails again, LspDetach fires, ...
local ACTION_COOLDOWN_MS = 30 * 1000

local state = {
    last_tick = nil,
    attach_fn = nil,
    root_markers = nil,
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

local function any_java_buffer_loaded()
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
        if vim.api.nvim_buf_is_loaded(buf) and vim.bo[buf].filetype == "java" then
            return true
        end
    end
    return false
end

local function restart_all_jdtls(reason)
    if state.recovering then
        return
    end
    if not any_java_buffer_loaded() then
        return
    end
    state.recovering = true
    mark_action()

    -- Force-stop existing clients. force=true skips the shutdown handshake,
    -- which is essential when the JVM is a zombie and won't reply.
    for _, client in ipairs(vim.lsp.get_clients({ name = "jdtls" })) do
        pcall(vim.lsp.stop_client, client.id, true)
    end

    -- Small delay so the old client is fully cleared before re-attaching.
    vim.defer_fn(function()
        local seen_roots = {}
        for _, buf in ipairs(vim.api.nvim_list_bufs()) do
            if vim.api.nvim_buf_is_loaded(buf) and vim.bo[buf].filetype == "java" then
                local fname = vim.api.nvim_buf_get_name(buf)
                local root = vim.fs.root(fname, state.root_markers)
                if root and not seen_roots[root] then
                    seen_roots[root] = true
                    vim.api.nvim_buf_call(buf, function()
                        state.attach_fn(buf)
                    end)
                end
            end
        end
        state.recovering = false
        if next(seen_roots) then
            vim.notify("JDTLS recovered (" .. reason .. ")", vim.log.levels.INFO)
        end
    end, 500)
end

-- Send a single LSP request with a timeout. Calls `done(alive)` exactly once.
-- A successful reply counts as alive. A timeout or failed send counts as dead.
-- An error reply is reported as `(false, err)` — the caller decides whether
-- error responses indicate a broken server (true for completion, where
-- internal-error replies mean the JDT scanner/parser state is corrupted) or
-- just a normal error condition (false for command probes, where errors can
-- be benign — wrong args, command not registered yet, etc.).
local function request_with_timeout(client, method, params, bufnr, timeout_ms, done)
    local responded = false
    local request_id

    local function finish(alive, err)
        if responded then
            return
        end
        responded = true
        done(alive, err)
    end

    local ok, req_id = client:request(method, params, function(err)
        finish(err == nil, err)
    end, bufnr)

    if not ok then
        finish(false, nil)
        return
    end
    request_id = req_id

    vim.defer_fn(function()
        if responded then
            return
        end
        pcall(function()
            client:cancel_request(request_id)
        end)
        finish(false, nil)
    end, timeout_ms)
end

-- Pick any Java buffer currently attached to this client. Needed for the
-- completion probe, which requires a known textDocument.
local function pick_attached_buffer(client)
    for _, buf in ipairs(vim.lsp.get_buffers_by_client_id(client.id)) do
        if vim.api.nvim_buf_is_loaded(buf) and vim.bo[buf].filetype == "java" then
            return buf
        end
    end
    return nil
end

-- Probe a single client. Tests two distinct code paths:
--   1. workspace/executeCommand java.project.getAll — basic command pipeline.
--      Treat error responses as alive (errors here can be benign).
--   2. textDocument/completion at line 0 col 0 of an attached buffer —
--      exercises the content-assist subsystem. After macOS suspend, JDTLS
--      typically responds to completion with internal errors like
--      `IllegalStateException: Invalid completion proposal` or
--      `NegativeArraySizeException` from the JDT scanner. Treat ANY error
--      response here as a corrupt server.
-- If either probe declares dead, the client is restarted.
local function probe_client(client, done)
    local cmd_ok = nil
    local completion_ok = nil

    local function maybe_finish()
        if cmd_ok == nil or completion_ok == nil then
            return
        end
        done(cmd_ok and completion_ok)
    end

    request_with_timeout(
        client,
        "workspace/executeCommand",
        {
            command = "java.project.getAll",
        },
        nil,
        PROBE_TIMEOUT_MS,
        function(alive, err)
            -- For the cmd probe, an error reply still proves the RPC channel
            -- is alive. Only timeout/send-failure (alive=false, err=nil) is
            -- treated as dead.
            cmd_ok = alive or err ~= nil
            maybe_finish()
        end
    )

    local buf = pick_attached_buffer(client)
    if not buf then
        -- No buffer to probe completion against — skip that half of the probe.
        completion_ok = true
        maybe_finish()
        return
    end

    local uri = vim.uri_from_bufnr(buf)
    request_with_timeout(
        client,
        "textDocument/completion",
        {
            textDocument = { uri = uri },
            position = { line = 0, character = 0 },
            context = { triggerKind = 1 }, -- Invoked
        },
        buf,
        COMPLETION_PROBE_TIMEOUT_MS,
        function(alive, err)
            -- For the completion probe, error responses mean the JDT
            -- scanner/parser is in a corrupt state. Only a clean reply counts
            -- as healthy.
            completion_ok = alive and err == nil
            maybe_finish()
        end
    )
end

-- Probe all jdtls clients in parallel. Calls `done(any_dead)` once every probe
-- has resolved (responded or timed out).
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
    if not any_java_buffer_loaded() then
        return
    end

    local clients = vim.lsp.get_clients({ name = "jdtls" })
    local gap_label = string.format("%ds", math.floor(gap / 1000))

    -- No clients but there are Java buffers — nothing to probe, just attach.
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

---@param attach_fn fun(buf: integer)  function that re-attaches jdtls to the current buffer
---@param root_markers string[]        markers used by vim.fs.root to find the project root
function M.setup(attach_fn, root_markers)
    state.attach_fn = attach_fn
    state.root_markers = root_markers
    state.last_tick = vim.uv.now()

    local group = vim.api.nvim_create_augroup("jdtls_sleep_recovery", { clear = true })

    -- BufEnter / FocusGained: catches the user actively coming back.
    -- CursorHold: fires after CursorHoldTime (default 4s) of idle, so even
    -- without focus events it triggers when the user pauses post-wake.
    vim.api.nvim_create_autocmd({ "BufEnter", "CursorHold", "FocusGained" }, {
        group = group,
        callback = check_gap,
    })

    -- LspDetach: when a jdtls client leaves a Java buffer for any reason
    -- (process exit, stop_client, etc.), re-attach. Skipped while we are
    -- mid-restart, and gated by ACTION_COOLDOWN_MS so a failing init can't
    -- trigger an infinite detach -> reattach -> init fail -> detach loop.
    vim.api.nvim_create_autocmd("LspDetach", {
        group = group,
        callback = function(args)
            if state.recovering or in_cooldown() then
                return
            end
            local client = vim.lsp.get_client_by_id(args.data.client_id)
            if not client or client.name ~= "jdtls" then
                return
            end
            local buf = args.buf
            -- Defer: let the detach finish before re-attaching, and let the
            -- buffer settle in case it's being closed.
            vim.defer_fn(function()
                if not vim.api.nvim_buf_is_loaded(buf) then
                    return
                end
                if vim.bo[buf].filetype ~= "java" then
                    return
                end
                if #vim.lsp.get_clients({ bufnr = buf, name = "jdtls" }) > 0 then
                    return
                end
                if in_cooldown() then
                    return
                end
                mark_action()
                vim.api.nvim_buf_call(buf, function()
                    state.attach_fn(buf)
                end)
                vim.notify("JDTLS reattached after detach", vim.log.levels.INFO)
            end, 1000)
        end,
    })

    vim.api.nvim_create_user_command("JdtlsRestart", function()
        -- Manual restart bypasses cooldown and recovering guard.
        state.recovering = false
        state.last_action_at = 0
        vim.notify("JDTLS: manual restart requested", vim.log.levels.INFO)
        restart_all_jdtls("manual")
    end, { desc = "Force-restart JDTLS for all Java buffers" })

    -- Diagnostic: probe each jdtls client and report results without
    -- restarting. Useful to verify whether the recovery probe correctly
    -- detects the broken state.
    vim.api.nvim_create_user_command("JdtlsHealthCheck", function()
        local clients = vim.lsp.get_clients({ name = "jdtls" })
        if #clients == 0 then
            vim.notify("JDTLS: no clients attached", vim.log.levels.WARN)
            return
        end
        vim.notify("JDTLS: probing " .. #clients .. " client(s)...", vim.log.levels.INFO)
        for _, client in ipairs(clients) do
            probe_client(client, function(alive)
                local msg =
                    string.format("JDTLS client %d (%s): %s", client.id, client.name, alive and "HEALTHY" or "BROKEN")
                vim.notify(msg, alive and vim.log.levels.INFO or vim.log.levels.ERROR)
            end)
        end
    end, { desc = "Probe JDTLS clients and report health without restarting" })
end

return M
