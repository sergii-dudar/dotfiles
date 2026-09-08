-- Patches a JDTLS client so that ~3s after the workspace settles
-- (no $/progress events for IDLE_MS following ServiceReady), it dispatches
-- update_projects_config to force m2e to re-run APT processors (MapStruct etc.).
--
-- The refresh is sent through the specific client that became ready. This
-- matters when several Java workspaces are open: nvim-jdtls's public
-- update_projects_config helper uses the current buffer, but these timers fire
-- asynchronously and the current buffer can belong to another project.
--
-- • setup — patch a JDTLS client with the workspace watcher (idempotent)
-- • mark_recovery_refresh — make newly-restarted clients run a post-refresh
--   full build after recovery (root-scoped when root dirs are supplied)
--
-- Why this is needed: m2e configurator wipes target/generated-sources/annotations
-- on project import. JDT's GeneratedFileManager state thinks impls already exist,
-- so a normal full build skips APT for them. Only update_projects_config resets
-- that state, causing APT to fire for all annotated sources.
--
-- Lifecycle (per client): waiting_ready → settling → refreshing → post_refresh
-- (→ building → post_build when recovering) → done. After the post-refresh
-- build settles, completion-facing caches (blink.cmp LSP source, jdtls
-- classpath cache, MapStruct engine caches) are cleared so freshly generated
-- MapStruct impls resolve without further manual pokes. A failed refresh
-- dispatch is retried a few times; the watcher disposes itself (timer + detach
-- autocmd) when it finishes or when the client stops, so jdtls restarts leave
-- nothing behind and the restarted client gets a fresh watcher.
--
-- Idempotent: marks the client with _patched_workspace_watcher so subsequent
-- calls (e.g. additional buffers attaching) are no-ops. Fires exactly once
-- per client lifetime.

local IDLE_MS = 3000
local POST_REFRESH_FALLBACK_MS = 10 * 1000
local RECOVERY_MARK_TTL_MS = 2 * 60 * 1000
local PROJECTS_TIMEOUT_MS = 10 * 1000
local REFRESH_RETRY_MS = 5 * 1000
local MAX_REFRESH_ATTEMPTS = 3

local log = require("utils.logging-util").new({
    name = "jdtls.status",
    filename = "jdtls-status.log",
    level = vim.log.levels.INFO,
})

local M = {}

local recovery_refresh = {
    until_at = 0,
    reason = nil,
    -- set of root_dirs the marker applies to; nil = any client (legacy fallback)
    roots = nil,
}

--- Mark soon-created JDTLS clients as recovery clients that need a full build.
--- When root_dirs is supplied the marker only applies to clients whose
--- config.root_dir matches, and each root is consumed on first claim so an
--- unrelated workspace starting within the TTL is not dragged into recovery.
---@param reason string|nil
---@param root_dirs string[]|nil
function M.mark_recovery_refresh(reason, root_dirs)
    recovery_refresh.until_at = vim.uv.now() + RECOVERY_MARK_TTL_MS
    recovery_refresh.reason = reason or "recovery"
    if type(root_dirs) == "table" and #root_dirs > 0 then
        recovery_refresh.roots = {}
        for _, root in ipairs(root_dirs) do
            recovery_refresh.roots[root] = true
        end
    else
        recovery_refresh.roots = nil
    end
end

--- Claim the active recovery refresh reason for a client, if the marker matches.
---@param client vim.lsp.Client
---@return string|nil
local function claim_recovery_reason(client)
    if vim.uv.now() > recovery_refresh.until_at then
        return nil
    end

    local roots = recovery_refresh.roots
    if roots then
        local root_dir = client.config and client.config.root_dir
        if not (root_dir and roots[root_dir]) then
            return nil
        end
        -- consume: one recovery build per restarted root
        roots[root_dir] = nil
    end

    return recovery_refresh.reason or "recovery"
end

--- Return a loaded Java buffer attached to the supplied client.
local function client_java_buffer(client, preferred_bufnr)
    if
        preferred_bufnr
        and vim.api.nvim_buf_is_valid(preferred_bufnr)
        and vim.api.nvim_buf_is_loaded(preferred_bufnr)
        and vim.bo[preferred_bufnr].filetype == "java"
        and vim.lsp.buf_is_attached(preferred_bufnr, client.id)
    then
        return preferred_bufnr
    end

    for bufnr in pairs(client.attached_buffers) do
        if
            vim.api.nvim_buf_is_valid(bufnr)
            and vim.api.nvim_buf_is_loaded(bufnr)
            and vim.bo[bufnr].filetype == "java"
        then
            return bufnr
        end
    end

    return nil
end

--- Notify the specific JDTLS client to refresh all imported project configs.
local function update_client_projects_config(client, bufnr, done)
    bufnr = client_java_buffer(client, bufnr)
    if not bufnr then
        log.fmt_warn("client_id=%d has no loaded Java buffer for project config refresh", client.id)
        done(false)
        return
    end

    local responded = false
    local ok, req_id = client:request("workspace/executeCommand", {
        command = "java.project.getAll",
    }, function(err, projects)
        if responded then
            return
        end
        responded = true

        if err then
            log.fmt_warn("client_id=%d java.project.getAll failed: %s", client.id, vim.inspect(err))
            done(false)
            return
        end
        if type(projects) ~= "table" or #projects == 0 then
            log.fmt_warn("client_id=%d java.project.getAll returned no projects", client.id)
            done(false)
            return
        end

        local params = {
            identifiers = vim.tbl_map(function(project)
                return { uri = project }
            end, projects),
        }
        local notify_ok, notify_err = pcall(function()
            client:notify("java/projectConfigurationsUpdate", params)
        end)
        if not notify_ok then
            log.fmt_warn("client_id=%d project config notify failed: %s", client.id, tostring(notify_err))
            done(false)
            return
        end

        log.fmt_info("client_id=%d dispatched project config refresh for %d projects", client.id, #projects)
        done(true)
    end, bufnr)

    if not ok then
        log.fmt_warn("client_id=%d could not request java.project.getAll", client.id)
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
        log.fmt_warn("client_id=%d java.project.getAll timed out after %dms", client.id, PROJECTS_TIMEOUT_MS)
        done(false)
    end, PROJECTS_TIMEOUT_MS)
end

--- Attach the JDTLS workspace watcher patch to the client.
---@param client vim.lsp.Client|nil
---@param bufnr integer|nil
function M.setup(client, bufnr)
    if not client or client.name ~= "jdtls" then
        return
    end
    if client._patched_workspace_watcher then
        return
    end
    ---@diagnostic disable-next-line: inject-field
    client._patched_workspace_watcher = true

    local recovery_reason = claim_recovery_reason(client)
    log.fmt_debug(
        "attached client_id=%d bufnr=%s recovery_reason=%s — installing workspace-settled watcher",
        client.id,
        tostring(bufnr),
        tostring(recovery_reason)
    )

    client.handlers = client.handlers or {}

    -- Lifecycle state; see the header comment for the transitions.
    local state = "waiting_ready"
    local refresh_attempts = 0
    local disposed = false
    local detach_autocmd
    local idle_timer = assert(vim.uv.new_timer())

    local on_idle

    --- Return whether the client can no longer serve requests.
    local function client_gone()
        if client.is_stopped and client:is_stopped() then
            return true
        end
        return client.rpc ~= nil and client.rpc.is_closing()
    end

    --- Tear down the watcher exactly once (timer + detach autocmd).
    local function dispose(why)
        if disposed then
            return
        end
        disposed = true
        state = "done"
        idle_timer:stop()
        idle_timer:close()
        if detach_autocmd then
            pcall(vim.api.nvim_del_autocmd, detach_autocmd)
            detach_autocmd = nil
        end
        log.fmt_debug("client_id=%d watcher disposed (%s)", client.id, why)
    end

    --- Start or restart the workspace-idle debounce timer.
    local function arm_idle_timer(delay_ms)
        if disposed then
            return
        end
        idle_timer:start(delay_ms or IDLE_MS, 0, on_idle)
    end

    --- Clear completion-facing caches so freshly generated APT output
    --- (MapStruct impls etc.) resolves without further manual pokes.
    --- Only caches of already-loaded modules are cleared: this must not pull
    --- the MapStruct engine into projects that never used it.
    local function refresh_completion_sources(why)
        local ok, recovery = pcall(require, "utils.java.jdtls-recovery")
        if ok and recovery.refresh_blink_lsp then
            recovery.refresh_blink_lsp(why)
        else
            log.fmt_warn("client_id=%d could not refresh blink (%s): %s", client.id, why, tostring(recovery))
        end

        local classpath_util = package.loaded["utils.java.jdtls-classpath-util"]
        if classpath_util and classpath_util.clear_cache then
            pcall(classpath_util.clear_cache)
        end

        local mapstruct_context = package.loaded["modules.java.mapstruct.context"]
        if mapstruct_context and mapstruct_context.clear_all_caches then
            pcall(mapstruct_context.clear_all_caches)
        end

        log.fmt_info("client_id=%d refreshed completion sources (%s)", client.id, why)
    end

    --- Finish the watcher: refresh completion sources and tear down.
    local function finalize(why)
        refresh_completion_sources(why)
        dispose("finalized: " .. why)
    end

    --- Dispatch the project config refresh, retrying on transient failures.
    local function dispatch_refresh()
        state = "refreshing"
        refresh_attempts = refresh_attempts + 1
        log.fmt_info(
            "idle %dms after ServiceReady — dispatching project config refresh for client_id=%d (attempt %d)",
            IDLE_MS,
            client.id,
            refresh_attempts
        )
        if refresh_attempts == 1 then
            vim.notify("🏄 JDTLS settled — refreshing project config")
        end
        update_client_projects_config(client, bufnr, function(sent)
            if disposed then
                return
            end

            if sent then
                state = "post_refresh"
                -- m2e re-imports + builds now; wait for that $/progress burst
                -- to settle, with a fallback in case no progress is reported.
                arm_idle_timer(POST_REFRESH_FALLBACK_MS)
                return
            end

            if refresh_attempts < MAX_REFRESH_ATTEMPTS then
                state = "settling"
                log.fmt_warn(
                    "client_id=%d project config refresh failed — retrying in %dms (attempt %d/%d)",
                    client.id,
                    REFRESH_RETRY_MS,
                    refresh_attempts,
                    MAX_REFRESH_ATTEMPTS
                )
                arm_idle_timer(REFRESH_RETRY_MS)
            else
                log.fmt_warn(
                    "client_id=%d project config refresh failed after %d attempts — giving up",
                    client.id,
                    MAX_REFRESH_ATTEMPTS
                )
                dispose("refresh failed after " .. MAX_REFRESH_ATTEMPTS .. " attempts")
            end
        end)
    end

    --- Request a full workspace build on this client (recovery only).
    local function request_full_workspace_build(reason)
        if client_gone() then
            dispose("client stopped before recovery build")
            return
        end

        state = "building"
        log.fmt_info("client_id=%d requesting full workspace build after %s", client.id, reason)
        local ok = client:request("java/buildWorkspace", true, function(err, result)
            if disposed then
                return
            end

            if err then
                log.fmt_warn("client_id=%d recovery build failed: %s", client.id, vim.inspect(err))
            else
                local statuses = {
                    [0] = "FAILED",
                    [1] = "SUCCEEDED",
                    [2] = "WITHERROR",
                    [3] = "CANCELLED",
                }
                log.fmt_info("client_id=%d recovery build result: %s", client.id, statuses[result] or tostring(result))
            end

            state = "post_build"
            arm_idle_timer(IDLE_MS)
        end)
        if not ok then
            log.fmt_warn("client_id=%d could not request full workspace build", client.id)
            finalize("recovery build request failed: " .. reason)
        end
    end

    on_idle = vim.schedule_wrap(function()
        if disposed then
            return
        end
        -- A progress event queued between timer fire and this scheduled
        -- callback re-arms the timer; honor the newer deadline.
        if idle_timer:is_active() then
            return
        end
        if client_gone() then
            dispose("client stopped")
            return
        end

        if state == "settling" then
            dispatch_refresh()
        elseif state == "post_refresh" then
            if recovery_reason then
                request_full_workspace_build(recovery_reason)
            else
                finalize("project config refresh settled")
            end
        elseif state == "post_build" then
            finalize("recovery build settled: " .. tostring(recovery_reason))
        end
    end)

    local prev_status = client.handlers["language/status"] or vim.lsp.handlers["language/status"]
    client.handlers["language/status"] = function(err, result, ctx)
        log.fmt_debug("STATUS type=%s message=%s", result and result.type or "nil", result and result.message or "nil")
        if prev_status then
            prev_status(err, result, ctx)
        end
        if not disposed and state == "waiting_ready" and result and result.type == "ServiceReady" then
            state = "settling"
            log.info("ServiceReady — starting idle debounce") -- INFO #1 (per session)
            arm_idle_timer(IDLE_MS)
        end
    end

    local prev_progress = client.handlers["$/progress"] or vim.lsp.handlers["$/progress"]
    client.handlers["$/progress"] = function(err, result, ctx)
        local val = result and result.value or {}
        log.fmt_debug(
            "PROGRESS kind=%s title=%s message=%s percentage=%s",
            val.kind or "nil",
            val.title or "nil",
            val.message or "nil",
            val.percentage and tostring(val.percentage) or "nil"
        )
        if prev_progress then
            prev_progress(err, result, ctx)
        end
        -- Re-arm the settle debounce only in states waiting for the workspace
        -- to go quiet; progress during an in-flight refresh request must not
        -- trigger the next stage early.
        if not disposed and (state == "settling" or state == "post_refresh" or state == "post_build") then
            arm_idle_timer(IDLE_MS)
        end
    end

    -- jdtls restart handling: when this client stops, close the timer and
    -- remove the autocmd so nothing leaks; the restarted client gets a fresh
    -- watcher via its own LspAttach.
    detach_autocmd = vim.api.nvim_create_autocmd("LspDetach", {
        callback = function(args)
            if args.data.client_id ~= client.id then
                return
            end
            -- Fires per buffer; dispose only when the client itself is going
            -- away, so buffer churn in a live session keeps the watcher alive.
            vim.schedule(function()
                if not disposed and client_gone() then
                    dispose("client stopped (LspDetach)")
                end
            end)
        end,
    })
end

return M
