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
--   full build after recovery
--
-- Why this is needed: m2e configurator wipes target/generated-sources/annotations
-- on project import. JDT's GeneratedFileManager state thinks impls already exist,
-- so a normal full build skips APT for them. Only update_projects_config resets
-- that state, causing APT to fire for all annotated sources.
--
-- Idempotent: marks the client with _patched_workspace_watcher so subsequent
-- calls (e.g. additional buffers attaching) are no-ops. Fires exactly once
-- per client lifetime.

local IDLE_MS = 3000
local RECOVERY_BUILD_FALLBACK_MS = 10 * 1000
local RECOVERY_MARK_TTL_MS = 2 * 60 * 1000
local PROJECTS_TIMEOUT_MS = 10 * 1000

local log = require("utils.logging-util").new({
    name = "jdtls.status",
    filename = "jdtls-status.log",
    level = vim.log.levels.INFO,
})

local M = {}

local recovery_refresh = {
    until_at = 0,
    reason = nil,
}

--- Mark soon-created JDTLS clients as recovery clients that need a full build.
function M.mark_recovery_refresh(reason)
    recovery_refresh.until_at = vim.uv.now() + RECOVERY_MARK_TTL_MS
    recovery_refresh.reason = reason or "recovery"
end

--- Return the active recovery refresh reason if the marker is still valid.
local function current_recovery_reason()
    if vim.uv.now() <= recovery_refresh.until_at then
        return recovery_refresh.reason or "recovery"
    end
    return nil
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

    for _, bufnr in ipairs(vim.lsp.get_buffers_by_client_id(client.id)) do
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
            if vim.fn.has("nvim-0.11") == 1 then
                client:notify("java/projectConfigurationsUpdate", params)
            else
                client.notify("java/projectConfigurationsUpdate", params)
            end
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

--- Refresh blink.cmp after JDTLS recovery has finished its workspace build.
local function refresh_blink_after_recovery_build(client, reason)
    vim.defer_fn(function()
        if client.rpc and client.rpc.is_closing() then
            return
        end

        local ok, recovery = pcall(require, "utils.java.jdtls-recovery")
        if not ok or not recovery.refresh_blink_lsp then
            log.fmt_warn("client_id=%d could not refresh blink after recovery build: %s", client.id, tostring(recovery))
            return
        end

        recovery.refresh_blink_lsp("recovery build complete: " .. reason)
        log.fmt_info("client_id=%d refreshed blink after recovery build (%s)", client.id, reason)
    end, IDLE_MS)
end

--- Request a full workspace build on the specific JDTLS client.
local function request_full_workspace_build(client, reason)
    if client.rpc and client.rpc.is_closing() then
        log.fmt_warn("client_id=%d skipped recovery build because RPC is closing", client.id)
        return
    end

    log.fmt_info("client_id=%d requesting full workspace build after %s", client.id, reason)
    local ok = client:request("java/buildWorkspace", true, function(err, result)
        if err then
            log.fmt_warn("client_id=%d recovery build failed: %s", client.id, vim.inspect(err))
            return
        end

        local statuses = {
            [0] = "FAILED",
            [1] = "SUCCEEDED",
            [2] = "WITHERROR",
            [3] = "CANCELLED",
        }
        log.fmt_info("client_id=%d recovery build result: %s", client.id, statuses[result] or tostring(result))
        refresh_blink_after_recovery_build(client, reason)
    end)
    if not ok then
        log.fmt_warn("client_id=%d could not request full workspace build", client.id)
    end
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
    client._patched_workspace_watcher = true

    local recovery_reason = current_recovery_reason()
    log.fmt_debug(
        "attached client_id=%d bufnr=%s recovery_reason=%s — installing workspace-settled watcher",
        client.id,
        tostring(bufnr),
        tostring(recovery_reason)
    )

    client.handlers = client.handlers or {}

    local service_ready = false
    local config_refresh_sent = false
    local recovery_build_sent = false
    local timer_closed = false
    local idle_timer = vim.uv.new_timer()

    local on_idle

    --- Close the idle timer exactly once.
    local function close_idle_timer()
        if timer_closed then
            return
        end
        idle_timer:stop()
        idle_timer:close()
        timer_closed = true
    end

    --- Start or restart the workspace-idle debounce timer.
    local function arm_idle_timer(delay_ms)
        if timer_closed or recovery_build_sent then
            return
        end
        if config_refresh_sent and not recovery_reason then
            return
        end
        idle_timer:start(delay_ms or IDLE_MS, 0, on_idle)
    end

    on_idle = vim.schedule_wrap(function()
        if not config_refresh_sent then
            config_refresh_sent = true
            log.fmt_info(
                "idle %dms after ServiceReady — dispatching project config refresh for client_id=%d",
                IDLE_MS,
                client.id
            )
            vim.notify("🏄 JDTLS settled — refreshing project config")
            update_client_projects_config(client, bufnr, function(sent)
                if recovery_reason and sent then
                    log.fmt_info(
                        "client_id=%d scheduled recovery full build after project config refresh (%s)",
                        client.id,
                        recovery_reason
                    )
                    arm_idle_timer(RECOVERY_BUILD_FALLBACK_MS)
                else
                    close_idle_timer()
                end
            end)
            return
        end

        if recovery_reason and not recovery_build_sent then
            recovery_build_sent = true
            close_idle_timer()
            request_full_workspace_build(client, recovery_reason)
        end
    end)

    local prev_status = client.handlers["language/status"] or vim.lsp.handlers["language/status"]
    client.handlers["language/status"] = function(err, result, ctx)
        log.fmt_debug("STATUS type=%s message=%s", result and result.type or "nil", result and result.message or "nil")
        if prev_status then
            prev_status(err, result, ctx)
        end
        if not service_ready and result and result.type == "ServiceReady" then
            service_ready = true
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
        if service_ready and not recovery_build_sent then
            arm_idle_timer(IDLE_MS)
        end
    end
end

return M
