-- Patches a JDTLS client so that ~3s after the workspace settles
-- (no $/progress events for IDLE_MS following ServiceReady), it dispatches
-- update_projects_config to force m2e to re-run APT processors (MapStruct etc.).
--
-- • setup — patch a JDTLS client with the workspace watcher (idempotent)
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

local log = require("utils.logging-util").new({
    name = "jdtls.status",
    filename = "jdtls-status.log",
    level = vim.log.levels.INFO,
})

local M = {}

--- Attach the JDTLS workspace watcher patch to the client.
function M.setup(client)
    if not client or client.name ~= "jdtls" then
        return
    end
    if client._patched_workspace_watcher then
        return
    end
    client._patched_workspace_watcher = true

    log.fmt_debug("attached client_id=%d — installing workspace-settled watcher", client.id)

    client.handlers = client.handlers or {}

    local service_ready = false
    local fired = false
    local idle_timer = vim.uv.new_timer()

    local on_idle = vim.schedule_wrap(function()
        if fired then
            return
        end
        fired = true
        idle_timer:stop()
        idle_timer:close()
        log.fmt_info("idle %dms after ServiceReady — dispatching update_projects_config", IDLE_MS) -- INFO #2 (per session)
        vim.notify("🏄 JDTLS settled — refreshing project config")
        require("jdtls").update_projects_config({ select_mode = "all" })
    end)

    local function arm_idle_timer()
        if fired then
            return
        end
        idle_timer:start(IDLE_MS, 0, on_idle)
    end

    local prev_status = client.handlers["language/status"] or vim.lsp.handlers["language/status"]
    client.handlers["language/status"] = function(err, result, ctx)
        log.fmt_debug("STATUS type=%s message=%s", result and result.type or "nil", result and result.message or "nil")
        if prev_status then
            prev_status(err, result, ctx)
        end
        if not service_ready and result and result.type == "ServiceReady" then
            service_ready = true
            log.info("ServiceReady — starting idle debounce") -- INFO #1 (per session)
            arm_idle_timer()
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
        if service_ready and not fired then
            arm_idle_timer()
        end
    end
end

return M
