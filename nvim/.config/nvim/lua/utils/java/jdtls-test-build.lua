-- Build barrier in front of a Java test run (`prepare_test_context` of the Java runner).
--
-- The JUnit runner executes the class files jdtls compiled into the module's output folders
-- (target/classes, target/test-classes) and never compiles itself. Two ways those are not what
-- the sources say:
--   • wiped outside jdtls: `mvn clean compile` leaves no target/test-classes, jdtls' saved build
--     state still says "built", so neither its startup build nor an incremental one compiles the
--     tests again and JUnit finds 0 tests (seen 2026-11-17). Only a full build recovers this.
--   • not built yet: the autobuild of the last save is still running or queued.
-- So right before the tests start: a full `java/buildWorkspace` when a class output folder of a
-- workspace project is missing or empty while its source folder holds Java sources, else an
-- incremental one (answers at once when nothing is pending, waits for a running build otherwise).
-- Deliberately not done at nvim/jdtls start: most sessions never run a test.
local M = {}

local nio = require("nio")
local spinner = require("utils.ui.spinner")
local log = require("utils.logging-util").new({
    name = "jdtls-test-build",
    filename = "jdtls-status.log",
    level = vim.log.levels.DEBUG,
})

M.config = {
    -- false = run the tests on whatever class files exist, as before this module
    enabled = true,
    -- incremental barrier when no output folder is missing; false = only the full-build recovery
    incremental = true,
    -- a jdtls that does not answer (hung, stopped mid-request) must not hold the tests back forever
    incremental_timeout_ms = 30 * 1000,
    full_timeout_ms = 10 * 60 * 1000,
    projects_timeout_ms = 5 * 1000,
}

-- org.eclipse.jdt.ls.core.internal.BuildWorkspaceStatus
local BUILD_STATUS = { [0] = "FAILED", [1] = "SUCCEED", [2] = "WITHERROR", [3] = "CANCELLED" }

local sp = { id = "jdtls_test_build", title = "JDTLS" }

-- Output folders that were still empty after a full build of this session (sources that
-- produce no class file, a module jdtls cannot build): not a reason for another full build
-- in front of every later test run.
local unrecoverable = {}

--- First source folder of a project's `.classpath` that holds Java sources while its class
--- output folder (the entry's `output`, else the default `kind="output"` entry) is missing or
--- empty. Entries that exclude everything (resource folders) are skipped.
---@param root string module directory
---@param skip? table<string, boolean> output folders (absolute) not to report
---@return string|nil output absolute path of the missing output folder
function M.missing_class_output_dir(root, skip)
    local file = io.open(root .. "/.classpath", "r")
    if not file then
        return nil
    end
    local content = file:read("*a")
    file:close()

    local default_output
    local sources = {}
    for entry in content:gmatch("<classpathentry[^>]->") do
        local kind = entry:match('kind="([^"]*)"')
        local path = entry:match('[^%w]path="([^"]*)"')
        if kind == "output" then
            default_output = path
        elseif kind == "src" and path and entry:match('excluding="([^"]*)"') ~= "**" then
            table.insert(sources, { path = path, output = entry:match('output="([^"]*)"') })
        end
    end

    local filled = {} -- output -> true once seen non-empty
    for _, src in ipairs(sources) do
        local output = src.output or default_output
        -- Absolute paths are other projects / linked folders: not ours to judge.
        if output and output:sub(1, 1) ~= "/" and src.path:sub(1, 1) ~= "/" and not filled[output] then
            local out_dir = root .. "/" .. output
            local handle = vim.uv.fs_scandir(out_dir)
            if handle ~= nil and vim.uv.fs_scandir_next(handle) ~= nil then
                filled[output] = true
            else
                local has_java = vim.fs.find(function(name)
                    return name:sub(-5) == ".java"
                end, { path = root .. "/" .. src.path, type = "file", limit = 1 })[1]
                if has_java and not (skip and skip[out_dir]) then
                    return out_dir
                end
            end
        end
    end
    return nil
end

--- LSP request for a nio task: answers `err, result`, with err = "timeout" when the server
--- did not answer in `timeout_ms` (the request is then canceled; a late answer is ignored).
---@type fun(client: vim.lsp.Client, method: string, params: any, timeout_ms: integer): any, any
local request = nio.wrap(function(client, method, params, timeout_ms, cb)
    local done = false
    local timer = vim.uv.new_timer()
    local function finish(err, result)
        if done then
            return
        end
        done = true
        if timer then
            timer:stop()
            timer:close()
        end
        cb(err, result)
    end
    local ok, request_id = client:request(method, params, finish)
    if not ok then
        finish("request not sent", nil)
        return
    end
    if timer then
        timer:start(
            timeout_ms,
            0,
            vim.schedule_wrap(function()
                if not done and request_id then
                    pcall(client.cancel_request, client, request_id)
                end
                finish("timeout", nil)
            end)
        )
    end
end, 5)

--- Module directories of the client's workspace projects (java.project.getAll), the client
--- root when they cannot be listed.
---@param client vim.lsp.Client
---@return string[]
local function project_roots(client)
    local roots = {}
    local err, uris =
        request(client, "workspace/executeCommand", { command = "java.project.getAll" }, M.config.projects_timeout_ms)
    if not err and type(uris) == "table" then
        for _, uri in ipairs(uris) do
            local path = vim.uri_to_fname(uri):gsub("/$", "")
            table.insert(roots, path)
        end
    end
    if vim.tbl_isempty(roots) and client.config and client.config.root_dir then
        table.insert(roots, client.config.root_dir)
    end
    return roots
end

--- Make the class files match the sources before a test run. Must run in a nio task. Never
--- blocks the run: without jdtls, or when the build fails, the tests start as they did before
--- and the runner reports what is missing.
---@return boolean ok, string|nil err `prepare_test_context` result
function M.ensure_built()
    if not M.config.enabled then
        return true
    end
    local client = vim.lsp.get_clients({ name = "jdtls", bufnr = 0 })[1] or vim.lsp.get_clients({ name = "jdtls" })[1]
    if not client then
        return true
    end

    local missing
    for _, root in ipairs(project_roots(client)) do
        missing = M.missing_class_output_dir(root, unrecoverable)
        if missing then
            break
        end
    end
    if not missing and not M.config.incremental then
        return true
    end

    local full = missing ~= nil
    local started = vim.uv.hrtime()
    if full then
        log.fmt_info("class output folder missing: %s, full build before the tests", missing)
        spinner.start("Compiling: " .. vim.fn.fnamemodify(missing, ":~:.") .. " is missing…", sp)
    end

    local timeout_ms = full and M.config.full_timeout_ms or M.config.incremental_timeout_ms
    local err, status = request(client, "java/buildWorkspace", full, timeout_ms)
    nio.scheduler()
    local status_name = BUILD_STATUS[status] or tostring(status)
    log.fmt_info(
        "%s build before the tests: %s%s in %dms",
        full and "full" or "incremental",
        status_name,
        err and (" (" .. vim.inspect(err) .. ")") or "",
        (vim.uv.hrtime() - started) / 1e6
    )

    if err or status ~= 1 then
        local msg = err == "timeout" and "Build before the tests did not finish in time"
            or err and "Build before the tests failed"
            or ("Build before the tests: " .. status_name)
        if full then
            spinner.stop(false, msg, sp)
        else
            vim.notify("JDTLS: " .. msg .. ", running the tests anyway", vim.log.levels.WARN)
        end
    elseif full then
        spinner.stop(true, "Compiled", sp)
    end
    if full and err ~= "timeout" then
        local handle = vim.uv.fs_scandir(missing)
        if handle == nil or vim.uv.fs_scandir_next(handle) == nil then
            unrecoverable[missing] = true
            log.fmt_warn("%s is still empty after a full build, not retried in this session", missing)
        end
    end
    return true
end

return M
