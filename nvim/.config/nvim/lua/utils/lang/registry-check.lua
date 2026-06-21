-- Validates `utils.lang.registry` metadata: missing fields, duplicate
-- filetypes, runner/report modules that fail to load, and missing report
-- component files.
--
-- - check - collect registry validation errors
-- - run - print/notify/raise the validation report
--
-- In-editor:
--   :LangRegistryCheck     -- prints + notifies; exit on error: no
--   :LangRegistryCheck!    -- raises (errors out); useful for `:source`-time checks
--
-- Headless (run from a separate terminal while editing in the main one;
-- the two nvim instances are independent processes):
--
--   nvim --headless "+lua require('utils.lang.registry-check').run({ raise = true })" +qa
--
-- With `raise = true`: errors go to stderr with a stacktrace and the process
-- exits non-zero (CI-friendly). Drop `raise = true` for a stdout-only report
-- with exit code 0.
--
-- Continuous re-check on file save (uses `entr`):
--
--   fd -e lua . lua/utils/lang lua/plugins/overseer/tasks lua/modules \
--     | entr -c nvim --headless \
--       "+lua require('utils.lang.registry-check').run({ raise = true })" +qa

local registry = require("utils.lang.registry")

local M = {}

--- Append one validation failure to the collected error list.
---@param errors string[]
---@param msg string
local function add_error(errors, msg)
    table.insert(errors, msg)
end

--- Check whether a metadata list is a non-empty table.
---@param values string[]|nil
---@return boolean
local function has_values(values)
    return type(values) == "table" and #values > 0
end

--- Check whether an Overseer component module file exists on runtimepath.
---@param path string
---@return boolean
local function component_exists(path)
    local rel = "lua/overseer/component/" .. path:gsub("%.", "/") .. ".lua"
    return #vim.api.nvim_get_runtime_file(rel, false) > 0
end

--- Validate every language registry entry.
--- Checks required metadata, duplicate filetypes, loadable runner/report modules,
--- and report component files without raising; callers receive all failures.
---@return boolean ok
---@return string[] errors
function M.check()
    local errors = {}
    local names = {}
    local runner_filetypes = {}
    local report_filetypes = {}

    for idx, entry in ipairs(registry.all()) do
        local label = entry.name or ("entry #" .. idx)

        if not entry.name or entry.name == "" then
            add_error(errors, "registry entry #" .. idx .. " is missing name")
        elseif names[entry.name] then
            add_error(errors, "duplicate language name: " .. entry.name)
        else
            names[entry.name] = true
        end

        if entry.primary then
            if not entry.project then
                add_error(errors, label .. ": primary entry has no project metadata")
            else
                if not has_values(entry.project.markers) then
                    add_error(errors, label .. ": primary project has no markers")
                end
                if type(entry.project.exts) ~= "table" or vim.tbl_isempty(entry.project.exts) then
                    add_error(errors, label .. ": primary project has no source extensions")
                end
            end
        end

        local runner = entry.runner
        if runner then
            if not has_values(runner.filetypes) then
                add_error(errors, label .. ": runner has no filetypes")
            end
            local ok, err = pcall(require, runner.module)
            if not ok then
                add_error(
                    errors,
                    label .. ": runner module failed to load: " .. runner.module .. " (" .. tostring(err) .. ")"
                )
            end
            for _, ft in ipairs(runner.filetypes or {}) do
                if runner_filetypes[ft] then
                    add_error(errors, label .. ": runner filetype duplicates " .. runner_filetypes[ft] .. ": " .. ft)
                end
                runner_filetypes[ft] = label
            end
        end

        local report = entry.report
        if report then
            if not has_values(report.filetypes) then
                add_error(errors, label .. ": report has no filetypes")
            end
            if not report.module or report.module == "" then
                add_error(errors, label .. ": report has no module")
            else
                local ok, err = pcall(require, report.module)
                if not ok then
                    add_error(
                        errors,
                        label .. ": report module failed to load: " .. report.module .. " (" .. tostring(err) .. ")"
                    )
                end
            end
            if not report.component or report.component == "" then
                add_error(errors, label .. ": report has no component")
            elseif not component_exists(report.component) then
                add_error(errors, label .. ": report component file not found: " .. report.component)
            end
            if not report.trouble_source or report.trouble_source == "" then
                add_error(errors, label .. ": report has no trouble_source")
            end
            if not report.diagnostic_source or report.diagnostic_source == "" then
                add_error(errors, label .. ": report has no diagnostic_source")
            end
            for _, ft in ipairs(report.filetypes or {}) do
                if report_filetypes[ft] then
                    add_error(errors, label .. ": report filetype duplicates " .. report_filetypes[ft] .. ": " .. ft)
                end
                report_filetypes[ft] = label
            end
        end
    end

    return #errors == 0, errors
end

--- Run the language registry validation report.
--- Prints, notifies, or raises based on options so the same checker can be used
--- interactively and from headless/CI-style Neovim commands.
---@param opts? { raise?: boolean, notify?: boolean }
---@return boolean ok
function M.run(opts)
    opts = opts or {}
    local ok, errors = M.check()
    if ok then
        local msg = "Language registry OK"
        if opts.notify then
            vim.notify(msg, vim.log.levels.INFO)
        else
            print(msg)
        end
        return true
    end

    local msg = "Language registry errors:\n" .. table.concat(errors, "\n")
    if opts.raise then
        error(msg)
    end
    if opts.notify then
        vim.notify(msg, vim.log.levels.ERROR)
    else
        print(msg)
    end
    return false
end

return M
