local registry = require("utils.lang.registry")

local M = {}

---@param errors string[]
---@param msg string
local function add_error(errors, msg)
    table.insert(errors, msg)
end

---@param values string[]|nil
---@return boolean
local function has_values(values)
    return type(values) == "table" and #values > 0
end

---@param path string
---@return boolean
local function component_exists(path)
    local rel = "lua/overseer/component/" .. path:gsub("%.", "/") .. ".lua"
    return #vim.api.nvim_get_runtime_file(rel, false) > 0
end

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
            if runner.enabled ~= false then
                local ok, err = pcall(require, runner.module)
                if not ok then
                    add_error(
                        errors,
                        label .. ": runner module failed to load: " .. runner.module .. " (" .. tostring(err) .. ")"
                    )
                end
                for _, ft in ipairs(runner.filetypes or {}) do
                    if runner_filetypes[ft] then
                        add_error(
                            errors,
                            label .. ": runner filetype duplicates " .. runner_filetypes[ft] .. ": " .. ft
                        )
                    end
                    runner_filetypes[ft] = label
                end
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
