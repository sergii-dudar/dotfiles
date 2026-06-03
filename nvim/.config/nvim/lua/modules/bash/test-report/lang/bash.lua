-- Bash / bashunit language adapter for test-report.
--
-- Test ID convention:
--   <abs_file_path>#<test_function_name>
-- The container is the absolute file path; member is the bash function name
-- (e.g. "test_equals_and_not_equals"). bashunit's XML uses humanized names —
-- junit-xml.lua maps them back via treesitter discovery of the source file.

local junit_xml = require("modules.bash.test-report.junit-xml")
local log = require("utils.logging-util").new({
    name = "test-report-bash",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

---@type test_report.LangAdapter
local M = {
    group_separator = "/",
    diagnostic_source = "bashunit",
    trouble_source = "bashunit_test_diagnostics",
}

-- file_path -> { positions = { [fn_name] = 0-indexed line }, mtime = number }
local positions_cache = {}

function M.clear_cache()
    positions_cache = {}
    junit_xml.clear_cache()
end

---@param dirs string[]
---@return table<string, test_report.TestResult>
function M.parse_results(dirs)
    local results = {}
    for _, dir in ipairs(dirs) do
        for _, filepath in ipairs(junit_xml.list_report_files(dir)) do
            for id, r in pairs(junit_xml.parse_file(filepath, dir)) do
                results[id] = r
            end
        end
    end
    return results
end

---@param container_id string  absolute file path
---@param report_dir string
---@return string|nil
function M.id_to_file(container_id, report_dir)
    if not container_id then
        return nil
    end
    if vim.fn.filereadable(container_id) == 1 then
        return container_id
    end
    return nil
end

local _query
local function bash_test_query()
    if _query then
        return _query
    end
    _query = vim.treesitter.query.parse(
        "bash",
        [[
        (function_definition
          name: (word) @fn.name
          (#match? @fn.name "^test_?")
        ) @fn.def
    ]]
    )
    return _query
end

---@param file_path string
---@param opts? test_report.FindOpts
---@return table<string, number> positions
---@return number|nil container_line
function M.find_test_positions(file_path, opts)
    file_path = vim.fn.fnamemodify(file_path, ":p")
    local mtime = vim.fn.getftime(file_path)
    local cached = positions_cache[file_path]
    if cached and cached.mtime == mtime then
        return cached.positions, cached.container_line
    end
    local positions = {}
    local container_line

    local silent = not opts or opts.silent ~= false
    local bufnr = vim.fn.bufadd(file_path)
    if not vim.api.nvim_buf_is_loaded(bufnr) then
        if silent then
            pcall(vim.cmd, "noautocmd call bufload(" .. bufnr .. ")")
        else
            vim.fn.bufload(bufnr)
        end
    end

    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "bash")
    if not ok or not parser then
        log.error("bash treesitter parser failed: " .. tostring(parser))
        return positions, nil
    end
    local tree = parser:parse()[1]
    if not tree then
        return positions, nil
    end
    local query = bash_test_query()
    for id, node in query:iter_captures(tree:root(), bufnr) do
        if query.captures[id] == "fn.def" then
            local sr, _, _, _ = node:range()
            if container_line == nil or sr < container_line then
                container_line = sr
            end
            -- find the name child
            for child in node:iter_children() do
                if child:type() == "word" then
                    local fn = vim.treesitter.get_node_text(child, bufnr)
                    positions[fn] = sr
                    break
                end
            end
        end
    end

    positions_cache[file_path] = { positions = positions, container_line = container_line, mtime = mtime }
    return positions, container_line
end

---@param container_id string  absolute file path
---@param stacktrace string
---@return number|nil
function M.extract_error_line(container_id, stacktrace)
    if not stacktrace or stacktrace == "" or not container_id then
        return nil
    end
    local basename = vim.fn.fnamemodify(container_id, ":t")
    local lnum = stacktrace:match(vim.pesc(container_id) .. ":(%d+)")
    if lnum then
        return tonumber(lnum)
    end
    lnum = stacktrace:match(vim.pesc(basename) .. ":(%d+)")
    if lnum then
        return tonumber(lnum)
    end
    return nil
end

---@return string
function M.get_test_report_dir()
    local ok, runner = pcall(require, "modules.bash.bashunit-test")
    if ok and runner.get_test_report_dir then
        return runner.get_test_report_dir()
    end
    local root = vim.fn.getcwd()
    local safe = root:gsub("[/\\]", "_"):gsub("^_+", "")
    return vim.fn.stdpath("cache") .. "/test-report/bash/" .. safe
end

---@param id string  "<abs_file_path>#<fn_name>"
---@return test_report.IdDisplay
function M.id_to_display(id)
    local container_id, member = id:match("^(.+)#(.+)$")
    if not container_id then
        return { container = id, member = "", group = nil }
    end
    local project_root
    local ok, runner = pcall(require, "modules.bash.bashunit-test")
    if ok and runner.project_root then
        project_root = runner.project_root()
    end
    local container_name = vim.fn.fnamemodify(container_id, ":t")
    local dir = vim.fn.fnamemodify(container_id, ":h")
    local group
    if project_root and vim.startswith(dir, project_root) then
        group = dir:sub(#project_root + 2)
        if group == "" then
            group = nil
        end
    else
        group = dir
    end
    return { container = container_name, member = member, group = group }
end

return M
