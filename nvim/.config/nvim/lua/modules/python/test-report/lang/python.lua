-- Python / pytest language adapter for test-report.
--
-- Test ID convention (matches pytest nodeid shape minus the parametrize):
--   <abs_file_path>::<method>            (module-level test function)
--   <abs_file_path>::<Class>::<method>   (class-based test method)
--
-- For parametrized invocations (e.g. test_fizzbuzz[1-1], test_fizzbuzz[3-Fizz])
-- we strip the [..] suffix and aggregate into a single TestResult whose
-- `invocations` array contains one entry per parametrize case.

local junit_xml = require("modules.python.test-report.junit-xml")
local log = require("utils.logging-util").new({
    name = "test-report-python",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

---@type test_report.LangAdapter
local M = {
    group_separator = "/",
    diagnostic_source = "pytest",
    trouble_source = "pytest_test_diagnostics",
}

-- file_path -> { positions, container_line, mtime }
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

--- container_id is the absolute file path (everything before the first `::`).
---@param container_id string
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
local function py_test_query()
    if _query then
        return _query
    end
    _query = vim.treesitter.query.parse(
        "python",
        [[
        (class_definition
          name: (identifier) @cls.name
          (#match? @cls.name "^Test|Test$")
        ) @cls.def

        (function_definition
          name: (identifier) @fn.name
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

    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "python")
    if not ok or not parser then
        log.error("python treesitter parser failed: " .. tostring(parser))
        return positions, nil
    end
    local tree = parser:parse()[1]
    if not tree then
        return positions, nil
    end

    -- Walk the root for module-level function/class definitions to ensure
    -- correct class-method association (queries alone don't give parentage).
    local root = tree:root()
    for child in root:iter_children() do
        local ctype = child:type()
        if ctype == "function_definition" then
            local name_node = child:field("name")[1]
            if name_node then
                local name = vim.treesitter.get_node_text(name_node, bufnr)
                if name:match("^test_?") then
                    local sr = child:range()
                    positions[name] = sr
                    if container_line == nil or sr < container_line then
                        container_line = sr
                    end
                end
            end
        elseif ctype == "decorated_definition" then
            -- Decorated function or class at module level.
            local def = child:field("definition")[1]
            if def then
                local dtype = def:type()
                if dtype == "function_definition" then
                    local name_node = def:field("name")[1]
                    if name_node then
                        local name = vim.treesitter.get_node_text(name_node, bufnr)
                        if name:match("^test_?") then
                            local sr = def:range()
                            positions[name] = sr
                            if container_line == nil or sr < container_line then
                                container_line = sr
                            end
                        end
                    end
                elseif dtype == "class_definition" then
                    local cname_node = def:field("name")[1]
                    if cname_node then
                        local cname = vim.treesitter.get_node_text(cname_node, bufnr)
                        if cname:match("^Test") or cname:match("Test$") then
                            local body = def:field("body")[1]
                            if body then
                                for m in body:iter_children() do
                                    if m:type() == "function_definition" then
                                        local mn = m:field("name")[1]
                                        if mn then
                                            local mname = vim.treesitter.get_node_text(mn, bufnr)
                                            if mname:match("^test_?") then
                                                local sr = m:range()
                                                positions[cname .. "." .. mname] = sr
                                            end
                                        end
                                    elseif m:type() == "decorated_definition" then
                                        local dd = m:field("definition")[1]
                                        if dd and dd:type() == "function_definition" then
                                            local mn = dd:field("name")[1]
                                            if mn then
                                                local mname = vim.treesitter.get_node_text(mn, bufnr)
                                                if mname:match("^test_?") then
                                                    local sr = dd:range()
                                                    positions[cname .. "." .. mname] = sr
                                                end
                                            end
                                        end
                                    end
                                end
                                local csr = def:range()
                                if container_line == nil or csr < container_line then
                                    container_line = csr
                                end
                            end
                        end
                    end
                end
            end
        elseif ctype == "class_definition" then
            local cname_node = child:field("name")[1]
            if cname_node then
                local cname = vim.treesitter.get_node_text(cname_node, bufnr)
                if cname:match("^Test") or cname:match("Test$") then
                    local body = child:field("body")[1]
                    if body then
                        for m in body:iter_children() do
                            if m:type() == "function_definition" then
                                local mn = m:field("name")[1]
                                if mn then
                                    local mname = vim.treesitter.get_node_text(mn, bufnr)
                                    if mname:match("^test_?") then
                                        local sr = m:range()
                                        positions[cname .. "." .. mname] = sr
                                    end
                                end
                            elseif m:type() == "decorated_definition" then
                                local dd = m:field("definition")[1]
                                if dd and dd:type() == "function_definition" then
                                    local mn = dd:field("name")[1]
                                    if mn then
                                        local mname = vim.treesitter.get_node_text(mn, bufnr)
                                        if mname:match("^test_?") then
                                            local sr = dd:range()
                                            positions[cname .. "." .. mname] = sr
                                        end
                                    end
                                end
                            end
                        end
                        local csr = child:range()
                        if container_line == nil or csr < container_line then
                            container_line = csr
                        end
                    end
                end
            end
        end
    end

    positions_cache[file_path] = { positions = positions, container_line = container_line, mtime = mtime }
    return positions, container_line
end

---@param container_id string
---@param stacktrace string
---@return number|nil
function M.extract_error_line(container_id, stacktrace)
    if not stacktrace or stacktrace == "" or not container_id then
        return nil
    end
    local file = container_id
    local basename = vim.fn.fnamemodify(file, ":t")
    -- pytest traceback uses "<file>:<line>: in <name>" format
    local lnum = stacktrace:match(vim.pesc(file) .. ":(%d+)")
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
    local ok, runner = pcall(require, "modules.python.pytest-test")
    if ok and runner.get_test_report_dir then
        return runner.get_test_report_dir()
    end
    local root = vim.fn.getcwd()
    local safe = root:gsub("[/\\]", "_"):gsub("^_+", "")
    return vim.fn.stdpath("cache") .. "/test-report/python/" .. safe
end

---@param id string  "<abs_file_path>#<member>" (member = "Class.method" or "test_fn")
---@return test_report.IdDisplay
function M.id_to_display(id)
    local file, member = id:match("^(.+)#(.+)$")
    if not file then
        return { container = id, member = "", group = nil }
    end

    local project_root
    local ok, runner = pcall(require, "modules.python.pytest-test")
    if ok and runner.project_root then
        project_root = runner.project_root()
    end
    local container_name = vim.fn.fnamemodify(file, ":t")
    local dir = vim.fn.fnamemodify(file, ":h")
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
