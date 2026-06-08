-- C# language adapter for test-report.
-- Provides:
--   * parse_results: delegates to trx-xml.lua (parses `dotnet test` TRX reports)
--   * id_to_file: maps "<class_fqn>" container IDs to source files via a per-root
--                 treesitter index of test classes.
--   * find_test_positions: treesitter scan of a file for test methods + class line.
--   * extract_error_line: regex over the .NET stacktrace ("...:line N").
--   * id_to_display: splits "<namespace>.<class>#<method>" -> { group, container, member }
--
-- Test ID convention (synced with trx-xml):
--   <class_fqn>#<method_name>   e.g. "CsXunitTests.ParameterizedTests#InlineData_IsEven"
-- where class_fqn is "<namespace>.<Outer>+<Inner>" (TRX className form).

local trx_xml = require("modules.cs.test-report.trx-xml")
local cs_ts = require("modules.cs.cs-ts-util")
local log = require("utils.logging-util").new({
    name = "test-report-cs",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

---@type test_report.LangAdapter
local M = {
    group_separator = ".",
    diagnostic_source = "dotnet-test",
    trouble_source = "dotnet_test_diagnostics",
}

-- root -> { container_files = { [class_fqn] = file }, built_at = hrtime }
local index_cache = {}

function M.clear_cache()
    index_cache = {}
end

---@param dirs string[]
---@return table<string, test_report.TestResult>
function M.parse_results(dirs)
    return trx_xml.parse_results(dirs)
end

--- Derive the run root from a report dir like ".../TestResults/nvim".
---@param report_dir string|nil
---@return string|nil
local function root_from_report_dir(report_dir)
    if not report_dir then
        return nil
    end
    local root = report_dir:match("^(.+)/TestResults/[^/]+/?$")
    if root then
        return root
    end
    return report_dir:match("^(.+)/TestResults/?$")
end

---@param report_dir string|nil
---@return string
local function resolve_root(report_dir)
    return root_from_report_dir(report_dir) or vim.fn.getcwd()
end

--- Build (or reuse) the class_fqn -> file index for a root by scanning .cs files.
---@param root string
---@return { container_files: table<string, string> }
local function build_index(root)
    local cached = index_cache[root]
    if cached then
        return cached
    end

    local t0 = vim.uv.hrtime()
    local container_files = {}
    local files = vim.fn.glob(root .. "/**/*.cs", false, true)
    for _, file_path in ipairs(files) do
        if not file_path:match("/bin/") and not file_path:match("/obj/") then
            for _, cls in ipairs(cs_ts.test_classes(file_path)) do
                container_files[cls.fqn] = vim.fn.fnamemodify(file_path, ":p")
            end
        end
    end

    log.info(
        string.format(
            "[perf cs_index] %s containers=%d files=%d build=%.1fms",
            root,
            vim.tbl_count(container_files),
            #files,
            (vim.uv.hrtime() - t0) / 1e6
        )
    )

    cached = { container_files = container_files, built_at = vim.uv.hrtime() }
    index_cache[root] = cached
    return cached
end

---@param container_id string
---@param report_dir string
---@return string|nil
function M.id_to_file(container_id, report_dir)
    local root = resolve_root(report_dir)
    local idx = build_index(root)
    return idx.container_files[container_id]
end

---@param file_path string
---@param opts? test_report.FindOpts
---@return table<string, number> positions, number|nil container_line
function M.find_test_positions(file_path, opts)
    file_path = vim.fn.fnamemodify(file_path, ":p")
    local bufnr = vim.fn.bufnr(file_path)
    if bufnr ~= -1 and vim.api.nvim_buf_is_loaded(bufnr) then
        return cs_ts.positions(bufnr)
    end
    return cs_ts.positions(file_path)
end

---@param container_id string
---@param stacktrace string
---@return number|nil
function M.extract_error_line(container_id, stacktrace)
    if not stacktrace or stacktrace == "" then
        return nil
    end
    local lnum = stacktrace:match(":line (%d+)")
    return lnum and tonumber(lnum) or nil
end

---@return string
function M.get_test_report_dir()
    return vim.fn.getcwd() .. "/TestResults/nvim"
end

---@param id string  "<namespace>.<class>#<method>"
---@return test_report.IdDisplay
function M.id_to_display(id)
    local container_id, member = id:match("^(.+)#(.+)$")
    if not container_id then
        return { container = id, member = "", group = nil }
    end
    -- container_id = "<namespace>.<class>" (class may be "Outer+Inner").
    local group, container_name = container_id:match("^(.-)%.([^%.]+)$")
    if not container_name then
        return { container = container_id, member = member, group = nil }
    end
    return { container = container_name, member = member, group = group }
end

return M
