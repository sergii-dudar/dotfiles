-- JavaScript / TypeScript (jest) language adapter for test-report.
--
-- Member identity is line-based: the jest report gives an authoritative
-- `location.line` per test, and treesitter gives the source line of every
-- `test`/`it` call. We key members by their 0-indexed source row ("L<row>")
-- so the two always align — including parametrized `test.each` where every
-- expansion shares one source line (aggregated into one member's invocations).
--
-- Container = the test FILE (id is `<abs_file>#L<row>`). The display name and
-- describe-chain group are taken from the jest report and cached per id.

local jest_json = require("modules.js.test-report.jest-json")
local log = require("utils.logging-util").new({
    name = "test-report-js",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

---@type test_report.LangAdapter
local M = {
    group_separator = "/",
    diagnostic_source = "jest",
    trouble_source = "jest_test_diagnostics",
}

-- file_path -> { positions = {member -> row0}, container_line, mtime }
local positions_cache = {}
-- id -> test_report.IdDisplay
local display_cache = {}

function M.clear_cache()
    positions_cache = {}
    display_cache = {}
end

--------------------------------------------------------------------------------
-- Treesitter discovery of every test/it call in a file, keyed by source line.

local TEST_IDENTS = { test = true, it = true, fit = true, xit = true }
local NAMESPACE_IDENTS = { describe = true, fdescribe = true, xdescribe = true }

---@param node TSNode callee (`function` field)
---@return string|nil
local function callee_root_ident(node, bufnr)
    local t = node:type()
    if t == "identifier" then
        return vim.treesitter.get_node_text(node, bufnr)
    elseif t == "member_expression" then
        local obj = node:field("object")[1]
        return obj and callee_root_ident(obj, bufnr) or nil
    elseif t == "call_expression" then
        local fn = node:field("function")[1]
        return fn and callee_root_ident(fn, bufnr) or nil
    end
    return nil
end

--- Return the 0-indexed start row of the first string/template argument of a
--- test/describe call (where jest reports the test location), or nil.
---@param call TSNode
---@param bufnr integer
---@return integer|nil
local function name_arg_row(call, bufnr)
    local args = call:field("arguments")[1]
    if not args then
        return nil
    end
    for arg in args:iter_children() do
        if arg:named() then
            local at = arg:type()
            if at == "comment" then
                -- skip leading comments
            elseif at == "string" or at == "template_string" then
                -- jest's `location.line` is the line of the call's `(`
                -- (the arguments node), not the name string itself — these
                -- differ for the multiline tagged-template `.each` form.
                local sr = args:range()
                return sr
            else
                -- first real argument is not a name string (e.g. the array of
                -- a `.each` inner call) -> not the name-bearing call
                return nil
            end
        end
    end
    return nil
end

---@param file_path string
---@return string|nil lang
local function ts_lang_for(file_path)
    local ext = file_path:match("%.([%w]+)$")
    if ext == "ts" then
        return "typescript"
    elseif ext == "tsx" then
        return "tsx"
    elseif ext == "jsx" or ext == "js" or ext == "cjs" or ext == "mjs" then
        return "javascript"
    end
    return "javascript"
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

    local lang = ts_lang_for(file_path)
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, lang)
    if not ok or not parser then
        log.error("js treesitter parser failed for " .. file_path .. ": " .. tostring(parser))
        positions_cache[file_path] = { positions = positions, container_line = nil, mtime = mtime }
        return positions, nil
    end
    local tree = parser:parse()[1]
    if not tree then
        return positions, nil
    end

    -- Walk all call_expression nodes; record leaf test/it calls and track the
    -- topmost `describe` so the aggregated container sign lands on the
    -- top-level describe line (NOT the first test, which would duplicate that
    -- test's own sign). Falls back to nil when the file has no describe.
    local describe_line
    local function walk(node)
        for child in node:iter_children() do
            if child:type() == "call_expression" then
                local fn = child:field("function")[1]
                local ident = fn and callee_root_ident(fn, bufnr) or nil
                if ident and TEST_IDENTS[ident] then
                    local row0 = name_arg_row(child, bufnr)
                    if row0 then
                        positions["L" .. row0] = row0
                    end
                elseif ident and NAMESPACE_IDENTS[ident] then
                    local sr = child:range()
                    if describe_line == nil or sr < describe_line then
                        describe_line = sr
                    end
                end
            end
            walk(child)
        end
    end
    walk(tree:root())
    container_line = describe_line

    positions_cache[file_path] = { positions = positions, container_line = container_line, mtime = mtime }
    return positions, container_line
end

--------------------------------------------------------------------------------
-- Report parsing.

---@param dirs string[]
---@return table<string, test_report.TestResult>
function M.parse_results(dirs)
    local results = {}
    for _, dir in ipairs(dirs) do
        for _, filepath in ipairs(jest_json.list_report_files(dir)) do
            local by_file = jest_json.parse_raw(filepath)
            for abs, by_line in pairs(by_file) do
                for row0, g in pairs(by_line) do
                    local member = "L" .. row0
                    local id = abs .. "#" .. member

                    local time = 0
                    for _, inv in ipairs(g.invocations) do
                        time = time + (inv.time or 0)
                    end

                    local ancestors = g.ancestors or {}
                    local member_display = g.leaf or ""
                    if #ancestors > 0 then
                        member_display = table.concat(ancestors, " › ") .. " › " .. member_display
                    end

                    results[id] = {
                        status = jest_json.aggregate_status(g.invocations),
                        time = time,
                        invocations = g.invocations,
                        errors = (#g.errors > 0) and g.errors or nil,
                        -- Display info travels WITH the result (which the core
                        -- persists/merges across runs) so names survive partial
                        -- reloads (e.g. a filtered single-test debug reload).
                        display = {
                            container = vim.fn.fnamemodify(abs, ":t"),
                            member = member_display,
                            file = abs,
                        },
                    }

                    -- Fast path cache (rebuilt every parse); the result.display
                    -- above is the durable source of truth.
                    display_cache[id] = results[id].display
                end
            end
        end
    end
    return results
end

--- container_id is the absolute file path (everything before `#`).
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

---@param container_id string
---@param stacktrace string
---@return number|nil
function M.extract_error_line(container_id, stacktrace)
    if not stacktrace or stacktrace == "" or not container_id then
        return nil
    end
    local file = container_id
    local pat = file:gsub("([^%w])", "%%%1") .. "%:(%d+)%:%d+"
    local line = stacktrace:match(pat)
    return line and tonumber(line) or nil
end

---@return string
function M.get_test_report_dir()
    local ok, runner = pcall(require, "modules.js.jest-test")
    if ok and runner.get_test_report_dir then
        return runner.get_test_report_dir()
    end
    local root = vim.fn.getcwd()
    local safe = root:gsub("[/\\]", "_"):gsub("^_+", "")
    return vim.fn.stdpath("cache") .. "/test-report/js/" .. safe
end

---@param id string  "<abs_file_path>#L<row>"
---@return test_report.IdDisplay
function M.id_to_display(id)
    local file = id:match("^(.+)#")
    local project_root
    local ok, runner = pcall(require, "modules.js.jest-test")
    if ok and runner.project_root then
        project_root = runner.project_root()
    end

    local function group_for(abs)
        local dir = vim.fn.fnamemodify(abs, ":h")
        if project_root and vim.startswith(dir, project_root) then
            local g = dir:sub(#project_root + 2)
            return (g ~= "") and g or nil
        end
        return dir
    end

    -- Resolve a human-readable name. Resolution order (each step is durable
    -- against a partial/filtered reload, since the result itself lives in the
    -- core's accumulated snapshot):
    --   1. per-parse fast cache
    --   2. `result.display` stored on the result at parse time
    --   3. `result.invocations[1].name` (the leaf test title — ALWAYS present)
    -- Only if no result exists at all do we fall back to the raw `L<row>` key.
    local disp = display_cache[id]
    local result
    if not disp then
        local ok_core, core = pcall(require, "modules.common.test-report")
        if ok_core and core.get_report_snapshot then
            local snap = core.get_report_snapshot()
            result = snap.results and snap.results[id]
            if result and result.display then
                disp = result.display
            end
        end
    end

    if disp then
        return {
            container = disp.container,
            member = disp.member,
            group = group_for(disp.file or file or id),
        }
    end

    -- Fallback: derive the member from the result's first invocation title.
    if result and result.invocations and result.invocations[1] and result.invocations[1].name then
        return {
            container = file and vim.fn.fnamemodify(file, ":t") or id,
            member = result.invocations[1].name,
            group = file and group_for(file) or nil,
        }
    end

    return {
        container = file and vim.fn.fnamemodify(file, ":t") or id,
        member = id:match("#(.+)$") or "",
        group = file and group_for(file) or nil,
    }
end

return M
