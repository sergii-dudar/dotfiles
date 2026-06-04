-- Lua / busted language adapter for test-report.
--
-- Test ID convention:
--   <abs_file_path>#<desc1::desc2::...::it_name>
-- The container ID is the absolute file path (busted has no "class" — files
-- are the natural container). The member is the full describe chain
-- ("::"-joined) followed by the `it` description.
--
-- Discovery is treesitter-based: we walk the `lua` parse tree looking for
-- function-call nodes whose function name is `describe`, `it`, `pending`,
-- or `it.skip` / `it.pending` (busted DSL).

local json_parser = require("modules.lua.test-report.json-parser")
local log = require("utils.logging-util").new({
    name = "test-report-lua",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

---@type test_report.LangAdapter
local M = {
    group_separator = "/",
    diagnostic_source = "busted",
    trouble_source = "busted_test_diagnostics",
}

-- file_path -> { positions = { [member_name] = 0-indexed line }, mtime = number, container_line = number|nil }
local positions_cache = {}

function M.clear_cache()
    positions_cache = {}
end

--------------------------------------------------------------------------------
-- Treesitter discovery

local DESCRIBE_NAMES = { describe = true }
local IT_NAMES = { it = true, pending = true, spec = true, test = true }

--- Get the function name of a function_call node.
--- Returns the callee identifier ("describe", "it", "pending", or "it.skip" etc.).
---@param node TSNode
---@param bufnr integer
---@return string|nil
local function call_function_name(node, bufnr)
    local fn = node:field("name")[1] or node:child(0)
    if not fn then
        return nil
    end
    local t = fn:type()
    if t == "identifier" then
        return vim.treesitter.get_node_text(fn, bufnr)
    end
    if t == "dot_index_expression" or t == "method_index_expression" then
        return vim.treesitter.get_node_text(fn, bufnr)
    end
    return vim.treesitter.get_node_text(fn, bufnr)
end

--- Returns true if `name` is a busted "it"-like leaf call.
local function is_it_name(name)
    if not name then
        return false
    end
    if IT_NAMES[name] then
        return true
    end
    -- it.skip / it.pending / pending.skip
    local base = name:match("^([%w_]+)%.")
    return base ~= nil and IT_NAMES[base]
end

local function is_describe_name(name)
    if not name then
        return false
    end
    if DESCRIBE_NAMES[name] then
        return true
    end
    local base = name:match("^([%w_]+)%.")
    return base ~= nil and DESCRIBE_NAMES[base]
end

--- Extract the first-argument string literal (the description) from a call.
---@param call TSNode  function_call node
---@param bufnr integer
---@return string|nil
local function first_string_arg(call, bufnr)
    local args = call:field("arguments")[1]
    if not args then
        -- fall back: walk children to find argument list
        for child in call:iter_children() do
            local ct = child:type()
            if ct == "arguments" or ct == "argument_list" then
                args = child
                break
            end
        end
    end
    if not args then
        return nil
    end
    for child in args:iter_children() do
        local t = child:type()
        if t == "string" then
            local raw = vim.treesitter.get_node_text(child, bufnr)
            -- Strip surrounding quotes/brackets
            if raw:sub(1, 1) == '"' or raw:sub(1, 1) == "'" then
                return raw:sub(2, -2)
            elseif raw:sub(1, 2) == "[[" then
                -- [[ ... ]] long bracket
                return raw:gsub("^%[=*%[", ""):gsub("%]=*%]$", "")
            end
            return raw
        elseif t == "expression" then
            -- some grammars wrap literals in `expression`
            for sub in child:iter_children() do
                if sub:type() == "string" then
                    local raw = vim.treesitter.get_node_text(sub, bufnr)
                    if raw:sub(1, 1) == '"' or raw:sub(1, 1) == "'" then
                        return raw:sub(2, -2)
                    end
                    return raw
                end
            end
        end
    end
    return nil
end

--- Walk the tree and collect tests with their full describe chain.
--- Also captures dynamic `it(<non-literal>)` calls: their describe-chain prefix
--- is recorded so we can later resolve parametrized member names (like
--- `"add[" .. tc.name .. "]"`) by prefix match against parse_results member ids.
---@param bufnr integer
---@return { name: string, line: number }[] tests
---@return number|nil first_describe_line
---@return { chain: string, line: number }[] dynamic_groups  longest-prefix-wins
local function collect_tests_in_buffer(bufnr)
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "lua")
    if not ok or not parser then
        return {}, nil, {}
    end
    local tree = parser:parse()[1]
    if not tree then
        return {}, nil, {}
    end
    local root = tree:root()

    local tests = {}
    local dynamic_groups = {}
    local first_describe_line = nil

    local function walk(node, chain)
        local t = node:type()
        if t == "function_call" then
            local fn_name = call_function_name(node, bufnr)
            if fn_name then
                if is_describe_name(fn_name) then
                    local desc = first_string_arg(node, bufnr)
                    if desc then
                        local line = node:start()
                        if not first_describe_line or line < first_describe_line then
                            first_describe_line = line
                        end
                        local new_chain = vim.deepcopy(chain)
                        table.insert(new_chain, desc)
                        for child in node:iter_children() do
                            walk(child, new_chain)
                        end
                        return
                    end
                elseif is_it_name(fn_name) then
                    local desc = first_string_arg(node, bufnr)
                    if desc then
                        local full = vim.deepcopy(chain)
                        table.insert(full, desc)
                        table.insert(tests, {
                            name = table.concat(full, "::"),
                            line = node:start(),
                        })
                    else
                        -- Dynamic test name (concatenation, format call, identifier, etc.).
                        -- Record the describe-chain prefix and the it() line so we can
                        -- map parametrized member names to this line by prefix matching.
                        if #chain > 0 then
                            table.insert(dynamic_groups, {
                                chain = table.concat(chain, "::"),
                                line = node:start(),
                            })
                        end
                    end
                    return
                end
            end
        end
        for child in node:iter_children() do
            walk(child, chain)
        end
    end

    walk(root, {})
    return tests, first_describe_line, dynamic_groups
end

--- Like collect_tests_in_buffer, but for an arbitrary file path (loads or uses buffer).
---@param file_path string
---@param opts? test_report.FindOpts
---@return { name: string, line: number }[] tests
---@return number|nil container_line
---@return { chain: string, line: number }[] dynamic_groups
local function collect_tests_in_file(file_path, opts)
    file_path = vim.fn.fnamemodify(file_path, ":p")
    local bufnr = vim.fn.bufnr(file_path)
    local created_buf = false
    if bufnr == -1 or not vim.api.nvim_buf_is_loaded(bufnr) then
        bufnr = vim.fn.bufadd(file_path)
        if opts and opts.silent == false then
            vim.fn.bufload(bufnr)
        else
            -- silent load without firing autocmds
            vim.fn.bufload(bufnr)
        end
        created_buf = true
    end
    local tests, line, dynamic_groups = collect_tests_in_buffer(bufnr)
    -- Don't wipe the buffer — test-report keeps it around for sign placement.
    return tests, line, dynamic_groups
end

--------------------------------------------------------------------------------
-- Public LangAdapter API

-- Members observed in the most recent parse_results call, keyed by absolute
-- file path. Used by find_test_positions to resolve dynamic (parametrized)
-- test names against describe-chain prefixes captured by treesitter.
local members_by_file = {}

--- Get dynamic_groups for a file by triggering a treesitter walk.
--- Used by parse_results to know which describe chains have dynamic it() bodies.
---@param file_path string
---@return { chain: string, line: number }[]
local function dynamic_groups_for_file(file_path)
    if vim.fn.filereadable(file_path) ~= 1 then
        return {}
    end
    local _, _, dynamic_groups = collect_tests_in_file(file_path, { silent = true })
    return dynamic_groups or {}
end

--- Pick the longest describe-chain prefix that matches `member` (encoded form).
--- Returns the matching encoded prefix string, or nil.
---@param member string  Encoded member name (with %23 for #).
---@param dynamic_groups { chain: string, line: number }[]  Already sorted by length desc.
---@return string|nil prefix_encoded
local function longest_dynamic_prefix(member, dynamic_groups)
    for _, g in ipairs(dynamic_groups) do
        local prefix_enc = g.chain:gsub("#", "%%23")
        if member == prefix_enc or vim.startswith(member, prefix_enc .. "::") then
            return prefix_enc
        end
    end
    return nil
end

--- Pick the most severe status across a set: failed > skipped > passed.
local function combine_status(a, b)
    if a == "failed" or b == "failed" then
        return "failed"
    end
    if a == "passed" or b == "passed" then
        return "passed"
    end
    return a or b or "skipped"
end

---@param dirs string[]
---@return table<string, test_report.TestResult>
function M.parse_results(dirs)
    local raw = json_parser.parse_results(dirs)

    -- Group raw results by file so we only treesitter-walk each file once.
    local by_file = {}
    for id, result in pairs(raw) do
        local container_id, member = id:match("^(.+)#(.+)$")
        if container_id and member then
            local bucket = by_file[container_id]
            if not bucket then
                bucket = {}
                by_file[container_id] = bucket
            end
            table.insert(bucket, { id = id, container_id = container_id, member = member, result = result })
        end
    end

    local aggregated = {}
    members_by_file = {}

    for file_path, entries in pairs(by_file) do
        local dgroups = dynamic_groups_for_file(file_path)
        -- Sort prefixes by length desc so the most specific match wins.
        table.sort(dgroups, function(a, b)
            return #a.chain > #b.chain
        end)

        local file_members = {}
        members_by_file[file_path] = file_members

        for _, e in ipairs(entries) do
            local prefix = longest_dynamic_prefix(e.member, dgroups)
            if prefix and prefix ~= e.member then
                -- Parametrized case: aggregate under <file>#<prefix>.
                local agg_id = file_path .. "#" .. prefix
                local agg = aggregated[agg_id]
                if not agg then
                    agg = {
                        status = e.result.status,
                        errors = nil,
                        time = 0,
                        invocations = {},
                    }
                    aggregated[agg_id] = agg
                end
                agg.status = combine_status(agg.status, e.result.status)
                agg.time = (agg.time or 0) + (e.result.time or 0)
                -- Carry only the FIRST error message per aggregate to keep
                -- Trouble output sane; full per-case details remain in invocations.
                if e.result.status == "failed" and not agg.errors and e.result.errors then
                    agg.errors = { e.result.errors[1] }
                end
                for _, inv in ipairs(e.result.invocations or {}) do
                    -- Decode the encoded suffix for display in invocations list.
                    local case_suffix = e.member
                    if vim.startswith(case_suffix, prefix .. "::") then
                        case_suffix = case_suffix:sub(#prefix + 3)
                    end
                    inv.name = (case_suffix:gsub("%%23", "#"))
                    table.insert(agg.invocations, inv)
                end
                file_members[prefix] = true
            else
                -- Literal test (or dynamic with no prefix match): keep as-is.
                aggregated[e.id] = e.result
                file_members[e.member] = true
            end
        end
    end

    return aggregated
end

---@param container_id string  Absolute file path
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

    local tests, container_line, dynamic_groups = collect_tests_in_file(file_path, opts)
    local positions = {}
    for _, t in ipairs(tests) do
        -- Mirror the json-parser's # -> %23 encoding so position keys match
        -- the member portion of result ids stored in last_results.
        local key = t.name:gsub("#", "%%23")
        positions[key] = t.line
    end

    -- Resolve dynamic (parametrized) test names: for every member observed in
    -- the most recent parse_results that wasn't matched by a literal it(),
    -- find the longest describe-chain prefix in dynamic_groups and use its line.
    if dynamic_groups and #dynamic_groups > 0 then
        local members = members_by_file[file_path] or {}
        -- Sort prefixes by length desc so the most specific match wins.
        table.sort(dynamic_groups, function(a, b)
            return #a.chain > #b.chain
        end)
        for member, _ in pairs(members) do
            if positions[member] == nil then
                for _, g in ipairs(dynamic_groups) do
                    local prefix_enc = g.chain:gsub("#", "%%23")
                    if member == prefix_enc or vim.startswith(member, prefix_enc .. "::") then
                        positions[member] = g.line
                        break
                    end
                end
            end
        end
    end
    positions_cache[file_path] = {
        positions = positions,
        container_line = container_line,
        mtime = mtime,
    }
    return positions, container_line
end

---@param container_id string
---@param stacktrace string
---@return number|nil
function M.extract_error_line(container_id, stacktrace)
    if not stacktrace or stacktrace == "" or not container_id then
        return nil
    end
    -- Look for "<file>:<line>" entries; prefer ones matching the container file.
    local container_basename = vim.fn.fnamemodify(container_id, ":t")
    for line in stacktrace:gmatch("[^\n]+") do
        -- Match this file specifically
        local lnum = line:match(vim.pesc(container_id) .. ":(%d+)")
        if lnum then
            return tonumber(lnum)
        end
        lnum = line:match(vim.pesc(container_basename) .. ":(%d+)")
        if lnum then
            return tonumber(lnum)
        end
    end
    -- Fallback: any `.lua:NNN`
    local lnum = stacktrace:match("([%w/_%-%.]+%.lua):(%d+)")
    return lnum and tonumber(stacktrace:match("[%w/_%-%.]+%.lua:(%d+)")) or nil
end

---@return string
function M.get_test_report_dir()
    -- Mirror the per-project pattern used by go/rust.
    local root = require("modules.lua.busted-test").project_root() or vim.fn.getcwd()
    local safe = root:gsub("[/\\]", "_"):gsub("^_+", "")
    return vim.fn.stdpath("cache") .. "/test-report/lua/" .. safe
end

---@param id string  "<abs_file_path>#<desc_chain>"
---@return test_report.IdDisplay
function M.id_to_display(id)
    local container_id, member = id:match("^(.+)#(.+)$")
    if not container_id then
        return { container = id, member = "", group = nil }
    end
    -- container_id is an absolute file path. Display: container = basename,
    -- group = directory (relative to project root or absolute).
    local project_root = nil
    local ok, busted = pcall(require, "modules.lua.busted-test")
    if ok and busted.project_root then
        project_root = busted.project_root()
    end
    -- Decode # back from %23 (encoded by json-parser to avoid id-separator collision).
    member = member:gsub("%%23", "#")
    local container_name = vim.fn.fnamemodify(container_id, ":t")
    local dir = vim.fn.fnamemodify(container_id, ":h")
    local group
    if project_root and vim.startswith(dir, project_root) then
        group = dir:sub(#project_root + 2) -- strip leading "/"
        if group == "" then
            group = nil
        end
    else
        group = dir
    end
    return { container = container_name, member = member, group = group }
end

return M
