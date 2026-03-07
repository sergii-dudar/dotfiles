local junit_xml = require("plugins.overseer.test-report.junit-xml")
local log = require("utils.logging-util").new({
    name = "test-report",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

local M = {}

---@class test_report.Invocation
---@field name string
---@field status "passed"|"failed"|"skipped"
---@field metadata? string
---@field stdout? string
---@field stderr? string
---@field stacktrace? string
---@field time? number

---@class test_report.TestResult
---@field status "passed"|"failed"|"skipped"
---@field errors? { message: string, line: number|nil }[]
---@field time? number
---@field invocations test_report.Invocation[]

---@class test_report.LangAdapter
---@field classname_to_file fun(classname: string, report_dir: string): string|nil
---@field find_test_positions fun(file_path: string): table<string, number>, number|nil
---@field extract_error_line fun(classname: string, stacktrace: string): number|nil
---@field get_test_report_dir fun(): string

local lang_adapters = {
    java = function()
        return require("plugins.overseer.test-report.lang.java")
    end,
}

local ns_diag = vim.api.nvim_create_namespace("overseer_test_report_diag")
local ns_signs = vim.api.nvim_create_namespace("overseer_test_report_signs")

local sign_config = {
    passed = { text = "", hl = "DiagnosticOk" },
    failed = { text = "", hl = "DiagnosticError" },
    skipped = { text = "", hl = "DiagnosticWarn" },
}
--      

-- Track buffers where we placed signs for efficient cleanup
local signed_buffers = {}

-- Store last results for output viewing
local last_results = {} -- { [classname#method] = TestResult }
local last_positions = {} -- { [file_path] = { [method_name] = 0-indexed line } }
local output_bufnr = nil -- scratch buffer for test output
local output_method = nil -- method name currently shown in output buffer

---@param report_dir string
---@param filetype string
function M.process(report_dir, filetype)
    log.info("process: report_dir=" .. tostring(report_dir) .. " ft=" .. tostring(filetype))

    local adapter_fn = lang_adapters[filetype]
    if not adapter_fn then
        log.error("no adapter for filetype: " .. tostring(filetype))
        vim.notify("test-report: no adapter for filetype: " .. filetype, vim.log.levels.ERROR)
        return
    end
    local adapter = adapter_fn()

    local results = junit_xml.parse_report_dir(report_dir)
    if vim.tbl_isempty(results) then
        log.warn("no results from XML parsing")
        vim.notify("test-report: no results from XML parsing in " .. report_dir, vim.log.levels.ERROR)
        return
    end

    log.info("parsed " .. vim.tbl_count(results) .. " test results")
    last_results = results
    last_positions = {}
    for id, r in pairs(results) do
        log.debug("  " .. id .. " -> " .. r.status)
    end

    -- Group results by classname
    local by_class = {}
    for id, result in pairs(results) do
        local classname, method = id:match("^(.+)#(.+)$")
        if classname and method then
            if not by_class[classname] then
                by_class[classname] = {}
            end
            by_class[classname][method] = result
        end
    end

    local qf_entries = {}

    for classname, methods in pairs(by_class) do
        local file_path = adapter.classname_to_file(classname, report_dir)
        log.debug("classname_to_file: " .. classname .. " -> " .. tostring(file_path))

        if file_path then
            -- Normalize to absolute path
            file_path = vim.fn.fnamemodify(file_path, ":p")
            log.debug("absolute path: " .. file_path)

            local test_positions, class_line = adapter.find_test_positions(file_path)
            last_positions[file_path] = test_positions
            log.debug("test_positions: ", test_positions)
            log.debug("class_line: " .. tostring(class_line))

            -- Resolve buffer number (find_test_positions ensures buffer is loaded)
            local bufnr = vim.fn.bufnr(file_path)
            log.debug("bufnr: " .. bufnr)
            if bufnr == -1 then
                log.error("buffer not found for: " .. file_path)
                vim.notify("test-report: buffer not found for: " .. file_path, vim.log.levels.ERROR)
                goto continue
            end

            local has_failure = false

            for method_name, result in pairs(methods) do
                local line = test_positions[method_name]
                log.debug("method=" .. method_name .. " status=" .. result.status .. " line=" .. tostring(line))
                if line ~= nil then
                    if result.status == "failed" then
                        has_failure = true
                    end

                    -- Place gutter sign + virtual text via extmark
                    local sign = sign_config[result.status]
                    if sign then
                        local mark_id = vim.api.nvim_buf_set_extmark(bufnr, ns_signs, line, 0, {
                            sign_text = sign.text,
                            sign_hl_group = sign.hl,
                            virt_text = { { " " .. sign.text, sign.hl } },
                            virt_text_pos = "eol",
                            priority = 20,
                        })
                        signed_buffers[bufnr] = true
                        log.debug("placed extmark id=" .. mark_id .. " sign='" .. sign.text .. "' at line " .. line)
                    end

                    -- Set diagnostics and quickfix for failures
                    if result.status == "failed" and result.errors then
                        local diagnostics = {}
                        for _, err in ipairs(result.errors) do
                            local err_line = err.line and (err.line - 1) or line
                            table.insert(diagnostics, {
                                lnum = err_line,
                                col = 0,
                                message = err.message,
                                severity = vim.diagnostic.severity.ERROR,
                                source = "junit",
                            })

                            table.insert(qf_entries, {
                                filename = file_path,
                                lnum = (err.line or (line + 1)),
                                col = 1,
                                text = method_name .. ": " .. err.message,
                                type = "E",
                            })
                        end

                        local existing = vim.diagnostic.get(bufnr, { namespace = ns_diag })
                        vim.list_extend(existing, diagnostics)
                        vim.diagnostic.set(ns_diag, bufnr, existing)
                        log.debug("set " .. #diagnostics .. " diagnostics for bufnr=" .. bufnr)
                    end
                else
                    log.warn("no treesitter position found for method: " .. method_name)
                end
            end

            -- Place class-level mark (aggregate: fail if any failed, pass if all passed)
            if class_line ~= nil then
                local class_status = has_failure and "failed" or "passed"
                local class_sign = sign_config[class_status]
                if class_sign then
                    vim.api.nvim_buf_set_extmark(bufnr, ns_signs, class_line, 0, {
                        sign_text = class_sign.text,
                        sign_hl_group = class_sign.hl,
                        virt_text = { { " " .. class_sign.text, class_sign.hl } },
                        virt_text_pos = "eol",
                        priority = 20,
                    })
                    log.debug("placed class mark: " .. class_status .. " at line " .. class_line)
                end
            end

            ::continue::
        else
            log.error("classname_to_file returned nil for: " .. classname)
            vim.notify("test-report: could not resolve file for class: " .. classname, vim.log.levels.ERROR)
        end
    end

    if #qf_entries > 0 then
        vim.fn.setqflist(qf_entries, "r")
        log.debug("set " .. #qf_entries .. " quickfix entries")
        -- vim.cmd("Trouble qflist open")
        vim.cmd("Trouble junit_diagnostics open")
    end

    -- Summary notification
    local total = vim.tbl_count(results)
    local failed = 0
    for _, r in pairs(results) do
        if r.status == "failed" then
            failed = failed + 1
        end
    end
    if failed > 0 then
        vim.notify("🚫 Tests Finished with failed " .. failed .. "/" .. total .. " tests", vim.log.levels.WARN)
    else
        vim.notify("🚀 " .. total .. " Tests Passed", vim.log.levels.INFO)
    end

    log.info("process complete")
end

function M.load_existing()
    local ft = vim.bo.filetype
    local adapter_fn = lang_adapters[ft]
    if not adapter_fn then
        vim.notify("test-report: no adapter for filetype: " .. ft, vim.log.levels.WARN)
        return
    end
    local adapter = adapter_fn()
    local report_dir = adapter.get_test_report_dir()
    if vim.fn.isdirectory(report_dir) ~= 1 then
        vim.notify("test-report: no report dir found: " .. report_dir, vim.log.levels.WARN)
        return
    end
    M.clear()
    M.process(report_dir, ft)
end

function M.clear()
    log.info("clear")
    -- Clear sign extmarks
    for bufnr, _ in pairs(signed_buffers) do
        if vim.api.nvim_buf_is_valid(bufnr) then
            vim.api.nvim_buf_clear_namespace(bufnr, ns_signs, 0, -1)
        end
    end
    signed_buffers = {}
    -- Clear diagnostics
    vim.diagnostic.reset(ns_diag)
    -- Clear stored results
    last_results = {}
    last_positions = {}
    -- Close output buffer if open
    if output_bufnr and vim.api.nvim_buf_is_valid(output_bufnr) then
        for _, win in ipairs(vim.api.nvim_list_wins()) do
            if vim.api.nvim_win_get_buf(win) == output_bufnr then
                vim.api.nvim_win_close(win, true)
            end
        end
    end
end

local function close_output_win()
    if output_bufnr and vim.api.nvim_buf_is_valid(output_bufnr) then
        for _, win in ipairs(vim.api.nvim_list_wins()) do
            if vim.api.nvim_win_get_buf(win) == output_bufnr then
                vim.api.nvim_win_close(win, true)
                return true
            end
        end
    end
    return false
end

local function find_method_at_cursor(file_path, cursor_line)
    local positions = last_positions[file_path]
    if not positions then
        return nil
    end
    local best_method, best_line = nil, -1
    for method, line in pairs(positions) do
        if line <= cursor_line and line > best_line then
            best_method = method
            best_line = line
        end
    end
    return best_method
end

local function find_result_for_method(method_name)
    for id, r in pairs(last_results) do
        local _, method = id:match("^(.+)#(.+)$")
        if method == method_name then
            return r
        end
    end
    return nil
end

local ns_output = vim.api.nvim_create_namespace("test_report_output")

local function make_separator(label)
    local total = 80
    local pad = total - #label - 2
    local left = math.floor(pad / 2)
    local right = pad - left
    return string.rep("=", left) .. " " .. label .. " " .. string.rep("=", right)
end

local function open_output(method_name, result)
    local lines = { "Test: " .. method_name, "Status: " .. result.status }
    if result.time then
        table.insert(lines, string.format("Time: %.3fs (total)", result.time))
    end
    table.insert(lines, "")

    -- Track lines for highlighting: { {line_idx, hl_group}, ... }
    local highlights = {}
    local function hl(hl_group)
        highlights[#highlights + 1] = { #lines - 1, hl_group }
    end

    local function add_section(label, text, hl_group)
        if text and text ~= "" then
            table.insert(lines, make_separator(label))
            hl(hl_group)
            vim.list_extend(lines, vim.split(text, "\n"))
            table.insert(lines, "")
        end
    end

    for i, inv in ipairs(result.invocations) do
        -- Invocation header for parameterized tests
        if #result.invocations > 1 then
            local inv_label = string.format("[%d] %s (%s, %.3fs)", i, inv.name, inv.status, inv.time or 0)
            table.insert(lines, make_separator(inv_label))
            hl(inv.status == "failed" and "DiagnosticError" or "DiagnosticOk")
        end

        -- Metadata (unique-id / display-name)
        if inv.metadata and inv.metadata ~= "" then
            local sep = string.rep("=", 80)
            table.insert(lines, sep)
            hl("DiagnosticHint")
            for _, meta_line in ipairs(vim.split(inv.metadata, "\n")) do
                if meta_line ~= "" then
                    table.insert(lines, meta_line)
                    hl("DiagnosticHint")
                end
            end
            table.insert(lines, sep)
            hl("DiagnosticHint")
        end

        add_section("stdout", inv.stdout, "DiagnosticOk")
        add_section("stderr", inv.stderr, "DiagnosticError")
        add_section("stacktrace", inv.stacktrace, "DiagnosticError")
    end

    require("overseer").close()
    output_bufnr = vim.api.nvim_create_buf(false, true)
    output_method = method_name
    vim.api.nvim_buf_set_lines(output_bufnr, 0, -1, false, lines)
    for _, entry in ipairs(highlights) do
        vim.api.nvim_buf_set_extmark(output_bufnr, ns_output, entry[1], 0, {
            line_hl_group = entry[2],
        })
    end
    vim.bo[output_bufnr].buftype = "nofile"
    vim.bo[output_bufnr].bufhidden = "wipe"
    vim.bo[output_bufnr].modifiable = false
    vim.cmd("botright split")
    vim.api.nvim_win_set_buf(0, output_bufnr)
    vim.api.nvim_win_set_height(0, math.min(#lines + 1, 15))
    require("utils.java.java-trace").highlight_java_test_trace(output_bufnr)
    vim.keymap.set("n", "q", function()
        vim.api.nvim_win_close(0, true)
    end, { buffer = output_bufnr, silent = true })
end

function M.show_test_output()
    local file_path = vim.api.nvim_buf_get_name(0)
    local cursor_line = vim.api.nvim_win_get_cursor(0)[1] - 1 -- 0-indexed

    local best_method = find_method_at_cursor(file_path, cursor_line)

    -- If output is visible, close it; if same method, just toggle off
    if close_output_win() and output_method == best_method then
        return
    end

    if not last_positions[file_path] then
        vim.notify("test-report: no test results for this file", vim.log.levels.WARN)
        return
    end

    if not best_method then
        vim.notify("test-report: no test method found at cursor", vim.log.levels.WARN)
        return
    end

    local result = find_result_for_method(best_method)
    if not result then
        vim.notify("test-report: no output for " .. best_method, vim.log.levels.WARN)
        return
    end

    open_output(best_method, result)
end

function M.hide_test_output()
    close_output_win()
end

return M