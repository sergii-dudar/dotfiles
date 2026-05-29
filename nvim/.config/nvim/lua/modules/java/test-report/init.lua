local junit_xml = require("modules.java.test-report.junit-xml")
local constants = require("utils.constants")
local spinner = require("utils.ui.spinner")
local nio = require("nio")
local log = require("utils.logging-util").new({
    name = "test-report",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

local M = {}

local process_generation = 0

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

---@class test_report.FindOpts
---@field silent? boolean If true (default), load buffer without firing autocmds (fast, no LSP/highlight). If false, full bufload triggering FileType cascade.

---@class test_report.LangAdapter
---@field classname_to_file fun(classname: string, report_dir: string): string|nil
---@field find_test_positions fun(file_path: string, opts?: test_report.FindOpts): table<string, number>, number|nil
---@field extract_error_line fun(classname: string, stacktrace: string): number|nil
---@field get_test_report_dir fun(): string
---@field clear_cache? fun()

local lang_adapters = {
    java = function()
        return require("modules.java.test-report.lang.java")
    end,
}

-- Adapters that have been loaded this session (for cache invalidation in M.clear)
local loaded_adapters = {}

---@class test_report.Config
---@field load_buffers boolean Load every test-class buffer with full autocmds (FileType -> LSP attach -> highlight). Slower but ready to navigate without rescue.
---@field load_only_buffers_with_error boolean Load only buffers containing failed tests with full autocmds. Other classes load silently.

---@type test_report.Config
local config = {
    load_buffers = false,
    load_only_buffers_with_error = false,
}

---@param opts? test_report.Config
function M.setup(opts)
    config = vim.tbl_extend("force", config, opts or {})
end

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
local last_class_files = {} -- { [classname] = file_path }
local last_filetype = nil -- filetype used to process results
local output_bufnr = nil -- scratch buffer for test output
local output_method = nil -- method name currently shown in output buffer

---@param report_dir string|string[]
---@param filetype string
function M.process(report_dir, filetype)
    -- Cancel any in-flight processing but don't wipe accumulated state
    process_generation = process_generation + 1
    spinner.cancel({ id = "junit_report" })
    local my_gen = process_generation

    nio.run(function()
        nio.scheduler()
        if process_generation ~= my_gen then
            return
        end

        local sp = { id = "junit_report", title = "JUnit Report" }
        local sp_stop = vim.tbl_extend("force", sp, { timeout = 1500 })
        spinner.start("Processing JUnit Report…", sp)

        local hr = vim.uv.hrtime
        local t_start = hr()

        local spinner_resolved = false
        local ok, pcall_err = pcall(function()
            local dirs = type(report_dir) == "table" and report_dir or { report_dir }
            log.info("process: dirs=" .. vim.inspect(dirs) .. " ft=" .. tostring(filetype))

            local adapter_fn = lang_adapters[filetype]
            if not adapter_fn then
                log.error("no adapter for filetype: " .. tostring(filetype))
                vim.notify("test-report: no adapter for filetype: " .. filetype, vim.log.levels.ERROR)
                spinner_resolved = true
                spinner.stop(false, "No adapter for filetype: " .. tostring(filetype), sp_stop)
                return
            end
            local adapter = adapter_fn()
            loaded_adapters[filetype] = adapter

            local all_files = {}
            for _, dir in ipairs(dirs) do
                vim.list_extend(all_files, junit_xml.list_report_files(dir))
            end

            local t_list = hr()
            local results = {}
            for _, filepath in ipairs(all_files) do
                for id, r in pairs(junit_xml.parse_file(filepath)) do
                    results[id] = r
                end
            end
            local t_parse = hr()

            if vim.tbl_isempty(results) then
                log.warn("no results from XML parsing")
                vim.notify("test-report: no results from XML parsing in: " .. vim.inspect(dirs), vim.log.levels.ERROR)
                spinner_resolved = true
                spinner.stop(false, "No results from XML parsing", sp_stop)
                return
            end

            log.info("parsed " .. vim.tbl_count(results) .. " test results")
            for id, r in pairs(results) do
                last_results[id] = r
            end
            last_filetype = filetype

            local affected_classes = {}
            for id in pairs(results) do
                local classname = id:match("^(.+)#.+$")
                if classname then
                    affected_classes[classname] = true
                end
            end

            -- Build by_class from merged last_results so reruns restore ALL accumulated
            -- results for affected classes (not just the current subset).
            local by_class = {}
            for id, result in pairs(last_results) do
                local classname, method = id:match("^(.+)#(.+)$")
                if classname and method and affected_classes[classname] then
                    local methods = by_class[classname]
                    if not methods then
                        methods = {}
                        by_class[classname] = methods
                    end
                    methods[method] = result
                end
            end

            local t_byclass = hr()
            local total_classes = vim.tbl_count(by_class)

            for classname, methods in pairs(by_class) do
                local file_path
                for _, dir in ipairs(dirs) do
                    file_path = adapter.classname_to_file(classname, dir)
                    if file_path then
                        break
                    end
                end

                if not file_path then
                    log.error("classname_to_file returned nil for: " .. classname)
                    vim.notify(
                        "test-report: could not resolve file for class: " .. classname,
                        vim.log.levels.ERROR
                    )
                    goto continue
                end

                file_path = vim.fn.fnamemodify(file_path, ":p")
                last_class_files[classname] = file_path

                local class_has_failure = false
                for _, r in pairs(methods) do
                    if r.status == "failed" then
                        class_has_failure = true
                        break
                    end
                end
                local silent = not (config.load_buffers or (config.load_only_buffers_with_error and class_has_failure))

                local test_positions, class_line = adapter.find_test_positions(file_path, { silent = silent })
                last_positions[file_path] = test_positions

                local bufnr = vim.fn.bufnr(file_path)
                if bufnr == -1 then
                    log.error("buffer not found for: " .. file_path)
                    vim.notify("test-report: buffer not found for: " .. file_path, vim.log.levels.ERROR)
                    goto continue
                end

                vim.api.nvim_buf_clear_namespace(bufnr, ns_signs, 0, -1)
                vim.diagnostic.reset(ns_diag, bufnr)

                local has_failure = false
                local diagnostics = {}

                for method_name, result in pairs(methods) do
                    local line = test_positions[method_name]
                    if line == nil then
                        log.warn("no treesitter position for method: " .. method_name)
                        goto next_method
                    end

                    if result.status == "failed" then
                        has_failure = true
                    end

                    local sign = sign_config[result.status]
                    if sign then
                        vim.api.nvim_buf_set_extmark(bufnr, ns_signs, line, 0, {
                            sign_text = sign.text,
                            sign_hl_group = sign.hl,
                            virt_text = { { " " .. sign.text, sign.hl } },
                            virt_text_pos = "eol",
                            priority = 20,
                        })
                        signed_buffers[bufnr] = true
                    end

                    if result.status == "failed" and result.errors then
                        for _, e in ipairs(result.errors) do
                            diagnostics[#diagnostics + 1] = {
                                lnum = e.line and (e.line - 1) or line,
                                col = 0,
                                message = e.message,
                                severity = vim.diagnostic.severity.ERROR,
                                source = "junit",
                            }
                        end
                    end

                    ::next_method::
                end

                if #diagnostics > 0 then
                    vim.diagnostic.set(ns_diag, bufnr, diagnostics)
                end

                if class_line ~= nil then
                    local class_sign = sign_config[has_failure and "failed" or "passed"]
                    if class_sign then
                        vim.api.nvim_buf_set_extmark(bufnr, ns_signs, class_line, 0, {
                            sign_text = class_sign.text,
                            sign_hl_group = class_sign.hl,
                            virt_text = { { " " .. class_sign.text, class_sign.hl } },
                            virt_text_pos = "eol",
                            priority = 20,
                        })
                        signed_buffers[bufnr] = true
                    end
                end

                ::continue::
            end

            local t_classloop = hr()

            require("overseer").close()

            local has_any_failures = false
            for _, r in pairs(last_results) do
                if r.status == "failed" then
                    has_any_failures = true
                    break
                end
            end
            if has_any_failures then
                vim.cmd("Trouble junit_diagnostics open")
            end

            local total = vim.tbl_count(results)
            local failed = 0
            for _, r in pairs(results) do
                if r.status == "failed" then
                    failed = failed + 1
                end
            end
            if failed > 0 then
                spinner.stop(false, "Tests Finished with failed " .. failed .. "/" .. total .. " tests", sp_stop)
            else
                spinner.stop(true, total .. " Tests Passed", sp_stop)
            end
            spinner_resolved = true

            require("modules.java.test-report.junit-report-view").refresh_if_open(M.get_report_snapshot())

            log.info(
                string.format(
                    "[perf process] total=%.1fms list=%.1fms parse=%.1fms(%d files) byclass=%.1fms classloop=%.1fms(%d classes)",
                    (hr() - t_start) / 1e6,
                    (t_list - t_start) / 1e6,
                    (t_parse - t_list) / 1e6,
                    #all_files,
                    (t_byclass - t_parse) / 1e6,
                    (t_classloop - t_byclass) / 1e6,
                    total_classes
                )
            )
        end)

        if not ok then
            log.error("process error: " .. tostring(pcall_err))
            spinner.stop(false, "JUnit Report processing failed", sp_stop)
        elseif not spinner_resolved then
            log.warn("process exited without resolving spinner")
            spinner.cancel(sp)
        end

        log.info("process complete")
    end)
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
    process_generation = process_generation + 1
    spinner.cancel({ id = "junit_report" })
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
    last_class_files = {}
    last_filetype = nil
    -- Drop per-adapter caches (class index, etc.) so a fresh scan happens next run
    for _, adapter in pairs(loaded_adapters) do
        if adapter.clear_cache then
            adapter.clear_cache()
        end
    end
    -- Close output buffer if open
    if output_bufnr and vim.api.nvim_buf_is_valid(output_bufnr) then
        for _, win in ipairs(vim.api.nvim_list_wins()) do
            if vim.api.nvim_win_get_buf(win) == output_bufnr then
                vim.api.nvim_win_close(win, true)
            end
        end
    end
end

--- Cancel in-flight processing without wiping accumulated state.
--- Used by overseer component lifecycle hooks (on_reset, on_dispose) so that
--- rerunning a subset of tests preserves diagnostics from previous runs.
function M.cancel()
    process_generation = process_generation + 1
    spinner.cancel({ id = "junit_report" })
    -- Close Trouble diagnostics at test start so only overseer output is visible during the run
    vim.cmd("Trouble junit_diagnostics close")
    log.info("cancel (preserving accumulated state)")
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

local function find_classname_for_file(file_path)
    for classname, path in pairs(last_class_files) do
        if path == file_path then
            return classname
        end
    end
    return nil
end

local function find_result_for_method(file_path, method_name)
    -- Precise lookup: resolve classname from file, then use full key
    local classname = find_classname_for_file(file_path)
    if classname then
        local key = classname .. "#" .. method_name
        if last_results[key] then
            return last_results[key]
        end
    end
    -- Fallback: match by method name only (e.g. classname lookup failed)
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
    vim.api.nvim_buf_set_lines(output_bufnr, 0, -1, false, lines)
    for _, entry in ipairs(highlights) do
        vim.api.nvim_buf_set_extmark(output_bufnr, ns_output, entry[1], 0, {
            line_hl_group = entry[2],
        })
    end

    require("utils.buffer-util").open_scratch_split(output_bufnr)
    -- require("utils.java.java-trace").highlight_java_test_trace(output_bufnr)
end

function M.show_test_output()
    local file_path = vim.api.nvim_buf_get_name(0)
    local cursor_line = vim.api.nvim_win_get_cursor(0)[1] - 1 -- 0-indexed

    local best_method = find_method_at_cursor(file_path, cursor_line)

    -- If output is visible, close it; if same method in same file, just toggle off
    local output_key = file_path .. "#" .. (best_method or "")
    if close_output_win() and output_method == output_key then
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

    local result = find_result_for_method(file_path, best_method)
    if not result then
        vim.notify("test-report: no output for " .. best_method, vim.log.levels.WARN)
        return
    end

    open_output(best_method, result)
    output_method = output_key
end

function M.hide_test_output()
    close_output_win()
    require("utils.dap-util").reset()
end

--- Show test output for a specific method and result (used by tree view).
---@param method_name string
---@param result test_report.TestResult
function M.show_output_for(method_name, result)
    close_output_win()
    open_output(method_name, result)
end

--- Return a snapshot of the last processed results for the tree view.
---@return { results: table, positions: table, class_files: table, filetype: string|nil }
function M.get_report_snapshot()
    return {
        results = last_results,
        positions = last_positions,
        class_files = last_class_files,
        filetype = last_filetype,
    }
end

--- Open or toggle the test report tree view.
function M.open_tree_view()
    local snapshot = M.get_report_snapshot()
    if vim.tbl_isempty(snapshot.results) then
        vim.notify("test-report: no test results available. Run tests first.", vim.log.levels.WARN)
        return
    end
    require("modules.java.test-report.junit-report-view").toggle(snapshot)
end

return M
