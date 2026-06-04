-- Generic test-report core: filetype-agnostic orchestration of test result
-- parsing, signs/diagnostics placement, output panel, and tree view.
--
-- Language-specific behavior is delegated to a `LangAdapter` (see types.lua),
-- looked up from `registry` by filetype.

require("modules.common.test-report.types")
local registry = require("modules.common.test-report.registry")
local spinner = require("utils.ui.spinner")
local nio = require("nio")
local log = require("utils.logging-util").new({
    name = "test-report",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

local M = {}

local process_generation = 0

-- Track adapters loaded this session (for cache invalidation in M.clear)
local loaded_adapters = {}

---@type test_report.Config
local config = {
    load_buffers = false,
    load_only_buffers_with_error = false,
}

---@param opts? test_report.Config
function M.setup(opts)
    config = vim.tbl_extend("force", config, opts or {})
end

local ns_diag = vim.api.nvim_create_namespace("test_report_diag")
local ns_signs = vim.api.nvim_create_namespace("test_report_signs")

local sign_config = {
    passed = { text = "", hl = "DiagnosticOk" },
    failed = { text = "", hl = "DiagnosticError" },
    skipped = { text = "", hl = "DiagnosticWarn" },
}

-- Track buffers where we placed signs for efficient cleanup
local signed_buffers = {}

-- Accumulated state (merged across runs)
local last_results = {} -- { [container_id#member] = TestResult }
local last_positions = {} -- { [file_path] = { [member_name] = 0-indexed line } }
local last_container_files = {} -- { [container_id] = file_path }
local last_filetype = nil -- filetype used to process results
local output_bufnr = nil -- scratch buffer for test output
local output_method = nil -- key currently shown in output buffer (file#member)

---@param id string
---@return string|nil container_id, string|nil member
local function split_id(id)
    return id:match("^(.+)#(.+)$")
end

---@param report_dir string|string[]
---@param filetype string
function M.process(report_dir, filetype)
    -- Cancel any in-flight processing but don't wipe accumulated state
    process_generation = process_generation + 1
    spinner.cancel({ id = "test_report" })
    local my_gen = process_generation

    nio.run(function()
        nio.scheduler()
        if process_generation ~= my_gen then
            return
        end

        local sp = { id = "test_report", title = "Test Report" }
        local sp_stop = vim.tbl_extend("force", sp, { timeout = 1500 })
        spinner.start("Processing Test Report…", sp)

        local hr = vim.uv.hrtime
        local t_start = hr()

        local spinner_resolved = false
        local ok, pcall_err = pcall(function()
            local dirs = type(report_dir) == "table" and report_dir or { report_dir }
            log.info("process: dirs=" .. vim.inspect(dirs) .. " ft=" .. tostring(filetype))

            local adapter = registry.get(filetype)
            if not adapter then
                log.error("no adapter for filetype: " .. tostring(filetype))
                vim.notify("test-report: no adapter for filetype: " .. filetype, vim.log.levels.ERROR)
                spinner_resolved = true
                spinner.stop(false, "No adapter for filetype: " .. tostring(filetype), sp_stop)
                return
            end
            loaded_adapters[filetype] = adapter

            local t_list = hr()
            local results = adapter.parse_results(dirs)
            local t_parse = hr()

            if vim.tbl_isempty(results) then
                log.warn("no results from parser")
                vim.notify("test-report: no results from parser in: " .. vim.inspect(dirs), vim.log.levels.ERROR)
                spinner_resolved = true
                spinner.stop(false, "No results from parser", sp_stop)
                return
            end

            log.info("parsed " .. vim.tbl_count(results) .. " test results")
            for id, r in pairs(results) do
                last_results[id] = r
            end
            last_filetype = filetype

            local affected_containers = {}
            for id in pairs(results) do
                local container_id = id:match("^(.+)#.+$")
                if container_id then
                    affected_containers[container_id] = true
                end
            end

            -- Build by_container from merged last_results so reruns restore ALL
            -- accumulated results for affected containers (not just the current subset).
            local by_container = {}
            for id, result in pairs(last_results) do
                local container_id, member = split_id(id)
                if container_id and member and affected_containers[container_id] then
                    local members = by_container[container_id]
                    if not members then
                        members = {}
                        by_container[container_id] = members
                    end
                    members[member] = result
                end
            end

            local t_byclass = hr()
            local total_containers = vim.tbl_count(by_container)

            for container_id, members in pairs(by_container) do
                local file_path
                for _, dir in ipairs(dirs) do
                    file_path = adapter.id_to_file(container_id, dir)
                    if file_path then
                        break
                    end
                end

                if not file_path then
                    log.error("id_to_file returned nil for: " .. container_id)
                    vim.notify("test-report: could not resolve file for: " .. container_id, vim.log.levels.ERROR)
                    goto continue
                end

                file_path = vim.fn.fnamemodify(file_path, ":p")
                last_container_files[container_id] = file_path

                local container_has_failure = false
                for _, r in pairs(members) do
                    if r.status == "failed" then
                        container_has_failure = true
                        break
                    end
                end
                local silent = not (
                    config.load_buffers or (config.load_only_buffers_with_error and container_has_failure)
                )

                local test_positions, container_line = adapter.find_test_positions(file_path, { silent = silent })
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

                for member_name, result in pairs(members) do
                    local line = test_positions[member_name]
                    if line == nil then
                        log.warn("no treesitter position for member: " .. member_name)
                        goto next_member
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
                                source = adapter.diagnostic_source,
                            }
                        end
                    end

                    ::next_member::
                end

                if #diagnostics > 0 then
                    vim.diagnostic.set(ns_diag, bufnr, diagnostics)
                end

                if container_line ~= nil then
                    local container_sign = sign_config[has_failure and "failed" or "passed"]
                    if container_sign then
                        vim.api.nvim_buf_set_extmark(bufnr, ns_signs, container_line, 0, {
                            sign_text = container_sign.text,
                            sign_hl_group = container_sign.hl,
                            virt_text = { { " " .. container_sign.text, container_sign.hl } },
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
            if has_any_failures and adapter.trouble_source then
                vim.cmd("Trouble " .. adapter.trouble_source .. " open")
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

            require("modules.common.test-report.report-view").refresh_if_open(M.get_report_snapshot())

            log.info(
                string.format(
                    "[perf process] total=%.1fms list=%.1fms parse=%.1fms byclass=%.1fms classloop=%.1fms(%d containers)",
                    (hr() - t_start) / 1e6,
                    (t_list - t_start) / 1e6,
                    (t_parse - t_list) / 1e6,
                    (t_byclass - t_parse) / 1e6,
                    (t_classloop - t_byclass) / 1e6,
                    total_containers
                )
            )
        end)

        if not ok then
            log.error("process error: " .. tostring(pcall_err))
            spinner.stop(false, "Test Report processing failed", sp_stop)
        elseif not spinner_resolved then
            log.warn("process exited without resolving spinner")
            spinner.cancel(sp)
        end

        log.info("process complete")
    end)
end

---@param filetype? string Override filetype detection (defaults to current buffer's).
function M.load_existing(filetype)
    local ft = filetype or vim.bo.filetype
    local adapter = registry.get(ft)
    if not adapter then
        vim.notify("test-report: no adapter for filetype: " .. ft, vim.log.levels.WARN)
        return
    end
    local report_dir = adapter.get_test_report_dir()
    local dirs = type(report_dir) == "table" and report_dir or { report_dir }
    local any_exists = false
    for _, d in ipairs(dirs) do
        if vim.fn.isdirectory(d) == 1 then
            any_exists = true
            break
        end
    end
    if not any_exists then
        vim.notify("test-report: no report dir found: " .. vim.inspect(dirs), vim.log.levels.WARN)
        return
    end
    M.clear()
    M.process(report_dir, ft)
end

function M.clear()
    process_generation = process_generation + 1
    spinner.cancel({ id = "test_report" })
    log.info("clear")
    for bufnr, _ in pairs(signed_buffers) do
        if vim.api.nvim_buf_is_valid(bufnr) then
            vim.api.nvim_buf_clear_namespace(bufnr, ns_signs, 0, -1)
        end
    end
    signed_buffers = {}
    vim.diagnostic.reset(ns_diag)
    last_results = {}
    last_positions = {}
    last_container_files = {}
    last_filetype = nil
    for _, adapter in pairs(loaded_adapters) do
        if adapter.clear_cache then
            adapter.clear_cache()
        end
    end
    if output_bufnr and vim.api.nvim_buf_is_valid(output_bufnr) then
        for _, win in ipairs(vim.api.nvim_list_wins()) do
            if vim.api.nvim_win_get_buf(win) == output_bufnr then
                vim.api.nvim_win_close(win, true)
            end
        end
    end
end

--- Cancel in-flight processing without wiping accumulated state.
function M.cancel()
    process_generation = process_generation + 1
    spinner.cancel({ id = "test_report" })
    -- Close any test-related Trouble views at test start; the right one (if any)
    -- will be reopened in process() once results are in.
    for _, adapter in pairs(loaded_adapters) do
        if adapter.trouble_source then
            pcall(vim.cmd, "Trouble " .. adapter.trouble_source .. " close")
        end
    end
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

local function find_member_at_cursor(file_path, cursor_line)
    local positions = last_positions[file_path]
    if not positions then
        return nil
    end
    local best_member, best_line = nil, -1
    for member, line in pairs(positions) do
        if line <= cursor_line and line > best_line then
            best_member = member
            best_line = line
        end
    end
    return best_member
end

local function find_container_id_for_file(file_path)
    for container_id, path in pairs(last_container_files) do
        if path == file_path then
            return container_id
        end
    end
    return nil
end

local function find_result_for_member(file_path, member_name)
    -- Precise lookup: resolve container_id from file, then use full key
    local container_id = find_container_id_for_file(file_path)
    if container_id then
        local key = container_id .. "#" .. member_name
        if last_results[key] then
            return last_results[key]
        end
    end
    -- Fallback: match by member name only
    for id, r in pairs(last_results) do
        local _, member = split_id(id)
        if member == member_name then
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

local function open_output(member_name, result)
    local lines = { "Test: " .. member_name, "Status: " .. result.status }
    if result.time then
        table.insert(lines, string.format("Time: %.3fs (total)", result.time))
    end
    table.insert(lines, "")

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
        if #result.invocations > 1 then
            local inv_label = string.format("[%d] %s (%s, %.3fs)", i, inv.name, inv.status, inv.time or 0)
            table.insert(lines, make_separator(inv_label))
            hl(inv.status == "failed" and "DiagnosticError" or "DiagnosticOk")
        end

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
end

function M.show_test_output()
    local file_path = vim.api.nvim_buf_get_name(0)
    local cursor_line = vim.api.nvim_win_get_cursor(0)[1] - 1 -- 0-indexed

    local best_member = find_member_at_cursor(file_path, cursor_line)

    local output_key = file_path .. "#" .. (best_member or "")
    if close_output_win() and output_method == output_key then
        return
    end

    if not last_positions[file_path] then
        vim.notify("test-report: no test results for this file", vim.log.levels.WARN)
        return
    end

    if not best_member then
        vim.notify("test-report: no test member found at cursor", vim.log.levels.WARN)
        return
    end

    local result = find_result_for_member(file_path, best_member)
    if not result then
        vim.notify("test-report: no output for " .. best_member, vim.log.levels.WARN)
        return
    end

    open_output(best_member, result)
    output_method = output_key
end

function M.hide_test_output()
    close_output_win()
    require("utils.dap-util").reset()
end

--- Show test output for a specific member and result (used by tree view).
---@param member_name string
---@param result test_report.TestResult
function M.show_output_for(member_name, result)
    close_output_win()
    open_output(member_name, result)
end

--- Return a snapshot of the last processed results for the tree view.
---@return test_report.Snapshot
function M.get_report_snapshot()
    return {
        results = last_results,
        positions = last_positions,
        container_files = last_container_files,
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
    require("modules.common.test-report.report-view").toggle(snapshot)
end

return M
