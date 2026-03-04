local junit_xml = require("plugins.overseer.test-report.junit-xml")
local log = require("utils.logging-util").new({
    name = "test-report",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

local M = {}

---@class test_report.TestResult
---@field status "passed"|"failed"|"skipped"
---@field errors? { message: string, line: number|nil }[]

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

-- Track buffers where we placed signs for efficient cleanup
local signed_buffers = {}

---@param report_dir string
---@param filetype string
function M.process(report_dir, filetype)
    log.info("process: report_dir=" .. tostring(report_dir) .. " ft=" .. tostring(filetype))

    local adapter_fn = lang_adapters[filetype]
    if not adapter_fn then
        log.warn("no adapter for filetype: " .. tostring(filetype))
        vim.notify("No test-report adapter for filetype: " .. filetype, vim.log.levels.WARN)
        return
    end
    local adapter = adapter_fn()

    local results = junit_xml.parse_report_dir(report_dir)
    if vim.tbl_isempty(results) then
        log.warn("no results from XML parsing")
        return
    end

    log.info("parsed " .. vim.tbl_count(results) .. " test results")
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
            log.debug("test_positions: ", test_positions)
            log.debug("class_line: " .. tostring(class_line))

            -- Resolve buffer number (find_test_positions ensures buffer is loaded)
            local bufnr = vim.fn.bufnr(file_path)
            log.debug("bufnr: " .. bufnr)
            if bufnr == -1 then
                log.warn("buffer not found for: " .. file_path)
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
            log.warn("classname_to_file returned nil for: " .. classname)
        end
    end

    if #qf_entries > 0 then
        vim.fn.setqflist(qf_entries, "r")
        log.debug("set " .. #qf_entries .. " quickfix entries")
    end
    log.info("process complete")
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
end

return M