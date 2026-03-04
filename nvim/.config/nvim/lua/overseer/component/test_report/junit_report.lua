local test_report = require("plugins.overseer.test-report")
local log = require("utils.logging-util").new({ name = "test-report-component", filename = "test-report.log", level = vim.log.levels.DEBUG })

---@type overseer.ComponentFileDefinition
return {
    desc = "Parse JUnit XML reports and display test results as signs and diagnostics",
    params = {
        report_dir = {
            type = "string",
            desc = "Path to the JUnit XML report directory",
        },
        filetype = {
            type = "string",
            desc = "Filetype used to resolve the language adapter",
        },
    },
    constructor = function(params)
        log.debug("constructor: report_dir=" .. tostring(params.report_dir) .. " filetype=" .. tostring(params.filetype))
        return {
            on_complete = function(self, task, status)
                log.debug("on_complete: status=" .. tostring(status) .. " report_dir=" .. tostring(params.report_dir))
                vim.schedule(function()
                    test_report.process(params.report_dir, params.filetype)
                end)
            end,

            on_reset = function(self, task)
                log.debug("on_reset")
                test_report.clear()
            end,

            on_dispose = function(self, task)
                log.debug("on_dispose")
                test_report.clear()
            end,
        }
    end,
}
