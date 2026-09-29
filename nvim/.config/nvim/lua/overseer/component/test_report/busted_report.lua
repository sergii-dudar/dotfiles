local test_report = require("modules.lua.test-report")
local log = require("utils.logging-util").new({
    name = "test-report-component",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

---@type overseer.ComponentFileDefinition
return {
    desc = "Parse busted NDJSON output and display results as signs and diagnostics",
    params = {
        report_dir = {
            desc = "Path to the busted NDJSON report directory or directories",
        },
        filetype = {
            type = "string",
            desc = "Filetype used to resolve the language adapter",
        },
    },
    constructor = function(params)
        log.debug(
            "constructor: report_dir=" .. tostring(params.report_dir) .. " filetype=" .. tostring(params.filetype)
        )
        return {
            -- Before the process spawns: drop the previous run's report files so a run that
            -- dies before writing its own (crash, OOM) is reported as "no results", not as
            -- the old results. Must not return false (that would veto the start).
            on_pre_start = function(self, task)
                test_report.prepare_run(params.report_dir, params.filetype)
            end,

            on_complete = function(self, task, status)
                log.debug("on_complete: status=" .. tostring(status) .. " report_dir=" .. tostring(params.report_dir))
                if status == require("overseer").STATUS.CANCELED then
                    -- A stopped run leaves stale (or partial) reports on disk; don't present
                    -- them as fresh results. cancel() also clears the tree view's running marks.
                    log.debug("on_complete: run canceled, skipping report processing")
                    test_report.cancel()
                    return
                end
                vim.schedule(function()
                    test_report.process(params.report_dir, params.filetype)
                end)
            end,

            on_reset = function(self, task)
                log.debug("on_reset")
                test_report.cancel()
            end,

            on_dispose = function(self, task)
                log.debug("on_dispose")
                test_report.cancel()
            end,
        }
    end,
}
