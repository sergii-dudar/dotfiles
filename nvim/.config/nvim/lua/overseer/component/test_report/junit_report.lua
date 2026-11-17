local test_report = require("modules.java.test-report")
local log = require("utils.logging-util").new({
    name = "test-report-component",
    filename = "test-report.log",
    level = vim.log.levels.DEBUG,
})

---@type overseer.ComponentFileDefinition
return {
    desc = "Parse JUnit XML reports and display test results as signs and diagnostics",
    params = {
        report_dir = {
            desc = "Path to the JUnit XML report directory or directories",
        },
        filetype = {
            type = "string",
            desc = "Filetype used to resolve the language adapter",
        },
        scope = {
            type = "opaque",
            optional = true,
            desc = "What the run covers (test_report.RunScope); nil = the whole report",
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

            -- The process is spawned: the tree view shows the run as in flight until this task
            -- completes, is canceled, reset or disposed.
            on_start = function(self, task)
                test_report.run_started(task.id, params.filetype, params.scope)
            end,

            on_complete = function(self, task, status)
                log.debug("on_complete: status=" .. tostring(status) .. " report_dir=" .. tostring(params.report_dir))
                if status == require("overseer").STATUS.CANCELED then
                    -- A stopped run leaves stale (or partial) reports on disk; don't present
                    -- them as fresh results. cancel() also clears the tree view's running marks.
                    log.debug("on_complete: run canceled, skipping report processing")
                    test_report.cancel(task.id)
                    return
                end
                vim.schedule(function()
                    test_report.process(params.report_dir, params.filetype, task.id)
                end)
            end,

            on_reset = function(self, task)
                log.debug("on_reset")
                test_report.cancel(task.id)
            end,

            on_dispose = function(self, task)
                log.debug("on_dispose")
                test_report.cancel(task.id)
            end,
        }
    end,
}
