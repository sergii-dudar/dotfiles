local M = {}

M.config = {
    win = {
        width = 0.7,
        height = 0.9,
        keys = {
            ["parse_trace"] = {
                "<leader>p",
                function(self)
                    local java_trace = require("utils.java.java-trace")
                    local common = require("utils.common-util")
                    local stack_trace = common.get_current_buffer_text()
                    self:close()
                    java_trace.show_stack_trace_qflist(stack_trace)
                end,
                desc = "Trace to QF",
                mode = "n",
            },
        },
    },
}

return M