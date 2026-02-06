---@type overseer.ComponentFileDefinition
return {
    desc = "Pipe task output to DAP REPL/console",
    constructor = function(params)
        return {
            on_output_lines = function(self, task, lines)
                vim.schedule(function()
                    local ok, dap = pcall(require, "dap")
                    if ok and dap.session() then
                        for _, line in ipairs(lines) do
                            dap.repl.append(line)
                        end
                    end
                end)
            end,
        }
    end,
}
