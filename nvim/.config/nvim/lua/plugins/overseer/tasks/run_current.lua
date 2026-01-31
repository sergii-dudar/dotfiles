return {
    name = "run currenta2",
    builder = function()
        local file = vim.fn.expand("%:p")
        local cmd = { file }
        if vim.bo.filetype == "go" then
            cmd = { "go", "run", file }
        elseif vim.bo.filetype == "python" then
            cmd = { "python", file }
        elseif vim.bo.filetype == "cpp" then
            local fileNameWithoutExt = vim.fn.expand("%:t:r")
            local fileDir = vim.fn.expand("%:p:h")
            cmd = {
                "sh",
                "-c",
                "g++ -std=c++23 "
                    .. fileDir
                    .. "/"
                    .. fileNameWithoutExt
                    .. "*.cpp -o /tmp/"
                    .. fileNameWithoutExt
                    .. " && /tmp/"
                    .. fileNameWithoutExt
                    .. " && rm /tmp/"
                    .. fileNameWithoutExt,
            }
        end
        return {
            cmd = cmd,
            -- add some components that will pipe the output to quickfix,
            -- parse it using errorformat, and display any matching lines as diagnostics.
            components = {
                { "on_output_quickfix", set_diagnostics = true },
                "on_result_diagnostics",
                "default",
            },
        }
    end,
    condition = {
        filetype = { "sh", "python", "go", "cpp" },
    },
}