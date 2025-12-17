return {
    "mbbill/undotree",
    keys = {
        {
            "<leader>uu",
            function()
                local before = vim.api.nvim_get_current_win()
                vim.cmd.UndotreeToggle()
                local after = vim.api.nvim_get_current_win()
                if before == after then
                    vim.cmd("wincmd w")
                end
            end,
            desc = "Undotree Toggle",
        },
    },
}