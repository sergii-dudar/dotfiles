return {
    "stevearc/oil.nvim",
    keys = {
        {
            "<leader>-",
            function()
                require("oil").toggle_float()
            end,
            desc = "Oil Toggle Float",
        },
    },
    config = function()
        local oil = require("oil")
        oil.setup({
            float = {
                -- Padding around the floating window
                padding = 5,
                -- max_width and max_height can be integers or a float between 0 and 1 (e.g. 0.4 for 40%)
                max_width = 0.5,
                max_height = 0,
                border = "rounded",
            },
        })
    end,
}
