return {
    "crnvl96/lazydocker.nvim",
    -- stylua: ignore
    keys = {
        { "<leader>dd", function() require("lazydocker").toggle({ engine = "docker" }) end, desc = "LazyDocker (docker)", },
    },
    opts = {
        window = {
            settings = {
                width = 0.9, -- Percentage of screen width (0 to 1)
                height = 1, -- Percentage of screen height (0 to 1)
                border = "rounded", -- See ':h nvim_open_win' border options
                relative = "editor", -- See ':h nvim_open_win' relative options
            },
        },
    },
}