return {
    "stevearc/oil.nvim",
    enabled = false,
    config = function()
        local oil = require("oil")
        oil.setup()
        vim.keymap.set("n", "-", oil.toggle_float, {})
    end,
}
