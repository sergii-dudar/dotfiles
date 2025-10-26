return { -- just for testing purposes of migration java.nvim to neo-tree
    "nvim-tree/nvim-tree.lua",
    config = function()
        require("nvim-tree").setup({
            update_focused_file = {
                enable = true,
                -- Other options for update_focused_file can be added here if needed
            },
            view = {
                centralize_selection = true,
                number = false,
                relativenumber = false,
            },
            renderer = {
                group_empty = true,
            },
        })
    end,
}
