return {
    "ThePrimeagen/harpoon",
    branch = "harpoon2",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {
        menu = {
            width = vim.api.nvim_win_get_width(0) - 4,
            border = "rounded",
            title_pos = "center",
        },
        settings = {
            save_on_toggle = true,
        },
    },
    keys = function()
        local toggle_opts = {
            border = "rounded",
            title_pos = "center",
            ui_width_ratio = 0.60,
            title = " Harpoon ",
        }
        local keys = {
            {
                "<leader>H",
                function()
                    require("harpoon"):list():add()
                end,
                desc = "Harpoon File",
            },
            {
                "<leader>h",
                function()
                    local harpoon = require("harpoon")
                    harpoon.ui:toggle_quick_menu(harpoon:list(), toggle_opts)
                end,
                desc = "Harpoon Quick Menu",
            },
            {
                "<C-P>",
                function()
                    require("harpoon"):list():prev()
                end,
                desc = "Harppon toggle previous",
            },
            {
                "<C-N>",
                function()
                    require("harpoon"):list():next()
                end,
                desc = "Harppon toggle previous",
            },
        }

        for i = 1, 9 do
            table.insert(keys, {
                "<leader>" .. i,
                function()
                    require("harpoon"):list():select(i)
                end,
                desc = "Harpoon to File " .. i,
            })
        end
        return keys
    end,
}
