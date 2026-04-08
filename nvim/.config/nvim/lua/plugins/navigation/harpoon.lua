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
    config = function(_, opts)
        local harpoon = require("harpoon")
        harpoon:setup(opts)

        -- Track last visited harpoon index for toggle
        local last_idx = nil
        local current_idx = nil

        harpoon:extend({
            SELECT = function(data)
                if data.idx ~= current_idx then
                    last_idx = current_idx
                    current_idx = data.idx
                end
            end,
        })

        local toggle_opts = {
            border = "rounded",
            title_pos = "center",
            ui_width_ratio = 0.60,
            title = " Harpoon ",
        }

        vim.keymap.set("n", "<leader>H", function()
            harpoon:list():add()
        end, { desc = "Harpoon File" })

        vim.keymap.set("n", "<leader>h", function()
            harpoon.ui:toggle_quick_menu(harpoon:list(), toggle_opts)
        end, { desc = "Harpoon Quick Menu" })

        -- vim.keymap.set("n", "<C-P>", function()
        --     harpoon:list():prev()
        -- end, { desc = "Harpoon prev" })

        vim.keymap.set("n", "<C-N>", function()
            harpoon:list():next()
        end, { desc = "Harpoon next" })

        vim.keymap.set("n", "<C-P>", function()
            if last_idx then
                harpoon:list():select(last_idx)
            end
        end, { desc = "Harpoon toggle last visited" })

        for i = 1, 9 do
            vim.keymap.set("n", "<leader>" .. i, function()
                harpoon:list():select(i)
            end, { desc = "Harpoon to File " .. i })
        end
    end,
}
