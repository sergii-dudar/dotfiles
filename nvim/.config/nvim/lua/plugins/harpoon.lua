return {
    "ThePrimeagen/harpoon",
    -- https://github.com/ThePrimeagen/harpoon/tree/harpoon2
    branch = "harpoon2",
    dependencies = { "nvim-lua/plenary.nvim", "nvim-telescope/telescope.nvim" },
    config = function()
        local harpoon = require('harpoon')
        harpoon:setup({})

        -- basic telescope configuration
        local conf = require("telescope.config").values
        local function toggle_telescope(harpoon_files)
            local file_paths = {}
            for _, item in ipairs(harpoon_files.items) do
                table.insert(file_paths, item.value)
            end

            require("telescope.pickers").new({}, {
                prompt_title = "Harpoon",
                finder = require("telescope.finders").new_table({
                    results = file_paths,
                }),
                previewer = conf.file_previewer({}),
                sorter = conf.generic_sorter({}),
            }):find()
        end

        vim.keymap.set("n", "<leader>ha", function() harpoon:list():add() end, { desc = "Harpoon add file" })
        vim.keymap.set("n", "<leader>hh>", function() toggle_telescope(harpoon:list()) end,{ desc = "Open harpoon window" })

        vim.keymap.set("n", "<leader>hj", function() harpoon:list():select(1) end)
        vim.keymap.set("n", "<leader>hk", function() harpoon:list():select(2) end)
        vim.keymap.set("n", "<leader>hl", function() harpoon:list():select(3) end)
        vim.keymap.set("n", "<leader>h;", function() harpoon:list():select(4) end)

        -- Toggle previous & next buffers stored within Harpoon list
        vim.keymap.set("n", "<leader>,", function() harpoon:list():prev() end)
        vim.keymap.set("n", "<leader>.", function() harpoon:list():next() end)

    end,
}