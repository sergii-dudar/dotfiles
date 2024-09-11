return {
    {
        "nvim-telescope/telescope.nvim",
        opts = function(_, opts)
            opts.defaults.path_display = { "smart" }

            local actions = require("telescope.actions")

            opts.defaults.mappings.i["<esc>"] = actions.close
            opts.defaults.mappings.i["<C-k>"] = actions.move_selection_previous
            opts.defaults.mappings.i["<C-j>"] = actions.move_selection_next
            opts.defaults.mappings.i["<C-q>"] = actions.send_selected_to_qflist + actions.open_qflist
            -- table.insert(opts.sources, { name = 'emoji' })
        end
    }
}