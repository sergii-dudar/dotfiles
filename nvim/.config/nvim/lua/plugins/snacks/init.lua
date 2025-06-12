--[[
    { "<leader>si", function() Snacks.picker.icons() end, desc = "Icons" },
    { "<leader>:", function() Snacks.picker.command_history() end, desc = "Command History" },
    { '<leader>s"', function() Snacks.picker.registers() end, desc = "Registers" },
    { '<leader>s/', function() Snacks.picker.search_history() end, desc = "Search History" },
    { "<leader>su", function() Snacks.picker.undo() end, desc = "Undotree" },
    { "<leader>sR", function() Snacks.picker.resume() end, desc = "Resume" },
]]

return {
    {
        "folke/snacks.nvim",
        opts = function(_, opts)
            -- vim.schedule(function()
            --     vim.api.nvim_set_hl(0, "SnacksPickerPathHidden", { link = "SnacksPickerPathNormal" })
            -- end)
            local pickers = require("plugins.snacks.configs.pickers")
            return vim.tbl_deep_extend("force", opts or {}, {
                zen = {
                    win = { style = "zen", width = 0.85 },
                },
                picker = pickers.picker,
                explorer = pickers.explorer,
                image = {
                    force = true,
                    enabled = true,
                    debug = { request = false, convert = false, placement = false },
                    math = { enabled = true },
                    doc = {
                        enabled = true,
                        inline = true,
                        float = true,
                        max_width = 80,
                        max_height = 40,
                    },
                    -- doc = { enabled = true, inline = true, float = false },
                },
                scroll = { enabled = false },
                -- bigfile = { enabled = true },
                dashboard = { enabled = true },
                -- indent = { enabled = true },
                -- input = { enabled = true },
                -- notifier = { enabled = true },
                quickfile = { enabled = true },
                statuscolumn = { enabled = true },
                words = { enabled = true },
            })
        end,
        keys = {
            -- {
            --     "<leader>.",
            --     function()
            --         Snacks.scratch()
            --     end,
            --     desc = "Toggle Scratch Buffer",
            -- },
            -- {
            --     "<leader>S",
            --     function()
            --         Snacks.scratch.select()
            --     end,
            --     desc = "Select Scratch Buffer",
            -- },
            { "<leader>.", LazyVim.pick("files", { root = false }), desc = "Find Files (cwd)" },
            { "<leader>/", LazyVim.pick("live_grep", { root = false }), desc = "Grep (cwd)" },
            { "<leader>m", LazyVim.pick("oldfiles", { cwd = vim.uv.cwd() }), desc = "Recent (cwd)" },
            { "<leader><space>", LazyVim.pick("oldfiles", { cwd = vim.uv.cwd() }), desc = "Recent (cwd)" },
            --{ "<leader>fR", function() Snacks.picker.recent({ filter = { cwd = true }}) end, desc = "Recent (cwd)" },
            {
                "<leader>sy",
                function()
                    Snacks.picker.cliphist()
                end,
                desc = "Search Yanks (cliphist)",
            },
            {
                "<leader>dps",
                function()
                    Snacks.profiler.scratch()
                end,
                desc = "Profiler Scratch Buffer",
            },
        },
    },
}
