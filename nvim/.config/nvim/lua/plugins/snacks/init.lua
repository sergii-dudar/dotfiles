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
            local dashboards = require("plugins.snacks.configs.dashboards")
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
                bigfile = { enabled = true },
                dashboard = dashboards.default,
                indent = { enabled = true },
                input = { enabled = true },
                styles = {
                    input = {
                        position = "float", -- "float"|"bottom"|"top"|"left"|"right"|"current"
                        width = 40,
                        -- position near to renamed item
                        relative = "cursor", -- 'cursor'|'editor'|'laststatus'|'mouse'|'tabline'|'win'
                        row = -3, -- Row of the window. Use <1 for relative row. (default: center)
                        col = 0, -- Column of the window. Use <1 for relative column. (default: center)
                    },
                },
                notifier = { enabled = true },
                quickfile = { enabled = true },
                statuscolumn = { enabled = true },
                words = { enabled = true },
            })
        end,
        -- stylua: ignore
        keys = {
            -- { "<leader>S", desc = "Scratch ..." },
            -- { "<leader>SS", function() Snacks.scratch() end, desc = "Toggle Scratch Buffer" },
            -- { "<leader>Sf", function() Snacks.scratch.select() end, desc = "Select Scratch Buffer", },
            -- { "<leader>S", function() Snacks.scratch.select() end, desc = "Select Scratch Buffer", },
            -- { "<leader>.", function() Snacks.scratch() end, desc = "Toggle Scratch Buffer", },
            { "<leader>S", function() Snacks.scratch() end, desc = "Toggle Scratch Buffer", },
            -- explorer
            { '<leader>"', function() Snacks.explorer({ cwd = LazyVim.root() }) end, desc = "Explorer Snacks (root dir)", },
            { "<leader>'", function() Snacks.explorer() end, desc = "Explorer Snacks (cwd)", },
            -- pickets
            { "<leader>.", LazyVim.pick("files", { root = false }), desc = "Find Files (cwd)" },
            { "<leader>/", LazyVim.pick("live_grep", { root = false }), desc = "Grep (cwd)" },
            { "<leader>m", LazyVim.pick("oldfiles", { cwd = vim.uv.cwd() }), desc = "Recent (cwd)" },
            -- { "<leader><space>", LazyVim.pick("oldfiles", { cwd = vim.uv.cwd() }), desc = "Recent (cwd)" },
            --{ "<leader>fR", function() Snacks.picker.recent({ filter = { cwd = true }}) end, desc = "Recent (cwd)" },
            { "<leader><space>", function() Snacks.picker.recent({ filter = { cwd = true }}) end, desc = "Recent (cwd)" },
            { "<leader>sy", function() Snacks.picker.cliphist() end, desc = "Search Yanks (cliphist)", },
            { "<leader>dps", function() Snacks.profiler.scratch() end, desc = "Profiler Scratch Buffer", },
            -- replace `Snacks.picker.buffers()` to snipe - simpler for me, and need less actions to buffer navigation and managing
            { "<leader>,", function() require("snipe").open_buffer_menu() end, desc = "Open Snipe buffer menu", },
            -- { "<leader>ghb", function() Snacks.git.blame_line() end, desc = "Blame Line (Snacks)" },
        },
    },
}
