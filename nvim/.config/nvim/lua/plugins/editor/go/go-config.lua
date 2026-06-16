local goft = { "go" }

local wk_augroup = vim.api.nvim_create_augroup("rust_config_which_key", { clear = true })
vim.api.nvim_create_autocmd("FileType", {
    group = wk_augroup,
    pattern = goft,
    callback = function(ev)
        ---@diagnostic disable-next-line: undefined-field
        if vim.bo[ev.buf].buftype ~= "" then
            return
        end
        require("which-key").add({
            ---@diagnostic disable-next-line: undefined-field
            buffer = ev.buf,
            { "<leader>j", group = "+go" },
            { "<leader>jc", group = "+go code/compile" },
            { "<leader>jd", group = "+go errors/diagnostics" },
            { "<leader>jo", group = "+go open" },
        })
    end,
})

return {
    {
        "ray-x/go.nvim",
        dependencies = { -- optional packages
            "ray-x/guihua.lua",
            "neovim/nvim-lspconfig",
            -- { "nvim-treesitter/nvim-treesitter", branch = 'main' } -- optional for master version
        },
        keys = {
            -- actions
            {
                "<leader>cc",
                function()
                    local action_names = require("utils.lang.go.lsp-go").code_action_auto_resolve_match_names
                    require("utils.lsp-util").code_action.resolve_context(action_names)
                end,
                ft = goft,
                desc = "Context Apply First Code Action [rust-analyzer]",
            },
        },
        opts = function()
            require("go").setup(opts)
            local format_sync_grp = vim.api.nvim_create_augroup("GoFormat", {})
            vim.api.nvim_create_autocmd("BufWritePre", {
                pattern = "*.go",
                callback = function()
                    require("go.format").goimports()
                end,
                group = format_sync_grp,
            })
            return {
                -- lsp_keymaps = false,
                -- other options
            }
        end,
        event = { "CmdlineEnter" },
        ft = { "go", "gomod" },
        build = ':lua require("go.install").update_all_sync()', -- if you need to install/update all binaries
    },
}
