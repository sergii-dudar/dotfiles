return {
    --{ "mg979/vim-visual-multi" },
    {
        -- "norcalli/nvim-colorizer.lua",
        "catgoose/nvim-colorizer.lua", -- fork with support of AARRGGBB and another color formats
        config = function()
            require("colorizer").setup({
                user_default_options = {
                    RRGGBBAA = true, -- #RRGGBBAA hex codes
                    AARRGGBB = true, -- 0xAARRGGBB hex codes
                },
            })
        end,
    },
    {
        "hedyhli/outline.nvim",
        opts = {
            preview_window = {
                auto_preview = true,
                width = 50, -- Percentage or integer of columns
                min_width = 50, -- Minimum number of columns
                height = 80, -- Percentage or integer of lines
                min_height = 10, -- Minimum number of lines
            },
            outline_window = {
                show_relative_numbers = true,
            },
        },
    },
    {
        "unblevable/quick-scope",
    },
    {
        "folke/flash.nvim",
        event = "VeryLazy",
        opts = {
            modes = {
                char = {
                    enabled = false, -- disable in favor of `quick-scope`
                    -- jump_labels = true, # f,F,t,T...
                    -- jump = {
                    --     autojump = true,
                    -- },
                },
            },
            search = {
                enabled = true,
            },
            -- jump = {
            --     -- automatically jump when there is only one match
            --     autojump = true,
            -- },
        },
    },
    {
        "MeanderingProgrammer/render-markdown.nvim",
        dependencies = { "nvim-treesitter/nvim-treesitter", "nvim-mini/mini.nvim" }, -- if you use the mini.nvim suite
        opts = {
            heading = {
                -- Highlight for the heading icon and extends through the entire line.
                -- Output is evaluated by `clamp(value, context.level)`.
                backgrounds = {
                    --"RenderMarkdownH1Bg",
                    "RenderMarkdownH2Bg",
                    "RenderMarkdownH3Bg",
                    "RenderMarkdownH4Bg",
                    "RenderMarkdownH5Bg",
                    "RenderMarkdownH6Bg",
                },
                -- Highlight for the heading and sign icons.
                -- Output is evaluated using the same logic as 'backgrounds'.
                foregrounds = {
                    --"RenderMarkdownH1",
                    "RenderMarkdownH2",
                    "RenderMarkdownH3",
                    "RenderMarkdownH4",
                    "RenderMarkdownH5",
                    "RenderMarkdownH6",
                },
            },
        },
    },
    {
        "phelipetls/jsonpath.nvim",
        ft = { "json", "jsonc" },
        keys = {
            {
                "<leader>cp",
                function()
                    local path = require("jsonpath").get()
                    vim.fn.setreg("+", path)
                    vim.notify("`" .. path .. "`\n copied to clipboard", vim.log.levels.INFO)
                end,
                { desc = "copy json path", buffer = true },
            },
        },
    },
    {
        "vinnymeller/swagger-preview.nvim",
        ft = { "yaml", "yml" },
        cmd = { "SwaggerPreview", "SwaggerPreviewStop", "SwaggerPreviewToggle" },
        keys = {
            { "<leader>ot", ":SwaggerPreviewToggle<CR>", desc = "Swagger Preview" },
        },
        build = "npm i",
        config = true,
    },
    -- { "nvim-mini/mini.nvim", version = "*" },
    {
        "nvim-mini/mini.cmdline",
        opts = {
            autocomplete = {
                enable = false,
            },
            -- show command's target range in a floating window
            autopeek = {
                enable = true,
                -- Number of lines to show above and below range lines
                n_context = 1,
            },
        },
    },
    -- { "skywind3000/asyncrun.vim" },
}
