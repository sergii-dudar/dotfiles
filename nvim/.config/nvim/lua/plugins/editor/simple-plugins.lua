return {
    --{ "mg979/vim-visual-multi" },
    {
        -- "norcalli/nvim-colorizer.lua",
        "catgoose/nvim-colorizer.lua", -- fork with support of AARRGGBB and another color formats
        config = function()
            require("colorizer").setup({
                options = {
                    parsers = {
                        rrggbbaa = true, -- #RRGGBBAA hex codes
                        aarrggbb = true, -- 0xAARRGGBB hex codes
                    },
                },
            })
        end,
    },
    -- translate
    {
        "acidsugarx/babel.nvim",
        dependencies = {
            "folke/snacks.nvim",
        },
        version = "*", -- recomended for the latest tag, not main
        opts = {
            target = "uk", -- target language
            provider = "google",
            display = "float", -- "float" or "picker"
            picker = "snacks", -- "auto", "telescope", "fzf", "snacks", "mini"
            keymaps = {
                translate = "<leader>tr",
                translate_word = "<leader>tw",
            },
        },
        keys = {
            { "<leader>tr", mode = "v", desc = "Translate selection" },
            { "<leader>tw", desc = "Translate word" },
        },
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
    -- Keep long URLs out of your way in (Neo)Vim (livesaver for javadoc with long jdt urls)
    { "qadzek/link.vim" },
    -- {
    --     "OXY2DEV/markview.nvim",
    --     lazy = false,
    --
    --     -- Completion for `blink.cmp`
    --     dependencies = { "saghen/blink.cmp" },
    -- },
    {
        "MeanderingProgrammer/render-markdown.nvim",
        -- enabled = false, -- This plugin uses extmarks to avoid reserved space issues
        ft = { "markdown" },
        dependencies = { "nvim-treesitter/nvim-treesitter", "nvim-mini/mini.nvim" },
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
            -- anti_conceal = {
            --     enabled = true,
            -- },
            -- Link rendering: show only link text, hide URL, no empty space
            link = {
                enabled = true, -- Enable to properly handle links without reserved space
                -- hyperlink = "Underlined",
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
    -- { "skywind3000/asyncrun.vim" },
}