return {
    {
        "folke/sidekick.nvim",
        keys = {
            { "<a-a>", false },
            {
                "<leader>aa",
                function()
                    require("sidekick.cli").toggle({ name = "copilot", focus = true })
                end,
                desc = "Toggle Copilot CLI",
            },
        },
        opts = {
            debug = true,
            nes = {
                enabled = false, -- disable Next Edit Suggestions as I don't like it, I need only CLI AI Chat
            },
            cli = {
                picker = "snacks",
                win = {
                    keys = {
                        files = { "<c-g>", "files", mode = "nt", desc = "open file picker" },
                    },
                },
                mux = {
                    -- backend = vim.env.ZELLIJ and "zellij" or "tmux", -- default to tmux unless zellij is detected
                    backend = "tmux",
                    enabled = true,
                    -- terminal: new sessions will be created for each CLI tool and shown in a Neovim terminal
                    -- window: when run inside a terminal multiplexer, new sessions will be created in a new tab
                    -- split: when run inside a terminal multiplexer, new sessions will be created in a new split
                    -- NOTE: zellij only supports `terminal`
                    create = "terminal", ---@type "terminal"|"window"|"split"
                    split = {
                        vertical = true, -- vertical or horizontal split
                        size = 0.5, -- size of the split (0-1 for percentage)
                    },
                },
                debug = false,
            },
        },
    },
    {
        "saghen/blink.cmp",
        opts = {
            sources = {
                providers = {
                    copilot = {
                        -- score_offset = 100,
                        score_offset = -1, -- lsp is top priority
                    },
                },
            },
        },
    },
}
