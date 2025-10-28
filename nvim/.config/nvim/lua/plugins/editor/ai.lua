return {
    {
        "folke/sidekick.nvim",
        opts = {
            debug = true,
            cli = {
                -- picker = "fzf-lua",
                -- picker = "snacks",
                mux = {
                    -- backend = vim.env.ZELLIJ and "zellij" or "tmux", -- default to tmux unless zellij is detected
                    backend = "tmux",
                    enabled = true,
                    -- terminal: new sessions will be created for each CLI tool and shown in a Neovim terminal
                    -- window: when run inside a terminal multiplexer, new sessions will be created in a new tab
                    -- split: when run inside a terminal multiplexer, new sessions will be created in a new split
                    -- NOTE: zellij only supports `terminal`
                    create = "window", ---@type "terminal"|"window"|"split"
                    split = {
                        vertical = true, -- vertical or horizontal split
                        size = 0.5, -- size of the split (0-1 for percentage)
                    },
                },
                debug = true,
                cli = {
                    -- picker = "fzf-lua",
                    -- win = { layout = "float" },
                    tools = {
                        debug = {
                            -- print env and read -p "any key to continue"
                            cmd = { "bash", "-c", "env | sort | bat -l env" },
                        },
                    },
                },
            },
        },
    },
}
