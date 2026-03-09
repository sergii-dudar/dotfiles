return {
    {
        "folke/sidekick.nvim",
        -- stylua: ignore
        keys = {
            { "<a-a>", false },
            { "<tab>", false },
            -- { "<leader>aa", function() require("sidekick.cli").toggle({ name = "copilot", focus = true }) end, desc = "Toggle Copilot CLI", },
            { "<leader>aa", function() require("sidekick.cli").toggle({ name = "claude", focus = true }) end, desc = "Toggle Copilot CLI", },
            -- { "<leader>an", function() require("sidekick").nes_jump_or_apply() end, desc = "Goto/Apply Next Edit Suggestion" },
            -- { "<leader>an", function() require("sidekick.nes").jump() end, desc = "Goto Next Edit Suggestion" },
            { "<leader>an", function() require("sidekick.nes").apply() end, desc = "Apply Current Edit Suggestion" },
        },
        opts = {
            debug = false,
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
    --[[ {
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
    }, ]]
    {
        "piersolenski/wtf.nvim",
        dependencies = {
            "nvim-lua/plenary.nvim",
            "MunifTanjim/nui.nvim",
            -- Optional: For WtfGrepHistory (pick one)
            -- "nvim-telescope/telescope.nvim",
            "folke/snacks.nvim",
            -- "ibhagwan/fzf-lua",
        },
        opts = {
            popup_type = "horizontal", -- "popup" | "horizontal" | "vertical",
            -- provider = "anthropic", -- "anthropic" | "copilot" | "deepseek" | "gemini" | "grok" | "ollama" | "openai",
            provider = "copilot",
            providers = {
                anthropic = {
                    url = "https://genai-sbox.rbi.tech/v1/messages",
                    api_key = function()
                        local claude_env = os.getenv("HOME") .. "/.claude/.env"
                        return require("utils.envs-util").load_env_file_variable(claude_env, "ANTHROPIC_API_KEY")
                    end,
                    -- model_id = "claude-opus-4-6",
                    model_id = "claude-sonnet-4-6",
                },
                copilot = {
                    model_id = "gpt-4o",
                    -- model_id = "claude-sonnet-4.5",
                },
            },
            search_engine = "google", -- "google" | "duck_duck_go" | "stack_overflow" | "github" | "phind" | "perplexity",
            picker = "snacks", -- "telescope" | "snacks" | "fzf-lua",
            hooks = {
                request_started = nil,
                request_finished = function()
                    if vim.bo.filetype == "markdown" then
                        -- means pupup with diagnostics answer opened
                        local bufnr = vim.api.nvim_get_current_buf()
                        vim.keymap.set("n", "q", function()
                            vim.api.nvim_buf_delete(bufnr, { force = true })
                        end, { buffer = bufnr, desc = "Close wtf.nvim popup" })
                    end
                end,
            },
        },
        -- stylua: ignore
        keys = {
            { "<leader>wd", mode = { "n", "x" }, function() require("wtf").diagnose() end, desc = "Debug diagnostic with AI", },
            { "<leader>wf", mode = { "n", "x" }, function() require("wtf").fix() end, desc = "Fix diagnostic with AI", },
            { mode = { "n" }, "<leader>ws", function() require("wtf").search() end, desc = "Search diagnostic with Google", },
            { mode = { "n" }, "<leader>wp", function() require("wtf").pick_provider() end, desc = "Pick provider", },
            { mode = { "n" }, "<leader>wh", function() require("wtf").history() end, desc = "Populate the quickfix list with previous chat history", },
            { mode = { "n" }, "<leader>wg", function() require("wtf").grep_history() end, desc = "Grep previous chat history with Telescope", },
        },
    },
}