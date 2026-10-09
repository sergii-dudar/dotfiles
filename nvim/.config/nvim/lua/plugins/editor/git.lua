return {
    -- multi-buffer diff view
    -- opened as a bottom panel through utils/zdiff-util.lua: <CR> sends the file to the editor window and keeps
    -- the diff visible; `:ZdiffPanel [ref]` (config/autocmds.lua) is the command form, while the plugin's own
    -- `:Zdiff` keeps its in-window behaviour
    {
        "martindur/zdiff.nvim",
        cmd = "Zdiff",
        keys = {
            {
                "<leader>zd",
                function()
                    require("utils.zdiff-util").open()
                end,
                desc = "Zdiff panel (uncommitted)",
            },
            {
                "<leader>zD",
                function()
                    require("utils.zdiff-util").open_default_branch()
                end,
                desc = "Zdiff panel (vs default branch)",
            },
        },
        opts = {},
    },
    {
        "lewis6991/gitsigns.nvim",
        keys = {
            { "<leader>gt", ":Gitsigns toggle_current_line_blame<CR>", desc = "Toggle Line Blame" },
        },
    },
    -- https://github.com/chojs23/ec
    -- very perspective with separated TUI tool to resolve conflics, can be runned in shell directly by `ec`
    -- stays the `git mergetool` (and lazygit's `M` in the conflicts panel); diffview below is the in-editor 3-way view
    {
        "chojs23/ec",
        keys = {
            { "<leader>gr", ":Ec<CR>", desc = "Open ec (easy-conflict)" },
        },
    },
    -- https://github.com/StackInTheWild/headhunter.nvim
    -- another simple nice plugin to resolve merge conflicts.
    --[[ {
        "StackInTheWild/headhunter.nvim",
        config = function()
            require("headhunter").setup()
        end,
    }, ]]
    -- https://github.com/spacedentist/resolve.nvim
    -- simple nice plugin to resolve merge conflicts.
    --[[ {
        "spacedentist/resolve.nvim",
        event = { "BufReadPre", "BufNewFile" },
        opts = {},
    }, ]]
    -- https://github.com/sindrets/diffview.nvim
    -- merge tool: `<leader>gm` (:DiffviewOpen) during a merge/rebase lists the conflicted files in the file panel
    -- and opens each as OURS | RESULT | THEIRS (`diff3_horizontal`, the IntelliJ layout; `diff4_mixed` adds BASE).
    -- `]x` / `[x` jump between conflicts, `<leader>co` / `ct` / `cb` / `ca` take ours / theirs / base / all for the
    -- hunk under the cursor (`<leader>cO` / `cT` / `cB` / `cA` in the file panel: whole file), `dx` drops the conflict
    -- region. Not wired as `git mergetool` (mergetool runs once per file, diffview handles the whole merge):
    -- resolve, `q` / :DiffviewClose, then `git add` / continue. Also a plain diff view (`:DiffviewOpen <ref>`,
    -- `:DiffviewOpen HEAD~1`) and file history (`<leader>gM` / `:DiffviewFileHistory [%]`).
    -- Lightly maintained since 2024 but works on nvim 0.12.
    {
        "sindrets/diffview.nvim",
        cmd = { "DiffviewOpen", "DiffviewClose", "DiffviewFileHistory", "DiffviewToggleFiles", "DiffviewFocusFiles" },
        keys = {
            { "<leader>gm", "<cmd>DiffviewOpen<CR>", desc = "Diffview: merge conflicts / working tree" },
            { "<leader>gM", "<cmd>DiffviewFileHistory %<CR>", desc = "Diffview: current file history" },
        },
        opts = {
            enhanced_diff_hl = true,
            view = {
                merge_tool = {
                    layout = "diff3_horizontal", -- OURS | RESULT | THEIRS
                    disable_diagnostics = true,
                    winbar_info = true, -- OURS / RESULT / THEIRS labels on top of each window
                },
            },
            keymaps = {
                view = { { "n", "q", "<cmd>DiffviewClose<CR>", { desc = "Close diffview" } } },
                file_panel = { { "n", "q", "<cmd>DiffviewClose<CR>", { desc = "Close diffview" } } },
                file_history_panel = { { "n", "q", "<cmd>DiffviewClose<CR>", { desc = "Close diffview" } } },
            },
        },
    },

    -- {
    --     "akinsho/git-conflict.nvim",
    --     version = "*",
    --     config = true,
    -- },
}
