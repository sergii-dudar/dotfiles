-- External tools this git setup relies on (git side configured in ~/dotfiles/git/.gitconfig).
-- Install: macOS `brew install <name>`; Arch `pacman -S <name>` (delta is `git-delta` there, ec comes from the AUR or
-- the install script in its README):
--   git >= 2.35   zdiff3 conflict markers (`merge.conflictstyle`), diffview needs >= 2.31
--   mergiraf      syntax-aware merge driver (`* merge=mergiraf` in ~/.gitattributes_global): auto-resolves import /
--                 adjacent-method conflicts before any UI sees them
--   ec            TUI conflict resolver: `git mergetool`, `:Ec` / <leader>gr here, lazygit's `M`; its `e` opens nvim
--                 with diffview (scripts/git/ec-editor.sh)
--   lazygit       <leader>gg (snacks.lazygit): quick conflict picks, staging, commits
--   delta         diff pager for git and lazygit (`core.pager`, lazygit/config.yml)
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
        opts = {
            float = {
                width = 1, -- 0.92,
                height = 1, -- 0.86,
                border = "none", -- [ "rounded", "none" ] no frame around the full-size float (ec draws its own pane borders)
                -- title = "ec",
                title = "", -- nvim rejects a float title without a border
                title_pos = "center",
                zindex = 50,
            },
        },
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
    -- `diff3_mixed` (OURS | THEIRS over a full-width RESULT) was tried and dropped: RESULT is not diffed against the
    -- sides there, so the changes are not visible. `g<C-x>` cycles layouts in an open view.
    -- Conflict keys (buffer-local in the view and the file panel): `]x` / `[x` jump, `<M-o>` / `<M-t>` / `<M-b>` /
    -- `<M-a>` take OURS / THEIRS / BASE / all for the conflict under the cursor, the same keys on a file in the file
    -- panel take it for the whole file, `dx` drops the conflict region. The plugin's `<leader>co` / `ct` / `cb` / `ca`
    -- (+ `cO` / `cT` / `cB` / `cA`) defaults are disabled: LSP keymaps (LazyVim defaults + server `keys`) are applied
    -- per buffer by Snacks.keymap on attach and again on every capability registration, i.e. after diffview mapped the
    -- RESULT buffer, so in a Java file `<leader>co` (organize imports), `<leader>ca` / `<leader>cA` (code / source
    -- action) replaced them; and diffview deletes every lhs it knows on close, which took the LSP keys with it.
    -- `<M-b>` shadows multicursor's "add cursor above" only inside diffview buffers.
    -- The file panel starts hidden (`view_opened` hook): `<leader>b` / :DiffviewToggleFiles shows it, `<leader>e`
    -- focuses it, `<Tab>` / `<S-Tab>` switch files without it (conflicts come first in that order).
    -- Not wired as `git mergetool` (mergetool runs once per file, diffview handles the whole merge): resolve,
    -- `q` / :DiffviewClose, then `git add` / continue. Also a plain diff view (`:DiffviewOpen <ref>`,
    -- `:DiffviewOpen HEAD~1`) and file history (`<leader>gM` / `:DiffviewFileHistory [%]`).
    -- Lightly maintained since 2024 but works on nvim 0.12.
    -- `]c` / `[c` are Neovim's own diff-hunk jumps (every difference between two windows, not only conflict blocks);
    -- `<Tab>` / `<S-Tab>` switch to the next / previous file and land on its first conflict.
    {
        "sindrets/diffview.nvim",
        cmd = { "DiffviewOpen", "DiffviewClose", "DiffviewFileHistory", "DiffviewToggleFiles", "DiffviewFocusFiles" },
        keys = {
            { "<leader>gm", "<cmd>DiffviewOpen<CR>", desc = "Diffview: merge conflicts / working tree" },
            { "<leader>gM", "<cmd>DiffviewFileHistory %<CR>", desc = "Diffview: current file history" },
        },
        opts = function()
            local actions = require("diffview.actions")
            local close = { "n", "q", "<cmd>DiffviewClose<CR>", { desc = "Close diffview" } }
            local disabled_defaults = {
                ["<leader>co"] = false,
                ["<leader>ct"] = false,
                ["<leader>cb"] = false,
                ["<leader>ca"] = false,
                ["<leader>cO"] = false,
                ["<leader>cT"] = false,
                ["<leader>cB"] = false,
                ["<leader>cA"] = false,
            }
            local function conflict_keys(choose, scope)
                return {
                    { "n", "<M-o>", choose("ours"), { desc = "Conflict: take OURS " .. scope } },
                    { "n", "<M-t>", choose("theirs"), { desc = "Conflict: take THEIRS " .. scope } },
                    { "n", "<M-b>", choose("base"), { desc = "Conflict: take BASE " .. scope } },
                    { "n", "<M-a>", choose("all"), { desc = "Conflict: take all " .. scope } },
                }
            end
            local view = vim.list_extend({ close }, conflict_keys(actions.conflict_choose, "(hunk)"))
            local file_panel = vim.list_extend({ close }, conflict_keys(actions.conflict_choose_all, "(whole file)"))
            return {
                enhanced_diff_hl = true,
                hooks = {
                    -- open with the file panel hidden (diff views only; the file-history panel stays)
                    view_opened = function(view)
                        if view.class:name() == "DiffView" and view.panel:is_open() then
                            view.panel:close()
                        end
                    end,
                },
                view = {
                    merge_tool = {
                        layout = "diff3_horizontal", -- OURS | RESULT | THEIRS
                        disable_diagnostics = true,
                        winbar_info = true, -- OURS / RESULT / THEIRS labels on top of each window
                    },
                },
                keymaps = {
                    view = vim.tbl_extend("error", disabled_defaults, view),
                    file_panel = vim.tbl_extend("error", disabled_defaults, file_panel),
                    file_history_panel = { close },
                },
            }
        end,
    },

    -- {
    --     "akinsho/git-conflict.nvim",
    --     version = "*",
    --     config = true,
    -- },
}
