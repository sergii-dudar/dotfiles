local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
    local lazyrepo = "https://github.com/folke/lazy.nvim.git"
    local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
    if vim.v.shell_error ~= 0 then
        vim.api.nvim_echo({
            { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
            { out, "WarningMsg" },
            { "\nPress any key to exit..." },
        }, true, {})
        vim.fn.getchar()
        os.exit(1)
    end
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
    spec = {
        -- add LazyVim and import its plugins
        { "LazyVim/LazyVim", import = "lazyvim.plugins" },

        -- import any extras modules here

        { import = "lazyvim.plugins.extras.dap.core" },
        { import = "lazyvim.plugins.extras.dap.nlua" }, -- jbyuki/one-small-step-for-vimkind config
        { import = "lazyvim.plugins.extras.test.core" },
        { import = "lazyvim.plugins.extras.ui.treesitter-context" },
        { import = "lazyvim.plugins.extras.coding.mini-surround" },

        -- { import = "lazyvim.plugins.extras.formatting.prettier" },

        -- ========= editor =========
        -- { import = "lazyvim.plugins.extras.editor.telescope" },
        -- { import = "lazyvim.plugins.extras.editor.fzf" },
        { import = "lazyvim.plugins.extras.editor.neo-tree" },
        { import = "lazyvim.plugins.extras.editor.snacks_picker" },
        -- { import = "lazyvim.plugins.extras.editor.snacks_explorer" },

        -- { import = "lazyvim.plugins.extras.editor.harpoon2" },
        { import = "lazyvim.plugins.extras.editor.illuminate" },
        { import = "lazyvim.plugins.extras.editor.mini-move" },
        { import = "lazyvim.plugins.extras.editor.inc-rename" },
        { import = "lazyvim.plugins.extras.coding.neogen" }, -- some issues with `JavaHello/spring-boot.nvim` disabled for now
        --{ import = "lazyvim.plugins.extras.editor.navic" },
        { import = "lazyvim.plugins.extras.editor.outline" },
        -- { import = "lazyvim.plugins.extras.editor.overseer" },
        --{ import = "lazyvim.plugins.extras.editor.fzf" },
        --{ import = "lazyvim.plugins.extras.editor.refactoring" },
        --{ import = "lazyvim.plugins.extras.vscode" },

        --comment it if want to use `nvim-snippets`
        { import = "lazyvim.plugins.extras.coding.luasnip" },
        --{ import = "lazyvim.plugins.extras.coding.nvim-cmp" },
        { import = "lazyvim.plugins.extras.coding.blink" },

        -- ========= lang =========
        { import = "lazyvim.plugins.extras.lang.markdown" },
        { import = "lazyvim.plugins.extras.lang.json" },
        { import = "lazyvim.plugins.extras.lang.python" },
        { import = "lazyvim.plugins.extras.lang.yaml" },
        { import = "lazyvim.plugins.extras.lang.helm" },
        -- { import = "lazyvim.plugins.extras.lang.java" }, using exended own version
        { import = "lazyvim.plugins.extras.lang.sql" },
        { import = "lazyvim.plugins.extras.lang.terraform" },
        { import = "lazyvim.plugins.extras.lang.docker" },
        { import = "lazyvim.plugins.extras.lang.git" },
        { import = "lazyvim.plugins.extras.lang.toml" },
        { import = "lazyvim.plugins.extras.lang.rust" },
        -- { import = "lazyvim.plugins.extras.lang.haskell" },
        { import = "lazyvim.plugins.extras.lang.clangd" },
        { import = "lazyvim.plugins.extras.lang.cmake" },

        -- ========= langs that using not very often: =========
        --{ import = "lazyvim.plugins.extras.lang.go" },
        --{ import = "lazyvim.plugins.extras.lang.omnisharp" },
        --{ import = "lazyvim.plugins.extras.lang.kotlin" },
        --{ import = "lazyvim.plugins.extras.lang.scala" },
        { import = "lazyvim.plugins.extras.lang.typescript" },

        -- ========= util =========
        --{ import = "lazyvim.plugins.extras.util.rest" },
        --{ import = "lazyvim.plugins.extras.util.project" },
        { import = "lazyvim.plugins.extras.util.dot" },

        -- ========= ai ==========
        { import = "lazyvim.plugins.extras.ai.sidekick" },
        { import = "lazyvim.plugins.extras.ai.copilot" },

        -- ========= import/override with your plugins =========
        { import = "plugins.ui" },
        { import = "plugins.navigation" },
        { import = "plugins.editor" },
        { import = "plugins.editor.java" },
        { import = "plugins.editor.shell" },
        { import = "plugins.editor.lua" },
        { import = "plugins.snacks" },
        { import = "plugins.fun" },
        { import = "plugins" },
    },
    defaults = {
        -- By default, only LazyVim plugins will be lazy-loaded. Your custom plugins will load during startup.
        -- If you know what you're doing, you can set this to `true` to have all your custom plugins lazy-loaded by default.
        lazy = false,
        -- It's recommended to leave version=false for now, since a lot the plugin that support versioning,
        -- have outdated releases, which may break your Neovim install.
        version = false, -- always use the latest git commit
        -- version = "*", -- try installing the latest stable version for plugins that support semver
    },
    --install = { colorscheme = { "tokyonight", "habamax" } },
    checker = {
        enabled = true, -- check for plugin updates periodically
        notify = false, -- notify on update
    }, -- automatically check for plugin updates
    performance = {
        rtp = {
            -- disable some rtp plugins
            disabled_plugins = {
                "gzip",
                -- "matchit",
                -- "matchparen",
                -- "netrwPlugin",
                "tarPlugin",
                "tohtml",
                "tutor",
                "zipPlugin",
            },
        },
    },
})