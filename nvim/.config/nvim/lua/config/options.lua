-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

vim.opt.tabstop = 4 -- spaces for tabs (prettier default)
vim.opt.shiftwidth = 4 -- 2 spaces for indent width
vim.opt.softtabstop = 4
vim.opt.expandtab = true
vim.opt.autoindent = true -- copy indent from current line when starting new one

vim.g.mapleader = " "
vim.g.maplocalleader = " "
vim.g.background = "light"
vim.g.have_nerd_font = true
vim.g.lazyvim_picker = "snacks"
-- vim.o.mousemoveevent = true

vim.scriptencoding = "utf-8"
vim.opt.encoding = "utf-8"
vim.opt.fileencoding = "utf-8"

vim.opt.wrap = false
vim.opt.swapfile = false
-- vim.opt.showtabline = 2
-- vim.opt.cursorcolumn = true

-- Enable syntax highlighting
--vim.cmd('syntax on')

vim.opt.hlsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true -- in case search by mixed case text, apply case-sensitive search
vim.opt.incsearch = true

vim.opt.swapfile = false
vim.opt.number = true
vim.wo.number = true
vim.opt.relativenumber = true
vim.opt.numberwidth = 2 -- set width of line number column
vim.opt.backup = false

-- buffer
-- vim.opt.hidden = true
-- vim.opt.autowrite = true
-- vim.opt.autowriteall = true

-- clipboard
vim.opt.clipboard:append("unnamedplus") -- use system clipboard as default register
--vim.opt.clipboard = 'unnamedplus'
--vim.opt.clipboard = ''

-- Preview substitutions
-- vim.opt.inccommand = "split"

vim.opt.showmode = false
--vim.opt.scrolloff = 10 -- number of lines to keep above/below cursor
vim.opt.sidescrolloff = 8 -- number of columns to keep to the left/right of cursor
vim.opt.scrolloff = 0
vim.opt.sidescrolloff = 0
vim.opt.signcolumn = "yes"
vim.opt.smartindent = true -- enable smart indentation
vim.opt.breakindent = true -- enable line breaking indentation

vim.opt.termguicolors = true
vim.opt.background = "dark"

vim.opt.cursorline = true

-- Enable mouse mode, useful for resizing splits, debugging etc
vim.opt.mouse = "a"
vim.opt.backspace = "indent,eol,start" -- allow backspace on indent, end of line or insert mode start position

-- split windows
vim.opt.splitright = true -- split vertical window to the right
vim.opt.splitbelow = true -- split horizontal window to the bottom

-- Set borders for all floating windows
vim.o.winborder = "rounded"

-- Save undo history
vim.opt.undofile = true

--LazyVim
vim.g.trouble_lualine = false

local augroup = vim.api.nvim_create_augroup
local myCustomGroup = augroup("myCustomStartupGroup", {})

-- gf, gF (to file and line), gx
vim.opt.path:append("**")

-- disable snack annimations
vim.g.snacks_animate = false

function open_tree_on_start()
    if require("utils.project-util").is_multifile_proj() then
        -- restore current proj session
        require("persistence").load()

        -- vim.notify("opening neo tree...", vim.log.levels.INFO)
        vim.cmd("Neotree filesystem reveal left")
        -- Snacks.picker.explorer()
    end
end

-- Improve diff experience
-- vim.opt.diffopt:append("algorithm:patience")
-- vim.opt.diffopt:append("indent-heuristic")

if vim.o.diff then
    -- vim.notify("Started in diff mode!", vim.log.levels.INFO)
    return
end

vim.api.nvim_create_autocmd("UiEnter", {
    desc = "Open Neotree automatically",
    group = myCustomGroup,
    --pattern = { "*" },
    callback = function()
        open_tree_on_start()

        -- open load buffer if opened in tab, or skip
        local timer = vim.uv.new_timer()
        timer:start(
            100,
            0,
            vim.schedule_wrap(function()
                -- if pcall(vim.cmd, "wincmd l") then
                --     vim.schedule(function()
                --         -- pcall(vim.cmd, "e")
                --         pcall(vim.cmd, "filetype detect")
                --         -- pcall(vim.cmd, "bufdo e") -- reload all opened tabs (buffers) after startup
                --     end)
                -- end
                -- pcall(vim.cmd, "wincmd l | filetype detect")
                pcall(vim.cmd, "wincmd l | e")
                timer:close()
            end)
        )

        -- vim.schedule(function()
        --     if pcall(vim.cmd, "wincmd l") then
        --         Snacks.notify.info("Here 1")
        --         vim.schedule(function()
        --         Snacks.notify.info("Here 2")
        --             pcall(vim.cmd, "e")
        --         end)
        --     end
        -- end)
        -- end
    end,
})
