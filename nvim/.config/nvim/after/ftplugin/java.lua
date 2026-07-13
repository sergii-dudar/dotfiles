-- Set shift width
--vim.opt.shiftwidth = 8
--vim.opt.tabstop = 4

-- Enable break indent
--vim.opt.breakindent = false

-- Configure break indent options
--vim.opt.breakindentopt = { shift = 2, min = 40 }

--vim.g.java_recommended_style = 0

--------------------------------------------------------------------
-- Options
--------------------------------------------------------------------

vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = true
vim.opt.shiftwidth = 4
vim.opt.autoindent = true
vim.opt.smartindent = true
vim.opt.breakindent = true
vim.opt.wrap = false

-- Set shift width
--vim.opt.shiftwidth = 8

-- Enable break indent

-- Configure break indent options
--vim.opt.breakindentopt = { shift = 2, min = 40 }

--vim.g.java_recommended_style = 0

-- Set options
--vim.opt.compatible = false
-- Enable filetype plugin and indent
--vim.cmd('filetype plugin indent on')

--vim.opt.indentexpr = ""

--set indentexpr?

--------------------------------------------------------------------
-- CMDs
--------------------------------------------------------------------

-- stylua: ignore start
-- local maven_compile = require("utils.java.maven-compile")
-- vim.api.nvim_create_user_command("MavenCompile", maven_compile.compile, {})
-- vim.api.nvim_create_user_command("MavenAutoCompileToggle", maven_compile.toggle_auto_compile, {})
-- vim.api.nvim_create_user_command("MavenCleanCompile", maven_compile.clean_compile, {})

--------------------------------------------------------------------
-- Keybinds
--------------------------------------------------------------------

-- vim.api.nvim_set_keymap("n", "<leader><F9>", ":MavenCompile<CR>", { noremap = true, silent = true, desc = "Maven Compile" })
-- vim.api.nvim_set_keymap("n", "<leader><F10>", ":MavenCleanCompile<CR>", { noremap = true, silent = true, desc = "Maven Clean Compile" })
vim.api.nvim_set_keymap("n", "<leader><F9>", ":JdtCompile incremental<CR>", { noremap = true, silent = true, desc = "JDTLS Compile Incremental" })
vim.api.nvim_set_keymap("n", "<leader><F10>", ":JdtCompile full<CR>", { noremap = true, silent = true, desc = "JDTLS Compile Full" })

----------------------------- Testing cmds start
----------------------------- Testing cmds end

--------------------------------------------------------------------
-- Indentation
--------------------------------------------------------------------

-- j1: indent Java anonymous classes correctly
-- +2s: continuation indent = 2 * shiftwidth (standard Java convention)
require("utils.indent-util").activate({ cinoptions = "j1,+2s" })

--------------------------------------------------------------------
-- Auto Cmd
--------------------------------------------------------------------