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

local map = LazyVim.safe_keymap_set

-- vim.api.nvim_set_keymap("n", "<leader><F9>", ":MavenCompile<CR>", { noremap = true, silent = true, desc = "Maven Compile" })
-- vim.api.nvim_set_keymap("n", "<leader><F10>", ":MavenCleanCompile<CR>", { noremap = true, silent = true, desc = "Maven Clean Compile" })
vim.api.nvim_set_keymap("n", "<leader><F9>", ":JdtCompile incremental<CR>", { noremap = true, silent = true, desc = "JDTLS Compile Incremental" })
vim.api.nvim_set_keymap("n", "<leader><F10>", ":JdtCompile full<CR>", { noremap = true, silent = true, desc = "JDTLS Compile Full" })

----------------------------- Testing cmds start
----------------------------- Testing cmds end

-- stylua: ignore end

-- NOTE: Trigger full compile AFTER JDTLS finishes workspace build.
-- Actual startup order: Building → Importing Maven → Synchronizing → Clean workspace
--   → Validate documents → Publish Diagnostics → Register Watchers → Send Classpath
-- "Publish Diagnostics" means JDTLS has validated code and knows about generated-sources dirs.
-- local log = require("utils.logging-util").new({
--     name = "jdtls.progress",
--     filename = "jdtls-progress.log",
--     level = vim.log.levels.DEBUG,
-- })
-- local old_handler = vim.lsp.handlers["$/progress"]
vim.lsp.handlers["$/progress"] = function(_, result, ctx)
    -- if old_handler then
    --     old_handler(_, result, ctx)
    -- end

    local val = result.value
    -- if val then
    --     log.fmt_debug("kind=%s message=%s", val.kind or "nil", val.message or "nil")
    -- end

    if not vim.g._is_workspace_built and val and val.kind == "end" and val.message == "Publish Diagnostics" then
        vim.g._is_workspace_built = true
        -- log.info("Publish Diagnostics completed — triggering JdtCompile full (1s delay)")
        vim.notify("🏄 JDTLS updating workspace started...")
        -- vim.defer_fn(function()
        --     pcall(vim.cmd, "JdtCompile full")
        --     -- log.info("JdtCompile full dispatched")
        -- end, 1000)
    end
end

--------------------------------------------------------------------
-- Indentation
--------------------------------------------------------------------

-- j1: indent Java anonymous classes correctly
-- +2s: continuation indent = 2 * shiftwidth (standard Java convention)
require("utils.indent-util").activate({ cinoptions = "j1,+2s" })

--------------------------------------------------------------------
-- Auto Cmd
--------------------------------------------------------------------

local java_group = vim.api.nvim_create_augroup("JavaGroup", { clear = true })

-- Setup user command for testing
local mapstruct = require("modules.java.mapstruct")
vim.keymap.set("n", "<leader>pm", function()
    mapstruct.goto_path_definition({ is_open_as_floating_win = true })
end, { desc = "Go to definition (MapStruct) Float" })
vim.keymap.set("n", "<leader>pM", function()
    mapstruct.goto_path_definition()
end, { desc = "Go to definition (MapStruct)" })

-- Highlight pattern in terminal output
-- vim.api.nvim_create_autocmd("TermOpen", {
--     group = java_group,
--     pattern = "*",
--     callback = function(args)
--         local buf = args.buf
--
--         -- Create a namespace for highlights
--         local ns = vim.api.nvim_create_namespace("TermStacktrace")
--
--         -- Periodically scan new lines
--         vim.fn.timer_start(100, function()
--             if not vim.api.nvim_buf_is_valid(buf) then
--                 return
--             end
--             local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
--             for i, line in ipairs(lines) do
--                 if line:find("%.java:%d+") then
--                     vim.api.nvim_buf_add_highlight(
--                         buf,
--                         ns,
--                         "Underlined", -- any highlight group
--                         i - 1,
--                         0,
--                         -1
--                     )
--                 end
--             end
--         end, { ["repeat"] = -1 })
--     end,
-- })