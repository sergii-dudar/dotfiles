-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
-- Add any additional autocmds here

local java_util = require("utils.java.java-common")
local augroup = vim.api.nvim_create_augroup
local customBuffer = augroup("custom_buffer", { clear = true })
local general_group = augroup("myCustomGroup", { clear = true })
local yank_group = augroup("HighlightYank", {})

local autocmd = vim.api.nvim_create_autocmd

-- start terminal in insert mode
-- autocmd("TermOpen", {
--     desc = "Auto enter insert mode when opening a terminal",
--     group = customBuffer,
--     pattern = "*",
--     callback = function()
--         -- Wait briefly just in case we immediately switch out of the buffer (e.g. Neotest)
--         vim.defer_fn(function()
--             if vim.api.nvim_buf_get_option(0, "buftype") == "terminal" then
--                 vim.cmd([[startinsert]])
--             end
--         end, 100)
--     end,
-- })
vim.api.nvim_create_autocmd("TermOpen", {
    pattern = "*",
    callback = function(args)
        -- Make the terminal leave insert mode and close with q
        vim.keymap.set("n", "q", "<cmd>bd!<CR>", { buffer = true })
    end,
})

-- highlight yanks
autocmd("TextYankPost", {
    group = yank_group,
    pattern = "*",
    callback = function()
        vim.highlight.on_yank({ timeout = 300 })
    end,
})

-- to be able to close dap hover popup by `q`: require("dap.ui.widgets").hover()
vim.api.nvim_create_autocmd("FileType", {
    pattern = "dap-float",
    callback = function()
        vim.api.nvim_buf_set_keymap(0, "n", "q", "<cmd>close!<CR>", { noremap = true, silent = true })
    end,
})

-------------------------------------------
------------ folke/trouble.nvim -----------

-- AUTOMATICALLY OPEN TROUBLE QUICKFIX (-- Test with something like: silent grep vim %)
vim.api.nvim_create_autocmd("QuickFixCmdPost", {
    callback = function()
        vim.cmd([[Trouble qflist open]])
    end,
})

-- OPEN TROUBLE QUICKFIX WHEN THE QF LIST OPENS
-- This is **NOT** recommended, since you won’t be able to use the quickfix list for other things.

vim.api.nvim_create_autocmd("BufRead", {
    callback = function(ev)
        if vim.bo[ev.buf].buftype == "quickfix" then
            vim.schedule(function()
                vim.cmd([[cclose]])
                vim.cmd([[Trouble qflist open]])
            end)
        end
    end,
})

-------------------------------------------
----------------- TESTS

--vim.api.nvim_create_autocmd("BufReadCmd", {
--    pattern = "*.class",
--    callback = function()
--        if vim.lsp.buf.server_ready() then
--            require('jdtls').open_classfile()
--        else
--            print("JDTLS client is not available")
--        end
--    end
--})

--autocmd("BufWritePre", {
--    pattern = "*",
--    callback = function()
--        vim.lsp.buf.format({ async = false })
--    end,
--})

--print("UiEnter1")
--vim.api.nvim_create_autocmd("VimEnter", {
--    callback = function()
--        -- Open Neotree automatically
--        require("neo-tree.command").execute({ toggle = true, dir = vim.loop.cwd() })
--    end
--})

-------------------------------------------
------------ winbar file path -----------
vim.api.nvim_create_autocmd("BufWinEnter", {
    pattern = "*",
    callback = function()
        -- skip if a pop up window
        if vim.fn.win_gettype() == "popup" then
            return
        end

        -- skip if new buffer
        if vim.bo.filetype == "" then
            return
        end

        vim.wo.winbar = "%{%v:lua.require'utils.nvim.winbar-util'.eval()%}"
    end,
    group = vim.api.nvim_create_augroup("WinBar", {}),
})

-------------------------------------------------------
--------------- project roots commands ----------------

if java_util.is_java_project() then
    local java_refactor_util = require("utils.java.refactor.java-refactor-util")
    vim.api.nvim_create_autocmd({ "FileType" }, {
        group = general_group,
        pattern = "fyler",
        callback = function(ev)
            -- BufLeave, BufHidden, BufUnload, WinLeave, BufWinLeave, BufDelete
            vim.api.nvim_create_autocmd({ "BufUnload" }, {
                group = general_group,
                buffer = ev.buf,
                callback = function()
                    vim.notify("Fyler: fixing after move is running...")
                    java_refactor_util.process_registerd_changes()
                    vim.notify("Fyler: fixing after move was finished!")
                end,
            })
        end,
    })
end

-------------------------------------------------------
------------ auto save on buff switch\leave -----------

vim.api.nvim_create_autocmd({ "FocusLost", "BufLeave" }, {
    pattern = "*",
    callback = function()
        if vim.bo.modified and vim.fn.expand("%") ~= "" then
            vim.cmd("silent! write")
        end
    end,
})

-------------------------------------------------------
----------------- Runner (Overseer) -------------------

vim.api.nvim_create_user_command("WatchRun", function()
    local overseer = require("overseer")
    overseer.run_task({ name = "run current", autostart = false }, function(task)
        if task then
            task:add_component({ "restart_on_save", paths = { vim.fn.expand("%:p") } })
            task:start()
            -- task:open_output("vertical")
            task:open_output("horizontal")
        else
            vim.notify("WatchRun not supported for filetype " .. vim.bo.filetype, vim.log.levels.ERROR)
        end
    end)
end, {})

-------------------------------------------------------
---------------- blink.cmp (cmp-dap) ------------------

vim.api.nvim_create_autocmd("FileType", {
    pattern = { "dap-repl", "dapui_watches", "dapui_hover" },
    callback = function()
        vim.b.completion = true
    end,
    desc = "Enable completion for DAP-REPL filetypes",
})

-----------------------------------------------------------
------------- enable\disable newline defaults -------------
--- useful by working with sensitive files

vim.api.nvim_create_user_command("EnableBinary", function()
    vim.bo.binary = true
    vim.bo.eol = false
    vim.opt.fixeol = false
    vim.cmd("update")
end, {})

vim.api.nvim_create_user_command("DisableBinary", function()
    vim.bo.binary = false
    vim.bo.eol = true -- restore eol to default (true)
    vim.opt.fixeol = true
    vim.cmd("update")
end, {})

-- tests
vim.api.nvim_create_user_command("RunMainClass", function()
    require("jdtls.dap").fetch_main_configs({
        config_overrides = {
            noDebug = false, -- This runs without debugging
        },
    }, function(configs)
        if #configs > 0 then
            -- dd(configs)
            require("dap").run(configs[1])
        end
    end)
end, {})

vim.api.nvim_create_user_command("LastRun", function()
    require("dap").run_last()
end, {})