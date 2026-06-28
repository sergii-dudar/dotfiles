-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
-- Add any additional autocmds here

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
-- vim.api.nvim_set_hl(0, "YankHighlight", { bg = "#A1C44B", fg = "#3b4261" })
autocmd("TextYankPost", {
    group = yank_group,
    pattern = "*",
    callback = function()
        -- vim.highlight.on_yank({ higroup = "YankHighlight", timeout = 300 })
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
---------------- dirsession  --------------

local function is_virtual_buf(name)
    return name:sub(1, 6) == "jdt://"
        or name:sub(1, 10) == "zipfile://"
        or name:sub(1, 12) == "miniicons://"
        or name:match("neo%-tree ")
        or name:match("%[dap%-repl%-%d+%]")
        or name:match("DAP [A-Z]")
end

vim.api.nvim_create_autocmd("VimLeavePre", {
    callback = function()
        -- If focused buffer is a virtual URI, switch to a real file first
        if is_virtual_buf(vim.api.nvim_buf_get_name(0)) then
            for _, buf in ipairs(vim.api.nvim_list_bufs()) do
                if
                    vim.api.nvim_buf_is_loaded(buf)
                    and not is_virtual_buf(vim.api.nvim_buf_get_name(buf))
                    and vim.api.nvim_buf_get_name(buf) ~= ""
                then
                    vim.api.nvim_set_current_buf(buf)
                    break
                end
            end
        end
        -- Close jdtls virtual buffers (jdt:// URIs) before saving session
        -- so resession doesn't try to restore them on next startup
        for _, buf in ipairs(vim.api.nvim_list_bufs()) do
            if is_virtual_buf(vim.api.nvim_buf_get_name(buf)) then
                vim.api.nvim_buf_delete(buf, { force = true })
            end
        end
        -- require("resession").save(vim.fn.getcwd(), { dir = "dirsession", notify = false })
    end,
})
-- vim.api.nvim_create_autocmd("StdinReadPre", {
--     callback = function()
--         -- Store this for later
--         vim.g.using_stdin = true
--     end,
-- })

-------------------------------------------
------------ winbar file path -----------

function set_wrap(filetype)
    if filetype == "OverseerOutput" then
        vim.wo.wrap = true
    end
end
function set_winbar(filetype)
    -- skip if a pop up window
    if vim.fn.win_gettype() == "popup" then
        return
    end

    if
        require("utils.string-util").any_eq(filetype, {
            "",
            "log",
            "OverseerOutput",
            "neo-tree",
            "test-report-view",
        })
    then
        vim.wo.winbar = nil
        return
    end

    -- vim.wo.winbar = require("utils.nvim.winbar-util").eval()
    local success, value = pcall(require("utils.nvim.winbar-util").eval)
    if success then
        vim.wo.winbar = value
    end
end

local winbar_group = vim.api.nvim_create_augroup("WinBar", { clear = true })

vim.api.nvim_create_autocmd("BufWinEnter", {
    pattern = "*",
    callback = function(event)
        local filetype = vim.bo[event.buf].filetype
        set_wrap(filetype)
        if not is_virtual_buf(vim.api.nvim_buf_get_name(event.buf)) then
            set_winbar(filetype)
        end
    end,
    group = winbar_group,
})

-------------------------------------------
------------ filetype rescue --------------
-- Buffers loaded silently (e.g. junit test-report uses `:noautocmd call bufload`
-- to skip the JDTLS attach + highlight cascade during processing) end up with no
-- filetype set — so syntax/LSP are missing when the user later opens them via
-- Trouble, picker, or `gd`. On window entry, detect and set the filetype so the
-- FileType autocmd fires its usual cascades. Cost shifts to user-visible open,
-- where it would have been paid anyway.
vim.api.nvim_create_autocmd("BufWinEnter", {
    group = general_group,
    callback = function(ev)
        if vim.bo[ev.buf].filetype ~= "" or vim.bo[ev.buf].buftype ~= "" then
            return
        end
        local name = vim.api.nvim_buf_get_name(ev.buf)
        if name == "" or is_virtual_buf(name) then
            return
        end
        local ft = vim.filetype.match({ buf = ev.buf })
        if ft then
            vim.bo[ev.buf].filetype = ft
        end
    end,
})

-------------------------------------------------------
--------------- project roots commands ----------------

-- Fyler.nvim: process registered Java refactoring changes on buffer close
if require("utils.java.java-common").is_java_project() then
    require("modules.java.refactor.integrations").setup_fyler_autocmd()
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

vim.api.nvim_create_user_command("LangRegistryCheck", function(opts)
    require("utils.lang.registry-check").run({ notify = true, raise = opts.bang })
end, { bang = true })

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
