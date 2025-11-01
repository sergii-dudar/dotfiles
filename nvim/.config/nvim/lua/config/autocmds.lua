-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
-- Add any additional autocmds here

local augroup = vim.api.nvim_create_augroup
local customBuffer = augroup("custom_buffer", { clear = true })
local myCustomGroup = augroup("myCustomGroup", {})
local yank_group = augroup("HighlightYank", {})

local autocmd = vim.api.nvim_create_autocmd

-- start terminal in insert mode
autocmd("TermOpen", {
    desc = "Auto enter insert mode when opening a terminal",
    group = customBuffer,
    pattern = "*",
    callback = function()
        -- Wait briefly just in case we immediately switch out of the buffer (e.g. Neotest)
        vim.defer_fn(function()
            if vim.api.nvim_buf_get_option(0, "buftype") == "terminal" then
                vim.cmd([[startinsert]])
            end
        end, 100)
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

-- winbar file path
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

-- Lua function for Java indentation
-- TODO: implement better behavior as current default
function GetJavaIndent()
    vim.notify("test")

    -- Get the current line number
    local lnum = vim.v.lnum
    -- Get the previous non-blank line number
    local prev_lnum = vim.fn.prevnonblank(lnum - 1)

    -- If there's no previous line, return 0 indentation
    if prev_lnum == 0 then
        return 0
    end

    -- Get the indentation level of the previous non-blank line
    local prev_indent = vim.fn.indent(prev_lnum)
    -- Get the content of the previous line
    local prev_line = vim.fn.getline(prev_lnum)

    -- Continuation indent: If the previous line does NOT end with ;, {, or }
    if not prev_line:find("[;{}]%s*$") then
        -- Apply continuation indent by adding 2 levels of shiftwidth
        return prev_indent + vim.bo.shiftwidth * 2
    end

    -- Normal indentation (like smartindent behavior)
    if prev_line:find("{%s*$") then
        -- Increase indent after an opening brace '{'
        return prev_indent + vim.bo.shiftwidth
    elseif prev_line:find("}%s*$") then
        -- Decrease indent after a closing brace '}'
        return prev_indent - vim.bo.shiftwidth
    end

    -- Default: Maintain the same indent as the previous line
    return prev_indent
end

-- Disable Tree-sitter indent for Java files
vim.api.nvim_create_autocmd({ "FileType" }, {
    pattern = "java",
    callback = function()
        vim.bo.indentexpr = ""
        --vim.bo.indentexpr = "v:lua.GetJavaIndent()"
    end,
})

-- vim.api.nvim_create_user_command("NeoTreeToggleGroupEmptyDirs", function()
--     local fs = require("neo-tree.sources.filesystem")
--     local state = require("neo-tree.sources.manager").get_state("filesystem")
--     local current = state
--         and state.default_component_configs
--         and state.default_component_configs.filesystem.group_empty_dirs
--     local new_value = not (current or fs.group_empty_dirs or false)
--
--     fs.group_empty_dirs = new_value
--     vim.notify("Neo-tree: group_empty_dirs = " .. tostring(new_value))
--     require("neo-tree.command").execute({ source = "filesystem", action = "refresh" })
-- end, {})

-- vim.api.nvim_create_user_command("NeoTreeToggleGroupEmptyDirs", function()
--     local fs = require("neo-tree.sources.filesystem")
--     local manager = require("neo-tree.sources.manager")
--
--     -- Get the current state
--     local state = manager.get_state("filesystem")
--     local current_value = fs.group_empty_dirs or false
--
--     -- Toggle the setting
--     fs.group_empty_dirs = not current_value
--
--     -- Close and reopen the filesystem to apply it
--     require("neo-tree.command").execute({ source = "filesystem", action = "close" })
--     require("neo-tree.command").execute({
--         source = "filesystem",
--         action = "show",
--         reveal = true,
--     })
--
--     vim.notify("Neo-tree: group_empty_dirs = " .. tostring(fs.group_empty_dirs))
-- end, {})

-- vim.keymap.set("n", "<leader>ge", function()
--     local neotree_config = require("neo-tree.sources.filesystem")
--     neotree_config.group_empty_dirs = not neotree_config.group_empty_dirs
--     vim.cmd("Neotree refresh")
-- end, {
--     desc = "Toggle grouping of empty directories in Neo-tree",
-- })

-- Hyprlang LSP
-- vim.api.nvim_create_autocmd({ "BufEnter", "BufWinEnter" }, {
--     pattern = { "*.hl", "hypr*.conf" },
--     callback = function(event)
--         print(string.format("starting hyprls for %s", vim.inspect(event)))
--         vim.lsp.start({
--             name = "hyprlang",
--             cmd = { "hyprls" },
--             root_dir = vim.fn.getcwd(),
--         })
--     end,
-- })
