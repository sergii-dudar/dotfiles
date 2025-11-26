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
vim.opt.shiftwidth = 4 --8, need create custom indentexpr to handle correctly where 8, and where 4
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
local maven_compile = require("utils.java.maven-compile")
vim.api.nvim_create_user_command("MavenCompile", maven_compile.compile, {})
vim.api.nvim_create_user_command("MavenAutoCompileToggle", maven_compile.toggle_auto_compile, {})
vim.api.nvim_create_user_command("MavenCleanCompile", maven_compile.clean_compile, {})

--------------------------------------------------------------------
-- Keybinds
--------------------------------------------------------------------

local map = LazyVim.safe_keymap_set

vim.api.nvim_set_keymap("n", "<leader><F9>", ":MavenCompile<CR>", { noremap = true, silent = true, desc = "Maven Compile" })
vim.api.nvim_set_keymap("n", "<leader><F10>", ":MavenCleanCompile<CR>", { noremap = true, silent = true, desc = "Maven Clean Compile" })
-- vim.api.nvim_set_keymap("n", "<leader><F11>", ":MavenAutoCompileToggle<CR>", { noremap = true, silent = true, desc = "Maven Auto Compile Toggle" })

--[[ local maven_tests = require("utils.java.maven-tests")
vim.keymap.set("n", "<leader><F6>", maven_tests.run_java_test_method, { noremap = true, silent = true, desc = "Maven Run Current Test Method" })
vim.keymap.set("n", "<leader>TM", maven_tests.run_java_test_method_debug, { noremap = true, silent = true, desc = "Maven Run Current Test Method (Debug)" })
vim.keymap.set("n", "<leader><F7>", maven_tests.run_java_test_class, { noremap = true, silent = true, desc = "Mave Run Current Test Class" })
vim.keymap.set("n", "<leader>TC", maven_tests.run_java_test_class_debug, { noremap = true, silent = true, desc = "Mave Run Current Test Class (Debug)" })
vim.keymap.set("n", "<leader><F8>", maven_tests.run_java_test_all, { noremap = true, silent = true, desc = "Mave Run Test All" })
vim.keymap.set("n", "<leader>TA", maven_tests.run_java_test_all_debug, { noremap = true, silent = true, desc = "Mave Run Test All (Debug)" }) ]]
-- vim.keymap.set("n", "<F9>", maven_tests.run_spring_boot, { noremap = true, silent = true, desc = "Maven Run Spring Boot" })
-- vim.keymap.set("n", "<F10>", maven_tests.run_spring_boot_debug, { noremap = true, silent = true, desc = "Maven Run Spring Boot (Debug)" })


local maven_tests_v2 = require("utils.java.maven-tests-v2")
vim.keymap.set("n", "<leader><F6>", maven_tests_v2.run_java_test_method, { noremap = true, silent = true, desc = "Maven Run Current Test Method" })
vim.keymap.set("n", "<leader><F7>", maven_tests_v2.run_java_test_class, { noremap = true, silent = true, desc = "Maven Run Current Test Class" })
vim.keymap.set("n", "<leader>d<F6>", function() maven_tests_v2.run_java_test_method(true) end, { noremap = true, silent = true, desc = "Maven (Debug) Current Test Method" })
vim.keymap.set("n", "<leader>d<F7>", function() maven_tests_v2.run_java_test_class(true) end, { noremap = true, silent = true, desc = "Maven (Debug) Current Test Class" })
vim.keymap.set("n", "<leader><F8>", maven_tests_v2.run_java_test_all, { noremap = true, silent = true, desc = "Maven Run Test All" })
vim.keymap.set("n", "<leader><F5>", maven_tests_v2.rerun_last_cmd, { noremap = true, silent = true, desc = "Maven Re-Run Last Test" })

-- map("v", "<leader>xp", function() require("utils.java.java-trace").parse_selected_trace_to_qflist() end, { desc = "Parse trace to quick fix list" })
-- map("n", "<leader>xp", function() require("utils.java.java-trace").parse_buffer_trace_to_qflist() end, { desc = "Parse trace to quick fix list" })

----------------------------- Testing cmds start
----------------------------- Testing cmds end

-- stylua: ignore end

--------------------------------------------------------------------
-- Format
--------------------------------------------------------------------

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

--------------------------------------------------------------------
-- Auto Cmd
--------------------------------------------------------------------

local java_group = vim.api.nvim_create_augroup("JavaGroup", { clear = true })

-- Disable Tree-sitter indent for Java files
vim.api.nvim_create_autocmd({ "FileType" }, {
    group = java_group,
    pattern = "java",
    callback = function()
        vim.bo.indentexpr = ""
        --vim.bo.indentexpr = "v:lua.GetJavaIndent()"
    end,
})

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
