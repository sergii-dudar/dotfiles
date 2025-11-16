-- Set shift width
--vim.opt.shiftwidth = 8
--vim.opt.tabstop = 4

-- Enable break indent
--vim.opt.breakindent = false

-- Configure break indent options
--vim.opt.breakindentopt = { shift = 2, min = 40 }

--vim.g.java_recommended_style = 0

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


----------------- Maven Cmds

-- stylua: ignore start
local maven_compile = require("utils.java.maven-compile")
vim.api.nvim_create_user_command("MavenCompile", function() maven_compile.compile() end, {})
vim.api.nvim_create_user_command("MavenAutoCompileToggle", function() maven_compile.toggle_auto_compile("toggle_compile") end, {})
vim.api.nvim_create_user_command("MavenCleanCompile", function() maven_compile.clearn_compile() end, {})

-- local java_utils = require("utils.java.utils")
local maven_tests = require("utils.java.maven-tests")
-- vim.api.nvim_create_user_command("MavenTest", function() maven_tests.test() end, {})
-- vim.api.nvim_create_user_command("MavenVerify", function() maven_tests.verify() end, {})


vim.api.nvim_create_user_command("MavenPrint", function() maven_tests.verify() end, {})

-- stylua: ignore end

----------------- Keybinds
local map = LazyVim.safe_keymap_set

vim.api.nvim_set_keymap("n", "<leader><F9>", ":MavenCompile<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader><F10>", ":MavenCleanCompile<CR>", { noremap = true, silent = true })
-- vim.api.nvim_set_keymap("n", "<leader><F11>", ":MavenAutoCompileToggle<CR>", { noremap = true, silent = true })

map("v", "<leader>xp", function()
    require("utils.java.java-trace").parse_selected_trace_to_qflist()
end, { desc = "Parse trace to quick fix list" })
map("n", "<leader>xp", function()
    require("utils.java.java-trace").parse_buffer_trace_to_qflist()
end, { desc = "Parse trace to quick fix list" })

map("n", "<leader>fs", function()
    local helpers = require("utils.common-util")

    local jdtls_client_id = helpers.get_client_id_by_name("jdtls")
    if jdtls_client_id then
        local current_buf_id = vim.api.nvim_get_current_buf()
        if not vim.lsp.buf_is_attached(current_buf_id, jdtls_client_id) then
            vim.lsp.buf_attach_client(current_buf_id, jdtls_client_id)

            LazyVim.info("jdtls client found by ID:" .. jdtls_client_id)
            LazyVim.info("attaching jdtls to current buffer by ID:" .. current_buf_id)
        end
    end

    local fileName = helpers.get_file_with_no_ext()
    -- LazyVim.info("fileName:" .. fileName)

    require("telescope.builtin").lsp_dynamic_workspace_symbols({
        symbols = LazyVim.config.get_kind_filter(),
        default_text = fileName,
    })
end, { desc = "Find word under curser in lsp dynamic_workspace_symbols" })

----------------- Format

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
