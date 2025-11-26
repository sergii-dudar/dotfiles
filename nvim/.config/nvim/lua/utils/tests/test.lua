-- 2. Target buffer and line index
-- local buf = vim.fn.bufnr("/home/serhii/dotfiles/test.txt")
-- require("utils.java.java-trace").highlight_java_test_trace(buf)

--vim.lsp.buf.workspace_symbol("ArrayList") -- open qflist

-- highlight columns

-- clear
-- ErrorMsg
-- WarningMsg
-- DiagnosticSignWarn
-- DiagnosticSignInfo
-- DiagnosticSignError
vim.fn.sign_unplace("MyGroup")

local bufnr = vim.api.nvim_get_current_buf()
vim.fn.sign_define("SUCCESS", { text = " ", texthl = "TodoSignTODO" })
vim.fn.sign_define("FAILED", { text = " ", texthl = "TodoSignFIX" })

vim.fn.sign_place(0, "MyGroup", "SUCCESS", bufnr, { lnum = 6 })
vim.fn.sign_place(0, "MyGroup", "FAILED", bufnr, { lnum = 8 })