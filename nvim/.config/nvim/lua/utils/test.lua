-- 2. Target buffer and line index
local buf = vim.fn.bufnr("/home/serhii/dotfiles/test.txt")
require("utils.java.java-trace").highlight_java_test_trace(buf)

vim.lsp.buf.workspace_symbol("ArrayList") -- open qflist
