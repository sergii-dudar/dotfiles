local fname = vim.api.nvim_buf_get_name(0)
local first_line = vim.api.nvim_buf_get_lines(0, 0, 1, false)[1] or ""
if not fname:match("/openapi/") and not first_line:match("^openapi:") then
    return
end

vim.lsp.start({
    name = "openapi-language-server",
    cmd = { vim.fn.expand("~/go/bin/openapi-language-server") },
    root_dir = vim.fs.root(fname, { ".git" }) or vim.fs.dirname(fname),
})
