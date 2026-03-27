local fname = vim.api.nvim_buf_get_name(0)
if not fname:match("/openapi/") then
    return
end

vim.lsp.start({
    name = "openapi-language-server",
    cmd = { vim.fn.expand("~/go/bin/openapi-language-server") },
    root_dir = vim.fs.root(fname, { ".git" }) or vim.fs.dirname(fname),
})
