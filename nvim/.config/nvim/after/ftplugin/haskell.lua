vim.api.nvim_create_user_command("HlsHardReset", function()
    -- Kill all running HLS servers
    vim.fn.system("pkill -f haskell-language-server")

    -- Reload current buffer after slight delay to let HLS restart
    vim.defer_fn(function()
        vim.cmd("edit!")
    end, 300)
end, { desc = "Forse HLS to restart fully and reload current buffer", nargs = "*" })

--[[ vim.api.nvim_create_user_command("HlsForceCheck", function()
    vim.lsp.buf_request(0, "textDocument/diagnostic", {
        textDocument = { uri = vim.uri_from_bufnr(0) },
    }, function(_, result, ctx, _)
        if result then
            vim.lsp.diagnostic.on_publish_diagnostics(_, result, ctx)
        end
    end)
end, { desc = "Forse HSL to recheck all diagnostics", nargs = "*" }) ]]

local current_file = vim.fn.expand("%:p")
local list_util = require("utils.list-util")
local ignore_dirs = {
    "xmobar",
    -- Add more directories as needed
}
if list_util.any_match(current_file, ignore_dirs) then
    -- disabled autoformat to work with dwm
    vim.b.autoformat = false
    vim.diagnostic.enable(false)
end