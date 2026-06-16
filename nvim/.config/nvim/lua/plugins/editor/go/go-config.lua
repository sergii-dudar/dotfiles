local goft = { "go" }

---@param bufnr integer
---@param lhs string
---@param rhs function
---@param desc string
local function go_keymap(bufnr, lhs, rhs, desc)
    vim.keymap.set("n", lhs, rhs, { buffer = bufnr, desc = desc })
end

local wk_augroup = vim.api.nvim_create_augroup("go_config_which_key", { clear = true })
vim.api.nvim_create_autocmd("FileType", {
    group = wk_augroup,
    pattern = goft,
    callback = function(ev)
        ---@diagnostic disable-next-line: undefined-field
        if vim.bo[ev.buf].buftype ~= "" then
            return
        end
        require("which-key").add({
            ---@diagnostic disable-next-line: undefined-field
            buffer = ev.buf,
            { "<leader>j", group = "+go" },
            { "<leader>jc", group = "+go code/compile" },
            { "<leader>jd", group = "+go errors/diagnostics" },
            { "<leader>jo", group = "+go open" },
        })
        go_keymap(ev.buf, "<leader>cc", function()
            local action_names = require("utils.lang.go.lsp-go").code_action_auto_resolve_match_names
            require("utils.lsp-util").code_action.resolve_context(action_names)
        end, "Context Apply First Code Action [gopls]")
        go_keymap(ev.buf, "<leader>jcc", function() end, "Test Compile [gopls]")
        -- TODO
    end,
})

return {}