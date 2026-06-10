local M = {}

---Per-primary-language LSP code-action data.
---Defined in `utils.lang.<lang>.lsp-<lang>` and consumed by that language's editor
---config for `<leader>cc`. Intentionally separate from the Overseer run/test/debug
---`task.lang.Runner` contract — this is an LSP concern.
---@class lang.LspCodeActions
---@field code_action_auto_resolve_match_names string[] Code-action title patterns (Lua patterns) auto-resolved by `<leader>cc`, in priority order; first match wins.

--- Apply a resolved LSP code action (handles resolve, edit, and command execution)
---@param action lsp.CodeAction
---@param client vim.lsp.Client
function M.apply_lsp_action(action, client)
    local bufnr = vim.api.nvim_get_current_buf()

    if not action.edit and not action.command and client:supports_method("codeAction/resolve") then
        client:request("codeAction/resolve", action, function(err, resolved)
            if err then
                vim.notify("Code action resolve error: " .. (err.message or "unknown"), vim.log.levels.WARN)
                return
            end
            M.apply_lsp_action(resolved or action, client)
        end, bufnr)
        return
    end

    if action.edit then
        vim.lsp.util.apply_workspace_edit(action.edit, client.offset_encoding)
    end

    if action.command then
        local command = type(action.command) == "table" and action.command or action
        client:request("workspace/executeCommand", command, function(err)
            if err then
                vim.notify(err.message or "Command execution failed", vim.log.levels.WARN)
            end
        end, bufnr)
    end
end

return M
