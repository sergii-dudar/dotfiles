-- Language LSP helpers: shared code-action contracts and application logic.
--
-- - apply_lsp_action - resolve/apply an LSP code action and execute its command

local M = {}

---Per-primary-language LSP code-action data.
---Defined in `utils.lang.<lang>.lsp-<lang>` and consumed by that language's editor
---config for `<leader>cc`. Intentionally separate from the Overseer run/test/debug
---`task.lang.Runner` contract — this is an LSP concern.
---@class lang.LspCodeActions
---@field code_action_auto_resolve_match_names string[] Code-action title patterns (Lua patterns) auto-resolved by `<leader>cc`, in priority order; first match wins.

--- Apply an LSP code action for the current buffer.
--- Resolves lazy actions when the client supports `codeAction/resolve`, applies
--- workspace edits with the client's offset encoding, then executes any command
--- returned by the action.
---@param action lsp.CodeAction
---@param client vim.lsp.Client
---@param on_applied? fun(action: lsp.CodeAction) called once the (possibly resolved)
---action's edit has been applied and any command request has completed; receives
---the final resolved action so callers can inspect its `edit` for post-processing.
function M.apply_lsp_action(action, client, on_applied)
    local bufnr = vim.api.nvim_get_current_buf()

    if not action.edit and not action.command and client:supports_method("codeAction/resolve") then
        client:request("codeAction/resolve", action, function(err, resolved)
            if err then
                vim.notify("Code action resolve error: " .. (err.message or "unknown"), vim.log.levels.WARN)
                return
            end
            M.apply_lsp_action(resolved or action, client, on_applied)
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
            if on_applied then
                on_applied(action)
            end
        end, bufnr)
    elseif on_applied then
        on_applied(action)
    end
end

return M
