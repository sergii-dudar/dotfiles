-- LSP client lookup helpers.
--
-- • get_clients_by_name — get all LSP clients by name (optionally buffer-scoped)
-- • get_client_by_name — get the first LSP client by name (optionally buffer-scoped)
-- • get_client_id_by_name — get LSP client ID by name
--
-- These wrap the built-in `vim.lsp.get_clients` (the canonical lookup API since
-- nvim 0.10) and are the single, preferred way to resolve a client by name.

local M = {}

--- Get all LSP clients matching `name`, optionally scoped to a buffer.
---@param name string client name (e.g. "jdtls")
---@param opts? { bufnr?: integer } extra filters forwarded to vim.lsp.get_clients
---@return vim.lsp.Client[]
function M.get_clients_by_name(name, opts)
    local filter = vim.tbl_extend("force", opts or {}, { name = name })
    return vim.lsp.get_clients(filter)
end

--- Get the first LSP client matching `name`, optionally scoped to a buffer.
---@param name string client name (e.g. "jdtls")
---@param opts? { bufnr?: integer } extra filters forwarded to vim.lsp.get_clients
---@return vim.lsp.Client|nil
function M.get_client_by_name(name, opts)
    return M.get_clients_by_name(name, opts)[1]
end

--[[ function M.get_client_id_by_name(name)
    local clients = vim.lsp.get_clients()
    for client_id, client in pairs(clients) do
        if client.name == name then
            return client_id
        end
    end
    return nil -- Return nil if no client with the specified name is found
end ]]

---@return integer|nil
function M.get_client_id_by_name(name)
    local client = M.get_client_by_name(name)
    return client and client.id or nil
end

local LspCodeAction = function()
    return {
        --[[ resolve_context = function(action_match_names)
            local is_match_found = false
            vim.lsp.buf.code_action({
                filter = function(action)
                    if is_match_found then
                        return false
                    end
                    for _, value in ipairs(action_match_names) do
                        if action.title:match(value) then
                            is_match_found = true
                            return true
                        end
                    end
                    return false
                end,
                apply = true,
            })
        end, ]]
        -- Apply the first code action matching the highest-priority pattern.
        -- `action_match_names` is consulted in order (outer loop), so the caller's
        -- priority is honored regardless of the order the server returns actions in.
        resolve_context = function(action_match_names)
            local lsp_lang_common = require("utils.lang.lsp-common")
            local bufnr = vim.api.nvim_get_current_buf()
            local clients = vim.lsp.get_clients({ bufnr = bufnr })
            if #clients == 0 then
                vim.notify("No LSP clients attached", vim.log.levels.INFO)
                return
            end

            local offset_encoding = clients[1].offset_encoding or "utf-16"
            local params = vim.lsp.util.make_range_params(0, offset_encoding)
            local cursor_lnum = vim.api.nvim_win_get_cursor(0)[1] - 1
            ---@diagnostic disable-next-line: inject-field
            params.context = {
                -- diagnostics = vim.lsp.diagnostic.get_line_diagnostics(bufnr), -- <- deprecated
                -- vim.diagnostic.get returns vim.Diagnostic shape; the LSP request needs lsp.Diagnostic shape
                diagnostics = vim.lsp.diagnostic.from(vim.diagnostic.get(bufnr, { lnum = cursor_lnum })),
                triggerKind = vim.lsp.protocol.CodeActionTriggerKind.Invoked,
            }

            vim.lsp.buf_request_all(bufnr, "textDocument/codeAction", params, function(results)
                for _, name_pattern in ipairs(action_match_names) do
                    for client_id, result in pairs(results) do
                        for _, action in ipairs(result.result or {}) do
                            if action.title and action.title:match(name_pattern) then
                                vim.schedule(function()
                                    local client = vim.lsp.get_client_by_id(client_id)
                                    if client then
                                        lsp_lang_common.apply_lsp_action(action, client)
                                    end
                                end)
                                return
                            end
                        end
                    end
                end
                vim.schedule(function()
                    vim.notify("No matching code action available", vim.log.levels.INFO)
                end)
            end)
        end,
        toggle = function(action_match_name1, action_match_name2)
            vim.lsp.buf.code_action({
                filter = function(action)
                    return action.title:match(action_match_name1) or action.title:match(action_match_name2)
                end,
                apply = true,
            })
        end,
        apply = function(action_match_name)
            vim.lsp.buf.code_action({
                filter = function(action)
                    return action.title:match(action_match_name)
                end,
                apply = true,
            })
        end,
    }
end
M.code_action = LspCodeAction()

return M
