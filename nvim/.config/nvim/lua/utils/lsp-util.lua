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
        resolve_context = function(action_match_names)
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
