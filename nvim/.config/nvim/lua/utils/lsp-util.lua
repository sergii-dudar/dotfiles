local M = {}

---@return vim.lsp.Client|nil
function M.get_client_by_name(name)
    local clients = vim.lsp.get_clients({ name = name }) -- or any client
    if not clients or vim.tbl_isempty(clients) then
        return nil
    else
        return clients[1]
    end
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
        apply_first_available = function(...)
            vim.ui.select = Snacks.picker.select
            local action_match_names = { ... }
            vim.lsp.buf.code_action({
                filter = function(action)
                    for _, value in ipairs(action_match_names) do
                        if action.title:match(value) then
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