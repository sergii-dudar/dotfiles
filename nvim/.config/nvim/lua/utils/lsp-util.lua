local M = {}

---@return vim.lsp.Client|nil
M.get_client_by_name = function(name)
    local clients = vim.lsp.get_clients({ name = name }) -- or any client
    if not clients or vim.tbl_isempty(clients) then
        return nil
    else
        return clients[1]
    end
end

--[[ M.get_client_id_by_name = function(name)
    local clients = vim.lsp.get_clients()
    for client_id, client in pairs(clients) do
        if client.name == name then
            return client_id
        end
    end
    return nil -- Return nil if no client with the specified name is found
end ]]

---@return integer|nil
M.get_client_id_by_name = function(name)
    local client = M.get_client_by_name(name)
    return client and client.id or nil
end

return M