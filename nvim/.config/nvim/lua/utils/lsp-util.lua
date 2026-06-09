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

--- Apply a resolved LSP code action (handles resolve, edit, and command execution)
---@param action lsp.CodeAction
---@param client vim.lsp.Client
local function apply_lsp_action(action, client)
    local bufnr = vim.api.nvim_get_current_buf()

    if not action.edit and not action.command and client:supports_method("codeAction/resolve") then
        client:request("codeAction/resolve", action, function(err, resolved)
            if err then
                vim.notify("Code action resolve error: " .. (err.message or "unknown"), vim.log.levels.WARN)
                return
            end
            apply_lsp_action(resolved or action, client)
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

--- Request code actions from all LSP clients, apply the first matching one
--- (respecting pattern priority order), or invoke a fallback if none match.
---@param action_match_names string[]
---@param fallback? fun()
local function request_and_apply_first(action_match_names, fallback)
    local bufnr = vim.api.nvim_get_current_buf()
    local clients = vim.lsp.get_clients({ bufnr = bufnr })
    local offset_encoding = clients[1] and clients[1].offset_encoding or "utf-16"
    local params = vim.lsp.util.make_range_params(0, offset_encoding)
    params.context = {
        diagnostics = vim.lsp.diagnostic.get_line_diagnostics(bufnr),
        triggerKind = vim.lsp.protocol.CodeActionTriggerKind.Invoked,
    }

    vim.lsp.buf_request_all(bufnr, "textDocument/codeAction", params, function(results)
        -- 1. Highest priority: import actions (auto-apply if single, picker if multiple)
        local import_pattern = "^Import '.*' %(.*%)$"
        local import_matches = {}
        for client_id, result in pairs(results) do
            for _, action in ipairs(result.result or {}) do
                if action.title and action.title:match(import_pattern) then
                    table.insert(import_matches, { action = action, client_id = client_id })
                end
            end
        end

        if #import_matches == 1 then
            vim.schedule(function()
                local match = import_matches[1]
                local client = vim.lsp.get_client_by_id(match.client_id)
                if client then
                    apply_lsp_action(match.action, client)
                end
            end)
            return
        elseif #import_matches > 1 then
            vim.schedule(function()
                local titles = vim.tbl_map(function(m)
                    return m.action.title
                end, import_matches)
                Snacks.picker.select(titles, { prompt = "Select import" }, function(choice, idx)
                    if not choice then
                        return
                    end
                    local match = import_matches[idx]
                    local client = vim.lsp.get_client_by_id(match.client_id)
                    if client then
                        apply_lsp_action(match.action, client)
                    end
                end)
            end)
            return
        end

        -- 2. Named action patterns (in caller's priority order)
        if action_match_names then
            for _, name_pattern in ipairs(action_match_names) do
                for client_id, result in pairs(results) do
                    for _, action in ipairs(result.result or {}) do
                        if action.title and action.title:match(name_pattern) then
                            vim.notify("matched with" .. name_pattern)
                            vim.schedule(function()
                                local client = vim.lsp.get_client_by_id(client_id)
                                if client then
                                    apply_lsp_action(action, client)
                                end
                            end)
                            return
                        end
                    end
                end
            end
        end

        -- 3. Fallback
        vim.schedule(function()
            if fallback then
                fallback()
            else
                vim.notify("No code actions available", vim.log.levels.INFO)
            end
        end)
    end)
end

local resolve_first = function(list_actions)
    request_and_apply_first(list_actions, function()
        require("modules.java.static-import-explorer").quick_import()
    end)
end

local LspCodeAction = function()
    return {
        resolve_imports = function()
            local word = vim.fn.expand("<cword>")
            local import_util = require("utils.java.java-import-util")
            if word ~= "" and (import_util.static_import_exists(word, 0) or import_util.import_exists(word, 0)) then
                vim.notify("[Import] Already imported: " .. word, vim.log.levels.INFO)
                return
            end
            resolve_first()
        end,
        resolve_context = function()
            local lang_runner_resolver = require("plugins.overseer.tasks.lang-runner-resolver")
            local action_match_names = lang_runner_resolver.resolve().code_action_auto_resolve_match_names or {}
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
