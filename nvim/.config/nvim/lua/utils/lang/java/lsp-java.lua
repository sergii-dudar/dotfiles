local lsp_lang_common = require("utils.lang.lsp-common")

local M = {}

M.code_action_auto_resolve_match_names = {
    "Add all missing imports",
    "Convert to static import %(replace all occurrences%)",
    -- "Convert to static import",
    "Correct package declaration",
    "Rename file to",
    -- "Rename type to",
    "Convert to method reference",
    "Convert to lambda expression",
    "Change body block to expression",
    "Change body expression to block",
    "Change type of '",
    "Create method '",
    "Add unimplemented methods",
}

--- Request code actions from all LSP clients, apply the first matching one
--- (respecting pattern priority order), or invoke a fallback if none match.
---@param action_match_names string[]
---@param fallback? fun()
local request_and_apply_first = function(action_match_names, fallback)
    local bufnr = vim.api.nvim_get_current_buf()
    local clients = vim.lsp.get_clients({ bufnr = bufnr })
    local offset_encoding = clients[1] and clients[1].offset_encoding or "utf-16"
    local params = vim.lsp.util.make_range_params(0, offset_encoding)
    ---@diagnostic disable-next-line: inject-field
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
                    lsp_lang_common.apply_lsp_action(match.action, client)
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
                        lsp_lang_common.apply_lsp_action(match.action, client)
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
                                    lsp_lang_common.apply_lsp_action(action, client)
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

function M.resolve_imports()
    local word = vim.fn.expand("<cword>")
    local import_util = require("utils.java.java-import-util")
    if word ~= "" and (import_util.static_import_exists(word, 0) or import_util.import_exists(word, 0)) then
        vim.notify("[Import] Already imported: " .. word, vim.log.levels.INFO)
        return
    end
    resolve_first()
end

return M