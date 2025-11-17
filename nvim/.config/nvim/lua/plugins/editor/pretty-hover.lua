-- A small and customizable Neovim plugin for pretty printing the hover information from LSP servers
return {
    "Fildo7525/pretty_hover",
    event = "LspAttach",
    config = function()
        local cfg = require("pretty_hover.config")
        local h_util = require("pretty_hover.core.util")
        local local_hover_request = require("pretty_hover.local_request").local_hover_request
        local compatibility = require("pretty_hover.core.compatibility")
        local list_util = require("utils.list-util")

        local supported_lsp_names = {
            "jdtls",
        }

        local get_current_active_client_extended = function()
            for _, client in ipairs(compatibility.get_clients()) do
                if
                    h_util.tbl_contains(client.config.filetypes, vim.bo.filetype)
                    or list_util.any_match(client.name, supported_lsp_names)
                then
                    return client
                end
            end
            return nil
        end

        local extended_hover = function(config)
            local params = vim.lsp.util.make_position_params(0, "utf-16")

            -- Check if the server for this file type exists and supports hover.
            local client = get_current_active_client_extended()

            local hover_support_present = client and client.capabilities.textDocument.hover
            -- dd(client)

            if not client or not hover_support_present then
                vim.notify(
                    "There is no client for this filetype or the client does not support the hover capability.",
                    vim.log.levels.WARN
                )
                return
            end

            config = config or {}
            cfg:instance().hover_cnf = config

            vim.lsp.buf_request_all(0, "textDocument/hover", params, local_hover_request)
        end
        require("pretty_hover").setup({})
        -- vim.keymap.set("n", "<leader>k", require("pretty_hover").hover, { buffer = true, desc = "Pretty hover" })
        vim.keymap.set("n", "<leader>k", extended_hover, { desc = "Pretty hover" })
    end,
}
