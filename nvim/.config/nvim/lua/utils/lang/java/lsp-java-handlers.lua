-- Java LSP handler utilities: install jdtls-specific diagnostics and hover
-- post-processing while preserving the original Neovim LSP handlers.
--
-- - setup - install the Java publishDiagnostics and hover overrides

local java_arg_highlight = require("utils.java.java-arg-highlight")
local java_format_checker = require("utils.java.java-format-checker")
local jdtls_util = require("utils.java.jdtls-util")

local M = {}

--- Install Java-specific LSP handler overrides.
--- Filters generated-source diagnostics, refreshes Java argument highlights and
--- format checks after diagnostic publishes, and removes empty hover responses
--- while normalizing jdtls markdown links.
function M.setup()
    local original_publish = vim.lsp.diagnostic.on_publish_diagnostics
    java_format_checker.setup()
    vim.lsp.handlers["textDocument/publishDiagnostics"] = function(err, result, ctx, config)
        -- if result and result.uri then
        --     local path = result.uri
        --     if path:match("/target/generated%-sources/") or path:match("/build/generated/") then
        --         -- Drop non-error diagnostics from generated `Java`(by jdtls) sources
        --         result.diagnostics = vim.tbl_filter(function(d)
        --             return d.source == "Java" and d.severity == vim.lsp.protocol.DiagnosticSeverity.Error
        --         end, result.diagnostics)
        --     end
        -- end
        if result and result.uri and result.diagnostics then
            local is_generated = result.uri:match("/target/generated%-sources/")
                or result.uri:match("/build/generated/")
            local java_diags = {}
            local filtered = {}
            for _, d in ipairs(result.diagnostics) do
                if d.source == "Java" then
                    -- Drop non-error Java diagnostics from generated sources
                    if not is_generated or d.severity == vim.lsp.protocol.DiagnosticSeverity.Error then
                        filtered[#filtered + 1] = d
                        java_diags[#java_diags + 1] = d
                    end
                else
                    filtered[#filtered + 1] = d
                end
            end
            result.diagnostics = filtered
            local bufnr = vim.uri_to_bufnr(result.uri)
            if vim.bo[bufnr].filetype ~= "java" then
                ---@diagnostic disable-next-line: redundant-parameter
                return original_publish(err, result, ctx, config)
            end
            vim.schedule(function()
                -- Always call apply() — even with empty java_diags — so that stale
                -- per-arg warnings/highlights from a previous publish get cleared
                -- when jdtls no longer reports the underlying diagnostic.
                java_arg_highlight.apply(bufnr, java_diags)
                java_format_checker.apply(bufnr)
            end)
        end
        ---@diagnostic disable-next-line: redundant-parameter
        return original_publish(err, result, ctx, config)
    end

    local original_handler = vim.lsp.buf_request_all
    ---@diagnostic disable-next-line: duplicate-set-field
    vim.lsp.buf_request_all = function(bufnr, method, params, handler)
        if method ~= "textDocument/hover" then
            original_handler(bufnr, method, params, handler)
            return
        end

        -- INFO: extension with filtering empty hover results to not show them in pretty-hover plugin
        original_handler(bufnr, method, params, function(results, ctx)
            local non_empty_result = {}
            for client_id, resp in pairs(results) do
                local is_jdtls = vim.lsp.get_client_by_id(client_id).name == "jdtls"
                local contents = resp and resp.result and resp.result.contents
                if contents then
                    local cont_type = type(contents)
                    if cont_type == "string" and contents ~= "" then
                        if is_jdtls then
                            resp.result.contents = jdtls_util.convert_markdown_links_to_references(contents)
                        end
                        non_empty_result[client_id] = resp
                    elseif cont_type == "table" and not vim.tbl_isempty(contents) then
                        if is_jdtls then
                            -- LSP hover can return contents in different formats:
                            -- 1. MarkupContent: { kind = "markdown", value = "..." }
                            if contents.kind and contents.value then
                                contents.value = jdtls_util.convert_markdown_links_to_references(contents.value)
                            -- 2. MarkedString array: { "text", { language = "java", value = "code" }, ... }
                            else
                                for i, item in ipairs(contents) do
                                    if type(item) == "string" then
                                        contents[i] = jdtls_util.convert_markdown_links_to_references(item)
                                    elseif type(item) == "table" and item.value then
                                        item.value = jdtls_util.convert_markdown_links_to_references(item.value)
                                    end
                                end
                            end
                        end
                        non_empty_result[client_id] = resp
                    end
                end
            end
            handler(non_empty_result, ctx)
        end)
    end
end

return M
