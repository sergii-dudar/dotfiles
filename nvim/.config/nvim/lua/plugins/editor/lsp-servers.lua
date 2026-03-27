-- Patterns for YAML files that should be treated as OpenAPI specs
-- local openapi_patterns = { "openapi.yaml", "openapi.yml", "**/openapi/*.yaml", "**/openapi/*.yml" }

return {
    -- ###### openapi #######
    {
        "armsnyder/openapi-language-server",
        -- build = "go install github.com/armsnyder/openapi-language-server@latest",
        build = "go install .",
        ft = { "yaml" },
        lazy = true,
    },
    {
        "neovim/nvim-lspconfig",
        ft = { "yaml" },
        opts = {
            servers = {
                ["openapi-language-server"] = {
                    cmd = { vim.fn.expand("~/go/bin/openapi-language-server") },
                    filetypes = { "yaml" },
                    root_dir = vim.fs.root(0, { ".git" }) or vim.uv.cwd(),
                    single_file_support = true,
                },
                -- yamlls = {
                --     -- Override LazyVim's before_init to load SchemaStore and then
                --     -- apply our openapi_patterns last so they win over SchemaStore's
                --     -- bare "openapi.yaml" which doesn't match files in subdirectories
                --     before_init = function(_, new_config)
                --         local schemas = require("schemastore").yaml.schemas()
                --         schemas["https://www.schemastore.org/openapi-3.X.json"] = openapi_patterns
                --         new_config.settings.yaml.schemas =
                --             vim.tbl_deep_extend("force", new_config.settings.yaml.schemas or {}, schemas)
                --     end,
                -- },
            },
        },
    },
}
