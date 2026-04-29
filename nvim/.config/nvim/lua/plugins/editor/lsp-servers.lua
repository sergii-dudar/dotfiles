return {
    -- ###### openapi #######
    -- openapi-language-server is started conditionally from ftplugin/yaml.lua
    {
        -- "armsnyder/openapi-language-server",
        "sergii-dudar/openapi-language-server",
        build = "go install .",
        ft = { "yaml" },
        lazy = true,
    },
}