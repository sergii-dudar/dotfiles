local jdtls_util = require("utils.java.java-common")
-- local java_util = require("utils.java.java-common")
local lemminx_jars = {}
for _, bundle in ipairs(vim.split(vim.fn.glob("$HOME/tools/java-extensions/lemminx/lemminx-ls/*.jar"), "\n")) do
    table.insert(lemminx_jars, bundle)
end
for _, bundle in ipairs(vim.split(vim.fn.glob("$HOME/tools/java-extensions/lemminx/lemminx-maven/*.jar"), "\n")) do
    table.insert(lemminx_jars, bundle)
end
-- NOTE: install lemminx with maven extension by run - ~/dotfiles/scripts/install/nvim/ls/lemminx-maven/load_and_build_all.sh
-- dd(vim.fn.join(lemminx_jars, ":"))

-- NOTE: cometime lemminx mave have some issue (Non-resolvable parent POM) with detecting last vertion in case pattern version like
-- in this case can help additing to .m2 lib root: maven-metadata-local.xml with setting last version to it. looks some issue with private remotes

return {
    "neovim/nvim-lspconfig",
    opts = {
        servers = {
            lemminx = {
                on_exit = function(code, signal, client_id)
                    if code ~= 0 then
                        vim.notify("lemminx crashed (code " .. code .. "), restarting...", vim.log.levels.WARN)
                        vim.defer_fn(function()
                            vim.cmd("LspStart lemminx")
                        end, 1000)
                    end
                end,
                -- capabilities = {
                --     workspace = {
                --         didChangeConfiguration = {
                --             dynamicRegistration = true,
                --         },
                --     },
                -- },
                cmd = {
                    jdtls_util.java_bin,
                    "-cp",
                    vim.fn.join(lemminx_jars, ":"),
                    "org.eclipse.lemminx.XMLServerLauncher",
                },
                filetypes = { "xml", "xsd", "xsl", "xslt", "svg" },
                root_dir = vim.fs.root(0, { ".git" }) or vim.uv.cwd(),
                single_file_support = true,
                settings = {
                    xml = {
                        -- logs = {
                        --     client = true,
                        --     file = "/tmp/lemminx.log",
                        -- },
                        -- maven = {
                        --     userSettings = vim.fn.expand("~/.m2/settings.xml"),
                        --     repo = {
                        --         ["local"] = vim.fn.expand("~/.m2/repository"),
                        --     },
                        --     central = {
                        --         skip = true,
                        --     },
                        -- },
                        -- Format:
                        -- https://github.com/eclipse-lemminx/lemminx/blob/main/docs/Configuration.md#all-formatting-options
                        -- https://github.com/redhat-developer/vscode-xml/blob/main/docs/Formatting.md#xmlformatxsischemalocationsplit
                        format = {

                            enabled = true, -- Enable/disable XML formatting
                            insertSpaces = true, -- indent using spaces
                            tabSize = 4, -- amount of spaces to indent by if insertSpaces == true
                            maxLineWidth = 0, -- Disable line-width-based content wrapping (0 disables the feature)
                            spaceBeforeEmptyCloseTag = false, -- Insert space before end of self-closing tags
                            preserveAttributeLineBreaks = true, -- Preserve line breaks before and after attributes
                            preserveEmptyContent = true, -- Preserve empty whitespace content (Legacy formatter only)
                            preservedNewlines = 2, -- Number of blank lines to leave between tags
                            splitAttributes = "alignWithFirstAttr", -- Split node attributes onto multiple lines: preserve/splitNewLine/alignWithFirstAttr
                            splitAttributesIndentSize = 3, -- Indentation level for attributes when split
                            trimTrailingWhitespace = true,
                            -- closingBracketNewLine = false, -- Put closing bracket on a new line for tags with multiple attributes
                            -- emptyElements = "ignore", -- Handling of empty elements: ignore/collapse/expand
                            -- xsiSchemaLocationSplit = "onPair", -- How to format xsi:schemaLocation content
                            -- enforceQuoteStyle = "ignore", -- Enforce preferred quote style or ignore
                            -- grammarAwareFormatting = true, -- Use Schema/DTD grammar information (Not supported by legacy formatter)
                            -- joinCDATALines = false, -- Join lines in CDATA content
                            -- joinCommentLines = true, -- Join lines in comments
                            -- joinContentLines = false, -- Normalize whitespace in element content
                            -- legacy = false, -- Use legacy XML formatter
                            -- preserveSpace = { -- Element names to preserve space
                            --     "literallayout",
                            --     "pre",
                            --     "programlisting",
                            --     "screen",
                            --     "synopsis",
                            --     "xd:pre",
                            --     "xsl:comment",
                            --     "xsl:processing-instruction",
                            --     "xsl:text",
                            -- },
                        },
                    },
                },
            },
        },
    },
}