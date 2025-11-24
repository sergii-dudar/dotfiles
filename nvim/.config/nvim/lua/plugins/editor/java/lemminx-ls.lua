local jdtls_util = require("utils.java.jdtls-config-util")
local lemminx_jars = {}
for _, bundle in ipairs(vim.split(vim.fn.glob("$HOME/tools/nvim-extensions/lemminx/lemminx-ls/*.jar"), "\n")) do
    table.insert(lemminx_jars, bundle)
end
for _, bundle in ipairs(vim.split(vim.fn.glob("$HOME/tools/nvim-extensions/lemminx/lemminx-maven/*.jar"), "\n")) do
    table.insert(lemminx_jars, bundle)
end
-- NOTE: install lemminx with maven extension by run - ~/dotfiles/bin/install/nvim/ls/lemminx-maven/load_and_build_all.sh
-- dd(vim.fn.join(lemminx_jars, ":"))

return {
    "neovim/nvim-lspconfig",
    opts = {
        servers = {
            lemminx = {
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
                        -- Format#  insertSpaces  https://github.com/eclipse-lemminx/lemminx/blob/main/docs/Configuration.md#all-formatting-options
                        format = {
                            enabled = true, -- Enable/disable XML formatting
                            insertSpaces = true, -- indent using spaces
                            tabSize = 4, -- amount of spaces to indent by if insertSpaces == true
                            maxLineWidth = 9999, -- Max line width for formatting (Not supported by legacy formatter)
                            spaceBeforeEmptyCloseTag = false, -- Insert space before end of self-closing tags
                            preserveAttributeLineBreaks = true, -- Preserve line breaks before and after attributes
                            preserveEmptyContent = true, -- Preserve empty whitespace content (Legacy formatter only)
                            preservedNewlines = 2, -- Number of blank lines to leave between tags
                            splitAttributes = "preserve", -- Split node attributes onto multiple lines: preserve/splitNewLine/alignWithFirstAttr
                            -- splitAttributesIndentSize = 2, -- Indentation level for attributes when split
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
