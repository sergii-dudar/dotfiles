
--[[
additional navigation:

1. Using Neovim's Built-in Navigation
Neovim has built-in commands to move through jump locations:
Ctrl+o: Move to the previous location (similar to "back" in IntelliJ).
Ctrl+i: Move to the next location (similar to "forward" in IntelliJ).

2.
gf - go to file under cursor,
gF - go to file under cursor and to number after : (File.java:20)

3.
:[line number] - got to line number in buffer

4.
cfdo %s/serhii_dudar/just_serhii/g | update | bd
cfdo %s/just_serhii/serhii_dudar/g | update | bd

]]

local java_util = require("utils.java-util")

return {
    "mfussenegger/nvim-jdtls",
    dependencies = {
        "williamboman/mason.nvim",
    },
    keys = {
            { "<leader>jc", ":JdtCompile<CR>", desc = "JDTLS Compile" },
            { "<leader>jf", ":JdtCompile full<CR>", desc = "JDTLS Compile Full" },
            { "<leader>jr", ":JdtRestart<CR>", desc = "JDTLS Restart" },
    },
    opts = {
        --[[
            By default LazyVim java are using - ~/.local/share/nvim/mason/packages/jdtls/bin/jdtls pythod wrapper
            that comes with jdtls server that automatically detect all optimal cmd values need to start jdtls server,
            because of that, no need to do it manually for 99.9% cases!
            SEE: ~/.local/share/nvim/mason/packages/jdtls/bin/jdtls.py for more details
        ]]
        settings = {
            java = {
                home = java_util.java21_dir,
                autobuild = {
                    enabled = false
                },
                -- Setup automatical package import oranization on file save
                saveActions = {
                    organizeImports = true
                },
                configuration = {
                    runtimes = {
                        {
                            name = "JavaSE-21",
                            path = java_util.java21_dir,
                        }
                    },
                    -- If changes to the project will require the developer to update the projects configuration advise the developer before accepting the change
                    updateBuildConfiguration = "interactive"
                },
                -- Enable code formatting
                format = {
                    enabled = true,
                    -- Use the Google Style guide for code formattingh
                    settings = {
                        url = java_util.java_google_style_file,
                        profile = "GoogleStyle",
                    },
                },
                -- Enable downloading archives from eclipse automatically
                eclipse = {
                    downloadSources = true,
                },
                -- enable inlay hints for parameter names,
                inlayHints = {
                    parameterNames = {
                        enabled = "all" --none, all,
                    },
                },
                -- Enable downloading archives from maven automatically
                maven = {
                    downloadSources = true,
                },
                -- Enable method signature help
                signatureHelp = {
                    enabled = true,
                    description = {
                        enabled = true,
                    }
                },
                -- Use the fernflower decompiler when using the javap command to decompile byte code back to java code
                contentProvider = {
                    preferred = "fernflower"
                },
                references = {
                    includeDecompiledSources = true,
                },
                implementationsCodeLens = {
                    enabled = true,
                },
                -- enable code lens in the lsp
                referencesCodeLens = {
                    enabled = true,
                },
                -- Customize completion options
                completion = {
                    favoriteStaticMembers = {
                        "org.hamcrest.MatcherAssert.assertThat",
                        "org.hamcrest.Matchers.*",
                        "org.hamcrest.CoreMatchers.*",
                        "org.junit.jupiter.api.Assertions.*",
                        "java.util.Objects.requireNonNull",
                        "java.util.Objects.requireNonNullElse",
                        "org.mockito.Mockito.*",
                    },
                    -- Try not to suggest imports from these packages in the code action window
                    filteredTypes = {
                        "com.sun.*",
                        "io.micrometer.shaded.*",
                        "java.awt.*",
                        "jdk.*",
                        "sun.*",
                    },
                    -- Set the order in which the language server should organize imports
                    importOrder = {
                        "com",
                        "jakarta",
                        "javax",
                        "java",
                        "org"
                    },
                    chain = {
                        enabled = false,
                    },
                },
                sources = {
                    -- How many classes from a specific package should be imported before automatic imports combine them all into a single import
                    organizeImports = {
                        starThreshold = 9999,
                        staticStarThreshold = 9999,
                    },
                },
                redhat = { telemetry = { enabled = false } },
                -- How should different pieces of code be generated?
                codeGeneration = {
                    -- When generating toString use a json format
                    toString = {
                        template = "${object.className}{${member.name()}=${member.value}, ${otherMembers}}"
                    },
                    -- When generating hashCode and equals methods use the java 7 objects method
                    hashCodeEquals = {
                        useJava7Objects = true
                    },
                    -- When generating code use code blocks
                    useBlocks = true
                },
                jdt = {
                    ls = {
                        lombokSupport = {
                            enabled = true
                        },
                        protofBufSupport = {
                            enabled = true
                        }
                    }
                }
            }
        }
    }
}