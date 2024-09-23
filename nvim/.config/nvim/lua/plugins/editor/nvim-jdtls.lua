local system = 'linux'
--if vim.fn.has 'win32' then
--    system = 'win'
--elseif vim.fn.has 'mac' then
--    system = 'mac'
--end

local home = os.getenv('HOME')
local project_name = vim.fn.fnamemodify(vim.fn.getcwd(), ':p:h:t')
local workspace_dir = vim.env.HOME .. '/jdtls-workspace/' .. project_name

local java21_dir = vim.fn.glob(home .. "/.sdkman/candidates/java/21.*-oracle")
local java21_bin = java21_dir .. "/bin/java";
--local java_google_style_file = home .. "/dotfiles/work/eclipse-java-google-style.xml";

--local mason = require('mason-registry')
--local jdtls_path = mason.get_package('jdtls'):get_install_path()
local jdtls_path = home .. "/.local/share/nvim/mason/packages/jdtls"
local lombok_path = jdtls_path .. '/lombok.jar'
local equinox_launcher_path = vim.fn.glob(jdtls_path .. '/plugins/org.eclipse.equinox.launcher_*.jar')
local config_path = vim.fn.glob(jdtls_path .. '/config_' .. system)
--print("config_path: "..config_path)
--print("config.cmd" .. vim.inspect(config.cmd))
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
        --cmd = {
        --    java21_bin,
        --    --'-Declipse.application=org.eclipse.jdt.ls.core.id1',
        --    --'-Dosgi.bundles.defaultStartLevel=4',
        --    --'-Declipse.product=org.eclipse.jdt.ls.core.product',
        --    --'-Dlog.protocol=true',
        --    --'-Dlog.level=ALL',
        --    --'-javaagent:' .. lombok_path,
        --    --'-Xmx4g',
        --    --'--add-modules=ALL-SYSTEM',
        --    --'--add-opens', 'java.base/java.util=ALL-UNNAMED',
        --    --'--add-opens', 'java.base/java.lang=ALL-UNNAMED',
        --    -- Eclipse jdtls location
        --    '-jar', equinox_launcher_path,
        --    --'-configuration', config_path,
        --    --'-data', workspace_dir
        --},
        --full_cmd = function(opts)
        --    return opts
        --end,
        --[[
            By default LazyVim java are using - ~/.local/share/nvim/mason/packages/jdtls/bin/jdtls pythod wrapper
            that comes with jdtls server that automatically detect all optimal cmd values need to start jdtls server,
            because of that, no need to do it manually for 99.9% cases!
            SEE: ~/.local/share/nvim/mason/packages/jdtls/bin/jdtls.py for more details
        ]]
        settings = {
            java = {
                autobuild = {
                    enabled = false
                },
                saveActions = {
                    organizeImports = true
                },
                configuration = {
                    runtimes = {
                        {
                            name = "JavaSE-21",
                            path = java21_dir,
                        }
                    },
                },
                format = {
                    enabled = false, -- see conform.nvim
                    --settings = {
                    --    url = java_google_style_file,
                    --    profile = "GoogleStyle",
                    --},
                },
                eclipse = {
                    downloadSources = true,
                },
                inlayHints = {
                    parameterNames = {
                        enabled = "all" --none, all,
                    },
                },
                maven = {
                    downloadSources = true,
                },
                references = {
                    includeDecompiledSources = true,
                },
                referencesCodeLens = {
                    enabled = true,
                },
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
                    organizeImports = {
                        starThreshold = 9999,
                        staticStarThreshold = 9999,
                    },
                },
                redhat = { telemetry = { enabled = false } },
                codeGeneration = {
                    toString = {
                        template = "${object.className}{${member.name()}=${member.value}, ${otherMembers}}",
                    },
                    useBlocks = true,
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
--return {
--    "mfussenegger/nvim-jdtls",
--    opts = {
--        jdtls = function(opts)
--            -- JDTLS (Java LSP) configuration
--            local jdtls = require('jdtls')
--            local mason = require('mason-registry')
--
--            local jdtls_path = mason.get_package('jdtls'):get_install_path()
--            local java_debug_path = mason.get_package('java-debug-adapter'):get_install_path()
--            local java_test_path = mason.get_package('java-test'):get_install_path()
--
--            local equinox_launcher_path = vim.fn.glob(jdtls_path .. '/plugins/org.eclipse.equinox.launcher_*.jar')
--            local system = 'linux'
--            if vim.fn.has 'win32' then
--                system = 'win'
--            elseif vim.fn.has 'mac' then
--                system = 'mac'
--            end
--            local config_path = vim.fn.glob(jdtls_path .. '/config_' .. system)
--            local lombok_path = jdtls_path .. '/lombok.jar'
--
--            local project_name = vim.fn.fnamemodify(vim.fn.getcwd(), ':p:h:t')
--            local workspace_dir = vim.env.HOME .. '/jdtls-workspace/' .. project_name
--            local java21_dir = "/home/serhii/.sdkman/candidates/java/21.0.2-oracle"
--            local java21_bin = java21_dir .. "/bin/java";
--
--            local jvmArg = "-javaagent:" .. jdtls_path .. "/lombok.jar"
--            table.insert(opts.cmd, "--jvm-arg=" .. jvmArg)
--
--            --local my_table = {
--            --    name = "Serhii",
--            --    age = 30,
--            --    skills = { "Lua", "Java", "Spring Boot" }
--            --}
--
--            -- Dump the table using vim.inspect()
--            --print("dump: " .. vim.inspect(my_table))
--            print("dump: my test dump log")
--
--            opts.settings = {
--                java = {
--                    configuration = {
--                        runtimes = {
--                            {
--                                name = "JavaSE-21",
--                                path = java21_dir,
--                            }
--                        },
--                    },
--                    format = {
--                        enabled = false,
--                        --settings = {
--                        --    url = vim.fn.expand("~/Workspace/eclipse-java-google-style.xml"),
--                        --    profile = "GoogleStyle",
--                        --},
--                    },
--                    eclipse = {
--                        downloadSources = true,
--                    },
--                    inlayHints = {
--                        parameterNames = {
--                            enabled = "all",
--                        },
--                    },
--                    maven = {
--                        downloadSources = true,
--                    },
--                    references = {
--                        includeDecompiledSources = true,
--                    },
--                    referencesCodeLens = {
--                        enabled = true,
--                    },
--                },
--                completion = {
--                    favoriteStaticMembers = {
--                        "org.hamcrest.MatcherAssert.assertThat",
--                        "org.hamcrest.Matchers.*",
--                        "org.hamcrest.CoreMatchers.*",
--                        "org.junit.jupiter.api.Assertions.*",
--                        "java.util.Objects.requireNonNull",
--                        "java.util.Objects.requireNonNullElse",
--                        "org.mockito.Mockito.*",
--                    },
--                    importOrder = {
--                        "java",
--                        "javax",
--                        "com",
--                        "org"
--                    },
--                },
--                extendedClientCapabilities = jdtls.extendedClientCapabilities,
--                sources = {
--                    organizeImports = {
--                        starThreshold = 9999,
--                        staticStarThreshold = 9999,
--                    },
--                },
--                redhat = { telemetry = { enabled = false } },
--                codeGeneration = {
--                    toString = {
--                        template = "${object.className}{${member.name()}=${member.value}, ${otherMembers}}",
--                    },
--                    useBlocks = true,
--                },
--            }
--
--            return opts
--        end,
--    },
--}