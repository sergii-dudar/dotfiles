-- JDTLS (Java LSP) configuration
local jdtls = require('jdtls')
local mason = require('mason-registry')

local jdtls_path = mason.get_package('jdtls'):get_install_path()
local java_debug_path = mason.get_package('java-debug-adapter'):get_install_path()
local java_test_path = mason.get_package('java-test'):get_install_path()

local equinox_launcher_path = vim.fn.glob(jdtls_path .. '/plugins/org.eclipse.equinox.launcher_*.jar')
local system = 'linux'
if vim.fn.has 'win32' then
    system = 'win'
elseif vim.fn.has 'mac' then
    system = 'mac'
end
local config_path = vim.fn.glob(jdtls_path .. '/config_' .. system)
local lombok_path = jdtls_path .. '/lombok.jar'

local project_name = vim.fn.fnamemodify(vim.fn.getcwd(), ':p:h:t')
local workspace_dir = vim.env.HOME .. '/jdtls-workspace/' .. project_name
local java21_dir = "/home/serhii/.sdkman/candidates/java/21.0.2-oracle"
local java21_bin = java21_dir .. "/bin/java";

-- Needed for debugging
local bundles = {
    vim.fn.glob('/home/serhii/.local/share/nvim/mason/share/java-debug-adapter/com.microsoft.java.debug.plugin.jar'),
}

-- Needed for running/debugging unit tests
vim.list_extend(bundles, vim.split(vim.fn.glob("/home/serhii/.local/share/nvim/mason/share/java-test/*.jar", 1), "\n"))

-- See `:help vim.lsp.start_client` for an overview of the supported `config` options.
local config = {
    -- The command that starts the language server
    -- See: https://github.com/eclipse/eclipse.jdt.ls#running-from-the-command-line
    cmd = {
        --'java',
        java21_bin,
        '-Declipse.application=org.eclipse.jdt.ls.core.id1',
        '-Dosgi.bundles.defaultStartLevel=4',
        '-Declipse.product=org.eclipse.jdt.ls.core.product',
        '-Dlog.protocol=true',
        '-Dlog.level=ALL',
        '-javaagent:' .. lombok_path,
        '-Xmx4g',
        '--add-modules=ALL-SYSTEM',
        '--add-opens', 'java.base/java.util=ALL-UNNAMED',
        '--add-opens', 'java.base/java.lang=ALL-UNNAMED',
        -- Eclipse jdtls location
        '-jar', equinox_launcher_path,
        -- TODO Update this to point to the correct jdtls subdirectory for your OS (config_linux, config_mac, config_win, etc)
        '-configuration', config_path,
        '-data', workspace_dir
    },

    -- This is the default if not provided, you can remove it. Or adjust as needed.
    -- One dedicated LSP server & client will be started per unique root_dir
    root_dir = require('jdtls.setup').find_root({ '.git', 'mvnw', 'pom.xml', 'build.gradle' }),

    -- Here you can configure eclipse.jdt.ls specific settings
    -- See https://github.com/eclipse/eclipse.jdt.ls/wiki/Running-the-JAVA-LS-server-from-the-command-line#initialize-request
    settings = {
        java = {
            -- TODO Replace this with the absolute path to your main java version (JDK 17 or higher)
            home = java21_dir,
            configuration = {
                updateBuildConfiguration = "interactive",
                -- TODO Update this by adding any runtimes that you need to support your Java projects and removing any that you don't have installed
                -- The runtime name parameters need to match specific Java execution environments.  See https://github.com/tamago324/nlsp-settings.nvim/blob/2a52e793d4f293c0e1d61ee5794e3ff62bfbbb5d/schemas/_generated/jdtls.json#L317-L334
                runtimes = {
                    --{
                    --    name = "JavaSE-11",
                    --    path = "/usr/lib/jvm/java-11-openjdk-amd64",
                    --},
                    --{
                    --    name = "JavaSE-17",
                    --    path = "/usr/lib/jvm/java-17-openjdk-amd64",
                    --},
                    --{
                    --    name = "JavaSE-19",
                    --    path = "/usr/lib/jvm/java-19-openjdk-amd64",
                    --}
                    {
                        name = "JavaSE-21",
                        path = java21_dir,
                    }
                }
            },
            eclipse = {
                downloadSources = true,
            },
            maven = {
                downloadSources = true,
            },
            implementationsCodeLens = {
                enabled = true,
            },
            referencesCodeLens = {
                enabled = true,
            },
            inlayHints = {
                parameterNames = {
                    enabled = "all" --'none',
                }
            },
            references = {
                includeDecompiledSources = true,
            },
            signatureHelp = {
                enabled = true,
                description = {
                    enabled = true,
                },
            },
            format = {
                enabled = true,
                -- Formatting works by default, but you can refer to a specific file/URL if you choose
                -- settings = {
                --   url = "https://github.com/google/styleguide/blob/gh-pages/intellij-java-google-style.xml",
                --   profile = "GoogleStyle",
                -- },
            },
            contentProvider = { preferred = 'fernflower' }, -- For decompiling sources with Lombok annotations
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
                "java",
                "javax",
                "com",
                "org"
            },
        },
        extendedClientCapabilities = jdtls.extendedClientCapabilities,
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
    },
    -- Needed for auto-completion with method signatures and placeholders
    capabilities = require('cmp_nvim_lsp').default_capabilities(),
    flags = {
        allow_incremental_sync = true,
    },
    init_options = {
        -- References the bundles defined above to support Debugging and Unit Testing
        bundles = bundles
    },
}

-- Needed for debugging
config['on_attach'] = function(client, bufnr)
    jdtls.setup_dap({ hotcodereplace = 'auto' })
    require('jdtls.dap').setup_dap_main_class_configs()
end

-- This starts a new client & server, or attaches to an existing client & server based on the `root_dir`.
jdtls.start_or_attach(config)
