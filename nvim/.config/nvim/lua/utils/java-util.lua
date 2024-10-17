local home = os.getenv('HOME')
local project_name = vim.fn.fnamemodify(vim.fn.getcwd(), ':p:h:t')
local workspace_dir = vim.env.HOME .. '/jdtls-workspace/' .. project_name

--local java_google_style_file = home .. "/dotfiles/work/formatter/eclipse-java-google-style.xml";

local system = 'linux'
--local jdtls_path = mason.get_package('jdtls'):get_install_path()
--local jdtls_path = home .. "/.local/share/nvim/mason/packages/jdtls"
--local lombok_path = jdtls_path .. '/lombok.jar'
--local equinox_launcher_path = vim.fn.glob(jdtls_path .. '/plugins/org.eclipse.equinox.launcher_*.jar')
--local config_path = vim.fn.glob(jdtls_path .. '/config_' .. system)
--print("config_path: "..config_path)
--print("config.cmd" .. vim.inspect(config.cmd))


local java21_dir = vim.fn.glob(home .. "/.sdkman/candidates/java/21.*-oracle")
local java21_bin = java21_dir .. "/bin/java"
--local java_google_style_file = home .. "/dotfiles/work/formatter/intellij-java-google-style.xml"
--local java_google_style_file = home .. "/dotfiles/work/formatter/eclipse-java-google-style.xml"
local java_google_style_file = home .. "/dotfiles/work/formatter/default_intellij_eclipse.xml"
--vim.notify(java_google_style_file)

local M = {}

M.is_installed = function(package_name, package_version)
    local mason_reg = require('mason-registry')
    local pkg = mason_reg.get_package(package_name)
    local is_installed = pkg:is_installed()

    if not is_installed then
        return false
    end

    local installed_version
    pkg:get_installed_version(function(ok, version)
        if not ok then
            return
        end

        installed_version = version
    end)

    return installed_version == package_version
end

M.install_pkgs = function(packages)
    local mason_reg = require('mason-registry')
    for _, dep in ipairs(packages) do
        if not M.is_installed(dep.name, dep.version) then
            local pkg = mason_reg.get_package(dep.name)

            pkg:install({
                version = dep.version,
                force = true,
            })
        end
    end
end

M.java21_dir = java21_dir
M.java_google_style_file = java_google_style_file
M.get_spring_boot_tools_path_ls_path = function()

    --M.install_pkgs({{ name = 'spring-boot-tools', version = '1.55.1' }})
    --local mason_reg = require('mason-registry')
    --local spring_boot_tools_path = mason_reg.get_package('spring-boot-tools'):get_install_path()
    --vim.notify(spring_boot_tools_path .. "/extension/language-server")
    --return spring_boot_tools_path .. "/extension/language-server"

    --local mason_reg = require('mason-registry')
    return home .. "/.local/share/nvim/mason/packages/spring-boot-tools/extension"
end

M.jdtls_settings = {
    java = {
        home = vim.fn.glob(home .. "/.sdkman/candidates/java/21.*-oracle/"),
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
                    name = "JavaSE-17",
                    path = vim.fn.glob(home .. "/.sdkman/candidates/java/17.*-oracle/"),
                },
                {
                    name = "JavaSE-21",
                    path = vim.fn.glob(home .. "/.sdkman/candidates/java/21.*-oracle/"),
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
                url = java_google_style_file,
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

return M