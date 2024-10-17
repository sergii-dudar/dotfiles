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
                -- Optional formatter profile name from the Eclipse formatter settings.
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
            enabled = true,
            -- Defines a list of static members or types with static members. Content
            -- assist will propose those static members even if the import is missing.
            favoriteStaticMembers = {
                "org.hamcrest.MatcherAssert.assertThat",
                "org.hamcrest.Matchers.*",
                "org.hamcrest.CoreMatchers.*",
                "org.junit.jupiter.api.Assertions.*",
                "java.util.Objects.requireNonNull",
                "java.util.Objects.requireNonNullElse",
                "org.mockito.Mockito.*",
            },
            -- Defines the type filters. All types whose fully qualified name matches
            -- the selected filter strings will be ignored in content assist or quick
            -- fix proposals and when organizing imports. For example 'java.awt.*' will
            -- hide all types from the awt packages.
            filteredTypes = {
                "com.sun.*",
                "io.micrometer.shaded.*",
                "java.awt.*",
                "jdk.*",
                "sun.*",
            },
            -- Defines the sorting order of import statements. A package or type name
            -- prefix (e.g. 'org.eclipse') is a valid entry. An import is always added
            -- to the most specific group.
            importOrder = {
                "", -- Import all other imports
                "java", -- java.*
                "javax", -- javax.*
                "jakarta", -- jakarta.*
                "#"        -- Import static all other imports
            },
            chain = {
                enabled = false,
            },
        },
        sources = {
            -- How many classes from a specific package should be imported before automatic imports combine them all into a single import
            organizeImports = {
                -- Specifies the number of imports added before a star-import declaration is used.
                --starThreshold = 9999,
                starThreshold = 5,
                -- Specifies the number of static imports added before a star-import declaration is used.
                --staticStarThreshold = 9999,
                staticStarThreshold = 3,
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
                    -- Whether to load lombok processors from project classpath
                    enabled = true
                },
                protofBufSupport = {
                    -- Specify whether to automatically add Protobuf output source directories to the classpath.
                    enabled = true
                }
            }
        },
        compile = {
            nullAnalysis = {
                mode = "automatic",
                nullable = {
                    "javax.annotation.Nullable",
                    "jakarta.annotation.Nullable",
                    "org.eclipse.jdt.annotation.Nullable",
                    "org.springframework.lang.Nullable"
                },
                nonnull = {
                    "javax.annotation.Nonnull",
                    "jakarta.annotation.Nonnull",
                    "org.eclipse.jdt.annotation.NonNull",
                    "org.springframework.lang.NonNull",
                }
            }
        }
    }
}

return M