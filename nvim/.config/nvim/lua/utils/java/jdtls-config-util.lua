local java_util = require("utils.java.java-common")
local home = os.getenv("HOME")
-- local project_name = vim.fn.fnamemodify(vim.fn.getcwd(), ":p:h:t")
-- local workspace_dir = vim.env.HOME .. "/jdtls-workspace/" .. project_name

local M = {}

-- ============================================================================
-- JDTLS FULL SETTINGS CONFIGURATION
-- urls:
-- - https://github.com/eclipse-jdtls/eclipse.jdt.ls/wiki/Running-the-JAVA-LS-server-from-the-command-line#initialize-request
-- - https://github.com/eclipse-jdtls/eclipse.jdt.ls/blob/main/org.eclipse.jdt.ls.core/src/org/eclipse/jdt/ls/core/internal/preferences/Preferences.java
-- - https://github.com/neovim/nvim-lspconfig/blob/master/lsp/jdtls.lua
--
-- PERFORMANCE OPTIMIZATIONS:
-- ✅ inlayHints: enabled (useful, low cost)
-- ❌ autobuild: disabled (HIGH COST - like IntelliJ, use :JdtCompile manually)
-- ❌ referencesCodeLens: disabled (HIGH COST - performance killer)
-- ❌ downloadSources: disabled (HIGH COST - auto-download slows indexing)
-- ❌ includeDecompiledSources: disabled (HIGH COST - slow search in references)
-- ❌ selectionRange: disabled (VERY LOW COST - safe to re-enable)
-- ❌ format.comments: disabled (LOW COST - safe to re-enable)
-- ❌ importHint: disabled (VERY LOW COST - safe to re-enable)
-- ❌ foldingRange: disabled (LOW-MEDIUM COST - safe to re-enable if you use folding)
-- NOTE: JDTLS still validates code in real-time without autobuild!
-- ============================================================================

M.jdtls_settings = {
    java = {
        -- ====================================================================
        -- GENERAL JAVA SETTINGS
        -- ====================================================================
        -- Specify Java home directory
        home = java_util.java_dir,
        -- JDK detection and configuration
        jdt = {
            ls = {
                -- Lombok support from project classpath
                lombokSupport = {
                    enabled = true,
                },
                -- Protocol Buffers support
                protofBufSupport = {
                    enabled = true,
                },
            },
        },
        -- ====================================================================
        -- PROJECT CONFIGURATION
        -- ====================================================================
        project = {
            -- Disable import hints (LOW COST - safe to re-enable if you want notifications)
            importHint = false,
            --[[ sourcePaths = {
                "src/main/java",
                "target/generated-sources/annotations",
            }, ]]
        },
        -- ====================================================================
        -- BUILD CONFIGURATION
        -- ====================================================================
        -- Automatic build settings
        autobuild = {
            -- Disabled for performance (like IntelliJ default)
            -- JDTLS still validates code in real-time without autobuild
            -- Use :JdtCompile manually when needed, or let Maven/Gradle build on run
            enabled = false,
        },
        -- Max number of concurrent builds
        maxConcurrentBuilds = 4,
        -- Compilation settings
        compile = {
            -- Null analysis annotations
            nullAnalysis = {
                -- NOTE: disabled, as nullAnalysis conflicting with lombok, expecially issues with apply project lombok.config
                mode = "disabled", -- "disabled", "interactive", or "automatic"
                nullable = {
                    "jakarta.annotation.Nullable",
                    --[[ "javax.annotation.Nullable",
                    "org.eclipse.jdt.annotation.Nullable",
                    "org.springframework.lang.Nullable",
                    "org.jetbrains.annotations.Nullable", ]]
                },
                nonnull = {
                    "jakarta.annotation.Nonnull",
                    --[[ "javax.annotation.Nonnull",
                    "org.eclipse.jdt.annotation.NonNull",
                    "org.springframework.lang.NonNull",
                    "org.jetbrains.annotations.NotNull", ]]
                },
            },
        },
        -- ====================================================================
        -- RUNTIME CONFIGURATION
        -- ====================================================================
        configuration = {
            -- Multiple JDK runtimes for different Java versions
            runtimes = {
                -- {
                --     name = "JavaSE-17",
                --     path = vim.fn.glob(home .. "/.sdkman/candidates/java/17.*-oracle/"),
                -- },
                {
                    name = "JavaSE-25",
                    path = java_util.java_dir,
                },
            },
            -- If changes to the project will require the developer to update the projects configuration advise the developer before accepting the change
            updateBuildConfiguration = "interactive", -- disabled, interactive, automatic
            maven = {
                userSettings = home .. "/.m2/settings.xml",
                globalSettings = home .. "/.m2/settings.xml",
            },
        },
        -- ====================================================================
        -- IMPORT SETTINGS (Maven/Gradle)
        -- ====================================================================
        import = {
            exclusions = {
                "**/.idea/**",
                "**/.settings/**",
                "**/.github/**",
                "**/node_modules/**",
                "**/.metadata/**",
                "**/archetype-resources/**",
                "**/META-INF/maven/**",
                "**/.git/**",
                "**/build/**",
                "**/target/classes/**",
                "**/target/test-classes/**",
                -- "**/target/**",
                -- "**/build/**",
            },
            maven = {
                enabled = true,
                offline = {
                    enabled = false,
                },
                -- disableTestClasspathFlag: false
                -- executable = home .. "/.sdkman/candidates/maven/current/bin/mvn",
            },
            gradle = {
                enabled = true,
                offline = {
                    enabled = false,
                },
                wrapper = {
                    enabled = true,
                },
            },
        },
        -- ====================================================================
        -- CODE COMPLETION
        -- ====================================================================
        completion = {
            enabled = true,
            -- Defines a list of static members or types with static members. Content
            -- assist will propose those static members even if the import is missing.
            favoriteStaticMembers = {
                -- "org.hamcrest.MatcherAssert.assertThat",
                -- "org.hamcrest.Matchers.*",
                -- "org.hamcrest.CoreMatchers.*",
                -- "org.junit.jupiter.api.Assertions.*",
                -- "java.util.Objects.requireNonNull",
                -- "java.util.Objects.requireNonNullElse",

                -- "org.junit.Assert.*",
                -- "org.junit.Assume.*",
                -- "org.junit.jupiter.api.Assertions.*",
                -- "org.junit.jupiter.api.Assumptions.*",
                -- "org.junit.jupiter.api.DynamicContainer.*",
                -- "org.junit.jupiter.api.DynamicTest.*",
                "org.assertj.core.api.Assertions",
                "org.assertj.core.api.Assertions.*",
                "org.assertj.core.api.Assertions.assertThat",
                "org.assertj.core.api.Assertions.assertThatThrownBy",
                "org.assertj.core.api.Assertions.assertThatExceptionOfType",
                "org.assertj.core.api.Assertions.catchThrowable",
                -- "java.util.Objects.requireNonNull",
                -- "java.util.Objects.requireNonNullElse",
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
                "org.graalvm.*",
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
                "#", -- Import static all other imports
            },

            -- "com",
            -- "lombok",
            -- "org",
            -- "ua",
            -- "java", -- java.*
            -- "javax", -- javax.*
            -- "jakarta", -- jakarta.*
            -- "", -- Import all other imports
            -- "#", -- Import static all other imports
            chain = {
                enabled = false,
            },
            -- guessMethodArguments = {
            --     -- enabled = true,
            --     enabled = {
            --         off = "off",
            --         insertParameterNames = "insertParameterNames",
            --         insertBestGuessedArguments = "insertBestGuessedArguments",
            --     },
            -- },
        },
        -- ====================================================================
        -- CODE FORMATTING
        -- ====================================================================
        format = {
            enabled = true,
            -- Use the Google Style guide for code formattingh
            settings = {
                url = java_util.java_formatter.file_url,
                -- Optional formatter profile name from the Eclipse formatter settings.
                profile = java_util.java_formatter.profile_name,
            },
            -- Format on type
            onType = {
                enabled = false,
            },
            -- Insert spaces
            insertSpaces = true,
            -- Tab size
            tabSize = 4,
            -- Disable comment formatting (LOW COST - only affects explicit formatting)
            comments = {
                enabled = false,
            },
        },
        -- ====================================================================
        -- SAVE ACTIONS
        -- ====================================================================
        saveActions = {
            -- Setup automatical package import oranization on file save
            organizeImports = true,
        },
        -- ====================================================================
        -- ORGANIZE IMPORTS
        -- ====================================================================
        sources = {
            -- How many classes from a specific package should be imported before automatic imports combine them all into a single import
            organizeImports = {
                -- Specifies the number of imports added before a star-import declaration is used.
                starThreshold = 9999,
                -- starThreshold = 5,
                -- Specifies the number of static imports added before a star-import declaration is used.
                staticStarThreshold = 9999,
                -- staticStarThreshold = 3,
            },
        },
        -- ====================================================================
        -- CODE GENERATION
        -- ====================================================================
        codeGeneration = {
            -- Generate method comments
            generateComments = false,
            -- Use blocks for generated code
            useBlocks = true,
            -- Insertion location
            insertionLocation = "afterCursor", -- or "beforeCursor", "lastMember"
            -- When generating toString use a json format
            toString = {
                template = "${object.className}{${member.name()}=${member.value}, ${otherMembers}}",
                codeStyle = "STRING_CONCATENATION", -- or "STRING_BUILDER", "STRING_FORMAT"
                skipNullValues = false,
                listArrayContents = true,
                limitElements = 0, -- 0 = unlimited
            },
            -- When generating hashCode and equals methods use the java 7 objects method
            hashCodeEquals = {
                useJava7Objects = true,
                useInstanceof = false,
            },
        },
        -- ====================================================================
        -- CODE LENS
        -- ====================================================================
        -- implementationCodeLens = "methods", -- all, types, methods
        -- enable code lens in the lsp
        referencesCodeLens = { -- shows inline information above methods, classes, and fields
            enabled = false, -- disabled for performance
        },
        -- ====================================================================
        -- INLAY HINTS
        -- ====================================================================
        inlayHints = {
            -- enable inlay hints for parameter names,
            parameterNames = {
                enabled = "all", -- 'none', 'literals', 'all'
            },
        },
        -- ====================================================================
        -- FOLDING
        -- ====================================================================
        -- Code folding regions (LOW-MEDIUM COST - calculates once per file)
        foldingRange = {
            enabled = false,
        },
        -- ====================================================================
        -- ECLIPSE SETTINGS
        -- ====================================================================
        eclipse = {
            -- Disable auto-downloading sources for better performance
            -- (you can manually download when needed)
            downloadSources = false,
        },
        -- ====================================================================
        -- CODE ACTIONS
        -- ====================================================================
        codeAction = {
            -- Sort members
            sortMembers = {
                avoidVolatileChanges = false,
            },
        },
        -- ====================================================================
        -- SELECTION RANGE
        -- ====================================================================
        -- Smart selection expansion (VERY LOW COST - only triggers on keybind)
        selectionRange = {
            enabled = false,
        },
        -- ====================================================================
        -- SIGNATURE HELP
        -- ====================================================================
        signatureHelp = {
            -- Enable method signature help
            enabled = true,
            description = {
                enabled = true,
            },
        },
        -- ====================================================================
        -- CONTENT PROVIDER
        -- ====================================================================
        contentProvider = {
            -- Use the fernflower decompiler when using the javap command to decompile byte code back to java code
            preferred = "fernflower",
        },
        -- ====================================================================
        -- RENAME
        -- ====================================================================
        rename = {
            enabled = true,
        },
        -- Disable auto-downloading Maven sources for better performance
        maven = {
            downloadSources = false, -- manually download when needed
            updateSnapshots = false, -- keep disabled for speed
        },
        references = {
            -- Disable searching decompiled sources in "Find References" for performance
            -- (Go to Definition will still work!)
            includeDecompiledSources = false,
        },
        redhat = { telemetry = { enabled = false } },
    },
}

return M
