local home = os.getenv("HOME")
-- local jdtlsc_util = require("utils.java.jdtls-config-util")
local java_util = require("utils.java.java-common")
-- local notify_title = { title = "Spring Boot Tools LS" }
--vim.lsp.set_log_level("warn")

-- stylua: ignore start

-- ---------------------------- java parse trace ----------------------------
vim.keymap.set("v", "<leader>jtp", function() require("utils.java.java-trace").parse_selected_trace_to_qflist() end, { desc = "[J]ava selected [t]race [p]arse to qflist" })
vim.keymap.set("n", "<leader>jtp", function() require("utils.java.java-trace").parse_buffer_trace_to_qflist() end, { desc = "[J]ava buffer [t]race [p]arse to qflist" })
vim.keymap.set("n", "<leader>jtl", function() require("utils.java.java-trace").parse_current_line_trace_to_qflist() end, { desc = "[J]ava [t]race current [l]ine parse to qflist" })
vim.keymap.set("n", "<leader>jtb", function() require("utils.java.java-trace").parse_trace_under_cursor_and_open_in_buffer() end, { desc = "[J]ava [t]race current line open in [b]uffer" })
-- vim.keymap.set("n", "<leader>jts", function() Snacks.scratch({ name = "Stack Trace Scratch", ft = "log" }) end, { desc = "[J]ava [t]race [s]cratch buffer" })
vim.keymap.set("n", "<leader>jts", function() require("utils.java.java-trace-scratch").openStackTraceScratch() end, { desc = "[J]ava [t]race [s]cratch buffer" })

-- map("n", "<leader>xh", function() require("utils.java.java-trace").highlight_java_test_trace_current_buf() end, { desc = "Highlight java stack trace" })
-- map("v", "<leader>xh", function() require("utils.java.java-trace").highlight_java_trace_selected() end, { desc = "Highlight java stack trace" })

-- ---------------------------- jdt links navigations, especially useful on jdtls hover navigations to decompiles classes
vim.keymap.set("n", "<leader>jla", function() require("utils.java.jdtls-util").extract_and_open_current_line_all_jdt_link() end, { desc = "Open [j]dt [l]inks [a]ll" })
vim.keymap.set("n", "<leader>jlc", function() require("utils.java.jdtls-util").extract_and_open_cursor_position_jdt_link() end, { desc = "Open [j]dt [l]ink Under [c]ursor" })
vim.keymap.set("n", "<leader>jlf", function() require("utils.java.jdtls-util").extract_and_open_current_line_first_jdt_link() end, { desc = "Open [j]dt [l]ink [f]irst" })

-- ---------------------------- code actions & lsp based extensions
-- vim.keymap.set("n", "<leader>cI", function() require("utils.java.java-import-util").import_class_and_replace() end, { desc = "[I]mport fqn class package and apply simple name" })
-- -- { "<leader>ci", function() require("utils.lsp-util").code_action.apply("Add all missing imports") end, desc = "Add all missing imports [jdtls]", }),
-- vim.keymap.set("n", "<leader>ce", function() require("utils.lsp-util").code_action.toggle("Change body expression to block", "Change body block to expression") end, { desc = "Toggle method body block/expressionn [jdtls]" })
-- vim.keymap.set("n", "<leader>ci", function() require("utils.lsp-util").code_action.resolve_imports() end, { desc = "Resolve imports [jdtls]" })
-- vim.keymap.set("n", "<leader>cc", function()
--     local action_names = require("utils.lang.java.lsp-java").code_action_auto_resolve_match_names
--     require("utils.lsp-util").code_action.resolve_context(action_names)
-- end, { desc = "Context Apply First Code Action [jdtls]" })

-- stylua: ignore end

return {
    {
        "folke/which-key.nvim",
        optional = true,
        opts = {
            spec = {
                { "<leader>j", group = "+java" },
                { "<leader>jt", group = "+java traces" },
                { "<leader>jl", group = "+jdtls links" },
                { "<leader>jc", group = "+jdtls code/compile" },
                { "<leader>jd", group = "+jdtls data control" },
            },
        },
    },
    {
        "folke/snacks.nvim",
        -- stylua: ignore
        keys = {
            -- { "<leader>b,", function() LazyVim.pick("files", { root = false, cwd = require("utils.java.java-common").get_buffer_project_path() }) end, desc = "Find File (Root of buffer)" },
            -- { "<leader>b/", function() LazyVim.pick("live_grep", { root = false, cwd = require("utils.java.java-common").get_buffer_project_path() }) end, desc = "Grep (Root of buffer)" },
            { "<leader>b,", function()
                local path = require("utils.java.java-common").get_buffer_project_path()
                local name = vim.fn.fnamemodify(path, ":t:r")
                Snacks.picker.files({ cwd = path, title = "Files in: " .. name })
            end, desc = "Find File (Root of buffer)" },
            { "<leader>b/", function()
                local path = require("utils.java.java-common").get_buffer_project_path()
                local name = vim.fn.fnamemodify(path, ":t:r")
                Snacks.picker.grep({ cwd = path, title = "Grep in: " .. name })
            end, desc = "Grep (Root of buffer)" },
        },
    },
    -- java code actions & lsp based extensions
    {
        "neovim/nvim-lspconfig",
        opts = {
            servers = {
                jdtls = {
                    -- stylua: ignore
                    keys = {
                        { "<leader>cI", function() require("utils.java.java-import-util").import_class_and_replace() end,  desc = "[I]mport fqn class package and apply simple name" },
                        { "<leader>cR", function() require("modules.java.refactor.integrations").snacks_rename_current() end, desc = "Rename File (Java)" },
                        -- { "<leader>ci", function() require("utils.lsp-util").code_action.apply("Add all missing imports") end, desc = "Add all missing imports [jdtls]", }),
                        { "<leader>ce", function() require("utils.lsp-util").code_action.toggle("Change body expression to block", "Change body block to expression") end, desc = "Toggle method body block/expressionn [jdtls]" },
                        { "<leader>ci", function() require("utils.lang.java.lsp-java").resolve_imports() end, desc = "Resolve imports [jdtls]" },
                        { "<leader>cc", function()
                            local action_names = require("utils.lang.java.lsp-java").code_action_auto_resolve_match_names
                            require("utils.lsp-util").code_action.resolve_context(action_names)
                        end,  desc = "Context Apply First Code Action [jdtls]" }
                    },
                },
            },
        },
    },
    -- JDTLS config based on LazyVim with Spring-Boot Tools LS support
    {
        "mfussenegger/nvim-jdtls",
        -- cond = java_util.is_java_project(),
        -- cond = global.is_not_limited,
        dependencies = {
            "mason-org/mason.nvim",
            "JavaHello/spring-boot.nvim",
        },
        -- stylua: ignore
        keys = {
            { "<leader>jcc", ":JdtCompile<CR>", desc = "JDTLS Compile" },
            { "<leader>jcf", ":JdtCompile full<CR>", desc = "JDTLS [C]ompile [F]ull" },
            { "<leader>jci", ":JdtCompile incremental<CR>", desc = "JDTLS [C]ompile [I]ncremental" },
            -- In general, when something is not working, “:JdtRestart” might fix things.
            { "<leader>jdr", ":JdtRestart<CR>", desc = "JDTLS [R]estart" },
            -- If you add a dependency to the POM or change one of the existing dependencies version, 
            -- you must run “:JdtUpdateConfig” so the Java LSP can download the new dependencies.
            -- { "<leader>jdu", ":JdtUpdateConfig<CR>", desc = "JDTLS Update Config" },
            { "<leader>jdu", function() require("jdtls").update_project_config() end, desc = "JDTLS Update Config [module of current buf]" },
            { "<leader>jdU", function() require("jdtls").update_projects_config({ select_mode = "all" }) end, desc = "JDTLS Update Config [all modules]" },
            -- In case of misalignments of the workspace, e.g., if you wipe the local Maven cache (“~/.m2/repository”) when restarting 
            -- an already opened project, you get errors due to unresolved dependencies, you can run “:JdtWipeDataAndRestart”
            -- which will resolve the Maven dependencies from scratch
            { "<leader>jdf", ":JdtWipeDataAndRestart<CR>", desc = "JDTLS Wipe Data and [F]ull Restart" },
            { "<leader>jdg", function()
                local path = java_util.get_buffer_project_path()
                local dir = java_util.get_build_layout(path).generated_annotations_dir
                vim.fn.delete(dir, "rf")
                vim.notify("Cleared: " .. dir)
                require("jdtls").update_project_config()
            end, desc = "Clear generated-sources and refresh JDTLS" },
            -- pkill -f jdtls; pkill -f 'org.eclipse.equinox.launcher'   # kill all JDTLS java procs
            -- rm -rf ~/.cache/nvim/jdtls/<project>                       # then wipe
            { "<leader>tg", function() require("jdtls.tests"):generate() end, desc = "[G]enerate Tests (jdtls)", },
            { "<leader>tj", function() require("jdtls.tests").goto_subjects() end, desc = "[J]ump to tests (jdtls)", },
            -- Proj Dependency Search: 
            { "<leader>j,", function() require("modules.java.dependencies-search").find_files() end, desc = "Find File in Dependencies", },
            { "<leader>j/", function() require("modules.java.dependencies-search").grep() end, desc = "Grep in Dependencies", },
            { "<leader>je", function() require("modules.java.dependencies-search").explore() end, desc = "Explore Dependency (jar)" },
            { "<leader>jj", function() require("utils.java.jdtls-util").open_fqn_under_cursor() end, desc = "Open FQN Under Cursor" },
            { "<leader>ji", function() require("modules.java.static-import-explorer").quick_import() end, desc = "Static Import Quick" },
            { "<leader>jI", function() require("modules.java.static-import-explorer").find() end, desc = "Static Import Search" },
        },
        opts = {
            --[[jdtls = {
                on_attach = function()
                    require("spring_boot").init_lsp_commands()
                    LazyVim.info("jdtls lsp initialized", notify_title)
                end,
            },]]
            -- extend_jdtls_bundles = function(bundles)
            --     vim.list_extend(bundles, require("spring_boot").java_extensions())
            --     vim.notify("jdtls bundles extende ", vim.log.levels.INFO)
            -- end,
            -- not applicable here, see how applied in java-immutable.lua
            --[[ settings = jdtlsc_util.jdtls_settings,
            test = false, -- issue with java-test in latest mason module, using from vscode build instead ]]
        },
    },
    -- vs spring-boot tools ls to integrate in jdtls
    {
        "JavaHello/spring-boot.nvim", --"eslam-allam/spring-boot.nvim"
        version = "*",
        ft = { "java", "yaml", "properties", "yml" },
        dependencies = {
            "mfussenegger/nvim-jdtls",
        },
        opts = function()
            local util = require("spring_boot.util")
            return {
                ls_path = vim.fn.glob("$MASON/share/vscode-spring-boot-tools/*.jar"),
                log_file = home .. "/.local/state/nvim/spring-boot-ls.log",
                autocmd = false, -- disable default autocmd, we'll create our own
                server = {
                    on_init = function(client, ctx)
                        client.server_capabilities.inlayHintProvider = false -- disable to not conflict with jdtls inlay hint
                        client.server_capabilities.documentHighlightProvider = false
                        util.boot_ls_init(client, ctx)
                    end,
                    settings = {
                        -- options: ~/.local/share/nvim/mason/packages/vscode-spring-boot-tools/extension/package.json
                        ["spring-boot"] = {
                            ls = {
                                problem = {
                                    -- [ IGNORE, INFO, WARNING, HINT, ERROR ]
                                    ["application-properties"] = {
                                        -- spring-boot.ls.problem.application-properties.PROP_UNKNOWN_PROPERTY
                                        PROP_UNKNOWN_PROPERTY = "IGNORE",
                                    },
                                    ["application-yaml"] = {
                                        -- pring-boot.ls.problem.application-yaml.YAML_UNKNOWN_PROPERTY
                                        YAML_UNKNOWN_PROPERTY = "IGNORE",
                                        YAML_SHOULD_ESCAPE = "IGNORE",
                                        YAML_VALUE_TYPE_MISMATCH = "IGNORE",
                                        YAML_INVALID_BEAN_PROPERTY = "IGNORE",
                                    },
                                    boot2 = {
                                        JAVA_PUBLIC_BEAN_METHOD = "IGNORE",
                                        MISSING_CONFIGURATION_ANNOTATION = "IGNORE",
                                        JAVA_CONSTRUCTOR_PARAMETER_INJECTION = "IGNORE",
                                    },
                                },
                            },
                        },
                    },
                },
            }
        end,
        config = function(_, opts)
            require("spring_boot").setup(opts)
            -- Create custom autocmd that checks if file is within cwd
            local launch = require("spring_boot.launch")
            local ls_config = launch.update_ls_config(opts)
            local group = vim.api.nvim_create_augroup("spring_boot_ls_custom", { clear = true })
            vim.api.nvim_create_autocmd("FileType", {
                group = group,
                pattern = { "java", "yaml", "jproperties" },
                desc = "Spring Boot Language Server (CWD check)",
                callback = function()
                    if require("utils.java.java-common").if_java_file_outside() then
                        return
                    end
                    -- Only start LSP if file is within current working directory
                    launch.start(ls_config)
                end,
            })
        end,
    },
    {
        -- "JavaHello/java-deps.nvim",
        "sergii-dudar/java-deps.nvim",
        ft = { "java" },
        lazy = true,
        -- stylua: ignore
        keys = {
            { "<leader>jdd", function() require('java-deps').toggle_outline() end, desc = "Toogle Java Dependencies" },
            -- :lua require('java-deps').open_outline()
            -- :lua require('java-deps').close_outline()
        },
        dependencies = {
            { "mfussenegger/nvim-jdtls" },
            { "hedyhli/outline.nvim" },
        },
        opts = {
            -- jdtls_name = "jdtls",
            options = {
                width = 60,
                show_relative_numbers = true,
                show_non_java_resources = true,
                position = "right",
                --     show_guides = true,
                --     auto_close = false,
                --     show_numbers = false,
                --     preview_bg_highlight = "Pmenu",
                --     winblend = 0,
                --     fold_markers = { "", "" },
                --     position = "right",
                --     wrap = false,
                --     hierarchical_view = true,
                --     keymaps = {
                --         close = "q",
                --         toggle_fold = "o",
                --     },
                --     symbols = {
                --         icons = {},
                --     },
            },
        },
        config = true,
    },
    -- -- Rename packages and imports also when renaming/moving files via nvim-tree (for Java) -- replaced with own refactor module
    -- {
    --     -- "simaxme/java.nvim",
    --     "sergii-dudar/java.nvim", -- my fork with [ neo-tree, oil.nvim, snacks rename ] support
    --     cond = lang_project.is("java") and global.is_not_limited,
    --     ft = "java",
    --     -- stylua: ignore
    --     keys = {
    --         { "<leader>cR", function() require("simaxme-java").snacks.rename_current() end, desc = "Rename File (Java)" },
    --     },
    --     dependencies = {
    --         "mfussenegger/nvim-jdtls",
    --         "nvim-tree/nvim-tree.lua",
    --     },
    --     config = function()
    --         require("simaxme-java").setup({
    --             rename = {
    --                 nvimtree = false,
    --                 neotree = true,
    --                 oilnvim = true,
    --             },
    --         })
    --     end,
    -- },
    -- Nice dependency tree viewers for Maven \ Gradle
    -- {
    --     "oclay1st/maven.nvim", -- "oclay1st/gradle.nvim",
    --     cmd = { "Maven", "MavenInit", "MavenExec", "MavenFavorites" },
    --     dependencies = {
    --         "nvim-lua/plenary.nvim",
    --         "MunifTanjim/nui.nvim",
    --     },
    --     opts = {}, -- options, see default configuration
    --     keys = {
    --         { "<leader>M", desc = "+Maven", mode = { "n", "v" } },
    --         { "<leader>Mm", "<cmd>Maven<cr>", desc = "Maven Projects" },
    --         { "<leader>Mf", "<cmd>MavenFavorites<cr>", desc = "Maven Favorite Commands" },
    --     },
    -- },
    -- {
    --     "neovim/nvim-lspconfig",
    --     cond = not java_util.is_java_project(),
    --     opts = {
    --         servers = {
    --             java_language_server = {},
    --         },
    --     },
    -- },
    -- {
    --     "elmcgill/springboot-nvim",
    --     ft = "java",
    --     dependencies = {
    --         "neovim/nvim-lspconfig",
    --         "mfussenegger/nvim-jdtls",
    --     },
    --     -- stylua: ignore
    --     config = function()
    --         local springboot_nvim = require("springboot-nvim")
    --         -- vim.keymap.set("n", "<leader>cjr", springboot_nvim.boot_run, { desc = "Spring Boot Run Project" })
    --         vim.keymap.set("n", "<leader>cjc", springboot_nvim.generate_class, { desc = "[J]ava Create [C]lass" })
    --         vim.keymap.set("n", "<leader>cji", springboot_nvim.generate_interface, { desc = "[J]ava Create [I]nterface" })
    --         vim.keymap.set("n", "<leader>cje", springboot_nvim.generate_enum, { desc = "[J]ava Create [E]num" })
    --         springboot_nvim.setup({})
    --     end,
    -- },
}
