-- This is the same as in lspconfig.configs.jdtls, but avoids
-- needing to require that when this module loads.
local java_filetypes = { "java" }

-- Utility function to extend or override a config table, similar to the way
-- that Plugin.opts works.
---@param config table
---@param custom function | table | nil
local function extend_or_override(config, custom, ...)
    if type(custom) == "function" then
        config = custom(config, ...) or config
    elseif custom then
        config = vim.tbl_deep_extend("force", config, custom) --[[@as table]]
    end
    return config
end

local logger = require("utils.logging-util").new({
    name = "jdtls-config",
    filename = "jdtls-config.log",
})

--- Return the major/minor ASM version bundled with Mason JDTLS.
local function jdtls_asm_version()
    local jars = vim.fn.glob("$MASON/packages/jdtls/plugins/org.objectweb.asm_*.jar", false, true)
    local jar = jars[1]
    if not jar then
        return nil
    end

    return vim.fn.fnamemodify(jar, ":t"):match("org%.objectweb%.asm_([%d%.]+)%.jar")
end

--- Return whether a dotted version is at least the supplied major/minor version.
local function version_at_least(version, major, minor)
    local version_major, version_minor = (version or ""):match("^(%d+)%.(%d+)")
    version_major = tonumber(version_major)
    version_minor = tonumber(version_minor)
    if not version_major or not version_minor then
        return false
    end

    return version_major > major or (version_major == major and version_minor >= minor)
end

--- Return a jar manifest as whitespace-normalized text.
local function jar_manifest_text(jar)
    local lines = vim.fn.systemlist({ "unzip", "-p", jar, "META-INF/MANIFEST.MF" })
    if vim.v.shell_error ~= 0 then
        return ""
    end

    return table.concat(lines, ""):gsub("%s+", "")
end

--- Return whether a Java test bundle requires an ASM range incompatible with JDTLS ASM 9.10+.
local function java_test_bundle_requires_old_asm(jar)
    local manifest = jar_manifest_text(jar)
    return manifest:find("org.objectweb.asm", 1, true)
        and (manifest:find("[9.9.0,9.10)", 1, true) ~= nil or manifest:find("[9.9.0,9.10.0)", 1, true) ~= nil)
end

--- Return whether JDTLS already ships the same bundle jar (same symbolic name + version).
--- OSGi rejects installing a duplicate name+version from a different location, which fails
--- the whole extension bundle list. The version is part of the jar filename, so an exact
--- filename match in the JDTLS plugins dir means an exact bundle collision.
local function jdtls_ships_same_bundle(jar)
    local name = vim.fn.fnamemodify(jar, ":t")
    local jdtls_plugin = vim.fn.expand("$MASON/packages/jdtls/plugins/" .. name)
    return vim.fn.filereadable(jdtls_plugin) == 1
end

--- Drop bundle paths that resolve (through symlinks) to a jar already in the list; order is kept.
---@param jars string[]
---@return string[]
local function dedupe_bundles(jars)
    local seen, out, dropped = {}, {}, {}
    for _, jar in ipairs(jars) do
        local real = vim.uv.fs_realpath(jar) or jar
        if seen[real] then
            table.insert(dropped, vim.fn.fnamemodify(jar, ":t"))
        else
            seen[real] = true
            table.insert(out, jar)
        end
    end
    if #dropped > 0 then
        logger.fmt_info("dropped duplicate bundle paths: %s", table.concat(dropped, ", "))
    end
    return out
end

--- Return whether a Mason java-test jar should be passed as a JDTLS extension bundle.
local function should_skip_java_test_bundle(jar, asm_version)
    local name = vim.fn.fnamemodify(jar, ":t")
    if name == "jacocoagent.jar" or name:match("%-jar%-with%-dependencies%.jar$") then
        return true, "runtime payload"
    end

    if jdtls_ships_same_bundle(jar) then
        return true, "same bundle name+version already shipped with JDTLS"
    end

    if version_at_least(asm_version, 9, 10) and java_test_bundle_requires_old_asm(jar) then
        return true, string.format("requires ASM < 9.10 but JDTLS has ASM %s", asm_version)
    end

    return false, nil
end

return {
    recommended = function()
        return LazyVim.extras.wants({
            ft = "java",
            root = {
                "build.gradle",
                "build.gradle.kts",
                "build.xml", -- Ant
                "pom.xml", -- Maven
                "settings.gradle", -- Gradle
                "settings.gradle.kts", -- Gradle
            },
        })
    end,

    -- Add java to treesitter.
    {
        "nvim-treesitter/nvim-treesitter",
        opts = { ensure_installed = { "java" } },
    },

    -- Ensure java debugger and test packages are installed.
    {
        "mfussenegger/nvim-dap",
        optional = true,
        opts = function()
            -- Simple configuration to attach to remote java debug process
            -- Taken directly from https://github.com/mfussenegger/nvim-dap/wiki/Java
            local dap = require("dap")
            dap.configurations.java = {
                {
                    type = "java",
                    request = "attach",
                    name = "Debug (Attach) - Remote",
                    hostName = "127.0.0.1",
                    port = 5005,
                },
            }
        end,
        dependencies = {
            {
                "mason-org/mason.nvim",
                opts = { ensure_installed = { "java-debug-adapter", "java-test" } },
            },
        },
    },

    -- Configure nvim-lspconfig to install the server automatically via mason, but
    -- defer actually starting it to our configuration of nvim-jtdls below.
    {
        "neovim/nvim-lspconfig",
        opts = {
            -- make sure mason installs the server
            servers = {
                jdtls = {},
            },
            setup = {
                jdtls = function()
                    return true -- avoid duplicate servers
                end,
            },
        },
    },

    -- Set up nvim-jdtls to attach to java files.
    {
        "mfussenegger/nvim-jdtls",
        dependencies = { "folke/which-key.nvim" },
        ft = java_filetypes,
        opts = function()
            local cmd = { vim.fn.exepath("jdtls") }

            -- Lombok agent
            if LazyVim.has("mason.nvim") then
                local lombok_jar = vim.fn.expand("$MASON/share/jdtls/lombok.jar")
                table.insert(cmd, string.format("--jvm-arg=-javaagent:%s", lombok_jar))
            end

            --==============================================================================
            -- JVM tuning (Java 25, 32 GB RAM, one JDTLS instance per microservice)
            --==============================================================================
            -- Measured 2026-10-28 on payment-card-transfer: JVM + OSGi boot is ~1.5 s of a ~14 s
            -- start-to-usable; the rest is the m2e import and the initial workspace build (CPU bound).
            -- Keep the JVM defaults unless a flag has a measured reason to differ.
            --
            -- Removed on purpose (the default is better or equal on this machine):
            --   -Xms4g                      the launcher already passes -Xms1G; 4g only delayed the first GC
            --                               and left ~3 GB RSS of uncollected garbage per idle instance
            --   -XX:+UseG1GC                default collector
            --   -XX:MaxGCPauseMillis=100    default 200; a lower target shrinks the young gen and adds GC
            --                               work in the build phase (an LSP does not need 100 ms pauses)
            --   -XX:G1ReservePercent=10     default
            --   -XX:+TieredCompilation      default
            --   -XX:CICompilerCount=2       ergonomic default here is 4; 2 halves JIT throughput in the
            --                               hottest phase (initial build / indexing)
            --   -XX:MaxMetaspaceSize=1g     default unlimited; a cap only turns a big workspace into
            --                               "OutOfMemoryError: Metaspace"
            --   -Dsun.zip.disableMemoryMapping=true  ignored since JDK 9 (ZipFile reimplemented in Java)
            --   -XX:TieredStopAtLevel=1     saved ~2 s startup but slowed workspace builds (no C2), widening
            --                               the window where "Go to Implementation" fails for generated
            --                               sources (MapStruct etc.)
            --   Class Data Sharing (-XX:+AutoCreateSharedArchive): measured ~0.1 s of a 1.5 s boot, needs
            --                               AllowArchivingWithJavaAgent because of Lombok and conflicts with
            --                               the launcher's --add-modules / --add-opens. Not worth it.

            -- Heap cap. Equals the 25 % ergonomic default on a 32 GB machine; explicit so smaller
            -- workstations get the same ceiling. The heap grows from the launcher's -Xms1G as needed.
            table.insert(cmd, "--jvm-arg=-Xmx8g")
            -- Spring Boot workspaces hold many duplicate strings; G1 string dedup saves memory at little cost.
            table.insert(cmd, "--jvm-arg=-XX:+UseStringDeduplication")
            -- Idle instances give committed heap back to the OS (JEP 346): a periodic concurrent cycle
            -- every 5 minutes without other GC activity, so several open microservices do not pin
            -- their peak RSS while the user works elsewhere.
            table.insert(cmd, "--jvm-arg=-XX:G1PeriodicGCInterval=300000")
            -- JDTLS loads several hundred MB of classes; a larger initial metaspace avoids the early
            -- metaspace-triggered full GCs during startup (the default initial size is ~21 MB).
            table.insert(cmd, "--jvm-arg=-XX:MetaspaceSize=256m")
            -- Long-running with full C2: more room than the 240 MB default so the JIT never stops
            -- compiling. Reserved address space, not committed memory, so it costs nothing until used.
            table.insert(cmd, "--jvm-arg=-XX:ReservedCodeCacheSize=512m")

            -- Alternative: ZGC for ultra-low latency (Java 15+, great for Java 25)
            -- ZGC has lower pause times but uses more memory - good if responsiveness is critical
            -- do
            --     table.insert(cmd, "--jvm-arg=-XX:+UseZGC")                  -- Use ZGC (sub-millisecond pauses)
            --     table.insert(cmd, "--jvm-arg=-XX:ZCollectionInterval=5")    -- Proactive GC every 5s
            --     table.insert(cmd, "--jvm-arg=-XX:ZUncommitDelay=300")       -- Return unused memory after 5min
            -- end
            --==============================================================================
            -- End JVM tuning
            --==============================================================================
            return {
                root_dir = function(path)
                    -- `vim.lsp.config.jdtls` comes from nvim-lspconfig's lsp/jdtls.lua; when nvim-jdtls is
                    -- loaded before it (e.g. an early require), root_markers is nil and vim.fs.root() finds
                    -- nothing — the server would then start without -data in the launcher's default
                    -- workspace (seen 2026-10-28). Fall back to the usual markers.
                    local cfg = vim.lsp.config.jdtls
                    local markers = cfg and cfg.root_markers
                        or {
                            "pom.xml",
                            "mvnw",
                            "build.gradle",
                            "build.gradle.kts",
                            "settings.gradle",
                            "settings.gradle.kts",
                            "gradlew",
                            ".git",
                        }
                    return vim.fs.root(path, markers)
                end,

                -- How to find the project name for a given root dir.
                project_name = function(root_dir)
                    return root_dir and vim.fs.basename(root_dir)
                end,

                -- Where are the config and workspace dirs for a project?
                jdtls_config_dir = function(project_name)
                    return vim.fn.stdpath("cache") .. "/jdtls/" .. project_name .. "/config"
                end,
                jdtls_workspace_dir = function(project_name)
                    return vim.fn.stdpath("cache") .. "/jdtls/" .. project_name .. "/workspace"
                end,

                -- How to run jdtls. This can be overridden to a full java command-line
                -- if the Python wrapper script doesn't suffice.
                cmd = cmd,
                full_cmd = function(opts)
                    local fname = vim.api.nvim_buf_get_name(0)
                    local root_dir = opts.root_dir(fname)
                    local project_name = opts.project_name(root_dir)
                    local cmd = vim.deepcopy(opts.cmd)
                    if project_name then
                        vim.list_extend(cmd, {
                            "-configuration",
                            opts.jdtls_config_dir(project_name),
                            "-data",
                            opts.jdtls_workspace_dir(project_name),
                        })
                    end
                    return cmd
                end,

                -- These depend on nvim-dap, but can additionally be disabled by setting false here.
                dap = { hotcodereplace = "auto", config_overrides = {} },
                -- Can set this to false to disable main class scan, which is a performance killer for large project
                dap_main = {},
                --#########################################
                --###### Custom Jdtls Config START ########
                --#########################################
                test = true, -- should be enabled, or vscode_java_test bundle will not be added
                -- test = { -- using jdtls tests to debug tests, as neotest-java have some issues with dap (for now)
                --     config_overrides = {
                --         vmArgs = string.format(
                --             "-javaagent:%s/tools/java-extensions/jmockit/jmockit.jar",
                --             os.getenv("HOME")
                --         ),
                --     },
                -- },
                settings = require("utils.java.jdtls-config-util").jdtls_settings,
                --#####################################
                --################ END ################
                --#####################################
            }
        end,
        --- Configure nvim-jdtls, Java tooling bundles, and the adaptive formatter.
        config = function(_, opts)
            require("utils.java.jdtls-adaptive-formatter").setup()

            -- Find the extra bundles that should be passed on the jdtls command-line
            -- if nvim-dap is enabled with java debug/test.
            local bundles = {} ---@type string[]
            if LazyVim.has("mason.nvim") then
                local mason_registry = require("mason-registry")
                if opts.dap and LazyVim.has("nvim-dap") and mason_registry.is_installed("java-debug-adapter") then
                    bundles =
                        vim.fn.glob("$MASON/share/java-debug-adapter/com.microsoft.java.debug.plugin-*jar", false, true)
                    -- java-test also depends on java-debug-adapter.
                    if opts.test and mason_registry.is_installed("java-test") then
                        -- vim.list_extend(bundles, vim.fn.glob("$MASON/share/java-test/*.jar", false, true))

                        -- ============== >>> Recover JDTLS start (1)
                        -- Filter out non-OSGi runtime jars. JDTLS init fails the
                        -- entire bundle list if any entry is not a valid bundle
                        -- (CoreException: Failed to load extension bundles).
                        -- The test runner jar and jacoco agent jar are runtime
                        -- payloads injected into test JVMs, not JDTLS bundles.
                        local asm_version = jdtls_asm_version()
                        local skipped_java_test_bundles = {}
                        for _, jar in ipairs(vim.fn.glob("$MASON/share/java-test/*.jar", false, true)) do
                            local name = vim.fn.fnamemodify(jar, ":t")
                            local skip, skip_reason = should_skip_java_test_bundle(jar, asm_version)
                            if skip then
                                table.insert(skipped_java_test_bundles, string.format("%s (%s)", name, skip_reason))
                            else
                                table.insert(bundles, jar)
                            end
                        end
                        if #skipped_java_test_bundles > 0 then
                            logger.fmt_warn(
                                "skipped java-test bundles: %s",
                                table.concat(skipped_java_test_bundles, ", ")
                            )
                        end
                        -- ============== <<< Recover JDTLS end (1)
                    end
                end
            end

            --###########################################
            --######## Custom Extensions START ##########
            --###########################################
            ---
            -- Include spring boot ls bundle if present
            ---
            -- vim.list_extend(bundles, vim.fn.glob("$MASON/share/vscode-java-decompiler/bundles/*.jar", false, true))

            -- local vscode_java_debug = vim.fn.glob("$HOME/.vscode/extensions/vscjava.vscode-java-debug-0.58.2025112507/server/*.jar", false, true)
            -- local vscode_java_test = vim.fn.glob("$HOME/.vscode/extensions/vscjava.vscode-java-test-*/server/*.jar", false, true)
            -- print(vscode_java_debug)
            -- print(vscode_java_test)

            -- vim.list_extend(bundles, vscode_java_debug)
            -- vim.list_extend(bundles, vscode_java_test)

            if LazyVim.has("java-deps.nvim") then
                local vscode_java_dependency = vim.fn.glob("$MASON/share/vscode-java-dependency/*.jar", false, true)
                -- print(vscode_java_dependency)
                vim.list_extend(bundles, vscode_java_dependency)
            end

            if LazyVim.has("spring-boot.nvim") then
                vim.list_extend(bundles, require("spring_boot").java_extensions())
            end

            -- Mason ships some bundles twice under one directory: a versioned jar plus an unversioned
            -- symlink to it (java-test's com.microsoft.java.test.plugin.jar). Passing both makes jdtls
            -- install/uninstall the same bundle twice and refresh the bundle set on every start
            -- ("Uninstalled … Installed …" pairs in the workspace .log). Keep one path per real file.
            bundles = dedupe_bundles(bundles)

            -- Java refactor file manager integrations (neo-tree, oil.nvim, snacks rename)
            require("modules.java.refactor.integrations").setup()

            opts.is_config_updated = false
            opts.on_attach = function(args)
                if not opts.is_config_updated then
                    -- vim.cmd("JdtUpdateConfig") -- push jdtls to update config to fix ghost diagnostics after up
                    -- Delay is important: JDTLS needs to finish startup first

                    -- local timer = vim.uv.new_timer()
                    -- timer:start(
                    --     5000,
                    --     0,
                    --     vim.schedule_wrap(function()
                    --         -- need to regenerate (repick) generated source codes like [mapscruct etc]
                    --         vim.notify("🏄 Jdt updating started...")
                    --         vim.cmd("JdtUpdateConfig")
                    --         -- vim.cmd("JdtCompile full")
                    --         opts.is_config_updated = true
                    --         timer:close()
                    --     end)
                    -- )
                end
            end

            --#####################################
            --################ END ################
            --#####################################

            --- Attach JDTLS to the current Java buffer unless it was manually stopped.
            local function attach_jdtls()
                local recovery = require("utils.java.jdtls-recovery")
                if recovery.is_manually_stopped() then
                    return
                end

                local fname = vim.api.nvim_buf_get_name(0)
                -- Don't attach LSP to Java files outside current working directory
                if require("utils.java.java-common").if_java_file_outside() then
                    return
                end

                -- Configuration can be augmented and overridden by opts.jdtls
                local root_dir = opts.root_dir(fname)
                if not root_dir then
                    -- Without a root there is no project name, hence no -configuration/-data: the launcher
                    -- would pick a shared default workspace (~/Library/Caches/jdtls/jdtls-<sha1(cwd)>).
                    logger.fmt_warn("jdtls: no project root found for %s — not starting", fname)
                    return
                end
                -- Per-root copy of the settings: only the importer of the build tool present at
                -- the root is enabled (Buildship otherwise boots on every Maven project and asks
                -- services.gradle.org for its version list at each start).
                local settings = require("utils.java.jdtls-config-util").settings_for_root(root_dir, opts.settings)
                local config = extend_or_override({
                    cmd = opts.full_cmd(opts),
                    root_dir = root_dir,
                    init_options = {
                        bundles = bundles,
                        -- The same settings also travel in the initialization options. jdtls applies
                        -- those in `initialize`, before it registers its preference listener; the
                        -- `workspace/didChangeConfiguration` Neovim sends afterwards then carries no
                        -- change. Sent only via didChangeConfiguration, `java.format.settings.url`
                        -- counts as a formatter change (null -> url) and
                        -- StandardProjectsManager.configureSettings(prefs, true) schedules the
                        -- "Clean workspace..." CLEAN build at every start: every class file rewritten,
                        -- every generated source deleted and regenerated (measured 2026-10-28/29 on a
                        -- project copy: 596/596 class files rewritten and the clean job in the trace
                        -- without this, no clean job with it).
                        settings = settings,
                    },
                    settings = settings,
                    filetypes = { "java" }, -- need to plugins like pretty-hover
                    -- enable CMP capabilities
                    capabilities = LazyVim.has("blink.cmp") and require("blink.cmp").get_lsp_capabilities()
                        or LazyVim.has("cmp-nvim-lsp") and require("cmp_nvim_lsp").default_capabilities()
                        or nil,
                }, opts.jdtls)

                -- Existing server will be reused if the root_dir matches.
                require("jdtls").start_or_attach(config)
                -- not need to require("jdtls.setup").add_commands(), start automatically adds commands
            end

            -- Attach the jdtls for each java buffer. HOWEVER, this plugin loads
            -- depending on filetype, so this autocmd doesn't run for the first file.
            -- For that, we call directly below.
            vim.api.nvim_create_autocmd("FileType", {
                pattern = java_filetypes,
                callback = attach_jdtls,
            })

            -- Setup keymap and dap after the lsp is fully attached.
            -- https://github.com/mfussenegger/nvim-jdtls#nvim-dap-configuration
            -- https://neovim.io/doc/user/lsp.html#LspAttach
            --- Restart JDTLS through the recovery module so old diagnostics are cleared.
            local function restart_jdtls_from_buffer_command()
                vim.notify("JDTLS: restarting...", vim.log.levels.INFO)
                require("utils.java.jdtls-recovery").restart("JdtRestart command")
            end

            --- Wipe the workspace cache and restart through the recovery module so old
            --- diagnostics are cleared. Shadows nvim-jdtls' :JdtWipeDataAndRestart, which
            --- starts a new client (new id -> new diagnostic namespace) without resetting
            --- the old one, causing duplicates to stack on every restart.
            local function wipe_and_restart_jdtls_from_buffer_command()
                vim.notify("JDTLS: wiping workspace cache and restarting...", vim.log.levels.INFO)
                require("utils.java.jdtls-recovery").hard_restart("JdtWipeDataAndRestart command")
            end

            vim.api.nvim_create_autocmd("LspAttach", {
                callback = function(args)
                    local client = vim.lsp.get_client_by_id(args.data.client_id)
                    if client and client.name == "jdtls" then
                        -- nvim-jdtls clobbers buffer-local 'path' with one module's source dirs
                        -- and stamps it onto every jdtls buffer, breaking cross-module `gf` on
                        -- classpath resources. Re-append a repo-root recursive entry to fix it.
                        require("utils.java.jdtls-gf-path").restore_multimodule_gf_path(args.buf)

                        -- Auto-refresh project config once workspace settles, so APT-generated
                        -- sources (MapStruct impls etc.) wiped during m2e configure get regenerated.
                        require("utils.java.jdtls-workspace-watcher").setup(client, args.buf)
                        vim.api.nvim_buf_create_user_command(
                            args.buf,
                            "JdtRestart",
                            restart_jdtls_from_buffer_command,
                            {
                                desc = "Restart JDTLS and clear stale diagnostics",
                                force = true,
                            }
                        )
                        vim.api.nvim_buf_create_user_command(
                            args.buf,
                            "JdtWipeDataAndRestart",
                            wipe_and_restart_jdtls_from_buffer_command,
                            {
                                desc = "Wipe JDTLS workspace cache and restart, clearing stale diagnostics",
                                force = true,
                            }
                        )
                        local wk = require("which-key")
                        wk.add({
                            {
                                mode = "n",
                                buffer = args.buf,
                                { "<leader>cx", group = "extract" },
                                { "<leader>cxv", require("jdtls").extract_variable_all, desc = "Extract Variable" },
                                { "<leader>cxc", require("jdtls").extract_constant, desc = "Extract Constant" },
                                { "<leader>cgs", require("jdtls").super_implementation, desc = "Goto Super" },
                                { "<leader>cgS", require("jdtls.tests").goto_subjects, desc = "Goto Subjects" },
                                { "<leader>co", require("jdtls").organize_imports, desc = "Organize Imports" },
                            },
                        })
                        wk.add({
                            {
                                mode = "x",
                                buffer = args.buf,
                                { "<leader>cx", group = "extract" },
                                {
                                    "<leader>cxm",
                                    [[<ESC><CMD>lua require('jdtls').extract_method(true)<CR>]],
                                    desc = "Extract Method",
                                },
                                {
                                    "<leader>cxv",
                                    [[<ESC><CMD>lua require('jdtls').extract_variable_all(true)<CR>]],
                                    desc = "Extract Variable",
                                },
                                {
                                    "<leader>cxc",
                                    [[<ESC><CMD>lua require('jdtls').extract_constant(true)<CR>]],
                                    desc = "Extract Constant",
                                },
                            },
                        })

                        if LazyVim.has("mason.nvim") then
                            local mason_registry = require("mason-registry")
                            if
                                opts.dap
                                and LazyVim.has("nvim-dap")
                                and mason_registry.is_installed("java-debug-adapter")
                            then
                                -- custom init for Java debugger
                                require("jdtls").setup_dap(opts.dap)
                                -- The main-class discovery is a workspace scan (vscode.java.resolveMainClass);
                                -- run it once per jdtls client, not on every Java buffer that attaches.
                                if opts.dap_main and not client._dap_main_configs_done then
                                    ---@diagnostic disable-next-line: inject-field
                                    client._dap_main_configs_done = true
                                    require("jdtls.dap").setup_dap_main_class_configs(opts.dap_main)
                                end

                                -- disabled jdtls test keybind in favor of neotest-java or custom runner
                                -- Java Test require Java debugger to work
                                --[[ if opts.test and mason_registry.is_installed("java-test") then
                                    -- custom keymaps for Java test runner (not yet compatible with neotest)
                                    wk.add({
                                        {
                                            mode = "n",
                                            buffer = args.buf,
                                            { "<leader>t", group = "test" },
                                            {
                                                "<leader>tC", -- "<leader>tt",
                                                function()
                                                    require("jdtls.dap").test_class({
                                                        config_overrides = type(opts.test) ~= "boolean"
                                                                and opts.test.config_overrides
                                                            or nil,
                                                    })
                                                end,
                                                desc = "Debug All Test (Jdtls)", -- "Run All Test",
                                            },
                                            {
                                                "<leader>td", -- "<leader>tr",
                                                function()
                                                    require("jdtls.dap").test_nearest_method({
                                                        config_overrides = type(opts.test) ~= "boolean"
                                                                and opts.test.config_overrides
                                                            or nil,
                                                    })
                                                end,
                                                desc = "Debug Nearest Test (Jdtls)", -- "Run Nearest Test",
                                            },
                                            {
                                                "<leader>tP", -- "<leader>tT",
                                                require("jdtls.dap").pick_test,
                                                desc = "Debug Pick Test (Jdtls)", -- "Run Test",
                                            },
                                            {
                                                "<leader>tD",
                                                function()
                                                    require("dap").run_last()
                                                end,
                                                desc = "Debug Re-Run Last (Jdtls)",
                                            },
                                        },
                                    })
                                end ]]
                            end
                        end

                        -- User can set additional keymaps in opts.on_attach
                        if opts.on_attach then
                            opts.on_attach(args)
                        end
                    end
                end,
            })

            -- Avoid race condition by calling attach the first time, since the autocmd won't fire.
            attach_jdtls()

            -- ============== >>> Recover JDTLS start (2)
            -- Recover JDTLS after system sleep. VimResume does not fire on system
            -- sleep and FocusGained is unreliable in tmux, so detection is done
            -- via wall-clock gap on cheap idle events. See module for details.
            require("utils.java.jdtls-recovery").setup(function(_)
                attach_jdtls()
            end)
            -- ============== <<< Recover JDTLS end (2)
        end,
    },
}
