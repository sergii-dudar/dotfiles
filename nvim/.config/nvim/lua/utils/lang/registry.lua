local M = {}
local entries_by_filetype ---@type table<string, lang.RegistryEntry>|nil

---Project-root detection metadata for a primary language.
---Consumed by `utils.lang.lang-project` to decide which primary editor config can load.
---@class lang.RegistryProject
---@field markers string[] Root marker filenames used by `utils.lang.lang-project` to detect a primary project from the current working directory. Examples: `pom.xml`, `Cargo.toml`, `go.mod`.
---@field exts table<string, boolean> Source-file extensions, without the leading dot, used as a marker-less fallback when no project root marker is found. Example: `{ rs = true }`.

---Overseer runner registration metadata.
---Connects Neovim filetypes to a runner module that implements the shared `task.lang.Runner` contract.
---@class lang.RegistryRunner
---@field module string Lua module path for the Overseer runner contract implementation under `lua/plugins/overseer/tasks/lang/`. The module should return a `task.lang.Runner`.
---@field filetypes string[] Neovim filetypes handled by this runner. These become keys in `lang-runner-resolver` and drive run/test/debug template availability.

---Test-report integration metadata.
---Connects filetypes to report parsers, Overseer report components, Trouble sources, and diagnostic filters.
---@class lang.RegistryReport
---@field module string Lua module path for the language test-report shim under `lua/modules/<lang>/test-report`. Requiring it registers the language adapter with `modules.common.test-report.registry`.
---@field component string Overseer component name used by `run_tests.lua` to process reports after a test task completes. Components live under `lua/overseer/component/`, so `test_report.cargo_report` maps to `lua/overseer/component/test_report/cargo_report.lua`.
---@field filetypes string[] Neovim filetypes whose test reports use this metadata. Used by `test-report-dispatcher.lua` for output, tree view, Trouble, and diagnostics picker actions.
---@field trouble_source string Trouble source name opened/toggled for test diagnostics produced by the language adapter. Must match the adapter's `trouble_source`.
---@field diagnostic_source string `vim.diagnostic` source value used for test diagnostics. Must match the adapter's `diagnostic_source` so diagnostics pickers can filter correctly.

---Top-level language metadata entry.
---A single entry can describe primary-language project detection, supported-language runners, and test-report wiring.
---@class lang.RegistryEntry
---@field name string Stable language identifier used in docs, registry checks, and primary-language detection results. Examples: `java`, `rust`, `python`.
---@field primary? boolean Marks this language as a primary/main language. Primary entries participate in project detection and can gate `plugins.editor.<lang>` imports.
---@field project? lang.RegistryProject Project detection metadata. Required when `primary = true`; ignored for supported-only languages.
---@field runner? lang.RegistryRunner Overseer run/test/debug metadata. Omit when a language has no custom runner integration.
---@field report? lang.RegistryReport Test-report metadata for report parsing, diagnostics, Trouble integration, output panels, and tree view support.

-- To disable any language, just uncomment it (and don't forget uncomment related language extras `lazyvim.plugins.extras.lang...` in `config.lazy.lua`)
-- to validate from cmd directly, can be used: `cd ~/.config/nvim && nvim --headless "+lua require('utils.lang.registry-check').run({ raise = true })" +qa`
---@type lang.RegistryEntry[]
local entries = {
    {
        name = "java",
        primary = true,
        project = {
            markers = {
                "pom.xml",
                "build.gradle",
                "build.gradle.kts",
                "settings.gradle",
                "settings.gradle.kts",
                "gradlew",
                "mvnw",
                "build.xml",
            },
            exts = { java = true },
        },
        runner = {
            module = "plugins.overseer.tasks.lang.java-runner",
            filetypes = { "java" },
        },
        report = {
            module = "modules.java.test-report",
            component = "test_report.junit_report",
            filetypes = { "java" },
            trouble_source = "junit_diagnostics",
            diagnostic_source = "junit",
        },
    },
    {
        name = "rust",
        primary = true,
        project = {
            markers = { "Cargo.toml" },
            exts = { rs = true },
        },
        runner = {
            module = "plugins.overseer.tasks.lang.rust-runner",
            filetypes = { "rust" },
        },
        report = {
            module = "modules.rust.test-report",
            component = "test_report.cargo_report",
            filetypes = { "rust" },
            trouble_source = "cargo_test_diagnostics",
            diagnostic_source = "cargo-test",
        },
    },
    {
        name = "python",
        runner = {
            module = "plugins.overseer.tasks.lang.python-runner",
            filetypes = { "python" },
        },
        report = {
            module = "modules.python.test-report",
            component = "test_report.pytest_report",
            filetypes = { "python" },
            trouble_source = "pytest_test_diagnostics",
            diagnostic_source = "pytest",
        },
    },
    {
        name = "bash",
        runner = {
            module = "plugins.overseer.tasks.lang.sh-runner",
            filetypes = { "sh" },
        },
        report = {
            module = "modules.bash.test-report",
            component = "test_report.bashunit_report",
            filetypes = { "sh", "bash" },
            trouble_source = "bashunit_test_diagnostics",
            diagnostic_source = "bashunit",
        },
    },
    {
        name = "lua",
        runner = {
            module = "plugins.overseer.tasks.lang.lua-runner",
            filetypes = { "lua" },
        },
        report = {
            module = "modules.lua.test-report",
            component = "test_report.busted_report",
            filetypes = { "lua" },
            trouble_source = "busted_test_diagnostics",
            diagnostic_source = "busted",
        },
    },
    {
        name = "c",
        runner = {
            module = "plugins.overseer.tasks.lang.clang-runner",
            filetypes = { "c" },
        },
    },
    --[[ {
        name = "cpp",
        runner = {
            module = "plugins.overseer.tasks.lang.cpp-runner",
            filetypes = { "cpp" },
        },
    }, ]]
    {
        name = "go",
        primary = true,
        project = {
            markers = {
                "go.mod",
            },
            exts = { go = true },
        },
        runner = {
            module = "plugins.overseer.tasks.lang.go-runner",
            filetypes = { "go" },
        },
        report = {
            module = "modules.go.test-report",
            component = "test_report.go_report",
            filetypes = { "go" },
            trouble_source = "go_test_diagnostics",
            diagnostic_source = "go-test",
        },
    },
    --[[ {
        name = "javascript",
        runner = {
            module = "plugins.overseer.tasks.lang.js-runner",
            filetypes = { "javascript", "typescript", "javascriptreact", "typescriptreact" },
        },
        report = {
            module = "modules.js.test-report",
            component = "test_report.jest_report",
            filetypes = { "javascript", "typescript", "javascriptreact", "typescriptreact" },
            trouble_source = "jest_test_diagnostics",
            diagnostic_source = "jest",
        },
    }, ]]
    --[[ {
        name = "cs",
        runner = {
            module = "plugins.overseer.tasks.lang.cs-runner",
            filetypes = { "cs" },
        },
        report = {
            module = "modules.cs.test-report",
            component = "test_report.cs_report",
            filetypes = { "cs" },
            trouble_source = "dotnet_test_diagnostics",
            diagnostic_source = "dotnet-test",
        },
    }, ]]
}

---@param index table<string, lang.RegistryEntry>
---@param entry lang.RegistryEntry
---@param filetypes string[]|nil
local function add_entry_filetypes(index, entry, filetypes)
    for _, ft in ipairs(filetypes or {}) do
        index[ft] = entry
    end
end

---@return table<string, lang.RegistryEntry>
local function build_filetype_index()
    local result = {}
    for _, entry in ipairs(entries) do
        if entry.runner then
            add_entry_filetypes(result, entry, entry.runner.filetypes)
        end
        if entry.report then
            add_entry_filetypes(result, entry, entry.report.filetypes)
        end
    end
    return result
end

---@param filetype string
---@param filetypes string[]|nil
---@return boolean
local function contains_filetype(filetype, filetypes)
    for _, ft in ipairs(filetypes or {}) do
        if ft == filetype then
            return true
        end
    end
    return false
end

---@return lang.RegistryEntry[]
function M.all()
    return entries
end

---@return lang.RegistryEntry[]
function M.primary()
    local result = {}
    for _, entry in ipairs(entries) do
        if entry.primary and entry.project then
            table.insert(result, entry)
        end
    end
    return result
end

---@return table<string, lang.RegistryEntry>
function M.by_filetype()
    if not entries_by_filetype then
        entries_by_filetype = build_filetype_index()
    end
    return entries_by_filetype
end

---@param filetype string|nil
---@return lang.RegistryEntry|nil
function M.for_filetype(filetype)
    if not filetype or filetype == "" then
        return nil
    end
    return M.by_filetype()[filetype]
end

---@param filetype string|nil
---@return lang.RegistryReport|nil
function M.report_for_filetype(filetype)
    if not filetype or filetype == "" then
        return nil
    end
    local entry = M.for_filetype(filetype)
    local report = entry and entry.report
    if report and contains_filetype(filetype, report.filetypes) then
        return report
    end
    return nil
end

return M
