# Registering a New Primary Language

This Neovim config is **language-agnostic by construction**: a Java project never
loads Rust tooling and vice-versa, and adding a new "primary" (main) language is a
matter of registering it with a handful of small registries — **no core code
changes**.

This doc covers:

- the difference between **primary** and **supported** languages,
- which languages are already registered,
- the registries (pluggable seams) that make it work,
- a complete, worked guide to promoting **Go** to a primary language (as example, it can be any you preffered language),
- an appendix for a brand-new language that has _no_ infrastructure yet.

---

## Primary vs supported languages

The real difference is **the level of customization**, not just a checklist of
registries. A _primary_ language is invested in until working with it in Neovim feels
like a full IDE — on par with a dedicated IDE such as IntelliJ IDEA for Java. (This
config grew out of exactly that move: leaving IntelliJ to do Java in Neovim without
giving up the IDE-grade workflow.) A _supported_ language gets a solid, productive
baseline, but not that bespoke, deep, IDE-level layer.

**Primary language** — a first-class "main" language you do daily work in, customised
to **IDE level**. It gets:

- **bespoke, IDE-grade tooling** maintained by hand — custom refactors, navigation,
  completion sources, project / dependency browsers, etc. (the part that takes the
  real investment),
- project-type **detection** + **isolation** (its tooling loads _only_ inside its own
  project type),
- a dedicated **editor config** under `lua/plugins/editor/<lang>/`,
- its **own LSP** wiring + code-action resolving under `lua/utils/lang/<lang>/`,
- the shared **`<leader>j…` keymap namespace** — safe to reuse per language because
  only one primary language's editor config is loaded at a time (`<leader>j…` I just get used to it, historically...),
- everything a supported language has (run / test / debug / report).

**Supported language** — has **run / test / debug / report** integration (overseer
runner + test-report adapter + DAP) and LSP via the default LazyVim/lspconfig
mechanism, but is **not** gated/isolated, and has no dedicated editor config or
`<leader>j` namespace.

| Capability                                            | Primary | Supported |
| ----------------------------------------------------- | :-----: | :-------: |
| `lang-project` detection registry                     |   ✅    |    ❌     |
| `lua/plugins/editor/<lang>/` config dir + load gating |   ✅    |    ❌     |
| `lua/utils/lang/<lang>/` LSP code-actions             |   ✅    |    ❌     |
| `<leader>j…` keymap namespace                         |   ✅    |    ❌     |
| overseer runner + test-report + DAP                   |   ✅    |    ✅     |
| LSP via mason + lspconfig                             |   ✅    |    ✅     |

> **What "IDE level" looks like in practice** — the Java primary language adds things a
> generic LSP setup doesn't: a custom MapStruct `@Mapping` completion source backed by
> an out-of-process Java server, a JUnit runner with per-test / parametrized execution,
> import & sibling-reference refactor fixers on file rename, a dependency-source
> browser, jdtls hover-link rewriting, argument-mismatch highlighting, and more. That
> depth — IntelliJ-grade Java in Neovim — is the whole point of a primary language, and
> the work you opt into when promoting one. A supported language (e.g. Go below) stops
> at a solid LSP + run / test / debug baseline.

### Currently registered

**Primary:**

- **Java** — jdtls, with deep Spring / MapStruct / JUnit / refactor tooling, custom
  LSP handlers (`utils/lang/java/lsp-java-handlers.lua`).
- **Rust** — rustaceanvim / rust-analyzer.

**Supported (run / test / debug / report):**

- **Go**, **Python**, **Bash**, **Lua**, **C#**, **JavaScript / TypeScript**, plus the
  trivial run-only `simple-runners` (C, C++, …).

> **Go is the worked example below** precisely because it already has the full
> _supported_ infrastructure (test runner, report adapter, DAP, dispatcher entries,
> constants) but is **not yet a primary language**. So the guide focuses on exactly
> the steps that turn a supported language into a primary one.

---

## The registries (the pluggable seams)

Every per-language behaviour hangs off one of these. Adding a language = adding one
small entry to each relevant registry. Nothing else in the config needs to change.

| Concern                       | File                                                                     | What you add                                  |
| ----------------------------- | ------------------------------------------------------------------------ | --------------------------------------------- |
| Project detection             | `lua/utils/lang/lang-project.lua`                                        | `registry` entry: markers + exts              |
| Editor-config load gating     | `lua/config/lazy.lua`                                                    | gated `{ import = … , cond = …is("<lang>") }` |
| Editor config (keymaps / LSP) | `lua/plugins/editor/<lang>/`                                             | new dir + `<lang>-config.lua`                 |
| LSP code-action data          | `lua/utils/lang/<lang>/lsp-<lang>.lua`                                   | new module                                    |
| LSP custom handlers _(opt)_   | `lua/utils/lang/lsp-land-handlers-resolver.lua`                          | `handlers_by_lang` entry                      |
| Overseer runner switch        | `lua/plugins/overseer/tasks/lang-runner-resolver.lua`                    | `type_to_resolver[<lang>]`                    |
| Runner contract               | `lua/plugins/overseer/tasks/lang/<lang>-runner.lua`                      | new module                                    |
| Test-report module            | `lua/modules/<lang>/test-report/`                                        | adapter (self-registers)                      |
| Test-report dispatch          | `lua/plugins/overseer/test-report-dispatcher.lua`                        | 3 filetype entries                            |
| Test-report component         | `lua/overseer/component/test_report/<lang>_report.lua` + `run_tests.lua` | component + mapping                           |
| Constants                     | `lua/utils/constants.lua`                                                | `M.<lang>` table                              |
| Tools                         | `lua/plugins/editor/mason.lua`                                           | LSP / DAP / linter / formatter                |
| Treesitter                    | `lua/plugins/editor/nvim-treesitter.lua`                                 | parsers                                       |
| Format / lint _(opt)_         | `conform.nvim.lua` / `linting.lua`                                       | `*_by_ft` entry                               |
| Picker excludes _(opt)_       | `lua/plugins/snacks/configs/pickers.lua`                                 | `exclude_by_lang` entry                       |
| Winbar path shaping _(opt)_   | `lua/utils/nvim/winbar-util.lua`                                         | `M.register(ft, …)`                           |
| Picker resource dirs _(opt)_  | `lua/utils/resource-cwd-resolver.lua`                                    | `M.register(ft, …)`                           |
| Buffer "work" types _(opt)_   | `lua/utils/buffer-util.lua`                                              | `work_buffer_types`                           |
| Snippets _(opt)_              | `lua/plugins/luasnip/snippets/<lang>/`                                   | new dir                                       |

---

## Worked example — promoting **Go** to a primary language

### 0. What Go already has (do NOT redo)

Go is already a _supported_ language; these exist and stay as-is:

- `lua/modules/go/go-test/init.lua` — `go test -json` runner (+ delve debug launcher)
- `lua/modules/go/test-report/` — adapter (`init.lua` shim, `lang/go.lua`, `json-parser.lua`)
- `lua/overseer/component/test_report/go_report.lua` — report component
- `lua/plugins/overseer/tasks/lang/go-runner.lua` — runner-contract glue
- `lua/plugins/overseer/test-report-dispatcher.lua` — already has the `go` entries
- `lua/plugins/overseer/tasks/run_tests.lua` — already maps `go = "test_report.go_report"`
- `lua/utils/constants.lua` — already has `M.go`

The runner just needs to be **switched on** (Step 2).

### 1. Register project detection — `lua/utils/lang/lang-project.lua`

Add Go to the `registry` table:

```lua
go = {
    markers = { "go.mod", "go.work" },
    exts = { go = true },
},
```

Now `require("utils.lang.lang-project").is("go")` returns `true` inside a Go module,
and the marker-less fallback recognises a folder of loose `*.go` files too.

### 2. Switch on the overseer runner — `lang-runner-resolver.lua`

Uncomment the existing (currently commented) line:

```lua
type_to_resolver["go"] = require("plugins.overseer.tasks.lang.go-runner")
```

That single line activates run / test / debug: the `<leader>t…` test keymaps, the
report dispatcher and DAP all flow through the contract `go-runner.lua` already
implements (`build_run_cmd`, `build_run_test_cmd`, `dap_launch`, `dap_launch_test`,
`get_test_report_dir`, …).

### 3. Per-language LSP code-action data — `utils/lang/go/lsp-go.lua`

Mirror `lua/utils/lang/rust/lsp-rust.lua`. Create `lua/utils/lang/go/lsp-go.lua`:

```lua
local M = {}

-- Code-action titles (Lua patterns) auto-resolved by <leader>cc, priority order.
M.code_action_auto_resolve_match_names = {
    "Organize Imports",
    "Extract function",
    "Extract variable",
    "Fill struct",
}

return M
```

(If Go ever needs the JDTLS-style import auto-resolve flow, add a `resolve_imports`
here too — see `lua/utils/lang/java/lsp-java.lua`. gopls usually doesn't need it.)

### 4. Editor config dir — `plugins/editor/go/go-config.lua`

Mirror `lua/plugins/editor/rust/rust-config.lua`. This wires the gopls server, its
keymaps, and the `<leader>j` Go group. Create `lua/plugins/editor/go/go-config.lua`:

```lua
return {
    {
        "folke/which-key.nvim",
        optional = true,
        opts = {
            spec = {
                { "<leader>j", group = "+go" },
                { "<leader>jc", group = "+go code/compile" },
            },
        },
    },
    {
        "neovim/nvim-lspconfig",
        opts = {
            servers = {
                gopls = {
                    settings = {
                        gopls = {
                            gofumpt = true,
                            staticcheck = true,
                            analyses = { unusedparams = true, shadow = true },
                        },
                    },
                    -- stylua: ignore
                    keys = {
                        { "<leader>jcc", function() vim.cmd("LspGoImports") end, desc = "Go organize imports" },
                        { "<leader>cc", function()
                            local action_names = require("utils.lang.go.lsp-go").code_action_auto_resolve_match_names
                            require("utils.lsp-util").code_action.resolve_context(action_names)
                        end, desc = "Context Apply First Code Action [gopls]" },
                    },
                },
            },
        },
    },
}
```

> The `<leader>j…` namespace is intentionally shared by every primary language —
> because only the _active_ project's editor config is imported (Step 5), Java's and
> Go's `<leader>j` maps never coexist. (The `<leader>jcc` map above is illustrative;
> wire whatever Go actions you want.)

### 5. Gate the editor config by project type — `config/lazy.lua`

Right next to the existing `java` / `rust` import lines, add:

```lua
{ import = "plugins.editor.go", cond = function() return require("utils.lang.lang-project").is("go") end, },
```

Now `plugins/editor/go/` loads **only** in a Go project — a Java or Rust project never
sees gopls keymaps, and vice-versa. This is the isolation guarantee.

### 6. Tools + parsers

`lua/plugins/editor/mason.lua` → `ensure_installed`, add:

```lua
-- Go
"gopls",
"delve",          -- DAP (dlv): required for <leader>td (debug test)
"gofumpt",        -- formatter
"goimports",
"golangci-lint",  -- linter
```

`lua/plugins/editor/nvim-treesitter.lua` → `ensure_installed`, add:

```lua
"go",
"gomod",
"gosum",
"gowork",
```

### 7. _(optional)_ Format / lint / pickers / UI polish

- **conform** (`conform.nvim.lua`): `opts.formatters_by_ft.go = { "goimports", "gofumpt" }`
- **lint** (`linting.lua`): register `golangci-lint` for the `go` filetype
- **picker excludes** (`pickers.lua`): add a `exclude_go = { "**/vendor", "**/bin" }`
  table and `go = exclude_go` in `exclude_by_lang`
- **winbar** (`winbar-util.lua`): `M.register("go", function(raw) … end)` only if you
  want custom path rendering — otherwise Go uses the generic shaper, which is fine
- **picker resources** (`resource-cwd-resolver.lua`): `M.register("go", …)` if Go has
  a resource-dir convention worth rooting `find/grep` at
- **buffer-util**: add `go = true` to `work_buffer_types`
- **snippets**: add `lua/plugins/luasnip/snippets/go/`
- **LSP handlers**: **skip.** Go (like Rust) needs no custom diagnostic/hover
  post-processing, so leave `lsp-land-handlers-resolver.lua` alone. Only Java
  registers handlers there. Add a `go = "utils.lang.go.lsp-go-handlers"` entry **only**
  if Go later needs jdtls-style handler overrides.

### 8. Verify

```vim
" inside a Go project (has go.mod):
:lua =require("utils.lang.lang-project").current()                                   " -> "go"
:lua =require("plugins.overseer.tasks.lang-runner-resolver").resolve("go") ~= nil    " -> true
```

Then, in a Go project: `<leader>j` shows the `+go` which-key group, `gd` / hover /
code-actions use gopls, the `<leader>t…` test keymaps run `go test`, `<leader>td`
debugs the current test via delve, and the test report renders in Trouble + the tree
view. Open a Rust or Java project afterwards and confirm **no** gopls keymaps appear —
that's the isolation working.

---

## Appendix — a brand-new language with no infrastructure

Go already had the run/test/report layer, so the guide above skipped it. A language
starting from zero also needs the following (use `lua/modules/go` or `lua/modules/rust`
as copy-paste templates):

1. **Runner contract** — `lua/plugins/overseer/tasks/lang/<lang>-runner.lua`
   implementing `task.lang.Runner`: `build_run_cmd`, `build_run_test_cmd`,
   `prepare_test_context?`, `dap_launch?`, `dap_launch_test?`, `get_test_report_dir?`.
2. **Test runner** — `lua/modules/<lang>/<framework>-test/init.lua`: builds the test
   command + DAP launch and writes a machine-readable report (JUnit XML / JSON).
3. **Report adapter** — `lua/modules/<lang>/test-report/`: `init.lua` registers the
   adapter with the shared core
   (`registry.register("<lang>", require("modules.<lang>.test-report.lang.<lang>"))`)
   and re-exports the core; `lang/<lang>.lua` parses results into diagnostics + signs.
4. **Report component** — `lua/overseer/component/test_report/<lang>_report.lua`.
5. **Dispatcher** — `test-report-dispatcher.lua`: add 3 filetype entries
   (`ft_to_module`, `ft_to_trouble_source`, `ft_to_diagnostic_source`).
6. **Component mapping** — `run_tests.lua`: add to `ft_to_report_component`.
7. **Constants** — `constants.lua`: add `M.<lang> = { <test_name>, <diagnostic_source>, <report_dir> }`.
8. **DAP adapter** — add to `mason.lua` if debugging is wanted.

Then follow Steps 1–8 above to make it a _primary_ language.

---

## Quick checklist — primary language

- [ ] `lang-project.lua` — markers + exts
- [ ] `lang-runner-resolver.lua` — `type_to_resolver[<lang>]`
- [ ] `utils/lang/<lang>/lsp-<lang>.lua` — code-action match names
- [ ] `plugins/editor/<lang>/<lang>-config.lua` — which-key `<leader>j` + LSP keys
- [ ] `config/lazy.lua` — gated `{ import = "plugins.editor.<lang>", cond = …is("<lang>") }`
- [ ] `mason.lua` — LSP + DAP + linter + formatter
- [ ] `nvim-treesitter.lua` — parsers
- [ ] run / test / report infra (already present for Go; see Appendix otherwise)
- [ ] _optional_: conform · lint · pickers `exclude_by_lang` · winbar `register` ·
      resource-cwd `register` · buffer-util · snippets · LSP handlers