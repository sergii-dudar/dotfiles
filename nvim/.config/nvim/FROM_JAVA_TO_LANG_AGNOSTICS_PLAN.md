# From Java to Language-Agnostic — Refactor Plan

Tracking doc for decoupling the Neovim config from a hard Java/JDTLS assumption so
additional "main" languages (Rust first, more later) get first-class support with
the same muscle-memory, in complete per-language isolation.

> **Status:** scan complete, implementation pending. Items below are ordered by
> impact. Each maps to an existing in-repo pattern — reuse them, don't invent new
> ones.
>
> **Editing rule for this refactor:** do **not** delete unused methods, comments,
> or commented-out code. Migrate Java-specific logic into per-language modules;
> leave dead-but-intentional code in place (mirrors the `archive/` convention).

---

## Goal

A Java project never loads Rust config (and vice-versa); shared/core code contains
**no** Java/JDTLS assumptions. Language specifics live behind registries keyed by
filetype / project type / LSP client name.

---

## Already in place (the target architecture — reuse these)

- `lua/utils/lang/lang-project.lua` — language registry (root markers + source
  exts), `M.current()` / `M.is(lang)`, session-cached, honors the LIMITED gate.
  Replaces `java-common.is_java_project()`. Java + Rust registered.
- `lua/config/lazy.lua` L100-101 — per-language editor specs imported only for the
  active project type: `{ import = "plugins.editor.java", cond = …is("java") }`,
  same for `rust`.
- `lua/utils/lang/lsp/` — per-language LSP layer: `lsp-common.lua` (generic
  `apply_lsp_action`), `lsp-java.lua`, `lsp-rust.lua`
  (`code_action_auto_resolve_match_names`, import resolve).
- `lua/utils/resource-cwd-resolver.lua` — `register(ft, resolver)` pattern for
  picker cwd/resources (Java registered).
- `lua/plugins/overseer/tasks/lang-runner-resolver.lua` — filetype → runner.
- `lua/plugins/overseer/test-report-dispatcher.lua` + `run_tests.lua` — filetype →
  test-report module / Trouble view / diagnostic source.
- `lua/utils/constants.lua` — per-language constant tables (`M.java`, `M.rust`, …).

**The pattern to apply everywhere below:** replace a hardcoded `"java"` / `jdtls`
branch with a lookup in one of these registries, and register the Java behaviour as
just one entry.

---

## 🔴 High priority — currently runs in *every* project (including Rust)

### 1. Decouple global LSP handlers  `[todo: la-lsp-handlers]`

**File:** `lua/plugins/editor/lsp.lua`

- L14-16 — `require("utils.java.java-arg-highlight")` and
  `require("utils.java.java-format-checker")` + `.setup()` at top level: loaded and
  active for **all** projects.
- L17-58 — `vim.lsp.handlers["textDocument/publishDiagnostics"]` override hardcodes
  `d.source == "Java"`, builds `java_diags`, guards `filetype ~= "java"`, and calls
  `java_arg_highlight.apply` / `java_format_checker.apply` on every publish.
- L60-100+ — `vim.lsp.buf_request_all` hover override hardcodes
  `is_jdtls = …name == "jdtls"` and `jdtls_util.convert_markdown_links_to_references`.

**Plan**

- [ ] Add a per-client/filetype handler registry (e.g.
      `utils/lang/lsp/lsp-handlers.lua`) with
      `register(client_name_or_ft, { on_publish_diagnostics?, on_hover? })`.
- [ ] Move the Java diagnostic post-processing (arg-highlight, format-checker,
      `source == "Java"` filtering) into `lsp-java.lua`, registered only when
      `lang-project.is("java")`.
- [ ] Move the jdtls hover link conversion into the same Java registration.
- [ ] Keep `lsp.lua` generic: install one dispatcher that fans out to registered
      hooks; no `require("utils.java.*")` at module scope.
- **Acceptance:** opening a Rust-only project never `require`s any `utils.java.*`;
      `:lua print(package.loaded["utils.java.jdtls-util"])` is `nil`.

### 2. Finish the code-action migration  `[todo: la-codeaction-migration]`

**Files:** `lua/utils/lsp-util.lua` (old `M.code_action`),
`lua/utils/lang/lsp/lsp-java.lua`, `lsp-rust.lua`, `lsp-common.lua`;
callers `plugins/editor/java/java-config.lua`, `plugins/editor/rust/rust-config.lua`.

Migration is half-done:
- Import flow → `lsp-java.lua` (`resolve_imports`, `resolve_first`).
- `resolve_context` / `toggle` / `apply` still in old `utils/lsp-util.lua`.
- `resolve_context(action_names)` **ignores its argument** and re-reads
  `lang-runner-resolver.resolve().code_action_auto_resolve_match_names` — so the
  match-name list is **duplicated** in both the overseer runner and `lsp-<lang>`.
- `lsp-java.lua` `request_and_apply_first` is declared as a **global** (missing
  `local`) — see Bugs.

**Plan**

- [ ] Move generic `request_and_apply_first` / `resolve_context` / `toggle` /
      `apply` into `lsp-common.lua` (they only need match-name patterns + a fallback).
- [ ] Make the passed `action_names` the single source of truth; delete the
      internal `lang-runner-resolver` lookup from `resolve_context` (keep the
      runner's copy only if the overseer flow still needs it — otherwise
      consolidate to `lsp-<lang>.code_action_auto_resolve_match_names`).
- [ ] Keep only per-language data + fallbacks in `lsp-<lang>.lua` (Java fallback:
      `static-import-explorer.quick_import`).
- [ ] Repoint callers; leave the commented old keymaps in place (preserve-code).
- **Acceptance:** `<leader>cc` (resolve_context) honors the language's own
      match-name list; no `utils/lsp-util.code_action` references remain in active
      keymaps (commented ones stay).

---

## 🟠 Medium priority

### 3. Gate global Java keymaps  `[todo: la-global-keymaps]`

- [ ] `lua/config/keymaps.lua` L162-172 — `<leader>Cjj` / `<leader>Cjy`
      (`java-tostring-parser`) are set unconditionally. Move into
      `plugins/editor/java/` (loaded only for Java projects). `<leader>Cjn` (json
      normalize) is language-agnostic — leave it.
- [ ] `lua/plugins/snacks/init.lua` L137-146 — `<leader>b,` / `<leader>b/` always
      call `java-common.get_buffer_project_path()`; in a non-Java buffer this
      returns `nil` → `fnamemodify(nil, …)` error. Replace with a lang-aware
      buffer-project-root resolver (extend `lang-project` or `resource-cwd-resolver`).

### 4. Generalize shared utils  `[todo: la-shared-utils]`

- [ ] `lua/utils/dap-util.lua` L182-188 (`pick_and_write`) — hardcodes
      `java-common` + Maven `src/test/resources`. Route through
      `resource-cwd-resolver` or a per-language "test resource dir" hook.
- [ ] `lua/utils/nvim/winbar-util.lua` L16-45 (`split_str_by_src`) — hardcodes
      `…/src/<root>/java/…` parsing and `.java` extension special-casing; falls back
      OK for other langs but the Java path shape is baked in. Generalize per
      language (registry of path-shapers) or treat Java as one registered shaper.
      (Also a stray global — see Bugs.)
- [ ] `lua/utils/buffer-util.lua` L13-16 — `work_buffer_types = { java, lua }`
      lacks `rust` and others. Derive from the `lang-project` registry exts.

### 5. Decouple completion from jdtls  `[todo: la-completion]`

- [ ] `lua/plugins/editor/blink-cmp.lua` L7 — `require("utils.java.jdtls-util")` at
      module scope (loads in every project).
- [ ] L153-165 — documentation `draw` converts links only when
      `opts.item.client_name == "jdtls"` (behaviour is correctly guarded, but the
      module is always loaded). Move the jdtls-specific `draw` branch behind a
      per-client documentation-formatter registry; lazy-require `jdtls-util` only
      inside the jdtls branch.
- [ ] L244 — MapStruct jar path is Java-specific; already inside the MapStruct
      source — fine, just confirm it only registers for Java projects.

---

## 🟡 Low priority / polish

### 6. Gate Java commands + tools  `[todo: la-commands-mason]`

- [ ] `lua/config/autocmds.lua` L288-299 — `:RunMainClass` user command requires
      `jdtls.dap` globally. Move into the Java editor config.
- [ ] `lua/plugins/editor/mason.lua` — installs `jdtls`, `vscode-spring-boot-tools`,
      `vscode-java-dependency`, `vscode-java-decompiler`, `gradle-language-server`,
      `google-java-format`, `checkstyle`, `ktlint` unconditionally (also Rust tools
      unconditionally). Decide: keep "install everything" or gate per-language via
      `lang-project`. (Not breaking — preference call.)
- [ ] `lua/plugins/editor/nvim-treesitter.lua` L41/L48 — Java-only special-cases
      (`indent disable = { "java" }`, context `on_attach` off for java). Generalize
      to a set if Rust needs similar; otherwise leave as deliberate Java tweaks.
- [ ] `lua/utils/cache-util.lua` (`M.java`) / `lua/utils/list-util.lua` L76 (logger
      named `java-refactor` in a general util) — cosmetic; nest/rename if desired.

### 7. Picker excludes (optional)

- [ ] `lua/plugins/snacks/configs/pickers.lua` L60-64 — excludes `target/maven-*`,
      `target/junit-report`, `build/junit-report` (Java build dirs). Add Rust
      `target/nextest` etc., or derive excludes per language. L216 is a personal
      work path — leave.

---

## Bugs found during the scan (incidental — fix opportunistically)

- [ ] `lua/utils/lang/lsp/lsp-java.lua` — `function request_and_apply_first(...)` is
      **global** (missing `local`); pollutes `_G`. Make it local (or move to
      `lsp-common`).
- [ ] `lua/utils/nvim/winbar-util.lua` — `function split_str_by_src(...)` is
      **global**; should be `local`.

---

## Explicitly NOT coupling — do not touch (correctly isolated)

These are Java by design and already gated/namespaced; leave as-is:

- `lua/utils/java/**`, `lua/modules/java/**`, `lua/plugins/editor/java/**`
- `after/ftplugin/java.lua`, `queries/java/**`
- `lua/plugins/luasnip/snippets/java/**` (per-language snippets)
- `lua/modules/blink/mapstruct-source/**`
- Registry-keyed dispatchers that already include Java as one entry:
  `lang-runner-resolver.lua`, `test-report-dispatcher.lua`, `run_tests.lua`,
  `overseer/component/test_report/*` (java = `junit_report`, rust = `cargo_report`, …)
- `**/archive/**`

---

## Verification (run after each item)

- [ ] `stylua nvim/.config/nvim/` (or the Mason stylua) — clean.
- [ ] Headless load sanity: `nvim --headless +qa` with no errors.
- [ ] Rust-only project: confirm no `utils.java.*` modules load
      (`:lua = vim.tbl_keys(package.loaded)` filtered for `java`), and `<leader>b,`,
      hover, diagnostics, code actions all work.
- [ ] Java project: confirm jdtls arg-highlight, format-checker, hover links,
      import/code-action flows still work (no regression).

---

## Session todo IDs

`la-lsp-handlers`, `la-codeaction-migration`, `la-global-keymaps`,
`la-shared-utils`, `la-completion`, `la-commands-mason`.
