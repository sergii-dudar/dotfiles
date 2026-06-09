# From Java to Language-Agnostic — Refactor Plan

Tracking doc for decoupling the Neovim config from a hard Java/JDTLS assumption so
additional "main" languages (Rust first, more later) get first-class support with
the same muscle-memory, in complete per-language isolation.

> **Status:** in progress — **Items 1–4 done** (2026-06-09). Items below are ordered
> by impact. Each maps to an existing in-repo pattern — reuse them, don't invent new
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
- `lua/utils/lang/` — per-language LSP layer: `lsp-common.lua` (generic
  `apply_lsp_action`), `java/lsp-java.lua`, `rust/lsp-rust.lua`
  (`code_action_auto_resolve_match_names`, import resolve), plus
  `lsp-land-handlers-resolver.lua` → loads `java/lsp-java-handlers.lua` for the
  active project language (Item 1).
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

## 🔴 High priority — currently runs in _every_ project (including Rust)

### 1. Decouple global LSP handlers ✅ DONE `[todo: la-lsp-handlers]`

**File:** `lua/plugins/editor/lsp.lua`

- L14-16 — `require("utils.java.java-arg-highlight")` and
  `require("utils.java.java-format-checker")` + `.setup()` at top level: loaded and
  active for **all** projects.
- L17-58 — `vim.lsp.handlers["textDocument/publishDiagnostics"]` override hardcodes
  `d.source == "Java"`, builds `java_diags`, guards `filetype ~= "java"`, and calls
  `java_arg_highlight.apply` / `java_format_checker.apply` on every publish.
- L60-100+ — `vim.lsp.buf_request_all` hover override hardcodes
  `is_jdtls = …name == "jdtls"` and `jdtls_util.convert_markdown_links_to_references`.

**Done** — implemented as a per-language handler module + a tiny resolver (no shared
hub: handler needs differ too much per language to unify):

- [x] `utils/lang/lsp-land-handlers-resolver.lua` — `setup()` resolves
      `lang-project.current()` and `require(mod).setup()` for that language if it has
      an entry in `handlers_by_lang` (`java` only today); skips otherwise.
- [x] `utils/lang/java/lsp-java-handlers.lua` — the full publishDiagnostics + hover
      (`buf_request_all`) block moved **verbatim** from `lsp.lua`, wrapped in
      `setup()`; owns its `utils.java.*` requires.
- [x] `lsp.lua` — Java block (93 lines) replaced by one
      `require("utils.lang.lsp-land-handlers-resolver").setup()`; kept `$/progress`,
      `diagnostic.config`, and the plugin spec. No `utils.java.*` at module scope.
- [x] Fixed 3 stale requires from the `lang/` restructure (`lsp-java.lua` →
      `utils.lang.lsp-common`; `java-config.lua` ×2 → `utils.lang.java.lsp-java`;
      `rust-config.lua` → `utils.lang.rust.lsp-rust`).
- **Verified:** stylua clean · luajit parses · headless: resolver skips a non-Java
  cwd, and `lsp-java-handlers.setup()` installs the override without error.
- **Note:** the hover override's generic empty-result filtering now applies only in
  Java projects (moved wholesale, by decision); revisit if Rust later needs it.

### 2. Per-language code-action resolving ✅ DONE (by design) `[todo: la-codeaction-migration]`

**Files:** `lua/utils/lsp-util.lua` (generic `M.code_action`),
`lua/utils/lang/java/lsp-java.lua`, `lua/utils/lang/rust/lsp-rust.lua`,
`lua/utils/lang/lsp-common.lua`;
callers `plugins/editor/java/java-config.lua`, `plugins/editor/rust/rust-config.lua`.

**Nothing to migrate — this is the intended per-language split** (my initial
"half-done" read was from stale scan notes; the code was since cleaned up):

- [x] `utils/lsp-util.lua` `M.code_action` is the **generic, language-agnostic
      mechanism** — `resolve_context(action_match_names)` / `toggle` / `apply` take
      match-name patterns as arguments (`lsp-util.lua:47` iterates the passed list;
      `lang-runner-resolver` is no longer referenced — no duplication).
- [x] Java _data_ + import-resolve flow live in `lang/java/lsp-java.lua`; Rust's
      match-names in `lang/rust/lsp-rust.lua`. Each caller fetches its language's
      `code_action_auto_resolve_match_names` and passes it in.
- [x] Rust intentionally has no import-resolve flow; it may stay that way or get an
      entirely different implementation later — per-language isolation, by design.

**Optional tidy-ups (not bugs, not required):**

- [x] `lang/java/lsp-java.lua:25` — `request_and_apply_first` declared without
      `local`, leaking a global `_G.request_and_apply_first` (used only at `:108`).
      Add `local` to contain it — hygiene only, it works as-is.
- [x] `lang/java/lsp-java.lua:82` — leftover debug `vim.notify("matched with" ..
name_pattern)` fires on every matched context action; drop if unwanted.

---

## 🟠 Medium priority

### 3. Gate global Java keymaps  ✅ DONE  `[todo: la-global-keymaps]`

- [x] `lua/plugins/snacks/init.lua` — the `<leader>b,` / `<leader>b/` buffer-root
      pickers that unconditionally called `java-common.get_buffer_project_path()`
      (→ `nil` error in non-Java buffers) were **commented out** (L135-136). No longer
      active, so no per-language gating needed for now.
- [x] **Decision:** `lua/config/keymaps.lua` L162-172 `<leader>Cjj` / `<leader>Cjy`
      (Java `toString` → JSON, via `java-tostring-parser`) **stay global in all project
      types for now** — intentionally *not* gated. `<leader>Cjn` (json normalize) is
      language-agnostic and also stays.

### 4. Generalize shared utils  ✅ DONE  `[todo: la-shared-utils]`

- [x] `lua/utils/dap-util.lua` (`pick_and_write`) — now roots the picker via
      `resource-cwd-resolver.resolve()` (per-filetype resource dir, cwd fallback);
      dropped the `java-common` require + Maven `src/test/resources` hardcode. Java
      behaviour preserved; non-Java falls back to cwd.
- [x] `lua/utils/nvim/winbar-util.lua` — refactored into a per-filetype shaper
      registry: `M.register(ft, shaper)`, a generic `default_shaper` (cwd-relative
      with ❯), and a registered `java_shaper` (dotted FQN). `M.eval` picks by
      filetype or default. `split_str_by_src` is gone (now `local` helpers); Java
      output is byte-identical to before. Add Rust/others later via `M.register`.
- [x] `lua/utils/buffer-util.lua` — `work_buffer_types` now includes `rust`
      (`get_active_ls_buffers` has only commented callers today; future-proofing).
      (Couldn't derive from `lang-project` — it stores exts like `rs`, not the
      `rust` filetype.)

### 5. Decouple completion from jdtls `[todo: la-completion]`

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

### 6. Gate Java commands + tools `[todo: la-commands-mason]`

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

## Stray globals / hygiene nits found during the scan (not functional bugs)

- [ ] `lua/utils/lang/java/lsp-java.lua:25` — `request_and_apply_first` declared
      without `local`, leaking a global `_G.request_and_apply_first` (used only at
      `:108`). Works fine; add `local` to contain it. (Also listed under Item 2.)
- [x] `lua/utils/nvim/winbar-util.lua` — `split_str_by_src` global → **fixed** in
      Item 4 (refactored into `local` shaper helpers).

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