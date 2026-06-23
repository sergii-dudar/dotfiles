# Static Import Explorer

Search for Java static members and add `import static` to the current buffer.
Solves jdtls's inability to resolve static members by name without knowing the container class.

## Keymaps

| Key | Description |
|-----|-------------|
| `<leader>ji` | **Quick find** — rg search in project `src/` + preferred deps, auto-import single match, fallback to full picker |
| `<leader>jI` | **Full picker** — Snacks grep picker with live search across project sources and optional dependency scopes |

## Quick Find (`<leader>ji`)

1. Grabs word under cursor, detects type (ALL_CAPS = field, camelCase = method)
2. Loads dependency sources if not loaded (for preferred deps)
3. Runs `rg` on current module's `src/` + preferred dep dirs with starts-with pattern
4. Single result → auto-imports (when `auto_apply_single = true`)
5. Multiple results → `vim.ui.select` with `Class.member (package)` display
6. No results or cancelled → falls back to full picker with all deps loaded

## Full Picker (`<leader>jI`)

Snacks grep picker pre-filled with search pattern based on word under cursor.
Shows results as `Class.member (package)` with syntax highlighting.

By default, the picker searches the current module sources plus preferred dependency dirs when dependency sources are already loaded. Use the picker keys below to widen or narrow the search without leaving the picker.

### Picker Keys

| Key | Action |
|-----|--------|
| `<C-w>` | Toggle full match / starts-with mode (reopens picker) |
| `<C-d>` | Toggle filtered dependency sources |
| `<C-a>` | Toggle ALL dependency sources |
| `<C-g>` | Set glob by class name (e.g. `Util` → `*Util*.java`) |
| `<C-x>` | Clear glob (reset to `*.java`) |

## Scope-aware Search

- **Main file** (`src/main/`) → uses `src/main/java` plus main-scope preferred/filtered/all deps when enabled
- **Test file** (`src/test/`) → uses `src/main/java`, `src/test/java`, and test-scope preferred/filtered/all deps when enabled

## Dependency Search Modes

The module has three dependency scopes:

1. **Preferred deps** are the default focused dependency set. They are configured with `preferred_deps_main` and `preferred_deps_test`, and are used by quick find for speed.
2. **Filtered deps** are toggled in the picker with `<C-d>`. They come from `dependencies-search.get_source_dirs(scope)`, so they follow that module's `include_dependencies` / `ignored_dependencies` filtering.
3. **All deps** are toggled in the picker with `<C-a>`. They come from `dependencies-search.get_source_dirs_all(scope)` and search every loaded dependency source dir.

Preferred deps keep the fast path small and stable. Filtered deps are a broader but still curated search. All deps are for discovery or missed results; they can be slower and noisier on large projects because ripgrep scans many more source directories.

Dependency coordinates work for both Maven and Gradle caches. Use `groupId`, `groupId:artifactId`, `!groupId` / `!groupId:artifactId` exclusions, and `coord#sub/path;other/path` subpath restrictions. Maven cache paths and Gradle cache paths are normalized before matching.

## Settings

```lua
local settings = {
    import_mode = "explicit",      -- "explicit": import static pkg.Class.member;
                                   -- "wildcard": import static pkg.Class.*;
    auto_apply_single = true,      -- skip select when only one match
    fallback_to_find = true,       -- open full picker (with all deps) when quick has no results
    include_generated_sources = false, -- include target/build generated Java sources

    -- Preferred deps included alongside src/ when dependency sources are loaded.
    -- Quick find loads dependency sources before searching preferred deps.
    preferred_deps_main = {
        "org.apache.commons",                  -- all artifacts in group
        "!org.apache.commons.commons-compress", -- exclude specific artifact
        "com.google.guava.guava",              -- specific artifact
    },
    -- Test scope gets these + preferred_deps_main (auto-merged at load time)
    preferred_deps_test = {
        "org.assertj",
        "org.mockito.mockito-core",
    },
}
```

### Preferred deps entry formats

| Format | Example | Description |
|--------|---------|-------------|
| `groupId` | `org.apache.commons` | All artifacts in group |
| `groupId:artifactId` | `com.google.guava:guava` | Specific artifact |
| `!groupId` / `!groupId:artifactId` | `!org.apache.commons:commons-compress` | Exclude from results |
| `coord#sub;sub` | `org.assertj:assertj-core#org/assertj/core/util;org/assertj/core/api` | Only specific subdirs within dep |

`preferred_deps_test` is extended with `preferred_deps_main` when the module is loaded. If you edit these lists during a running Neovim session, reload the module or restart Neovim. Run `:DepSearchReset` when dependency sources were already loaded, because preferred dep directories are cached.

## Public API

| Method | Description |
|--------|-------------|
| `M.quick_import()` | Fast static-import resolver for the word under cursor. Searches module sources + preferred deps, auto-applies one match, shows `vim.ui.select` for multiple matches, and falls back to the picker on miss/cancel. |
| `M.find()` | Opens the interactive Snacks picker for the word under cursor. Supports dependency-scope toggles, class-name glob filtering, and full-match vs starts-with matching. |

### Search patterns

- **Fields** (ALL_CAPS): `\w+[\s]+WORD[\s]*=` — matches any type before constant (covers interface fields without `static`)
- **Methods** (camelCase): `static[\s]+.*[\s]+word\(` — requires `static`, not `public` (covers interface static methods)

## Commands

| Command | Description |
|---------|-------------|
| `:DepSearchReset` | Clear dep cache + preferred dep cache, reload on next use |

## File Structure

- `init.lua` — settings, state, public API (`find`, `quick_import`)
- `picker.lua` — Snacks grep picker: actions, keys, format, confirm
- `util.lua` — shared utilities: search pattern builder, member extractor, import inserter, rg parser, preferred dep resolver

## Dependencies

- `utils.java.java-common` — `get_buffer_project_path()`, `file_to_fqcn()`, `is_test_file()`
- `modules.java.dependencies-search` — dependency source dirs (main/test scoped), loading
- `Snacks.picker` — grep picker (full find)
- `rg` (ripgrep) — quick find search
