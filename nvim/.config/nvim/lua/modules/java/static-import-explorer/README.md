# Static Import Explorer

Search for Java static members and add `import static` to the current buffer.
Solves jdtls's inability to resolve static members by name without knowing the container class.

## Keymaps

| Key | Description |
|-----|-------------|
| `<leader>ji` | **Quick find** — rg search in project `src/` + preferred deps, auto-import single match, fallback to full picker |
| `<leader>jI` | **Full picker** — Snacks grep picker with live search across project + dependencies |

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

### Picker Keys

| Key | Action |
|-----|--------|
| `<C-w>` | Toggle full match / starts-with mode (reopens picker) |
| `<C-d>` | Toggle filtered dependency sources |
| `<C-a>` | Toggle ALL dependency sources |
| `<C-g>` | Set glob by class name (e.g. `Util` → `*Util*.java`) |
| `<C-x>` | Clear glob (reset to `*.java`) |

## Scope-aware Search

- **Main file** (`src/main/`) → searches `src/main/java` + main-scope deps only
- **Test file** (`src/test/`) → searches `src/main/java` + `src/test/java` + test-scope deps (main + test)

## Settings

```lua
local settings = {
    import_mode = "explicit",      -- "explicit": import static pkg.Class.member;
                                   -- "wildcard": import static pkg.Class.*;
    auto_apply_single = true,      -- skip select when only one match
    fallback_to_find = true,       -- open full picker (with all deps) when quick has no results

    -- Preferred deps always included in search alongside src/
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
| `!groupId` | `!org.apache.commons.commons-compress` | Exclude from results |
| `coord#sub;sub` | `org.assertj:assertj-core#org/assertj/core/util;org/assertj/core/api` | Only specific subdirs within dep |

### Search patterns

- **Fields** (ALL_CAPS): `\w+[\s]+WORD[\s]*=` — matches any type before constant (covers interface fields without `static`)
- **Methods** (camelCase): `static[\s]+.*[\s]+word\(` — requires `static`, not `public` (covers interface static methods)

## Commands

| Command | Description |
|---------|-------------|
| `:DepSearchReset` | Clear dep cache + preferred dep cache, reload on next use |

## File Structure

- `init.lua` — settings, state, public API (`find`, `find_quick`)
- `picker.lua` — Snacks grep picker: actions, keys, format, confirm
- `util.lua` — shared utilities: search pattern builder, member extractor, import inserter, rg parser, preferred dep resolver

## Dependencies

- `utils.java.java-common` — `get_buffer_project_path()`, `file_to_fqcn()`, `is_test_file()`
- `modules.java.dependencies-search` — dependency source dirs (main/test scoped), loading
- `Snacks.picker` — grep picker (full find)
- `rg` (ripgrep) — quick find search
