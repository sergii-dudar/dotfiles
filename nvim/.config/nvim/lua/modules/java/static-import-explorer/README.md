# Static Import Explorer

Search for Java static members and add `import static` to the current buffer.
Solves jdtls's inability to resolve static members by name without knowing the container class.

## Keymaps

| Key | Description |
|-----|-------------|
| `<leader>ji` | **Quick find** — rg search in project `src/`, auto-import single match, fallback to full picker |
| `<leader>jI` | **Full picker** — Snacks grep picker with live search across project + dependencies |

## Quick Find (`<leader>ji`)

1. Grabs word under cursor, detects type (ALL_CAPS = field, camelCase = method)
2. Runs `rg` on current module's `src/` with starts-with pattern
3. Single result → auto-imports (when `auto_apply_single = true`)
4. Multiple results → `vim.ui.select` with `Class.member (package)` display
5. No results or cancelled → falls back to full picker with all deps loaded

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

## Settings

In `init.lua`:

```lua
local settings = {
    import_mode = "explicit",      -- "explicit": import static pkg.Class.member;
                                   -- "wildcard": import static pkg.Class.*;
    auto_apply_single = true,      -- skip select when only one match
    fallback_to_find = true,       -- open full picker (with all deps) when quick has no results
}
```

## File Structure

- `init.lua` — settings, state, public API (`find`, `find_quick`)
- `picker.lua` — Snacks grep picker: actions, keys, format, confirm
- `util.lua` — shared utilities: search pattern builder, member extractor, import inserter, rg parser

## Dependencies

- `utils.java.java-common` — `get_buffer_project_path()`, `file_to_fqcn()`
- `modules.java.dependencies-search` — dependency source dirs, loading
- `Snacks.picker` — grep picker (full find)
- `rg` (ripgrep) — quick find search
