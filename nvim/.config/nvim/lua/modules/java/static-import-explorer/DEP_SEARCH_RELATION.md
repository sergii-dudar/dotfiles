# Relation To Dependencies Search

`modules.java.static-import-explorer` reuses `modules.java.dependencies-search` as its dependency source directory provider. Static-import search still owns its own matching logic, result parsing, picker actions, and import insertion; `dependencies-search` only supplies dependency source roots and path-normalization helpers.

## What Is Reused

| From `dependencies-search` | Used by `static-import-explorer` | Purpose |
|----------------------------|----------------------------------|---------|
| `is_loaded()` | `init.lua`, `picker.lua`, `util.lua` | Checks whether dependency source dirs are already available. |
| `load_sources({ bufnr, on_done })` | `init.lua`, `picker.lua` | Lazily loads/extracts dependency sources before quick import or picker dependency toggles need them. |
| `get_source_dirs(scope)` | `util.get_search_dirs()` | Returns filtered dependency dirs for `<C-d>` picker mode. |
| `get_source_dirs_all(scope)` | `util.get_preferred_dep_dirs()`, `util.get_search_dirs()` | Returns all dependency dirs for preferred-dep resolution and `<C-a>` picker mode. |
| `coord_match_path(path)` | `util.filter_dirs_by_patterns()` and preferred-dep exclusions | Normalizes Maven and Gradle cache paths before coordinate matching. |
| `reset()` / `:DepSearchReset` side effect | Calls `static-import-explorer.util.clear_preferred_cache()` | Invalidates preferred dependency dir cache after dependency source state changes. |

## Search Modes

Static import search has three dependency scopes:

1. **Preferred deps**
   - Configured in `static-import-explorer/init.lua` as `preferred_deps_main` and `preferred_deps_test`.
   - Used by `quick_import()` and by the picker default search when dependency sources are loaded.
   - Resolved from `dependencies-search.get_source_dirs_all(scope)`, so preferred deps can include libraries outside the filtered dependency-search set.

2. **Filtered deps**
   - Enabled in the static-import picker with `<C-d>`.
   - Uses `dependencies-search.get_source_dirs(scope)`.
   - This is the only static-import mode affected by `dependencies-search` `include_dependencies` / `ignored_dependencies`.

3. **All deps**
   - Enabled in the static-import picker with `<C-a>`.
   - Uses `dependencies-search.get_source_dirs_all(scope)`.
   - Ignores `include_dependencies` / `ignored_dependencies` and searches every loaded dependency source dir.

## Impact Of `include_dependencies` / `ignored_dependencies`

`include_dependencies` and `ignored_dependencies` in `dependencies-search/init.lua` affect static-import search only through filtered dependency mode:

```text
dependencies-search include/ignored filters
        |
        v
dependencies-search.get_source_dirs(scope)
        |
        v
static-import picker <C-d> filtered deps mode
```

They do not affect:

- `quick_import()` default search.
- Preferred dependency resolution from `preferred_deps_main` / `preferred_deps_test`.
- Picker default search when it only uses module sources + preferred deps.
- Picker `<C-a>` all-deps mode.
- Quick-import fallback picker, which starts with `include_all_deps = true`.

## Coordinate Matching

Both modules use dependency coordinates such as:

- `groupId`
- `groupId:artifactId`
- `!groupId` or `!groupId:artifactId` for preferred-dep exclusions
- `groupId:artifactId#sub/path;other/path` for preferred-dep subpath restrictions

`static-import-explorer` converts these entries to slash-form path patterns and calls `dependencies-search.coord_match_path(path)` before matching. That makes the same coordinate entries work for both Maven cache paths and Gradle cache paths.

## Cache And Reset Behavior

`static-import-explorer` caches resolved preferred dependency dirs in `util.lua`:

```lua
preferred_cache = { main = nil, test = nil }
```

The cache is based on `dependencies-search.get_source_dirs_all(scope)`. When dependency sources are reset through `dependencies-search.reset()` or `:DepSearchReset`, `dependencies-search` also calls:

```lua
require("modules.java.static-import-explorer.util").clear_preferred_cache()
```

This keeps preferred dependency dirs consistent with the dependency source cache.

## Practical Rules

- Change `static-import-explorer` defaults with `preferred_deps_main` / `preferred_deps_test`.
- Change `<C-d>` filtered dependency behavior with `dependencies-search` `include_dependencies` / `ignored_dependencies`.
- Use `<C-a>` in the picker when you want to ignore filters and search every loaded dependency source dir.
- Run `:DepSearchReset` after changing dependency source filters or preferred dependency lists during a running Neovim session.
