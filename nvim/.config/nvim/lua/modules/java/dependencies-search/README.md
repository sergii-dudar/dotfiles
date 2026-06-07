# Dependencies Search

Search and browse dependency source code using Snacks picker.
Extracts `*-sources.jar` files and searches the extracted directories. Works with both
build tools: Maven (`~/.m2/repository/`, sources jar beside the binary) and Gradle
(`~/.gradle/caches/modules-2/files-2.1/`, sources jar in a sibling SHA1 hash dir). The jar
list itself comes from the JDTLS classpath, so the active build tool is followed automatically.

## Keymaps

Dependency sources are loaded lazily — the first time you trigger any of the
keymaps below (or the `<leader>ci` static-import fallback) they are resolved
from the JDTLS classpath and extracted. There is no separate "load" keymap.

| Key | Description |
|-----|-------------|
| `<leader>j.` | Find file in dependency sources |
| `<leader>j/` | Grep in dependency sources |
| `<leader>je` | Explore dependency module (tree view) |

## Picker Keys (files/grep)

| Key | Action |
|-----|--------|
| `<C-o>` | Toggle jdtls/file opener |
| `<C-a>` | Toggle all/filtered source dirs |
| `<C-s>` | Scope to single module |

## Explorer Keys

| Key | Action |
|-----|--------|
| `<C-o>` | Toggle jdtls/file opener |
| `<C-s>` | Change module |

## How It Works

1. Gets classpath from running JDTLS instance (`scope = "test"`)
2. For each `.jar` in classpath, looks for `*-sources.jar` / `*-sources/` in same directory
3. Extracts unzipped sources async if needed
4. Stores filtered (`source_dirs`) and unfiltered (`source_dirs_all`) lists
5. Current module `src/` is always included in search dirs

## Dependency Filtering

Three levels:

1. **`include_dependencies`** (whitelist) — when non-empty, ONLY matching jars included. Format: `"groupId"` or `"groupId:artifactId"`
2. **`ignored_dependencies`** (blacklist) — used when include list is empty
3. **`ignored_extensions` / `ignored_file_names` / `ignored_packages`** — file-level filtering via Snacks `exclude`

## Opening Files

- **jdtls mode** (default): `.java` files open via `jdt_open_class(fqcn)`; non-java files via `jdt://jarentry/` URI (Maven), falling back to a raw open for Gradle
- **file mode**: raw file open (toggle with `<C-o>`)
- **project files** (from `src/`, not the Maven/Gradle dependency cache): always open as regular files

## Public API (for reuse)

```lua
M.load_sources(opts?)  -- opts: { on_done?, bufnr? }
M.find_files()
M.grep()
M.explore()
M.is_loaded()
M.get_source_dirs()      -- filtered
M.get_source_dirs_all()  -- all
M.get_exclude()
```

## File Structure

- `init.lua` — main module: loading, filtering, pickers, state
- `jarentry.lua` — builds and opens `jdt://jarentry/` URIs for non-java resources

## Dependencies

- `utils.java.jdtls-classpath-util` — classpath from JDTLS
- `utils.java.java-common` — `file_to_fqcn()`
- `utils.java.jdtls-util` — `jdt_open_class()`
- `utils.ui.spinner` — loading spinner
- `Snacks.picker` — files, grep, explorer pickers
