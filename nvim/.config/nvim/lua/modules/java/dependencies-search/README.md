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
| `<C-a>` | Toggle all/filtered source dirs, then rerun the current search |
| `<C-s>` | Scope to single module |

## Explorer Keys

| Key | Action |
|-----|--------|
| `<C-o>` | Toggle jdtls/file opener |
| `<C-s>` | Change module |

## How It Works

1. Gets classpath from running JDTLS instance (`scope = "test"`)
2. For each `.jar` in classpath, resolves `*-sources.jar` / `*-sources/` using Maven or Gradle cache layout
3. Extracts unzipped sources async if needed
4. Stores filtered (`source_dirs`) and unfiltered (`source_dirs_all`) lists
5. Current module `src/` is always included in search dirs

## Dependency Filtering

The files/grep pickers have two search modes:

1. **Filtered mode** is the default. It searches the current module `src/` plus the dependency dirs allowed by `include_dependencies` / `ignored_dependencies`. This is faster and gives less noisy results because Snacks/ripgrep scans fewer library directories.
2. **All mode** is toggled with `<C-a>` inside the files/grep picker. It searches the current module `src/` plus every loaded dependency source dir. This is slower on large projects, but useful when you are exploring unknown code, checking whether a filtered library hides a result, or debugging missed matches.

Control the default filtered library set in `init.lua`:

1. **`include_dependencies`** is whitelist mode. When this list is non-empty, only matching jars are searched by default. Use `"groupId"` to include all artifacts in a group, or `"groupId:artifactId"` for one artifact.
2. **`ignored_dependencies`** is blacklist mode. It is used only when `include_dependencies` is empty. Matching jars are excluded from the default search, while the rest are included.
3. **`ignored_extensions` / `ignored_file_names` / `ignored_packages`** are file-level picker excludes applied after the library set is chosen.

The same `groupId` / `groupId:artifactId` strings work for both Maven and Gradle dependencies. Maven paths already use slash-separated group directories, while Gradle paths store the group as a dotted segment; the module normalizes both before matching.

Examples:

```lua
local include_dependencies = {
    "ua.raiffeisen.payments",
    "org.assertj:assertj-core",
}

local ignored_dependencies = {
    "org.springframework",
    "com.google",
}
```

To search one more library by default, add its group or `groupId:artifactId` to `include_dependencies`.
To search most libraries by default, make `include_dependencies = {}` and keep only noisy groups in `ignored_dependencies`.
To search absolutely everything by default, make both lists empty, but expect slower picker searches and more irrelevant matches.

After changing these constants in a running Neovim session, restart Neovim or reload this module. If dependency sources were already loaded, run `:DepSearchReset` before opening the picker again.

## Opening Files

- **jdtls mode** (default): `.java` files open via `jdt_open_class(fqcn)`; non-java files open via `jdt://jarentry/` when Maven metadata can be built, otherwise they fall back to a raw file open
- **file mode**: raw file open (toggle with `<C-o>`)
- **project files** (from `src/`, not the Maven/Gradle dependency cache): always open as regular files

## Public API (for reuse)

| Method | Description |
|--------|-------------|
| `M.load_sources(opts?)` | Loads main/test dependency source directories from JDTLS classpaths and extracts missing sources jars. `opts` supports `{ on_done?, bufnr? }`. |
| `M.find_files()` | Opens a Snacks file picker over the current dependency source scope. |
| `M.grep()` | Opens a Snacks grep picker over the current dependency source scope. |
| `M.explore()` | Prompts for a dependency module and opens a tree explorer rooted at its extracted sources. |
| `M.reset()` | Clears loaded source dirs, module/exclude state, selected explorer module, and dependent preferred-dependency cache. |
| `M.is_loaded()` | Returns whether dependency source directories are already loaded. |
| `M.get_source_dirs(scope?)` | Returns filtered source directories for `main` or `test` scope; defaults to `test`. |
| `M.get_source_dirs_all(scope?)` | Returns unfiltered source directories for `main` or `test` scope; defaults to `test`. |
| `M.get_exclude()` | Returns Snacks picker exclude globs for ignored extensions, file names, and packages. |
| `M.coord_match_path(path)` | Normalizes Maven/Gradle dependency paths into slash-form coordinate paths for matching filters. |

## File Structure

- `init.lua` — main module: loading, filtering, pickers, state
- `jarentry.lua` — builds and opens `jdt://jarentry/` URIs for non-java resources

## Dependencies

- `utils.java.jdtls-classpath-util` — classpath from JDTLS
- `utils.java.java-common` — `file_to_fqcn()`
- `utils.java.jdtls-util` — `jdt_open_class()`
- `utils.ui.spinner` — loading spinner
- `Snacks.picker` — files, grep, explorer pickers
