# Java Refactor Module - Analysis & Bug Report

> Generated: 2026-03-23 | Status: Working but unstable | Files: 3 Lua modules (~1550 LOC total)

## Architecture Summary

```
init.lua (1214 lines) - Orchestration & command building
  |-- import-fixer.lua (356 lines) - Import management for moved files
  |-- sibling-usage-fixer.lua (137 lines) - Cross-reference fixes for batch moves
```

### How It Works

1. **Registration phase**: `fyler.nvim` calls `register_change(src, dst)` for each file/dir move
2. **Trigger**: When fyler buffer unloads (`BufUnload` autocmd), `process_registerd_changes()` fires
3. **Analysis phase**: Detects module path, classifies moves (file vs dir), identifies siblings, auto-mirrors test packages
4. **Command building**: Generates a mix of shell commands (sed/rg) and Lua functions
5. **Execution**: Shell commands run in a terminal buffer via `jobstart`; Lua ops run in a callback after shell completion
6. **Cleanup**: Reopens moved buffers in their original windows

### Integration Points

- `plugins/navigation/fyler-nvim.lua` -> `register_change()` in `on_rename` hook
- `config/autocmds.lua` -> `process_registerd_changes()` on fyler `BufUnload`
- Both guarded by `java_util.is_java_project()`

### Dependencies

External: `rg` (ripgrep), `fd`, `gsed`/`sed` (GNU sed)
Internal: `utils/common-util`, `utils/string-util`, `utils/buffer-util`, `utils/logging-util`, `utils/ui/spinner`, `utils/global-util`

---

## Bugs Found

### CRITICAL

#### BUG-1: Consecutive type references not replaced (sed overlapping match problem)

**File**: `init.lua:246`, `init.lua:265`
**Pattern**: `s/([[:space:],;(}<])OldType([[:space:],;(}\\.>])/\1NewType\2/g`

The sed `g` flag replaces all **non-overlapping** matches. The trailing captured char of match N is consumed and not available as the leading char of match N+1.

**Example**:
```java
Map<OldType, OldType> map;  // Only first OldType replaced
//         ^ comma consumed by first match's \2, not available as \1 for second match
```

**Result**: `Map<NewType, OldType> map;` - second occurrence missed.

**Fix options**:
- Run the sed command twice (double-pass)
- Use lookahead/lookbehind if supported by sed version
- Use a word-boundary approach: `\bOldType\b` (but sed ERE doesn't have `\b`; could use `perl -pe` instead)
- Use `rg --replace` instead of `sed` (ripgrep supports `\b`)

---

#### BUG-2: Same-package import removal deletes subpackage imports

**File**: `init.lua:359-360`
```lua
string.format("%s -i '/^import %s\\./d' %s", sed, package_declaration_dst_escaped, dst)
```

Pattern `/^import com\.example\.service\./d` matches ANY import starting with `com.example.service.`, including subpackages.

**Example**: File moved to `com.example.service`. This sed command would remove:
```java
import com.example.service.impl.SomeClass;  // INCORRECTLY DELETED
import com.example.service.util.Helper;      // INCORRECTLY DELETED
```

**Fix**: Match only direct package members (no more dots after the last segment):
```lua
string.format("%s -i '/^import %s\\.[A-Z][^.]*;$/d' %s", sed, package_declaration_dst_escaped, dst)
```
This ensures only `import com.example.service.ClassName;` is removed (uppercase after last dot, no more dots before `;`).

---

#### BUG-3: Annotations (`@OldType`) not matched by boundary pattern

**File**: `init.lua:246`, `init.lua:265`, `sibling-usage-fixer.lua:118`

The sed boundary character class `[[:space:],;(}<]` does not include `@`. Java annotations are a very common use of type names:

```java
@OldType           // NOT matched - @ not in leading boundary class
@OldType(value=1)  // NOT matched
```

The `@` char needs to be added to the leading boundary class: `[[:space:],;(}<@]`

---

#### BUG-4: Constructor not renamed for cross-package move+rename

**File**: `init.lua:208-219` (step 1 only handles class/interface/enum/record declarations)

When a file is both **moved to a different package** AND **renamed** simultaneously, the constructor name is not updated in the moved file itself. Step 1 handles type declaration keywords but not constructors. Step 2 (type symbol replacement) only processes files that **import** the old package - the moved file doesn't import itself.

**Example**: Moving `com.example.service.User.java` to `com.example.model.UserEntity.java`:
- `class User` -> `class UserEntity` (step 1 handles this)
- `public User(String name)` -> NOT updated (constructor missed)

**Note**: This only occurs when package AND name change simultaneously. Same-package renames are handled because step 2 includes all files in the same directory via `fd`.

**Fix**: Add explicit constructor rename step for the moved file:
```lua
-- After step 1, add constructor fix:
local fix_constructor_cmd = string.format(
    "%s -i -E 's/(public|protected|private)([[:space:]]+)%s([[:space:]]*(\\(|\\{))/\\1\\2%s\\3/g' %s",
    sed, old_type_name, new_type_name, dst
)
```

---

### MEDIUM

#### BUG-5: Type name at absolute line start not matched

**File**: `init.lua:246`, `init.lua:265`

The pattern requires a leading char from `[[:space:],;(}<]`. A type name at column 0 (no indentation) won't match. Rare in formatted Java but possible in generated code or minimal formatting.

**Fix**: Add `^` as an alternative: `(^|[[:space:],;(}<@])OldType...`

---

#### BUG-6: Module detection uses only first registered change

**File**: `init.lua:611-614`
```lua
local first_change_path = all_registered_changes[1].src
module_path = detect_module_path(first_change_path)
```

In a multi-module Maven/Gradle project, if files from different modules are batch-moved, only the first change's module is detected. All subsequent `rg` searches are scoped to the wrong module for other changes.

**Fix**: Detect module per-change, or group changes by module before processing:
```lua
-- Group changes by module
local changes_by_module = {}
for _, change in ipairs(all_registered_changes) do
    local mod = detect_module_path(change.src) or "project-root"
    changes_by_module[mod] = changes_by_module[mod] or {}
    table.insert(changes_by_module[mod], change)
end
```

---

#### BUG-7: Shell injection via file paths with special characters

**File**: Throughout all files

Paths are inserted into shell commands with single-quote wrapping (`'%s'`), but paths containing single quotes, backticks, or `$()` constructs would break or allow injection.

**Example**: A file named `it's-a-test.java` would break:
```bash
rg --color=never -l 'pattern' '/path/to/it's-a-test.java'
#                                              ^ breaks quoting
```

**Fix**: Escape single quotes in paths before interpolation:
```lua
local function shell_escape(s)
    return "'" .. s:gsub("'", "'\\''") .. "'"
end
```

---

#### BUG-8: Missing `--no-run-if-empty` on xargs (Linux issue)

**File**: `init.lua:263-266`, `init.lua:319-326`, and other `rg | xargs sed` pipelines

On Linux, GNU xargs runs the command even with empty stdin input. GNU sed with `-i` and no file arguments prints an error ("no input files") and exits non-zero. The `|| echo 'skipped'` catches this, but it's noisy and could mask real errors.

On macOS, BSD xargs doesn't run with empty input, so this only affects Linux.

**Fix**: Add `-r` (or `--no-run-if-empty`) to all xargs invocations:
```lua
"rg ... | xargs -r %s -i ..."
```

---

#### BUG-9: FQN replacement boundary pattern too narrow (step 3)

**File**: `init.lua:319-326`
```lua
's/%s([;.$"]|$)/%s\\1/g'
```

The boundary `([;.$"]|$)` doesn't handle:
- `)` - FQN in method parameter: `method(com.example.MyClass)`
- `>` - FQN in generics: `List<com.example.MyClass>`
- ` ` - FQN followed by space: `com.example.MyClass instance`
- `(` - FQN in annotation: `@com.example.MyClass(param)`

These are less common than import-style FQNs but do occur in Java.

**Fix**: Expand the boundary: `([;.$"[:space:]()><,]|$)`

---

### LOW / COSMETIC

#### BUG-10: Typo in type annotation `rejactor` -> `refactor`

**File**: `init.lua:29,32`
```lua
---@class java.rejactor.FileMove
---@field siblings? java.rejactor.FileMove[]
```

Should be `java.refactor.FileMove`. Appears in 5 places across the file in `@class` and `@param` annotations.

---

#### BUG-11: Missing nil checks for `io.popen`

**File**: `init.lua:637,916` and several places in import-fixer.lua

```lua
local handle = io.popen("fd --max-depth 1 . '" .. src_parent .. "' | wc -l")
local count = tonumber(handle:read("*all"))  -- crashes if handle is nil
```

If `io.popen` fails, `handle` is nil and indexing it crashes. Should add nil guards:
```lua
local handle = io.popen(cmd)
if not handle then return end
```

---

#### BUG-12: `build_fix_java_file_after_change_cmds` returns `{}` on error but return value is unused

**File**: `init.lua:189`

The function modifies `result_cmds` in-place via `table.insert`. On error it returns `{}`, but the caller (`build_fix_java_proj_after_change_cmds` at line 544) never checks this return value. The early return silently skips the file without logging at the call site.

**Fix**: Either log at the call site, or change the error path to `return false` and check it.

---

## Stability Risks (Not Bugs, But Fragile)

### RISK-1: Entire command chain fails if any single sed/rg fails

Shell commands are joined with `&&`, so if any command in the chain fails, all subsequent commands are skipped. A single file with special characters or a missing directory can abort the entire batch refactoring.

**Mitigation**: Consider using `;` instead of `&&` for independent operations, or wrap each command in a subshell with error handling.

---

### RISK-2: No dry-run or preview mode

Changes are applied directly to files with no preview. Combined with the `&&` chaining, a partially-failed refactoring leaves the project in an inconsistent state. The only safety net is git.

---

### RISK-3: `io.popen` usage blocks Neovim's event loop

Multiple `io.popen` calls (in import-fixer.lua and sibling-usage-fixer.lua) are synchronous and block Neovim. For large projects with many files, this can cause UI freezes during the Lua operation phase.

---

### RISK-4: Test mirror modifies `all_registered_changes` during processing

**File**: `init.lua:941-944`

Test mirrors are appended to `all_registered_changes` mid-processing. The subsequent loops (lines 956+) iterate over the now-larger list. While this works by design (test dirs need processing too), it's fragile - adding any iteration that holds an index into the list before this point would break.

---

### RISK-5: sed regex escaping is manual and incomplete

Package names are escaped with `gsub("%.", "\\.")` but this only handles dots. If a package segment contains other regex-special characters (unlikely in Java but possible in Kotlin/Scala), the sed command would break or match incorrectly.

---

## Test Coverage

**No tests exist.** The module has `test_mode = true/false` flag to enable direct execution without UI, which is designed for testing but no test files have been written.

The existing README lists a test suite as a planned feature.

---

## Quick Reference: Processing Flow

```
register_change(src, dst)     -- Accumulates in all_registered_changes[]
       |
process_registerd_changes()   -- Main entry point
       |
       +-- detect_module_path()         -- Scope to Maven/Gradle module
       +-- Track open buffers           -- For reopen after refactoring
       +-- Auto-mirror test packages    -- Move test dirs to match main
       +-- Classify changes:
       |   +-- Directory moves (depth >= 2) -> build_fix_java_package_after_change_cmds()
       |   +-- File moves               -> build_fix_java_file_after_change_cmds()
       |
       +-- For each file change, build operations:
       |   1. Fix type declaration (class/interface/enum/record)
       |   2. Fix type symbols where imported (explicit + wildcard imports)
       |   2.1. Fix sibling usage (Lua: sibling-usage-fixer)
       |   3. Fix FQN references (all file types)
       |   4. Fix package declaration in moved file
       |   4.1. Remove same-package imports
       |   5. Fix old package imports (Lua: import-fixer)
       |   6. Fix file paths in resources
       |
       +-- For each dir change, build operations:
       |   1. Fix package FQN names (all file types)
       |   2. Fix package file paths
       |
       +-- Separate shell cmds from Lua ops
       +-- Delete old buffers
       +-- Execute: shell (via terminal) -> callback(Lua ops -> reopen buffers)
       +-- Clear all_registered_changes
```

## File-by-File Summary

### init.lua (1214 lines)
- **Lines 1-30**: Module header, imports, logger, types
- **Lines 36-80**: `run_cmd()` - async shell execution in terminal buffer
- **Lines 82-136**: Constants (package roots, sed binary), `detect_module_path()`
- **Lines 148-429**: `build_fix_java_file_after_change_cmds()` - core file move logic (6 steps)
- **Lines 435-529**: `build_fix_java_package_after_change_cmds()` - directory move logic (2 steps)
- **Lines 534-566**: `build_fix_java_proj_after_change_cmd()` - routes to file or dir handler
- **Lines 568-578**: `register_change()` - public API, accumulates changes
- **Lines 583-595**: `get_all_src_siblings()` - finds files from same source directory
- **Lines 597-1204**: `process_registerd_changes()` - main orchestration (600+ lines)
  - **597-624**: Init, module detection
  - **629-681**: Buffer tracking
  - **683-948**: Test mirror logic (auto-move test packages, cleanup empties)
  - **950-1062**: Change classification and operation building
  - **1065-1203**: Execution (shell + Lua + buffer reopen)
- **Lines 1208-1212**: `process_single_file_change()` - convenience wrapper

### import-fixer.lua (356 lines)
- **fix_old_package_imports()**: Adds imports to the moved file for types left in old dir, fixes old dir files and test files that use the moved type, handles wildcard imports

### sibling-usage-fixer.lua (137 lines)
- **fix_sibling_usage()**: Adds import + updates type references for a single sibling type in one file

---

## Priority Fix Order

1. **BUG-2** (same-package import removal) - data corruption, easy fix
2. **BUG-1** (consecutive type refs) - most commonly hit in real code
3. **BUG-3** (annotations) - very common in Spring Boot projects
4. **BUG-5** (line-start types) - improves boundary pattern robustness
5. **BUG-7** (shell injection) - safety
6. **BUG-8** (xargs -r) - Linux compat
7. **BUG-4** (constructor rename) - edge case but real
8. **BUG-6** (multi-module) - if you work with multi-module projects
9. **BUG-9** (FQN boundary) - uncommon patterns
10. **BUG-10-12** (cosmetic) - low priority
