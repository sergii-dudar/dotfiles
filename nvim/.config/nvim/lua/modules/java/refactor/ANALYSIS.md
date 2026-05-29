# Java Refactor Module — Architecture & Analysis

> Updated: 2026-05-30 | Status: Working, actively maintained | Files: 9 Lua modules (~1760 LOC)

## Architecture

```
init.lua (316 lines)          — Mediator: register, orchestrate, public API
├── constants.lua (106)       — Package roots, OS/shell utilities, sed patterns
├── canonical.lua (166)       — Canonical transformation detection + dest correction
├── mirror-sync.lua (420)     — Test↔src mirror compute, dedup, physical move, cleanup
├── buffer-manager.lua (178)  — Buffer track, delete, reopen
├── cmd-builder.lua (456)     — Build shell/Lua fix operations for files & packages
├── executor.lua (121)        — Terminal execution, test-mode, operation separation
├── import-fixer.lua (402)    — Import management for moved files
└── sibling-usage-fixer.lua (168) — Cross-reference fixes for batch moves
```

### Processing Flow

```
register_change(src, dst)           — Accumulates in all_registered_changes[]
       |
process_registerd_changes()         — Main orchestration (init.lua)
       |
       ├── 1. detect_module_path()          [constants.lua]
       ├── 2. track_buffers()               [buffer-manager.lua]
       ├── 3. canonical.detect()            [canonical.lua]
       │       └── canonical.correct_destinations()
       ├── 4. mirror_sync.sync()            [mirror-sync.lua]
       │       ├── detect_structural_refactorings()
       │       ├── compute_individual_mirrors()
       │       │       └── is_parent_of_canonical() filter
       │       ├── deduplicate_mirrors()
       │       ├── perform_physical_moves()
       │       └── cleanup_empty_dirs()
       ├── 5. build_operations()            [init.lua — package-move dedup]
       │       └── cmd_builder.build_fix_commands()  [cmd-builder.lua]
       ├── 6. executor.separate_operations()  [executor.lua]
       ├── 7. buffer_manager.delete_old_buffers()  [buffer-manager.lua]
       ├── 8. composite_callback:
       │       ├── Lua ops (import-fixer, sibling-usage-fixer)
       │       ├── buffer_manager.reopen_buffers()
       │       └── mirror_sync.cleanup_empty_dirs()
       └── 9. executor.run_cmd() OR schedule callback  [executor.lua]
```

### Integration Points

- `plugins/navigation/fyler-nvim.lua` → `register_change()` in `on_rename` hook
- `config/autocmds.lua` → `process_registerd_changes()` on fyler `BufUnload`
- Both guarded by `java_util.is_java_project()`

### Dependencies

External: `rg` (ripgrep), `fd`, `gsed`/`sed` (GNU sed), `find`
Internal: `utils/{common,string,buffer,list,logging}-util`, `utils/ui/spinner`, `utils/global-util`

---

## Key Algorithms

### Canonical Transformation (canonical.lua)

When a file manager (fyler.nvim) renames directories, it emits intermediate events with **wrong**
destinations (e.g., `payments → govern` instead of `payments → govern/test/other`).

**Solution**: Extract the **canonical** old→new prefix from file-level moves (always correct):
1. Strip package root and filename from a file move's src/dst
2. Compare segments from the END to find common suffix
3. Remaining head segments are the changed prefix pair

Then correct all directory-level destinations using this canonical transformation.

### Parent-of-Canonical Filter

Directory events that are PARENTS of the canonical prefix (e.g., `ua/raiffeisen` when canonical is
`ua/raiffeisen/payments`) represent partial renames. Mirroring/sed'ing these would only partially
transform the path. They are filtered out in both mirror computation and package-move building.

### Mirror Deduplication

After filtering parent-of-canonical, mirrors are deduplicated by keeping only the **shallowest**
per branch. The shallowest mirror's `mv` covers all nested content physically, and its sed
(with `.` in trailing boundary) covers all subpackage declarations.

### Package-Move Deduplication

Same principle: shallowest correct sed covers all deeper packages. The trailing boundary
`([;$"[:space:].,()><@]|$)` includes `.` which matches subpackage separators.

---

## Bugs Fixed (from original ANALYSIS)

| Bug | Description | Status |
|-----|-------------|--------|
| BUG-1 | Consecutive type refs (sed overlap) | ✅ Fixed: double-pass sed |
| BUG-2 | Same-package import deletes subpackages | ✅ Fixed: `[A-Z][^.]*;$` |
| BUG-3 | Annotations (`@Type`) not matched | ✅ Fixed: `@` in boundary |
| BUG-4 | Constructor not renamed for move+rename | ✅ Fixed: explicit step |
| BUG-5 | Type at line start not matched | ✅ Fixed: `^` in pattern |
| BUG-7 | Shell injection via paths | ✅ Fixed: `shell_escape()` |
| BUG-8 | Missing xargs `-r` on Linux | ✅ Fixed: OS-aware xargs |
| BUG-9 | FQN boundary too narrow | ✅ Fixed: expanded chars |
| RISK-1 | `&&` chain fails on first error | ✅ Fixed: uses `;` separator |

### Remaining Known Issues

| Issue | Description | Priority |
|-------|-------------|----------|
| BUG-6 | Module detection uses first change only | Low (single-module typical) |
| BUG-10 | Typo `rejactor` in annotations | Cosmetic |
| RISK-2 | No dry-run/preview mode | Medium |
| RISK-3 | `io.popen` blocks event loop | Low (fast in practice) |

---

## Test Coverage

**No automated tests yet.** The `M.test_mode = true` flag enables direct execution without UI,
designed for integration tests. Test project available at:
`/Users/iuada144/serhii.home/work/git.work/ua-payments-common-infra/infra-metrics`

---

## Module Responsibilities

### constants.lua
- `package_roots` — recognized Java source trees
- `sed`, `xargs` — OS-appropriate commands
- `shell_escape()` — safe path quoting
- `build_type_replace_expr()` — double-pass sed for type symbols
- `detect_module_path()` — find Maven/Gradle module root
- `get_project_root()` — dynamic cwd
- `get_package_path()` — extract package-relative path

### canonical.lua
- `detect(all_changes)` → `CanonicalTransformation|nil`
- `correct_destinations(all_changes, canonical)` — mutates change.dst in-place
- `is_parent_of_canonical(change, canonical)` → boolean

### mirror-sync.lua
- `sync(all_changes, canonical, module_path, buffers)` → mirrors applied
- `cleanup_empty_dirs(module_path)` — find+delete empty dirs in source trees

### buffer-manager.lua
- `track_buffers(all_changes)` → `BufferReopenInfo[]`
- `delete_old_buffers(buffers)` — switch current to scratch, delete others
- `reopen_buffers(buffers)` — open files at new paths in original windows

### cmd-builder.lua
- `build_fix_commands(context, module_path)` → `RefactorOperation[]|nil`
- `get_all_src_siblings(context, all_changes)` → sibling moves

### executor.lua
- `run_cmd(cmd_string, callback)` — terminal split with spinner
- `execute_test_mode(shell_cmds, lua_ops)` → boolean
- `separate_operations(ops)` → shell_cmds, lua_ops

### init.lua (mediator)
- `M.register_change(src, dst)` — accumulate changes
- `M.process_registerd_changes()` — orchestrate all steps
- `M.process_single_file_change(src, dst)` — convenience wrapper
- `build_operations()` (local) — package-move detection + dedup + cmd building
