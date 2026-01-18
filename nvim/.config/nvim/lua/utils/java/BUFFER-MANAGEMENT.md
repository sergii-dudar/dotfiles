# Buffer Management for Java Refactoring

## Overview

The Java refactoring utility now includes comprehensive buffer management that automatically tracks and reopens Neovim buffers when Java files are moved during package refactoring.

## How It Works

### 1. Track Opened Buffers (Lines 454-499)

When `process_registerd_changes()` is called, the tool first tracks all opened buffers:

1. **Scans all registered changes** (both main and test files)
2. **Finds opened buffers** for files that will be moved
3. **Calculates new paths** for each buffer based on the move operation
4. **Saves buffer information** to `opened_buffers_to_reopen` table

For each change:
- If it's a **directory move**: Finds all `.java` files in the directory and checks if any are open
- If it's a **file move**: Checks if that specific file is open

### 2. Track Test Buffers (Lines 524-557)

When test packages are automatically mirrored:

1. **Before physical test file moves**, scans for opened test buffers
2. **Calculates new test paths** based on the mirror operation
3. **Adds test buffers** to the reopen list

This ensures test files that are automatically moved also get their buffers updated.

### 3. Delete Old Buffers (Lines 1032-1053)

**BEFORE applying any file system changes**, the tool deletes old buffers:

1. **For each tracked buffer**:
   - Checks if the buffer is still valid
   - **Stores the window ID** before deletion (to restore later)
   - **Deletes the old buffer** pointing to the old file location
   - Logs any deletion failures

**Why delete before changes?** This prevents having buffers pointing to non-existent files during the refactoring process.

### 4. Apply Refactoring Changes (Lines 1047-1081)

After buffers are deleted, the tool executes all refactoring commands **asynchronously**:
- Starts a job via `vim.fn.jobstart` to run the refactoring commands
- Files are physically moved
- Package declarations are updated
- Imports are fixed
- Job runs in background terminal window

### 5. Reopen Buffers at New Locations (in job `on_exit` callback)

**Critical timing detail**: The buffer reopening is passed as a **callback** to the refactoring job and only executes AFTER the job completes successfully. This ensures files exist before we try to reopen them.

When the refactoring job finishes successfully:

1. **on_exit callback is triggered** (lines 52-68 in run_cmd)
2. **Callback reopens all buffers** using `vim.schedule()`
3. **For each tracked buffer**:
   - Checks if the new file exists at the new location
   - If buffer had a window: Opens new file in **the same window**
   - If buffer was hidden: Adds to buffer list with `:badd`
   - **Triggers filetype detection** to reattach LSP, Treesitter, etc.

## Example Workflow

### User Scenario

User has these files open in Neovim:
```
src/main/java/com/example/Employee.java (in window 1)
src/test/java/com/example/EmployeeTest.java (in window 2)
src/main/java/com/example/service/EmployeeService.java (hidden buffer)
```

User moves package `com/example` → `ua/dsm/corp` using fyler.

### What Happens

1. **Tracking phase**:
   - Tool finds 3 opened buffers
   - Calculates new paths:
     - `src/main/java/ua/dsm/corp/Employee.java`
     - `src/test/java/ua/dsm/corp/EmployeeTest.java` (auto-mirrored)
     - `src/main/java/ua/dsm/corp/service/EmployeeService.java`
   - Stores window IDs for each buffer

2. **Buffer deletion phase** (BEFORE changes):
   - Deletes all 3 old buffers
   - Windows become empty (showing no file)
   - **No buffers point to non-existent files**

3. **Refactoring phase**:
   - Files are physically moved to new locations
   - Package declarations are updated
   - Imports are fixed
   - Test files are mirrored

4. **Buffer restoration phase** (AFTER changes):
   - Window 1: Opens `ua/dsm/corp/Employee.java` with updated content
   - Window 2: Opens `ua/dsm/corp/EmployeeTest.java` with updated content
   - Hidden buffer: `ua/dsm/corp/service/EmployeeService.java` loaded in buffer list
   - All buffers have LSP, Treesitter, and other features reattached

## Benefits

1. **No duplicate buffers**: By deleting old buffers BEFORE moving files, you never have buffers pointing to non-existent files
2. **Seamless workflow**: No need to manually close and reopen files after refactoring
3. **Preserves window layout**: Files reopen in the same windows they were in
4. **Automatic LSP reattachment**: Filetype detection ensures LSP clients reconnect
5. **Works with hidden buffers**: Even non-displayed buffers are tracked and updated
6. **Handles test files**: Automatically tracks test buffers that are mirrored

## Implementation Details

### Buffer Tracking Structure

```lua
{
    old_path = "/path/to/old/File.java",
    new_path = "/path/to/new/File.java",
    buf_id = 42,    -- Neovim buffer ID
    win_id = 1001   -- Window ID (set during deletion phase)
}
```

The `win_id` is captured BEFORE deleting the buffer so we can restore the file in the same window after refactoring.

### Key Functions Used

- `buffer_util.find_buf_by_path(path)`: Finds buffer by file path
- `vim.api.nvim_buf_is_valid(buf_id)`: Checks if buffer still exists
- `vim.fn.bufwinid(buf_id)`: Gets window displaying buffer
- `vim.api.nvim_buf_delete(buf_id, {force=false})`: Closes old buffer
- `vim.api.nvim_win_call(win_id, fn)`: Executes command in specific window
- `vim.schedule(fn)`: Defers execution to ensure file system sync

### Error Handling

- Uses `pcall()` when deleting buffers to handle already-deleted buffers
- Checks if new file exists before attempting to open it
- Logs warnings if new file cannot be found
- Gracefully handles invalid buffer IDs

## Logging

The feature includes detailed logging:

```
INFO: Tracking opened buffers before applying changes...
INFO: Will reopen buffer: /old/path/File.java -> /new/path/File.java
INFO: Will reopen test buffer: /old/test/path/FileTest.java -> /new/test/path/FileTest.java
INFO: Found 3 opened buffers to reopen after changes
INFO: Deleting 3 old buffers before applying changes...
INFO: Deleted old buffer: /old/path/File.java
INFO: Deleted old buffer: /old/test/path/FileTest.java
[Refactoring commands execute here]
INFO: Reopening 3 buffers from new locations...
INFO: Reopened buffer in window: /new/path/File.java
INFO: Loaded buffer: /hidden/path/Service.java
INFO: Buffer reopening completed
```

## Critical Implementation Details

### Asynchronous Execution Timing

The refactoring commands execute **asynchronously** via `vim.fn.jobstart`. This means:

- The job starts in the background and `run_cmd` returns immediately
- Buffer reopening MUST wait for job completion (via callback)
- **Previous bug**: Buffer reopening happened immediately after starting the job, before files were moved
- **Fix**: Buffer reopening now happens in the job's `on_exit` callback, ensuring files exist first

This is why only the focused buffer appeared to work before - the timing race condition meant most files hadn't been moved yet when reopening was attempted.

## Limitations

- **Test mode**: Buffer reopening is only active in normal mode, not test mode (test mode uses synchronous execution)
- **File must exist**: If the new file doesn't exist (e.g., due to refactoring failure), buffer won't be reopened
- **Same filetype**: Assumes files remain `.java` files (which is correct for this use case)
- **Success required**: Buffers only reopen if refactoring job exits with code 0 (success)

## Future Enhancements

Potential improvements:
- Support for reopening buffers in test mode
- Preserve cursor position and folds when reopening
- Handle resource files (.properties, .xml) in addition to .java files
- Preserve view state (scroll position, etc.)
