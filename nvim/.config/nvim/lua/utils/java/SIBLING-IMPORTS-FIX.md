# Sibling Imports Fix

## Problem

When moving multiple Java files from one package to another (e.g., from `service/` to `service/impl/`) simultaneously, imports were not being correctly updated:

### Example Scenario

Moving these files from `ua.dsm.corp.EmployeeManagementSystem.service` to `ua.dsm.corp.EmployeeManagementSystem.service.impl`:
- `ServiceEmployee.java`
- `MarkerInterface.java`
- `ServiceEmployeeIn.java`

### What Went Wrong

1. **In moved files**: When processing `ServiceEmployee.java`:
   - Script scanned old `service/` directory
   - Found `MarkerInterface.java` still there (during processing)
   - Added: `import ua.dsm.corp.EmployeeManagementSystem.service.MarkerInterface;`
   - **But `MarkerInterface` is ALSO being moved to `service.impl`!**
   - **Should have been**: `import ua.dsm.corp.EmployeeManagementSystem.service.impl.MarkerInterface;`

2. **In files that stayed**: Files remaining in `service/` directory didn't get updated imports for the moved classes

## Root Cause

The `fix-old-imports.sh` script processed each file individually without knowing which OTHER files were being moved in the same batch (siblings). It would:
- Scan the old directory
- Find all Java files still present
- Add imports from the OLD location
- Not realize those files were ALSO moving to the NEW location

## Solution

### Enhanced Bash Script (`fix-old-imports.sh`)

**Added 7th parameter**: `SIBLING_TYPES` - comma-separated list of type names also being moved

**New logic**:
```bash
# Check if a type is a sibling (also being moved)
is_sibling() {
    local type=$1
    for sibling in "${SIBLINGS_ARRAY[@]}"; do
        if [[ "$sibling" == "$type" ]]; then
            return 0
        fi
    done
    return 1
}

# When adding imports:
if is_sibling "$filename"; then
    import_package="$NEW_PACKAGE"  # Import from new location!
else
    import_package="$OLD_PACKAGE"  # Import from old location
fi
```

### Enhanced Lua Code

**Lines 254-265**: Build sibling types list
```lua
local sibling_types = {}
if context.siblings and not vim.tbl_isempty(context.siblings) then
    for _, sibling in ipairs(context.siblings) do
        local sibling_type = sibling.src:match("([^/]+)%.java$")
        if sibling_type then
            table.insert(sibling_types, sibling_type)
        end
    end
end
local sibling_types_str = table.concat(sibling_types, ",")
```

**Lines 267-280**: Pass siblings to script
```lua
local fix_old_file_imports = string.format(
    '"%s" "%s" "%s" "%s" "%s" "%s" "%s" "%s"',
    global.dotfiles_path("work/java/remane/fix-old-imports.sh"),
    src:match("(.+)/[^/]+$"), -- OLD_DIR
    package_declaration_src, -- OLD_PACKAGE
    package_declaration_dst, -- NEW_PACKAGE
    dst, -- NEW_FILE_PATH
    old_type_name, -- OLD_TYPE_NAME
    new_type_name, -- NEW_TYPE_NAME
    sibling_types_str -- SIBLING_TYPES ✨ NEW
)
```

## How It Works Now

### When moving `ServiceEmployee.java`, `MarkerInterface.java`, and `ServiceEmployeeIn.java` from `service/` to `service/impl/`:

1. **Processing `ServiceEmployee.java`**:
   - Siblings: `MarkerInterface`, `ServiceEmployeeIn`
   - Scans old `service/` directory
   - Finds `MarkerInterface.java`
   - Checks: Is `MarkerInterface` a sibling? **YES**
   - Adds: `import ua.dsm.corp.EmployeeManagementSystem.service.impl.MarkerInterface;` ✅
   - Finds `ServiceEmployeeIn.java`
   - Checks: Is `ServiceEmployeeIn` a sibling? **YES**
   - Adds: `import ua.dsm.corp.EmployeeManagementSystem.service.impl.ServiceEmployeeIn;` ✅

2. **Processing `MarkerInterface.java`**:
   - Siblings: `ServiceEmployee`, `ServiceEmployeeIn`
   - Same logic applies
   - Imports siblings from NEW location (`service.impl`) ✅

3. **Files that stayed in `service/`**:
   - Still get imports for moved types (via `fix-java-sibling-usage.sh`)
   - Correctly import from new `service.impl` package ✅

## Example Output

### Before Fix

```java
// ServiceEmployee.java (moved to service/impl/)
package ua.dsm.corp.EmployeeManagementSystem.service.impl;

import ua.dsm.corp.EmployeeManagementSystem.service.MarkerInterface; // ❌ WRONG!
import ua.dsm.corp.EmployeeManagementSystem.service.ServiceEmployeeIn; // ❌ WRONG!

public class ServiceEmployee implements MarkerInterface {
    // ...
}
```

### After Fix

```java
// ServiceEmployee.java (moved to service/impl/)
package ua.dsm.corp.EmployeeManagementSystem.service.impl;

import ua.dsm.corp.EmployeeManagementSystem.service.impl.MarkerInterface; // ✅ CORRECT!
import ua.dsm.corp.EmployeeManagementSystem.service.impl.ServiceEmployeeIn; // ✅ CORRECT!

public class ServiceEmployee implements MarkerInterface {
    // ...
}
```

## Backwards Compatibility

The 7th parameter (`SIBLING_TYPES`) is **optional**:
- If not provided, script behaves as before (imports from OLD_PACKAGE)
- Existing code that doesn't pass siblings still works
- New code gets the enhanced behavior

## Testing

To test this fix, move multiple files from one package to another:

```lua
-- In fyler.nvim, move multiple files at once:
-- service/ServiceEmployee.java → service/impl/ServiceEmployee.java
-- service/MarkerInterface.java → service/impl/MarkerInterface.java
-- service/ServiceEmployeeIn.java → service/impl/ServiceEmployeeIn.java
```

Check the logs:
```
INFO: Sibling type being moved: MarkerInterface
INFO: Sibling type being moved: ServiceEmployeeIn
DEBUG: Import fix command with siblings: MarkerInterface,ServiceEmployeeIn
```

Verify imports in moved files reference the correct new package locations.

## Benefits

1. **Correct imports**: Sibling classes are imported from their new location
2. **Batch moves work**: Multiple files can be moved together without import issues
3. **No manual fixes**: Imports are automatically corrected during refactoring
4. **Maintains consistency**: All imports reflect actual file locations
