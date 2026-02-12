-- Utility to get Gradle project name (equivalent to Maven's artifactId)
local M = {}

---@param project_dir string
---@return string|nil
function M.get_project_name(project_dir)
    -- Strategy 1: Try gradle command (most reliable but slow)
    local gradle_result = M.get_project_name_from_gradle(project_dir)

    if gradle_result then
        return gradle_result
    end

    -- Strategy 2: Use directory name (Gradle's standard behavior for submodules)
    -- In Gradle, submodules are referenced by their directory name in settings.gradle
    -- For example: include ':api-module' where 'api-module' is the directory name
    -- So we just return the last part of the path
    return M.get_project_name_from_path(project_dir)
end

---@param project_dir string
---@return string|nil
function M.get_project_name_from_gradle(project_dir)
    -- Try both gradle and gradlew
    for _, gradle_cmd in ipairs({ "./gradlew", "gradle" }) do
        local handle = io.popen(
            string.format(
                "cd '%s' && %s properties --console=plain -q 2>/dev/null | grep '^name:' | cut -d' ' -f2",
                project_dir,
                gradle_cmd
            )
        )
        if handle then
            local result = handle:read("*a")
            handle:close()

            if result and result ~= "" then
                -- Trim whitespace
                result = result:match("^%s*(.-)%s*$")
                -- Make sure it looks like a valid project name
                if result:match("^[a-zA-Z0-9._-]+$") then
                    return result
                end
            end
        end
    end

    return nil
end

---@param project_dir string
---@return string|nil
function M.get_project_name_from_path(project_dir)
    -- Extract the last component of the path (directory name)
    -- This is what Gradle uses for submodule names by default
    -- For example: /path/to/my-project/api-module -> "api-module"
    local name = project_dir:match("([^/\\]+)[/\\]?$")
    if name and name ~= "" then
        return name
    end

    return nil
end

return M
