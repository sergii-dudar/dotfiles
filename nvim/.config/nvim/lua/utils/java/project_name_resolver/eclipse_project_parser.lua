-- Utility to get the Eclipse project name (the name JDTLS registered the project under)
--
-- • get_project_name — parse .project to extract the top-level projectDescription name

local M = {}

--- Read the Eclipse project name from the module's `.project` file.
--- Unlike the build file, it stays correct for names JDTLS/Buildship changed on import
--- (e.g. a Gradle root project imported as `<rootProject.name>-<dir>`).
---@param project_dir string
---@return string|nil
function M.get_project_name(project_dir)
    local project_path = project_dir .. "/.project"
    local content = require("lib.file").read_file(project_path)
    if not content then
        return nil
    end

    local ok, root = pcall(require("lib.xml").parse, content)
    if not ok or not root or type(root.projectDescription) ~= "table" then
        return nil
    end

    local name = root.projectDescription.name
    if type(name) ~= "string" then
        return nil
    end

    name = name:match("^%s*(.-)%s*$")
    if name == "" then
        return nil
    end

    return name
end

return M
