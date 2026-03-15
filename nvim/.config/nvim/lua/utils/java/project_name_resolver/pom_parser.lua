-- Utility to get Maven project artifactId
local M = {}

---@param project_dir string
---@return string|nil
function M.get_artifact_id(project_dir)
    local pom_path = project_dir .. "/pom.xml"
    local content = require("lib.file").read_file(pom_path)
    if not content then
        return nil
    end

    local ok, root = pcall(require("lib.xml").parse, content)
    if not ok or not root or not root.project then
        return nil
    end

    return root.project.artifactId
end

return M
