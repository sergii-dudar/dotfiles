-- Utility to get Maven project artifactId
local M = {}

---@param project_dir string
---@return string|nil
function M.get_artifact_id(project_dir)
    -- Strategy 1: Try xmllint (fastest and recomended: around 20ms!).
    local xmllint_result = M.get_artifact_id_from_xmllint(project_dir)
    if xmllint_result then
        return xmllint_result
    end

    -- Strategy 2: Try Maven command (most reliable but slower - ~90x slower that by xmllint! mvn by self not very fast)
    local maven_result = M.get_artifact_id_from_maven(project_dir)
    if maven_result then
        return maven_result
    end

    -- Strategy 3: Fallback to manual parsing pom.xml
    local pom_path = project_dir .. "/pom.xml"
    return M.get_artifact_id_from_pom(pom_path)
end

---@param project_dir string
---@return string|nil
function M.get_artifact_id_from_xmllint(project_dir)
    local pom_path = project_dir .. "/pom.xml"

    -- Check if pom.xml exists
    local pom_file = io.open(pom_path, "r")
    if not pom_file then
        return nil
    end
    pom_file:close()

    -- Use xmllint with XPath to extract artifactId
    -- This XPath gets the first artifactId that is a direct child of project (not in parent/dependencies)
    local handle = io.popen(
        string.format(
            "xmllint --xpath \"/*[local-name()='project']/*[local-name()='artifactId'][1]/text()\" '%s' 2>/dev/null",
            pom_path
        )
    )
    if not handle then
        return nil
    end

    local result = handle:read("*a")
    local success = handle:close()

    -- xmllint returns exit code 0 only on success
    if success and result and result ~= "" then
        -- Trim whitespace
        result = result:match("^%s*(.-)%s*$")
        -- Make sure it looks like a valid artifactId (not an error message)
        if result:match("^[a-zA-Z0-9._-]+$") then
            return result
        end
    end

    return nil
end

---@param project_dir string
---@return string|nil
function M.get_artifact_id_from_maven(project_dir)
    local handle = io.popen(
        string.format(
            "cd '%s' && mvn help:evaluate -Dexpression=project.artifactId -q -DforceStdout -o 2>/dev/null",
            project_dir
        )
    )
    if not handle then
        return nil
    end

    local result = handle:read("*a")
    handle:close()

    if result and result ~= "" then
        -- Trim whitespace
        result = result:match("^%s*(.-)%s*$")
        -- Make sure it looks like a valid artifactId (not an error message)
        if result:match("^[a-zA-Z0-9._-]+$") then
            return result
        end
    end

    return nil
end

---@param pom_path string
---@return string|nil
function M.get_artifact_id_from_pom(pom_path)
    local file = io.open(pom_path, "r")
    if not file then
        return nil
    end

    local content = file:read("*all")
    file:close()

    -- Match <artifactId>...</artifactId> but not in <parent> or <dependency> sections
    local lines = {}
    for line in content:gmatch("[^\r\n]+") do
        table.insert(lines, line)
    end

    local in_parent = false
    local in_dependency = false
    local in_plugin = false

    for _, line in ipairs(lines) do
        if line:match("<parent>") then
            in_parent = true
        elseif line:match("</parent>") then
            in_parent = false
        elseif line:match("<dependency>") or line:match("<dependencies>") then
            in_dependency = true
        elseif line:match("</dependency>") or line:match("</dependencies>") then
            in_dependency = false
        elseif line:match("<plugin>") or line:match("<plugins>") then
            in_plugin = true
        elseif line:match("</plugin>") or line:match("</plugins>") then
            in_plugin = false
        elseif not in_parent and not in_dependency and not in_plugin then
            local artifact_id = line:match("<artifactId>([^<]+)</artifactId>")
            if artifact_id then
                return artifact_id
            end
        end
    end

    return nil
end

return M