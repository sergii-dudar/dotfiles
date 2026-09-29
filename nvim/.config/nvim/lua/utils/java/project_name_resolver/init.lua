-- Project name resolver: determines Maven/Gradle module name from project directory.
--
-- • resolve_project_name — resolve module name from .project, then by project type (maven/gradle)

local logging = require("utils.logging-util")
local log = logging.new({ name = "module_resolver", filename = "project_name_resolver.log", logging })
log.set_level(vim.log.levels.DEBUG)

local eclipse_project_parser = require("utils.java.project_name_resolver.eclipse_project_parser")
local pom_parser = require("utils.java.project_name_resolver.pom_parser")
local gradle_parser = require("utils.java.project_name_resolver.gradle_parser")
local java_common = require("utils.java.java-common")

local M = {}

--- Strip trailing path separators, so build file paths and the directory name fallback stay valid.
---@param dir string
---@return string
local function strip_trailing_slashes(dir)
    local stripped = dir:gsub("/+$", "")
    return stripped ~= "" and stripped or dir
end

--- Resolve project name using multiple strategies in order of reliability:
--- 1. Read the Eclipse project name from module's .project (the name JDTLS registered)
--- 2. Parse module's pom.xml (Maven) or settings.gradle (Gradle)
--- 3. Use directory name as last resort
---
--- @param module_dir string|nil The module directory (current buffer project by default, cwd if buffer has none)
--- @param project_type string|nil "maven" or "gradle"
--- @return string The resolved project name
function M.resolve_project_name(module_dir, project_type)
    -- print(require("utils.java.project_name_resolver").resolve_project_name())
    -- print(require("utils.java.java-common").get_buffer_project_path())

    module_dir = module_dir or java_common.get_buffer_project_path()
    if not module_dir then
        -- Buffer outside of any project (dependency source, scratch file, etc.): the debugged project is the cwd one
        module_dir = vim.fn.getcwd()
        project_type = project_type or java_common.detect_project_type_at(module_dir)
        log.warn("No project root for current buffer, using cwd: " .. module_dir)
    end
    module_dir = strip_trailing_slashes(module_dir)
    project_type = project_type or java_common.detect_project_type()
    log.debug("Resolving projectName for module: " .. module_dir)

    -- Strategy 1: Eclipse .project file (the name JDTLS registered the project under)
    local eclipse_name = eclipse_project_parser.get_project_name(module_dir)
    if eclipse_name then
        log.info("Resolved projectName from .project: " .. eclipse_name)
        return eclipse_name
    end

    -- Strategy 2: Parse build file (pom.xml or settings.gradle)
    if project_type == "maven" then
        local pom_path = module_dir .. "/pom.xml"
        if vim.fn.filereadable(pom_path) == 1 then
            local artifact_id = pom_parser.get_artifact_id(module_dir)
            if artifact_id then
                log.info("Resolved projectName from pom.xml: " .. artifact_id)
                return artifact_id
            end
        end
    elseif project_type == "gradle" then
        local project_name = gradle_parser.get_project_name(module_dir)
        if project_name then
            log.info("Resolved projectName from Gradle: " .. project_name)
            return project_name
        end
    end

    -- Strategy 3: Use directory name as last resort
    -- (":t" only: ":r" would cut a dotted directory name, e.g. "com.example.app" -> "com.example")
    local dir_name = vim.fn.fnamemodify(module_dir, ":t")
    log.warn("Could not resolve projectName from build file, using directory name: " .. dir_name)
    return dir_name
end

return M
