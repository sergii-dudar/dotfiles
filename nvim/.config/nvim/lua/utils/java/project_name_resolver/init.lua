local logging = require("utils.logging-util")
local log = logging.new({ name = "module_resolver", filename = "project_name_resolver.log", logging })
log.set_level(vim.log.levels.DEBUG)

local pom_parser = require("utils.java.project_name_resolver.pom_parser")
local gradle_parser = require("utils.java.project_name_resolver.gradle_parser")
local java_common = require("utils.java.java-common")

local M = {}

--- Resolve project name using multiple strategies in order of reliability:
--- 1. Parse module's pom.xml (Maven) or settings.gradle (Gradle)
--- 2. Use directory name as last resort
---
--- @param module_dir string|nil The module directory
--- @param project_type string|nil "maven" or "gradle"
--- @return string The resolved project name
function M.resolve_project_name(module_dir, project_type)
    -- print(require("utils.java.project_name_resolver").resolve_project_name())
    -- print(require("utils.java.java-common").get_buffer_project_path())

    module_dir = module_dir or java_common.get_buffer_project_path()
    project_type = module_dir or java_common.detect_project_type()
    log.debug("Resolving projectName from build file for module: " .. module_dir)

    -- Strategy 1: Parse build file (pom.xml or settings.gradle)
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

    -- Strategy 2: Use directory name as last resort
    local dir_name = vim.fn.fnamemodify(module_dir, ":t:r")
    log.warn("Could not resolve projectName from build file, using directory name: " .. dir_name)
    return dir_name
end

return M
