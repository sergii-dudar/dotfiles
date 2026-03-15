-- Build and open jdt://jarentry/ URIs for resource files inside dependency jars.
-- Format: jdt://jarentry/{entry_path}?={project}/{escaped_jar}={maven_metadata}

local M = {}

---Build jdt://jarentry/ URI from an extracted source file path.
---@param file_path string absolute path to a file inside an extracted sources dir
---@param source_dirs string[] list of all source dirs to match against
---@return string|nil uri
function M.build_uri(file_path, source_dirs)
    local source_dir
    for _, dir in ipairs(source_dirs) do
        if file_path:sub(1, #dir + 1) == dir .. "/" then
            source_dir = dir
            break
        end
    end
    if not source_dir then
        return nil
    end

    local entry_path = file_path:sub(#source_dir + 2)
    local jar_path = source_dir:gsub("-sources$", "") .. ".jar"

    local clients = vim.lsp.get_clients({ name = "jdtls" })
    if #clients == 0 then
        return nil
    end
    local root_dir = clients[1].config.root_dir
    local project_name = require("utils.java.project_name_resolver.pom_parser").get_artifact_id(root_dir)
        or vim.fn.fnamemodify(root_dir, ":t")

    local repo_rel = jar_path:match(".m2/repository/(.+)")
    if not repo_rel then
        return nil
    end
    local parts = vim.split(repo_rel, "/")
    if #parts < 4 then
        return nil
    end
    local version = parts[#parts - 1]
    local artifact_id = parts[#parts - 2]
    local group_parts = {}
    for i = 1, #parts - 3 do
        table.insert(group_parts, parts[i])
    end
    local group_id = table.concat(group_parts, ".")

    local escaped_jar = jar_path:gsub("/", "%%5C/")

    return string.format(
        "jdt://jarentry/%s?=%s/%s=/maven.pomderived=/true=/=/maven.groupId=/%s=/=/maven.artifactId=/%s=/=/maven.version=/%s=/=/maven.scope=/compile=/=/maven.pomderived=/true=/",
        entry_path,
        project_name,
        escaped_jar,
        group_id,
        artifact_id,
        version
    )
end

---Open a resource file via jdt://jarentry/ URI.
---@param file_path string absolute path to extracted source file
---@param source_dirs string[] list of all source dirs
---@param line? number line to jump to
---@return boolean success
function M.open(file_path, source_dirs, line)
    local uri = M.build_uri(file_path, source_dirs)
    if not uri then
        return false
    end
    vim.cmd("edit " .. vim.fn.fnameescape(uri))
    if line and line > 1 then
        pcall(vim.api.nvim_win_set_cursor, 0, { line, 0 })
    end
    return true
end

return M
