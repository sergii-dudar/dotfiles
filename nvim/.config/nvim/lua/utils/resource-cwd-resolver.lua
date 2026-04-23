local M = {}

local uv = vim.uv or vim.loop

---@class ResourceResolveResult
---@field dirs string[] absolute paths to search
---@field title string picker title

---@alias ResourceResolver fun(bufnr: integer): ResourceResolveResult|nil

---@type table<string, ResourceResolver>
local resolvers = {}

--- Register a resolver for a specific filetype.
---@param ft string filetype (e.g. "java", "python")
---@param resolver ResourceResolver
function M.register(ft, resolver)
    resolvers[ft] = resolver
end

--- Resolve resource directories for the given buffer based on its filetype.
--- Returns nil when no resolver is registered for the current filetype.
---@param bufnr? integer
---@return ResourceResolveResult|nil
function M.resolve(bufnr)
    bufnr = bufnr or vim.api.nvim_get_current_buf()
    local ft = vim.bo[bufnr].filetype
    local resolver = resolvers[ft]
    if resolver then
        return resolver(bufnr)
    end
    return nil
end

--- Filter dirs to only those that exist on disk.
---@param dirs string[]
---@return string[]
local function existing_dirs(dirs)
    return vim.tbl_filter(function(d)
        return uv.fs_stat(d) ~= nil
    end, dirs)
end

-- Java resolver: detect Maven/Gradle module layout and return resource dirs.
M.register("java", function(bufnr)
    local java_common = require("utils.java.java-common")
    local module_root = java_common.get_buffer_project_path(bufnr)
    if not module_root then
        return nil
    end

    local file_path = vim.api.nvim_buf_get_name(bufnr)
    local is_test = file_path:match("/src/test/") ~= nil
    local module_name = vim.fn.fnamemodify(module_root, ":t")

    local dirs
    if is_test then
        dirs = existing_dirs({
            module_root .. "/src/test/resources",
            module_root .. "/src/main/resources",
        })
    else
        dirs = existing_dirs({
            module_root .. "/src/main/resources",
        })
    end

    if #dirs == 0 then
        return nil
    end

    local scope = is_test and "test+main" or "main"
    return {
        dirs = dirs,
        title = "Resources [" .. module_name .. "] (" .. scope .. ")",
    }
end)

return M
