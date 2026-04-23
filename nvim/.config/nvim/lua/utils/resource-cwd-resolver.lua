local M = {}

local uv = vim.uv or vim.loop

---@class ResourceResolveResult
---@field dirs string[] absolute paths to search
---@field title string picker title

---@alias ResourceResolver fun(bufnr: integer): ResourceResolveResult|nil

---@type ResourceResolver[]
local resolvers = {}

--- Register a resolver. Resolvers are tried in registration order;
--- the first non-nil result wins.
---@param resolver ResourceResolver
function M.register(resolver)
    resolvers[#resolvers + 1] = resolver
end

--- Resolve resource directories for the given buffer.
---@param bufnr? integer
---@return ResourceResolveResult|nil
function M.resolve(bufnr)
    bufnr = bufnr or vim.api.nvim_get_current_buf()
    for _, resolver in ipairs(resolvers) do
        local result = resolver(bufnr)
        if result then
            return result
        end
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
-- Works regardless of buffer filetype — inspects the buffer path for Java module structure.
M.register(function(bufnr)
    local java_common = require("utils.java.java-common")
    local module_root = java_common.get_buffer_project_path(bufnr)
    if not module_root then
        return nil
    end

    -- Verify this is actually a Java module (has src/main or src/test)
    local src_main = module_root .. "/src/main"
    local src_test = module_root .. "/src/test"
    if uv.fs_stat(src_main) == nil and uv.fs_stat(src_test) == nil then
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
