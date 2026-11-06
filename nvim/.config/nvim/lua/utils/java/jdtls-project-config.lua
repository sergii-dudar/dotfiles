-- Manual "reload the build configuration" for jdtls (`<leader>jdu`).
--
-- `java.configuration.updateBuildConfiguration` is `disabled` in jdtls-config-util.lua: after a pom.xml / Gradle
-- file change jdtls neither prompts nor re-imports, it only marks the build file with the diagnostic "The build
-- file has been changed and may need reload to make it effective" (StandardProjectsManager.fileChanged, jdtls
-- 1.61). This module is the explicit trigger: it sends `java/projectConfigurationsUpdate` for the current buffer,
-- which may be a Java file (its module is updated) or the build file itself. jdtls is attached to Java buffers
-- only, so for a build file the client is found through its workspace root instead of the buffer.
local M = {}

local lsp_util = require("utils.lsp-util")

-- file names jdtls treats as build files of the Maven and Gradle importers
local BUILD_FILES = {
    ["pom.xml"] = true,
    ["build.gradle"] = true,
    ["build.gradle.kts"] = true,
    ["settings.gradle"] = true,
    ["settings.gradle.kts"] = true,
    ["gradle.properties"] = true,
}

--- Return whether the path names a Maven/Gradle build file.
---@param path string
---@return boolean
function M.is_build_file(path)
    return BUILD_FILES[vim.fs.basename(path)] == true
end

--- Return whether `path` is `root` or lies below it.
---@param path string
---@param root string
---@return boolean
local function is_under(path, root)
    root = root:gsub("/+$", "")
    return path == root or path:sub(1, #root + 1) == root .. "/"
end

--- Find the jdtls client responsible for a buffer: the attached one, otherwise the client whose workspace root
--- contains the file (the deepest root wins). Symlinked paths are compared resolved as well.
---@param bufnr integer
---@return vim.lsp.Client|nil
function M.find_client(bufnr)
    local attached = lsp_util.get_client_by_name("jdtls", { bufnr = bufnr })
    if attached then
        return attached
    end
    local path = vim.api.nvim_buf_get_name(bufnr)
    if path == "" then
        return nil
    end
    local real_path = vim.uv.fs_realpath(path) or path
    local best, best_len = nil, -1
    for _, client in ipairs(lsp_util.get_clients_by_name("jdtls")) do
        local root = client.root_dir or (client.config and client.config.root_dir)
        if type(root) == "string" and root ~= "" then
            local real_root = vim.uv.fs_realpath(root) or root
            if (is_under(path, root) or is_under(real_path, real_root)) and #root > best_len then
                best, best_len = client, #root
            end
        end
    end
    return best
end

--- Ask jdtls to re-read the build configuration of the project that owns the buffer.
--- A build file with unsaved changes is written first: jdtls reads build files from disk.
---@param bufnr integer|nil buffer number, current buffer when nil or 0
---@return boolean sent
function M.update(bufnr)
    if bufnr == nil or bufnr == 0 then
        bufnr = vim.api.nvim_get_current_buf()
    end
    local path = vim.api.nvim_buf_get_name(bufnr)
    if path == "" then
        vim.notify("JDTLS: this buffer has no file, nothing to update", vim.log.levels.WARN)
        return false
    end
    local client = M.find_client(bufnr)
    if not client or client:is_stopped() then
        vim.notify("JDTLS: no running jdtls for " .. vim.fs.basename(path), vim.log.levels.WARN)
        return false
    end
    if M.is_build_file(path) and vim.bo[bufnr].modified then
        local ok, err = pcall(vim.api.nvim_buf_call, bufnr, function()
            vim.cmd("silent update")
        end)
        if not ok then
            vim.notify("JDTLS: could not save " .. vim.fs.basename(path) .. ": " .. tostring(err), vim.log.levels.ERROR)
            return false
        end
    end
    -- nvim-jdtls' update_project_config() sends the deprecated singular `java/projectConfigurationUpdate` as an
    -- LSP *request*, but the server implements it as a @JsonNotification -> newer lsp4j throws "Local endpoint
    -- returned null from its request method". Send the non-deprecated plural method as a notification.
    client:notify("java/projectConfigurationsUpdate", { identifiers = { { uri = vim.uri_from_bufnr(bufnr) } } })
    local deps = package.loaded["modules.java.dependencies-search"]
    if type(deps) == "table" and type(deps.reset) == "function" then
        deps.reset()
    end
    vim.notify("JDTLS: updating project configuration (" .. vim.fs.basename(path) .. ")", vim.log.levels.INFO)
    return true
end

return M
