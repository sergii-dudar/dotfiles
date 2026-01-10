local M = {}

M.current_java_package = function()
    local file = vim.api.nvim_buf_get_name(0)
    -- local file =
    --     "/home/serhii/serhii.home/git/tests/serhii-application/src/main/java/ua/serhii/application/mapper/data/UserMapper.java"

    local src = file:match("src/.*/java/(.*)")
    if not src then
        return ""
    end

    src = src:gsub("/[^/]+%.java$", "")
    local pkg, _ = src:gsub("/", ".")

    if pkg ~= "" then
        return "package " .. pkg .. ";"
    end
    return ""
end

M.current_java_file_name = function()
    local file = vim.api.nvim_buf_get_name(0)
    local name = vim.fn.fnamemodify(file, ":t:r")

    if not name then
        return ""
    end

    return name .. " "
end

return M
