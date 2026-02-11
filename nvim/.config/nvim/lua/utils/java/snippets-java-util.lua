local M = {}

function M.current_java_package()
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

function M.current_java_file_name()
    local file = vim.api.nvim_buf_get_name(0)
    local name = vim.fn.fnamemodify(file, ":t:r")

    if not name then
        return ""
    end

    return name .. " "
end

function M.add_imports(imports)
    local skip_on_first = 0
    return function()
        if skip_on_first == 0 then
            -- to skip import on firts init and preview, to get rid of doulbe import at first call
            skip_on_first = skip_on_first + 1
            return ""
        end

        -- local imports = { "org.slf4j.LoggerFactory", "ua.serhii.application.model.User", "ua.serhii.application.mapper.data.UserMapper" }
        local first_line = vim.api.nvim_buf_get_lines(0, 0, 1, false)[1]
        if not first_line then
            return
        end
        local line_to_insert = first_line:match("^package ") and 2 or 0
        for _, import in ipairs(imports) do
            vim.api.nvim_buf_set_lines(0, 0 + line_to_insert, 0 + line_to_insert, false, { "import " .. import .. ";" })
            line_to_insert = line_to_insert + 1
        end
        return ""
    end
end

return M
