local M = {}

-- Split class name and line number (e.g., "java.util.List:50")
-- local class_name, line_number = arg:match("([^:]+):?(%d*)")

M.jdt_open_class = function(class_name, line_number)
    if not class_name or class_name == "" then
        vim.notify("Please provide a class name.", vim.log.levels.WARN)
        return
    end
    line_number = line_number or 0

    -- Request LSP to find the symbol
    vim.lsp.buf_request(0, "workspace/symbol", { query = class_name }, function(err, result, _, _)
        if err then
            vim.notify("Error: " .. tostring(err), vim.log.levels.WARN)
            return
        end
        if not result or vim.tbl_isempty(result) then
            vim.notify("Class not found: " .. class_name, vim.log.levels.WARN)
            return
        end

        -- Filter for exact matches or the best candidate (usually the first Class/Interface)
        -- Note: You might want to refine filtering if you get too many results
        local target = result[1]

        -- If multiple results, try to find the one that is a Class (Kind 5) or Interface (Kind 11)
        for _, symbol in ipairs(result) do
            if symbol.kind == 5 or symbol.kind == 11 then
                target = symbol
                break
            end
        end

        -- Open the file (JDTLS handles the jdt:// URI automatically)

        -- dd(target)
        -- vim.notify("Opening " .. uri, vim.log.levels.INFO)
        -- vim.cmd("edit " .. vim.uri_to_fname(uri))

        -- vim.cmd("edit [D]" .. class_name .. ".class")
        -- vim.cmd("edit [D]" .. class_name .. ".java")
        -- vim.cmd("edit " .. vim.uri_to_fname(uri))
        local uri = target.location.uri or target.uri
        vim.cmd("edit " .. uri)
        require("jdtls").open_classfile(uri)

        -- Jump to line number if provided
        if line_number and line_number ~= "" then
            local line = tonumber(line_number)
            vim.defer_fn(function()
                pcall(vim.api.nvim_win_set_cursor, 0, { line, 0 })
                vim.cmd("normal! zz") -- Center the screen
            end, 10)
        end
    end)
end

return M
