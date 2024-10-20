-- Function to check if the import already exists
local function import_exists(import_statement)
    for _, line in ipairs(vim.api.nvim_buf_get_lines(0, 0, -1, false)) do
        if line:match(import_statement) then
            return true
        end
    end
    return false
end

-- Function to insert the import if it doesn't exist
local function insert_import(import_statement)
    if not import_exists(import_statement) then
        -- Find the correct place to insert the import
        local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
        local insert_line = 0
        for i, line in ipairs(lines) do
            if line:match("^package ") then
                insert_line = i + 1
                break
            elseif line:match("^import ") then
                insert_line = i
            end
        end
        -- Insert the import
        vim.api.nvim_buf_set_lines(0, insert_line, insert_line, false, { import_statement })
    end
end

-- Function to replace the class under cursor
local function replace_full_to_simple_class_name(full_class, simple_class)
    local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
    for i, line in ipairs(lines) do
        lines[i] = string.gsub(line, full_class, simple_class)
    end
    vim.api.nvim_buf_set_lines(0, 0, -1, false, lines)
end

-- Import java class name under cursor, and apply simple class name in all buffer
local function import_class_and_replace()
    local simple_class_name = vim.fn.expand('<cword>')
    local full_name_under_cursor = vim.fn.expand("<cWORD>")

    --local remove_all_part = full_name_under_cursor:match("^(.*)%." .. simple_class_name)
    local remove_all_part = full_name_under_cursor:match("^([%a%.]+)%." .. simple_class_name .. "%(?")


    if not remove_all_part then
        vim.notify('class \'' .. simple_class_name .. '\' already was imported!', vim.log.levels.INFO)
        return
    end

    local full_class_name = remove_all_part .. "." .. simple_class_name

    local import_statement = "import " .. full_class_name .. ";"

    replace_full_to_simple_class_name(full_class_name, simple_class_name)
    insert_import(import_statement)
end

local M = {}
M.import_class_and_replace = import_class_and_replace
return M