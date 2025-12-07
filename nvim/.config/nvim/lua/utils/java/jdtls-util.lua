local M = {}

-- Split class name and line number (e.g., "java.util.List:50")
-- local class_name, line_number = arg:match("([^:]+):?(%d*)")
local list_util = require("utils.list-util")
local util = require("utils.common-util")
local lsp_util = require("utils.lsp-util")
local string_util = require("utils.string-util")

---@param class_names [string]
---@param handler function([table])
M.jdt_load_unique_class_list = function(class_names, handler)
    local all_items = {}
    class_names = list_util.to_unique_list(class_names)
    local pending = #class_names

    -- Request LSP to find the symbol
    local jdtls_client = lsp_util.get_client_by_name("jdtls")

    if not jdtls_client then
        vim.notify("⚠️ JDTLS is not connected to current buffer to resolve symbol request")
        return
    end

    -- vim.notify("✅ Jdtls items to load " .. pending)

    for _, class_name in ipairs(class_names) do
        if not class_name or class_name == "" then
            vim.notify("Please provide a class name.", vim.log.levels.WARN)
            return
        end

        -- vim.lsp.buf_request(0, "workspace/symbol", { query = class_name }, function(err, result, _, _)

        local class_name_query = vim.split(class_name, "%$")[1] -- we need only top level class, to position we have linenumber
        -- vim.notify("requesting " .. class_name_query)
        jdtls_client:request("workspace/symbol", { query = class_name_query }, function(err, result, ctx)
            pending = pending - 1

            if err then
                vim.notify("⚠️ Error: " .. tostring(err), vim.log.levels.WARN)
                return
            end
            if not result or vim.tbl_isempty(result) then
                vim.notify("⚠️ Class not found: " .. class_name_query, vim.log.levels.WARN)
                return
            end

            -- dd(result)
            -- Filter for exact matches or the best candidate (usually the first Class/Interface)
            -- Note: might add filtering if we get too many results

            -- If multiple results, try to find the one that is a Class (Kind 5) or Interface (Kind 11)
            local target = nil
            -- dd(result)
            if #result > 1 then
                local class_package, simple_class_name = string_util.split_by_last_dot(class_name)

                local single_result = list_util.findFirst(result, function(item)
                    return string_util.starts_with(item.containerName, class_package) and item.name == simple_class_name
                end)

                if single_result then
                    target = single_result
                else
                    vim.notify(
                        "⚠️ Found more than one lsp symbols, and not found unique by qualifier, first will be picked"
                    )
                    target = result[1]
                end
            else
                target = result[1]
            end
            target.name = class_name

            -- table.insert(all_items, target)
            all_items[class_name] = target

            if pending == 0 then
                -- vim.notify("✅ Jdtls items successfully loaded " .. #all_items)
                handler(all_items)
            end
        end)
    end
end

---@param class_name string
---@param handler function(table)
M.jdt_load_unique_class = function(class_name, handler)
    M.jdt_load_unique_class_list({ class_name }, function(responseList)
        handler(responseList[class_name])
    end)
end

M.jdt_open_class = function(class_name, line_number)
    if not class_name or class_name == "" then
        vim.notify("Please provide a class name.", vim.log.levels.WARN)
        return
    end
    line_number = line_number or 0

    -- vim.lsp.buf_request(0, "workspace/symbol", { query = class_name }, function(err, result, _, _)
    M.jdt_load_unique_class(class_name, function(result)
        -- Open the file and jump to the position
        vim.lsp.util.show_document(result.location, "utf-8", { focus = true })

        -- Open the file (JDTLS handles the jdt:// URI automatically)
        -- local uri = target.location.uri or target.uri
        -- require("jdtls").open_classfile(uri)

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

M.parse_jdt_link = function(encoded_jdt_link)
    if not encoded_jdt_link then
        return nil, nil
    end

    -- 2. Decode URL encoding
    local decoded = encoded_jdt_link:gsub("%%(%x%x)", function(hex)
        return string.char(tonumber(hex, 16))
    end)
    decoded = decoded:gsub("%(", ".")
    -- decoded now looks like: <java.lang(Object.class#268

    -- local class_full, line = decoded:match("<([%w%._]+[^#]+)#(%d+)")
    local class_full, line = decoded:match("<([^#]+)#(%d+)")
    class_full = class_full:gsub(".class", "")

    return { class_name = class_full, line_number = tonumber(line) }
end

local jdt_endoced_pattern = "%/%%3C[^%s)]+"
local extract_jdt_first_link = function(jdt_link_text)
    -- 1. Find the encoded tail: %3Cjava.lang%28Object.class#268
    local encoded = jdt_link_text:match(jdt_endoced_pattern)
    return M.parse_jdt_link(encoded)
end

local extract_jdt_all_links = function(jdt_link_text)
    local results = {}
    for encoded in jdt_link_text:gmatch(jdt_endoced_pattern) do
        local result = M.parse_jdt_link(encoded)
        if result then
            table.insert(results, result)
        end
    end
    return results
end

M.extrace_and_open_first_jdt_link = function(line)
    local extracted = extract_jdt_first_link(line)
    if not extracted then
        vim.notify(string.format("⚠️ Can't extract class name with line from %s", line))
        return
    end
    vim.cmd("wincmd k | l")
    M.jdt_open_class(extracted.class_name, extracted.line_number)
end

M.extrace_and_open_all_jdt_link = function(line)
    local extracted = extract_jdt_all_links(line)
    if not extracted or vim.tbl_isempty(extracted) then
        vim.notify(string.format("⚠️ Can't extrct any class name with line from %s", line))
        return
    end
    vim.cmd("wincmd k | l")
    for _, value in ipairs(extracted) do
        M.jdt_open_class(value.class_name, value.line_number)
    end
end

M.extrace_and_open_current_line_first_jdt_link = function()
    local current_line = util.get_line_under_cursor()
    M.extrace_and_open_first_jdt_link(current_line)
end

M.extrace_and_open_current_line_all_jdt_link = function()
    local current_line = util.get_line_under_cursor()
    M.extrace_and_open_all_jdt_link(current_line)
end

M.extrace_and_open_cursor_position_jdt_link = function()
    local cursor_token = util.get_token_under_cursor()
    M.extrace_and_open_first_jdt_link(cursor_token)
end

-- M.jdt_open_class = function(class_name, line_number)
--     if not class_name or class_name == "" then
--         vim.notify("Please provide a class name.", vim.log.levels.WARN)
--         return
--     end
--     line_number = line_number or 0
--
--     -- Request LSP to find the symbol
--     local jdtls_client = lsp_util.get_client_by_name("jdtls")
--
--     if not jdtls_client then
--         vim.notify("⚠️ JDTLS is not connected to current buffer to resolve symbol request")
--         return
--     end
--
--     -- vim.lsp.buf_request(0, "workspace/symbol", { query = class_name }, function(err, result, _, _)
--     jdtls_client:request("workspace/symbol", { query = class_name }, function(err, result, ctx)
--         if err then
--             vim.notify("Error: " .. tostring(err), vim.log.levels.WARN)
--             return
--         end
--         if not result or vim.tbl_isempty(result) then
--             vim.notify("Class not found: " .. class_name, vim.log.levels.WARN)
--             return
--         end
--
--         -- dd(result)
--         -- Filter for exact matches or the best candidate (usually the first Class/Interface)
--         -- Note: might add filtering if we get too many results
--
--         -- If multiple results, try to find the one that is a Class (Kind 5) or Interface (Kind 11)
--         local target = nil
--         -- dd(result)
--         if #result > 1 then
--             local single_result = list_util.find_by(result, "containerName", class_name)
--             if single_result then
--                 target = single_result
--             else
--                 vim.notify("⚠️ Found more than one lsp symbols, first will be picked")
--                 target = result[1]
--             end
--
--             -- vim.notify("⚠️ Found more than one lsp symbols, first will be picked")
--             -- dd(result)
--             -- for _, symbol in ipairs(result) do
--             --     if symbol.kind == 5 or symbol.kind == 11 then
--             --         target = symbol
--             --         break
--             --     end
--             -- end
--
--             -- Snacks.picker.lsp_workspace_symbols({
--             --     search = "HashMap",
--             --     on_close = function(picker)
--             --         dd(picker:selected())
--             --     end,
--             -- })
--         else
--             target = result[1]
--         end
--
--         -- Open the file and jump to the position
--         vim.lsp.util.show_document(target.location, "utf-8", { focus = true })
--
--         -- Open the file (JDTLS handles the jdt:// URI automatically)
--         -- local uri = target.location.uri or target.uri
--         -- require("jdtls").open_classfile(uri)
--
--         -- Jump to line number if provided
--         if line_number and line_number ~= "" then
--             local line = tonumber(line_number)
--             vim.defer_fn(function()
--                 pcall(vim.api.nvim_win_set_cursor, 0, { line, 0 })
--                 vim.cmd("normal! zz") -- Center the screen
--             end, 10)
--         end
--     end)
-- end

--- Find word under curser in lsp dynamic_workspace_symbols
M.connect_jdtls_and_search_symbol_under_cursor = function()
    local jdtls_client_id = lsp_util.get_client_id_by_name("jdtls")
    if jdtls_client_id then
        local current_buf_id = vim.api.nvim_get_current_buf()
        if not vim.lsp.buf_is_attached(current_buf_id, jdtls_client_id) then
            vim.lsp.buf_attach_client(current_buf_id, jdtls_client_id)

            LazyVim.info("jdtls client found by ID:" .. jdtls_client_id)
            LazyVim.info("attaching jdtls to current buffer by ID:" .. current_buf_id)
        end
    end

    local fileName = util.get_file_with_no_ext()
    LazyVim.info("fileName:" .. fileName)

    -- require("telescope.builtin").lsp_dynamic_workspace_symbols({
    --     symbols = LazyVim.config.get_kind_filter(),
    --     default_text = fileName,
    -- })

    Snacks.picker.lsp_workspace_symbols({
        search = fileName,
    })
end

--[[ local open_jdt_link = function(uri)
    local client
    for _, c in ipairs(vim.lsp.get_active_clients()) do
        if
            c.config.init_options
            and c.config.init_options.extendedClientCapabilities
            and c.config.init_options.extendedClientCapabilities.classFileContentsSupport
        then
            client = c
            break
        end
    end
    assert(client, "Must have a buffer open with a language client connected to eclipse.jdt.ls to load JDT URI")
    --local buf = vim.api.nvim_get_current_buf()
    local params = {
        uri = uri,
    }
    local response = nil
    local cb = function(err, result)
        response = { err, result }
    end
    local ok, request_id = client.request("java/classFileContents", params, cb, buf)
    assert(ok, "Request to `java/classFileContents` must succeed to open JDT URI. Client shutdown?")
    local timeout_ms = 2000
    local wait_ok, reason = vim.wait(timeout_ms, function()
        return response
    end)
    local log_path = require("jdtls.path").join(vim.fn.stdpath("cache"), "lsp.log")
    local buf_content
    if wait_ok and #response == 2 and response[2] then
        local content = response[2]
        if content == "" then
            buf_content = {
                "Received response from server, but it was empty. Check the log file for errors",
                log_path,
            }
        else
            buf_content = vim.split(response[2], "\n", { plain = true })
        end
    else
        local error_msg
        if not wait_ok then
            client.cancel_request(request_id)
            local wait_failure = {
                [-1] = "timeout",
                [-2] = "interrupted",
                [-3] = "error",
            }
            error_msg = wait_failure[reason]
        else
            error_msg = response[1]
        end
        buf_content = {
            "Failed to load content for uri",
            uri,
            "",
            "Error was: ",
        }
        vim.list_extend(buf_content, vim.split(vim.inspect(error_msg), "\n"))
        vim.list_extend(buf_content, { "", "Check the log file for errors", log_path })
    end

    vim.cmd("enew")
    local buf = vim.api.nvim_get_current_buf()
    vim.bo[buf].modifiable = true
    vim.bo[buf].swapfile = false
    vim.bo[buf].buftype = "nofile"
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, buf_content)
    vim.bo[buf].filetype = "java"
    vim.bo[buf].modifiable = false
end ]]

return M