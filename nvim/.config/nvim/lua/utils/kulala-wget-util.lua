-- Kulala wget helpers for copying HTTP requests and importing clipboard commands.

local M = {}

---@class KulalaWgetOptions
---@field insecure? boolean Force certificate verification off in the converted command.

local curl_ignored_long_flags = {
    ["--compressed"] = true,
    ["--fail"] = true,
    ["--fail-with-body"] = true,
    ["--globoff"] = true,
    ["--http1.0"] = true,
    ["--http1.1"] = true,
    ["--include"] = true,
    ["--location"] = true,
    ["--location-trusted"] = true,
    ["--no-buffer"] = true,
    ["--no-progress-meter"] = true,
    ["--path-as-is"] = true,
    ["--show-error"] = true,
    ["--silent"] = true,
    ["--verbose"] = true,
}

local curl_ignored_long_values = {
    ["--connect-timeout"] = true,
    ["--cookie-jar"] = true,
    ["--dump-header"] = true,
    ["--max-redirs"] = true,
    ["--max-time"] = true,
    ["--output"] = true,
    ["--retry"] = true,
    ["--retry-delay"] = true,
    ["--retry-max-time"] = true,
    ["--speed-limit"] = true,
    ["--speed-time"] = true,
    ["--write-out"] = true,
}

local wget_ignored_long_flags = {
    ["--auth-no-challenge"] = true,
    ["--content-disposition"] = true,
    ["--content-on-error"] = true,
    ["--continue"] = true,
    ["--debug"] = true,
    ["--delete-after"] = true,
    ["--https-only"] = true,
    ["--ignore-length"] = true,
    ["--keep-session-cookies"] = true,
    ["--no-cache"] = true,
    ["--no-clobber"] = true,
    ["--no-cookies"] = true,
    ["--no-hsts"] = true,
    ["--no-http-keep-alive"] = true,
    ["--no-netrc"] = true,
    ["--no-verbose"] = true,
    ["--quiet"] = true,
    ["--server-response"] = true,
    ["--spider"] = true,
    ["--verbose"] = true,
}

local wget_ignored_long_values = {
    ["--append-output"] = true,
    ["--compression"] = true,
    ["--connect-timeout"] = true,
    ["--directory-prefix"] = true,
    ["--dns-timeout"] = true,
    ["--local-encoding"] = true,
    ["--max-redirect"] = true,
    ["--output-document"] = true,
    ["--output-file"] = true,
    ["--read-timeout"] = true,
    ["--remote-encoding"] = true,
    ["--retry-on-http-error"] = true,
    ["--secure-protocol"] = true,
    ["--timeout"] = true,
    ["--tries"] = true,
    ["--wait"] = true,
    ["--waitretry"] = true,
}

--- Quote one shell word using POSIX single-quote syntax.
---@param value string
---@return string
local function shell_quote(value)
    return "'" .. value:gsub("'", "'\\''") .. "'"
end

--- Split a shell command without evaluating substitutions or executing anything.
---@param command string
---@return string[]|nil words
---@return string|nil err
local function shell_words(command)
    if type(command) ~= "string" or not command:find("%S") then
        return nil, "Clipboard does not contain a command"
    end

    local words = {}
    local current = {}
    local quote
    local token_started = false
    local index = 1

    while index <= #command do
        local char = command:sub(index, index)

        if quote == "'" then
            if char == "'" then
                quote = nil
            else
                table.insert(current, char)
            end
            index = index + 1
        elseif quote == '"' then
            if char == '"' then
                quote = nil
                index = index + 1
            elseif char == "\\" then
                local next_char = command:sub(index + 1, index + 1)
                if next_char == "" then
                    return nil, "Trailing backslash in command"
                elseif next_char == "\n" then
                    index = index + 2
                elseif next_char == '"' or next_char == "\\" or next_char == "$" or next_char == "`" then
                    table.insert(current, next_char)
                    index = index + 2
                else
                    table.insert(current, char)
                    table.insert(current, next_char)
                    index = index + 2
                end
            else
                table.insert(current, char)
                index = index + 1
            end
        elseif char == "'" or char == '"' then
            quote = char
            token_started = true
            index = index + 1
        elseif char == "\\" then
            local next_char = command:sub(index + 1, index + 1)
            if next_char == "" then
                return nil, "Trailing backslash in command"
            elseif next_char == "\n" then
                index = index + 2
            else
                table.insert(current, next_char)
                token_started = true
                index = index + 2
            end
        elseif char:match("%s") then
            if token_started then
                table.insert(words, table.concat(current))
                current = {}
                token_started = false
            end
            index = index + 1
        elseif char == "#" and not token_started then
            local newline = command:find("\n", index + 1, true)
            index = newline and newline + 1 or #command + 1
        elseif char:match("[;&|<>]") then
            return nil, "Shell operators are not supported in imported commands"
        else
            table.insert(current, char)
            token_started = true
            index = index + 1
        end
    end

    if quote then
        return nil, "Unterminated quote in command"
    end
    if token_started then
        table.insert(words, table.concat(current))
    end
    if #words == 0 then
        return nil, "Clipboard does not contain a command"
    end

    return words
end

--- Return the lowercase executable basename without a Windows suffix.
---@param path string
---@return string
local function executable_name(path)
    return (path:match("([^/]+)$") or path):lower():gsub("%.exe$", "")
end

--- Find the first argument after an expected curl or wget executable.
---@param words string[]
---@param expected "curl"|"wget"
---@return integer|nil first_argument
---@return string|nil err
local function command_arguments_start(words, expected)
    local index = 1
    if words[index] == "$" or words[index] == "%" then
        index = index + 1
    end
    if executable_name(words[index] or "") == "command" then
        index = index + 1
    end

    local executable = executable_name(words[index] or "")
    if expected == "wget" and executable == "busybox" and executable_name(words[index + 1] or "") == "wget" then
        return index + 2
    end
    if executable ~= expected then
        return nil, ("Expected a %s command, got %s"):format(expected, words[index] or "nothing")
    end
    return index + 1
end

--- Split a GNU-style long option into its name and optional inline value.
---@param word string
---@return string option
---@return string|nil inline_value
local function split_long_option(word)
    local equals = word:find("=", 1, true)
    if not equals then
        return word, nil
    end
    return word:sub(1, equals - 1), word:sub(equals + 1)
end

--- Read an inline or following option value.
---@param words string[]
---@param index integer
---@param inline_value string|nil
---@param option string
---@return string|nil value
---@return integer next_index
---@return string|nil err
local function option_value(words, index, inline_value, option)
    if inline_value ~= nil then
        return inline_value, index
    end
    if words[index + 1] == nil then
        return nil, index, option .. " requires a value"
    end
    return words[index + 1], index + 1
end

--- Store a single request body, distinguishing literal data from @file input.
---@param request table
---@param value string
---@param allow_file boolean
---@return string|nil err
local function set_body(request, value, allow_file)
    if request.body_data ~= nil or request.body_file ~= nil then
        return "Multiple request bodies are not supported"
    end

    if allow_file and value:sub(1, 1) == "@" then
        request.body_file = value:sub(2)
    else
        request.body_data = value
    end
    return nil
end

--- Check whether a line contains Kulala's insecure cURL metadata.
---@param line string
---@return boolean
local function is_insecure_metadata(line)
    local metadata = line:match("^%s*#%s*(.-)%s*$") or line:match("^%s*//%s*(.-)%s*$")
    return metadata == "@curl-insecure"
end

--- Check a line range for Kulala's insecure cURL metadata.
---@param lines string[]
---@param first integer
---@param last integer
---@return boolean
local function range_is_insecure(lines, first, last)
    for index = first, last do
        if is_insecure_metadata(lines[index] or "") then
            return true
        end
    end
    return false
end

--- Detect insecure metadata in the current request or an earlier Shared block.
---@return boolean
local function current_request_is_insecure()
    local lines = vim.api.nvim_buf_get_lines(vim.api.nvim_get_current_buf(), 0, -1, false)
    if #lines == 0 then
        return false
    end

    local blocks = {}
    local block_start = 1
    local block_name
    for index, line in ipairs(lines) do
        local name = line:match("^%s*###%s*(.-)%s*$")
        if name ~= nil then
            if index > block_start then
                table.insert(blocks, {
                    first = block_start,
                    last = index - 1,
                    name = block_name,
                })
            end
            block_start = index
            block_name = name
        end
    end
    table.insert(blocks, {
        first = block_start,
        last = #lines,
        name = block_name,
    })

    local cursor_line = vim.api.nvim_win_get_cursor(0)[1]
    local current_block_index = #blocks
    for index, block in ipairs(blocks) do
        if cursor_line >= block.first and cursor_line <= block.last then
            current_block_index = index
            break
        end
    end

    local current_block = blocks[current_block_index]
    if range_is_insecure(lines, current_block.first, current_block.last) then
        return true
    end

    for index = 1, current_block_index - 1 do
        local block = blocks[index]
        local name = (block.name or ""):lower()
        if (name == "shared" or name == "shared each") and range_is_insecure(lines, block.first, block.last) then
            return true
        end
    end
    return false
end

--- Insert Kulala's insecure metadata before the imported request line.
---@param lines string[]
local function add_insecure_metadata(lines)
    for _, line in ipairs(lines) do
        if is_insecure_metadata(line) then
            return
        end
    end

    local insert_index = 1
    while insert_index <= #lines do
        local line = lines[insert_index]
        if not line:match("^%s*$") and not line:match("^%s*#") and not line:match("^%s*//") then
            break
        end
        insert_index = insert_index + 1
    end
    table.insert(lines, insert_index, "# @curl-insecure")
end

--- Parse the request-relevant portion of a cURL command.
---@param words string[]
---@param start_index integer
---@return table|nil request
---@return string|nil err
local function parse_curl_words(words, start_index)
    local request = {
        headers = {},
    }
    local index = start_index
    local positional_only = false

    while index <= #words do
        local word = words[index]

        if positional_only then
            if request.url then
                return nil, "Multiple cURL URLs are not supported"
            end
            request.url = word
        elseif word == "--" then
            positional_only = true
        elseif word:sub(1, 2) == "--" then
            local option, inline_value = split_long_option(word)
            local value
            local err

            if option == "--request" then
                value, index, err = option_value(words, index, inline_value, option)
                request.method = value
            elseif option == "--header" then
                value, index, err = option_value(words, index, inline_value, option)
                if value then
                    table.insert(request.headers, value)
                end
            elseif option == "--user-agent" then
                value, index, err = option_value(words, index, inline_value, option)
                request.user_agent = value
            elseif option == "--cookie" then
                value, index, err = option_value(words, index, inline_value, option)
                request.cookie = value
            elseif option == "--data" or option == "--data-ascii" or option == "--data-binary" then
                value, index, err = option_value(words, index, inline_value, option)
                if value and not err then
                    err = set_body(request, value, true)
                end
            elseif option == "--data-raw" then
                value, index, err = option_value(words, index, inline_value, option)
                if value and not err then
                    err = set_body(request, value, false)
                end
            elseif option == "--json" then
                value, index, err = option_value(words, index, inline_value, option)
                if value and not err then
                    err = set_body(request, value, true)
                    table.insert(request.headers, "Content-Type: application/json")
                    table.insert(request.headers, "Accept: application/json")
                end
            elseif option == "--url" then
                value, index, err = option_value(words, index, inline_value, option)
                if request.url then
                    return nil, "Multiple cURL URLs are not supported"
                end
                request.url = value
            elseif option == "--user" then
                value, index, err = option_value(words, index, inline_value, option)
                request.user = value
            elseif option == "--referer" then
                value, index, err = option_value(words, index, inline_value, option)
                request.referer = value
            elseif option == "--cert" then
                value, index, err = option_value(words, index, inline_value, option)
                request.certificate = value
            elseif option == "--key" then
                value, index, err = option_value(words, index, inline_value, option)
                request.private_key = value
            elseif option == "--cacert" then
                value, index, err = option_value(words, index, inline_value, option)
                request.ca_certificate = value
            elseif option == "--upload-file" then
                value, index, err = option_value(words, index, inline_value, option)
                if value and not err then
                    err = set_body(request, value, false)
                    request.body_file = request.body_data
                    request.body_data = nil
                    request.method = request.method or "PUT"
                end
            elseif option == "--oauth2-bearer" then
                value, index, err = option_value(words, index, inline_value, option)
                if value then
                    table.insert(request.headers, "Authorization: Bearer " .. value)
                end
            elseif option == "--head" then
                request.method = "HEAD"
            elseif option == "--insecure" then
                request.insecure = true
            elseif option == "--http2" or option == "--http3" or option == "--http3-only" then
                return nil, option .. " cannot be represented by GNU wget"
            elseif option == "--form" or option == "--form-string" or option == "--data-urlencode" then
                return nil, option .. " conversion is not supported"
            elseif curl_ignored_long_values[option] then
                _, index, err = option_value(words, index, inline_value, option)
            elseif not curl_ignored_long_flags[option] then
                return nil, "Unsupported cURL option for wget conversion: " .. option
            end

            if err then
                return nil, err
            end
        elseif word:sub(1, 1) == "-" and word ~= "-" then
            local option = word:sub(1, 2)
            local inline_value = #word > 2 and word:sub(3) or nil
            local value
            local err

            if option == "-X" then
                value, index, err = option_value(words, index, inline_value, option)
                request.method = value
            elseif option == "-H" then
                value, index, err = option_value(words, index, inline_value, option)
                if value then
                    table.insert(request.headers, value)
                end
            elseif option == "-A" then
                value, index, err = option_value(words, index, inline_value, option)
                request.user_agent = value
            elseif option == "-b" then
                value, index, err = option_value(words, index, inline_value, option)
                request.cookie = value
            elseif option == "-d" then
                value, index, err = option_value(words, index, inline_value, option)
                if value and not err then
                    err = set_body(request, value, true)
                end
            elseif option == "-u" then
                value, index, err = option_value(words, index, inline_value, option)
                request.user = value
            elseif option == "-e" then
                value, index, err = option_value(words, index, inline_value, option)
                request.referer = value
            elseif option == "-T" then
                value, index, err = option_value(words, index, inline_value, option)
                if value and not err then
                    err = set_body(request, value, false)
                    request.body_file = request.body_data
                    request.body_data = nil
                    request.method = request.method or "PUT"
                end
            elseif option == "-o" or option == "-m" or option == "-w" or option == "-c" or option == "-D" then
                _, index, err = option_value(words, index, inline_value, option)
            elseif word == "-I" then
                request.method = "HEAD"
            elseif word == "-k" then
                request.insecure = true
            elseif word:match("^%-[vsSLfgNiOq0]+$") then
                -- These options only affect cURL output or transport behavior.
            elseif option == "-F" then
                return nil, "-F conversion is not supported"
            else
                return nil, "Unsupported cURL option for wget conversion: " .. word
            end

            if err then
                return nil, err
            end
        else
            if request.url then
                return nil, "Multiple cURL URLs are not supported"
            end
            request.url = word
        end

        index = index + 1
    end

    if not request.url or request.url == "" then
        return nil, "cURL command does not contain a URL"
    end
    if request.body_data ~= nil or request.body_file ~= nil then
        request.method = request.method or "POST"
    end

    return request
end

--- Render a parsed cURL request as a GNU wget command.
---@param request table
---@return string
local function render_wget(request)
    local parts = {
        "wget",
        "--quiet",
        "--output-document=-",
    }

    if request.method then
        table.insert(parts, "--method=" .. shell_quote(request.method))
    end
    for _, header in ipairs(request.headers) do
        table.insert(parts, "--header=" .. shell_quote(header))
    end
    if request.cookie then
        table.insert(parts, "--header=" .. shell_quote("Cookie: " .. request.cookie))
    end
    if request.user_agent then
        table.insert(parts, "--user-agent=" .. shell_quote(request.user_agent))
    end
    if request.referer then
        table.insert(parts, "--referer=" .. shell_quote(request.referer))
    end
    if request.user then
        local username, password = request.user:match("^([^:]*):(.*)$")
        table.insert(parts, "--user=" .. shell_quote(username or request.user))
        if password then
            table.insert(parts, "--password=" .. shell_quote(password))
        end
    end
    if request.insecure then
        table.insert(parts, "--no-check-certificate")
    end
    if request.certificate then
        table.insert(parts, "--certificate=" .. shell_quote(request.certificate))
    end
    if request.private_key then
        table.insert(parts, "--private-key=" .. shell_quote(request.private_key))
    end
    if request.ca_certificate then
        table.insert(parts, "--ca-certificate=" .. shell_quote(request.ca_certificate))
    end
    if request.body_file then
        table.insert(parts, "--body-file=" .. shell_quote(request.body_file))
    elseif request.body_data ~= nil then
        table.insert(parts, "--body-data=" .. shell_quote(request.body_data))
    end
    table.insert(parts, shell_quote(request.url))

    return table.concat(parts, " ")
end

--- Parse the request-relevant portion of a GNU wget command.
---@param words string[]
---@param start_index integer
---@return table|nil request
---@return string|nil err
local function parse_wget_words(words, start_index)
    local request = {
        headers = {},
    }
    local index = start_index
    local positional_only = false

    while index <= #words do
        local word = words[index]

        if positional_only then
            if request.url then
                return nil, "Multiple wget URLs are not supported"
            end
            request.url = word
        elseif word == "--" then
            positional_only = true
        elseif word:sub(1, 2) == "--" then
            local option, inline_value = split_long_option(word)
            local value
            local err

            if option == "--method" then
                value, index, err = option_value(words, index, inline_value, option)
                request.method = value
            elseif option == "--header" then
                value, index, err = option_value(words, index, inline_value, option)
                if value then
                    table.insert(request.headers, value)
                end
            elseif option == "--body-data" then
                value, index, err = option_value(words, index, inline_value, option)
                if value and not err then
                    err = set_body(request, value, false)
                end
            elseif option == "--body-file" then
                value, index, err = option_value(words, index, inline_value, option)
                if value and not err then
                    err = set_body(request, "@" .. value, true)
                end
            elseif option == "--post-data" then
                value, index, err = option_value(words, index, inline_value, option)
                if value and not err then
                    err = set_body(request, value, false)
                    request.method = request.method or "POST"
                end
            elseif option == "--post-file" then
                value, index, err = option_value(words, index, inline_value, option)
                if value and not err then
                    err = set_body(request, "@" .. value, true)
                    request.method = request.method or "POST"
                end
            elseif option == "--user-agent" then
                value, index, err = option_value(words, index, inline_value, option)
                request.user_agent = value
            elseif option == "--referer" then
                value, index, err = option_value(words, index, inline_value, option)
                request.referer = value
            elseif option == "--user" or option == "--http-user" then
                value, index, err = option_value(words, index, inline_value, option)
                request.user = value
            elseif option == "--password" or option == "--http-password" then
                value, index, err = option_value(words, index, inline_value, option)
                request.password = value
            elseif option == "--proxy-user" then
                value, index, err = option_value(words, index, inline_value, option)
                request.proxy_user = value
            elseif option == "--proxy-password" then
                value, index, err = option_value(words, index, inline_value, option)
                request.proxy_password = value
            elseif option == "--load-cookies" then
                value, index, err = option_value(words, index, inline_value, option)
                request.cookie_file = value
            elseif option == "--save-cookies" then
                _, index, err = option_value(words, index, inline_value, option)
            elseif option == "--certificate" then
                value, index, err = option_value(words, index, inline_value, option)
                request.certificate = value
            elseif option == "--private-key" then
                value, index, err = option_value(words, index, inline_value, option)
                request.private_key = value
            elseif option == "--ca-certificate" then
                value, index, err = option_value(words, index, inline_value, option)
                request.ca_certificate = value
            elseif option == "--no-check-certificate" then
                request.insecure = true
            elseif wget_ignored_long_values[option] then
                _, index, err = option_value(words, index, inline_value, option)
            elseif not wget_ignored_long_flags[option] then
                return nil, "Unsupported wget option for Kulala import: " .. option
            end

            if err then
                return nil, err
            end
        elseif word:sub(1, 1) == "-" and word ~= "-" then
            local is_ignored_compound = word == "-nv" or word == "-nc" or word == "-nd" or word == "-nH"
            if not is_ignored_compound then
                local position = 2
                while position <= #word do
                    local option = word:sub(position, position)
                    if option == "q" or option == "S" or option == "d" or option == "v" then
                        position = position + 1
                    elseif
                        option == "O"
                        or option == "o"
                        or option == "a"
                        or option == "t"
                        or option == "T"
                        or option == "P"
                        or option == "U"
                    then
                        local inline_value = position < #word and word:sub(position + 1) or nil
                        local value
                        local err
                        value, index, err = option_value(words, index, inline_value, "-" .. option)
                        if err then
                            return nil, err
                        end
                        if option == "U" then
                            request.user_agent = value
                        end
                        position = #word + 1
                    elseif option == "4" or option == "6" or option == "b" or option == "c" or option == "N" then
                        position = position + 1
                    else
                        return nil, "Unsupported wget option for Kulala import: -" .. option
                    end
                end
            end
        else
            if request.url then
                return nil, "Multiple wget URLs are not supported"
            end
            request.url = word
        end

        index = index + 1
    end

    if not request.url or request.url == "" then
        return nil, "wget command does not contain a URL"
    end
    if request.body_data ~= nil or request.body_file ~= nil then
        if not request.method then
            return nil, "wget --body-data/--body-file requires --method"
        end
    end
    if request.password and not request.user then
        return nil, "wget password was provided without a user"
    end

    return request
end

--- Render a parsed wget request as a cURL command accepted by Kulala.
---@param request table
---@return string
local function render_curl(request)
    local parts = { "curl" }

    if request.method then
        table.insert(parts, "-X")
        table.insert(parts, shell_quote(request.method))
    end
    for _, header in ipairs(request.headers) do
        table.insert(parts, "-H")
        table.insert(parts, shell_quote(header))
    end
    if request.body_file then
        table.insert(parts, "--data-binary")
        table.insert(parts, shell_quote("@" .. request.body_file))
    elseif request.body_data ~= nil then
        table.insert(parts, "--data-binary")
        table.insert(parts, shell_quote(request.body_data))
    end
    if request.user_agent then
        table.insert(parts, "-A")
        table.insert(parts, shell_quote(request.user_agent))
    end
    if request.referer then
        table.insert(parts, "--referer")
        table.insert(parts, shell_quote(request.referer))
    end
    if request.user then
        table.insert(parts, "--user")
        table.insert(parts, shell_quote(request.user .. ":" .. (request.password or "")))
    end
    if request.proxy_user then
        table.insert(parts, "--proxy-user")
        table.insert(parts, shell_quote(request.proxy_user .. ":" .. (request.proxy_password or "")))
    end
    if request.cookie_file then
        table.insert(parts, "--cookie")
        table.insert(parts, shell_quote(request.cookie_file))
    end
    if request.insecure then
        table.insert(parts, "--insecure")
    end
    if request.certificate then
        table.insert(parts, "--cert")
        table.insert(parts, shell_quote(request.certificate))
    end
    if request.private_key then
        table.insert(parts, "--key")
        table.insert(parts, shell_quote(request.private_key))
    end
    if request.ca_certificate then
        table.insert(parts, "--cacert")
        table.insert(parts, shell_quote(request.ca_certificate))
    end
    table.insert(parts, shell_quote(request.url))

    return table.concat(parts, " ")
end

--- Convert one cURL command into an equivalent GNU wget command.
---@param command string
---@param opts? KulalaWgetOptions
---@return string|nil wget
---@return string|nil err
function M.curl_to_wget(command, opts)
    local words, err = shell_words(command)
    if not words then
        return nil, err
    end
    local start_index
    start_index, err = command_arguments_start(words, "curl")
    if not start_index then
        return nil, err
    end
    local request
    request, err = parse_curl_words(words, start_index)
    if not request then
        return nil, err
    end
    if opts and opts.insecure then
        request.insecure = true
    end
    return render_wget(request)
end

--- Convert one GNU wget command into a cURL command accepted by Kulala.
---@param command string
---@param opts? KulalaWgetOptions
---@return string|nil curl
---@return string|nil err
---@return { insecure: boolean }|nil metadata
function M.wget_to_curl(command, opts)
    local words, err = shell_words(command)
    if not words then
        return nil, err
    end
    local start_index
    start_index, err = command_arguments_start(words, "wget")
    if not start_index then
        return nil, err
    end
    local request
    request, err = parse_wget_words(words, start_index)
    if not request then
        return nil, err
    end
    if opts and opts.insecure then
        request.insecure = true
    end
    return render_curl(request), nil, { insecure = request.insecure == true }
end

--- Copy the Kulala request under the cursor as a GNU wget command.
---@param opts? KulalaWgetOptions
function M.copy_as_wget(opts)
    local bridge = require("kulala.cmd.kulala_core_bridge")
    local logger = require("kulala.logger")
    if not bridge.enabled() then
        return logger.error("kulala-core is not available")
    end

    local globals = require("kulala.globals")
    local curl, err = bridge.to_curl_at_cursor(nil, globals.NAME .. "/" .. globals.VERSION)
    if not curl then
        return logger.error(err or "Failed to copy request as wget")
    end

    local wget
    local insecure = (opts and opts.insecure) or current_request_is_insecure()
    wget, err = M.curl_to_wget(curl, { insecure = insecure })
    if not wget then
        return logger.error(err or "Failed to convert request to wget")
    end

    vim.fn.setreg("+", wget)
    logger.info("Copied wget command to clipboard")
end

--- Parse a GNU wget command from the clipboard into the current HTTP buffer.
---@param opts? KulalaWgetOptions
function M.from_wget(opts)
    local logger = require("kulala.logger")
    local curl, err, metadata = M.wget_to_curl(vim.fn.getreg("+"), opts)
    if not curl then
        return logger.error(err or "Failed to convert wget command")
    end

    local bridge = require("kulala.cmd.kulala_core_bridge")
    local lines
    lines, err = bridge.from_curl(curl)
    if not lines then
        return logger.error(err or "kulala-core from_curl failed")
    end
    if metadata and metadata.insecure then
        add_insecure_metadata(lines)
    end
    vim.api.nvim_put(lines, "l", false, false)
end

return M
