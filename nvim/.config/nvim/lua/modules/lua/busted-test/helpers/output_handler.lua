-- Slim busted output handler: emits one JSON line per test result.
-- Loaded via:  busted --output=<this-file> -Xoutput=<json-path> ...
--
-- We subscribe to busted's base events and write each completed test to the
-- output file. Output format (one JSON object per line, NDJSON):
--   {
--     "file":         "/abs/path/spec.lua",
--     "descriptions": ["outer", "inner", "it name"],
--     "status":       "success"|"failure"|"error"|"pending",
--     "duration":     0.012,
--     "message":      "...optional error message...",
--     "trace":        "...optional traceback..."
--   }
--
-- We intentionally also forward to the utfTerminal handler so the user sees
-- normal busted output in the overseer panel.

return function(options)
    local busted = require("busted")
    local handler = require("busted.outputHandlers.base")()
    local io_open = io.open

    -- Resolve output file from -Xoutput=<path>
    local output_file
    if type(options.arguments) == "table" then
        output_file = options.arguments[1]
    end
    if not output_file or output_file == "" then
        io.stderr:write("busted output_handler: no -Xoutput=<path> provided\n")
        os.exit(2)
    end

    -- Truncate / create the output file up-front so consumers always find it.
    do
        local f = io_open(output_file, "w")
        if f then
            f:close()
        end
    end

    -- Mirror terminal output so the user still sees regular busted UI in overseer.
    local utf_terminal_options = {}
    for k, v in pairs(options) do
        utf_terminal_options[k] = v
    end
    utf_terminal_options.arguments = {}
    local utf_terminal_handler = require("busted.outputHandlers.utfTerminal")(utf_terminal_options)
    utf_terminal_handler:subscribe(utf_terminal_options)

    --- Build the full descriptions chain (describe -> describe -> it).
    ---@param element table
    ---@return string[] descriptions, string file_path
    local function chain_for(element)
        local parent = busted.parent(element)
        local names = { element.name or element.descriptor }
        while parent and (parent.name or parent.descriptor) and parent.descriptor ~= "file" do
            table.insert(names, 1, parent.name or parent.descriptor)
            parent = busted.parent(parent)
        end
        local file
        if parent and parent.descriptor == "file" then
            file = parent.name or (element.trace and element.trace.source)
        else
            file = element.trace and element.trace.source
        end
        if file and file:sub(1, 1) == "@" then
            file = file:sub(2)
        end
        -- Normalize to absolute path so consumers don't depend on busted's cwd.
        if file and file:sub(1, 1) ~= "/" then
            local cwd = (os.getenv("PWD") or "")
            if cwd ~= "" then
                file = cwd .. "/" .. file
            end
        end
        return names, file
    end

    local function write_entry(entry)
        local f = io_open(output_file, "a")
        if not f then
            return
        end
        -- Minimal JSON encoder for our flat shape (use vim.json if available
        -- in a future neovim-driven invocation, but busted runs under plain
        -- lua too — so we hand-roll to stay portable).
        local function esc(s)
            s = tostring(s or "")
            s = s:gsub("\\", "\\\\"):gsub('"', '\\"'):gsub("\n", "\\n"):gsub("\r", "\\r"):gsub("\t", "\\t")
            -- Strip remaining control chars.
            s = s:gsub("[%c]", "")
            return s
        end
        local descs = {}
        for _, d in ipairs(entry.descriptions or {}) do
            table.insert(descs, '"' .. esc(d) .. '"')
        end
        f:write(
            string.format(
                '{"file":"%s","descriptions":[%s],"status":"%s","duration":%s,"message":"%s","trace":"%s"}\n',
                esc(entry.file),
                table.concat(descs, ","),
                esc(entry.status),
                tostring(entry.duration or 0),
                esc(entry.message),
                esc(entry.trace)
            )
        )
        f:close()
    end

    local function record(status, element, parent, msg, trace)
        local descs, file = chain_for(element)
        local duration = 0
        if element.duration then
            duration = element.duration
        end
        local trace_str = ""
        if trace then
            trace_str = (trace.traceback or trace.source or "") .. (trace.message and ("\n" .. trace.message) or "")
        end
        write_entry({
            file = file,
            descriptions = descs,
            status = status,
            duration = duration,
            message = msg or "",
            trace = trace_str,
        })
    end

    handler.testEnd = function(element, parent, status, trace)
        if status == "success" then
            record("success", element, parent)
        elseif status == "pending" then
            record("pending", element, parent)
        end
        -- failure/error are reported via failureTest/errorTest below.
        return nil, true
    end

    handler.failureTest = function(element, parent, msg, trace)
        record("failure", element, parent, msg, trace)
        return nil, true
    end

    handler.errorTest = function(element, parent, msg, trace)
        record("error", element, parent, msg, trace)
        return nil, true
    end

    -- File-level errors (compile errors, missing requires) are still useful.
    handler.error = function(element, parent, msg, trace)
        if element.descriptor == "file" then
            local file = element.name
            if file and file:sub(1, 1) == "@" then
                file = file:sub(2)
            end
            write_entry({
                file = file or "<unknown>",
                descriptions = { "<file-load-error>" },
                status = "error",
                duration = 0,
                message = msg or "",
                trace = (trace and (trace.traceback or trace.source)) or "",
            })
        end
        return nil, true
    end

    busted.subscribe({ "test", "end" }, handler.testEnd)
    busted.subscribe({ "failure", "it" }, handler.failureTest)
    busted.subscribe({ "error", "it" }, handler.errorTest)
    busted.subscribe({ "error" }, handler.error)

    return handler
end
