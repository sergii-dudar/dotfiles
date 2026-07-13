-- `<leader>jj` dispatcher: open a Java FQN, or resolve a path-valued variable to a file.
--
-- Two behaviors share one keymap, chosen by the treesitter node under the cursor:
--   • branch A (any filetype, or a type reference in Java) — open the Java type whose
--     dotted FQN is under the cursor, via `jdtls-util.open_fqn_under_cursor()`.
--   • branch B (Java value reference) — recursively resolve a variable / static field /
--     string literal whose value is a file path and jump to that file. Resolution covers
--     string literals, `+` concatenation, references to other constants (possibly in other
--     files, via jdtls go-to-definition), and the string-literal/identifier arguments of a
--     method/constructor call (e.g. `TestUtil.readFromFile("json/.../response.json")`).
--
-- Public:
-- • goto_under_cursor — keymap entry point; classifies the cursor node and dispatches
--
-- The resolution phase never moves the cursor and never swaps the window buffer, so the
-- jumplist ends with exactly the origin position + the opened file (clean <C-o>/<C-i>).

local M = {}

local jdtls_util = require("utils.java.jdtls-util")
local lsp_util = require("utils.lsp-util")
local java_common = require("utils.java.java-common")

local MAX_DEPTH = 12

-- ---------------------------------------------------------------------------
--  treesitter helpers
-- ---------------------------------------------------------------------------

--- Get the parsed java tree root for a (loaded) buffer.
---@param bufnr integer
---@return TSNode|nil
local function java_root(bufnr)
    local ok, parser = pcall(vim.treesitter.get_parser, bufnr, "java")
    if not ok or not parser then
        return nil
    end
    local tree = parser:parse()[1]
    return tree and tree:root() or nil
end

--- Strip the surrounding quotes from a `string_literal` node's text.
---@param node TSNode
---@param bufnr integer
---@return string
local function string_literal_value(node, bufnr)
    local text = vim.treesitter.get_node_text(node, bufnr)
    -- Drop the leading/trailing double quotes; text_block (""") is left as-is.
    return (text:gsub('^"', ""):gsub('"$', ""))
end

--- Find a same-file declarator initializer for a simple name (local var or field).
--- Returns the `value` node of the nearest matching declarator, or nil.
---@param root TSNode
---@param bufnr integer
---@param name string
---@return TSNode|nil
local function find_local_declarator_value(root, bufnr, name)
    local found
    local function walk(node)
        if found then
            return
        end
        if node:type() == "variable_declarator" then
            local name_node = node:field("name")[1]
            if name_node and vim.treesitter.get_node_text(name_node, bufnr) == name then
                found = node:field("value")[1]
                return
            end
        end
        for child in node:iter_children() do
            walk(child)
        end
    end
    walk(root)
    return found
end

--- For an identifier/field_access reference, return the anchor identifier node (the one a
--- go-to-definition should be issued on) and its simple name.
---@param node TSNode
---@param bufnr integer
---@return TSNode|nil anchor, string|nil name
local function reference_anchor(node, bufnr)
    if node:type() == "field_access" then
        local field = node:field("field")[1]
        if field then
            return field, vim.treesitter.get_node_text(field, bufnr)
        end
    end
    if node:type() == "identifier" then
        return node, vim.treesitter.get_node_text(node, bufnr)
    end
    return nil, nil
end

-- ---------------------------------------------------------------------------
--  jdtls cross-file go-to-definition (no window/cursor movement)
-- ---------------------------------------------------------------------------

--- Build an LSP position for a node start in `bufnr`, in the client's offset encoding.
---@param bufnr integer
---@param node TSNode
---@param encoding string
---@return { line: integer, character: integer }
local function lsp_position(bufnr, node, encoding)
    local row, col = node:range()
    local line = vim.api.nvim_buf_get_lines(bufnr, row, row + 1, false)[1] or ""
    local ok, idx = pcall(vim.str_utfindex, line, encoding, col)
    return { line = row, character = ok and idx or col }
end

--- Ensure the file behind `uri` is loaded and attached to jdtls (so it is didOpen'd and
--- treesitter can parse it), and return its bufnr. Returns nil for non-file uris.
---@param uri string
---@param jdtls_id integer
---@return integer|nil
local function ensure_loaded_attached(uri, jdtls_id)
    if not uri:match("^file:") then
        return nil -- jdt:// / jar contents are out of scope for path resolution
    end
    local path = vim.uri_to_fname(uri)
    local bufnr = vim.fn.bufadd(path)
    vim.fn.bufload(bufnr)
    if not vim.lsp.buf_is_attached(bufnr, jdtls_id) then
        vim.lsp.buf_attach_client(bufnr, jdtls_id)
    end
    return bufnr
end

--- Resolve a reference's declaration via jdtls and return { bufnr, value_node } for the
--- enclosing declarator, or nil when it can't be followed (library, no initializer, …).
---@param bufnr integer
---@param anchor TSNode
---@param visited table<string, boolean>
---@return integer|nil, TSNode|nil
local function follow_definition(bufnr, anchor, visited)
    local client = lsp_util.get_client_by_name("jdtls")
    if not client then
        return nil
    end

    local params = {
        textDocument = { uri = vim.uri_from_bufnr(bufnr) },
        position = lsp_position(bufnr, anchor, client.offset_encoding),
    }

    local function request()
        local resp = client:request_sync("textDocument/definition", params, 3000, bufnr)
        return resp and not resp.err and resp.result or nil
    end

    local result = request()
    local loc = result and (vim.islist(result) and result[1] or result)
    if not loc then
        return nil
    end

    local uri = loc.uri or loc.targetUri
    local range = loc.range or loc.targetSelectionRange or loc.targetRange
    if not uri or not range then
        return nil
    end

    local key = uri .. ":" .. range.start.line .. ":" .. range.start.character
    if visited[key] then
        return nil
    end
    visited[key] = true

    local jdtls_id = client.id
    local target_buf = ensure_loaded_attached(uri, jdtls_id)
    if not target_buf then
        return nil
    end

    local root = java_root(target_buf)
    if not root then
        return nil
    end

    -- Locate the declaration node at the definition range and climb to its declarator.
    local node = root:named_descendant_for_range(
        range.start.line,
        range.start.character,
        range.start.line,
        range.start.character
    )
    while node and node:type() ~= "variable_declarator" do
        node = node:parent()
    end
    if not node then
        return nil
    end
    return target_buf, node:field("value")[1]
end

-- ---------------------------------------------------------------------------
--  recursive value resolution
-- ---------------------------------------------------------------------------

--- Resolve a java expression node to its string value, or nil when unresolvable.
---@param bufnr integer
---@param node TSNode|nil
---@param depth integer
---@param visited table<string, boolean>
---@return string|nil
local function resolve_value(bufnr, node, depth, visited)
    if not node or depth > MAX_DEPTH then
        return nil
    end

    local t = node:type()

    if t == "string_literal" then
        return string_literal_value(node, bufnr)
    end

    if t == "parenthesized_expression" then
        for child in node:iter_children() do
            if child:named() then
                return resolve_value(bufnr, child, depth + 1, visited)
            end
        end
        return nil
    end

    if t == "binary_expression" then
        local left = node:field("left")[1]
        local right = node:field("right")[1]
        local lv = resolve_value(bufnr, left, depth + 1, visited)
        local rv = resolve_value(bufnr, right, depth + 1, visited)
        if lv and rv then
            return lv .. rv
        end
        return nil
    end

    if t == "identifier" or t == "field_access" then
        local anchor, name = reference_anchor(node, bufnr)
        if not anchor or not name then
            return nil
        end
        -- Cheap path: a sibling local/field in the same file (no LSP round-trip).
        local root = java_root(bufnr)
        if root then
            local local_value = find_local_declarator_value(root, bufnr, name)
            if local_value then
                return resolve_value(bufnr, local_value, depth + 1, visited)
            end
        end
        -- Cross-file: follow the definition via jdtls.
        local target_buf, value_node = follow_definition(bufnr, anchor, visited)
        if target_buf and value_node then
            return resolve_value(target_buf, value_node, depth + 1, visited)
        end
        return nil
    end

    if t == "method_invocation" or t == "object_creation_expression" then
        -- The path is almost always a string-literal/identifier argument, e.g.
        --   TestUtil.readFromFile("json/.../response.json")
        --   JsonPathUtil.builder(SOME_JSON_CONSTANT)...
        local candidates = {}
        local args = node:field("arguments")[1]
        if args then
            for child in args:iter_children() do
                if child:named() then
                    local v = resolve_value(bufnr, child, depth + 1, visited)
                    if v then
                        table.insert(candidates, v)
                    end
                end
            end
        end
        -- Prefer an argument that looks like a path (has a separator or extension).
        for _, v in ipairs(candidates) do
            if v:match("[/\\]") or v:match("%.%w+$") then
                return v
            end
        end
        if candidates[1] then
            return candidates[1]
        end
        -- Builder-style chain (a().b().c()): descend into the receiver.
        local object = node:field("object")[1]
        if object then
            return resolve_value(bufnr, object, depth + 1, visited)
        end
        return nil
    end

    return nil -- other expression kinds are out of scope
end

-- ---------------------------------------------------------------------------
--  open the resolved path (gf-style) with a clean jumplist
-- ---------------------------------------------------------------------------

--- Build the list of directories to search a relative path against.
---@param bufnr integer
---@return string[]
local function search_dirs(bufnr)
    local dirs = {}
    local resolved = require("utils.resource-cwd-resolver").resolve(bufnr)
    if resolved and resolved.dirs then
        vim.list_extend(dirs, resolved.dirs)
    end
    local module_root = java_common.get_buffer_project_path(bufnr)
    if module_root then
        table.insert(dirs, module_root)
    end
    local file_dir = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(bufnr), ":h")
    if file_dir ~= "" then
        table.insert(dirs, file_dir)
    end
    table.insert(dirs, vim.fn.getcwd())
    return dirs
end

--- Resolve a path string to an existing, readable absolute file, or nil.
---@param path string
---@param bufnr integer
---@return string|nil
local function resolve_existing_file(path, bufnr)
    path = vim.trim(path)
    if path == "" then
        return nil
    end

    -- Absolute / home-relative path: use directly when readable.
    local expanded = vim.fn.fnamemodify(vim.fn.expand(path), ":p")
    if vim.fn.filereadable(expanded) == 1 then
        return expanded
    end

    -- Relative path: search against module resource roots / cwd (gf semantics).
    local dirs = search_dirs(bufnr)
    local found = vim.fn.findfile(path, table.concat(dirs, ","))
    if found ~= "" then
        return vim.fn.fnamemodify(found, ":p")
    end

    -- Last resort: explicit join against each candidate dir.
    for _, dir in ipairs(dirs) do
        local candidate = dir .. "/" .. path
        if vim.fn.filereadable(candidate) == 1 then
            return vim.fn.fnamemodify(candidate, ":p")
        end
    end
    return nil
end

--- Open the file in the current window, recording the origin in the jumplist.
---@param file string absolute path
local function open_file(file)
    vim.cmd("normal! m'") -- push current position → single jumplist entry
    local bufnr = vim.fn.bufadd(file)
    vim.fn.bufload(bufnr)
    vim.bo[bufnr].buflisted = true
    vim.api.nvim_win_set_buf(0, bufnr)
end

--- Warn that a resolved path string didn't match an existing file.
---@param path string
local function warn_missing(path)
    vim.notify("⚠️ No file found for resolved path: " .. path, vim.log.levels.WARN)
end

--- Try Vim's built-in `gf` on the cursor (open the file whose name is under it). `gf` is a
--- jump, so it records the origin in the jumplist itself. Returns true if a file opened.
---@return boolean
local function try_gf()
    local before = vim.api.nvim_get_current_buf()
    local ok = pcall(vim.cmd, "normal! gf")
    return ok and vim.api.nvim_get_current_buf() ~= before
end

-- ---------------------------------------------------------------------------
--  dispatch
-- ---------------------------------------------------------------------------

--- Classify the node under the cursor as a "type" reference or a "value" reference.
---@return "type"|"value"
local function classify_cursor()
    local node = vim.treesitter.get_node()
    while node do
        local t = node:type()
        if
            t == "scoped_type_identifier"
            or t == "type_identifier"
            or t == "generic_type"
            or t == "import_declaration"
            or t == "package_declaration"
        then
            return "type"
        end
        if
            t == "string_literal"
            or t == "field_access"
            or t == "binary_expression"
            or t == "variable_declarator"
            or t == "local_variable_declaration"
            or t == "argument_list"
            or t == "method_invocation"
        then
            return "value"
        end
        node = node:parent()
    end
    return "value"
end

--- Whether the cursor sits on a string literal (vs a variable/field reference). Used to
--- pick the right fallback: a string literal is not a go-to-definition target.
---@return boolean
local function cursor_on_string()
    local node = vim.treesitter.get_node()
    while node do
        local t = node:type()
        if t == "string_literal" then
            return true
        end
        if
            t == "identifier"
            or t == "field_access"
            or t == "method_invocation"
            or t == "binary_expression"
            or t == "argument_list"
        then
            return false
        end
        node = node:parent()
    end
    return false
end

--- Resolve a path-valued reference under the cursor and open the file it points to.
--- On failure, the second return value is the path we resolved but couldn't locate (or nil
--- when nothing resolved), so the caller can decide whether/when to warn.
---@return boolean handled, string|nil unresolved_path
local function resolve_and_open()
    local node = vim.treesitter.get_node()
    if not node then
        return false, nil
    end

    local bufnr = vim.api.nvim_get_current_buf()
    local resolved = resolve_value(bufnr, node, 0, {})
    if not resolved then
        return false, nil
    end

    local file = resolve_existing_file(resolved, bufnr)
    if not file then
        return false, resolved
    end

    open_file(file)
    return true, nil
end

--- Keymap entry point for `<leader>jj`.
-- lua require("utils.java.goto-file-under-cursor").goto_under_cursor()
function M.goto_under_cursor()
    if vim.bo.filetype ~= "java" then
        return jdtls_util.open_fqn_under_cursor()
    end

    if classify_cursor() == "type" then
        return jdtls_util.open_fqn_under_cursor()
    end

    -- Value reference (field / variable / string): try to resolve a file path and open it.
    local opened, missing = resolve_and_open()
    if opened then
        return
    end

    -- Resolution failed (no string resolved, or resolved one but no file matched it).
    -- Pick the fallback by what the cursor sits on:
    --   • a string literal → try Vim's `gf` first (the cursor may already sit on a plain
    --     path that `gf`'s own search knows about), then fall back to the FQN opener; a
    --     literal is not a go-to-definition target.
    --   • a variable/field reference → go-to-definition lands on its declaration.
    if cursor_on_string() then
        if try_gf() then
            return
        end
        if missing then
            warn_missing(missing)
        end
        jdtls_util.open_fqn_under_cursor()
    else
        if missing then
            warn_missing(missing)
        end
        vim.lsp.buf.definition()
    end
end

return M
