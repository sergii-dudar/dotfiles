-- MapStruct-aware reference finder.
--
-- Standard `gr` (textDocument/references) on a model/DTO field only surfaces the
-- getter/setter calls MapStruct emits into the generated `*MapperImpl.java`; the
-- `@Mapping(source=…)` / `@Mapping(target=…)` declarations in the mapper interfaces
-- are invisible to JDTLS (opaque string literals). This module augments the native
-- references with those declaration lines by exploiting JDTLS's own override linkage:
--
--   field usage in *MapperImpl.java  --(enclosing @Override method)-->
--   textDocument/declaration          --(super method)-->
--   interface mapping method          --(scan @Mapping block)--> declaration line
--
-- Entry point: `M.find_references(opts)`, registered as the Java `references`
-- navigation handler in `utils/lang/java/lsp-java.lua`. When no generated-impl usage
-- is found, or nothing is added, it defers to the standard references picker via
-- `opts.fallback` so non-mapper `gr` behaves exactly as before.

local lsp_util = require("utils.lsp-util")
local java_ts = require("utils.java.java-ts-util")
local logging_util = require("utils.logging-util")
local log = logging_util.new({ name = "MapStructRefs", filename = "mapstruct-source.log" })

local M = {}

-- How far above a mapping method we scan for its @Mapping annotation block.
local MAX_ANNOTATION_SCAN_LINES = 60

--- Whether a file path is a generated MapStruct mapper implementation.
--- Maven emits into `target/generated-sources/`, Gradle into `build/generated/`;
--- the class itself is named `<Mapper>Impl`.
---@param file string
---@return boolean
local function is_generated_mapper_impl(file)
    if type(file) ~= "string" then
        return false
    end
    local in_generated = file:find("/target/generated%-sources/") or file:find("/build/generated/")
    if not in_generated then
        return false
    end
    return file:match("Impl%.java$") ~= nil
end

--- Normalize an LSP definition/declaration result into a single location.
--- Handles `Location`, `Location[]`, and `LocationLink[]` shapes.
---@param result any
---@return { uri: string, line: integer, character: integer }|nil
local function first_location(result)
    if not result then
        return nil
    end
    if result.uri or result.targetUri then
        result = { result }
    end
    local loc = result[1]
    if not loc then
        return nil
    end
    local uri = loc.uri or loc.targetUri
    local range = loc.range or loc.targetSelectionRange or loc.targetRange
    if not uri or not range then
        return nil
    end
    return { uri = uri, line = range.start.line, character = range.start.character }
end

--- The last dot-separated segment of a MapStruct path expression.
---@param path string
---@return string
local function last_segment(path)
    return path:match("([%w_$]+)%s*$") or path
end

--- Collect `@Mapping` declaration locations in a mapper interface that reference
--- `field_name`, by scanning the annotation block directly above a mapping method.
---@param iface_uri string
---@param method_line integer 0-indexed line of the interface method declaration
---@param field_name string
---@return { uri: string, lnum: integer, col: integer, text: string }[]
local function collect_mapping_locations(iface_uri, method_line, field_name)
    local bufnr = vim.uri_to_bufnr(iface_uri)
    vim.fn.bufload(bufnr)

    local found = {}
    -- Walk upward over the contiguous annotation/continuation block above the method.
    local first_line = math.max(0, method_line - MAX_ANNOTATION_SCAN_LINES)
    local lines = vim.api.nvim_buf_get_lines(bufnr, first_line, method_line, false)

    for idx = #lines, 1, -1 do
        local line = lines[idx]
        local trimmed = line:gsub("^%s+", "")

        -- Stop once we leave the annotation block: a non-blank line that is not an
        -- annotation, a wrapped annotation argument, or a comment marks the previous
        -- member. (MapStruct @Mapping annotations sit directly above the method.)
        local is_block_line = trimmed == ""
            or trimmed:sub(1, 1) == "@"
            or trimmed:sub(1, 1) == '"'
            or trimmed:sub(1, 1) == ","
            or trimmed:sub(1, 1) == ")"
            or trimmed:sub(1, 1) == "("
            or trimmed:sub(1, 2) == "//"
            or trimmed:sub(1, 1) == "*"
            or trimmed:sub(1, 2) == "/*"
            or trimmed:match("^[%w_]+%s*=") ~= nil
        if not is_block_line then
            break
        end

        local lnum0 = first_line + idx - 1 -- 0-indexed buffer line
        for _, attr in ipairs({ "source", "target" }) do
            local value_start = line:find(attr .. '%s*=%s*"')
            if value_start then
                local path = line:match(attr .. '%s*=%s*"([^"]*)"')
                if path and last_segment(path) == field_name then
                    local quote_col = line:find('"', value_start) or value_start
                    table.insert(found, {
                        uri = iface_uri,
                        lnum = lnum0 + 1, -- 1-indexed for picker item
                        col = quote_col, -- 0-indexed (quote_col is 1-indexed byte -> use as col start)
                        text = trimmed,
                    })
                end
            end
        end
    end

    return found
end

--- Resolve the interface mapping method for an overriding impl method.
--- Primary: textDocument/declaration (confirmed to hop to the parent type). Fallback:
--- the JDTLS `java.action.gotoSuperImplementation` command if declaration stays in the
--- same (impl) file or returns nothing.
---@param client vim.lsp.Client
---@param impl_uri string
---@param pos { line: integer, character: integer }
---@param cb fun(loc: { uri: string, line: integer, character: integer }|nil)
local function resolve_interface_method(client, impl_uri, pos, cb)
    local params = {
        textDocument = { uri = impl_uri },
        position = pos,
    }

    client:request("textDocument/declaration", params, function(_, result)
        local loc = first_location(result)
        if loc and loc.uri ~= impl_uri then
            cb(loc)
            return
        end

        -- Fallback: super-implementation command (impl override -> interface method).
        client:request("workspace/executeCommand", {
            command = "java.action.gotoSuperImplementation",
            arguments = { { uri = impl_uri, position = pos } },
        }, function(_, super_result)
            cb(first_location(super_result))
        end)
    end)
end

--- Whether a reference's file path denotes a test source. Test references are
--- hidden by default (toggle back with <C-t>). Heuristic: the path contains a
--- `test` directory segment — e.g. `…/src/test/java/…`.
---@param file string
---@return boolean
local function is_test_ref(file)
    return type(file) == "string" and file:lower():find("/test/") ~= nil
end

--- Build a Snacks picker item from a location-like table.
---@param file string absolute path or jdt:// uri
---@param lnum integer 1-indexed line
---@param col integer 0-indexed column
---@param text string
---@return snacks.picker.finder.Item
local function make_item(file, lnum, col, text)
    return {
        text = file .. " " .. (text or ""),
        file = file,
        pos = { lnum, col },
        line = text,
    }
end

--- Show references (+ @Mapping declarations) in a Snacks picker. Test references
--- (under `src/test`) are hidden by default; <C-t> toggles them back in, appended
--- below the source references. `items` is the full ordered list (mappings first,
--- then native references); the split into shown/hidden happens here.
---@param items snacks.picker.finder.Item[]
---@param title string
local function show(items, title)
    -- Same layout the standard lsp_references picker uses (see snacks/configs/pickers.lua).
    local layouts = require("plugins.snacks.configs.layouts")

    local visible, tests = {}, {}
    for _, item in ipairs(items) do
        if is_test_ref(item.file) then
            item.main = false
            table.insert(tests, item)
        else
            item.main = true
            table.insert(visible, item)
        end
    end

    local show_tests = false

    Snacks.picker.pick({
        title = title,
        format = "file",
        auto_confirm = true,
        layout = layouts.custom_vertical,
        -- Keep test references below source ones even while filtering: `main` groups
        -- them (true -> 0, false -> 1), then normal score/idx ordering applies within
        -- each group. (With an empty query Snacks already preserves the finder order.)
        sort = { fields = { "main", { name = "score", desc = true }, "idx" } },
        finder = function()
            if not show_tests or #tests == 0 then
                return visible
            end
            -- Source references on top, test references appended at the bottom.
            local merged = {}
            for _, item in ipairs(visible) do
                table.insert(merged, item)
            end
            for _, item in ipairs(tests) do
                table.insert(merged, item)
            end
            return merged
        end,
        win = {
            input = { keys = { ["<c-t>"] = { "toggle_tests", mode = { "i", "n" }, desc = "Toggle test references" } } },
            list = { keys = { ["<c-t>"] = { "toggle_tests", mode = { "n" }, desc = "Toggle test references" } } },
        },
        actions = {
            toggle_tests = function(picker)
                show_tests = not show_tests
                picker:find()
            end,
        },
    })
end

--- Find references for the field under the cursor, augmented with MapStruct
--- `@Mapping` declaration lines. Ordering: `@Mapping` declarations first, then source
--- (main) references, then test references. Falls back to the standard references
--- picker only when references can't be resolved (no jdtls client / empty result).
---@param opts { bufnr?: integer, row?: integer, col?: integer, fallback?: fun() }
function M.find_references(opts)
    opts = opts or {}
    local bufnr = opts.bufnr or vim.api.nvim_get_current_buf()
    local row = opts.row or (vim.api.nvim_win_get_cursor(0)[1] - 1)
    local col = opts.col or vim.api.nvim_win_get_cursor(0)[2]
    local fallback = opts.fallback or function()
        Snacks.picker.lsp_references()
    end

    local field_name = vim.fn.expand("<cword>")

    local client = lsp_util.get_client_by_name("jdtls", { bufnr = bufnr })
    if not client or field_name == "" then
        fallback()
        return
    end

    local encoding = client.offset_encoding or "utf-16"
    local ref_params = {
        textDocument = vim.lsp.util.make_text_document_params(bufnr),
        position = { line = row, character = col },
        context = { includeDeclaration = true },
    }

    client:request("textDocument/references", ref_params, function(err, locations)
        if err or not locations or vim.tbl_isempty(locations) then
            vim.schedule(fallback)
            return
        end

        vim.schedule(function()
            -- Native items are always kept in full ("keep impl").
            local native_qf = vim.lsp.util.locations_to_items(locations, encoding)
            local items = {}
            for _, qf in ipairs(native_qf) do
                table.insert(items, make_item(qf.filename, qf.lnum, (qf.col or 1) - 1, qf.text))
            end

            -- Collect unique enclosing methods of usages inside generated mapper impls.
            local impl_methods = {}
            local seen_method = {}
            for _, loc in ipairs(locations) do
                local uri = loc.uri or loc.targetUri
                local range = loc.range or loc.targetRange
                if uri and range then
                    local file = vim.uri_to_fname(uri)
                    if is_generated_mapper_impl(file) then
                        local mbuf = vim.uri_to_bufnr(uri)
                        vim.fn.bufload(mbuf)
                        local m = java_ts.get_enclosing_method_name_pos(mbuf, range.start.line, range.start.character)
                        if m then
                            local key = uri .. ":" .. m.method_row
                            if not seen_method[key] then
                                seen_method[key] = true
                                table.insert(impl_methods, {
                                    uri = uri,
                                    pos = { line = m.name_row, character = m.name_col },
                                })
                            end
                        end
                    end
                end
            end

            log.debug("references:", #locations, "impl methods:", #impl_methods, "field:", field_name)

            -- No mapper-impl usage: no @Mapping to add, but still apply the
            -- source-then-test ordering to the plain references ("default" gr).
            if #impl_methods == 0 then
                show(items, "Jdtls References")
                return
            end

            -- Fan out declaration lookups (pending-counter join, per jdtls-util pattern).
            local synthetic = {}
            local seen_mapping = {}
            local pending = #impl_methods

            local function finish()
                local mapping_items = {}
                for _, s in ipairs(synthetic) do
                    local dedup_key = s.uri .. ":" .. s.lnum
                    if not seen_mapping[dedup_key] then
                        seen_mapping[dedup_key] = true
                        table.insert(mapping_items, make_item(vim.uri_to_fname(s.uri), s.lnum, s.col, s.text))
                    end
                end

                -- Order: @Mapping declarations first, then the native references.
                -- `show` splits test references out (hidden by default). `mapping_items`
                -- may be empty when all mappings are implicit — then it is just the refs.
                local title = #mapping_items > 0 and "Jdtls References (+@Mapping)" or "Jdtls References"
                for _, native in ipairs(items) do
                    table.insert(mapping_items, native)
                end
                show(mapping_items, title)
            end

            for _, method in ipairs(impl_methods) do
                resolve_interface_method(client, method.uri, method.pos, function(iface)
                    if iface then
                        log.debug("impl", method.uri, "-> iface", iface.uri, "line", iface.line)
                        vim.schedule(function()
                            local mappings = collect_mapping_locations(iface.uri, iface.line, field_name)
                            for _, mp in ipairs(mappings) do
                                table.insert(synthetic, mp)
                            end
                            pending = pending - 1
                            if pending == 0 then
                                finish()
                            end
                        end)
                    else
                        pending = pending - 1
                        if pending == 0 then
                            vim.schedule(finish)
                        end
                    end
                end)
            end
        end)
    end, bufnr)
end

return M
