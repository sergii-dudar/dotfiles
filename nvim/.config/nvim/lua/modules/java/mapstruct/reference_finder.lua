-- MapStruct-aware `gr` (references).
--
-- Standard `gr` (textDocument/references) on a model/DTO field only surfaces the
-- getter/setter calls MapStruct emits into the generated `*MapperImpl.java`; the
-- `@Mapping(source=…)` / `@Mapping(target=…)` (and `@ValueMapping`) declarations in
-- the mapper types are invisible to JDTLS (opaque string literals). This module
-- surfaces those declaration lines too, then hands the whole result to Snacks' own
-- `lsp_references` picker so the native half (include_current filtering, jdt://
-- preview, dedup, auto_confirm/jump) is reused verbatim.
--
-- Discovery — anchor on the references JDTLS already returns:
--   field usage in *MapperImpl.java  --(class `extends`/`implements` clause)-->
--   textDocument/definition           --(resolve the mapper type)-->
--   scan the whole mapper file for @Mapping/@ValueMapping source|target segments.
-- The scan is whole-file (not one method's block) because deeply-nested property
-- paths compile into private helper methods that override nothing — so only the
-- mapper *type*, not a specific method, is resolvable from the impl usage.
--
-- Presentation — a Snacks multi-finder: finder #1 is the built-in `lsp_references`
-- (native refs), finder #2 injects the precomputed @Mapping lines. A `transform`
-- ranks @Mapping declarations on top and test references at the bottom (hidden by
-- default, <C-t> toggles). Entry point: `M.find_references(opts)`, registered as the
-- Java `references` navigation handler in `utils/lang/java/lsp-java.lua`; it defers
-- to `opts.fallback` when there is no jdtls client / no symbol under the cursor.

local lsp_util = require("utils.lsp-util")
local logging_util = require("utils.logging-util")
local log = logging_util.new({ name = "MapStructRefs", filename = "mapstruct-source.log" })

local M = {}

--- Whether a reference URI points at a generated MapStruct mapper implementation.
--- Two shapes are recognized:
---  • project builds — Maven emits into `target/generated-sources/`, Gradle into
---    `build/generated/`; the class is named `<Mapper>Impl.java`.
---  • library mappers — when the mapper (and thus its generated impl) ships inside a
---    dependency jar, JDTLS surfaces it as a `jdt://…/<Mapper>MapperImpl.class` URI, so
---    match the impl type name directly rather than a source directory.
---@param uri string
---@return boolean
local function is_generated_mapper_impl(uri)
    if type(uri) ~= "string" then
        return false
    end

    if vim.startswith(uri, "jdt://") then
        local type_name = uri:match("([%w_%$]+)%.class") or uri:match("([%w_%$]+)%.java")
        return type_name ~= nil and type_name:find("Mapper") ~= nil and type_name:find("Impl") ~= nil
    end

    local file = vim.uri_to_fname(uri)
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

--- Property names that a reference to `word` could appear as inside a MapStruct
--- path. Always the identifier itself; plus, when the cursor is on an accessor
--- (`getFoo` / `setFoo` / `isFoo`), the decapitalized property name (`foo`) — since
--- MapStruct paths use the property name, not the accessor.
---@param word string
---@return table<string, boolean>
local function candidate_names(word)
    local names = { [word] = true }
    local rest = word:match("^get(%u[%w_$]*)$") or word:match("^set(%u[%w_$]*)$") or word:match("^is(%u[%w_$]*)$")
    if rest then
        names[rest:sub(1, 1):lower() .. rest:sub(2)] = true
    end
    return names
end

--- Whether any dot/`.`-separated segment of a MapStruct path is one of `names`.
--- Segments are matched anywhere in the path (`a.field.c`), so multi-field source
--- and target paths surface too — not just the final segment.
---@param path string
---@param names table<string, boolean>
---@return boolean
local function path_matches(path, names)
    for segment in path:gmatch("[%w_$]+") do
        if names[segment] then
            return true
        end
    end
    return false
end

--- Scan an entire mapper interface file for `@Mapping` declarations whose source or
--- target path references one of `names`. We scan the whole file (not just the block
--- above one method) because MapStruct emits deeply-nested property assignments into
--- private helper methods that override nothing — so the impl usage can't be linked to
--- a specific interface method, only to the interface type. Matching any path segment
--- also surfaces multi-field paths (`a.field.c`).
---@param iface_uri string
---@param names table<string, boolean>
---@return { uri: string, lnum: integer, col: integer, text: string }[]
local function scan_interface_mappings(iface_uri, names)
    local bufnr = vim.uri_to_bufnr(iface_uri)
    vim.fn.bufload(bufnr)

    local found = {}
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)

    for idx, line in ipairs(lines) do
        for _, attr in ipairs({ "source", "target" }) do
            local value_start = line:find(attr .. '%s*=%s*"')
            if value_start then
                local path = line:match(attr .. '%s*=%s*"([^"]*)"')
                if path and path_matches(path, names) then
                    local quote_col = line:find('"', value_start) or value_start
                    table.insert(found, {
                        uri = iface_uri,
                        lnum = idx, -- 1-indexed line for picker item
                        col = quote_col, -- 0-indexed path start (quote_col is 1-indexed quote byte)
                        text = line:gsub("^%s+", ""),
                    })
                end
            end
        end
    end

    return found
end

--- Locate the position of the mapper type name in a generated impl's supertype
--- clause, so a `textDocument/definition` there resolves the mapper. MapStruct
--- generates two shapes depending on how the mapper is declared:
---  • interface mapper — `class FooMapperImpl implements FooMapper` (`implements`).
---  • abstract-class mapper — `class FooMapperImpl extends FooMapper` (`extends`);
---    these use `protected abstract` mapping methods, common for `@Mapping`-heavy
---    ISO 20022 document mappers.
--- Match `extends` first (an abstract-class mapper only extends the mapper), then
--- fall back to `implements`. The class declaration sits near the top of a generated
--- impl, so only the head is scanned.
---@param bufnr integer
---@return { row: integer, col: integer }|nil # 0-indexed row/col of the mapper type name
local function get_impl_interface_pos(bufnr)
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, 400, false)
    for idx, line in ipairs(lines) do
        if line:find("class%s+[%w_$]+") then
            local _, kw_end = line:find("extends%s+")
            if not kw_end then
                _, kw_end = line:find("implements%s+")
            end
            if kw_end then
                return { row = idx - 1, col = kw_end }
            end
        end
    end
    return nil
end

--- Whether a reference's file path denotes a test source. Test references are
--- hidden by default (toggle back with <C-t>). Heuristic: the path contains a
--- `test` directory segment — e.g. `…/src/test/java/…`.
---@param file string
---@return boolean
local function is_test_ref(file)
    return type(file) == "string" and file:lower():find("/test/") ~= nil
end

--- Whether a native reference item is the symbol under the cursor (the location
--- `gr` was invoked on). Mirrors Snacks' `include_current = false` filter
--- (snacks/picker/source/lsp/init.lua): drop only the cursor location, so `gr` on
--- a declaration removes the self-match (leaving real usages) while `gr` on a usage
--- still lists the declaration. Without this, an unused field returns exactly one
--- location — itself — which the picker's `auto_confirm` silently jumps to.
---@param item vim.quickfix.entry
---@param cur_file string normalized current file path
---@param cur_lnum integer 1-indexed cursor line
---@return boolean
local function is_current_location(item, cur_file, cur_lnum)
    if not item.filename or vim.fs.normalize(item.filename) ~= cur_file then
        return false
    end
    if not item.lnum then
        return false
    end
    if item.lnum == cur_lnum then
        return true
    end
    if item.end_lnum then
        return item.lnum <= cur_lnum and item.end_lnum >= cur_lnum
    end
    return false
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

--- Open Snacks' `lsp_references` picker, augmented with our precomputed @Mapping
--- declaration lines via a multi-finder. Finder #1 is the built-in `lsp_references`
--- (native references — include_current filtering, jdt:// preview, dedup,
--- auto_confirm/jump all inherited from the source defaults); finder #2 injects
--- `mapping_items`. Test references are ranked to the bottom and hidden by default;
--- <C-t> toggles them.
---@param mapping_items snacks.picker.finder.Item[] precomputed @Mapping lines (may be empty)
---@param show_tests boolean initial test-reference visibility
local function open_picker(mapping_items, show_tests)
    -- Same layout the standard lsp_references picker uses (see snacks/configs/pickers.lua).
    local layouts = require("plugins.snacks.configs.layouts")
    local title = #mapping_items > 0 and "Jdtls References (+@Mapping)" or "Jdtls References"

    Snacks.picker.lsp_references({
        title = title,
        layout = layouts.custom_vertical,
        -- Multi-finder: reuse the built-in lsp_references finder (source_id 1) for
        -- native references, then inject our @Mapping lines (source_id 2).
        finder = {
            "lsp_references",
            function()
                return mapping_items
            end,
        },
        -- Group order, preserved even while filtering: @Mapping declarations (0) on
        -- top, source references (1), test references (2) at the bottom. Test refs are
        -- dropped entirely while hidden; <C-t> re-runs the finder to reveal them.
        transform = function(item)
            local is_mapping = item.source_id == 2
            local test = not is_mapping and is_test_ref(item.file)
            if test and not show_tests then
                return false
            end
            item.rank = is_mapping and 0 or (test and 2 or 1)
        end,
        sort = { fields = { "rank", { name = "score", desc = true }, "idx" } },
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

--- @Mapping-augmented `gr` for the field under the cursor. Discovers the mapper
--- `@Mapping` lines that reference the field, then opens Snacks' `lsp_references`
--- picker (which supplies the native references) with those lines injected on top.
--- Defers to `opts.fallback` only when there is no jdtls client / no symbol.
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

    -- Property names a @Mapping path segment could use for this field (handles the
    -- cursor being on a getFoo/setFoo/isFoo accessor as well as the field itself).
    local names = candidate_names(field_name)

    local encoding = client.offset_encoding or "utf-16"
    local cur_file = vim.fs.normalize(vim.api.nvim_buf_get_name(bufnr))
    local cur_lnum = row + 1

    -- Our own references request is used only to discover mapper-impl anchors; the
    -- picker's built-in lsp_references finder issues its own request for display.
    local ref_params = {
        textDocument = vim.lsp.util.make_text_document_params(bufnr),
        position = { line = row, character = col },
        context = { includeDeclaration = true },
    }

    client:request("textDocument/references", ref_params, function(err, locations)
        vim.schedule(function()
            -- No anchors to work from: open the picker with native references only.
            -- The built-in finder handles display and reports "No results" itself
            -- when there genuinely are none.
            if err then
                log.warn("references request failed:", err.message or vim.inspect(err))
            end
            if err or not locations or vim.tbl_isempty(locations) then
                open_picker({}, false)
                return
            end

            -- Whether any non-test reference other than the cursor's own location
            -- exists. When none does (and we add no @Mapping lines), the picker would
            -- otherwise be empty with tests hidden — so reveal the test refs instead.
            local visible_source, has_test = false, false
            for _, qf in ipairs(vim.lsp.util.locations_to_items(locations, encoding)) do
                if is_test_ref(qf.filename) then
                    has_test = true
                elseif not is_current_location(qf, cur_file, cur_lnum) then
                    visible_source = true
                end
            end

            -- Unique generated mapper-impl files among the references. Each impl maps
            -- exactly one mapper type, so we resolve/scan per impl file.
            local impl_uris, seen_impl = {}, {}
            for _, loc in ipairs(locations) do
                local uri = loc.uri or loc.targetUri
                if uri and is_generated_mapper_impl(uri) and not seen_impl[uri] then
                    seen_impl[uri] = true
                    table.insert(impl_uris, uri)
                end
            end

            log.debug("references:", #locations, "impl files:", #impl_uris, "field:", field_name)

            -- No mapper-impl usage: plain references, fully delegated to the builtin.
            if #impl_uris == 0 then
                open_picker({}, not visible_source and has_test)
                return
            end

            -- Fan out one mapper-type lookup per impl (pending-counter join, per
            -- jdtls-util pattern).
            local synthetic, seen_mapping = {}, {}
            local pending = #impl_uris

            local function finish()
                local mapping_items = {}
                for _, s in ipairs(synthetic) do
                    local dedup_key = s.uri .. ":" .. s.lnum
                    if not seen_mapping[dedup_key] then
                        seen_mapping[dedup_key] = true
                        table.insert(mapping_items, make_item(vim.uri_to_fname(s.uri), s.lnum, s.col, s.text))
                    end
                end
                -- With @Mapping lines there is always visible content; otherwise fall
                -- back to the all-tests reveal decision.
                local show_tests = #mapping_items == 0 and not visible_source and has_test
                open_picker(mapping_items, show_tests)
            end

            -- Decrement exactly once per impl (guarded), so a failed definition lookup
            -- or a not-yet-materialized jdt:// buffer can never leave `pending` above
            -- zero and the picker unshown.
            local function step_done()
                pending = pending - 1
                if pending == 0 then
                    finish()
                end
            end

            for _, impl_uri in ipairs(impl_uris) do
                local mbuf = vim.uri_to_bufnr(impl_uri)
                vim.fn.bufload(mbuf)
                local pos = get_impl_interface_pos(mbuf)

                if not pos then
                    log.warn("could not find supertype clause in impl:", impl_uri)
                    step_done()
                else
                    -- Resolve the mapper type (works past private helper methods that
                    -- override nothing), then scan its whole file.
                    client:request("textDocument/definition", {
                        textDocument = { uri = impl_uri },
                        position = { line = pos.row, character = pos.col },
                    }, function(_, def_result)
                        vim.schedule(function()
                            local iface = first_location(def_result)
                            if iface then
                                log.debug("impl", impl_uri, "-> mapper", iface.uri)
                                local ok, mappings = pcall(scan_interface_mappings, iface.uri, names)
                                if ok and mappings then
                                    for _, mp in ipairs(mappings) do
                                        table.insert(synthetic, mp)
                                    end
                                elseif not ok then
                                    log.warn("scan_interface_mappings failed:", mappings)
                                end
                            end
                            step_done()
                        end)
                    end)
                end
            end
        end)
    end, bufnr)
end

return M
