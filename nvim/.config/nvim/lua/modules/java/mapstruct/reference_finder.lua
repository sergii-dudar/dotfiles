-- MapStruct-aware `gr` (references).
--
-- Standard `gr` (textDocument/references) on a model/DTO field only surfaces the
-- getter/setter calls MapStruct emits into the generated `*MapperImpl.java`; the
-- `@Mapping(source=…)` / `@Mapping(target=…)` and `@ValueMapping(source=…/target=…)`
-- declarations in the mapper types are invisible to JDTLS (opaque string literals). This
-- module surfaces those declaration lines too, alongside the native references, in one
-- Snacks picker.
--
-- Discovery — references for the symbol under the cursor AND its sibling field/accessors
-- (get/set/is + record accessor) declared in the same enclosing type, unioned (MapStruct
-- emits accessor-based code, so a bare field alone can miss the generated usages). The
-- locations both feed the native items and anchor a per-reference @Mapping resolution.
-- For each reference that lands in a generated *MapperImpl.java we resolve the exact
-- interface @Mapping that produced it (reference_resolver, treesitter on the already-
-- loaded impl buffer):
--   field usage in *MapperImpl.java
--     --(resolve_sinks: enclosing @Override mapping method + target setter "sink")-->
--   deeply-nested paths compile into private helper methods that override nothing, so
--   the walk climbs helper -> caller (call-site search) until it reaches the public
--   method, carrying the leaf sink;
--     --(class `extends`/`implements` clause -> textDocument/definition)-->
--   resolve the mapper type, gate on @Mapper, then keep only that method's
--   @Mapping(target ~ sink). So `gr` on `Product.name` no longer drags in `Country.name`,
--   `Department.name`, or a target-side `name` from unrelated methods.
-- A reference with no target setter — a field used inside a `conditionExpression` /
-- `expression` / `defaultExpression` `java(…)` block (which compiles to an `if (…)` guard,
-- not an assignment) — resolves to a method-only descriptor, and we surface that method's
-- @Mapping lines whose expression-family attribute mentions the field.
--
-- Presentation — a single SYNCHRONOUS finder over lists we own: @Mapping declarations
-- on top, source references next, test references at the bottom (hidden by default,
-- <C-t> toggles). We build the native items ourselves (mirroring Snacks'
-- `include_current = false`) rather than composing the built-in async `lsp_references`
-- finder, because re-running that finder on every <C-t> dropped the injected @Mapping
-- items; a synchronous finder makes the toggle deterministic. We still reuse the
-- Snacks picker chrome (layout, previewer, jump, sort). Entry point:
-- `M.find_references(opts)`, registered as the Java `references` navigation handler in
-- `utils/lang/java/lsp-java.lua`; defers to `opts.fallback` when there is no jdtls
-- client / no symbol under the cursor.

local lsp_util = require("utils.lsp-util")
local java_ts = require("utils.java.java-ts-util")
local picker_util = require("utils.snacks-pickers-util")
local reference_resolver = require("modules.java.mapstruct.reference_resolver")
local spinner = require("utils.ui.spinner")
local logging_util = require("utils.logging-util")
local log = logging_util.new({ name = "MapStructRefs", filename = "mapstruct-source.log" })

-- Search-spinner notifier id + a watchdog timeout (ms) that force-stops it if a jdtls
-- request ever hangs, so the spinner can never be left spinning.
local SPINNER_ID = "mapstruct-references"
local SPINNER_TIMEOUT_MS = 30000

local M = {}

--- Session-persistent settings for the references picker. These live for the whole
--- Neovim session (not per-picker), so a toggle carries over to the next `gr`.
---@class MapStructRefsSettings
---@field show_test_references boolean Include `src/test` references in the picker. Toggle with <C-t>; carries to subsequent pickers this session. Useful while working on tests.
M.settings = {
    show_test_references = false,
}

--- Cheap URI prefilter for reference locations worth inspecting as a generated
--- mapper impl. MapStruct names the generated class `<X>Impl` where the mapper type
--- `<X>` need NOT contain "Mapper" (it can be any name, e.g. `FooConverter`), so we
--- only require the `Impl` suffix here — no "Mapper" in the name, no generated-dir.
--- Content qualification (qualifies_as_generated_impl) and the decisive parent
--- `@Mapper` gate (declares_mapper) run afterwards on the loaded buffer.
---@param uri string
---@return boolean
local function candidate_impl_uri(uri)
    if type(uri) ~= "string" then
        return false
    end
    if vim.startswith(uri, "jdt://") then
        local type_name = uri:match("([%w_%$]+)%.class") or uri:match("([%w_%$]+)%.java")
        return type_name ~= nil and type_name:match("Impl$") ~= nil
    end
    return vim.uri_to_fname(uri):match("Impl%.java$") ~= nil
end

--- Whether a loaded `*Impl` buffer looks annotation-processor-generated. Signals:
---  • on-disk — a `generated-sources` / `build/generated` dir, OR a `@Generated`
---    annotation (`javax.annotation.processing.Generated`, retention SOURCE — so it
---    is present in generated `.java` but absent from decompiled library bytecode).
---  • library (`jdt://`) — `@Generated` is stripped from bytecode, so accept here and
---    let the parent `@Mapper` gate decide.
--- This is a candidate filter only; declares_mapper is what actually confirms a
--- MapStruct mapper (so non-MapStruct generated `*Impl` classes are rejected there).
---@param bufnr integer
---@param uri string
---@return boolean
local function qualifies_as_generated_impl(bufnr, uri)
    if vim.startswith(uri, "jdt://") then
        return true
    end
    local file = vim.uri_to_fname(uri)
    if file:find("/target/generated%-sources/") or file:find("/build/generated/") then
        return true
    end
    for _, line in ipairs(vim.api.nvim_buf_get_lines(bufnr, 0, 60, false)) do
        if line:find("@Generated") then
            return true
        end
    end
    return false
end

--- Whether a loaded buffer declares a type annotated `@Mapper` (org.mapstruct.Mapper,
--- retention CLASS — visible in both source and decompiled bytecode). This is the
--- decisive gate: only after a resolved supertype is confirmed a MapStruct mapper do
--- we scan it, so generated `*Impl` classes from other processors (Dagger, Lombok,
--- Immutables, …) are ignored. Excludes `@MapperConfig` via a word boundary.
---@param bufnr integer
---@return boolean
local function declares_mapper(bufnr)
    for _, line in ipairs(vim.api.nvim_buf_get_lines(bufnr, 0, 400, false)) do
        if line:find("@Mapper%f[%W]") then
            return true
        end
    end
    return false
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

--- Locate the position of the mapper type name in a generated impl's supertype
--- clause, so a `textDocument/definition` there resolves the mapper. Only accepts a
--- class whose own name ends in `Impl` (the MapStruct-generated shape), then:
---  • interface mapper — `class FooImpl implements Foo` (`implements`).
---  • abstract-class mapper — `class FooImpl extends Foo` (`extends`); these use
---    `protected abstract` mapping methods, common for `@Mapping`-heavy ISO 20022
---    document mappers.
--- Match `extends` first (an abstract-class mapper only extends the mapper), then
--- fall back to `implements`. The class declaration sits near the top of a generated
--- impl, so only the head is scanned.
---@param bufnr integer
---@return { row: integer, col: integer }|nil # 0-indexed row/col of the mapper type name
local function mapper_impl_supertype_pos(bufnr)
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, 400, false)
    for idx, line in ipairs(lines) do
        local cls = line:match("class%s+([%w_$]+)")
        if cls then
            -- First top-level class in a generated impl file. If it isn't an `*Impl`,
            -- this isn't the generated shape we expect — stop.
            if not cls:match("Impl$") then
                return nil
            end
            local _, kw_end = line:find("extends%s+")
            if not kw_end then
                _, kw_end = line:find("implements%s+")
            end
            return kw_end and { row = idx - 1, col = kw_end } or nil
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

--- Build a Snacks picker item from a location-like table. `end_pos` ({1-indexed lnum,
--- 0-indexed exclusive end col}) makes the Snacks preview highlight the symbol span — the
--- word itself, exactly as `Snacks.picker.lsp_references` does (`pos`→`end_pos` extmark in
--- snacks/picker/core/preview.lua) — instead of only the result line.
---@param file string absolute path or jdt:// uri
---@param lnum integer 1-indexed line
---@param col integer 0-indexed column
---@param text string
---@param end_pos? integer[] {lnum, col} end of the highlight span (exclusive col)
---@return snacks.picker.finder.Item
local function make_item(file, lnum, col, text, end_pos)
    return {
        text = file .. " " .. (text or ""),
        file = file,
        pos = { lnum, col },
        end_pos = end_pos,
        line = text,
    }
end

--- End of a native reference's highlight span (the symbol under the range), mirroring
--- `Snacks.picker.source.lsp` (fix_locs): prefer the quickfix end when Neovim populated
--- it, else derive a same-line end from the original LSP range carried in `user_data`.
--- Returns nil when the span can't be determined (highlight falls back to the line).
---@param qf vim.quickfix.entry
---@return integer[]|nil # {lnum, col} exclusive end col (0-indexed)
local function native_end_pos(qf)
    if qf.end_lnum and qf.end_col then
        return { qf.end_lnum, qf.end_col - 1 }
    end
    local range = qf.user_data and qf.user_data.range
    if range and range.start.line == range["end"].line then
        return { qf.lnum, (qf.col or 1) - 1 + (range["end"].character - range.start.character) }
    end
    return nil
end

--- Open the references picker over lists we fully control: `@Mapping` declarations
--- on top, then source references, then test references at the bottom. A single
--- SYNCHRONOUS finder returns the current view, so <C-t> (toggle tests) re-runs it
--- deterministically — the @Mapping items are always re-emitted. (An earlier version
--- used a multi-finder with the built-in async `lsp_references` finder; re-running
--- that on every toggle dropped the injected @Mapping items, so we own the item list
--- instead and reuse only the Snacks picker chrome — layout, previewer, jump, sort.)
---@param mapping_items snacks.picker.finder.Item[] precomputed @Mapping lines (may be empty)
---@param native_items snacks.picker.finder.Item[] native references (declaration already filtered out)
local function open_picker(mapping_items, native_items)
    -- Same layout the standard lsp_references picker uses (see snacks/configs/pickers.lua).
    local layouts = require("plugins.snacks.configs.layouts")

    -- Split native references into source vs test so tests can sit at the bottom and
    -- be hidden by default.
    local sources, tests = {}, {}
    for _, item in ipairs(native_items) do
        if is_test_ref(item.file) then
            table.insert(tests, item)
        else
            table.insert(sources, item)
        end
    end

    if #mapping_items == 0 and #sources == 0 and #tests == 0 then
        vim.notify("No references found", vim.log.levels.INFO)
        return
    end

    local title = #mapping_items > 0 and "Jdtls References (+@Mapping)" or "Jdtls References"
    -- Show tests when they are the only thing to show (else the picker would look
    -- empty), regardless of the persisted flag.
    local force_tests = #mapping_items == 0 and #sources == 0 and #tests > 0

    -- Rank groups so order is preserved even while filtering: @Mapping (0), source
    -- refs (1), test refs (2). Test visibility follows the session-persistent flag
    -- (M.settings.show_test_references), re-read on every finder run so <C-t> toggles
    -- take effect and carry to the next picker.
    local function build()
        local show_tests = M.settings.show_test_references or force_tests
        local items = {}
        for _, it in ipairs(mapping_items) do
            it.rank = 0
            items[#items + 1] = it
        end
        for _, it in ipairs(sources) do
            it.rank = 1
            items[#items + 1] = it
        end
        if show_tests then
            for _, it in ipairs(tests) do
                it.rank = 2
                items[#items + 1] = it
            end
        end
        return items
    end

    Snacks.picker.pick({
        title = title,
        format = "file",
        auto_confirm = true,
        layout = layouts.custom_vertical,
        sort = { fields = { "rank", { name = "score", desc = true }, "idx" } },
        finder = build,
        win = {
            input = { keys = { ["<c-t>"] = { "toggle_tests", mode = { "i", "n" }, desc = "Toggle test references" } } },
            list = { keys = { ["<c-t>"] = { "toggle_tests", mode = { "n" }, desc = "Toggle test references" } } },
        },
        actions = {
            -- A `@Mapping` / reference can live in a `jdt://` (decompiled library) mapper,
            -- whose source loads asynchronously; keep the jump position honest so we don't
            -- get parked on line 1 (see snacks-pickers-util.reposition_after_jump).
            confirm = picker_util.confirm_with_reposition,
            -- Flip the session-persistent flag so the choice carries to later pickers.
            toggle_tests = function(picker)
                M.settings.show_test_references = not M.settings.show_test_references
                vim.notify(
                    "Test references " .. (M.settings.show_test_references and "shown" or "hidden") .. " (session)",
                    vim.log.levels.INFO
                )
                picker:find()
            end,
        },
    })
end

--- @Mapping-augmented `gr` for the field under the cursor. Unions references for the
--- symbol and its sibling accessors, resolves each generated-impl usage to the exact
--- interface `@Mapping` (via its target sink), then opens a single SYNCHRONOUS Snacks
--- picker over lists we own: `@Mapping` declarations on top, then source/test references
--- (see `open_picker`). Defers to `opts.fallback` only when there is no jdtls client /
--- no symbol under the cursor.
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

    -- `gr` on a *type* name (a class / record / interface / enum, or a reference to one)
    -- carries no MapStruct meaning — `@Mapping` paths address members, not types — so a
    -- type would otherwise drag in every mapper `@Mapping` line touching that type's fields.
    -- Defer to the plain reference search instead of augmenting.
    if java_ts.is_type_symbol_at(bufnr, row, col) then
        fallback()
        return
    end

    local encoding = client.offset_encoding or "utf-16"
    local cur_file = vim.fs.normalize(vim.api.nvim_buf_get_name(bufnr))
    local cur_lnum = row + 1

    -- Search spinner: started here, always stopped exactly once — by `show` on the first
    -- terminal result, or by the watchdog if a jdtls request hangs / never returns — so it
    -- can never be left spinning.
    spinner.start("🔍 Searching references…", { id = SPINNER_ID })

    local finished = false
    local watchdog = vim.uv.new_timer()

    --- Stop the spinner exactly once and tear down the watchdog.
    ---@param stop_fn fun()
    local function finish_spinner(stop_fn)
        if finished then
            return
        end
        finished = true
        if watchdog then
            watchdog:stop()
            watchdog:close()
            watchdog = nil
        end
        stop_fn()
    end

    if watchdog then
        watchdog:start(
            SPINNER_TIMEOUT_MS,
            0,
            vim.schedule_wrap(function()
                finish_spinner(function()
                    spinner.stop(false, "Reference search timed out", { id = SPINNER_ID, timeout = 3000 })
                end)
            end)
        )
    end

    -- Every terminal result funnels through here: stop the spinner (once), then open the
    -- picker (or its "No references found" notice). Results that arrive after a watchdog
    -- timeout still open the picker; the spinner is simply already stopped.
    local function show(mapping_items, native_items)
        finish_spinner(function()
            spinner.cancel({ id = SPINNER_ID })
        end)
        open_picker(mapping_items, native_items)
    end

    -- Resolve the (unioned) reference locations: build the native items, then map each
    -- generated-impl reference to the exact interface @Mapping via its target sink.
    ---@param names table<string, boolean> field + accessor tokens, used to highlight the word in @Mapping lines
    local function process(locations, names)
        -- Native reference items: drop the cursor's own location (Snacks
        -- `include_current = false` behaviour) and dedup by file:line. `native_end_pos`
        -- gives the symbol span so the preview highlights the word, not just the line.
        local native_items, seen_native = {}, {}
        for _, qf in ipairs(vim.lsp.util.locations_to_items(locations, encoding)) do
            if not is_current_location(qf, cur_file, cur_lnum) then
                local key = qf.filename .. ":" .. qf.lnum
                if not seen_native[key] then
                    seen_native[key] = true
                    table.insert(
                        native_items,
                        make_item(qf.filename, qf.lnum, (qf.col or 1) - 1, qf.text, native_end_pos(qf))
                    )
                end
            end
        end

        -- Candidate `*Impl` reference positions grouped by file (cheap URI prefilter;
        -- content qualification + parent @Mapper gate happen per file below). We keep
        -- every position, not just the file, because each anchors a per-method sink
        -- resolution on the impl buffer (which mapping method + which target setter).
        local impl_refs, impl_uris = {}, {}
        for _, loc in ipairs(locations) do
            local uri = loc.uri or loc.targetUri
            local range = loc.range or loc.targetSelectionRange or loc.targetRange
            if uri and range and candidate_impl_uri(uri) then
                if not impl_refs[uri] then
                    impl_refs[uri] = {}
                    table.insert(impl_uris, uri)
                end
                table.insert(impl_refs[uri], { line = range.start.line, character = range.start.character })
            end
        end

        log.debug("references:", #locations, "impl candidates:", #impl_uris, "field:", field_name)

        -- No candidate impl usage: plain references, no @Mapping lines to add.
        if #impl_uris == 0 then
            show({}, native_items)
            return
        end

        -- Fan out one mapper-type lookup per impl (pending-counter join, per jdtls-util
        -- pattern). Per impl we resolve the interface type once, gate on @Mapper, then
        -- keep only the @Mapping lines its reference sinks point at.
        local mapping_items, seen_mapping = {}, {}
        local pending = #impl_uris
        -- Set when a helper walk hit MAX_DEPTH: some deeply-nested mapping may be missing,
        -- so we tell the user rather than silently dropping it.
        local depth_truncated = false

        local function finish()
            if depth_truncated then
                vim.notify(
                    ("[MapStruct] Some deeply-nested mappings may be missing (path nesting deeper than %d levels)"):format(
                        reference_resolver.MAX_DEPTH
                    ),
                    vim.log.levels.WARN
                )
            end
            show(mapping_items, native_items)
        end

        -- Decrement exactly once per impl (guarded), so a failed definition lookup or a
        -- not-yet-materialized jdt:// buffer can never leave `pending` above zero and the
        -- picker unshown.
        local function step_done()
            pending = pending - 1
            if pending == 0 then
                finish()
            end
        end

        for _, impl_uri in ipairs(impl_uris) do
            local mbuf = vim.uri_to_bufnr(impl_uri)
            vim.fn.bufload(mbuf)
            local pos = qualifies_as_generated_impl(mbuf, impl_uri) and mapper_impl_supertype_pos(mbuf) or nil

            -- Resolve, from each reference in this impl, the enclosing @Override mapping
            -- method + target sink (climbing private helper methods that override
            -- nothing). These pin the exact interface @Mapping below.
            local sinks = {}
            if pos then
                for _, ref in ipairs(impl_refs[impl_uri]) do
                    local part, truncated = reference_resolver.resolve_sinks(mbuf, ref.line, ref.character)
                    vim.list_extend(sinks, part)
                    depth_truncated = depth_truncated or truncated
                end
            end

            if not pos or #sinks == 0 then
                -- Not a generated mapper impl (hand-written `*Impl`, no supertype clause,
                -- …) or nothing resolvable: keep native references only.
                step_done()
            else
                -- Resolve the mapper type from the impl supertype clause (works past
                -- private helpers), confirm it is a MapStruct `@Mapper`, then match only
                -- the resolved methods' @Mapping(target ~ sink) lines.
                local sent = client:request("textDocument/definition", {
                    textDocument = { uri = impl_uri },
                    position = { line = pos.row, character = pos.col },
                }, function(_, def_result)
                    vim.schedule(function()
                        local iface = first_location(def_result)
                        if iface then
                            local ibuf = vim.uri_to_bufnr(iface.uri)
                            vim.fn.bufload(ibuf)
                            if declares_mapper(ibuf) then
                                log.debug("impl", impl_uri, "-> @Mapper", iface.uri)
                                local ok, mappings = pcall(reference_resolver.mappings_for_sinks, ibuf, sinks, names)
                                if ok and mappings then
                                    local iface_file = vim.uri_to_fname(iface.uri)
                                    for _, mp in ipairs(mappings) do
                                        local dedup_key = iface.uri .. ":" .. mp.lnum
                                        if not seen_mapping[dedup_key] then
                                            seen_mapping[dedup_key] = true
                                            -- Highlight the matched word (source/target segment)
                                            -- when the resolver located it; else the line.
                                            local pos_col = mp.hl_start or mp.col
                                            local end_pos = mp.hl_end and { mp.lnum, mp.hl_end } or nil
                                            table.insert(
                                                mapping_items,
                                                make_item(iface_file, mp.lnum, pos_col, mp.text, end_pos)
                                            )
                                        end
                                    end
                                elseif not ok then
                                    log.warn("mappings_for_sinks failed:", mappings)
                                end
                            else
                                log.debug("supertype not @Mapper, skipping:", iface.uri)
                            end
                        end
                        step_done()
                    end)
                end)
                -- A definition request that fails to send would never call step_done via
                -- its callback; decrement here so the impl join can never stall.
                if not sent then
                    step_done()
                end
            end
        end
    end

    -- Union references for the symbol under the cursor AND its sibling field/accessors
    -- (get/set/is + record accessor) declared in the SAME enclosing type. MapStruct emits
    -- accessor-based code, so `gr` on a bare field can otherwise miss the generated
    -- usages; querying the siblings makes it work regardless of which one you invoke on.
    -- Siblings are found only in the current buffer (the model class) and scoped to the
    -- enclosing type, so same-named members of sibling classes are never conflated.
    local names = reference_resolver.property_sibling_names(field_name)
    local raw_positions = reference_resolver.sibling_declaration_positions(bufnr, row, col, names)
    table.insert(raw_positions, 1, { line = row, character = col })

    local positions, seen_pos = {}, {}
    for _, p in ipairs(raw_positions) do
        local key = p.line .. ":" .. p.character
        if not seen_pos[key] then
            seen_pos[key] = true
            positions[#positions + 1] = p
        end
    end

    local doc_params = vim.lsp.util.make_text_document_params(bufnr)
    local all_locations, seen_loc = {}, {}
    local pending_refs = #positions

    local function collect(locs)
        for _, loc in ipairs(locs or {}) do
            local uri = loc.uri or loc.targetUri
            local range = loc.range or loc.targetSelectionRange or loc.targetRange
            if uri and range then
                local key = uri .. ":" .. range.start.line .. ":" .. range.start.character
                if not seen_loc[key] then
                    seen_loc[key] = true
                    all_locations[#all_locations + 1] = loc
                end
            end
        end
    end

    local function on_refs_done()
        if #all_locations == 0 then
            show({}, {})
            return
        end
        process(all_locations, names)
    end

    local function step_ref()
        pending_refs = pending_refs - 1
        if pending_refs == 0 then
            on_refs_done()
        end
    end

    -- One references request per unique position; join on a pending counter, then resolve
    -- the union once. A request that fails to send still decrements (scheduled, after the
    -- loop) so the join — and the spinner — can never stall.
    for _, qp in ipairs(positions) do
        local sent = client:request("textDocument/references", {
            textDocument = doc_params,
            position = { line = qp.line, character = qp.character },
            context = { includeDeclaration = true },
        }, function(err, locations)
            vim.schedule(function()
                if err then
                    log.warn("references request failed:", err.message or vim.inspect(err))
                end
                collect(locations)
                step_ref()
            end)
        end, bufnr)
        if not sent then
            vim.schedule(step_ref)
        end
    end
end

return M
