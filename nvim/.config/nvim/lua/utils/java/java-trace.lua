-- Java stack trace utilities: parse, highlight, and navigate stack traces.
-- Supports visual selection, buffer-wide, and single-line trace parsing to quickfix list.
--
-- • highlight_java_test_trace_lines — highlight trace lines in buffer with extmarks
-- • highlight_java_test_trace_text / _current_buf / _selected — highlight variants
-- • show_stack_trace_qflist — display parsed trace entries in quickfix
-- • parse_selected_trace_to_qflist — parse visual selection trace to qflist
-- • parse_buffer_trace_to_qflist — parse full buffer trace to qflist
-- • parse_current_line_trace_to_qflist — parse single line trace to qflist
-- • parse_trace_under_cursor_and_open_in_buffer — open trace class via JDTLS

local M = {}

local stacktrace_ns = vim.api.nvim_create_namespace("StackTraceNs")
local common = require("utils.common-util")
local java_util = require("utils.java.java-common")
local jdtls_util = require("utils.java.jdtls-util")
local lsp_util = require("utils.lsp-util")

local project_link_color = "#1E90FF"
local ex_link_color = "#8A2BE2"
vim.api.nvim_set_hl(0, "TraceProjectClassHl", {
    fg = project_link_color,
    underline = true,
    bold = true,
})
vim.api.nvim_set_hl(0, "TraceNonProjectClassHl", {
    fg = ex_link_color,
    underline = true,
    bold = true,
})

--- Highlight stack trace class references in the given lines.
function M.highlight_java_test_trace_lines(lines, trace_selected_line)
    trace_selected_line = (trace_selected_line and trace_selected_line - 1) or 0
    -- vim.notify("selected_line: " .. trace_selected_line)
    for i, line in ipairs(lines) do
        local trace = java_util.parse_java_class_trace_line(line)
        if trace then
            local line_ix = i - 1 + trace_selected_line
            local file_path = java_util.java_class_to_proj_path(trace.class_path)
            local hl_group = file_path and "TraceProjectClassHl" or "TraceNonProjectClassHl"
            -- local buffer = vim.api.nvim_get_current_buf()
            vim.api.nvim_buf_set_extmark(0, stacktrace_ns, line_ix, trace.line_start_position, {
                end_line = line_ix, -- only current line
                end_col = trace.line_end_position,
                hl_group = hl_group,
            })
        end
    end
end

---@param text string
function M.highlight_java_test_trace_text(text, selected_line)
    local lines = {}
    for line in text:gmatch("[^\n]+") do
        table.insert(lines, line)
    end

    M.highlight_java_test_trace_lines(lines, selected_line)
end

--- Highlight stack trace references in a buffer.
function M.highlight_java_test_trace(buffer)
    if vim.api.nvim_buf_is_valid(buffer) then
        local lines = vim.api.nvim_buf_get_lines(buffer, 0, -1, false)
        M.highlight_java_test_trace_lines(lines)
    end
end

--- Highlight stack trace references in the current buffer.
function M.highlight_java_test_trace_current_buf()
    M.highlight_java_test_trace(vim.api.nvim_get_current_buf())
end

--- Highlight stack trace references in the visual selection.
function M.highlight_java_trace_selected()
    local stack_trace = common.get_visual_selection()
    M.highlight_java_test_trace_text(stack_trace, vim.fn.getpos("v")[2])
end

--- Resolve an LSP location URI (file:// or jdt://) to a *loaded* buffer plus a
--- safe (in-range) line number, for use as a quickfix `bufnr` entry.
---
--- Why pre-load instead of passing the URI as a qf `filename`:
---   • jdt:// is a pseudo-path. Trouble's qf source drops entries whose filename
---     Vim can't resolve to a real file (valid == 0) on every list refresh, which
---     manifested as the quickfix list "losing" its library frames on round-trip.
---   • Trouble derives the filetype via vim.filetype.match() on the raw jdt:// string
---     for unloaded buffers, mis-detecting it (e.g. as `stata`) → missing TS parser.
---   • The trace line can exceed a decompiled class's length → "Invalid cursor line".
--- Loading the buffer up-front (nvim-jdtls' BufReadCmd fetches source/decompiled
--- content synchronously and sets filetype=java) sidesteps all three: Trouble sees a
--- real, loaded `java` buffer and a clamped, in-range line.
---@param uri string
---@param line integer 1-based line from the stack trace
---@return integer|nil bufnr, integer lnum
local function resolve_uri_to_buf(uri, line)
    local bufnr = vim.uri_to_bufnr(uri)
    if not vim.api.nvim_buf_is_loaded(bufnr) then
        pcall(vim.fn.bufload, bufnr)
    end
    if not vim.api.nvim_buf_is_loaded(bufnr) then
        return nil, line
    end
    -- Trouble's main-window detection (view/main.lua `_valid`) rejects any window
    -- whose buffer has a non-empty 'buftype'. nvim-jdtls marks jdt:// (decompiled)
    -- buffers as `nofile`, so once Trouble points the main window at one it can no
    -- longer find a valid jump target and starts loading the *next* selected item
    -- into the Trouble (qflist) window itself. Presenting the decompiled buffer as a
    -- normal, file-less, read-only buffer keeps that window a valid jump target so
    -- navigation round-trips cleanly. (file:// source buffers are already buftype="".)
    if vim.bo[bufnr].buftype == "nofile" then
        pcall(function()
            vim.bo[bufnr].buftype = ""
            vim.bo[bufnr].swapfile = false
            vim.bo[bufnr].modified = false
        end)
    end
    local line_count = vim.api.nvim_buf_line_count(bufnr)
    if line_count > 0 and line > line_count then
        line = line_count
    end
    return bufnr, line
end

---@type parce java trace to locations (local project files and load from jdtls in case lib refs)
local parse_java_stack_trace = function(trace, result_callback)
    local loc_result_items_map = {}
    local jdt_classes = {}

    -- for i, parsed in ipairs(java_util.parse_java_mvn_run_class_text(trace)) do
    local parsed_trace = java_util.parse_java_mvn_run_class_text(trace)
    for i, parsed in ipairs(parsed_trace) do
        local file_path = java_util.java_class_to_proj_path(parsed.class_path)
        if file_path then
            loc_result_items_map[i] = {
                filename = file_path,
                lnum = parsed.class_line_number,
                end_lnum = parsed.class_line_number,
                col = 1,
            }
        else
            table.insert(jdt_classes, parsed.class_path)
        end
    end

    if vim.tbl_isempty(jdt_classes) then
        local loc_result_items = {}
        local n = 0
        for i, parsed in ipairs(parsed_trace) do
            local loc_item = loc_result_items_map[i]
            if loc_item then
                n = n + 1
                loc_item.text = string.format("( %s ) %s", n, parsed.method)
                table.insert(loc_result_items, loc_item)
            end
        end
        result_callback(loc_result_items)
    else
        jdtls_util.jdt_load_unique_class_list(jdt_classes, function(jdt_results_items_map)
            local all_result_items = {}
            local n = 0
            for i, parsed in ipairs(parsed_trace) do
                local loc_item = loc_result_items_map[i]
                local jdt_sym_loc_item = jdt_results_items_map[parsed.class_path]
                if loc_item then
                    n = n + 1
                    loc_item.text = string.format("( %s ) %s", n, parsed.method)
                    table.insert(all_result_items, loc_item)
                elseif jdt_sym_loc_item and jdt_sym_loc_item.location then
                    n = n + 1
                    -- Resolve the library class to a loaded buffer and reference it by `bufnr`
                    -- (not a raw jdt:// `filename`) so the quickfix entry survives Trouble's
                    -- list refreshes, gets the right `java` filetype, and lands on an in-range
                    -- line. The line itself comes from the parsed trace (clamped). See
                    -- resolve_uri_to_buf() for the rationale. nvim 0.12's
                    -- vim.lsp.util.symbols_to_items() is intentionally avoided here: it requires a
                    -- position encoding and otherwise asserts an LSP client on the current buffer,
                    -- which the trace/qflist buffer never has.
                    local bufnr, lnum = resolve_uri_to_buf(jdt_sym_loc_item.location.uri, parsed.class_line_number)
                    local jdt_item = {
                        lnum = lnum,
                        end_lnum = lnum,
                        col = 1,
                        end_col = 1,
                        valid = 1,
                        text = string.format("( %s ) %s.%s", n, parsed.class_path, parsed.method),
                    }
                    if bufnr then
                        jdt_item.bufnr = bufnr
                    else
                        -- Couldn't load (no jdtls / fetch failed): fall back to the resolved path.
                        jdt_item.filename = vim.uri_to_fname(jdt_sym_loc_item.location.uri)
                    end
                    table.insert(all_result_items, jdt_item)
                else
                    vim.notify(string.format("⚠️ class %s was not found", parsed.class_path))
                end
            end
            -- dd(all_result_items)
            result_callback(all_result_items)
        end)
    end
end

--- Show parsed stack trace entries in the quickfix list.
function M.show_stack_trace_qflist(stack_trace)
    parse_java_stack_trace(stack_trace, function(trace_items)
        -- dd(trace_items)
        vim.fn.setqflist({}, "r", { title = "Trace Quickfix List", items = trace_items })
        vim.cmd("Trouble qflist toggle sort={}")
    end)
end

--- Parse the selected stack trace into the quickfix list.
function M.parse_selected_trace_to_qflist()
    local stack_trace = common.get_visual_selection()
    M.show_stack_trace_qflist(stack_trace)
end

--- Parse the current buffer stack trace into the quickfix list.
function M.parse_buffer_trace_to_qflist()
    local stack_trace = common.get_current_buffer_text()
    M.show_stack_trace_qflist(stack_trace)
end

--- Parse the current line stack trace into the quickfix list.
function M.parse_current_line_trace_to_qflist()
    local stack_trace = common.get_line_under_cursor()
    M.show_stack_trace_qflist(stack_trace)
end

--- Find a normal editing window (listed, normal buftype, not current)
local function find_edit_win()
    local cur_win = vim.api.nvim_get_current_win()
    for _, win in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
        if win ~= cur_win then
            local buf = vim.api.nvim_win_get_buf(win)
            local bt = vim.bo[buf].buftype
            if bt == "" and vim.bo[buf].buflisted then
                return win
            end
        end
    end
    return nil
end

--- Open the trace entry under the cursor in a buffer.
function M.parse_trace_under_cursor_and_open_in_buffer()
    local trace_line = common.get_line_under_cursor()
    local parsed = java_util.parse_java_class_trace_line(trace_line)
    if not parsed then
        vim.notify(string.format("⚠️ cant parse java class from line:\n%s", trace_line))
        return
    end

    local edit_win = find_edit_win()
    if not edit_win then
        vim.cmd("above split")
        edit_win = vim.api.nvim_get_current_win()
    else
        vim.api.nvim_set_current_win(edit_win)
    end

    local file_path = java_util.java_class_to_proj_path(parsed.class_path)
    if file_path then
        vim.cmd(string.format("edit +%d %s", parsed.class_line_number, file_path))
    else
        local jdtls_client_id = lsp_util.get_client_id_by_name("jdtls")
        if jdtls_client_id then
            local current_buf_id = vim.api.nvim_get_current_buf()
            if not vim.lsp.buf_is_attached(current_buf_id, jdtls_client_id) then
                vim.lsp.buf_attach_client(current_buf_id, jdtls_client_id)
            end
            jdtls_util.jdt_open_class(parsed.class_path, parsed.class_line_number)
        else
            vim.notify(string.format("⚠️ JDTLS is not running to open %s", parsed.class_path))
        end
    end
end

return M
