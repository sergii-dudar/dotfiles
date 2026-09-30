--- jdtls clean-ups on demand.
---
--- jdtls runs the clean-ups listed in `java.cleanup.actions` (see jdtls-config-util) in two places: on save when
--- `java.saveActions.cleanup` is on, and for the client-specific request `java/cleanup`, which returns a
--- WorkspaceEdit without touching the buffer. This module uses the request to offer three things:
---
---   * `M.preview()`  - Snacks picker with one item per hunk (diff preview), <CR> applies all, <Esc> discards
---   * `M.apply()`    - apply the clean-up straight away (one undo step)
---   * on save        - notification with the number of possible changes and a short summary per hunk
---
--- jdtls merges every enabled clean-up into a single edit, so the individual clean-up names are not available
--- per hunk; the summary shows the first changed line of each hunk instead.
local M = {}

local TITLE = "Java clean up"

M.config = {
    -- Report possible clean-ups after every write of a Java buffer with jdtls attached.
    notify_on_save = true,
    -- Hunk summaries listed in the on-save notification.
    notify_max_hunks = 5,
    -- Context lines around each hunk in the diff preview.
    context = 3,
    -- Longest hunk summary shown in the picker list / notification.
    summary_width = 80,
}

---@class JdtlsCleanupHunk
---@field lnum integer first changed line (1-based, in the current buffer)
---@field summary string first changed line, trimmed
---@field text string unified diff of this hunk, including a file header
---@field old_first integer first replaced line (1-based); for a pure insertion the line the new lines go before
---@field old_count integer number of replaced lines (0 for a pure insertion)
---@field replacement string[] lines that take the place of the replaced ones

---@class JdtlsCleanupResult
---@field bufnr integer
---@field changedtick integer buffer tick when the request was sent
---@field offset_encoding string
---@field edits lsp.TextEdit[] edits returned by jdtls (normally one whole-document edit)
---@field old_lines string[] buffer content when the request was sent
---@field new_lines string[] buffer content after the clean-up
---@field hunks JdtlsCleanupHunk[]
---@field diff string unified diff of everything, including a file header

local in_flight = {} ---@type table<integer, boolean>

local function notify(msg, level)
    vim.notify(msg, level or vim.log.levels.INFO, { title = TITLE })
end

local function jdtls_client(bufnr)
    return vim.lsp.get_clients({ bufnr = bufnr, name = "jdtls" })[1]
end

local function trim_summary(line)
    line = vim.trim(line)
    local width = M.config.summary_width
    if vim.fn.strdisplaywidth(line) > width then
        line = vim.fn.strcharpart(line, 0, width - 1) .. "…"
    end
    return line
end

--- Apply LSP text edits to a copy of `old_lines` and return the resulting lines (the buffer is untouched).
---@param old_lines string[]
---@param edits lsp.TextEdit[]
---@param offset_encoding string
---@return string[]
function M.preview_lines(old_lines, edits, offset_encoding)
    local scratch = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_lines(scratch, 0, -1, false, old_lines)
    vim.lsp.util.apply_text_edits(edits, scratch, offset_encoding)
    local new_lines = vim.api.nvim_buf_get_lines(scratch, 0, -1, false)
    vim.api.nvim_buf_delete(scratch, { force = true })
    return new_lines
end

--- Diff old vs. new lines into hunks. Each hunk is one raw change group (no merging of nearby changes, so
--- three separate simplifications stay three hunks) rendered as a unified diff with context lines.
---@param old_lines string[]
---@param new_lines string[]
---@param name string file name used in the diff header
---@return JdtlsCleanupHunk[] hunks
---@return string diff the whole unified diff with header
function M.hunks(old_lines, new_lines, name)
    local old_text = table.concat(old_lines, "\n") .. "\n"
    local new_text = table.concat(new_lines, "\n") .. "\n"
    local ctx = M.config.context
    local header = ("--- a/%s\n+++ b/%s\n"):format(name, name)
    local unified = vim.diff(old_text, new_text, { result_type = "unified", ctxlen = ctx }) --[[@as string]]
    local groups = vim.diff(old_text, new_text, { result_type = "indices" }) --[[@as integer[][] ]]
    local hunks = {} ---@type JdtlsCleanupHunk[]
    for _, group in ipairs(groups) do
        local start_a, count_a, start_b, count_b = group[1], group[2], group[3], group[4]
        -- with count 0 the start is the line *after which* the change happens
        local old_first = count_a > 0 and start_a or start_a + 1
        local old_last = count_a > 0 and start_a + count_a - 1 or start_a
        local new_first = count_b > 0 and start_b or start_b + 1
        local before_start = math.max(1, old_first - ctx)
        local after_end = math.min(#old_lines, old_last + ctx)
        local lines = {}
        for i = before_start, old_first - 1 do
            lines[#lines + 1] = " " .. old_lines[i]
        end
        local summary
        for i = old_first, old_last do
            lines[#lines + 1] = "-" .. old_lines[i]
            summary = summary or trim_summary(old_lines[i])
        end
        for i = start_b, start_b + count_b - 1 do
            lines[#lines + 1] = "+" .. new_lines[i]
            summary = summary or trim_summary(new_lines[i])
        end
        for i = old_last + 1, after_end do
            lines[#lines + 1] = " " .. old_lines[i]
        end
        local n_before, n_after = old_first - before_start, after_end - old_last
        local hunk_header = ("@@ -%d,%d +%d,%d @@"):format(
            before_start,
            n_before + count_a + n_after,
            new_first - n_before,
            n_before + count_b + n_after
        )
        hunks[#hunks + 1] = {
            lnum = old_first,
            summary = summary or "",
            text = header .. hunk_header .. "\n" .. table.concat(lines, "\n") .. "\n",
            old_first = old_first,
            old_count = count_a,
            replacement = vim.list_slice(new_lines, start_b, start_b + count_b - 1),
        }
    end
    return hunks, header .. unified
end

--- Ask jdtls for the clean-up of a buffer without applying it.
---@param bufnr integer
---@param cb fun(result: JdtlsCleanupResult|nil, err: string|nil)
function M.request(bufnr, cb)
    local client = jdtls_client(bufnr)
    if not client then
        return cb(nil, "jdtls is not attached to this buffer")
    end
    local uri = vim.uri_from_bufnr(bufnr)
    local changedtick = vim.api.nvim_buf_get_changedtick(bufnr)
    local ok = client:request("java/cleanup", { uri = uri }, function(err, result)
        if err then
            return cb(nil, "java/cleanup failed: " .. tostring(err.message or err))
        end
        if not vim.api.nvim_buf_is_valid(bufnr) then
            return cb(nil, "buffer is gone")
        end
        local edits = result and result.changes and result.changes[uri] or nil
        if not edits and result and result.changes then
            -- jdtls may echo the uri with a different encoding; there is only ever one document in the answer
            for _, document_edits in pairs(result.changes) do
                edits = document_edits
            end
        end
        edits = edits or {}
        local old_lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
        local new_lines = #edits > 0 and M.preview_lines(old_lines, edits, client.offset_encoding) or old_lines
        local name = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(bufnr), ":t")
        local hunks, diff = M.hunks(old_lines, new_lines, name)
        cb({
            bufnr = bufnr,
            changedtick = changedtick,
            offset_encoding = client.offset_encoding,
            edits = edits,
            old_lines = old_lines,
            new_lines = new_lines,
            hunks = hunks,
            diff = diff,
        })
    end, bufnr)
    if not ok then
        cb(nil, "jdtls did not accept the java/cleanup request")
    end
end

--- A result can only be applied to the buffer it was computed from, unchanged since.
---@param result JdtlsCleanupResult
---@return boolean
local function still_applicable(result)
    if not vim.api.nvim_buf_is_valid(result.bufnr) then
        notify("Buffer is gone, nothing applied", vim.log.levels.WARN)
        return false
    end
    -- the tick also moves on undo/redo, so fall back to comparing the content when it differs
    if
        vim.api.nvim_buf_get_changedtick(result.bufnr) ~= result.changedtick
        and not vim.deep_equal(vim.api.nvim_buf_get_lines(result.bufnr, 0, -1, false), result.old_lines)
    then
        notify("Buffer changed since the clean-up was computed, run it again", vim.log.levels.WARN)
        return false
    end
    if #result.hunks == 0 then
        notify("Nothing to clean up")
        return false
    end
    return true
end

--- Apply a previously requested result as a whole. Refuses when the buffer changed in between.
---@param result JdtlsCleanupResult
---@return boolean applied
function M.apply_result(result)
    if not still_applicable(result) then
        return false
    end
    vim.lsp.util.apply_text_edits(result.edits, result.bufnr, result.offset_encoding)
    notify(("Applied %d change(s), undo with u"):format(#result.hunks))
    return true
end

--- Text edit replacing the old lines `first .. first + count - 1` (1-based, count 0 = insert before `first`) with
--- `replacement`. A change reaching the end of the buffer is anchored on the end of the previous line, because a
--- range ending on the line after the last one is not a valid position.
---@param old_lines string[]
---@param first integer
---@param count integer
---@param replacement string[]
---@param offset_encoding string
---@return lsp.TextEdit
local function line_range_edit(old_lines, first, count, replacement, offset_encoding)
    local total = #old_lines
    local end_line = first - 1 + count -- 0-based, exclusive
    if end_line < total then
        return {
            range = { start = { line = first - 1, character = 0 }, ["end"] = { line = end_line, character = 0 } },
            newText = #replacement > 0 and (table.concat(replacement, "\n") .. "\n") or "",
        }
    end
    local last_len = vim.str_utfindex(old_lines[total], offset_encoding)
    if first <= 1 then
        return {
            range = { start = { line = 0, character = 0 }, ["end"] = { line = total - 1, character = last_len } },
            newText = table.concat(replacement, "\n"),
        }
    end
    local prev_len = vim.str_utfindex(old_lines[first - 1], offset_encoding)
    return {
        range = {
            start = { line = first - 2, character = prev_len },
            ["end"] = { line = total - 1, character = last_len },
        },
        newText = #replacement > 0 and ("\n" .. table.concat(replacement, "\n")) or "",
    }
end

--- Apply only some hunks of a result (built from the raw change groups, one undo step).
---@param result JdtlsCleanupResult
---@param hunks JdtlsCleanupHunk[]
---@return boolean applied
function M.apply_hunks(result, hunks)
    if #hunks == 0 or not still_applicable(result) then
        return false
    end
    local edits = {} ---@type lsp.TextEdit[]
    for _, hunk in ipairs(hunks) do
        edits[#edits + 1] =
            line_range_edit(result.old_lines, hunk.old_first, hunk.old_count, hunk.replacement, result.offset_encoding)
    end
    vim.lsp.util.apply_text_edits(edits, result.bufnr, result.offset_encoding)
    notify(("Applied %d of %d change(s), undo with u"):format(#hunks, #result.hunks))
    return true
end

--- Apply the clean-up to a buffer right away.
---@param bufnr? integer defaults to the current buffer
function M.apply(bufnr)
    bufnr = bufnr or vim.api.nvim_get_current_buf()
    M.request(bufnr, function(result, err)
        if err then
            return notify(err, vim.log.levels.WARN)
        end
        M.apply_result(result)
    end)
end

--- Show the clean-up as a diff picker; <CR> applies everything, closing the picker discards it.
---@param bufnr? integer defaults to the current buffer
function M.preview(bufnr)
    bufnr = bufnr or vim.api.nvim_get_current_buf()
    M.request(bufnr, function(result, err)
        if err then
            return notify(err, vim.log.levels.WARN)
        end
        if #result.hunks == 0 then
            return notify("Nothing to clean up")
        end
        local name = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(bufnr), ":t")
        local items = {
            { text = "all changes", label = "ALL", summary = ("%d hunk(s)"):format(#result.hunks), diff = result.diff },
        }
        for _, hunk in ipairs(result.hunks) do
            items[#items + 1] = {
                text = ("L%d %s"):format(hunk.lnum, hunk.summary),
                label = ("L%d"):format(hunk.lnum),
                summary = hunk.summary,
                diff = hunk.text,
                hunk = hunk,
            }
        end
        Snacks.picker.pick({
            title = ("%s: %d change(s) in %s  <CR> apply current/marked (<Tab> marks), ALL = everything, <Esc> discard"):format(
                TITLE,
                #result.hunks,
                name
            ),
            items = items,
            layout = { preset = "custom_vertical", layout = { width = 0, height = 0, [2] = { height = 0.85 } } },
            preview = "diff",
            format = function(item)
                return {
                    { ("%-6s"):format(item.label), "SnacksPickerLabel" },
                    { item.summary, "SnacksPickerComment" },
                }
            end,
            confirm = function(picker)
                local selected = picker:selected({ fallback = true })
                picker:close()
                local chosen, all = {}, #selected == 0
                for _, item in ipairs(selected) do
                    if item.hunk then
                        chosen[#chosen + 1] = item.hunk
                    else
                        all = true
                    end
                end
                if all or #chosen == #result.hunks then
                    M.apply_result(result)
                else
                    M.apply_hunks(result, chosen)
                end
            end,
        })
    end)
end

--- Report possible clean-ups after a write (silent when there are none or jdtls is not attached).
---@param bufnr integer
function M.notify_on_save(bufnr)
    if not M.config.notify_on_save or in_flight[bufnr] or not jdtls_client(bufnr) then
        return
    end
    in_flight[bufnr] = true
    M.request(bufnr, function(result, err)
        in_flight[bufnr] = nil
        if err or #result.hunks == 0 then
            return
        end
        local lines = { ("%d possible change(s): <leader>jcu to review, <leader>jcU to apply"):format(#result.hunks) }
        local shown = math.min(#result.hunks, M.config.notify_max_hunks)
        for i = 1, shown do
            lines[#lines + 1] = ("L%-5d %s"):format(result.hunks[i].lnum, result.hunks[i].summary)
        end
        if #result.hunks > shown then
            lines[#lines + 1] = ("… %d more"):format(#result.hunks - shown)
        end
        notify(table.concat(lines, "\n"))
    end)
end

--- User commands and the on-save autocmd. Called once from the Java plugin config.
function M.setup()
    local group = vim.api.nvim_create_augroup("JavaCleanupOnSave", { clear = true })
    vim.api.nvim_create_autocmd("BufWritePost", {
        group = group,
        pattern = "*.java",
        desc = "Java clean up: report possible changes",
        callback = function(args)
            M.notify_on_save(args.buf)
        end,
    })
    vim.api.nvim_create_user_command("JavaCleanup", function(opts)
        if opts.bang then
            M.apply()
        else
            M.preview()
        end
    end, { bang = true, desc = "jdtls clean-ups (java.cleanup.actions): preview as diff, or apply with !" })
    vim.api.nvim_create_user_command("JavaCleanupNotifyToggle", function()
        M.config.notify_on_save = not M.config.notify_on_save
        notify("On-save report " .. (M.config.notify_on_save and "enabled" or "disabled"))
    end, { desc = "Toggle the on-save report of possible jdtls clean-ups" })
end

return M
