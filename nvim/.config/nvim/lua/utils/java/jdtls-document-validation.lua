-- Keeps diagnostics of open Java buffers current while jdtls validates only the buffer that changed.
--
-- jdtls defaults `java.edit.validateAllOpenBuffersOnChanges` to true: every change in one buffer re-validates
-- (reconciles) EVERY open working copy. Measured 2026-10-30 on a project copy with 15 open buffers (A/B harness,
-- ~/tools/tests/jdtls-ab): five edits cost 7.9 s / 7.2 s of server CPU with the default and 1.3 s / 1.4 s with
-- the setting off, while the edited buffer's own diagnostics arrived in the same ~2.1 s or sooner. The config now
-- sets the preference to false (jdtls-config-util.lua) and this module fills the gap the way vscode-java does
-- (diagnostic.ts sends `java/validateDocument` for the editor that becomes active):
--   * BufEnter: a buffer is re-validated when any attached buffer of the same client changed since the buffer was
--     last validated (its own edits count as validated: jdtls reconciles the changed document itself);
--   * BufWritePost: shortly after a save the other open buffers are re-validated too, so Trouble's workspace view
--     is consistent once the build that the save starts has the new source.
-- `java/validateDocument` is a notification jdtls honours only while validate-all is off (measured: answered in
-- 0.7-0.9 s; ignored with the default), so the setting and this module belong together.
local M = {}

M.config = {
    -- coalesce `:wa` and quick successive saves before refreshing the other buffers
    save_refresh_delay_ms = 500,
}

---@class JdtlsValidationState
---@field edits integer number of edits seen in any attached buffer of the client
---@field validated table<integer, integer> bufnr -> value of `edits` when the buffer was last validated
---@field timer uv.uv_timer_t|nil save-refresh timer
local states = {} ---@type table<integer, JdtlsValidationState>

--- Send `java/validateDocument` for the buffer and record it as current.
---@param client vim.lsp.Client
---@param st JdtlsValidationState
---@param bufnr integer
---@return boolean sent
local function send_validate(client, st, bufnr)
    if client:is_stopped() or not vim.api.nvim_buf_is_valid(bufnr) or not vim.lsp.buf_is_attached(bufnr, client.id) then
        return false
    end
    st.validated[bufnr] = st.edits
    client:notify("java/validateDocument", { textDocument = { uri = vim.uri_from_bufnr(bufnr) } })
    return true
end

--- Re-validate every tracked buffer that is behind the edit counter, except `except`.
---@param client vim.lsp.Client
---@param st JdtlsValidationState
---@param except integer|nil
local function refresh_stale(client, st, except)
    for bufnr, seen in pairs(st.validated) do
        if bufnr ~= except and seen ~= st.edits then
            send_validate(client, st, bufnr)
        end
    end
end

--- Forget a buffer; drop the client's state when it was the last one.
---@param client vim.lsp.Client
---@param st JdtlsValidationState
---@param bufnr integer
---@param group integer
local function untrack(client, st, bufnr, group)
    st.validated[bufnr] = nil
    pcall(vim.api.nvim_del_augroup_by_id, group)
    if next(st.validated) == nil then
        if st.timer then
            st.timer:stop()
            st.timer:close()
            st.timer = nil
        end
        if states[client.id] == st then
            states[client.id] = nil
        end
    end
end

--- Track a buffer attached to a jdtls client (call from LspAttach).
---@param client vim.lsp.Client
---@param bufnr integer
function M.setup(client, bufnr)
    if type(client) ~= "table" or client.name ~= "jdtls" or not vim.api.nvim_buf_is_valid(bufnr) then
        return
    end
    local st = states[client.id]
    if not st then
        st = { edits = 0, validated = {} }
        states[client.id] = st
    end
    if st.validated[bufnr] ~= nil then
        return -- already tracked (LspAttach fires again on re-attach)
    end
    st.validated[bufnr] = st.edits -- didOpen made jdtls validate it

    local group = vim.api.nvim_create_augroup(string.format("jdtls_document_validation_%d_%d", client.id, bufnr), {
        clear = true,
    })
    vim.api.nvim_create_autocmd({ "TextChanged", "TextChangedI", "TextChangedP" }, {
        group = group,
        buffer = bufnr,
        desc = "jdtls: count an edit; the changed buffer is validated by the server itself",
        callback = function()
            st.edits = st.edits + 1
            st.validated[bufnr] = st.edits
        end,
    })
    vim.api.nvim_create_autocmd("BufEnter", {
        group = group,
        buffer = bufnr,
        desc = "jdtls: re-validate a buffer that other edits may have invalidated",
        callback = function()
            if st.validated[bufnr] ~= st.edits then
                send_validate(client, st, bufnr)
            end
        end,
    })
    vim.api.nvim_create_autocmd("BufWritePost", {
        group = group,
        buffer = bufnr,
        desc = "jdtls: after a save re-validate the other open buffers",
        callback = function()
            if not st.timer then
                st.timer = vim.uv.new_timer()
            end
            st.timer:stop()
            st.timer:start(
                M.config.save_refresh_delay_ms,
                0,
                vim.schedule_wrap(function()
                    refresh_stale(client, st, bufnr)
                end)
            )
        end,
    })
    vim.api.nvim_create_autocmd({ "BufWipeout", "LspDetach" }, {
        group = group,
        buffer = bufnr,
        desc = "jdtls: stop tracking the buffer",
        callback = function(args)
            if args.event == "LspDetach" and type(args.data) == "table" and args.data.client_id ~= client.id then
                return
            end
            untrack(client, st, bufnr, group)
        end,
    })
end

--- Test seam: tracked state of a client.
---@param client_id integer
---@return JdtlsValidationState|nil
function M._state(client_id)
    return states[client_id]
end

return M
