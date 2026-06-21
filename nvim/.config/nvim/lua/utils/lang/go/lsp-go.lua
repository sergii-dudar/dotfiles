-- Go LSP metadata: code-action title patterns that are safe to auto-resolve for
-- Go buffers.

---@type lang.LspCodeActions
local M = {}

M.code_action_auto_resolve_match_names = {
    "Convert to raw string literal",
    "Convert to interpreted string literal",
}

return M
