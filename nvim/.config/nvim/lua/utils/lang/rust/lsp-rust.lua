-- Rust LSP metadata: code-action title patterns that are safe to auto-resolve
-- for Rust buffers.

---@type lang.LspCodeActions
local M = {}

M.code_action_auto_resolve_match_names = {
    "Rewrite as raw string",
    "Rewrite as regular string",
}

return M
