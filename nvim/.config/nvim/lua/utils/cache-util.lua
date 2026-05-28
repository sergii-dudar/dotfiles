-- In-memory cache tables for expensive Java operations.
-- Stores workspace/symbol lookups and javap bytecode results to avoid repeated LSP/shell calls.

local M = {}

M.java = {
    jdt_load_workspace_symbol_map = {},
    javap_results_map = {},
}

return M
