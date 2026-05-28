--- Reusable chain-aware indentation for languages with method chaining
--- (Java, Rust, C#, Kotlin, TypeScript, etc.)
---
--- • chain_indentexpr — indentexpr function that detects chaining and adjusts indent
--- • activate — set up chain-aware indentation for current buffer
---
--- Usage in after/ftplugin/<lang>.lua:
---
---   -- Java (with anonymous class + continuation indent options for cindent fallback)
---   require("utils.indent-util").activate({ cinoptions = "j1,+2s" })
---
---   -- Rust / C# / Kotlin / TypeScript (defaults are fine)
---   require("utils.indent-util").activate()

local M = {}

---@param trimmed string
---@return boolean
local function is_chain_line(trimmed)
    return trimmed:match("^%.%a") ~= nil
end

--- Scan upward to find the indent for a method chain continuation.
--- Returns the indent of existing chain lines, or chain_origin + 2*sw for a new chain.
---@param from_lnum number
---@param sw number
---@return number
local function find_chain_indent(from_lnum, sw)
    local scan = from_lnum
    local chain_indent = nil
    while scan > 0 do
        local trimmed = vim.fn.trim(vim.fn.getline(scan))
        if is_chain_line(trimmed) then
            chain_indent = vim.fn.indent(scan)
            scan = vim.fn.prevnonblank(scan - 1)
        else
            return chain_indent or (vim.fn.indent(scan) + sw * 2)
        end
    end
    return chain_indent or 0
end

--- Chain-aware indentexpr function.
--- Handles method chain continuations (.method().method()...),
--- falls back to cindent() for everything else.
---
--- Referenced via: v:lua.require'utils.indent-util'.chain_indentexpr()
---@return number
function M.chain_indentexpr()
    local lnum = vim.v.lnum
    local prev_lnum = vim.fn.prevnonblank(lnum - 1)
    if prev_lnum == 0 then
        return 0
    end

    local sw = vim.bo.shiftwidth
    local prev_indent = vim.fn.indent(prev_lnum)
    local prev_trimmed = vim.fn.trim(vim.fn.getline(prev_lnum))
    local curr_trimmed = vim.fn.trim(vim.fn.getline(lnum))

    -- Current line starts with '.method(' → align with existing chain
    if is_chain_line(curr_trimmed) then
        return find_chain_indent(prev_lnum, sw)
    end

    -- Previous line is a chain member (starts with '.method')
    if is_chain_line(prev_trimmed) then
        if prev_trimmed:match(";%s*$") then
            -- Chain ended with ';' → delegate to cindent for next statement
            return vim.fn.cindent(lnum)
        end
        -- Chain still open → same indent for next .method() call
        return prev_indent
    end

    -- Everything else (braces, keywords, etc.) → cindent
    return vim.fn.cindent(lnum)
end

--- Activate chain-aware indentation for the current buffer.
--- Deferred with vim.schedule to run after LazyVim's treesitter indentexpr override.
---@param opts? { cinoptions?: string }
function M.activate(opts)
    opts = opts or {}
    local buf = vim.api.nvim_get_current_buf()
    vim.schedule(function()
        if vim.api.nvim_buf_is_valid(buf) then
            vim.api.nvim_buf_call(buf, function()
                vim.bo.indentexpr = "v:lua.require'utils.indent-util'.chain_indentexpr()"
                if opts.cinoptions then
                    vim.bo.cinoptions = opts.cinoptions
                end
                vim.opt_local.indentkeys:append("0.")
            end)
        end
    end)
end

return M
