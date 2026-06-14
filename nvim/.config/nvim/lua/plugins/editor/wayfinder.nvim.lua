local function hl(name)
    local ok, value = pcall(vim.api.nvim_get_hl, 0, { name = name, link = false })
    return ok and value or {}
end

-- gruvbox-material palette. Pulled from the theme's own semantic groups so it
-- tracks colorscheme/variant changes, with hard-background hex fallbacks.
local function palette()
    return {
        fg = hl("Fg").fg or 0xd4be98,
        grey = hl("Grey").fg or 0x928374,
        red = hl("Red").fg or 0xea6962,
        orange = hl("Orange").fg or 0xe78a4e,
        yellow = hl("Yellow").fg or 0xd8a657,
        green = hl("Green").fg or 0xa9b665,
        aqua = hl("Aqua").fg or 0x89b482,
        blue = hl("Blue").fg or 0x7daea3,
        purple = hl("Purple").fg or 0xd3869b,
        sel_bg = hl("Visual").bg or hl("CursorLine").bg or 0x45403d,
        ctx_bg = hl("CursorLine").bg or 0x282828,
    }
end

function setup_hl()
    local p = palette()
    local set = vim.api.nvim_set_hl

    set(0, "WayfinderNormal", { default = true, link = "NormalFloat" })
    set(0, "WayfinderBorder", { fg = p.grey })
    set(0, "WayfinderTitle", { fg = p.green, bold = true })
    set(0, "WayfinderFacet", { fg = p.grey })
    set(0, "WayfinderFacetActive", { fg = p.yellow, bold = true })
    set(0, "WayfinderCount", { fg = p.grey })
    set(0, "WayfinderHeader", { fg = p.aqua, bold = true })
    set(0, "WayfinderLabel", { fg = p.fg })
    set(0, "WayfinderLabelSoft", { fg = p.grey })
    set(0, "WayfinderPath", { fg = p.grey })
    set(0, "WayfinderBadgeLsp", { fg = p.blue })
    set(0, "WayfinderBadgeText", { fg = p.grey })
    set(0, "WayfinderBadgeTest", { fg = p.green })
    set(0, "WayfinderBadgeGit", { fg = p.orange })
    set(0, "WayfinderPreviewContext", { bg = p.ctx_bg })
    set(0, "WayfinderPreviewTarget", { fg = p.yellow, bg = p.sel_bg, bold = true })
    set(0, "WayfinderTrail", { fg = p.green })
    set(0, "WayfinderDim", { fg = p.grey })

    -- Selected item row
    set(0, "WayfinderSelection", { bg = p.sel_bg })
    set(0, "WayfinderSelectionAccent", { fg = p.green, bg = p.sel_bg, bold = true })
    set(0, "WayfinderSelectionLabel", { fg = p.fg, bg = p.sel_bg, bold = true })
    set(0, "WayfinderSelectionPath", { fg = p.grey, bg = p.sel_bg })
    set(0, "WayfinderSelectionMuted", { fg = p.grey, bg = p.sel_bg })
end

-- code exploration
return {
    "error311/wayfinder.nvim",
    opts = {
        layout = {
            width = 0.95,
            height = 0.95,
            show_hints = true,
        },
    },
    keys = {
        { "<leader>wF", "<Plug>(WayfinderOpen)", desc = "Wayfinder" },
        { "<leader>wtn", "<Plug>(WayfinderTrailNext)", desc = "Wayfinder Trail Next" },
        { "<leader>wtp", "<Plug>(WayfinderTrailPrev)", desc = "Wayfinder Trail Prev" },
        { "<leader>wto", "<Plug>(WayfinderTrailOpen)", desc = "Wayfinder Trail Open" },
        { "<leader>wts", "<Plug>(WayfinderTrailShow)", desc = "Wayfinder Trail Show" },
    },
    config = function(_, opts)
        require("wayfinder").setup(opts)
        setup_hl()
    end,
}

