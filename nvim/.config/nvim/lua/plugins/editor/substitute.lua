-- gbprod/substitute.nvim — register-based editing operators (vim-subversive + vim-exchange in one plugin).
-- Visual mode only: `s`/`S` in normal and operator-pending mode stay with flash.nvim (LazyVim defaults); the
-- flash spec in simple-plugins.lua only releases visual-mode `s`, so flash jump cannot extend a visual selection.
--
--   s (visual)     replace the selection with the register content, register stays intact
--   X (visual)     exchange: mark the selection, then `X` on a second selection swaps the two
--
-- Not mapped on purpose: the upstream normal-mode operators (`s{motion}`, `ss`, `S`, `sx{motion}`, `sxx`, `sxc`)
-- would collide with flash.nvim, and the range substitution (`<leader>s` upstream) is covered by `<leader>R` /
-- grug-far. Keeping `s` the only visual mapping with that prefix also makes it fire without a `timeoutlen` wait.
return {
    "gbprod/substitute.nvim",
    opts = {
        -- yank_substituted_text = true, -- swap semantics: the replaced text goes to the unnamed register
        -- preserve_cursor_position = true,
        highlight_substituted_text = {
            enabled = true,
            timer = 500,
        },
        exchange = {
            motion = false,
            use_esc_to_cancel = true,
            preserve_cursor_position = false,
        },
    },
    keys = {
        {
            "s",
            mode = "x",
            function()
                require("substitute").visual()
            end,
            desc = "Substitute (register)",
        },
        {
            "X",
            mode = "x",
            function()
                require("substitute.exchange").visual()
            end,
            desc = "Exchange",
        },
    },
}
