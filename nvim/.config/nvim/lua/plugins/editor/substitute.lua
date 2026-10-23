-- gbprod/substitute.nvim — register-based editing operators (vim-subversive + vim-exchange in one plugin).
-- TRIAL next to flash.nvim: flash's `s`/`S` are disabled in simple-plugins.lua for the time being
-- (flash jump moved to <CR>, treesitter selection stays on <C-Space>); revert there if the trial fails.
--
--   s{motion} / ss / S / s (visual)    replace the text with the register content, register stays intact
--   sx{motion} sx{motion} / sxx / X    exchange the two regions (sxc cancels a pending exchange)
--   sR{motion1}{motion2} / sRR{motion} / sR (visual)
--                                      :s over the range of the last motion, search prefilled with the
--                                      text under the first motion / the word under cursor / the selection
--   `sR` is used instead of the upstream `<leader>s` default because `<leader>s` is the search group.
return {
    "gbprod/substitute.nvim",
    opts = {
        -- yank_substituted_text = true, -- swap semantics: the replaced text goes to the unnamed register
        -- preserve_cursor_position = true,
        highlight_substituted_text = {
            enabled = true,
            timer = 500,
        },
        range = {
            prefix = "s", -- :s command used for the range substitution ("S" would pick vim-abolish)
            prompt_current_text = false, -- prefill the replacement with the searched text
            confirm = false, -- add the `c` flag to the :s command
            complete_word = false, -- wrap the search in \< \>
        },
        exchange = {
            motion = false,
            use_esc_to_cancel = true,
            preserve_cursor_position = false,
        },
    },
    -- stylua: ignore
    keys = {
        -- substitute
        {"s", function() require("substitute").operator() end, desc = "Substitute (register)",},
        {"ss", function() require("substitute").line() end, desc = "Substitute line (register)",},
        {"S", function() require("substitute").eol() end, desc = "Substitute to EOL (register)",},
        {"s", mode = "x", function() require("substitute").visual() end, desc = "Substitute (register)",},
        -- exchange
        {"sx", function() require("substitute.exchange").operator() end, desc = "Exchange",},
        {"sxx", function() require("substitute.exchange").line() end, desc = "Exchange line",},
        {"X", mode = "x", function() require("substitute.exchange").visual() end, desc = "Exchange",},
        {"sxc", function() require("substitute.exchange").cancel() end, desc = "Exchange cancel",},
        -- substitute over range
        {"sR", function() require("substitute.range").operator() end, desc = "Substitute over range",},
        {"sRR", function() require("substitute.range").word() end, desc = "Substitute word over range",},
        {"sR", mode = "x", function() require("substitute.range").visual() end, desc = "Substitute selection over range",},
    },
}
