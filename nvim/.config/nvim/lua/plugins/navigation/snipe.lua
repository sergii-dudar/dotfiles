return {
    "leath-dub/snipe.nvim",
    -- replaced in snasks
    -- keys = {
    --     {
    --         "<leader>,",
    --         function()
    --             require("snipe").open_buffer_menu()
    --         end,
    --         desc = "Open Snipe buffer menu",
    --     },
    -- },
    opts = {
        ui = {
            position = "center", -- "topleft"|"bottomleft"|"topright"|"bottomright"|"center"|"cursor"
            open_win_override = {
                title = "Buffers",
                border = "rounded", -- single|rounded
            },
            preselect_current = false,
            text_align = "file-first", -- "left"|"right"|"file-first"
        },
        hints = {
            -- Charaters to use for hints
            -- make sure they don't collide with the navigation keymaps
            -- If you remove `j` and `k` from below, you can navigate in the plugin
            -- dictionary = "sadflewcmpghio",
            dictionary = "asfghl;wertyuiop",
        },
        navigate = {
            -- In case you changed your mind, provide a keybind that lets you
            -- cancel the snipe and close the window.
            -- cancel_snipe = "<esc>",
            cancel_snipe = "q",

            -- Remove "j" and "k" from your dictionary to navigate easier to delete
            -- Close the buffer under the cursor
            -- NOTE: Make sure you don't use the character below on your dictionary
            close_buffer = "d",
            next_page = "J",
            prev_page = "K",
        },
        -- Define the way buffers are sorted by default
        -- Can be any of "default" (sort buffers by their number) or "last" (sort buffers by last accessed)
        -- If you choose "last", it will be modifying sorting the boffers by last
        -- accessed, so the "a" will always be assigned to your latest accessed
        -- buffer
        -- If you want the letters not to change, leave the sorting at default
        sort = "default",
    },
}