return {
    {
        "leath-dub/snipe.nvim",
        -- replaced in snasks
        -- stylua: ignore
        keys = {
            -- { "<leader>,", function() require("snipe").open_buffer_menu() end, desc = "Open Snipe buffer menu", },
        },
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
                -- dictionary = "asfghl;wertyuiop",
                dictionary = "1234567890sadflewcmpghio",
            },
            navigate = {
                -- In case you changed your mind, provide a keybind that lets you
                -- cancel the snipe and close the window.
                cancel_snipe = "<esc>",
                -- cancel_snipe = "q",

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
    },
    {
        "chrisgrieser/nvim-early-retirement",
        config = true,
        event = "VeryLazy",
        opts = {
            -- If a buffer has been inactive for this many minutes, close it.
            retirementAgeMins = 5, -- 20,

            -- Filetypes to ignore.
            ignoredFiletypes = {},

            -- Ignore files matching this lua pattern; empty string disables this setting.
            ignoreFilenamePattern = "",

            -- Will not close the alternate file.
            ignoreAltFile = true,

            -- Minimum number of open buffers for auto-closing to become active. E.g.,
            -- by setting this to 4, no auto-closing will take place when you have 3
            -- or fewer open buffers. Note that this plugin never closes the currently
            -- active buffer, so a number < 2 will effectively disable this setting.
            minimumBufferNum = 5,

            -- Ignore buffers with unsaved changes. If false, the buffers will
            -- automatically be written and then closed.
            ignoreUnsavedChangesBufs = true,

            -- Ignore non-empty buftypes, for example terminal buffers
            ignoreSpecialBuftypes = true,

            -- Ignore visible buffers. Buffers that are open in a window or in a tab
            -- are considered visible by vim. ("a" in `:buffers`)
            ignoreVisibleBufs = true,

            -- ignore unloaded buffers. Session-management plugin often add buffers
            -- to the buffer list without loading them.
            ignoreUnloadedBufs = false,

            -- Show notification on closing. Works with plugins like nvim-notify.
            notificationOnAutoClose = false,

            -- When a file is deleted, for example via an external program, delete the
            -- associated buffer as well. Requires Neovim >= 0.10.
            -- (This feature is independent from the automatic closing)
            deleteBufferWhenFileDeleted = false,
        },
    },
}
