-- Multiple cursors, IntelliJ-style (clone caret below/above, column selection -> carets,
-- "add selection for next occurrence").
--
-- Chosen over `mg979/vim-visual-multi` and `smoka7/multicursors.nvim`: its cursors are real Neovim
-- cursors running real commands, so LSP / treesitter / blink.cmp / undo / dot-repeat / macros all
-- work per cursor. It also has a "keymap layer" concept - extra mappings exist only while a buffer
-- actually has cursors - which matters here because the leader and <C-*> space is already full.
--
-- Why these keys (checked against this config, AeroSpace and tmux):
--   * `<M-h/j/k/l>` are `mini.move` (LazyVim `editor.mini-move` extra), so cloning uses the shifted
--     pair `<M-J>` / `<M-K>`, which reads nicely as "move line" + shift.
--   * `<C-n>` / `<C-p>` are harpoon (`plugins/navigation/harpoon.lua`), `<C-h/j/k/l>` are
--     vim-tmux-navigator, `<C-d>` / `<C-u>` are centered scroll, `<C-]>` is toggle.nvim,
--     `<C-s>` is save-all, `<C-a>` is select-all - none of them are reused below.
--   * On macOS AeroSpace owns `alt-{e,f,s,w,tab,enter,equal,minus,comma,period,slash,1-9}` and
--     `alt-shift-{e,f,h,r,semicolon,tab,1-9}` in `mode.main`; nothing below collides.
--     `alt-shift-j` / `alt-shift-k` are bound by AeroSpace only inside `mode.service`, so in normal
--     use they pass straight through to the terminal.
--   * tmux has no active `M-` bindings (`tmux/.tmux.conf:136-139` are commented out), and
--     ghostty / kitty / alacritty all send Option as Alt.
--
-- Note: plain `<C-v>` + `I` / `A` / `$A` / `c` still works and is often enough for pure column
-- edits. This plugin is for the cases where each cursor needs its own motion (`ciw` over words of
-- different length, `.` repeat, LSP-aware edits at every caret).
return {
    "jake-stewart/multicursor.nvim",
    branch = "1.0", -- pinned as recommended by upstream README
    -- stylua: ignore
    keys = {
        -- Clone the cursor onto the line below/above at the same column (IntelliJ "clone caret").
        { "<M-J>", function() require("multicursor-nvim").lineAddCursor(1) end, mode = { "n", "x" }, desc = "Multicursor: add cursor below" },
        { "<M-K>", function() require("multicursor-nvim").lineAddCursor(-1) end, mode = { "n", "x" }, desc = "Multicursor: add cursor above" },

        -- Same as above, but leave the passed-over line without a cursor.
        { "<M-Down>", function() require("multicursor-nvim").lineSkipCursor(1) end, mode = { "n", "x" }, desc = "Multicursor: skip line below" },
        { "<M-Up>", function() require("multicursor-nvim").lineSkipCursor(-1) end, mode = { "n", "x" }, desc = "Multicursor: skip line above" },

        -- Turn the current visual selection into cursors - one per line. This is the
        -- `<C-v>`-block -> carets flow (works from `v` and `V` too).
        { "<M-v>", function() require("multicursor-nvim").visualToCursors() end, mode = "x", desc = "Multicursor: selection -> cursors" },

        -- Word/selection under cursor -> next/previous occurrence (IntelliJ `Alt+J`, VSCode `<C-d>`).
        { "<M-n>", function() require("multicursor-nvim").matchAddCursor(1) end, mode = { "n", "x" }, desc = "Multicursor: add at next match" },
        { "<M-N>", function() require("multicursor-nvim").matchAddCursor(-1) end, mode = { "n", "x" }, desc = "Multicursor: add at prev match" },
        { "<M-x>", function() require("multicursor-nvim").matchSkipCursor(1) end, mode = { "n", "x" }, desc = "Multicursor: skip this match" },
        { "<M-a>", function() require("multicursor-nvim").matchAllAddCursors() end, mode = { "n", "x" }, desc = "Multicursor: add at all matches" },

        -- Split the selection on a regex, leaving a cursor between the pieces (e.g. split a
        -- comma-separated argument list), or add a cursor at every regex match inside it.
        { "<M-p>", function() require("multicursor-nvim").splitCursors() end, mode = "x", desc = "Multicursor: split selection by regex" },
        { "<M-m>", function() require("multicursor-nvim").matchCursors() end, mode = "x", desc = "Multicursor: match in selection by regex" },

        -- Freeze the extra cursors so only the main one moves; press again to drop a cursor where
        -- the main one currently is. `<Esc>` (see the layer below) un-freezes them.
        { "<M-q>", function() require("multicursor-nvim").toggleCursor() end, mode = { "n", "x" }, desc = "Multicursor: toggle cursor" },

        -- Bring back the cursors cleared by the last `<Esc>` (the `gv` of multicursor).
        { "<M-r>", function() require("multicursor-nvim").restoreCursors() end, mode = "n", desc = "Multicursor: restore cursors" },

        -- Add/remove a cursor by ctrl-clicking.
        { "<C-LeftMouse>", function() require("multicursor-nvim").handleMouse() end, mode = "n", desc = "Multicursor: add cursor at click" },
        { "<C-LeftDrag>", function() require("multicursor-nvim").handleMouseDrag() end, mode = "n", desc = "Multicursor: drag cursor" },
        { "<C-LeftRelease>", function() require("multicursor-nvim").handleMouseRelease() end, mode = "n", desc = "Multicursor: release cursor" },
    },
    config = function()
        local mc = require("multicursor-nvim")

        mc.setup()

        --- Collapses multicursor state one step per `<Esc>`: the first press re-enables cursors
        --- frozen by `<M-q>`, and otherwise clears them back down to a single cursor.
        local function escape_cursors()
            if not mc.cursorsEnabled() then
                mc.enableCursors()
            else
                mc.clearCursors()
            end
        end

        -- Layer mappings are buffer-local and live only while the buffer has cursors, so they can
        -- safely shadow global keys - `<Esc>` is `nohlsearch` in `config/keymaps.lua:10`, and it
        -- goes back to that as soon as the cursors are gone.
        mc.addKeymapLayer(function(layer_set)
            layer_set({ "n", "x" }, "<M-Left>", mc.prevCursor, { desc = "Multicursor: prev cursor" })
            layer_set({ "n", "x" }, "<M-Right>", mc.nextCursor, { desc = "Multicursor: next cursor" })
            layer_set({ "n", "x" }, "<M-d>", mc.deleteCursor, { desc = "Multicursor: delete main cursor" })
            layer_set("n", "<Esc>", escape_cursors, { desc = "Multicursor: enable / clear cursors" })
        end)

        -- The plugin already registers sensible defaults (`MultiCursorCursor` reversed,
        -- `MultiCursorVisual` -> `Visual`, `MultiCursorMatchPreview` -> `Search`, ...), which follow
        -- the colorscheme. Override here if the extra cursors are hard to spot:
        -- local hl = vim.api.nvim_set_hl
        -- hl(0, "MultiCursorCursor", { link = "Cursor" })
        -- hl(0, "MultiCursorVisual", { link = "Visual" })
        -- hl(0, "MultiCursorSign", { link = "SignColumn" })
        -- hl(0, "MultiCursorMatchPreview", { link = "Search" })
        -- hl(0, "MultiCursorDisabledCursor", { reverse = true })
        -- hl(0, "MultiCursorDisabledVisual", { link = "Visual" })
        -- hl(0, "MultiCursorDisabledSign", { link = "SignColumn" })

        -- Other actions worth knowing about, left unbound to keep the Alt space small:
        -- mc.addCursorOperator     -- `gaip` -> a cursor on every line of a paragraph
        -- mc.alignCursors          -- align the columns of all cursors
        -- mc.duplicateCursors      -- clone every cursor, disabling the originals
        -- mc.insertVisual          -- `I` for each line of a (non-block) visual selection
        -- mc.appendVisual          -- `A` for each line of a (non-block) visual selection
        -- mc.transposeCursors      -- rotate the text between cursors
        -- mc.sequenceIncrement     -- `g<C-a>` across all cursors as one sequence
        -- mc.searchAddCursor       -- add a cursor at the next `/` search result
        -- mc.diagnosticAddCursor   -- add a cursor at the next diagnostic
        -- mc.operator              -- `<key>iwap`: cursor on every match of `iw` inside `ap`
    end,
}
