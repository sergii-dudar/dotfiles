local customMapping = function()
    local api = require('Comment.api')

    -- Toggle current line (linewise) using C-/
    vim.keymap.set('n', '<C-_>', api.toggle.linewise.current)

    -- Toggle current line (blockwise) using C-\
    vim.keymap.set('n', '<C-\\>', api.toggle.blockwise.current)

    -- Toggle lines (linewise) with dot-repeat support
    -- Example: <leader>gc3j will comment 4 lines
    vim.keymap.set(
        'n', '<leader>gc', api.call('toggle.linewise', 'g@'),
        { expr = true }
    )

    -- Toggle lines (blockwise) with dot-repeat support
    -- Example: <leader>gb3j will comment 4 lines
    vim.keymap.set(
        'n', '<leader>gb', api.call('toggle.blockwise', 'g@'),
        { expr = true }
    )


    -- vim mode
    local esc = vim.api.nvim_replace_termcodes(
        '<ESC>', true, false, true
    )

    -- Toggle selection (linewise)
    vim.keymap.set('x', '<leader>c', function()
        vim.api.nvim_feedkeys(esc, 'nx', false)
        api.toggle.linewise(vim.fn.visualmode())
    end)

    -- Toggle selection (blockwise)
    vim.keymap.set('x', '<leader>b', function()
        vim.api.nvim_feedkeys(esc, 'nx', false)
        api.toggle.blockwise(vim.fn.visualmode())
    end)
end

return {
    {
        'numToStr/Comment.nvim',
        opts = {
            -- add any options here
        },
        config = function()
            require('Comment').setup()

            -- NORMAL mode
            -- `gcc` - Toggles the current line using linewise comment
            -- `gbc` - Toggles the current line using blockwise comment
            -- `[count]gcc` - Toggles the number of line given as a prefix-count using linewise
            -- `[count]gbc` - Toggles the number of line given as a prefix-count using blockwise
            -- `gc[count]{motion}` - (Op-pending) Toggles the region using linewise comment
            -- `gb[count]{motion}` - (Op-pending) Toggles the region using blockwise comment

            -- VISUAL mode
            -- `gc` - Toggles the region using linewise comment
            -- `gb` - Toggles the region using blockwise comment


            -- customMapping()
        end
    }
}
