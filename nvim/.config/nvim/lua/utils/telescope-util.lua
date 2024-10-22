local init_telescope = function()

    local actions = require('telescope.actions')
    local action_state = require('telescope.actions.state')
    local telescope_builtin = require('telescope.builtin')

    local M = {}

    -- Function to change working directory and re-run live_grep
    M.change_directory_and_search = function(prompt_bufnr, current_opts)
        actions.close(prompt_bufnr)

        telescope_builtin.file_browser({
            prompt_title = "Select New Directory",
            cwd = vim.loop.cwd(), -- Start from the current working directory
            attach_mappings = function(_, map)
                map('i', '<CR>', function(inner_bufnr)
                    local new_cwd = action_state.get_selected_entry().path
                    actions.close(inner_bufnr)

                    current_opts.prompt_title = (current_opts.glob_pattern or ".* | ") .. vim.loop.cwd()

                    -- Perform live_grep with the updated directory and the same options
                    telescope_builtin.live_grep(vim.tbl_extend('force', current_opts, { cwd = new_cwd }))
                end)
                return true
            end,
        })
    end

    -- Function to change file extension and re-run live_grep
    M.change_extension_and_search = function(prompt_bufnr, current_opts)
        actions.close(prompt_bufnr)

        -- Prompt for file extension
        vim.ui.input({ prompt = 'Enter file extension (e.g., js, lua): ' }, function(input_ext)
            if input_ext then
                -- Perform live_grep with the new file extension filter
                current_opts.glob_pattern = "*." .. input_ext
                current_opts.prompt_title = (current_opts.glob_pattern or ".* | ") .. vim.loop.cwd()
                telescope_builtin.live_grep(vim.tbl_extend('force', current_opts, { glob_pattern = "*." .. input_ext }))
            end
        end)
    end

    -- Combined function for live_grep with dynamic directory and file extension change
    M.grep_with_dynamic_options = function(opts)
        opts = opts or {}

        -- Save initial options to maintain context between actions
        local current_opts = vim.deepcopy(opts)
        current_opts.prompt_title = (current_opts.glob_pattern or ".* | ") .. vim.loop.cwd()

        telescope_builtin.live_grep({
            -- Pass any initial options
            prompt_title = current_opts.prompt_title,
            cwd = current_opts.cwd or vim.loop.cwd(),
            glob_pattern = current_opts.glob_pattern,

            -- Attach custom mappings for changing directory and file extension
            attach_mappings = function(prompt_bufnr, map)
                -- Bind <C-d> to dynamically change the working directory
                map('i', '<C-d>', function()
                    M.change_directory_and_search(prompt_bufnr, current_opts)
                end)
                map('n', '<C-d>', function()
                    M.change_directory_and_search(prompt_bufnr, current_opts)
                end)

                -- Bind <C-e> to dynamically change the file extension filter
                map('i', '<C-e>', function()
                    M.change_extension_and_search(prompt_bufnr, current_opts)
                end)
                map('n', '<C-e>', function()
                    M.change_extension_and_search(prompt_bufnr, current_opts)
                end)

                return true
            end,
        })
    end

    return M

end

return init_telescope()