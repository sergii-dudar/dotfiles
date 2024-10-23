return {
  "nvim-neo-tree/neo-tree.nvim",
  opts = {
    source_selector = {
      winbar = true,
      statusline = false,
    },
    window = {
      mappings = {
        --    e = 'none'
        ["/"] = "noop",
        ["<esc>"] = "noop",
      },
      --position = 'float',
    },
    popup_border_style = "rounded", --'solid',
    filesystem = {
      filtered_items = {
        visible = true,
        hide_dotfiles = false,
        hide_gitignored = true,
      },
      follow_current_file = {
        enabled = true, -- This will find and focus the file in the active buffer every time
        --               -- the current file is changed while the tree is open.
        leave_dirs_open = true, -- `false` closes auto expanded dirs, such as with `:Neotree reveal`
      },
      group_empty_dirs = true,
    },
    event_handlers = {
      {
        event = "neo_tree_buffer_enter",
        handler = function()
          vim.opt_local.relativenumber = true
        end,
      },
    },
    default_component_configs = {
      last_modified = {
        enabled = true,
        required_width = 75,
      },
      file_size = {
        enabled = false,
      },
    },
  },
}