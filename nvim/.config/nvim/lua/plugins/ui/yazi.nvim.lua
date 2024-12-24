return {
    "mikavilpas/yazi.nvim",
    event = "VeryLazy",
    keys = {
        {
            "<leader>yy",
            "<cmd>Yazi<cr>",
            desc = "Open yazi at the current file",
        },
        {
            -- Open in the current working directory
            "<leader>yw",
            "<cmd>Yazi cwd<cr>",
            desc = "Open the file manager in nvim's working directory",
        },
        -- {
        --     -- NOTE: this requires a version of yazi that includes
        --     -- https://github.com/sxyazi/yazi/pull/1305 from 2024-07-18
        --     --"<c-up>",
        --     "<leader>yy",
        --     "<cmd>Yazi toggle<cr>",
        --     desc = "Resume the last yazi session",
        -- },
    },
    opts = {
        -- if you want to open yazi instead of netrw, see below for more info
        open_for_directories = false,
        keymaps = {
            show_help = "<f1>",
        },
    },
}
