return {
    "mistricky/codesnap.nvim",
    build = "make build_generator",
    keys = {
        { "<leader>SS", "<cmd>CodeSnap<cr>", mode = "x", desc = "Save selected code snapshot into clipboard" },
        { "<leader>SP", "<cmd>CodeSnapSave<cr>", mode = "x", desc = "Save selected code snapshot in ~/Pictures" },
    },
    opts = {
        save_path = "~/Pictures",
        has_breadcrumbs = true,
        bg_theme = "bamboo",
    },
    config = function()
        require("codesnap").setup({
            mac_window_bar = false,
            title = "CodeSnap.nvim",
            code_font_family = "CaskaydiaCove Nerd Font",
            watermark_font_family = "Pacifico",
            watermark = "CodeSnap.nvim",
            bg_theme = "default",
            breadcrumbs_separator = "/",
            has_breadcrumbs = false,
            has_line_number = false,
            show_workspace = false,
            min_width = 0,
            bg_x_padding = 0,
            bg_y_padding = 0,
            bg_padding = 0,
            save_path = os.getenv("XDG_PICTURES_DIR") or (os.getenv("HOME") .. "/Pictures"),
        })
    end,
}
