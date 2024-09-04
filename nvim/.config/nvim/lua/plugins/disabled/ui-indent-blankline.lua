return {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    ---@module "ibl"
    ---@type ibl.config
    opts = {},
    enabled = false,
    config = function ()
        require("ibl").setup({
            scope = { enabled = false },
           -- indent = {
           --     --char = "▏"
           --     char = "╎"
           -- }
        })
    end
}