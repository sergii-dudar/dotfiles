require("full-border"):setup({
    -- Available values: ui.Border.PLAIN, ui.Border.ROUNDED
    type = ui.Border.ROUNDED,
})

-- https://github.com/dedukun/relative-motions.yazi
require("relative-motions"):setup({ show_numbers = "relative", show_motion = true })

-- https://github.com/Rolv-Apneseth/starship.yazi
require("starship"):setup(
    --{ config_file = "~/dotfiles/starship/.config/starship.toml" }
)

require("git"):setup()

require("copy-file-contents"):setup({
    append_char = "\n",
    notification = true,
})

require("system-clipboard")
