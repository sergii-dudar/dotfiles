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

-- https://github.com/sxyazi/yazi/blob/shipped/yazi-plugin/preset/components/status.lua
function Status:mode()
    local mode = tostring(self._tab.mode):upper()

    local style = self:style()
    return ui.Line({
        ui.Span(THEME.status.separator_open):fg(style.main.bg),
        ui.Span(" " .. mode .. " "):style(style.main),
        ui.Span(THEME.status.separator_close):fg(style.main.bg):bg(style.alt.bg),
    })
end

function Status:name()
    local h = self._current.hovered
    if not h then
        return ""
    end

    return " 👉 " .. h.name:gsub("\r", "?", 1)
end
