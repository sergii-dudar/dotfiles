local awful = require("awful")
local vars = require("modules.variables")

local M = {}

M.mpris = awful.widget.watch(
    { awful.util.shell, "-c", "playerctl status && playerctl metadata" },
    2,
    function(widget, stdout)
        local escape_f = require("awful.util").escape
        local mpris_now = {
            state = "N/A",
            artist = "N/A",
            title = "N/A",
            art_url = "N/A",
            album = "N/A",
            album_artist = "N/A",
        }

        mpris_now.state = string.match(stdout, "Playing") or string.match(stdout, "Paused") or "N/A"

        -- for k, v in string.gmatch(stdout, "'[^:]+:([^']+)':[%s]<%[?'([^']+)'%]?>") do
        --     if k == "artUrl" then
        --         mpris_now.art_url = v
        --     elseif k == "artist" then
        --         mpris_now.artist = escape_f(v)
        --     elseif k == "title" then
        --         mpris_now.title = escape_f(v)
        --     elseif k == "album" then
        --         mpris_now.album = escape_f(v)
        --     elseif k == "albumArtist" then
        --         mpris_now.album_artist = escape_f(v)
        --     end
        -- end

        for line in stdout:gmatch("[^\r\n]+") do
            local key, value = line:match("xesam:(%w+)%s+(.+)")
            if key and value then
                if key == "artist" then
                    mpris_now.artist = escape_f(value)
                elseif key == "title" then
                    mpris_now.title = escape_f(value)
                elseif key == "album" then
                    mpris_now.album = escape_f(value)
                elseif key == "albumArtist" then
                    mpris_now.album_artist = escape_f(value)
                end
            elseif line:match("mpris:artUrl%s+(.+)") then
                mpris_now.art_url = line:match("mpris:artUrl%s+(.+)")
            end
        end
        -- customize here
        --widget:set_text(mpris_now.artist .. " - " .. mpris_now.title)

        --local full_text = mpris_now.artist .. " - " .. mpris_now.title
        local full_text = mpris_now.title
        if #full_text > 30 then
            full_text = string.sub(full_text, 1, 30) .. "…" -- Add ellipsis for trimmed text
        end

        -- Set font and color
        local font = vars.font.widget
        local color = "#ca9ee6"
        local formatted_text = string.format(" <span font='%s' foreground='#8caaee'> </span>", font)
            .. string.format("<span font='%s' foreground='%s'>%s</span>", font, color, full_text)

        widget:set_markup(formatted_text)
    end
)

return M
