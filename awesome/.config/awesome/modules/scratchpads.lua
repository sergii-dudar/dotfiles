-- https://github.com/hadiali6/scratchpad

local scratchpad = require("libraries.scratchpad")

local awful = require("awful")
-- Get the focused screen's geometry
local screen_geometry = awful.screen.focused().geometry
local screen_width = screen_geometry.width
local screen_height = screen_geometry.height

-- Initialize a table which will contain all of your scratchpad objects.
local pads = scratchpad.group:new({
    id = "my-group-id",
    scratchpads = {},
})

local function new_scratchpad(id, cmd)
    local default_width = screen_width * 0.65
    local default_height = screen_height * 0.7
    local default_x = screen_width / 2 - default_width / 2
    local default_y = screen_height / 2 - default_height / 2

    return scratchpad:new({
        id = id,
        command = cmd,
        group = pads,
        client_options = {
            floating = true,
            ontop = false,
            above = false,
            skip_taskbar = false,
            sticky = false,
            geometry = {
                width = default_width,
                height = default_height,
                x = default_x,
                y = default_y,
            },
        },
        scratchpad_options = {
            reapply_options = false,
            only_one = false,
        },
    })
end

M = {}
M.yazi = new_scratchpad("yazi-scratchpad-id-1", "kitty --class=yazi -e yazi")
M.telegram = new_scratchpad("telegram-scratchpad-id-2", "telegram-desktop")
M.youtube_music = new_scratchpad(
    "youtube_music-scratchpad-id-3",
    "google-chrome-stable --profile-directory=Default --app-id=cinhimbnkkaeohfgghhklpknlkffjgod"
)
M.google_chat = new_scratchpad(
    "google_chat",
    "google-chrome-stable --profile-directory=Default --app-id=mdpkiolbdkhdjpekfbkbmhigcaggjagi"
)

pads:add_scratchpad(M.yazi)
pads:add_scratchpad(M.telegram)
pads:add_scratchpad(M.youtube_music)
pads:add_scratchpad(M.google_chat)

return M
