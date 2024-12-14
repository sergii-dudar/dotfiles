-- https://github.com/hadiali6/scratchpad

local scratchpad = require("scratchpad")
local vars = require("modules.variables")
local util = require("util.common-util")

-- Initialize a table which will contain all of your scratchpad objects.
local pads = scratchpad.group:new({
    id = "my-group-id",
    scratchpads = {},
})

local function new_scratchpad(id, cmd, factor_width, factor_height)
    local window_width = util.calculate_window_width(factor_width)
    local window_height = util.calculate_window_height(factor_height)
    local window_x = vars.settings.screen_width / 2 - window_width / 2
    local window_y = vars.settings.screen_height / 2 - window_height / 2

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
                width = window_width,
                height = window_height,
                x = window_x,
                y = window_y,
            },
        },
        scratchpad_options = {
            reapply_options = true,
            only_one = true,
        },
    })
end

M = {}
M.yazi = new_scratchpad("yazi-scratchpad-id-1", "kitty --class=yazi -e yazi")
M.telegram = new_scratchpad("telegram-scratchpad-id-2", "telegram-desktop")
M.nautilus = new_scratchpad("nautilus-scratchpad-id-3", "nautilus")
M.youtube_music = new_scratchpad(
    "youtube_music-scratchpad-id-4",
    "google-chrome-stable --profile-directory=Default --app-id=cinhimbnkkaeohfgghhklpknlkffjgod"
)
M.google_chat = new_scratchpad(
    "google_chat_scratchpad-id-5",
    "google-chrome-stable --profile-directory=Default --app-id=mdpkiolbdkhdjpekfbkbmhigcaggjagi"
)
M.monkey_type = new_scratchpad("google_chat_scratchpad-id-6", vars.run.monkey_type, 0.75, 0.8)

pads:add_scratchpad(M.yazi)
pads:add_scratchpad(M.telegram)
pads:add_scratchpad(M.nautilus)
pads:add_scratchpad(M.youtube_music)
pads:add_scratchpad(M.google_chat)
pads:add_scratchpad(M.monkey_type)

return M
