local gears = require("gears")
local awful = require("awful")
local vars = require("modules.variables")

local M = {}

M.extend_globalkeys = function(globalkeys)
    -- Bind all key numbers to tags.
    -- Be careful: we use keycodes to make it work on any keyboard layout.
    -- This should map on the top row of your keyboard, usually 1 to 9.
    for i = 1, 9 do
        globalkeys = gears.table.join(
            globalkeys,
            -- View tag only.
            awful.key({ vars.key.modkey }, "#" .. i + 9, function()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    tag:view_only()
                end
            end, { description = "view tag #" .. i, group = "tag" }),
            -- Toggle tag display.
            awful.key({ vars.key.modkey, "Control" }, "#" .. i + 9, function()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    awful.tag.viewtoggle(tag)
                end
            end, { description = "toggle tag #" .. i, group = "tag" }),
            -- Move client to tag.
            awful.key({ vars.key.modkey, "Shift" }, "#" .. i + 9, function()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:move_to_tag(tag)
                    end
                end
            end, { description = "move focused client to tag #" .. i, group = "tag" }),
            -- Toggle tag on focused client.
            awful.key({ vars.key.modkey, "Control", "Shift" }, "#" .. i + 9, function()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:toggle_tag(tag)
                    end
                end
            end, { description = "toggle focused client on tag #" .. i, group = "tag" }),
            awful.key({ vars.key.modkey, "Shift" }, ",", function()
                local c = client.focus
                if c then
                    c:move_to_screen()
                end
            end, { description = "move window to next screen", group = "client" }),
            awful.key({ vars.key.modkey, "Shift" }, ".", function()
                local c = client.focus
                if c then
                    c:move_to_screen()
                end
            end, { description = "move window to next screen", group = "client" })
        )
    end

    return globalkeys
end

return M
