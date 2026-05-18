---------------
---- INPUT ----
---------------

hl.config({
    input = {
        kb_layout = "us,ua",
        kb_variant = "",
        kb_model = "",
        kb_options = "", -- grp:win_space_toggle (see keybindings)
        kb_rules = "",

        follow_mouse = 1,

        sensitivity = 0, -- -1.0 - 1.0, 0 means no modification.

        repeat_delay = 275, -- 200, 250
        repeat_rate = 30,

        touchpad = {
            natural_scroll = false,
        },
    },
})

-- hl.gesture({
--     fingers = 3,
--     direction = "horizontal",
--     action = "workspace",
-- })

-- Example per-device config
-- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Devices/ for more
hl.device({
    name = "epic-mouse-v1",
    sensitivity = -0.5,
})
