----------------
--- MONITORS ---
----------------

-- I'm never using laptop screen, to speed, disable it manually as early as possible
hl.monitor({ output = "eDP-1", disabled = true })
-- hl.workspace_rule({ workspace = "9", monitor = "HDMI-A-3" })

-- See https://wiki.hypr.land/Configuring/Basics/Monitors/
-- hl.monitor({ output = "", mode = "preferred", position = "auto", scale = "auto" })

hl.monitor({ output = "HDMI-A-1", mode = "preferred", position = "0x0", scale = 1 })
hl.monitor({ output = "HDMI-A-2", mode = "preferred", position = "0x0", scale = 1 })
hl.monitor({ output = "HDMI-A-3", mode = "preferred", position = "3840x0", scale = 1, transform = 3 })

-- output "HDMI-A-1" mode 3840x2160@60.000Hz pos 0 0
-- output "HDMI-A-2" mode 3840x2160@60.000Hz pos 0 0
-- output "HDMI-A-3" mode 2560x1440@74.983Hz transform 90 pos 3840 0
