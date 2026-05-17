-------------------------------
---- ENVIRONMENT VARIABLES ----
-------------------------------
-- See https://wiki.hypr.land/Configuring/Advanced-and-Cool/Environment-variables/

hl.env("XCURSOR_SIZE", "30")
hl.env("XCURSOR_THEME", "elementary")
hl.env("HYPRCURSOR_SIZE", "30")
hl.env("HYPRCURSOR_THEME", "elementary")
hl.env("QT_QPA_PLATFORMTHEME", "qt5ct") -- change to qt6ct if you have that

-- unscale XWayland
hl.config({
    xwayland = {
        force_zero_scaling = true,
    },
})

-- toolkit-specific scale
-- env = GDK_SCALE,1
hl.env("XCURSOR_SIZE", "24")
