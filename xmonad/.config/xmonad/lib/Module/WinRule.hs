module Module.WinRule (manageHookConfig) where

import XMonad

-- Hooks
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, doRectFloat, isFullscreen)

-- Config Modules

import qualified Module.Variable as V
import qualified Util.Common as U

manageHookConfig =
    composeAll
        [ resource =? "desktop_window" --> doIgnore
        , resource =? "kdesktop" --> doIgnore
        , applyFloatToClass "MPlayer"
        , applyFloatToClass "Gimp"
        , applyFloatToClass "qBittorrent"
        , applyFloatToClass "Arandr"
        , applyFloatToClass "Blueman-manager"
        , applyFloatToClass "Gpick"
        , applyFloatToClass "Kruler"
        , applyFloatToClass "MessageWin" -- kalarm.
        , applyFloatToClass "Sxiv"
        , applyFloatToClass "Wpa_gui"
        , applyFloatToClass "veromix"
        , applyFloatToClass "xtightvncviewer"
        , applyFloatToClass "pavucontrol"
        , applyFloatToClass "gnome-system-monitor"
        , applyFloatToClass "gnome-control-center"
        , applyFloatToClass "gnome-calculator"
        , applyFloatToClass "org.gnome.Characters"
        , applyFloatToClass "org.gnome.clocks"
        , applyFloatToClass "gnome-calendar"
        , applyFloatToClass "Gnome-disks"
        , applyFloatToClass "Nm-connection-editor"
        , applyFloatToClass "ViberPC"
        , applyFloatToClass "vlc"
        , applyFloatToClass "snapshot"
        , applyFloatToClass "Gcolor3"
        , applyFloatToClass "Glate"
        , applyFloatToInstance "disc_ugd"
        , applyFloatToInstance "htop_info"
        , applyFloatToInstance "disc_usabe_info"
        , workspaceToClass 1 "org.wezfurlong.wezterm"
        , workspaceTo 1 "com.ghostty.group01" "ghostty"
        , workspaceToClass 2 "jetbrains-idea"
        , workspaceToClass 2 "Code"
        , workspaceTo 2 "Brave-browser" "brave-browser"
        ]
    where
        applyFloatToClass cname = className =? cname --> rectFloat
        applyFloatToInstance iname = resource =? iname --> rectFloat
        applyFloatTo cname iname = className =? cname <&&> resource =? iname --> rectFloat
        workspaceToClass wnum cname = className =? cname --> toShift wnum
        workspaceToInstance wnum iname = resource =? iname --> toShift wnum
        workspaceTo wnum cname iname = className =? cname <&&> resource =? iname --> toShift wnum
        rectFloat = doRectFloat U.toRationalRect
        toShift wnum = doShift (V.workspacesList !! (wnum - 1))