module Module.RunnerApp where

import qualified Util.Common as U
import qualified Util.Element as E
import qualified Util.Variable as V

import Xmobar

appsMenuRunner :: String
appsMenuRunner = E.spaceWrapLeft 8 $ "<icon=haskell.xpm/>"

settingsRunner :: String
settingsRunner = E.spaceWrapLeft 6 $ "<icon=settings.xpm/>"

intellijRunner :: String
intellijRunner = E.spaceWrapLeft 2 $ "<icon=intellij.xpm/>"

torrentRunner :: String
torrentRunner = E.spaceWrapLeft 3 $ "<icon=qbittorrent.xpm/>"

evinceRunner :: String
evinceRunner = E.spaceWrapLeft 3 $ "<icon=book.xpm/>"

browserRunner :: String
browserRunner = E.spaceWrapLeft 3 $ "<icon=browser.xpm/>"

terminalRunner :: String
terminalRunner = "<icon=terminal.xpm/>"

powerMenuRunner :: String
powerMenuRunner =
    E.fgColor "#d35f5e"
        . E.spaceWrap 2 2
        . E.oneAction V.menusPower
        . E.font 2
        $ "\xf011 "

--  <icon=settings.xpm/>
-- util.to_imagebox_runner(settings_icon, vars.run.gnome_settings)

--
-- local red = "#d35f5e"
--
-- --local settings = util.to_text_icon_runner(" ", 19, "#3071db", vars.run.gnome_settings)
-- local powermenu = util.to_text_icon_runner(" ", vars.font.default_size, red, vars.run.powermenu)
--