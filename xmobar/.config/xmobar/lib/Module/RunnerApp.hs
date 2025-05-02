module Module.RunnerApp where

import qualified Util.Element as E
import qualified Util.Variable as V

import Xmobar

appsMenuRunner :: String
appsMenuRunner =
    E.spaceWrapLeft 8
        . E.oneAction V.menusApps
        $ "<icon=haskell.xpm/>"

settingsRunner :: String
settingsRunner =
    E.spaceWrapLeft 6
        . E.oneAction V.appsGnomeSettings
        $ "<icon=settings.xpm/>"

intellijRunner :: String
intellijRunner =
    E.spaceWrapLeft 2
        . E.oneAction V.appsIntellij
        $ "<icon=intellij.xpm/>"

torrentRunner :: String
torrentRunner =
    E.spaceWrapLeft 3
        . E.oneAction V.appsQbittorrent
        $ "<icon=qbittorrent.xpm/>"

evinceRunner :: String
evinceRunner =
    E.spaceWrapLeft 3
        . E.oneAction V.appsBookReader
        $ "<icon=book.xpm/>"

browserRunner :: String
browserRunner =
    E.spaceWrapLeft 3
        . E.oneAction V.appsBrowser
        $ "<icon=browser.xpm/>"

terminalRunner :: String
terminalRunner = E.twoAction V.appsGhostty V.appsKitty "<icon=terminal.xpm/>"

powerMenuRunner :: String
powerMenuRunner =
    E.spaceWrap 2 8
        . E.oneAction V.menusPower
        $ "<icon=power.xpm/>"