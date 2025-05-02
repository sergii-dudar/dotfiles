module Module.RunnerApp
    ( appsMenuRunner
    , settingsRunner
    , intellijRunner
    , torrentRunner
    , evinceRunner
    , browserRunner
    , terminalRunner
    , powerMenuRunner
    ) where

import qualified Util.Element as E
import qualified Util.Variable as V

import Xmobar

appsMenuRunner :: String
appsMenuRunner = oneIconActionSpaceLeft V.menusApps "haskell" 8

settingsRunner :: String
settingsRunner = oneIconActionSpaceLeft V.appsGnomeSettings "settings" 6

intellijRunner :: String
intellijRunner = oneIconActionSpaceLeft V.appsIntellij "intellij" 2

torrentRunner :: String
torrentRunner = oneIconActionSpaceLeft V.appsQbittorrent "qbittorrent" 3

evinceRunner :: String
evinceRunner = oneIconActionSpaceLeft V.appsBookReader "book" 3

browserRunner :: String
browserRunner = oneIconActionSpaceLeft V.appsBrowser "browser" 3

terminalRunner :: String
terminalRunner = twoIconAction V.appsGhostty V.appsKitty "terminal"

powerMenuRunner :: String
powerMenuRunner = oneIconActionSpace V.menusPower "power" 2 8

oneIconActionSpace :: String -> String -> Int -> Int -> String
oneIconActionSpace cmd xmpIconName pixelsl pixelsr =
    E.spaceWrap pixelsl pixelsr
        . E.oneAction cmd
        $ toIcon xmpIconName

oneIconActionSpaceLeft :: String -> String -> Int -> String
oneIconActionSpaceLeft cmd xmpIconName pixelsl =
    E.spaceWrapLeft pixelsl
        . E.oneAction cmd
        $ toIcon xmpIconName

twoIconAction :: String -> String -> String -> String
twoIconAction cmd1 cmd2 xmpIconName = E.twoAction cmd1 cmd2 $ toIcon xmpIconName

toIcon :: String -> String
toIcon xmpIconName = "<icon=" ++ xmpIconName ++ ".xpm/>"
