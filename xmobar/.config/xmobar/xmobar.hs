import Xmobar

import Data.List (intercalate)
import qualified Module.Battery as Battery
import qualified Module.Date as Date
import qualified Module.RunnerApp as RunnerApp
import qualified Module.RunnerTemplate as RunnerTemplate
import qualified Module.Simple as Simple
import qualified Module.System as System
import qualified Module.Volume as Volume
import qualified Util.Common as U
import qualified Util.Element as E
import qualified Util.Variable as V
import qualified Xmobar as E

config :: Config
config =
    defaultConfig
        { overrideRedirect = False -- allows XMonad to manage/reserve space dynamically.
        , dpi = 96
        , font = V.defaultFont
        , additionalFonts = V.additionalFonts
        , bgColor = "#232634"
        , fgColor = "#94928F"
        , lowerOnStart = True
        , position = TopSize L 100 35
        , iconRoot = V.xmobarResourcesDir
        , allDesktops = True
        , commands =
            [ Run $ XPropertyLog "_XMONAD_TRAYPAD"
            , Run Simple.openWeatherCommand
            , Run Simple.kbdCommand
            , Run System.cpuCommand
            , Run System.memoryCommand
            , Run System.diskCommand
            , Run System.cpuTempCommand
            , Run Volume.alsaCommand
            , Run Date.dateCommand
            , Run Battery.batteryCommand
            , Run UnsafeXMonadLog
            ]
        , sepChar = "%"
        , alignSep = "}{"
        , template =
            concat
                [ appRunnersLeft
                , modulesSpace
                , "%UnsafeXMonadLog%"
                , " } "
                , RunnerTemplate.dateRunner
                , " { "
                , intercalate
                    modulesSpace
                    [ RunnerTemplate.kbdRunner
                    , RunnerTemplate.alsaRunner
                    , RunnerTemplate.batteryRunner
                    , RunnerTemplate.memoryRunner
                    , RunnerTemplate.cpuRunner
                    , RunnerTemplate.coreTempRunner
                    , RunnerTemplate.diskRunner
                    , RunnerTemplate.weatherRunner
                    , appRunnersRight
                    , modulesBg "%_XMONAD_TRAYPAD% "
                    ]
                ]
        }

modulesSpace :: String
modulesSpace = E.space 3

modulesBg :: String -> String
modulesBg = E.color "#d35f5e" "#2E3440:0"

appRunnersLeft :: String
appRunnersLeft = modulesBg RunnerApp.appsMenuRunner

appRunnersRight :: String
appRunnersRight =
    modulesBg $
        concat
            [ RunnerApp.settingsRunner
            , RunnerApp.intellijRunner
            , RunnerApp.torrentRunner
            , RunnerApp.evinceRunner
            , RunnerApp.browserRunner
            , RunnerApp.terminalRunner
            , RunnerApp.powerMenuRunner
            ]

main :: IO ()
main = configFromArgs config >>= xmobar