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
                [ "%UnsafeXMonadLog%"
                , " } "
                , RunnerTemplate.dateRunner
                , " { "
                , intercalate
                    "<hspace=3/>"
                    [ RunnerTemplate.kbdRunner
                    , RunnerTemplate.alsaRunner
                    , RunnerTemplate.batteryRunner
                    , RunnerTemplate.memoryRunner
                    , RunnerTemplate.cpuRunner
                    , RunnerTemplate.coreTempRunner
                    , RunnerTemplate.diskRunner
                    , RunnerTemplate.weatherRunner
                    , appRunners
                    , E.color "#7C8377" "#2E3440:0" "%_XMONAD_TRAYPAD% "
                    ]
                ]
        }

appRunners :: String
appRunners =
    E.color "#d35f5e" "#2E3440:0" $
        concat
            [ RunnerApp.appsMenuRunner
            , RunnerApp.settingsRunner
            , RunnerApp.intellijRunner
            , RunnerApp.torrentRunner
            , RunnerApp.evinceRunner
            , RunnerApp.browserRunner
            , RunnerApp.terminalRunner
            , RunnerApp.powerMenuRunner
            ]

main :: IO ()
main = xmobar config