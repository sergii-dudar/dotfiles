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

buildConfig :: Bool -> Config
buildConfig hasBattery =
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
                , E.modulesSpace
                , "%UnsafeXMonadLog%"
                , " } "
                , RunnerTemplate.dateRunner
                , " { "
                , intercalate E.modulesSpace $ buildRightSection hasBattery
                ]
        }

buildRightSection :: Bool -> [String]
buildRightSection hasBattery =
    [ RunnerTemplate.kbdRunner
    , RunnerTemplate.alsaRunner
    ]
        ++ [RunnerTemplate.batteryRunner | hasBattery]
        ++ [ RunnerTemplate.memoryRunner
           , RunnerTemplate.cpuRunner
           , RunnerTemplate.coreTempRunner
           , RunnerTemplate.diskRunner
           , RunnerTemplate.weatherRunner
           , appRunnersRight
           , E.modulesBg "%_XMONAD_TRAYPAD% "
           ]

appRunnersLeft :: String
appRunnersLeft = E.modulesBg RunnerApp.appsMenuRunner

appRunnersRight :: String
appRunnersRight =
    E.modulesBg $
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
main = do
    hasBattery <- Battery.batteryExists
    let config = buildConfig hasBattery :: Config
    configFromArgs config >>= xmobar