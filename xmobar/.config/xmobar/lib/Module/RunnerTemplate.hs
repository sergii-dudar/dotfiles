module Module.RunnerTemplate where

import qualified Util.Common as U
import qualified Util.Element as E
import qualified Util.Variable as V

batteryRunner :: String
batteryRunner = "%battery%"

coreTempRunner :: String
coreTempRunner = "%coretemp%"

alsaRunner :: String
alsaRunner =
    E.twoAction V.runVolumeToggle V.appsVolumeControl
        . E.action V.runVolumeIncrease 4
        . E.action V.runVolumeDecrease 5
        $ "%alsa:default:Master%"

dateRunner :: String
dateRunner = E.twoAction V.appsGnomeClocks V.appsGnomeCalendar "%date%"

kbdRunner :: String
kbdRunner = E.twoAction V.runChangeLanguage V.runChangeLanguage "%kbd%"

weatherRunner :: String
weatherRunner = E.oneAction V.runWeatherWeb "%openweather%"

memoryRunner :: String
memoryRunner = E.oneAction V.appsTerminalHtop "%memory%"

cpuRunner :: String
cpuRunner = E.oneAction V.appsGnomeSystemMonitor "%cpu%"

diskRunner :: String
diskRunner = E.oneAction V.appsTerminalDiscGdu "%disku%"
