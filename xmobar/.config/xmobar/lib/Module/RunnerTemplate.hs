module Module.RunnerTemplate (dateRunner) where

import qualified Util.Common as U
import qualified Util.Element as E
import qualified Util.Variable as V

import Xmobar

dateRunner :: String
dateRunner =
    E.action V.menusApps 1
        . E.action V.menusPower 3
        $ "%date%"