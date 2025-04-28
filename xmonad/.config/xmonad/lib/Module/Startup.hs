module Module.Startup (runStartup) where

import XMonad

-- Hooks
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, doRectFloat, isFullscreen)

-- Config Modules

import qualified Module.Variable as V
import qualified Util.Common as U

runStartup :: X ()
runStartup = do
    spawn "killall trayer ; " -- kill current trayer on each restart
    spawn
        ( "sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 "
            ++ colorTrayer
            ++ " --height 22"
        )

    setWMName "LG3D"