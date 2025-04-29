module Module.Startup (runStartup) where

import qualified Module.Variable as V

import XMonad

runStartup :: X ()
runStartup = do
    spawn V.appsTrayerRun

-- setWMName "LG3D"