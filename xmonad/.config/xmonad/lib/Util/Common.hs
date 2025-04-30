module Util.Common
    ( toRationalRect
    , notifySend
    , buildNotifyMsg
    ) where

import qualified Module.Variable as V
import XMonad
import qualified XMonad.StackSet as W

widthFactor = V.settingsFloatFactorWidth
heightFactor = V.settingsFloatFactorHeight
x = (1 - widthFactor) / 2
y = (1 - heightFactor) / 2

toRationalRect = W.RationalRect x y widthFactor heightFactor

buildNotifyMsg msg = "notify-send \"" ++ msg ++ "\" -t 700"
notifySend msg = spawn $ buildNotifyMsg msg