module Util.Common
    ( buildNotifyMsg
    ) where

import Xmobar

import qualified Module.Variable as V

buildNotifyMsg msg = "notify-send \"" ++ msg ++ "\" -t 700"