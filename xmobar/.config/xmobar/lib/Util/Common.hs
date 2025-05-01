module Util.Common
    ( buildNotifyMsg
    ) where

import Xmobar

import qualified Util.Variable as V

buildNotifyMsg msg = "notify-send \"" ++ msg ++ "\" -t 700"