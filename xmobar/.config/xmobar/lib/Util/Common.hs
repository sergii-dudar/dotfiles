module Util.Common where

import Xmobar

import qualified Util.Variable as V

buildNotifyMsg :: String -> String
buildNotifyMsg msg = "notify-send \"" ++ msg ++ "\" -t 700"