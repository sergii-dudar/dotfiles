import XMonad

import Color.DoomOne
import Util.Common
import Util.CommonTwo
import Util.Env.Environment

main :: IO ()
main = do
    putStrLn $ testFunc "1" "2"
    putStrLn $ testFuncTwo "1" "2"
    xmonad def