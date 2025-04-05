import XMonad

import Colors.DoomOne
import Utils.Common
import Utils.CommonTwo

main :: IO ()
main = do
    putStrLn $ testFunc "1" "2"
    putStrLn $ testFuncTwo "1" "2"
    xmonad def
