import Xmobar

config :: Config
config =
    defaultConfig
        { font = "xft:Mononoki Nerd Font:size=10"
        , bgColor = "#282c34"
        , fgColor = "#bbc2cf"
        , position = Top
        , commands =
            [ Run $ Date "%a %b %_d %Y %H:%M:%S" "date" 10
            , Run $ Cpu ["-L", "3", "-H", "50", "--normal", "green", "--high", "red"] 10
            , Run $ Memory ["-t", "Mem: <usedratio>%"] 10
            ]
        , sepChar = "%"
        , alignSep = "}{"
        , template = "%cpu% | %memory% }{ %date%"
        }

main :: IO ()
main = xmobar config