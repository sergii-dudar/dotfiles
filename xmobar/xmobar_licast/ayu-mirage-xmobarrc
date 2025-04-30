Config { font = "JetBrainsMono Nerd Font Bold 12"
        , borderColor = "#212733"
        , border = TopB
        , bgColor = "#212733"
        , fgColor = "#d9d7ce"
        , position = TopHM 33 4 6 6 0
        , commands = [ Run Cpu ["-t","<total>%"] 10
                     , Run Memory ["-t","<usedratio>%"] 10
                     , Run WeatherX "KY70"
                        [ ("clear", "󰼷")
                        , ("sunny", "󰼷")
                        , ("mostly clear", "󰖕")
                        , ("mostly sunny", "󰖕")
                        , ("partly sunny", "⛅")
                        , ("fair", "🌑")
                        , ("cloudy","󰖐")
                        , ("overcast","☁")
                        , ("partly cloudy", "⛅")
                        , ("mostly cloudy", "󰖕")
                        , ("considerable cloudiness", "󰖐")]
                        ["-t", "<fn=2><skyConditionS></fn> <tempF>°"
                        , "-L","10", "-H", "25", "--normal", "black"
                        , "--high", "lightgoldenrod4", "--low", "darkseagreen4"]
                         18000
                     , Run DiskU [("/","<used>/<size>")] [] 10
                     , Run Com "uname" ["-r"] "" 36000
                     , Run CommandReader "/usr/local/bin/subs.sh" "subs"
                     , Run CommandReader "rofi -show drun" "menu"
                     , Run Date "%b %_d - %I:%M" "date" 10
                     , Run UnsafeStdinReader
                     ]
        , sepChar = "%"
        , alignSep = "}{"
        , template = " <action=`rofi -show drun` button=1> <fc=#ed8274,#212733>  </fc></action>  |  %UnsafeStdinReader%  }{ %KY70%   <fc=#cfbafa></fc>  %disku%   <fc=#fad07b>󰗃</fc>  %subs%   <fc=#a6cc70></fc>   %cpu%   <fc=#6dcbfa>󰒋</fc>  %memory%   <fc=#a6cc70>󰥔 </fc> %date%  "
        }
