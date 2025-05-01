# xdo id -n xmobar | xargs -I {} xdo hide {} || xdo id -n xmobar | xargs -I {} xdo show {}
dbus-send --session --dest=org.Xmobar.Control --type=method_call '/org/Xmobar/Control' org.Xmobar.Control.SendSignal string:'Toggle 0'
