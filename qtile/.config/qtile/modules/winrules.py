from libqtile import hook, layout
from libqtile.config import (
    Match,
    Rule,
)
from util import colors

colors = colors.current()

############################################################
####### Open specific applications in floating mode ########
############################################################
floating_layout_theme = {
    "border_width": 3,
    "border_focus": colors.float_border_focus,
}
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry

        Match(wm_class="qBittorrent"),
        Match(wm_class="pavucontrol"),
        # Match(wm_class="org.gnome.Nautilus"),
        Match(wm_class="gnome-system-monitor"),
        Match(wm_class="Nm-connection-editor"),
        Match(wm_class="ViberPC"),
        Match(wm_class="vlc"),
        Match(wm_class="gnome-calculator"),
        Match(wm_class="snapshot"),
        Match(wm_class="Gcolor3"),
        Match(wm_class="org.gnome.Characters"),
        Match(wm_class="org.gnome.clocks"),
        Match(wm_class="gnome-calendar"),
        Match(wm_class="Gnome-disks"),
        Match(wm_class="Glate"),

        #Match(title="Telegram"),

        # Monkeytype
        Match(wm_class="Google-chrome", wm_instance_class="crx_picebhhlijnlefeleilfbanaghjlkkna"),
        # Google Chat
        #Match(wm_class="Google-chrome", wm_instance_class="crx_mdpkiolbdkhdjpekfbkbmhigcaggjagi"),
        # Youtube music
        #Match(wm_class="Google-chrome", wm_instance_class="crx_cinhimbnkkaeohfgghhklpknlkffjgod")
    ],
    **floating_layout_theme
    #border_focus=3
)
#floating_layout.border_focus=3

############################################################
######## Open applications on specific workspaces ##########
############################################################

# Define rules to assign specific applications to groups
rules_list = [
        { "rule": Rule(Match(wm_class="org.wezfurlong.wezterm")), "group": "1" },
        { "rule": Rule(Match(wm_class="jetbrains-idea")), "group": "2" },
        { "rule": Rule(Match(wm_class="Code")), "group": "2" },
        { "rule": Rule(Match(wm_class="Google-chrome", wm_instance_class="google-chrome")), "group": "3" },
        { "rule": Rule(Match(wm_class="kitty")), "group": "4" },
]

@hook.subscribe.client_new
def assign_app_group(client):
    for item in rules_list:
        if item["rule"].matches(client):
            client.togroup(item["group"])
            break