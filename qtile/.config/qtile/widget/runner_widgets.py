from qtile_extras import widget

from libqtile import qtile
from util import colors
from util.util import to_mouse_callbacks, to_span
from util.vars import var
from widget.wcommon import (
    applications_launcher,
    decorations_no_round,
    decorations_round,
    decorations_round_left,
    decorations_round_right,
    icon_widget_defaults,
    text_widget_defaults,
)

colors = colors.current()
icons_dir = var.path.home_dir + "/dotfiles/awesome/.config/awesome/themes/personal/icons/"
icons_tdir = var.path.home_dir + "/dotfiles/awesome/.config/awesome/themes/personal/icons/test/"

kitty_icon = icons_dir + "kitty.svg"
wezterm_icon = icons_dir + "wezterm.png"
ghostty_icon = icons_dir + "ghostty.png"
pipette_icon = icons_dir + "pipette.png"
# chrome_icon = icons_dir + "google-chrome.svg"
brave_icon = icons_dir + "brave.png"
intellij_icon = icons_dir + "intellij.svg"
insomnia_icon = icons_dir + "insomnia.png"
torrent_icon = icons_dir + "qbittorrent.svg"
applications_icon = icons_tdir + "qtile.png"
settings_icon = icons_tdir + "settings4.png"

def create_image_widget(image_path, cmd, margin: int = 3):
    return widget.Image(
        filename=image_path,
        margin=margin,
        mouse_callbacks=dict(
            Button1=lambda: qtile.spawn(cmd),
        ),
        **icon_widget_defaults,
        **decorations_no_round
    )

def create_text_widget(content, foreground, size, cmd, padding: int | None = None):
    return widget.TextBox(
        padding=padding if padding is not None else 3,
        text=content,
        fontsize=size,
        foreground=foreground,
        **to_mouse_callbacks(cmd),
        **icon_widget_defaults,
        **decorations_no_round
    )

space = to_span(" ", None, 3)

#applications = create_text_widget(" ", colors.colors.color4, 25, var.run.mymenu)
applications = create_image_widget(applications_icon, var.run.mymenu, 1)
powermenu = create_text_widget("  ", colors.colors.color8, 24, var.run.powermenu, 0)
#settings = create_text_widget(space + "⚙️", colors.colors.color8, 23, var.run.gnome_settings)
settings = create_image_widget(settings_icon, var.run.gnome_settings, 1)

kitty_runner = create_image_widget(kitty_icon, var.run.terminal_kitty, -1)
wezterm_terminal = create_image_widget(wezterm_icon, "wezterm", 1)
ghostty_terminal = create_image_widget(ghostty_icon, "ghostty", -1)
pipette = create_image_widget(pipette_icon, "gpick", 2)
# chrome = create_image_widget(chrome_icon, "google-chrome-stable")
brave = create_image_widget(brave_icon, "~/dotfiles/bin/start-browser")
intellij = create_image_widget(intellij_icon, "intellij-idea-ultimate", 4)
insomnia = create_image_widget(insomnia_icon, "/opt/insomnia/insomnia %U", 2)
torrent = create_image_widget(torrent_icon, "env QT_SCALE_FACTOR=1.4 qbittorrent", 2)