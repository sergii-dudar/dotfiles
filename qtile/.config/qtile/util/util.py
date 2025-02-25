import os
import subprocess

from libqtile import qtile
from libqtile.log_utils import logger
from util import colors
from util.vars import var


def to_font(size: float) -> str:
    return var.font.default_font + " " + str(size)

def to_span(content: str, foreground: str | None = None, font_size: float | None = None) -> str:
    """
    Converts content into an HTML-like span string with optional foreground color and font size.

    Args:
        content (str): The span content.
        foreground (str | None): The foreground color for the span content (optional).
        font_size (int | None): The font size for the span content (optional).

    Returns:
        str: The formatted span string.
    """
    # Set default values
    content = content or ""
    foreground = f"foreground='{foreground}'" if foreground is not None else ""
    font_size = font_size or var.font.default_font_size
    font = f"font='{to_font(font_size)}'"

    # Return the formatted span
    return f"<span {foreground} {font}>{content}</span>"

def load_env(path):
    # logger.error("---->>>> " + path)

    env_dict = {}

    # Open the .env file
    with open(path) as f:
        for line in f:
            # Remove any leading/trailing whitespace and skip comments
            line = line.strip()
            if line and not line.startswith('#'):
                # Split the line into key and value
                key, value = line.split('=', 1)
                env_dict[key.strip()] = value.strip()

    return env_dict

def get_privat_env_var(key: str) -> str:
    env_dict = load_env(var.path.home_dir + "/private.env")
    return env_dict[key]

def to_mouse_callbacks(left_click_cmd: str | None = None, right_click_cmd: str | None = None) -> dict:

    left_callback = dict({})
    if left_click_cmd is not None:
        left_callback = dict(Button1=lambda: qtile.spawn(left_click_cmd))

    right_callback = dict({})
    if right_click_cmd is not None:
        right_callback = dict(Button3=lambda: qtile.spawn(right_click_cmd))

    return dict(
        mouse_callbacks = dict(
            **left_callback,
            **right_callback
        ),
    )

def calculate_window_width(factor_width: float | None = None):
    factor_width = factor_width if factor_width is not None else var.settings.default_factor_width
    return int(var.settings.screen_width * factor_width)

def calculate_window_height(factor_height: float | None = None):
    factor_height = factor_height if factor_height is not None else var.settings.default_factor_height
    return int(var.settings.screen_height * factor_height)

def get_monitor_count():
    output = subprocess.run(["xrandr", "--listmonitors"], capture_output=True, text=True)
    first_line = output.stdout.split("\n")[0]  # Get the first line: "Monitors: 2"
    return int(first_line.split()[1])  # Extract the number "2"

