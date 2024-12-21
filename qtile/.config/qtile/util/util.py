import os

from libqtile.log_utils import logger
from util import colors, vars
from util.vars import (
    default_font,
    default_font_size,
    default_font_widget,
    default_font_widget_size,
)


def to_font(size: float) -> str:
    return default_font + " " + str(size)

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
    font_size = font_size or default_font_size
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
    env_dict = load_env(vars.home_dir + "/private.env")
    return env_dict[key]
