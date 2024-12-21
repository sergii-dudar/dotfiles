from qtile_extras import widget

from libqtile.log_utils import logger
from util import colors, vars
from util.util import get_privat_env_var, to_span
from widget.wcommon import decorations_round, decorations_round_right, text_widget_defaults

colors = colors.current()

app_key = get_privat_env_var("OPEN_WEATHER_API_KEY")
# logger.error("app_key " + app_key)

weather = widget.OpenWeather(
    #app_key="bd5e378503939ddaee76f12ad7a97608",
    coordinates={"longitude": "28.481", "latitude": "49.2328"},
    units="metric",        # Use "imperial" for Fahrenheit
    update_interval=900,   # Update every 15 minutes
        format = " {main_temp:.0f}"
        + to_span(" ", colors.colors.color4[0], 5)
        + to_span("°", colors.colors.color11[0])
        + to_span("C", colors.colors.color4[0])
        + to_span(" ", colors.colors.color4[0], 9)
        + "{icon} ",
    foreground=colors.colors.color3,
    **text_widget_defaults,
    **decorations_round
)
