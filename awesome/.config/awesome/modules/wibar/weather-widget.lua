local vars = require("modules.variables")
local lain = require("lain")
local util = require("util.common-util")
local helpers = require("lain.helpers")

local gray = "#94928F"
local markup = lain.util.markup

-- API:
-- https://api.openweathermap.org/geo/1.0/zip?zip=21012,UA&appid=[key]
-- https://api.openweathermap.org/data/2.5/weather?lat=49.2328&lon=28.481&appid=[key]&lang=en

-- widget description: https://github.com/lcpz/lain/wiki/weather

local api_key = util.local_env_key("OPEN_WEATHER_API_KEY")
local vn_ua_geo = {
    lat = 49.2328,
    lon = 28.481,
}
local weather = lain.widget.weather({
    APPID = api_key,
    notification_preset = { font = vars.font.default, fg = gray },
    weather_na_markup = markup.fontfg(vars.font.default, gray, "N/A"),
    notification_text_fun = function(wn)
        local day = os.date("%a %d", wn["dt"])
        local temp = math.floor(wn["main"]["temp"])
        local desc = wn["weather"][1]["description"]
        return string.format("<b>%s</b>: %s, %d°C", day, desc, temp)
    end,
    --icons_path = helpers.icons_dir .. "openweather_api/",
    lat = vn_ua_geo.lat,
    lon = vn_ua_geo.lon,
    lang = "ua",
    showpopup = "off", -- off on text, bellow on on result widget by attach
    settings = function()
        local units = math.floor(weather_now["main"]["temp"])
        widget:set_markup(
            markup.fontfg(vars.font.default, gray, units) .. markup.fontfg(vars.font.default, "#6272a4", "°C")
        )
    end,
})

local weather_with_icon = util.add_icon_widget_to_widget({
    icon_widget = weather.icon,
    target_widget = weather,
    icon_right_margin = 5,
})

-- add weather hover notification to whole with icon result widget
weather.attach(weather_with_icon)

return {
    weather = weather_with_icon,
}

--[[ 
response examplse:
{
    "coord": {
        "lon": 28.481,
        "lat": 49.2328
    },
    "weather": [
        {
            "id": 804,
            "main": "Clouds",
            "description": "overcast clouds",
            "icon": "04n"
        }
    ],
    "base": "stations",
    "main": {
        "temp": 277.19,
        "feels_like": 276,
        "temp_min": 277.19,
        "temp_max": 277.19,
        "pressure": 1021,
        "humidity": 94,
        "sea_level": 1021,
        "grnd_level": 988
    },
    "visibility": 10000,
    "wind": {
        "speed": 1.5,
        "deg": 260,
        "gust": 3.05
    },
    "clouds": {
        "all": 100
    },
    "dt": 1734542962,
    "sys": {
        "country": "UA",
        "sunrise": 1734501417,
        "sunset": 1734530921
    },
    "timezone": 7200,
    "id": 689558,
    "name": "Vinnytsia",
    "cod": 200
} ]]
