local vars = require("modules.variables")
local gears = require("gears")
local awful = require("awful")
local lain = require("lain")
local util = require("util.common-util")
local helpers = require("lain.helpers")

local gray = "#94928F"
local markup = lain.util.markup

-- https://api.openweathermap.org/geo/1.0/zip?zip=21012,UA&appid=[key]
-- https://api.openweathermap.org/data/2.5/weather?lat=49.2328&lon=28.481&appid=[key]&lang=en

local api_key = util.locad_env_key("OPEN_WEATHER_API_KEY")
local vn_ua_geo = {
    lat = 49.2328,
    lon = 28.481,
}
print("api key " .. api_key)
local weather = lain.widget.weather({
    APPID = api_key,
    notification_preset = { font = vars.font.default, fg = gray },
    weather_na_markup = markup.fontfg(vars.font.default, gray, "N/A"),
    --icons_path = helpers.icons_dir .. "openweather_api/",
    lat = vn_ua_geo.lat,
    lon = vn_ua_geo.lon,
    settings = function()
        local units = math.floor(weather_now["main"]["temp"])
        widget:set_markup(markup.fontfg(vars.font.default, gray, units .. "°C"))
    end,
})

return {
    weather = util.add_icon_widget_to_widget({
        icon_widget = weather.icon,
        target_widget = weather,
        icon_right_margin = 5,
    }),
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
