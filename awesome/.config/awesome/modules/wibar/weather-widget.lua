local vars = require("modules.variables")
local gears = require("gears")
local awful = require("awful")
local lain = require("lain")
local util = require("util.common-util")

local gray = "#94928F"
local markup = lain.util.markup

-- https://api.openweathermap.org/geo/1.0/zip?zip=21012,UA&appid=bd5e378503939ddaee76f12ad7a97608
-- https://api.openweathermap.org/data/2.5/weather?lat=49.2328&lon=28.481&appid=bd5e378503939ddaee76f12ad7a97608&lang=en

local weather = lain.widget.weather({
    APPID = "bd5e378503939ddaee76f12ad7a97608",
    --notification_preset = { font = "Terminus 10", fg = theme.fg_normal },
    --weather_na_markup = markup.fontfg(theme.font, "#eca4c4", "N/A "),
    lat = 49.2328,
    lon = 28.481,
    settings = function()
        local name = weather_now["name"]
        local descr = weather_now["weather"][1]["main"]:lower()
        local units = math.floor(weather_now["main"]["temp"])
        widget:set_markup(markup.fontfg(vars.font.default, "#eca4c4", descr .. " @ " .. units .. "°C "))
    end,
})

--
-- local weather = lain.widget.alsa({
--     settings = function()
--         local icon = util.to_span(" ", "#ca9ee6")
--         if volume_now.status == "off" then
--             icon = util.to_span(" ", "#d35f5e")
--         elseif tonumber(volume_now.level) == 0 then
--             icon = util.to_span(" ", "#ca9ee6")
--         elseif tonumber(volume_now.level) <= 10 then
--             icon = util.to_span(" ", "#ca9ee6")
--         end
--
--         --widget:set_markup(markup.font(theme.font, " " .. volume_now.level .. "% "))
--         widget:set_markup(
--             markup.font(vars.font.widget, markup(gray, icon .. util.vars.icon_widget_space .. volume_now.level .. "%"))
--         )
--     end,
-- })

return {
    weather = weather,
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
