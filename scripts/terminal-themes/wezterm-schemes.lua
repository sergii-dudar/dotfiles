-- Dumps the names of wezterm's built-in colour schemes on the error log channel, one per line after a TT_SCHEME tab;
-- wezterm.sh runs `WEZTERM_LOG=error wezterm --config-file wezterm-schemes.lua show-keys` and filters them out.
local wezterm = require("wezterm")
local names = {}
for name in pairs(wezterm.color.get_builtin_schemes()) do
    names[#names + 1] = name
end
table.sort(names)
for _, name in ipairs(names) do
    wezterm.log_error("TT_SCHEME\t" .. name)
end
return {}
