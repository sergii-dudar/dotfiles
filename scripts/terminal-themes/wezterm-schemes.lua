-- Dumps wezterm's built-in colour schemes on the error log channel, one per line: TT_SCHEME, name, background as
-- rrggbb (empty when the scheme sets none or it does not parse), tab-separated; wezterm.sh runs
-- `WEZTERM_LOG=error wezterm --config-file wezterm-schemes.lua show-keys` and filters them out.
local wezterm = require("wezterm")
local schemes = wezterm.color.get_builtin_schemes()
local names = {}
for name in pairs(schemes) do
    names[#names + 1] = name
end
table.sort(names)
for _, name in ipairs(names) do
    local bg = ""
    local ok, color = pcall(wezterm.color.parse, schemes[name].background or "")
    if ok and color then
        local r, g, b = color:srgba_u8()
        bg = string.format("%02x%02x%02x", r, g, b)
    end
    wezterm.log_error("TT_SCHEME\t" .. name .. "\t" .. bg)
end
return {}