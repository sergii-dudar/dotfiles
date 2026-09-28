-- Formatting helpers shared by the test-report core (notifications) and the tree view,
-- so a duration reads the same in both places.

local M = {}

--- Format a duration in seconds: `1.23s` from 1s up, else whole milliseconds. Reporters
--- round to milliseconds, so a recorded 0 means "faster than 1ms", shown as `<1ms`; a
--- missing (nil/negative) time renders as "".
---@param t number|nil
---@return string
function M.time(t)
    if type(t) ~= "number" or t < 0 then
        return ""
    end
    local ms = math.floor(t * 1000 + 0.5)
    if ms >= 1000 then
        return string.format("%.2fs", t)
    end
    if ms == 0 then
        return "<1ms"
    end
    return string.format("%dms", ms)
end

return M
