local M = {}

function M.toggle()
    local gaps = hl.get_config("general.gaps_in")
    if gaps.top > 0 then
        hl.config({
            -- animations = { enabled = false },
            -- decoration = { shadow = { enabled = false }, blur = { enabled = false } },
            general = { gaps_in = 0, gaps_out = 0, border_size = 1 },
            decoration = { rounding = 0 },
        })
    else
        hl.exec_cmd("hyprctl reload")
    end
end

return M