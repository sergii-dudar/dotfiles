local M = {}

-- stylua: ignore
local spinners = {
    -- spinner1 = { "⠁","⠂","⠄","⡀","⢀","⠠","⠐","⠈" },
    spinner2 = { "⠋","⠙","⠹","⠸","⠼","⠴","⠦","⠧","⠇","⠏" },
    -- spinner3 = { "⠋","⠙","⠚","⠒","⠂","⠂","⠒","⠲","⠴","⠦","⠖","⠒","⠐","⠐","⠒","⠓","⠋" },
    -- spinner4 = { "⠁","⠉","⠙","⠚","⠒","⠂","⠂","⠒","⠲","⠴","⠤","⠄","⠄","⠤","⠴","⠲","⠒","⠂","⠂","⠒","⠚","⠙","⠉","⠁" },
    -- spinner5 = { "◐","◓","◑","◒" },
    -- spinner6 = { "◴","◷","◶","◵" },
    -- spinner7 = { "▖","▘","▝","▗" },
    -- spinner8 = { "▌","▀","▐","▄" },
    -- spinner9 = { "←","↖","↑","↗","→","↘","↓","↙" },
    -- spinner10 = { "⣾","⣽","⣻","⢿","⡿","⣟","⣯","⣷" },
    -- spinner11 = { "🭑","🭓","🭕","🭒" },
    -- spinner12 = { "🌝", "🌑","🌒","🌓","🌔","🌕","🌖","🌗","🌘", "🌚" },
    -- spinner13 = { "▁", "▂", "▃", "▄", "▅", "▆", "▇", "█" },
    -- spinner14 = { "🕛", "🕧", "🕐", "🕜", "🕑", "🕝", "🕒", "🕞", "🕓", "🕟", "🕔", "🕠", "🕕", "🕡", "🕖", "🕢", "🕗", "🕣", "🕘", "🕤", "🕙", "🕥", "🕚", "🕦" },
}

local spinner_frames = spinners.spinner2

local spinner_timer = nil
local spinner_index = 1
local spinner_message = ""
-- local spinner_ns = vim.api.nvim_create_namespace("job_spinner")

function M.start(msg)
    spinner_message = msg or "Running…"
    spinner_index = 1

    if spinner_timer then
        spinner_timer:stop()
        spinner_timer:close()
    end

    spinner_timer = vim.loop.new_timer()
    spinner_timer:start(
        0,
        100,
        vim.schedule_wrap(function()
            local frame = spinner_frames[spinner_index]
            spinner_index = (spinner_index % #spinner_frames) + 1

            vim.api.nvim_echo({ { frame .. " " .. spinner_message, "ModeMsg" } }, false, {})
        end)
    )
end

function M.stop(success, msg)
    if spinner_timer then
        spinner_timer:stop()
        spinner_timer:close()
        spinner_timer = nil
    end

    -- local icon = success and "✔" or "✘"
    local icon = success and "✅🎉" or "❌"
    local text = msg or (success and "Done" or "Failed")

    vim.api.nvim_echo({ { icon .. " " .. text, success and "DiffAdded" or "DiffRemoved" } }, false, {})
end

return M