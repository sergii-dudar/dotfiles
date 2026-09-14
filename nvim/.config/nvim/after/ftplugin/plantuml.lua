local PLANTUML_HTTP_PORT = 8098

local function local_plantuml_server_running(cb)
    vim.system({
        "curl",
        "-s",
        "-o",
        "/dev/null",
        "-w",
        "%{http_code}",
        "--max-time",
        "1",
        "http://127.0.0.1:" .. PLANTUML_HTTP_PORT .. "/",
    }, { text = true }, function(obj)
        vim.schedule(function()
            cb(obj.code == 0)
        end)
    end)
end

local function ensure_local_plantuml_server(cb)
    local_plantuml_server_running(function(running)
        if running then
            cb()
            return
        end
        local ok = pcall(vim.system, { "plantuml", "--http-server:" .. PLANTUML_HTTP_PORT }, { detach = true })
        if not ok then
            vim.notify("plantuml: could not start local http-server (is `plantuml` on PATH?)", vim.log.levels.ERROR)
            return
        end
        vim.defer_fn(cb, 1000) -- give the JVM a moment to bind the port
    end)
end

vim.api.nvim_create_user_command("PlantumlRun", function()
    ensure_local_plantuml_server(function()
        vim.cmd("PlantumlServerStart")
        vim.cmd("PlantumlLaunchBrowser")
        vim.cmd("PlantumlUpdate")
    end)
end, { desc = "Render current PlantUML file as SVG in a live-updating local browser preview" })

-- The live viewer (:PlantumlRun) only ever shows the most recently updated
-- file, since it's one browser tab reused across buffers. This opens the
-- current buffer's rendered SVG at its own URL instead, so several diagrams
-- can be open side by side, each a snapshot of the buffer content at the time
-- of opening (won't live-update on save, unlike the :PlantumlRun viewer).
vim.api.nvim_create_user_command("PlantumlOpenSvg", function()
    ensure_local_plantuml_server(function()
        local encoder = require("plantuml.encoder")
        local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
        local text = table.concat(lines, "\n")
        if text:match("^%s*$") then
            vim.notify("PlantUML: buffer is empty", vim.log.levels.WARN)
            return
        end
        local url = encoder.encode(text, "http://127.0.0.1:" .. PLANTUML_HTTP_PORT)
        vim.ui.open(url)
    end)
end, { desc = "Open current PlantUML diagram as a standalone SVG in a new browser tab" })

-- stylua: ignore
vim.api.nvim_set_keymap("n", "<leader>ru", ":PlantumlOpenSvg<CR>", { noremap = true, silent = true, desc = "PlantUML: open diagram as standalone SVG tab" })
vim.api.nvim_set_keymap("n", "<leader>rr", ":PlantumlRun<CR>", { noremap = true, silent = true, desc = "Run plantuml" })
