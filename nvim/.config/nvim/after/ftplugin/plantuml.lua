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
