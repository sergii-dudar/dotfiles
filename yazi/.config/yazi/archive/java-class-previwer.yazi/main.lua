local M = {}

function M:peek(job)
    -- os.execute("/bin/echo " .. tostring(job.file.url) .. " > /tmp/logs.txt")
    local basename = tostring(job.file.url):match("([^/]+)%.%w+$")
    local decompiledUrl = os.getenv("TMPDIR") .. "Syntax" .. basename .. ".java"
    -- os.execute("/bin/echo " .. decompiledUrl .. " > /tmp/logs.txt")

    local url = Url(decompiledUrl)
    local child = Command("cat"):arg({ tostring(url) }):stdout(Command.PIPED):stderr(Command.PIPED):spawn()

    if not child then
        return require("code"):peek(job)
    end

    local limit = job.area.h
    local i, lines = 0, ""
    repeat
        local next, event = child:read_line()
        if event == 1 then
            return require("code"):peek(job)
        elseif event ~= 0 then
            break
        end

        i = i + 1
        if i > job.skip then
            lines = lines .. next
        end
    until i >= job.skip + limit

    child:start_kill()
    if job.skip > 0 and i < job.skip + limit then
        ya.emit("peek", { math.max(0, i - limit), only_if = url, upper_bound = true })
    else
        lines = lines:gsub("\t", string.rep(" ", rt.preview.tab_size))
        ya.preview_widget(
            job,
            ui.Text.parse(lines):area(job.area):wrap(rt.preview.wrap == "yes" and ui.Wrap.YES or ui.Wrap.NO)
        )
    end
end

function M:seek(job)
    require("code"):seek(job)
end

function M:preload(job)
    -- os.execute("/bin/echo " .. tostring(job.file.url) .. " > /tmp/logs.txt")
    os.execute(os.getenv("HOME") .. "/dotfiles/bin/java/decompile.sh " .. tostring(job.file.url))
    return true
end

return M