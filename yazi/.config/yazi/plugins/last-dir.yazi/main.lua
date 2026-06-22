local M = {}

--- Record the directory that is about to be left.
local before_cd = ya.sync(function(st, target)
    local cwd = tostring(cx.active.current.cwd)
    if target and target ~= cwd then
        st.prev = cwd
    end
    st.cwd = cwd
end)

--- Update the tracked current directory after a successful cd.
local after_cd = ya.sync(function(st, url)
    local cwd = url or tostring(cx.active.current.cwd)
    if st.cwd and st.cwd ~= cwd then
        st.prev = st.cwd
    end
    st.cwd = cwd
end)

--- Return the previous directory after reconciling the current cwd.
local target = ya.sync(function(st)
    local cwd = tostring(cx.active.current.cwd)
    if st.cwd and st.cwd ~= cwd then
        st.prev = st.cwd
        st.cwd = cwd
    elseif not st.cwd then
        st.cwd = cwd
    end

    return st.prev
end)

--- Start tracking the last visited directory.
function M:setup()
    local function stash(body)
        before_cd(body and body.target and tostring(body.target))
        return body
    end

    ps.sub("ind-stash", stash)
    ps.sub("relay-stash", stash)
    ps.sub("cd", function(body)
        after_cd(body and body.url and tostring(body.url))
    end)
end

--- Switch to the previously visited directory.
function M:entry()
    local prev = target()
    if not prev then
        return ya.notify({
            title = "Last directory",
            content = "No previous directory yet.",
            timeout = 3,
            level = "warn",
        })
    end

    ya.emit("cd", { prev, raw = true })
end

return M
