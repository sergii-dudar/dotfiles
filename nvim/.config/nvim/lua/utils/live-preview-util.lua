-- Start/stop the live-preview.nvim server (plugin spec: plugins/tools/live-preview.lua) for one file.
-- Used by the `markup` runner (plugins/overseer/tasks/lang/markup-runner.lua) behind <leader>rr / <leader>rs for
-- html/xhtml/markdown/asciidoc/svg buffers; nothing here is bound to a key of its own.

local M = {}

local PREFERRED_PORT = 5500
local PLUGIN = "live-preview.nvim"

---@param port integer
---@return boolean
local function port_is_free(port)
    local sock = vim.uv.new_tcp()
    if not sock then
        return false
    end
    local ok = sock:bind("127.0.0.1", port) == 0 and sock:listen(1, function() end) == 0
    sock:close()
    return ok
end

--- Every Neovim instance runs its own server (tmux: several at once). The plugin only warns when the port is
--- taken and then silently serves nothing, so pick the first free port from the preferred one upwards.
---@return integer
local function free_port()
    for port = PREFERRED_PORT, PREFERRED_PORT + 50 do
        if port_is_free(port) then
            return port
        end
    end
    return PREFERRED_PORT
end

--- The plugin is lazy (loads on :LivePreview); make sure it is on the runtime path before requiring it.
local function load_plugin()
    local ok, lazy = pcall(require, "lazy")
    if ok then
        lazy.load({ plugins = { PLUGIN } })
    end
end

---@return boolean
function M.is_running()
    local lp = package.loaded["livepreview"]
    return lp ~= nil and lp.is_running()
end

--- Start (replacing any running server) the live preview of `file` and open it in the browser.
--- Webroot is cwd (so `../x.css` links resolve like in IntelliJ); a file outside cwd is served from its own
--- directory because the plugin builds the URL relative to cwd and errors otherwise.
---@param file string absolute path
---@return string? url nil on failure
---@return string? err
function M.start(file)
    load_plugin()
    local lp = require("livepreview")
    local config = require("livepreview.config")
    local utils = require("livepreview.utils")

    file = vim.fs.normalize(file)
    if not utils.supported_filetype(file) then
        return nil, "live preview supports html/xhtml/markdown/asciidoc/svg files, not " .. file
    end
    local rel = vim.fs.relpath(vim.fs.normalize(vim.uv.cwd() or ""), file)
    config.set({ port = free_port(), dynamic_root = rel == nil })
    if not lp.start(file, config.config.port) then
        return nil, "live-preview.nvim could not start its server"
    end
    local urlpath = rel or vim.fs.basename(file)
    local url = ("http://%s:%d/%s"):format(config.config.address, config.config.port, vim.uri_encode(urlpath))
    utils.open_browser(url, config.config.browser)
    return url
end

function M.stop()
    if M.is_running() then
        require("livepreview").close()
    end
end

return M
