-- live-preview.nvim: IntelliJ-style "open in browser and auto-refresh" for the file in the current buffer.
--   html / xhtml           reload on save of any .html/.xhtml/.htm/.css/.js under the webroot
--   markdown / adoc / svg  re-rendered on every change (no save needed), browser scroll follows the cursor
-- The HTTP + WebSocket server runs inside Neovim on libuv: no node/npm/python involved (the LazyVim
-- markdown extra's markdown-preview.nvim, a node app, is disabled below in favour of this one).
--
-- Started like every other runner, no key of its own: <leader>rr in an html/xhtml/markdown/asciidoc/svg buffer
-- runs the `markup` runner (plugins/overseer/tasks/lang/markup-runner.lua, in-process via `run_direct` on top of
-- utils/live-preview-util.lua), <leader>rs stops the server, <leader>rl starts it again.
-- Manual use: :LivePreview start [file] | close | pick   |   :checkhealth livepreview   |   :h livepreview
-- Webroot: cwd, so links above the file (`../x.css`) resolve like in IntelliJ; a file outside cwd is served from
-- its own directory instead (the plugin builds the URL relative to cwd and errors otherwise).

--- The plugin recognises files by extension (.html/.md/.adoc/.svg); .xhtml/.htm are html for our purposes.
---@param name string?
---@return boolean
local function is_html_variant(name)
    return name ~= nil and (name:match("%.xhtml$") or name:match("%.htm$")) ~= nil
end

--- Brave when installed (same browser as the previous overseer html runner), otherwise the system default via
--- vim.ui.open. `browser` is run through `sh -c`, so the zsh `brave` alias does not count on macOS: use the app.
---@return string
local function browser_cmd()
    if vim.fn.has("mac") == 1 then
        if vim.fn.isdirectory("/Applications/Brave Browser.app") == 1 then
            return 'open -a "Brave Browser"'
        end
    elseif vim.fn.executable("brave") == 1 then
        return "brave"
    elseif vim.fn.executable("brave-browser") == 1 then
        return "brave-browser"
    end
    return "default"
end

return {
    {
        "brianhuster/live-preview.nvim",
        cmd = "LivePreview",
        opts = {
            port = 5500, -- utils/live-preview-util picks the first free port from here on every start
            browser = browser_cmd(),
            dynamic_root = false,
            sync_scroll = true,
            picker = "snacks.picker", -- :LivePreview pick lists the previewable files under cwd
        },
        config = function(_, opts)
            -- Runs before the server modules are loaded (plugin/livepreview.lua only pulls in utils + health),
            -- so every later `supported_filetype` capture sees .xhtml/.htm as html: :LivePreview start accepts
            -- them, the server injects the reload script and serves them as text/html.
            local utils = require("livepreview.utils")
            local supported_filetype = utils.supported_filetype
            utils.supported_filetype = function(name)
                if is_html_variant(name) then
                    return "html"
                end
                return supported_filetype(name)
            end

            -- The plugin's own save watcher only reloads the page for .html/.css/.js changes; cover .xhtml/.htm.
            -- Fires only while an html-type preview is running (the watcher is started by the plugin itself).
            vim.api.nvim_create_autocmd("User", {
                group = vim.api.nvim_create_augroup("LivePreviewHtmlVariants", { clear = true }),
                pattern = "LivePreviewDirChanged",
                callback = function(ev)
                    if not is_html_variant(ev.data and ev.data.filename) then
                        return
                    end
                    local server = require("livepreview.server")
                    for _, client in ipairs(server.connecting_clients) do
                        server.websocket.send_json(client, { type = "reload" })
                    end
                end,
            })

            require("livepreview.config").set(opts)
        end,
    },
    -- LazyVim markdown extra ships markdown-preview.nvim (node app) on <leader>cp for markdown; previews run through
    -- <leader>rr here, so it is dropped. Delete this line to get that plugin and its key back.
    { "iamcco/markdown-preview.nvim", enabled = false },
}
