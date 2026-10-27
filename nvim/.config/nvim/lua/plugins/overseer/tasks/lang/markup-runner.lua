-- Browser live preview runner for markup buffers (html, xhtml, markdown, asciidoc, svg), registered in
-- utils/lang/registry.lua as the `markup` language. <leader>rr starts the live-preview.nvim server
-- (plugins/tools/live-preview.lua) in-process through `run_direct` (no overseer task, like `dap_launch` for debug),
-- <leader>rs stops it through `stop_direct`, <leader>rl starts it again for the current buffer.
-- Reload: html/xhtml on save (of any html/css/js under the webroot = cwd), markdown/asciidoc/svg as you type.
-- Run only: no debug, test or compile commands.

local lp_util = require("utils.live-preview-util")

local M = {}

--- Required by the contract, never executed: `run_direct` takes precedence in overseer-util.run_current().
---@return string[]
function M.build_run_cmd()
    return { "echo", "markup files are previewed in-process (run_direct)" }
end

function M.run_direct()
    local url, err = lp_util.start(vim.fn.expand("%:p"))
    if url then
        vim.notify("Live preview: " .. url)
    else
        vim.notify(err or "live preview failed to start", vim.log.levels.WARN)
    end
end

---@return boolean stopped
function M.stop_direct()
    if not lp_util.is_running() then
        return false
    end
    lp_util.stop()
    vim.notify("Live preview stopped")
    return true
end

return M
