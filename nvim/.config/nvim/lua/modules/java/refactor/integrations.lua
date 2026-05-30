-- File manager integrations for Java refactor module.
-- Provides hooks for: neo-tree.nvim, oil.nvim, fyler.nvim, Snacks.rename
-- Replaces sergii-dudar/java.nvim (simaxme-java) with our more powerful implementation.
--
-- Call sites:
--   autocmds.lua          → setup_fyler_autocmd()  (BufUnload processing)
--   fyler-nvim.lua        → fyler_on_rename()      (on_rename hook)
--   jdtls-config.lua      → setup()                (neo-tree, oil.nvim)
--   java-config.lua       → snacks_rename_current() (keymap)

local M = {}

local java_util = require("utils.java.java-common")

--- Neo-tree.nvim: subscribe to file_renamed/file_moved events.
--- These fire for both files AND directories.
local function setup_neotree()
    local java_refactor = require("modules.java.refactor")

    local handle_rename = function(data)
        if not java_util.is_java_project() then
            return
        end
        vim.schedule(function()
            java_refactor.process_single_file_change(data.source, data.destination)
        end)
    end

    local events = require("neo-tree.events")
    events.subscribe({
        event = events.FILE_RENAMED,
        handler = handle_rename,
    })

    events.subscribe({
        event = events.FILE_MOVED,
        handler = handle_rename,
    })
end

--- Oil.nvim: subscribe to OilActionsPost autocmd for move actions.
local function setup_oil()
    local java_refactor = require("modules.java.refactor")

    vim.api.nvim_create_autocmd("User", {
        pattern = "OilActionsPost",
        callback = function(event)
            if not java_util.is_java_project() then
                return
            end

            local actions = event.data and event.data.actions or {}
            for _, action in ipairs(actions) do
                if action.type == "move" then
                    local old_name = (action.src_url or ""):gsub("^oil://", "")
                    local new_name = (action.dest_url or ""):gsub("^oil://", "")
                    if old_name ~= "" and new_name ~= "" then
                        vim.schedule(function()
                            java_refactor.process_single_file_change(old_name, new_name)
                        end)
                    end
                end
            end
        end,
    })
end

-- ============================================================================
-- Public API
-- ============================================================================

--- Fyler.nvim on_rename hook — registers change for batch processing (Java)
--- or falls back to Snacks LSP rename for non-Java projects.
--- Called from: fyler-nvim.lua opts.hooks.on_rename
function M.fyler_on_rename(src, dst)
    if java_util.is_java_project() then
        require("modules.java.refactor").register_change(src, dst)
    else
        Snacks.rename.on_rename_file(src, dst)
    end
end

--- Fyler.nvim BufUnload autocmd — processes all registered changes when fyler closes.
--- Called from: autocmds.lua
function M.setup_fyler_autocmd()
    vim.api.nvim_create_autocmd({ "FileType" }, {
        pattern = "fyler",
        callback = function(ev)
            vim.api.nvim_create_autocmd({ "BufUnload" }, {
                buffer = ev.buf,
                callback = function()
                    vim.notify("Fyler: fixing after move is running...")
                    require("modules.java.refactor").process_registerd_changes()
                    vim.notify("Fyler: fixing after move was finished!")
                end,
            })
        end,
    })
end

--- Rename current file via Snacks.rename with Java refactoring.
--- Called from: java-config.lua keymap `<leader>cR`
function M.snacks_rename_current()
    local java_refactor = require("modules.java.refactor")

    Snacks.rename.rename_file({
        on_rename = function(new_name, old_name)
            java_refactor.process_single_file_change(old_name, new_name)
        end,
    })
end

--- Setup neo-tree and oil.nvim integrations.
--- Called from: jdtls-config.lua (after plugins are loaded).
function M.setup()
    if LazyVim.has("neo-tree.nvim") then
        -- vim.notify("Refactoring init neo-tree.nvim")
        setup_neotree()
    end

    if LazyVim.has("oil.nvim") then
        -- vim.notify("Refactoring init oil.nvim")
        setup_oil()
    end

    -- Fyler.nvim: process registered Java refactoring changes on buffer close
    if LazyVim.has("fyler.nvim") then
        -- vim.notify("Refactoring init fyler.nvim")
        M.setup_fyler_autocmd()
    end
end

return M
