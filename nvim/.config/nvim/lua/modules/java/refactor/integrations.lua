-- File manager integrations for Java refactor module.
-- Provides hooks for: neo-tree.nvim, oil.nvim, Snacks.rename
-- Replaces sergii-dudar/java.nvim (simaxme-java) with our more powerful implementation.
--
-- Usage: require("modules.java.refactor.integrations").setup()
-- Automatically detects available plugins via LazyVim.has() and sets up accordingly.

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

--- Rename current file via Snacks.rename with Java refactoring.
--- Intended for keymap: `<leader>cR`
function M.snacks_rename_current()
    local java_refactor = require("modules.java.refactor")

    Snacks.rename.rename_file({
        on_rename = function(new_name, old_name)
            java_refactor.process_single_file_change(old_name, new_name)
        end,
    })
end

--- Setup all available integrations.
--- Detects installed plugins via LazyVim.has() and defers subscription until plugin loads.
--- Safe to call multiple times — only runs once.
function M.setup()
    if M._setup_done then
        return
    end
    M._setup_done = true

    if LazyVim.has("neo-tree.nvim") then
        -- Defer neo-tree subscription until it actually loads (it's lazy-loaded by keys)
        local function try_setup_neotree()
            if package.loaded["neo-tree"] then
                setup_neotree()
                return true
            end
            return false
        end

        if not try_setup_neotree() then
            vim.api.nvim_create_autocmd("User", {
                pattern = "LazyLoad",
                callback = function(event)
                    if event.data == "neo-tree.nvim" then
                        setup_neotree()
                        return true -- remove autocmd
                    end
                end,
            })
        end
    end

    if LazyVim.has("oil.nvim") then
        setup_oil()
    end
end

return M
