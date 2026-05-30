-- File manager integrations for Java refactor module.
-- Provides hooks for: neo-tree.nvim, oil.nvim, Snacks.rename
-- Replaces sergii-dudar/java.nvim (simaxme-java) with our more powerful implementation.

local M = {}

local java_util = require("utils.java.java-common")

--- Setup neo-tree.nvim integration.
--- Subscribes to FILE_RENAMED and FILE_MOVED events (fires for both files and directories).
function M.setup_neotree()
    local ok, events = pcall(require, "neo-tree.events")
    if not ok then
        return
    end

    local java_refactor = require("modules.java.refactor")

    local handle_rename = function(data)
        if not java_util.is_java_project() then
            return
        end
        vim.schedule(function()
            java_refactor.process_single_file_change(data.source, data.destination)
        end)
    end

    events.subscribe({
        event = events.FILE_RENAMED,
        handler = handle_rename,
    })

    events.subscribe({
        event = events.FILE_MOVED,
        handler = handle_rename,
    })
end

--- Setup oil.nvim integration.
--- Subscribes to OilActionsPost autocmd for move actions.
function M.setup_oil()
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

--- Setup all integrations based on config.
---@param opts? { neotree?: boolean, oil?: boolean }
function M.setup(opts)
    opts = opts or { neotree = true, oil = true }

    if opts.neotree ~= false then
        M.setup_neotree()
    end

    if opts.oil ~= false then
        M.setup_oil()
    end
end

return M
