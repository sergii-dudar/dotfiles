local Preview = require("snacks.picker.preview")

local M = {}

function M.pick()
    Snacks.picker({
        title = "Directories",
        finder = "proc",
        format = "file",
        cmd = "fd",
        args = { "--type", "d", "--hidden", "--exclude", ".git" },
        transform = function(item)
            item.file = item.text
            item.dir = true
        end,
        preview = function(ctx)
            Preview.cmd({
                "eza",
                "--tree",
                "--icons",
                "--level=1",
                "--color=always",
                "--group-directories-first",
                ctx.item.text,
            }, ctx)
        end,
        actions = {
            grep_in_dir = function(picker, item)
                picker:close()
                if item then
                    Snacks.picker.grep({ cwd = item.text, title = "Search: " .. item.text })
                end
            end,
            files_in_dir = function(picker, item)
                picker:close()
                if item then
                    Snacks.picker.files({ cwd = item.text, title = "Files: " .. item.text })
                end
            end,
        },
        win = {
            input = {
                keys = {
                    ["<C-s>"] = { "grep_in_dir", mode = { "i", "n" } },
                    ["<C-d>"] = { "files_in_dir", mode = { "i", "n" } },
                },
            },
            list = {
                keys = {
                    ["<C-s>"] = { "grep_in_dir", mode = { "i", "n" } },
                    ["<C-d>"] = { "files_in_dir", mode = { "i", "n" } },
                },
            },
        },
        confirm = function(picker, item)
            local selected = picker:selected()
            if #selected == 0 and item then
                selected = { item }
            end
            picker:close()
            if #selected > 1 then
                local dirs = vim.tbl_map(function(i)
                    return i.text
                end, selected)
                Snacks.picker.grep({ dirs = dirs, title = "Search: [" .. #dirs .. " dirs]" })
            elseif #selected == 1 then
                Snacks.picker.grep({ cwd = selected[1].text, title = "Search: " .. selected[1].text })
            end
        end,
    })
end

return M
