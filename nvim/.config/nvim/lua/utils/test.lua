-- Scratch/experimental file: Snacks picker explorer test (not a module, not required anywhere).

Snacks.picker.explorer({
    auto_close = true,
    follow_file = false,
    actions = {
        write_to_file = function(picker, item)
            if not item then
                return
            end
            if item.dir then
                local Tree = require("snacks.explorer.tree")
                Tree:toggle(item.file)
                require("snacks.explorer.actions").update(picker, { refresh = true })
                return
            end
            picker:close()
            -- write_lines(lines, item.file)
            vim.notify(item.file)
        end,
    },
    win = {
        input = {
            keys = {
                ["<CR>"] = { "write_to_file", mode = { "i", "n" } },
            },
        },
        list = {
            keys = {
                ["<CR>"] = "write_to_file",
                ["l"] = "write_to_file",
            },
        },
    },
})
