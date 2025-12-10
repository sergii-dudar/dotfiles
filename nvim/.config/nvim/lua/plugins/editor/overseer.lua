return {
    "stevearc/overseer.nvim",
    event = "VeryLazy",
    enabled = false,
    opts = {
        task_list = {
            min_height = 16,
        },
    },
    config = function(_, opts)
        require("overseer").setup(opts)
        require("overseer").register_template({
            name = "Maven",
            params = function()
                return {
                    subcommand = {
                        desc = "Maven subcommand",
                        type = "list",
                        delimiter = " ",
                        subtype = {
                            type = "enum",
                            choices = {
                                "clean",
                                "compile",
                                "package",
                                "install",
                                "test",
                                "verify",
                                "deploy",
                                "dependency:tree",
                                "-DskipTests",
                                "-Dmaven.test.skip=true",
                            },
                        },
                    },
                }
            end,
            builder = function(params)
                local maven = require("utils.java.maven-util")
                local settings = maven.get_maven_settings()
                local file = vim.fn.expand("%")
                local cmd = { "mvn" }
                vim.list_extend(cmd, params.subcommand)
                if settings then
                    table.insert(cmd, "-s")
                    table.insert(cmd, settings)
                end
                if maven.is_pom_file(file) then
                    table.insert(cmd, "-f")
                    table.insert(cmd, file)
                end
                return {
                    cmd = cmd,
                }
            end,
            condition = {
                filetype = { "java", "xml" },
                callback = function(param)
                    if param.filetype == "xml" then
                        local maven = require("utils.java.maven-util")
                        return maven.is_pom_file(vim.fn.expand("%"))
                    end
                    return true
                end,
            },
        })
    end,
}

-- vim.api.nvim_set_keymap("n", "<F6>", "<cmd>CompilerOpen<cr>", { noremap = true, silent = true })
--
-- -- Redo last selected option
-- vim.api.nvim_set_keymap(
--     "n",
--     "<S-F6>",
--     "<cmd>CompilerStop<cr>" -- (Optional, to dispose all tasks before redo)
--         .. "<cmd>CompilerRedo<cr>",
--     { noremap = true, silent = true }
-- )
--
-- -- Toggle compiler results
-- vim.api.nvim_set_keymap("n", "<S-F7>", "<cmd>CompilerToggleResults<cr>", { noremap = true, silent = true })
--
