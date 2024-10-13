return {
    "stevearc/overseer.nvim",
    event = "VeryLazy",
    opts = {
        task_list = {
            min_height = 16,
        },
    },
    config = function(_, opts)
        require("overseer").setup(opts)
        require("overseer").register_template {
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
                local maven = require("utils.maven")
                local settings = maven.get_maven_settings()
                local file = vim.fn.expand "%"
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
                        local maven = require("utils.maven")
                        return maven.is_pom_file(vim.fn.expand "%")
                    end
                    return true
                end,
            },
        }
    end,
}