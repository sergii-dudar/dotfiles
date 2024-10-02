return {
    "CRAG666/code_runner.nvim",
    config = function()
        require('code_runner').setup({
            filetype = {
                java = {
                    --"java -classpath $(mvn -o -q dependency:build-classpath -Dmdep.outputFile=/dev/stdout -DincludeScope=runtime):target/classes",
                    "java -classpath $($HOME/dotfiles/work/java/mvn_cp_cash.sh):target/classes",
                    --"$(grep '^package' $dir/$fileName | awk '{print $2}' | sed 's/;//').$fileNameWithoutExt"
                    "$(grep '^package' $file | awk '{print $2}' | sed 's/;//').$fileNameWithoutExt"
                },
                python = "python3 -u",
                typescript = "deno run",
                rust = {
                    "cd $dir &&",
                    "rustc $fileName &&",
                    "$dir/$fileNameWithoutExt"
                },
                c = function(...)
                    c_base = {
                        "cd $dir &&",
                        "gcc $fileName -o",
                        "/tmp/$fileNameWithoutExt",
                    }
                    local c_exec = {
                        "&& /tmp/$fileNameWithoutExt &&",
                        "rm /tmp/$fileNameWithoutExt",
                    }
                    vim.ui.input({ prompt = "Add more args:" }, function(input)
                        c_base[4] = input
                        vim.print(vim.tbl_extend("force", c_base, c_exec))
                        require("code_runner.commands").run_from_fn(vim.list_extend(c_base, c_exec))
                    end)
                end,
            },
        })

        vim.keymap.set('n', '<leader>rr', ':RunCode<CR>', { noremap = true, silent = false, desc = "Run Code" })
        vim.keymap.set('n', '<leader>rf', ':RunFile<CR>', { noremap = true, silent = false, desc = "Run File" })
        vim.keymap.set('n', '<leader>rp', ':RunProject<CR>', { noremap = true, silent = false, desc = "Run Project" })
        vim.keymap.set('n', '<leader>rc', ':RunClose<CR>', { noremap = true, silent = false, desc = "Run Close" })

        --vim.keymap.set('n', '<leader>rft', ':RunFile tab<CR>', { noremap = true, silent = false, desc = "Run File tab" })
        --vim.keymap.set('n', '<leader>crf', ':CRFiletype<CR>', { noremap = true, silent = false, desc = "CRF iletype" })
        --vim.keymap.set('n', '<leader>crp', ':CRProjects<CR>', { noremap = true, silent = false, desc = "CR Projects" })
    end
}