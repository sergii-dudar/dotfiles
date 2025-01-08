local java_util = require("utils.java.java-util")

return {
    "CRAG666/code_runner.nvim",
    keys = {
        { "<leader>r", desc = "Run ..." },
    },
    config = function()
        require("code_runner").setup({
            -- choose default mode (valid term, tab, float, toggle, vimux)
            mode = "term",
            -- add hot reload
            hot_reload = true,
            -- Focus on runner window(only works on toggle, term and tab mode)
            focus = false,
            term = {
                --  Position to open the terminal, this option is ignored if mode ~= term
                position = "bot",
                -- window size, this option is ignored if mode == tab
                size = 12,
            },
            filetype = {
                java = {
                    --"java -classpath $(mvn -o -q dependency:build-classpath -Dmdep.outputFile=/dev/stdout -DincludeScope=runtime):target/classes",
                    --"$(grep '^package' $dir/$fileName | awk '{print $2}' | sed 's/;//').$fileNameWithoutExt"
                    --"~/.jdks/corretto-21.0.4/bin/java",
                    --"~/.sdkman/candidates/java/21.*-amzn/bin/java",
                    --"~/.sdkman/candidates/java/21.*-oracle/bin/java",
                    java_util.java21_bin,
                    "-classpath $($HOME/dotfiles/work/java/mvn_cp_cash.sh $dir)",
                    "$(grep '^package' $file | awk '{print $2}' | sed 's/;//').$fileNameWithoutExt",

                    --"echo $($HOME/dotfiles/work/java/mvn_cp_cash.sh $dir)",
                },
                python = "python3.12 -u",
                typescript = "deno run",
                lua = "lua $dir/$fileName",
                sh = "bash $dir/$fileName",
                haskell = "runhaskell $dir/$fileName",
                rust = {
                    "cd $dir",
                    "&& rustc $fileName -o /tmp/$fileNameWithoutExt",
                    "&& /tmp/$fileNameWithoutExt",
                    "&& rm /tmp/$fileNameWithoutExt",
                },
                go = { "go run $dir/$fileName" },
                c = {
                    "cd $dir",
                    "&& gcc -Wno-format $fileName -o /tmp/$fileNameWithoutExt",
                    "&& /tmp/$fileNameWithoutExt",
                    "&& rm /tmp/$fileNameWithoutExt",
                },
                cpp = {
                    "cd $dir",
                    "&& g++ $fileName -o /tmp/$fileNameWithoutExt",
                    "&& /tmp/$fileNameWithoutExt",
                    "&& rm /tmp/$fileNameWithoutExt",
                },
                -- c = function(...)
                --     c_base = {
                --         "cd $dir &&",
                --         "gcc $fileName -o",
                --         "/tmp/$fileNameWithoutExt",
                --     }
                --     local c_exec = {
                --         "&& /tmp/$fileNameWithoutExt &&",
                --         "rm /tmp/$fileNameWithoutExt",
                --     }
                --     vim.ui.input({ prompt = "Add more args:" }, function(input)
                --         c_base[4] = input
                --         vim.print(vim.tbl_extend("force", c_base, c_exec))
                --         require("code_runner.commands").run_from_fn(vim.list_extend(c_base, c_exec))
                --     end)
                -- end,
            },
            --project = {
            --    ["~/serhii.home/prev_work/GL_WORK/git.work/ticket%-service"] = {
            --        name = "Run Profile1",
            --        description = "Run Profile1",
            --        --file_name = "POO/main.py"
            --        command = "java -classpath $($HOME/dotfiles/work/java/mvn_cp_cash.sh):target/classes "
            --            .."$(grep '^package' $file | awk '{print $2}' | sed 's/;//').$fileNameWithoutExt"
            --    },
            --    --["~/serhii.home/prev_work/GL_WORK/git.work/ticket-service"] = {
            --    --    name = "Run Profile2",
            --    --    description = "Run Profile2",
            --    --    --file_name = "POO/main.py"
            --    --    command = "java -classpath $($HOME/dotfiles/work/java/mvn_cp_cash.sh):target/classes "
            --    --        .."$(grep '^package' $file | awk '{print $2}' | sed 's/;//').$fileNameWithoutExt"
            --    --},
            --}
        })

        vim.keymap.set("n", "<leader>rr", ":RunCode<CR>", { noremap = true, silent = false, desc = "Run Code" })
        vim.keymap.set("n", "<leader>rf", ":RunFile<CR>", { noremap = true, silent = false, desc = "Run File" })
        vim.keymap.set("n", "<leader>rp", ":RunProject<CR>", { noremap = true, silent = false, desc = "Run Project" })
        vim.keymap.set("n", "<leader>rc", ":RunClose<CR>", { noremap = true, silent = false, desc = "Run Close" })

        --vim.keymap.set('n', '<leader>rft', ':RunFile tab<CR>', { noremap = true, silent = false, desc = "Run File tab" })
        --vim.keymap.set('n', '<leader>crf', ':CRFiletype<CR>', { noremap = true, silent = false, desc = "CRF iletype" })
        --vim.keymap.set('n', '<leader>crp', ':CRProjects<CR>', { noremap = true, silent = false, desc = "CR Projects" })

        vim.api.nvim_create_autocmd("TermOpen", {
            group = vim.api.nvim_create_augroup("RunClose", { clear = true }),
            desc = "RunClose with q",
            callback = function()
                vim.keymap.set("n", "q", "<cmd>RunClose<cr>", { buffer = true })
            end,
        })
    end,
}
