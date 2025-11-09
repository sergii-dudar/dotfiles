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
            hot_reload = false,
            -- Focus on runner window(only works on toggle, term and tab mode)
            focus = false,
            term = {
                --  Position to open the terminal, this option is ignored if mode ~= term
                position = "bot",
                -- window size, this option is ignored if mode == tab
                size = 12,
                --size = 25,
            },
            filetype = {
                html = {
                    'brave "$dir/$fileName" > /dev/null 2>&1',
                    '|| "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser" "$dir/$fileName" > /dev/null 2>&1',
                },
                java = {
                    --'echo "$dir , $fileNameWithoutExt"',
                    "$HOME/dotfiles/work/java/runner "
                        .. java_util.java_bin
                        .. " $dir $fileNameWithoutExt",
                },
                yaml = {
                    -- k8s
                    "kubectl apply -f $dir/$fileName && echo $dir/$fileName has successfully applied",
                    -- "echo $dir/$fileName",
                },
                python = "python3.12 -u",
                typescript = "deno run",
                lua = "lua $dir/$fileName",
                sh = "bash $dir/$fileName",
                --haskell = "echo $dir/$fileName",
                haskell = {
                    "cd $dir",
                    --'echo "$dir , $fileNameWithoutExt"',

                    -- '&& stack build --color "always" --verbosity "warn"',
                    -- '&& current_dir=$(pwd); depth=0; while [ "$current_dir" != "/" ] && [ $depth -lt 5 ]; do find "$current_dir" -maxdepth 1 -type f -name "*.cabal" -exec basename {} .cabal \\;; current_dir=$(dirname "$current_dir"); depth=$((depth + 1)); done | sort | uniq',
                    -- "| xargs -I {} stack exec {}-exe",

                    --"&& runhaskell $fileName",

                    -- combined runner:
                    -- 1. trying to find {app-name}.cabal file in current and up to 5 up directories
                    -- 2. in case presents `cabal` file, get app name and run by `stack build && stack exec {app-name}-exe` command as `stack` project
                    -- 3. in case it's not `stack` project, run as single haskell file by `runhaskell $fileName`
                    '&& result=$(current_dir=$(pwd); depth=0; while [ "$current_dir" != "/" ] && [ $depth -lt 3 ]; do command find "$current_dir" -maxdepth 1 -type f -name "*.cabal" -exec basename {} .cabal \\;; current_dir=$(dirname "$current_dir"); depth=$((depth + 1)); done | sort | uniq)',
                    '; [ -n "$result" ] && ( stack build --color "always" --verbosity "warn" && stack exec "$result"-exe) || runhaskell $fileName',
                },
                cabal = { -- to be able to run from cabal file
                    "cd $dir",
                    '&& stack build --color "always" --verbosity "warn"',
                    "&& stack exec $fileNameWithoutExt-exe",
                },
                -- rust = {
                --     "cd $dir",
                --     "&& rustc $fileName -o /tmp/$fileNameWithoutExt",
                --     "&& /tmp/$fileNameWithoutExt",
                --     "&& rm /tmp/$fileNameWithoutExt",
                -- },
                rust = {
                    "cd $dir",
                    -- "&& cargo -q build",
                    "&& cargo -q run", --build & run
                },
                go = { "go run $dir/$fileName" },
                c = {
                    "cd $dir",
                    "&& gcc -std=c17 -Wno-format $fileName -o /tmp/$fileNameWithoutExt", -- -std=c11, -std=c17, -std=c23(c2x)
                    "&& /tmp/$fileNameWithoutExt",
                    "&& rm /tmp/$fileNameWithoutExt",
                },
                cpp = {
                    "cd $dir",
                    -- "&& g++ -std=c++23 $fileName -o /tmp/$fileNameWithoutExt", -- -std=c++11, -std=c++14, -std=c++17 (DEFAULT), -std=c++20, -std=c++23, -std=c++26(c++2c)
                    "&& g++ -std=c++23 $fileNameWithoutExt*.cpp -o /tmp/$fileNameWithoutExt", -- -std=c++11, -std=c++14, -std=c++17 (DEFAULT), -std=c++20, -std=c++23, -std=c++26(c++2c)
                    "&& /tmp/$fileNameWithoutExt",
                    "&& rm /tmp/$fileNameWithoutExt",
                },
                cs = {
                    "cd $dir",
                    -- "dotnet build && dotnet run"
                    "&& dotnet run",
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
            project = {
                -- mvn multi module project, run service module (with depencency to another module[s])
                ["~/serhii.home/work/git.work/ua-payments-payment-prevalidation"] = {
                    name = "Run Profile1",
                    description = "Run Profile1",
                    command = java_util.java_bin
                        .. " -classpath "
                        .. "$(cd payment-prevalidation && $HOME/dotfiles/work/java/mvn_cp_cash.sh $PWD)"
                        .. ":$(cd payment-prevalidation-api && $HOME/dotfiles/work/java/mvn_cp_cash.sh $PWD)"
                        .. " ua.raiffeisen.payments.paymentprevalidation.Application",
                },
            },
        })

        vim.keymap.set("n", "<leader>rr", ":RunCode<CR>", { noremap = true, silent = false, desc = "Run Code" })
        vim.keymap.set("n", "<leader>rf", ":RunFile<CR>", { noremap = true, silent = false, desc = "Run File" })
        vim.keymap.set("n", "<leader>rp", ":RunProject<CR>", { noremap = true, silent = false, desc = "Run Project" })
        vim.keymap.set("n", "<leader>rc", ":RunClose<CR>", { noremap = true, silent = false, desc = "Run Close" })
        --vim.keymap.set("n", "<leader>rd", ":CrStopHr<CR>", { noremap = true, silent = false, desc = "Stop Hot Reload" })

        --vim.keymap.set('n', '<leader>rft', ':RunFile tab<CR>', { noremap = true, silent = false, desc = "Run File tab" })
        --vim.keymap.set('n', '<leader>crf', ':CRFiletype<CR>', { noremap = true, silent = false, desc = "CRF iletype" })
        --vim.keymap.set('n', '<leader>crp', ':CRProjects<CR>', { noremap = true, silent = false, desc = "CR Projects" })

        local map = LazyVim.safe_keymap_set
        vim.api.nvim_create_autocmd("TermOpen", {
            group = vim.api.nvim_create_augroup("RunClose", { clear = true }),
            desc = "RunClose with q",
            callback = function()
                map("n", "q", "<cmd>RunClose<cr>", { buffer = true })
                map("n", "p", function()
                    require("utils.java.java-trace").parse_buffer_trace_to_qflist()
                end, { buffer = true })
            end,
        })
    end,
}
