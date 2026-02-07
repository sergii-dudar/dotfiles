local build_resolvers = function()
    local type_to_resolver = {}

    type_to_resolver["go"] = function()
        local file = vim.fn.expand("%:p")
        return { "go", "run", file }
    end

    type_to_resolver["python"] = function()
        local file = vim.fn.expand("%:p")
        return { "python3.14", file }
    end

    type_to_resolver["java"] = function()
        local java_runner = require("plugins.overseer.tasks.lang.java-runner")
        return java_runner.build_cmd()
    end

    type_to_resolver["html"] = function()
        local dir = vim.fn.expand("%:p:h")
        local fileName = vim.fn.expand("%:t")
        return {
            "sh",
            "-c",
            'brave "'
                .. dir
                .. "/"
                .. fileName
                .. '" > /dev/null 2>&1 '
                .. '|| "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser" "'
                .. dir
                .. "/"
                .. fileName
                .. '" > /dev/null 2>&1',
        }
    end

    type_to_resolver["yaml"] = function()
        local dir = vim.fn.expand("%:p:h")
        local fileName = vim.fn.expand("%:t")
        return {
            "sh",
            "-c",
            "kubectl apply -f "
                .. dir
                .. "/"
                .. fileName
                .. " && echo "
                .. dir
                .. "/"
                .. fileName
                .. " has successfully applied",
        }
    end

    type_to_resolver["typescript"] = function()
        local file = vim.fn.expand("%:p")
        return { "deno", "run", file }
    end

    type_to_resolver["javascript"] = function()
        local file = vim.fn.expand("%:p")
        return { "deno", "run", file }
    end

    type_to_resolver["lua"] = function()
        local dir = vim.fn.expand("%:p:h")
        local fileName = vim.fn.expand("%:t")
        return { "lua", dir .. "/" .. fileName }
    end

    type_to_resolver["sh"] = function()
        local dir = vim.fn.expand("%:p:h")
        local fileName = vim.fn.expand("%:t")
        return { "bash", dir .. "/" .. fileName }
    end

    type_to_resolver["haskell"] = function()
        local dir = vim.fn.expand("%:p:h")
        local fileName = vim.fn.expand("%:t")
        return {
            "sh",
            "-c",
            "cd "
                .. dir
                .. ' && result=$(current_dir=$(pwd); depth=0; while [ "$current_dir" != "/" ] && [ $depth -lt 3 ]; do command find "$current_dir" -maxdepth 1 -type f -name "*.cabal" -exec basename {} .cabal \\;; current_dir=$(dirname "$current_dir"); depth=$((depth + 1)); done | sort | uniq)'
                .. '; [ -n "$result" ] && ( stack build --color "always" --verbosity "warn" && stack exec "$result"-exe) || runhaskell '
                .. fileName,
        }
    end

    type_to_resolver["cabal"] = function()
        local dir = vim.fn.expand("%:p:h")
        local fileNameWithoutExt = vim.fn.expand("%:t:r")
        return {
            "sh",
            "-c",
            "cd "
                .. dir
                .. ' && stack build --color "always" --verbosity "warn" && stack exec '
                .. fileNameWithoutExt
                .. "-exe",
        }
    end

    type_to_resolver["rust"] = function()
        local dir = vim.fn.expand("%:p:h")
        return {
            "sh",
            "-c",
            "cd " .. dir .. " && cargo -q run",
        }
    end

    type_to_resolver["c"] = function()
        local dir = vim.fn.expand("%:p:h")
        local fileName = vim.fn.expand("%:t")
        local fileNameWithoutExt = vim.fn.expand("%:t:r")
        return {
            "sh",
            "-c",
            "cd "
                .. dir
                .. " && gcc -std=c17 -Wno-format "
                .. fileName
                .. " -o /tmp/"
                .. fileNameWithoutExt
                .. " && /tmp/"
                .. fileNameWithoutExt
                .. " && rm /tmp/"
                .. fileNameWithoutExt,
        }
    end

    type_to_resolver["cpp"] = function()
        local fileNameWithoutExt = vim.fn.expand("%:t:r")
        local fileDir = vim.fn.expand("%:p:h")
        return {
            "sh",
            "-c",
            "g++ -std=c++23 "
                .. fileDir
                .. "/"
                .. fileNameWithoutExt
                .. "*.cpp -o /tmp/"
                .. fileNameWithoutExt
                .. " && /tmp/"
                .. fileNameWithoutExt
                .. " && rm /tmp/"
                .. fileNameWithoutExt,
        }
    end

    type_to_resolver["cs"] = function()
        local dir = vim.fn.expand("%:p:h")
        return {
            "sh",
            "-c",
            "cd " .. dir .. " && dotnet run",
        }
    end

    return type_to_resolver
end

return {
    name = "RUN_CURRENT",
    builder = function()
        local resolvers = build_resolvers()
        local type_resolver = resolvers[vim.bo.filetype]
        local result_cmd
        if type_resolver then
            result_cmd = type_resolver()
        else
            local file = vim.fn.expand("%:p")
            vim.notify(file .. " is not supported.")
            result_cmd = { "echo", file .. " is not supported." }
        end
        return {
            cmd = result_cmd,
            -- add some components that will pipe the output to quickfix,
            -- parse it using errorformat, and display any matching lines as diagnostics.
            components = {
                { "on_output_quickfix", set_diagnostics = true },
                "on_result_diagnostics",
                "on_exit_set_status",
                "on_complete_dispose",
                -- "on_complete_notify",
                -- { "on_complete_dispose", require_view = { "SUCCESS", "FAILURE" } },
                -- "on_result_diagnostics_trouble",
                -- "open_output",
                -- "on_output_notify",
                --"default",
            },
        }
    end,
    condition = {
        filetype = {
            "sh",
            "python",
            "go",
            "cpp",
            "java",
            "html",
            "yaml",
            "typescript",
            "javascript",
            "lua",
            "haskell",
            "cabal",
            "rust",
            "c",
            "cs",
        },
    },
}
