function build_cmd_only_resolver(build_cmd)
    local resolver = {}
    resolver.build_cmd = build_cmd
    return resolver
end

local M = {}

function M.register(type_to_resolver)
    type_to_resolver = type_to_resolver or {}

    -- type_to_resolver["go"] = build_cmd_only_resolver(function()
    --     local file = vim.fn.expand("%:p")
    --     return { "go", "run", file }
    -- end)

    -- type_to_resolver["python"] = build_cmd_only_resolver(function()
    --     local file = vim.fn.expand("%:p")
    --     return { "python3.14", file }
    -- end)

    type_to_resolver["html"] = build_cmd_only_resolver(function()
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
    end)

    type_to_resolver["yaml"] = build_cmd_only_resolver(function()
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
    end)

    type_to_resolver["typescript"] = build_cmd_only_resolver(function()
        local file = vim.fn.expand("%:p")
        return { "deno", "run", file }
    end)

    -- type_to_resolver["javascript"] = build_cmd_only_resolver(function()
    --     local file = vim.fn.expand("%:p")
    --     return { "deno", "run", file }
    -- end)

    type_to_resolver["lua"] = build_cmd_only_resolver(function()
        local dir = vim.fn.expand("%:p:h")
        local fileName = vim.fn.expand("%:t")
        return { "lua", dir .. "/" .. fileName }
    end)

    -- type_to_resolver["sh"] = build_cmd_only_resolver(function()
    --     local dir = vim.fn.expand("%:p:h")
    --     local fileName = vim.fn.expand("%:t")
    --     return { "bash", dir .. "/" .. fileName }
    -- end)

    type_to_resolver["haskell"] = build_cmd_only_resolver(function()
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
    end)

    type_to_resolver["cabal"] = build_cmd_only_resolver(function()
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
    end)

    type_to_resolver["rust"] = build_cmd_only_resolver(function()
        local dir = vim.fn.expand("%:p:h")
        return {
            "sh",
            "-c",
            "cd " .. dir .. " && cargo -q run",
        }
    end)

    type_to_resolver["c"] = build_cmd_only_resolver(function()
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
    end)

    type_to_resolver["cpp"] = build_cmd_only_resolver(function()
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
    end)

    type_to_resolver["cs"] = build_cmd_only_resolver(function()
        local dir = vim.fn.expand("%:p:h")
        return {
            "sh",
            "-c",
            "cd " .. dir .. " && dotnet run",
        }
    end)

    return type_to_resolver
end

return M
