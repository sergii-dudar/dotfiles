function build_cmd_only_resolver(build_cmd)
    local resolver = {}
    resolver.build_cmd = build_cmd
    return resolver
end

local type_to_resolver = {}

type_to_resolver["java"] = require("plugins.overseer.tasks.lang.java-runner")

type_to_resolver["go"] = build_cmd_only_resolver(function()
    local file = vim.fn.expand("%:p")
    return { "go", "run", file }
end)

type_to_resolver["python"] = build_cmd_only_resolver(function()
    local file = vim.fn.expand("%:p")
    return { "python3.14", file }
end)

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

type_to_resolver["javascript"] = build_cmd_only_resolver(function()
    local file = vim.fn.expand("%:p")
    return { "deno", "run", file }
end)

type_to_resolver["lua"] = build_cmd_only_resolver(function()
    local dir = vim.fn.expand("%:p:h")
    local fileName = vim.fn.expand("%:t")
    return { "lua", dir .. "/" .. fileName }
end)

type_to_resolver["sh"] = build_cmd_only_resolver(function()
    local dir = vim.fn.expand("%:p:h")
    local fileName = vim.fn.expand("%:t")
    return { "bash", dir .. "/" .. fileName }
end)

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

local M = {}

---@class task.lang.Runner
---@field build_cmd function
---@field build_debug_cmd function|nil - [build_debug_cmd] requiring to have defined [dap_attach_to_remote]
---@field dap_attach_to_remote function|nil
---@field dap_launch function|nil - [dap_launch] requiring to have defined [dap_launch_rerun]
---@field dap_launch_rerun function|nil

---@return task.lang.Runner|nil
function M.resolve(filetype)
    local type_resolver = type_to_resolver[filetype]
    if type_resolver then
        return type_resolver
    else
        vim.notify(filetype .. " have no any registered resolvers.", vim.log.levels.WARN)
        return nil
    end
end

M.types_supported = {} -- all basic support it's shell cmd to run by overseer
M.types_supported_debug_cmd = {}
-- M.types_supported_dap_launch = {}
local types_supported_debug_cmd_flag = {}
local types_supported_dap_launch_flag = {}

function M.is_type_supported_debug_cmd(filetype)
    return types_supported_debug_cmd_flag[filetype]
end

function M.is_type_supported_dap_launch(filetype)
    return types_supported_dap_launch_flag[filetype]
end

for key, resolver in pairs(type_to_resolver) do
    table.insert(M.types_supported, key)
    if resolver.build_debug_cmd and resolver.dap_attach_to_remote then
        table.insert(M.types_supported_debug_cmd, key)
        types_supported_debug_cmd_flag[key] = true
    end
    if resolver.dap_launch then
        -- table.insert(M.types_supported_dap_launch, key)
        types_supported_dap_launch_flag[key] = true
    end
end

--[[ print(vim.tbl_contains(M.types_supported, "jaav"))
print(vim.tbl_contains(M.types_supported, "java"))
print(vim.tbl_contains(M.types_supported_debug_cmd_flag, "jaav"))
print(vim.tbl_contains(M.types_supported_debug_cmd_flag, "java"))
print(M.types_supported_debug_cmd)
print(M.types_supported_dap_launch) ]]

return M
