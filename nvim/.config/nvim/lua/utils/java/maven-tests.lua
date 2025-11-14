local M = {}

local function dedupe_qf(qf)
    local seen = {}
    local out = {}

    for _, item in ipairs(qf) do
        -- unique key for each error location
        local key = table.concat({
            item.filename,
            item.lnum,
            item.col,
            item.text,
        }, ":")

        if not seen[key] then
            seen[key] = true
            table.insert(out, item)
        end
    end

    return out
end

local function parse_maven_output(lines)
    local qf = {}

    -- local file_pattern = "([^:%[]+):%[?(%d+),?(%d*)%]?%s*(.*)"
    local file_pattern = "%[ERROR%]%s+([^:]+):%[(%d+),(%d+)%]%s*(.*)"
    for _, line in ipairs(lines) do
        local file, lnum, col, msg = line:match(file_pattern)
        if file then
            table.insert(qf, {
                filename = file,
                lnum = tonumber(lnum) or 1,
                col = tonumber(col) or 1,
                text = msg,
                type = "E",
            })
        end
    end

    return qf
end

local function run_maven(cmd_args)
    vim.notify("Running: mvn " .. table.concat(cmd_args, " "), vim.log.levels.INFO)

    vim.system({ "mvn", unpack(cmd_args) }, { text = true }, function(res)
        local combined = vim.split(res.stdout .. res.stderr, "\n")
        dd(combined)
        local qf = parse_maven_output(combined)
        qf = dedupe_qf(qf)
        dd(qf)

        -- if next(qf) == nil then
        vim.schedule(function()
            if res.code == 0 then
                vim.notify("Maven OK", vim.log.levels.INFO)
                -- vim.fn.qflist({})
                -- vim.fn.cmd("Trouble diagnostics close")
                vim.cmd("Trouble qflist close")
            else
                vim.notify("Maven NOT OK", vim.log.levels.WARN)
                vim.fn.setqflist(qf)
                vim.cmd("Trouble qflist toggle")
                -- vim.fn.cmd("Trouble diagnostics close")
                -- vim.cmd("copen")
            end
        end)
    end)
end

M.compile = function()
    run_maven({ "-q", "compile" })
end

M.clearn_compile = function()
    run_maven({ "-q", "clean", "compile" })
end

M.test = function()
    run_maven({ "-q", "-DskipTests=false", "test" })
end

M.verify = function()
    run_maven({ "-q", "verify" })
end

return M

--[[ vim.api.nvim_create_user_command("RunMyScript", function()
    -- 1. Run the command and get output
    local output = vim.fn.system("mvn clean verify -q")
    -- if vim.v.shell_error ~= 0 then
    --     vim.notify("Command failed: " .. output, vim.log.levels.INFO)
    --     return
    -- end

    local qf_list = {}

    -- 2. Parse the raw string output
    for line in vim.gsplit(output, "\n") do
        -- Lua pattern to match: [INFO] src/main.lua:25: Found it
        local _, _, filename, lnum, text = line:find("^%[.+%]%s+(.+):(%d+):%s+(.+)$")
        if filename then
            -- 3. Build the list of tables
            table.insert(qf_list, {
                filename = filename,
                lnum = tonumber(lnum),
                text = text,
            })
        end
    end
    dd(table)

    -- 4. Send the list to the quickfix
    if #qf_list > 0 then
        vim.fn.setqflist(qf_list)
        vim.cmd("copen")
    else
        print("No matches found.")
    end
end, {}) ]]
