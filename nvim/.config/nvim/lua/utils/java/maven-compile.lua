local M = {}

local maven_util = require("utils.java.maven-util")

local java_namespace = vim.api.nvim_create_namespace("java.compile.namespace")
local compile_autocmds = {}

local function parse_maven_output_diagnostics(lines)
    local i = 1
    while i <= #lines do
        while lines[i + 1] and lines[i + 1]:match("^%s") do
            lines[i] = lines[i] .. " " .. lines[i + 1]:gsub("^%s+", "")
            table.remove(lines, i + 1)
        end
        lines[i] = lines[i]:gsub("%s+", " ")
        i = i + 1
    end

    local grouped = {}

    local file_pattern = "%[([A-Z]+)%]%s+([^:]+):%[(%d+),(%d+)%]%s*(.*)"
    -- local line =
    --     '[WARNING] /home/serhii/serhii.home/git/tests/serhii-application/src/main/java/ua/serhii/application/mapper/UserMapper.java:[14,13] Unmapped target properties: "age, address".'
    -- local level, file, lnum, col, msg = line:match(file_pattern)
    -- print(to_severity(level))

    for _, line in ipairs(lines) do
        local level, file, lnum, col, msg = line:match(file_pattern)
        if file then
            grouped[file] = grouped[file] or {}
            col = (tonumber(col) or 1) - 1
            table.insert(grouped[file], {
                lnum = (tonumber(lnum) or 1) - 1,
                col = col,
                end_col = col + 20,
                message = msg,
                -- severity = vim.diagnostic.severity.ERROR,
                severity = maven_util.to_severity(level),
                source = "Maven",
            })
        end
    end

    return grouped
end

local function run_maven_compile(cmd_args)
    vim.notify("🚀 mvn " .. table.concat(cmd_args, " "), vim.log.levels.INFO)

    vim.system({ "mvn", unpack(cmd_args) }, { text = true }, function(res)
        local combined = vim.split(res.stdout .. res.stderr, "\n")
        local parsed = parse_maven_output_diagnostics(combined)

        -- if next(qf) == nil then
        vim.schedule(function()
            if res.code == 0 then
                vim.notify("✅🎉 Maven OK", vim.log.levels.INFO)
                -- vim.fn.qflist({})
                -- vim.cmd("Trouble diagnostics close")
                -- vim.cmd("Trouble qflist close")
                vim.diagnostic.reset(java_namespace)
            else
                vim.notify("❌ Maven NOT OK", vim.log.levels.WARN)

                for file, diags in pairs(parsed) do
                    -- load all buffers in hidden mode
                    local bufnr = vim.fn.bufadd(file)
                    vim.fn.bufload(bufnr)

                    diags = maven_util.dedupe_file_diagnstics(diags)
                    vim.diagnostic.set(java_namespace, bufnr, diags, {})
                end

                -- vim.fn.setqflist(qf)
                -- vim.cmd("Trouble qflist toggle")
                vim.cmd("Trouble diagnostics open")
            end
        end)
    end)
end

M.toggle_auto_compile = function(key)
    key = key or "toggle_compile"
    if compile_autocmds[key] then
        -- remove existing autocmd
        vim.api.nvim_del_autocmd(compile_autocmds[key])
        compile_autocmds[key] = nil
        print("📴 Autocmd removed for key:", key)
    else
        -- Create an autocommand which will run the check_current_buffer
        -- function whenever we enter or save the buffer.
        -- "BufWritePost", "BufEnter"
        local id = vim.api.nvim_create_autocmd({ "BufWritePost" }, {
            pattern = "*.java",
            callback = function()
                print("🚀 Mave auto compile triggered!")
                M.compile()
            end,
        })
        compile_autocmds[key] = id
        print("🚀 Autocmd created for key:", key)
    end
end

M.compile = function()
    run_maven_compile({ "-q", "compile" })
    -- run_maven({ "compile" })
end

M.clean_compile = function()
    run_maven_compile({ "-q", "clean", "compile" })
    -- run_maven({ "clean", "compile" })
end

return M
