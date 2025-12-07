local M = {}

local maven_util = require("utils.java.maven-util")
local spinner = require("utils.ui.spinner")
local java_util = require("utils.java.java-common")
local constants = require("utils.constants")

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

    for _, line in ipairs(lines) do
        local parsed = java_util.parse_mvn_compile_java_line(line)
        if parsed then
            grouped[parsed.file] = grouped[parsed.file] or {}
            local col = (tonumber(parsed.col) or 1) - 1
            table.insert(grouped[parsed.file], {
                lnum = (tonumber(parsed.lnum) or 1), -- - 1,
                col = col,
                end_col = col + 20,
                message = parsed.message,
                -- severity = vim.diagnostic.severity.ERROR,
                severity = parsed.severity,
                source = constants.java.maven_diagnostics_compile_source,
            })
        end
    end

    return grouped
end

local function run_maven_compile(cmd_args)
    -- vim.notify("🚀 mvn " .. table.concat(cmd_args, " "), vim.log.levels.INFO)
    spinner.start("🚀 mvn " .. table.concat(cmd_args, " "))

    vim.system({ "mvn", unpack(cmd_args) }, { text = true, cwd = vim.fn.getcwd() }, function(res)
        local combined = vim.split(res.stdout .. res.stderr, "\n")
        local parsed = parse_maven_output_diagnostics(combined)

        -- if next(qf) == nil then
        vim.schedule(function()
            spinner.stop(res.code == 0, "Maven compile")
            if res.code == 0 then
                -- vim.notify("✅🎉 Maven OK", vim.log.levels.INFO)
                -- vim.fn.qflist({})
                -- vim.cmd("Trouble diagnostics close")
                -- vim.cmd("Trouble qflist close")
                vim.cmd("JdtUpdateConfig") -- tell jdts about fix
                -- vim.diagnostic.reset(java_namespace)
                vim.diagnostic.reset()
            else
                -- vim.notify("❌ Maven NOT OK", vim.log.levels.WARN)

                vim.diagnostic.reset()
                for file, diags in pairs(parsed) do
                    -- load all buffers in hidden mode
                    local bufnr = vim.fn.bufadd(file)
                    vim.fn.bufload(bufnr)

                    diags = maven_util.dedupe_file_diagnstics(diags)
                    vim.diagnostic.set(java_namespace, bufnr, diags, {})
                end

                -- vim.fn.setqflist(qf)
                -- vim.cmd("Trouble qflist toggle")
                -- vim.cmd("Trouble diagnostics open")
                vim.cmd("Trouble maven_compile_diagnostics open")
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
    run_maven_compile({ "-q", "compile", "test-compile" })
    -- run_maven({ "compile" })
end

M.clean_compile = function()
    run_maven_compile({ "-q", "clean", "compile", "test-compile" })
    -- run_maven({ "clean", "compile" })
end

return M
