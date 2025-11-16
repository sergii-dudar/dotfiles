local M = {}

local java_namespace = vim.api.nvim_create_namespace("java.namespace")

local severity_map = {
    ERROR = "ERROR",
    WARNING = "WARN",
    INFO = "INFO",
    HINT = "HINT",
}

local function to_severity(s)
    local key = severity_map[s:upper()] or "ERROR"
    return vim.diagnostic.severity[key]
end

local function dedupe_file_diagnstics(diadnostics)
    local seen = {}
    local out = {}

    for _, item in ipairs(diadnostics) do
        -- unique key for each error location
        local key = table.concat({
            item.lnum,
            item.col,
        }, ":")

        if not seen[key] then
            seen[key] = true
            table.insert(out, item)
        end
    end

    return out
end

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
                severity = to_severity(level),
                source = "Maven",
            })
        end
    end

    return grouped
end

local function run_maven(cmd_args)
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

                    diags = dedupe_file_diagnstics(diags)
                    vim.diagnostic.set(java_namespace, bufnr, diags, {})
                end

                -- vim.fn.setqflist(qf)
                -- vim.cmd("Trouble qflist toggle")
                vim.cmd("Trouble diagnostics open")
            end
        end)
    end)
end

M.compile = function()
    run_maven({ "-q", "compile" })
    -- run_maven({ "compile" })
end

M.clearn_compile = function()
    run_maven({ "-q", "clean", "compile" })
    -- run_maven({ "clean", "compile" })
end

M.test = function()
    run_maven({ "-q", "-DskipTests=false", "test" })
end

M.verify = function()
    run_maven({ "-q", "verify" })
end

return M
