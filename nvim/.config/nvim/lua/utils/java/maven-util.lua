local M = {}

local function maven_settings()
    if vim.fn.filereadable(vim.fn.expand("~/.m2/settings.xml")) == 1 then
        return vim.fn.expand("~/.m2/settings.xml")
    end
    local maven_home = vim.env["MAVEN_HOME"]
    if maven_home and vim.fn.filereadable(maven_home .. "/conf/settings.xml") then
        return maven_home .. "/conf/settings.xml"
    end
end

M.get_maven_settings = function()
    return vim.env["MAVEN_SETTINGS_XML"] or maven_settings()
end

M.is_pom_file = function(file)
    return vim.endswith(file, "pom.xml")
end

local severity_map = {
    ERROR = "ERROR",
    WARNING = "WARN",
    INFO = "INFO",
    HINT = "HINT",
}

M.to_severity = function(log_level)
    local key = severity_map[log_level:upper()] or "ERROR"
    return vim.diagnostic.severity[key]
end

M.dedupe_file_diagnstics = function(file_diadnostics)
    local seen = {}
    local out = {}

    for _, item in ipairs(file_diadnostics) do
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

return M
