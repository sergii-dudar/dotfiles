-- Maven settings resolution and diagnostic parsing helpers.
--
-- • get_maven_settings — resolve path to active Maven settings.xml
-- • is_pom_file — check if a file path is a pom.xml
-- • to_severity — map maven severity string to vim diagnostic severity
-- • dedupe_file_diagnstics — deduplicate diagnostics for a file

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

--- Resolve the active Maven settings.xml path.
function M.get_maven_settings()
    return vim.env["MAVEN_SETTINGS_XML"] or maven_settings()
end

--- Check whether the path points to a pom.xml file.
function M.is_pom_file(file)
    return vim.endswith(file, "pom.xml")
end

local severity_map = {
    ERROR = "ERROR",
    WARNING = "WARN",
    INFO = "INFO",
    HINT = "HINT",
}

--- Map a severity string to a vim diagnostic severity.
function M.to_severity(log_level)
    local key = severity_map[log_level:upper()] or "ERROR"
    return vim.diagnostic.severity[key]
end

--- Remove duplicate diagnostics for a file.
function M.dedupe_file_diagnstics(file_diadnostics)
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
