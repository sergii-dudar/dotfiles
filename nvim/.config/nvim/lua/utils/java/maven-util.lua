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

return M
