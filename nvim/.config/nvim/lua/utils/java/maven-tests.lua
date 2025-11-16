local M = {}

M.test = function()
    -- run_maven({ "-q", "-DskipTests=false", "test" })
end

M.verify = function()
    -- run_maven({ "-q", "verify" })
end

return M