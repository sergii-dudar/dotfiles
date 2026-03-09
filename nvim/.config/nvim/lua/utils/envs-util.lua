local M = {}

---@param env_file string - path to env file
---@return table<string, string>
function M.load_env_file(env_file)
    local content = require("lib.file").read_file(env_file)
    if not content then
        return {}
    end

    local envs = {}
    for line in content:gmatch("[^\r\n]+") do
        -- skip comments and empty lines
        if not line:match("^%s*#") and not line:match("^%s*$") then
            local key, value = line:match("^%s*([^=]+)%s*=%s*(.-)%s*$")
            if key then
                envs[key] = value
            end
        end
    end
    return envs
end

function M.load_env_file_variable(env_file, key)
    local envs_table = M.load_env_file(env_file)
    return envs_table[key]
end

--- function to load personal private values
function M.load_private_var(private_key)
    local env_file_path = os.getenv("HOME") .. "/private.env"
    return M.load_env_file_variable(env_file_path, private_key)
end

return M
