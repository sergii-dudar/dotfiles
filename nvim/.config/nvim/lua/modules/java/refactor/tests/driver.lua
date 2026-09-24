-- Headless driver for modules.java.refactor (see README.md in this directory).
--
--   nvim --headless -u NONE --cmd 'set rtp+=<nvim config dir>' -l driver.lua <src> <dst> [<src> <dst> ...]
--
-- Performs each move like a file manager would (rename, creating parent directories), registers it, then
-- processes all registered changes in test mode and prints "RESULT: true|false".
local args = _G.arg
local refactor = require("modules.java.refactor")
refactor.test_mode = true

for i = 1, #args, 2 do
    local src, dst = args[i], args[i + 1]
    local parent = dst:match("(.+)/[^/]+$")
    if parent then
        vim.fn.mkdir(parent, "p")
    end
    local ok, err = os.rename(src, dst)
    if not ok then
        print("MOVE FAILED: " .. src .. " -> " .. dst .. ": " .. tostring(err))
        os.exit(1)
    end
    refactor.register_change(src, dst)
end

local ok = refactor.process_registerd_changes()
print("RESULT: " .. tostring(ok))
