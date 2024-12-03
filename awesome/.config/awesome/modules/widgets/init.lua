local menubar = require("menubar")
local vars = require("modules.variables")

M = {}

M.setup = function()
    -- Menubar configuration
    menubar.utils.terminal = vars.app.terminal

    print("called...")
    -- TODO:
end

return M
