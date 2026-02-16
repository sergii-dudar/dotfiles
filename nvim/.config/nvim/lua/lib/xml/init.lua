local xml2lua = require("lib.xml.internal")
local xml_tree = require("lib.xml.tree")

local M = {}

---@param xml_data string
---@return table
function M.parse(xml_data)
    local handler = xml_tree()
    local parser = xml2lua.parser(handler)
    parser:parse(xml_data)
    return handler.root
end

return M
