---@meta

--[[

Scratchpad Group module for AwesomeWM.

]]--

local math = math
local pairs = pairs
local rawset = rawset
local require = require
local setmetatable = setmetatable
local string = string
local table = table

local gears = require("gears")

---@class group: gears.object
---@field id string # Identifier.
---@field scratchpads table<string|number, scratchpad> # Table of scratchpads.
local group = {}

---Constructor of the Scratchpad Group object.
---@param args table # Arguments.
---@return group # Scratchpad group object.
function group:new(args)
    local object = setmetatable({}, self)
    self.__index = self
    args.validate = args.validate or true
    object.id = args.id or string.sub(math.random(), 3)
    object.scratchpads = args.scratchpads or {}
    return gears.object({ class = object })
end

---Adds a new scratchpad to the group
---@param new_scratchpad scratchpad # Scratchpad to add.
function group:add_scratchpad(new_scratchpad)
    for _, scratchpad in pairs(self.scratchpads) do
        if scratchpad.id == new_scratchpad.id then
            error("There is already a scratchpad with that ID.")
            return
        end
    end
    table.insert(self.scratchpads, new_scratchpad)
end

---Remove a scratchpad from the group.
---@param scratchpad_id string # The scratchpad to remove.
---@return scratchpad # The scratchpad that was removed.
function group:remove_scratchpad(scratchpad_id)
    local ret
    for key, scratchpad in pairs(self.scratchpads) do
        if scratchpad_id == scratchpad.id then
            ret = scratchpad
            rawset(self.scratchpads, key, nil)
        end
    end
    return ret
end

---Gets a scratchpad from the group.
---@param scratchpad_id string? # Scratchpad identifier.
---@param key (string|number)? # Key/Index in the scratchpad table.
---@return scratchpad: Scratchpad object.
function group:get_scratchpad(scratchpad_id, key)
    if key and self.scratchpads[key] then
        return self.scratchpads[key]
    end
    local ret
    for _, scratchpad in pairs(self.scratchpads) do
        if scratchpad_id == scratchpad.id then
            ret = scratchpad
            break
        end
    end
    return ret
end

---Runs a callback function for each scratchpad in the group.
---@param callback fun(scratchpad: scratchpad): nil # Function to run.
function group:do_for_each_scratchpad(callback)
    for _, scratchpad in pairs(self.scratchpads) do
        callback(scratchpad)
    end
end

---Runs a callback function for a scratchpad given its id or key.
---@param scratchpad_id string? # Scratchpad identifier.
---@param key (string|number)? # Key/Index in the scratchpad table.
---@param callback fun(scratchpad: scratchpad): nil # Function to run.
function group:do_for_scratchpad(scratchpad_id, key, callback)
    if key and self.scratchpads[key] then
        callback(self.scratchpads[key])
    end
    if not scratchpad_id then
        return
    end
    for _, scratchpad in pairs(self.scratchpads) do
        if scratchpad_id == scratchpad.id then
            callback(scratchpad)
            break
        end
    end
end

return group
