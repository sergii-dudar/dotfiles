---@meta

--[[

Utility functions for the scratchpad.

]]--

local pairs = pairs
local require = require
local type = type

local awful = require("awful")

local utils = {}

---Mutates a table with any value it doesnt have from a given source table.
---@param target table # Table to mutate.
---@param source table # Table to source values from.
---@return table # Target table.
utils.combine = function(target, source)
    for k, v in pairs(source) do
        if not target[k] then
            target[k] = v
        end
    end
    return target
end

---Mutates a client boolean options to a specified booleans.
---@param bool boolean # What to set to all client options.
---@param client client # Client to mutate.
---@param table table # Table of options to iterate through.
utils.set_boolean_client_options = function(bool, client, table)
    for option, _ in pairs(table) do
        if table[option] and type(table[option]) == "boolean" then
            client[option] = bool
        end
    end
end

---Mutates a client by enabling its client properties as defined by the given properties.
---@param client client # Client to mutate.
---@param screen screen # The screen the client is in.
---@param properties table # Properties to enable.
utils.enable_client_properties = function(client, screen, properties)
    utils.set_boolean_client_options(true, client, properties)
    if properties.geometry then
        client:geometry({
            x      = screen.geometry.x + properties.geometry.x,
            y      = screen.geometry.y + properties.geometry.y,
            width  = properties.geometry.width,
            height = properties.geometry.height,
        })
    end
end

---Mutates a client by disabling its client properties as defined by the given properties.
---@param client client # Client to mutate.
---@param screen screen # The screen the client is in.
---@param properties table # Properties to disable.
utils.disable_client_properties = function(client, screen, properties)
    utils.set_boolean_client_options(false, client, properties)
    if client.hidden then
        client.hidden = false
        client:move_to_tag(screen.selected_tag)
    end
end

---Enables a client on the given screen.
---@param client client # Client to enable.
---@param screen screen # Screen for the client to be enabled in.
utils.enable_client = function(client, screen)
    client.hidden = false
    client:move_to_tag(awful.tag.selected(screen))
    client:activate({})
end

---Disables a given client, removing it from all tags.
---@param client client: Client to disable.
utils.disable_client = function(client)
    client.hidden = true
    client:tags({})
end

return utils
