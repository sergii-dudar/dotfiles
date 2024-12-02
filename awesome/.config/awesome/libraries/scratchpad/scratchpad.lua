---@meta

--[[

Scratchpad module for AwesomeWM.

]]--

local math = math
local pairs = pairs
local require = require
local setmetatable = setmetatable
local string = string
local tostring = tostring

local awful = require("awful")
local gears = require("gears")
local utils = require(tostring(...):match("(.+)%..+$") .. ".utils")

local capi = { client = client }

local default_client_options = {
    floating = true,
    ontop = false,
    above = false,
    skip_taskbar = false,
    sticky = false,
    geometry = { width = 1200, height = 900, x = 360, y = 90 },
}
local default_scratchpad_options = {
    reapply_options = false,
    only_one = false,
}

---@class scratchpad: gears.object
---@field id string # Identifier.
---@field command string? # Shell command used to spawn a client.
---@field group (group|table)? # A common group of scratchpads.
---@field client client? # Current scratchpad client.
---@field screen screen? # The screen that the scratchpad displays to.
---@field client_options table # Proporties applied to the client as scratchpad.
---@field scratchpad_options table # Additional features added to the scratchpad.
local scratchpad = {}

---Constructor for the scratchpad class.
---@param args? table # Arguments.
---@return scratchpad # Scratchpad object inheriting from gears.object.
function scratchpad:new(args)
    args = args or {}
    local object = setmetatable({}, self)
    self.__index = self
    object.id = args.id or string.sub(math.random(), 3)
    object.command = args.command
    object.group = args.group
    object.client = args.client
    object.screen = args.screen or awful.screen.focused()
    object.client_options = args.client_options
    object.scratchpad_options = args.scratchpad_options
    utils.combine(object.client_options, default_client_options)
    utils.combine(object.scratchpad_options, default_scratchpad_options)
    return gears.object({ class = object })
end

local signal_callbacks = {
    apply_client = nil,
    remove_client = nil,
    reapply_options = function(scratchpad_object)
        utils.enable_client_properties(
            scratchpad_object.client,
            scratchpad_object.screen,
            scratchpad_object.client_options
        )
    end,
    only_one = function(current_scratchpad)
        if not current_scratchpad.group then
            return
        end
        for _, scratchpad_object in pairs(current_scratchpad.group.scratchpads) do
            if
                scratchpad_object.client
                and scratchpad_object.client ~= current_scratchpad.client
                and scratchpad_object.client.hidden == false
            then
                scratchpad_object:turn_off()
            end
        end
    end,
}

function scratchpad:attatch_unmanage_signal()
    signal_callbacks.remove_client = function(client)
        if self.client == client then
            self.client = nil
            if self.scratchpad_options.reapply_options then
                self:disconnect_signal(
                    "scratchpad::reapply_options",
                    signal_callbacks.reapply_options
                )
            end
            if self.scratchpad_options.only_one then
                self:disconnect_signal(
                    "scratchpad::only_one",
                    signal_callbacks.only_one
                )
            end
            capi.client.disconnect_signal(
                "request::unmanage",
                signal_callbacks.remove_client
            )
        end
    end
    capi.client.connect_signal(
        "request::unmanage",
        signal_callbacks.remove_client
    )
end

function scratchpad:attatch_manage_signal()
    signal_callbacks.apply_client = function(client)
        self.client = client
        utils.enable_client_properties(
            self.client,
            self.screen,
            self.client_options
        )
        if self.scratchpad_options.reapply_options then
            self:connect_signal(
                "scratchpad::reapply_options",
                signal_callbacks.reapply_options
            )
        end
        if self.scratchpad_options.only_one then
            self:connect_signal(
                "scratchpad::only_one",
                signal_callbacks.only_one
            )
        end
        self:emit_signal("scratchpad::only_one")
        capi.client.disconnect_signal(
            "request::manage",
            signal_callbacks.apply_client
        )
    end
    capi.client.connect_signal("request::manage", signal_callbacks.apply_client)
end

---Enable current scratchpad client visibility.
function scratchpad:turn_on()
    if not self.client then
        self:attatch_manage_signal()
        self:attatch_unmanage_signal()
        if self.command then
            awful.spawn(self.command, false)
            self:emit_signal("scratchpad::on")
        end
        return
    end
    self:emit_signal("scratchpad::only_one")
    self:emit_signal("scratchpad::reapply_options")
    utils.enable_client(self.client, self.screen)
    self:emit_signal("scratchpad::on")
end

---Disable current scratchpad client visibility.
function scratchpad:turn_off()
    if not self.client then
        self:attatch_manage_signal()
        self:attatch_unmanage_signal()
        return
    end
    utils.disable_client(self.client)
    self:emit_signal("scratchpad::off")
end

---Toggle current scratchpad client visibility. If there isnt one, spawn a new one.
function scratchpad:toggle()
    if not self.client then
        self:attatch_manage_signal()
        self:attatch_unmanage_signal()
        if self.command then
            awful.spawn(self.command, false)
            self:emit_signal("scratchpad::on")
        end
        return
    end
    if self.client.hidden then
        utils.enable_client(self.client, self.screen)
        self:emit_signal("scratchpad::on")
        self:emit_signal("scratchpad::only_one")
        self:emit_signal("scratchpad::reapply_options")
    else
        utils.disable_client(self.client)
        self:emit_signal("scratchpad::off")
    end
end

---Set a new clinet into the scratchpad at runtime.
---If it's already within the scratchpad, eject the client into the current tag.
---Otherwise set the passed in client to the client within the scratchpad.
---@param new_client client # Client to get set to the current scratchpad.
function scratchpad:set(new_client)
    local remove_client = function()
        utils.disable_client_properties(
            self.client,
            self.screen,
            self.client_options
        )
        self.client = nil
    end
    if self.client and self.client == new_client then
        remove_client()
        return
    end
    if self.client then
        remove_client()
    else
        self:attatch_manage_signal()
        self:attatch_unmanage_signal()
    end
    capi.client.emit_signal("request::manage", new_client)
    utils.enable_client_properties(
        self.client,
        self.screen,
        self.client_options
    )
    self.client:raise()
end

return scratchpad
