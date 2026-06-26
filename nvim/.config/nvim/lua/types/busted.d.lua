---@meta

---@alias BustedCallback fun()
---@alias BustedAsyncCallback fun(done: fun(...: any))
---@alias BustedTestCallback BustedCallback|BustedAsyncCallback

---@class BustedBlock
---@operator call(string, BustedTestCallback)
---@field skip fun(name: string, fn?: BustedTestCallback)
---@field only fun(name: string, fn: BustedTestCallback)

---@type BustedBlock
describe = describe

---@type BustedBlock
it = it

---@param name string
---@param fn? BustedTestCallback
function pending(name, fn) end

---@param fn BustedCallback
function setup(fn) end

---@param fn BustedCallback
function teardown(fn) end

---@param fn BustedCallback
function before_each(fn) end

---@param fn BustedCallback
function after_each(fn) end

---@param fn BustedAsyncCallback
---@return BustedAsyncCallback
function async(fn) end

---@class LuassertSpy
---@operator call(...: any): any
---@field revert fun(self: LuassertSpy)
---@field returns fun(...: any): LuassertSpy
---@field invokes fun(fn: fun(...: any): any): LuassertSpy

---@class LuassertSpyModule
---@field new fun(fn: fun(...: any): any): LuassertSpy
---@field on fun(target: table, key: string): LuassertSpy

---@type LuassertSpyModule
spy = spy

---@param target table
---@param key string
---@return LuassertSpy
function stub(target, key) end

---@class LuassertMockModule
---@operator call(target: table, stub_all?: boolean): table
---@field revert fun(target: table)

---@type LuassertMockModule
mock = mock

---@class BustedAssertSpy
---@field was_called fun(count?: integer)
---@field was_called_with fun(...: any)
---@field was_not_called_with fun(...: any)

---@class BustedAssert
---@field are BustedAssert
---@field is BustedAssert
---@field is_not BustedAssert
---@field has BustedAssert
---@field equal fun(expected: any, actual: any, message?: string)
---@field equals fun(expected: any, actual: any, message?: string)
---@field same fun(expected: any, actual: any, message?: string)
---@field truthy fun(value: any, message?: string)
---@field falsy fun(value: any, message?: string)
---@field true fun(value: any, message?: string)
---@field false fun(value: any, message?: string)
---@field nil fun(value: any, message?: string)
---@field is_true fun(value: any, message?: string)
---@field is_false fun(value: any, message?: string)
---@field is_nil fun(value: any, message?: string)
---@field is_truthy fun(value: any, message?: string)
---@field is_falsy fun(value: any, message?: string)
---@field matches fun(pattern: string, actual: string, message?: string)
---@field has_error fun(fn: fun(), expected?: string|fun(message: string): boolean, message?: string)
---@field spy fun(value: LuassertSpy): BustedAssertSpy
---@field stub fun(value: LuassertSpy): BustedAssertSpy

---@type BustedAssert
assert = assert
