-- Thin shim: registers the Rust language adapter with the generic
-- test-report core and re-exports the core's public API so callers
-- (overseer component, dispatcher) work transparently.

local registry = require("modules.common.test-report.registry")
local core = require("modules.common.test-report")

registry.register("rust", require("modules.rust.test-report.lang.rust"))

return core
