-- Thin shim: registers the Python language adapter with the generic
-- test-report core and re-exports the core's public API so callers
-- (overseer component, dispatcher) work transparently.

local registry = require("modules.common.test-report.registry")
local core = require("modules.common.test-report")

registry.register("python", require("modules.python.test-report.lang.python"))

return core
