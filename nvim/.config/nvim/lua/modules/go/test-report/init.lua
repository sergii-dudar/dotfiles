-- Thin shim: registers the Go language adapter with the generic test-report
-- core and re-exports the core's public API so callers (overseer component,
-- dispatcher) work transparently.

local registry = require("modules.common.test-report.registry")
local core = require("modules.common.test-report")

registry.register("go", require("modules.go.test-report.lang.go"))

return core
