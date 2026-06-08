-- Thin shim: registers the C# language adapter with the generic test-report core
-- and re-exports the core's public API so callers (overseer component, dispatcher)
-- work transparently.

local registry = require("modules.common.test-report.registry")
local core = require("modules.common.test-report")

registry.register("cs", require("modules.cs.test-report.lang.cs"))

return core
