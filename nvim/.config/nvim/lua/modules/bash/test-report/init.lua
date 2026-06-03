-- Thin shim: registers the Bash/bashunit language adapter with the generic
-- test-report core and re-exports the core's public API so callers
-- (overseer component, dispatcher) work transparently.

local registry = require("modules.common.test-report.registry")
local core = require("modules.common.test-report")

registry.register("sh", require("modules.bash.test-report.lang.bash"))
registry.register("bash", require("modules.bash.test-report.lang.bash"))

return core
