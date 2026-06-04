-- Thin shim: registers the Java language adapter with the generic
-- test-report core and re-exports the core's public API so existing
-- callers (overseer component, keymaps, etc.) keep working unchanged.

local registry = require("modules.common.test-report.registry")
local core = require("modules.common.test-report")

registry.register("java", require("modules.java.test-report.lang.java"))

return core
