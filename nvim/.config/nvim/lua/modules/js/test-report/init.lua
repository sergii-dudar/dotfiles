-- Thin shim: registers the JavaScript/TypeScript (jest) language adapter with
-- the generic test-report core for every JS/TS filetype and re-exports the
-- core's public API so callers (overseer component, dispatcher) work
-- transparently.

local registry = require("modules.common.test-report.registry")
local core = require("modules.common.test-report")

local adapter = require("modules.js.test-report.lang.js")
for _, ft in ipairs({ "javascript", "typescript", "javascriptreact", "typescriptreact" }) do
    registry.register(ft, adapter)
end

return core
