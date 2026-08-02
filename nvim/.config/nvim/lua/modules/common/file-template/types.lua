-- Shared type annotations for the generic file-template engine.
--
-- The engine fills a freshly created, still empty buffer with a language
-- template. Templates are plain LuaSnip snippets resolved by trigger, so the
-- expanded result keeps its insert/choice nodes and stays interactive.

---Resolved information about the buffer a template is about to be applied to.
---The core fills the generic fields; a language adapter may extend it with
---language specific data (Java adds `package`, `source_set`, `is_test`).
---@class file_template.Context
---@field bufnr integer Buffer the template will be expanded into.
---@field filetype string Buffer filetype, used to resolve the language adapter.
---@field path string Absolute file path of the buffer.
---@field dir string Directory holding the file.
---@field filename string File name including extension, e.g. `UserService.java`.
---@field basename string File name without extension, e.g. `UserService`.
---@field ext string File extension without the leading dot, e.g. `java`.
---@field package? string Language package/namespace of the file, when the adapter can resolve one.
---@field source_set? string Build source set the file belongs to, e.g. `main` or `test`.
---@field is_test? boolean Whether the file belongs to a test source set.

---A single template rule. All matcher fields present on the rule must match
---(logical AND); the globs listed inside one field are alternatives (OR).
---A rule without any matcher field is a catch-all.
---
---Globs are anchored on both ends; `*` matches any characters (dots included)
---and `?` exactly one. A `*` may be used on either side or both, e.g.
---`*.listeners` (a `listeners` segment at the end), `*.listeners.*`
---(sub-packages of it), `*.listeners*` (either) or `listeners` (exact).
---@class file_template.Rule
---@field snippet string LuaSnip trigger of the template to expand.
---@field packages? string[] Globs matched case-insensitively against `Context.package`.
---@field filename? string[] Globs matched against `Context.basename` (class name).
---@field path? string[] Globs matched against `Context.path`.
---@field source_set? string[] Globs matched case-insensitively against `Context.source_set`.
---@field when? fun(ctx: file_template.Context): boolean Extra arbitrary condition.
---@field choice? integer Choice index preselected on the template's first choice node.
---@field desc? string Label shown by the `:FileTemplate` picker.

---Per-language adapter. Registers itself with `modules.common.file-template.registry`
---when the module is first required, mirroring the test-report adapter pattern.
---@class file_template.Adapter
---@field lang string Stable language identifier, e.g. `java`.
---@field filetypes string[] Neovim filetypes handled by this adapter.
---@field snippet_filetypes? string[] LuaSnip filetypes searched (in order) when resolving a rule trigger. Defaults to the buffer filetype.
---@field rules fun(ctx: file_template.Context): file_template.Rule[] Ordered rule list, first match wins.
---@field context? fun(ctx: file_template.Context): table|nil Extra language specific context fields merged into the base context.
---@field enabled? fun(ctx: file_template.Context): boolean Guard deciding whether templates apply to this buffer at all.

return {}
