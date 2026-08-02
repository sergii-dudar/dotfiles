# file-template

Fills a **freshly created, still empty file** with a language template instead of
leaving a blank buffer behind, so creating `UserService.java` in a `*.service`
package immediately gives you a Spring `@Service` class.

Generic core (this directory) + per-language adapters
(`lua/modules/<lang>/file-template/`), mirroring the `common/test-report` pattern.

## How it triggers

The trigger is the **buffer**, not the file manager, so every creation path works
the same way — neo-tree, fyler.nvim, oil, snacks explorer, yazi, plain `:e`:

`FileType` (for filetypes that have an adapter) → deferred check → apply when all
of the following hold:

- the buffer is the focused one, normal (`buftype == ""`), modifiable, unmodified
- the buffer is empty
- the file is missing or zero bytes on disk
- the adapter's `enabled(ctx)` guard passes (Java: file lives in a real
  `src/<set>/java/` source root)
- a rule matches

Re-opening an already filled file never re-applies, because the file is no longer
empty. `vim.b.file_template_applied` records the trigger that was used.

## Templates are LuaSnip snippets

A rule points at a **LuaSnip trigger**, not at template text. Nothing is
duplicated: the same snippet expands manually while typing and automatically into
a new file, and insert/choice nodes stay live — you land inside the template with
`<Tab>` jumps ready and `<C-e>` cycling choices.

Java resolves triggers against `java-template` first, then `java`:

- `java-template` — whole-file templates that must **not** pollute normal
  completion (`class`, `abstract`, `interface`, `enum`, `record`, `exception`,
  `repository`, `test`), defined in
  `lua/plugins/luasnip/snippets/java/snippets-java-templates.lua`
- `java` — existing snippets reused as-is (`util`, `usecase`, `gateway`,
  `component`, `controller`, `properties`, `mapper`, `immutable`, `mutable`)

## Configuring rules

Everything is driven by an ordered rule list — for Java:
`lua/modules/java/file-template/rules.lua`. First match wins. Within a rule every
matcher field present must match (AND); globs inside one field are alternatives
(OR). A rule without matcher fields is a catch-all.

```lua
{ snippet = "util",      packages = { "*.util", "*.utils", "*.helper*" } },
{ snippet = "component", packages = { "*.service", "*.services" }, choice = 3 }, -- @Service
{ snippet = "test",      filename = { "*Test", "*Tests", "*IT" } },
{ snippet = "class" },                                                            -- catch-all
```

| field        | matched against                            | case         |
| ------------ | ------------------------------------------ | ------------ |
| `packages`   | dotted package (`com.acme.user.service`)   | insensitive  |
| `filename`   | class name without extension               | sensitive    |
| `path`       | absolute file path                         | sensitive    |
| `source_set` | maven/gradle source set (`main`, `test`, …)| insensitive  |
| `when`       | `fun(ctx): boolean`                        | —            |

Extras: `choice` preselects an index on the template's **first** choice node
(e.g. `component` → 1 `@Component`, 2 `@Configuration`, 3 `@Service`,
4 `@Repository`), and `desc` labels the entry in the picker.

Globs: `*` = any characters, `?` = one character, anchored on both ends.

## Manual use

- `:FileTemplate` — pick a template for the current buffer (with completion)
- `:FileTemplate <trigger>` — apply a specific one, e.g. `:FileTemplate interface`

## Disabling

```lua
vim.g.file_template_enabled = false   -- globally
vim.b.file_template_disabled = true   -- for one buffer
require("modules.common.file-template").setup({ notify = true }) -- report what was applied
```

## Adding a language

1. Create `lua/modules/<lang>/file-template/{init,rules}.lua`. `init.lua` sets
   `lang`, `filetypes`, optional `snippet_filetypes` / `context` / `enabled`, a
   `rules()` function, and calls `registry.register(M)`.
2. Add the `filetype -> module` entry to `adapter_modules` in `registry.lua`
   (or call `registry.register_module(ft, module)`).
3. Register whole-file snippets under a `<lang>-template` LuaSnip filetype so
   they stay out of normal completion.

## Files

| file           | purpose                                                        |
| -------------- | -------------------------------------------------------------- |
| `init.lua`     | autocmd + `:FileTemplate`, blank-buffer guards, apply/pick      |
| `registry.lua` | filetype → adapter, lazily requiring the language module        |
| `matcher.lua`  | glob → Lua pattern, rule matching                               |
| `expand.lua`   | LuaSnip lookup + expansion + choice preselect                   |
| `types.lua`    | shared LuaLS annotations                                        |
