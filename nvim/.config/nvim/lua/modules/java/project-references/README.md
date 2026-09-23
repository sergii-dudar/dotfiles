# `modules.java.project-references` — fast, project-scoped `gR`

`gr` asks jdtls for references. For an overriding method (say
`TransferDirectionResolver#accept` implementing `Consumer#accept`) jdtls
searches every project **and** dependency jar and, because JDT's method search
is polymorphic, also reports every `consumer.accept(...)` made through the
interface. Slow, and noisy.

`gR` answers a narrower question, in milliseconds:

> where in **this project** (cwd) is `<Type>#<method>` called on a receiver
> whose **declared type is exactly `<Type>`**?

## How it works

1. **Target** — `resolver.resolve_target` names the `<Type>#<method>` under the
   cursor. On a method *declaration* that is just the enclosing type
   (treesitter, no LSP, instant). On a *call site* it asks jdtls for the
   definition (sync, 2 s budget) and takes the declaring type; without a client
   it falls back to the receiver's declared type in the current buffer.
2. **Candidates** — one `rg -t java` over cwd for `.method(`, `::method` and
   bare `method(` (`Snacks.picker.grep`, gitignore honoured, `**/target/**` and
   `**/build/generated/**` skipped).
3. **Verification** — a picker `transform` parses each hit's file once with
   treesitter and resolves the receiver:

   | call shape                | resolved as                                        |
   | ------------------------- | -------------------------------------------------- |
   | `foo.m()` / `foo::m`      | declared type of `foo` (innermost enclosing scope) |
   | `this.foo.m()`            | declared type of field `foo`                       |
   | `m()` / `this.m()`        | enclosing type chain, its superclass, or a `import static Type.m` |
   | `super.m()`               | superclass of the enclosing type                   |
   | `Type.m()` / `Type::m`    | `Type` (UpperCamel identifier without a declaration) |
   | `new Type().m()`          | `Type`                                             |
   | `((Type) x).m()`          | `Type`                                             |
   | `verify(x).m()`, `when(x).m()`, `given(x)…`, `then(x).should()…`, `inOrder.verify(x)…` | declared type of the mock `x` (labelled `mock`) |
   | call chains, untyped lambda params, `var` without `new` | *unresolved* |

   Strings and comments never match: treesitter sees a `string_literal` /
   `line_comment` there, not an identifier.

   Types are compared by **simple name** only — no import resolution. That is
   the trade-off that keeps it fast, and inside one project tree it is accurate.

## Picker

- **strict** (default): only hits whose receiver resolves to the target type,
  plus the method's own declaration (labelled `decl`). The title shows how
  many hits are hidden.
- `<C-a>` — toggle **all calls**: every textual call of that method name, each
  labelled with the resolved receiver type, or `?` when it could not be resolved.
- Everything else is the standard Snacks grep picker (preview, `<C-q>` to
  quickfix, fuzzy filter on the file/line text).

## Wiring

- Keymap: `gR` in `lua/plugins/editor/lsp.lua` →
  `utils.lang.lsp-navigation.project_references()`.
- Java handler: `utils/lang/java/lsp-java.lua` (`navigation.project_references`)
  calls `M.find()`. When the cursor is not on a method name/call the handler
  declines and the generic fallback `Snacks.picker.grep_word()` runs instead
  (still fast, still cwd-only, just textual).
- `M.settings` — `default_mode`, `ft`, `exclude`, `lsp_timeout_ms`,
  `include_declaration`. `M.find()` returns `true, picker` so tests can drive it.
- Implementation note: Snacks steps finders from a libuv check callback (fast
  context), so each file's treesitter parse is hopped to the main loop. Snacks'
  own `Async:schedule` is not usable for that here — the grep process shares
  the coroutine and wakes it on every stdout chunk — hence the re-suspend loop
  in `on_main_loop`.

## Known limits

- Simple-name comparison: two types with the same simple name in different
  packages are indistinguishable.
- A call through a *subclass*-typed variable (`SubResolver r; r.accept()`)
  shows up under "all calls" labelled `SubResolver`, not in strict mode — the
  module does not walk hierarchies.
- Hits are taken from disk (ripgrep); an unsaved buffer can be one edit ahead.
