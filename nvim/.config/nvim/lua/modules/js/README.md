# JavaScript / TypeScript test integration (jest)

Adds Java-parity test run / debug / report capabilities for jest projects.

**Both JavaScript and TypeScript projects are supported** via a single runner:
jest. TypeScript is transpiled by the project's own jest config (`ts-jest` /
babel), so there is one runner, one parser, and one ID format for every JS/TS
filetype (`javascript`, `typescript`, `javascriptreact`, `typescriptreact`).

The report is jest's **built-in JSON output** (`--json --testLocationInResults
--outputFile=`) — no extra reporter package (`jest-junit`) is required. Each
test carries an authoritative `location.line`, which we use to align report
results with treesitter-discovered source positions.

## Keymaps

- `<leader>tt` — run test under cursor (`jest <file> -t <name>`)
- `<leader>td` — debug test under cursor (node + jest via pwa-node / js-debug)
- `<leader>tf` / `<leader>tF` — run / debug all tests in current file
- `<leader>ta` — run all project tests
- `<leader>tm` / `<leader>tM` — run all project tests (aliased to ALL_TESTS)
- `<leader>tp` / `<leader>tP` — run / debug the `test.each` block under cursor
  (jest expands parameters at runtime, so this runs the whole `.each` block,
  not a single iteration)
- `<leader>tl` — re-run last
- `<leader>tD` — toggle last between debug / non-debug
- `<leader>to/tO/tL/tv/txx/txd` — show/hide/load output, tree view, Trouble, picker

After a run finishes, pass/fail signs appear at each `test`/`it` line plus an
aggregated summary sign at the top of the file.

## Plain run / debug (non-test)

`<leader>rr` / `<leader>rd` run and debug the current file with **Deno**
(`deno run`). Deno executes both `.js` and `.ts` natively, so one config covers
both. Debug uses the `pwa-node` (js-debug) adapter with
`runtimeExecutable = "deno"` + `--inspect-wait` and `attachSimplePort = 9229`.
This is separate from the jest test pipeline.

## Required dependencies

| Tool                       | Purpose                                                   | Install |
|----------------------------|-----------------------------------------------------------|---------|
| `jest`                     | Test runner. Emits JSON via `--json --outputFile=`.       | `npm i -D jest` (project devDependency) |
| `node`                     | Runs jest; runs `jest.js` for `<leader>td` test debug.    | nvm / system node |
| `ts-jest` (TS only)        | TypeScript transpile for jest.                            | `npm i -D ts-jest typescript` |
| `js-debug-adapter`         | DAP adapter (`pwa-node`) for test + plain debug.          | `:MasonInstall js-debug-adapter` |
| `deno`                     | Plain `<leader>rr` / `<leader>rd` runtime.                | system deno |
| Tree-sitter `javascript`/`typescript`/`tsx` | Discovers `test`/`it`/`describe` calls. | `:TSInstall javascript typescript tsx` |

jest binary resolution order:

1. `<ancestor>/node_modules/.bin/jest` (walked up from the project root).
2. `npx jest`.
3. `jest` on `$PATH`.

## Project layout assumptions

Project root is detected by walking up from the current file looking for any
of: `jest.config.{js,ts,cjs,mjs,json}`, `package.json`, or `.git`.

Tests follow standard jest conventions:

- Files named `*.test.{js,ts,jsx,tsx}` / `*.spec.*` (per the project's jest config).
- `test(...)` / `it(...)` blocks, optionally nested inside `describe(...)`.
- `test.each` / `it.each` / `describe.each` parametrized tests (array, named,
  and tagged-template forms) are aggregated into a single test result whose
  `invocations` list contains one entry per expansion.

## How it works

1. **Run** — `lua/modules/js/jest-test/init.lua` builds a `jest` command
   tailored to the selected `task.test_type` and always appends
   `--json --testLocationInResults --outputFile=<cache>/test-report/js/<safe-project-path>/results.json`.
   The task runs with `cwd = project_root` so jest finds its config.
2. **Debug** — `dap_launch_test()` launches `node <root>/node_modules/jest/bin/jest.js`
   via the `pwa-node` adapter with `--runInBand`, the same target/pattern as a
   normal run, source maps enabled (so `.ts` breakpoints resolve through
   ts-jest). A session-name-scoped (`"Jest: Debug test"`) `event_terminated`
   listener reloads the report afterwards — it does **not** interfere with the
   plain `<leader>rd` Deno debug session.
3. **Report** — `modules.js.test-report` registers a `LangAdapter` with the
   shared `modules.common.test-report` core. `jest-json.lua` parses jest's JSON,
   groups a file's assertions by `location.line` (so each `test.each` block
   collapses to one test with N invocations), and cleans ANSI from failure
   messages.
4. **Signs / Trouble / tree** — handled by the shared core
   (`modules.common.test-report`). Diagnostic source: `jest`. Trouble source:
   `jest_test_diagnostics`.

## Test ID convention

```
<abs_file_path>#L<source_row>
```

Members are keyed by **source line** (`L<0-indexed-row>`) rather than test name.
jest's `location.line` and the treesitter start-line of each `test`/`it` call
always agree on this row — including parametrized `test.each`, where every
expansion shares one source line. This is what makes parametrize aggregation
and single-test reruns robust without parsing runtime-expanded names. The `#`
separator is required by the shared test-report core. Human-readable names
(`describe › nested › title`) are resolved for display via `id_to_display`.

## Single-test (filtered) runs

`<leader>tt` runs `jest -t <pattern>`. jest still includes the file's
non-matching tests in the JSON, marked `pending` (indistinguishable from a
deliberate `test.skip`). To avoid clobbering sibling results, the runner writes
a `.run-meta` marker for filtered runs and the parser drops `pending` results
only then. Full / file / directory runs keep `pending` so deliberately
skipped tests still show a skipped marker.

## Files

```
modules/js/
├── jest-test/init.lua            — command builder + DAP entry point + cursor→pattern
└── test-report/
    ├── init.lua                  — registers the adapter for all JS/TS filetypes
    ├── jest-json.lua             — jest JSON parser (group-by-line, ANSI clean)
    └── lang/js.lua               — LangAdapter (treesitter positions, ids, diagnostics)
```

Wire-up lives in:
- `lua/plugins/overseer/tasks/lang/js-runner.lua` — delegates test methods here;
  also holds the Deno plain run/debug config.
- `lua/plugins/overseer/tasks/lang-runner-resolver.lua` — registers the runner
  for `javascript` / `typescript` / `javascriptreact` / `typescriptreact`.
- `lua/plugins/overseer/tasks/run_tests.lua` — maps the filetypes → `jest_report`
  (and threads the optional `cwd` from the TestCmd to the overseer task).
- `lua/plugins/overseer/test-report-dispatcher.lua` — registers dispatch maps.
- `lua/overseer/component/test_report/jest_report.lua` — overseer component.
- `lua/plugins/editor/trouble-nvim.lua` — Trouble filter source.
- `lua/utils/constants.lua` — `M.js` block (diagnostic / source names).

## Known limitations

- **`<leader>tp` single iteration**: jest expands `test.each` parameters at
  runtime, so an individual iteration cannot be targeted by name ahead of time;
  `<leader>tp` runs the whole `.each` block.
- **Two tests on one source line**: members are keyed by line, so two distinct
  `test(...)` calls written on the same physical line would merge into one
  result. Avoid one-liners with multiple tests (extremely rare in practice).
- **Deliberate `test.skip` during a filtered run**: dropped along with the
  pattern-excluded `pending` tests (see "Single-test runs"). Full/file/dir runs
  show skips normally.
