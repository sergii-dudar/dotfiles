# Python test integration (pytest)

Adds Java-parity test run / debug / report capabilities for Python projects.

**Both pytest projects and unittest projects are supported** via a single
runner: pytest. pytest natively discovers and runs `unittest.TestCase`
classes, so we avoid a second parser, a second ID format, and the extra
`unittest-xml-reporting` dependency that pure stdlib unittest would require
to emit JUnit XML.

## Keymaps

- `<leader>tt` — run test under cursor (`pytest <file>::<Class>::<test>`)
- `<leader>td` — debug test under cursor (debugpy via nvim-dap-python)
- `<leader>tf` — run all tests in current file
- `<leader>ta` — run all tests in current file's directory
- `<leader>tm` / `<leader>tM` — run all project tests (aliased to ALL_TESTS)
- `<leader>tl` — re-run last
- `<leader>tD` — toggle last between debug / non-debug
- `<leader>to/tO/tL/tv/txx/txd` — show/hide/load output, tree view, Trouble, picker

After a run finishes, pass/fail signs appear at each `def test_*` line (and
class-method line) plus an aggregated summary sign at the top of the file.

## Required dependencies

| Tool                 | Purpose                                              | Install |
|----------------------|------------------------------------------------------|---------|
| `pytest`             | Test runner. Emits JUnit XML via `--junit-xml=`.     | `pip install pytest` (into the project venv) |
| `debugpy`            | DAP adapter for `<leader>td`. Only needed for debug. | `:MasonInstall debugpy` (or `pip install debugpy`) |
| `nvim-dap-python`    | Wraps debugpy + auto-detects venv.                   | Auto-installed by LazyVim's `lang.python` extra |
| Tree-sitter `python` | Discovers `def test_*` and `class Test*`.            | `:TSInstall python` (LazyVim default) |

Python interpreter resolution order:

1. `constants.python.python_bin` (manual override in `lua/utils/constants.lua`,
   `nil` by default) — if executable.
2. `$VIRTUAL_ENV/bin/python`.
3. `<project_root>/.venv/bin/python` or `<project_root>/venv/bin/python`.
4. `python3` / `python` on `$PATH`.

Whichever interpreter is chosen must have `pytest` importable (`<py> -c "import pytest"`).
If not, the runner aborts with a clear `pip install pytest` message instead of
silently breaking.

## Project layout assumptions

Project root is detected by walking up from the current file looking for any
of: `pyproject.toml`, `setup.py`, `setup.cfg`, `pytest.ini`, `tox.ini`,
`.git`, or a `tests/` / `test/` directory.

Tests follow standard pytest conventions:

- Files named `test_*.py` or `*_test.py` (configurable via pytest.ini).
- Test functions: `def test_*` at module level.
- Test methods: `class Test*` (or `*Test`) containing `def test_*` methods.
- unittest-style `class Foo(unittest.TestCase): def test_*` is fully supported.
- `@pytest.mark.parametrize` invocations are aggregated into a single test
  result whose `invocations` list contains one entry per parametrize case.

## How it works

1. **Run** — `lua/modules/python/pytest-test/init.lua` builds a `python -m pytest`
   command tailored to the selected `task.test_type` and always appends
   `--junit-xml=<cache>/test-report/python/<safe-project-path>/junit.xml`.
2. **Debug** — `dap_launch_test()` configures the LazyVim-registered `python`
   DAP adapter (debugpy) with `module = "pytest"`, the same args as a normal
   run, `cwd = project_root`, and `pythonPath` set to the resolved interpreter.
   A session-name-scoped `event_terminated` listener reloads the JUnit report
   after the session ends (does not interfere with non-test `<leader>rd`
   python debug sessions).
3. **Report** — `modules.python.test-report` registers a `LangAdapter` with
   the shared `modules.common.test-report` core. `junit-xml.lua` parses
   pytest's XML, derives the source file from `classname` (pytest doesn't
   reliably emit `file=` attrs by default), splits class vs module-level
   tests, and aggregates parametrize invocations.
4. **Signs / Trouble / tree** — handled by the shared core
   (`modules.common.test-report`). Diagnostic source: `pytest`. Trouble
   source: `pytest_test_diagnostics`.

## Test ID convention

```
<abs_file_path>#<test_function>             (module-level)
<abs_file_path>#<TestClass>.<test_method>   (class-based method)
```

The `#` separator is required by the shared test-report core (which splits
ids on the first `#`). Member shape (`Class.method` vs `test_fn`) matches
the keys returned by `find_test_positions`.

Parametrize invocations (`test_fizzbuzz[3-Fizz]`, `test_fizzbuzz[5-Buzz]`,
...) share the same id; each invocation lives under `result.invocations`.

## Files

```
modules/python/
├── pytest-test/init.lua          — command builder + DAP entry point
└── test-report/
    ├── init.lua                  — registers `python` adapter with shared core
    ├── junit-xml.lua             — pytest XML parser
    └── lang/python.lua           — LangAdapter (positions, ids, diagnostics)
```

Wire-up lives in:
- `lua/plugins/overseer/tasks/lang/python-runner.lua` — delegates test methods here.
- `lua/plugins/overseer/tasks/run_tests.lua` — maps `python` → `pytest_report`.
- `lua/plugins/overseer/test-report-dispatcher.lua` — registers dispatch maps.
- `lua/overseer/component/test_report/pytest_report.lua` — overseer component.
- `lua/plugins/editor/trouble-nvim.lua` — Trouble filter source.
- `lua/utils/constants.lua` — `M.python` block (interpreter path + source names).

## Known limitations

- **`unittest.subTest`**: each subTest passes/fails individually at the
  Python level, but pytest reports them as a single `<testcase>` whose
  failure body lists each failing subTest. Signs appear on the parent test
  function — no per-subTest granularity. Same limitation as neotest-python.
- **Pytest without `file=` attr**: by default pytest doesn't include the
  `file` attribute on `<testcase>` elements; the parser falls back to
  deriving the file path from the dotted `classname` (e.g.
  `tests.test_classes.TestBankAccount` → `tests/test_classes.py`). Works for
  standard layouts and `pythonpath = src` style projects (we probe `src/`,
  `lib/`, `app/` and finally `globpath` as a last resort).
- **Collection-phase errors** appear as `<error>` (not `<failure>`); they
  are mapped to `status = "failed"` and shown as diagnostics on the line
  reported in the traceback.
- **debugpy stop-on-entry**: the debugpy adapter does NOT stop on entry by
  default (unlike bashdb). Breakpoints set inside test bodies fire normally.
