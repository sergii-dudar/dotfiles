-- Shared constants: diagnostic source names, junit paths, UI dimensions.

local M = {}

M.java = {
    maven_diagnostics_test_source = "maven-test",
    maven_diagnostics_compile_source = "maven-compile",
    junit = "junit",
    junit_report_dir = "/target/junit-report",
}
M.rust = {
    cargo_test = "cargo-test",
    cargo_test_diagnostics_source = "cargo_test_diagnostics",
    nextest_report_dir = "/target/nextest/default",
}
M.lua = {
    busted = "busted",
    busted_test_diagnostics_source = "busted_test_diagnostics",
}
M.go = {
    go_test = "go-test",
    go_test_diagnostics_source = "go_test_diagnostics",
}
M.bash = {
    bashunit = "bashunit",
    bashunit_test_diagnostics_source = "bashunit_test_diagnostics",
    -- Absolute path to the bashunit binary used for `<leader>t*` runs/debug.
    -- Set to your system install (e.g. via `~/.local/bin/bashunit`). Leave nil
    -- to fall back to auto-detection (project lib/bashunit -> $PATH).
    bashunit_bin = "/home/serhii/.local/bin/bashunit",
}
M.python = {
    pytest = "pytest",
    pytest_test_diagnostics_source = "pytest_test_diagnostics",
    -- Absolute path to a Python interpreter for `<leader>t*` test runs/debug.
    -- Leave nil to fall back to auto-detection: $VIRTUAL_ENV -> ./.venv ->
    -- ./venv -> python3/python on $PATH.
    python_bin = nil,
}
M.output = {
    height_rows = 10,
}

return M
