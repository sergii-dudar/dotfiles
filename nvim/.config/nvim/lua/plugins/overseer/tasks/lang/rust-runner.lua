local M = {}

function M.build_compile_cmd()
    local dir = vim.fn.expand("%:p:h")
    return {
        "sh",
        "-c",
        "cd " .. dir .. " && cargo build",
    }
end

---@return table
function M.build_run_cmd()
    local dir = vim.fn.expand("%:p:h")
    return {
        "sh",
        "-c",
        "cd " .. dir .. " && cargo -q run",
    }
end

-- ---@return table
-- function M.build_debug_cmd()
-- end

-- function M.dap_attach_to_remote(port)
-- end

---@return table|nil
local function get_rust_binary_info()
    local dir = vim.fn.expand("%:p:h")
    local cargo_metadata_cmd = string.format("cd %s && cargo metadata --format-version 1 --no-deps 2>/dev/null", dir)
    local handle = io.popen(cargo_metadata_cmd)
    if not handle then
        vim.notify("Failed to spawn cargo metadata", vim.log.levels.ERROR)
        return nil
    end
    local metadata_json = handle:read("*a")
    handle:close()

    local ok, metadata = pcall(vim.fn.json_decode, metadata_json)
    if not ok or type(metadata) ~= "table" or not metadata.workspace_root or not metadata.packages then
        vim.notify("Failed to parse Cargo metadata", vim.log.levels.ERROR)
        return nil
    end

    local workspace_root = metadata.workspace_root

    -- Find the first binary target
    local binary_name = nil
    for _, package in ipairs(metadata.packages) do
        for _, target in ipairs(package.targets or {}) do
            if vim.tbl_contains(target.kind or {}, "bin") then
                binary_name = target.name
                break
            end
        end
        if binary_name then
            break
        end
    end

    if not binary_name then
        vim.notify("No binary target found in Cargo project", vim.log.levels.ERROR)
        return nil
    end

    local program = workspace_root .. "/target/debug/" .. binary_name

    return {
        program = program,
        workspace_root = workspace_root,
    }
end

function M.dap_launch()
    vim.cmd.RustLsp("debug")
    --[[ require("plugins.overseer.overseer-task-util").run_compile(function()
        local binary_info = get_rust_binary_info()
        if not binary_info then
            return
        end

        require("dap").run({
            name = "LLDB: Launch",
            type = "codelldb",
            request = "launch",
            program = binary_info.program,
            cwd = binary_info.workspace_root,
            stopOnEntry = false,
            args = {},
            -- codelldb uses `terminal` (not `console`); values: console|integrated|external.
            terminal = "integrated",
            -- Enable Rust language features in LLDB (pretty-printing, traits, etc.).
            sourceLanguages = { "rust" },
            -- Tell codelldb to use its native expression evaluator (better Rust support
            -- than the default `simple` evaluator).
            expressions = "native",
            -- Source map: rustc embeds the rustc commit hash in source paths for stdlib;
            -- remap so stepping into std works if the user has rust-src installed.
            initCommands = {
                "settings set target.inline-breakpoint-strategy always",
            },
        })
    end) ]]
end

function M.dap_launch_rerun()
    M.dap_launch()
end

function M.dap_launch_test(context)
    return require("modules.rust.cargo-test").dap_launch_test(context)
end

---@param context task.lang.Context
---@return boolean ok, string|nil err
function M.prepare_test_context(context)
    return require("modules.rust.cargo-test").prepare_test_context(context)
end

---@param context task.lang.Context
---@return task.lang.test.TestCmd
function M.build_run_test_cmd(context)
    return require("modules.rust.cargo-test").build_run_test_cmd(context)
end

---@return string|nil
function M.get_test_report_dir()
    return require("modules.rust.cargo-test").get_test_report_dir()
end

-- Test types whose target is the cursor/file (guarded against a run/test key
-- mix-up). Project-wide types are not tied to the current buffer.
local CURSOR_BOUND_TEST_TYPES = {
    [task.test_type.CURRENT_TEST] = true,
    [task.test_type.CURRENT_PARAMETRIZED_NUM_TEST] = true,
}

-- Guard against the <leader>r… / <leader>t… mix-up. Rust has no `src/test`
-- convention and a binary's `main.rs` often holds both `fn main` and a
-- `#[cfg(test)]` module, so detection is cursor-scoped: it asks where the cursor
-- actually is rather than what the file contains.
---@type task.lang.RunGuard
M.run_guard = {
    -- Gate <leader>r… (cargo run / debug the binary).
    can_run = function()
        if require("utils.rust.rust-ts-util").is_test_context_at_cursor() then
            return false, "✋ Cursor is in a test — use <leader>t… (test runner), not <leader>r…"
        end
        return true
    end,
    -- Gate <leader>t… (run/debug the test at/around the cursor).
    ---@param context task.lang.Context
    can_test = function(context)
        local rust_ts = require("utils.rust.rust-ts-util")
        local t = context.test_type
        if CURSOR_BOUND_TEST_TYPES[t] then
            if rust_ts.is_test_context_at_cursor() then
                return true
            end
            if rust_ts.has_main_function() then
                return false, "✋ Cursor is not in a test (main/binary) — use <leader>r… (run/debug)"
            end
            return false, "✋ Cursor is not in a test — use <leader>r… (run/debug), not <leader>t…"
        end
        if t == task.test_type.FILE_TESTS then
            if not rust_ts.file_has_tests() then
                return false, "✋ This file has no tests — use <leader>r… (run/debug), not <leader>t…"
            end
        end
        return true
    end,
}

return M