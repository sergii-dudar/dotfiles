local M = {}

--------------------------------------------------------------------------------
-- lsp
M.code_action_auto_resolve_match_names = {
    "Rewrite as raw strin",
    "Rewrite as regular string",
}

--------------------------------------------------------------------------------
-- runners

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

return M
