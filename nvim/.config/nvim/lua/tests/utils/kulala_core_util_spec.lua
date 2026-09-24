local helper = require("tests.utils.spec_helper")

describe("utils.kulala-core-util", function()
    local core_util
    local state
    local BIN = "/data/kulala.nvim/bin/kulala-core"
    local VERIFY_CMD = { "codesign", "--verify", "--strict", BIN }
    local SIGN_CMD = { "codesign", "--force", "--sign", "-", BIN }

    --- Script vim.system so consecutive codesign invocations exit with the given codes.
    ---@param codes integer[]
    ---@param stderr? string
    local function script_codesign(codes, stderr)
        local call = 0
        vim.system = function(command, opts, on_exit)
            table.insert(state.system_calls, { command = command, opts = opts })
            call = call + 1
            on_exit({ code = codes[call] or 0, stdout = "", stderr = stderr or "" })
        end
    end

    --- Build a libuv-like stat result for the fake binary.
    ---@param size integer
    ---@param mtime integer
    local function stat_for(size, mtime)
        return { size = size, mtime = { sec = mtime } }
    end

    --- Run ensure_signed and capture the reported outcome.
    ---@param opts? table
    ---@return string status
    ---@return string|nil err
    local function ensure_signed(opts)
        local status, err
        opts = opts or {}
        opts.path = opts.path or BIN
        opts.on_done = function(s, e)
            status, err = s, e
        end
        core_util.ensure_signed(opts)
        return status, err
    end

    before_each(function()
        _, state = helper.reset_vim()
        vim.fn.executable = function()
            return 1
        end
        vim.uv.fs_stat = function()
            return stat_for(100, 1)
        end
        core_util = helper.reload("utils.kulala-core-util")
    end)

    after_each(function()
        helper.clear_stub_modules({ "utils.kulala-core-util", "kulala.backend" })
    end)

    it("skips verification outside macOS", function()
        -- given
        vim.uv.os_uname = function()
            return { sysname = "Linux" }
        end

        -- when
        local status, err = ensure_signed()

        -- then
        assert.are.equal("skipped", status)
        assert.are.equal("not macOS", err)
        assert.are.equal(0, #state.system_calls)
    end)

    it("skips when kulala-core is not installed", function()
        -- given
        vim.uv.fs_stat = function()
            return nil
        end

        -- when
        local status, err = ensure_signed()

        -- then
        assert.are.equal("skipped", status)
        assert.are.equal("kulala-core not installed: " .. BIN, err)
        assert.are.equal(0, #state.system_calls)
    end)

    it("keeps a valid signature untouched", function()
        -- given
        script_codesign({ 0 })

        -- when
        local status, err = ensure_signed()

        -- then
        assert.are.equal("valid", status)
        assert.is_nil(err)
        assert.are.equal(1, #state.system_calls)
        assert.are.same(VERIFY_CMD, state.system_calls[1].command)
        assert.are.equal(0, #state.notifications)
    end)

    it("re-signs kulala-core when macOS rejects its signature", function()
        -- given
        script_codesign({ 1, 0 })

        -- when
        local status, err = ensure_signed()

        -- then
        assert.are.equal("resigned", status)
        assert.is_nil(err)
        assert.are.equal(2, #state.system_calls)
        assert.are.same(VERIFY_CMD, state.system_calls[1].command)
        assert.are.same(SIGN_CMD, state.system_calls[2].command)
        assert.are.equal(1, #state.notifications)
        assert.are.equal(vim.log.levels.INFO, state.notifications[1].level)
    end)

    it("reports a failed re-sign", function()
        -- given
        script_codesign({ 1, 1 }, "  boom\n")

        -- when
        local status, err = ensure_signed()

        -- then
        assert.are.equal("failed", status)
        assert.are.equal("boom", err)
        assert.are.equal(1, #state.notifications)
        assert.are.equal(vim.log.levels.ERROR, state.notifications[1].level)
        assert.is_truthy(state.notifications[1].message:find(BIN, 1, true))
    end)

    it("verifies each binary once per session", function()
        -- given
        script_codesign({ 0 })
        ensure_signed()

        -- when
        local status = ensure_signed()

        -- then
        assert.are.equal("valid", status)
        assert.are.equal(1, #state.system_calls)
    end)

    it("re-verifies after the binary changes on disk", function()
        -- given
        script_codesign({ 0, 0 })
        ensure_signed()
        vim.uv.fs_stat = function()
            return stat_for(200, 2)
        end

        -- when
        local status = ensure_signed()

        -- then
        assert.are.equal("valid", status)
        assert.are.equal(2, #state.system_calls)
    end)

    it("resolves the binary path from the kulala backend", function()
        -- given
        helper.stub_module("kulala.backend", {
            get_bin_path = function()
                return BIN
            end,
        })
        script_codesign({ 0 })

        -- when
        local resolved = core_util.core_path()
        core_util.ensure_signed()

        -- then
        assert.are.equal(BIN, resolved)
        assert.are.same(VERIFY_CMD, state.system_calls[1].command)
    end)

    it("returns nil when the kulala backend is unavailable", function()
        -- given
        helper.clear_stub_modules("kulala.backend")

        -- when
        local resolved = core_util.core_path()
        local status, err
        core_util.ensure_signed({
            on_done = function(s, e)
                status, err = s, e
            end,
        })

        -- then
        assert.is_nil(resolved)
        assert.are.equal("skipped", status)
        assert.are.equal("kulala-core path not resolved", err)
    end)
end)
