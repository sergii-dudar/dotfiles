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

describe("utils.kulala-core-util CA trust", function()
    local core_util
    local state
    local dir = helper.tmp_dir .. "nvim-kulala-core-util-spec"
    local BUNDLE = dir .. "/certs/curl-ca-bundle.pem"
    local ROOTS = dir .. "/cert.pem"
    local KEYCHAIN = dir .. "/System.keychain"
    local OPTS = { bundle = BUNDLE, roots = ROOTS, keychain = KEYCHAIN }
    local SYSTEM_CA = "/etc/pki/tls/certs/ca-bundle.crt"
    local LINUX_OPTS = { bundle = BUNDLE, system_ca_files = { "/etc/ssl/certs/ca-certificates.crt", SYSTEM_CA } }
    local ROOTS_PEM = "-----BEGIN CERTIFICATE-----\nroot\n-----END CERTIFICATE-----\n"
    local KEYCHAIN_PEM = "-----BEGIN CERTIFICATE-----\nbank\n-----END CERTIFICATE-----\n"
    local EXPORT_CMD = { "security", "find-certificate", "-a", "-p", KEYCHAIN }
    local RESTART_CMD = { "pkill", "-f", "/kulala-core --daemon$" }
    local HEADER = "Errors in request https://bank.test/keys at line: 3"
    local TRACE = "* Host bank.test:443 was resolved.\n* TLS alert, unknown CA (560):\n"
    local TLS_REASON = "curl: (60) SSL certificate OpenSSL verify result: self-signed certificate (19)"
    local HINT = "kulala's curl does not trust this server's CA: run :KulalaTrustRefresh"
    ---@type table<string, integer> modification time per existing path
    local mtimes

    --- Read a whole file, nil when it does not exist.
    ---@param path string
    ---@return string|nil
    local function read_file(path)
        local file = io.open(path, "r")
        if not file then
            return nil
        end
        local content = file:read("*a")
        file:close()
        return content
    end

    --- Create a file with the given content.
    ---@param path string
    ---@param content string
    local function write_file(path, content)
        local file = assert(io.open(path, "w"))
        file:write(content)
        file:close()
    end

    --- Script vim.system():wait() results by executable name.
    ---@param results table<string, table>
    local function script_system(results)
        vim.system = function(command, opts)
            table.insert(state.system_calls, { command = command, opts = opts })
            return {
                wait = function()
                    return results[command[1]] or { code = 0, stdout = "", stderr = "" }
                end,
            }
        end
    end

    --- Pretend to run on a Fedora-like Linux: no keychain, the CA file is not where the bundled curl looks.
    local function use_linux()
        vim.uv.os_uname = function()
            return { sysname = "Linux" }
        end
        mtimes = { [SYSTEM_CA] = 300 }
    end

    --- Install a fake kulala.logger that records what would be shown.
    ---@return table logger
    ---@return table[] shown recorded { message, lines_no, opts } calls
    local function stub_logger()
        local shown = {}
        local logger = {
            error = function(message, lines_no, opts)
                table.insert(shown, { message = message, lines_no = lines_no, opts = opts })
            end,
        }
        helper.stub_module("kulala.logger", logger)
        return logger, shown
    end

    before_each(function()
        _, state = helper.reset_vim()
        os.execute("rm -rf " .. dir .. " && mkdir -p " .. dir .. "/certs")
        write_file(ROOTS, ROOTS_PEM)
        mtimes = { [ROOTS] = 100, [KEYCHAIN] = 200 }
        vim.fn.executable = function()
            return 1
        end
        vim.uv.fs_stat = function(path)
            return mtimes[path] and { size = 1, mtime = { sec = mtimes[path] } } or nil
        end
        vim.uv.os_getpid = function()
            return 42
        end
        script_system({ security = { code = 0, stdout = KEYCHAIN_PEM, stderr = "" } })
        core_util = helper.reload("utils.kulala-core-util")
    end)

    after_each(function()
        helper.clear_stub_modules({ "utils.kulala-core-util", "kulala.logger" })
        os.execute("rm -rf " .. dir)
    end)

    it("creates a missing bundle from the public roots and the keychain", function()
        -- when
        local bundle, status, err = core_util.ensure_ca_bundle(OPTS)

        -- then
        assert.are.equal(BUNDLE, bundle)
        assert.are.equal("rebuilt", status)
        assert.is_nil(err)
        assert.are.equal(ROOTS_PEM .. "\n" .. KEYCHAIN_PEM, read_file(BUNDLE))
        assert.are.same(EXPORT_CMD, state.system_calls[1].command)
        assert.are.equal(dir .. "/certs", state.mkdirs[1].path)
        assert.is_nil(read_file(BUNDLE .. ".42.tmp"))
    end)

    it("rebuilds a bundle that is older than the keychain", function()
        -- given
        write_file(BUNDLE, "stale")
        mtimes[BUNDLE] = 150

        -- when
        local bundle, status = core_util.ensure_ca_bundle(OPTS)

        -- then
        assert.are.equal(BUNDLE, bundle)
        assert.are.equal("rebuilt", status)
        assert.are.equal(ROOTS_PEM .. "\n" .. KEYCHAIN_PEM, read_file(BUNDLE))
    end)

    it("keeps a bundle that is newer than its sources", function()
        -- given
        write_file(BUNDLE, "current")
        mtimes[BUNDLE] = 200

        -- when
        local bundle, status = core_util.ensure_ca_bundle(OPTS)

        -- then
        assert.are.equal(BUNDLE, bundle)
        assert.are.equal("fresh", status)
        assert.are.equal("current", read_file(BUNDLE))
        assert.are.equal(0, #state.system_calls)
    end)

    it("keeps the previous bundle when the keychain export fails", function()
        -- given
        write_file(BUNDLE, "previous")
        mtimes[BUNDLE] = 150
        script_system({ security = { code = 1, stdout = "", stderr = "  denied\n" } })

        -- when
        local bundle, status, err = core_util.ensure_ca_bundle(OPTS)

        -- then
        assert.are.equal(BUNDLE, bundle)
        assert.are.equal("failed", status)
        assert.are.equal("keychain export failed: denied", err)
        assert.are.equal("previous", read_file(BUNDLE))
        assert.are.equal(1, #state.notifications)
        assert.are.equal(vim.log.levels.WARN, state.notifications[1].level)
    end)

    it("uses the system CA file on Linux without generating anything", function()
        -- given
        use_linux()

        -- when
        local ca_file, status, err = core_util.ensure_ca_bundle(LINUX_OPTS)

        -- then
        assert.are.equal(SYSTEM_CA, ca_file)
        assert.are.equal("system", status)
        assert.is_nil(err)
        assert.is_nil(read_file(BUNDLE))
        assert.are.equal(0, #state.system_calls)
    end)

    it("prefers a hand-made bundle on Linux", function()
        -- given
        use_linux()
        mtimes[BUNDLE] = 1

        -- when
        local ca_file, status = core_util.ensure_ca_bundle(LINUX_OPTS)

        -- then
        assert.are.equal(BUNDLE, ca_file)
        assert.are.equal("system", status)
    end)

    it("reports a Linux system without a known CA file", function()
        -- given
        use_linux()
        mtimes[SYSTEM_CA] = nil

        -- when
        local ca_file, status, err = core_util.ensure_ca_bundle(LINUX_OPTS)

        -- then
        assert.is_nil(ca_file)
        assert.are.equal("skipped", status)
        assert.are.equal("no system CA file found", err)
    end)

    it("moves curl's reason and a trust hint under the request error header", function()
        -- given
        local message = HEADER .. "\n" .. TRACE .. TLS_REASON .. "\nMore details here"

        -- when
        local explained, shown = core_util.explain_curl_error(message)

        -- then
        local lines = vim.split(explained, "\n")
        assert.are.equal(3, shown)
        assert.are.same({ HEADER, TLS_REASON, HINT }, { lines[1], lines[2], lines[3] })
        assert.are.equal("* Host bank.test:443 was resolved.", lines[4])
        assert.are.equal("More details here", lines[#lines])
    end)

    it("adds no trust hint to other curl failures", function()
        -- given
        local reason = "curl: (6) Could not resolve host: bank.test"

        -- when
        local explained, shown = core_util.explain_curl_error(HEADER .. "\n* trace\n" .. reason)

        -- then
        assert.are.equal(2, shown)
        assert.are.equal(HEADER .. "\n" .. reason .. "\n* trace\n" .. reason, explained)
    end)

    it("leaves messages without a curl failure alone", function()
        -- then
        assert.is_nil(core_util.explain_curl_error(HEADER .. "\nscript failed: boom"))
        assert.is_nil(core_util.explain_curl_error("Parser error\n" .. TLS_REASON))
        assert.is_nil(core_util.explain_curl_error(nil))
    end)

    it("wraps the kulala error logger once", function()
        -- given
        local logger, shown = stub_logger()

        -- when
        local wrapped = core_util.explain_request_errors()
        local wrapped_error = logger.error
        local wrapped_again = core_util.explain_request_errors()
        logger.error(HEADER .. "\n" .. TRACE .. TLS_REASON, 2)
        logger.error("something else", 1, { report = true })

        -- then
        assert.is_true(wrapped)
        assert.is_true(wrapped_again)
        local explained = table.concat({ HEADER, TLS_REASON, HINT, TRACE .. TLS_REASON }, "\n")
        assert.are.equal(wrapped_error, logger.error)
        assert.are.equal(explained, shown[1].message)
        assert.are.equal(3, shown[1].lines_no)
        assert.are.same({ message = "something else", lines_no = 1, opts = { report = true } }, shown[2])
    end)

    it("does nothing when the kulala logger is unavailable", function()
        -- given
        helper.stub_module("kulala.logger", { error = "not a function" })

        -- then
        assert.is_false(core_util.explain_request_errors())
    end)

    it("refreshes the trust: rebuilds a fresh bundle and restarts the daemon", function()
        -- given
        write_file(BUNDLE, "current")
        mtimes[BUNDLE] = 200

        -- when
        local status = core_util.refresh_trust(OPTS)

        -- then
        assert.are.equal("rebuilt", status)
        assert.are.equal(ROOTS_PEM .. "\n" .. KEYCHAIN_PEM, read_file(BUNDLE))
        assert.are.equal(BUNDLE, vim.env.CURL_CA_BUNDLE)
        assert.are.same(EXPORT_CMD, state.system_calls[1].command)
        assert.are.same(RESTART_CMD, state.system_calls[2].command)
        assert.are.equal(1, #state.notifications)
        assert.are.equal(vim.log.levels.INFO, state.notifications[1].level)
    end)

    it("refreshes the trust on Linux by restarting the daemon with the system CA file", function()
        -- given
        use_linux()

        -- when
        local status = core_util.refresh_trust(LINUX_OPTS)

        -- then
        assert.are.equal("system", status)
        assert.are.equal(SYSTEM_CA, vim.env.CURL_CA_BUNDLE)
        assert.are.equal(1, #state.system_calls)
        assert.are.same(RESTART_CMD, state.system_calls[1].command)
        assert.are.equal(vim.log.levels.INFO, state.notifications[1].level)
        assert.is_truthy(state.notifications[1].message:find(SYSTEM_CA, 1, true))
    end)

    it("leaves the daemon alone when the bundle cannot be rebuilt", function()
        -- given
        mtimes[KEYCHAIN] = nil

        -- when
        local status = core_util.refresh_trust(OPTS)

        -- then
        assert.are.equal("skipped", status)
        assert.are.equal(0, #state.system_calls)
        assert.are.equal(1, #state.notifications)
        assert.are.equal(vim.log.levels.WARN, state.notifications[1].level)
    end)

    it("sets up the trust before kulala starts", function()
        -- given
        local logger, shown = stub_logger()

        -- when
        core_util.setup_ca_trust(OPTS)
        logger.error(HEADER .. "\n" .. TRACE .. TLS_REASON, 2)
        state.user_commands.KulalaTrustRefresh.callback({})

        -- then
        assert.are.equal(BUNDLE, vim.env.CURL_CA_BUNDLE)
        assert.are.equal(3, shown[1].lines_no)
        assert.are.same(RESTART_CMD, state.system_calls[3].command)
    end)

    it("keeps a CURL_CA_BUNDLE that is already set", function()
        -- given
        vim.env.CURL_CA_BUNDLE = "/custom/bundle.pem"

        -- when
        core_util.setup_ca_trust(OPTS)

        -- then
        assert.are.equal("/custom/bundle.pem", vim.env.CURL_CA_BUNDLE)
        assert.are.equal(ROOTS_PEM .. "\n" .. KEYCHAIN_PEM, read_file(BUNDLE))
    end)
end)
