-- Keeps the auto-downloaded kulala-core binary runnable on macOS.
--
-- kulala-core releases ship a Bun-built, linker-signed ad-hoc binary. macOS 27+ rejects that
-- signature and SIGKILLs the process on launch (exit 137, no output), which kulala.nvim then
-- reports as "invalid kulala-core parse output" / "No requests found in the document".
-- Re-signing the file ad-hoc (`codesign --force --sign -`) makes it runnable again.
--
-- Also keeps kulala-core's TLS trust equal to the system's. kulala-core 3.x always runs its bundled static
-- curl (OpenSSL, trusts only /etc/ssl/cert.pem) and ignores KULALA_CURL_PATH, so hosts signed by a CA that
-- lives only in the macOS System keychain (bank, Zscaler) fail with `curl: (60)`. curl honours
-- CURL_CA_BUNDLE, so a bundle of the public roots plus the keychain export is maintained here.
-- Linux has no second store: the distribution's CA file already holds the locally installed CAs, so
-- CURL_CA_BUNDLE just points at it (the bundled curl only knows /etc/ssl/certs/ca-certificates.crt).

local M = {}

---@alias KulalaCoreSignStatus
---| "skipped"  # not macOS, codesign missing, or binary not installed
---| "pending"  # a verification for this binary is already running
---| "valid"    # signature accepted by macOS
---| "resigned" # signature was invalid and has been replaced
---| "failed"   # re-signing failed; see err

---@class KulalaCoreSignOptions
---@field path? string kulala-core binary; defaults to the kulala.nvim backend path
---@field on_done? fun(status: KulalaCoreSignStatus, err?: string)

---@alias KulalaCaBundleStatus
---| "skipped" # nothing to hand to curl or to rebuild from; see err
---| "system"  # not macOS: nothing is generated, a hand-made bundle or the system CA file is used as is
---| "fresh"   # macOS: bundle is newer than its sources
---| "rebuilt" # macOS: bundle was regenerated
---| "failed"  # macOS: regeneration failed, the previous bundle (if any) is kept; see err

---@class KulalaCaBundleOptions
---@field bundle? string CA bundle handed to curl; defaults to ~/tools/certs/curl-ca-bundle.pem
---@field roots? string macOS: PEM file with the public roots; defaults to /etc/ssl/cert.pem
---@field keychain? string macOS: keychain with the locally installed CAs; defaults to the System keychain
---@field system_ca_files? string[] not macOS: CA files to probe; defaults to the common Linux locations
---@field force? boolean macOS: rebuild even when the bundle is newer than its sources

local NOTIFY_TITLE = "kulala-core"

local DEFAULT_CA_BUNDLE = "~/tools/certs/curl-ca-bundle.pem"
local SYSTEM_ROOTS = "/etc/ssl/cert.pem"
local SYSTEM_KEYCHAIN = "/Library/Keychains/System.keychain"
--- System CA files of the common Linux distributions, in the order Go's crypto/x509 probes them.
local LINUX_CA_FILES = {
    "/etc/ssl/certs/ca-certificates.crt", -- Debian, Ubuntu, Arch, Gentoo
    "/etc/pki/tls/certs/ca-bundle.crt", -- Fedora, RHEL 6
    "/etc/ssl/ca-bundle.pem", -- openSUSE
    "/etc/pki/tls/cacert.pem", -- OpenELEC
    "/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem", -- CentOS, RHEL 7+
    "/etc/ssl/cert.pem", -- Alpine
}
local PEM_MARKER = "-----BEGIN CERTIFICATE-----"
--- pkill -f pattern: the daemon's whole command line is `<path>/kulala-core --daemon`.
local DAEMON_PATTERN = "/kulala-core --daemon$"
local TRUST_REFRESH_COMMAND = "KulalaTrustRefresh"
--- Start of the message kulala.nvim logs for a failed request, followed by curl's verbose trace.
local REQUEST_ERROR_PREFIX = "Errors in request "
--- curl exit codes for "the server certificate could not be verified against the CA bundle".
local TLS_TRUST_CODES = { ["60"] = true, ["77"] = true }
local TLS_TRUST_HINT = "kulala's curl does not trust this server's CA: run :" .. TRUST_REFRESH_COMMAND

---@type table<string, true> fingerprints of binaries verified in this session
local verified = {}
---@type table<string, true> fingerprints with a verification in flight
local pending = {}
---@type table|nil kulala.logger module whose `error` is already wrapped
local explained_logger

--- Whether the running platform enforces code signatures (macOS only).
---@return boolean
local function is_macos()
    local uname = vim.uv.os_uname()
    return uname ~= nil and uname.sysname == "Darwin"
end

--- Fingerprint the binary so a re-downloaded or re-signed file gets verified again.
---@param path string
---@return string|nil fingerprint nil when the file does not exist
local function fingerprint(path)
    local stat = vim.uv.fs_stat(path)
    if not stat then
        return nil
    end
    local mtime = type(stat.mtime) == "table" and stat.mtime.sec or 0
    return string.format("%s|%d|%d", path, stat.size or 0, mtime)
end

--- Notify from any context (vim.system callbacks run outside the main loop).
---@param message string
---@param level integer
local function notify(message, level)
    vim.schedule(function()
        vim.notify(message, level, { title = NOTIFY_TITLE })
    end)
end

--- Resolve the kulala-core binary path from the kulala.nvim backend (honours `kulala_core.path`).
---@return string|nil
function M.core_path()
    local ok, backend = pcall(require, "kulala.backend")
    if not ok or type(backend) ~= "table" or type(backend.get_bin_path) ~= "function" then
        return nil
    end
    local ok_path, path = pcall(backend.get_bin_path)
    if ok_path and type(path) == "string" and path ~= "" then
        return path
    end
    return nil
end

--- Verify the kulala-core code signature and re-sign it ad-hoc when macOS rejects it.
--- Asynchronous and idempotent: each distinct binary (path + size + mtime) is verified once per session.
---@param opts? KulalaCoreSignOptions
function M.ensure_signed(opts)
    opts = opts or {}
    local on_done = opts.on_done

    --- Report the outcome on the main loop.
    ---@param status KulalaCoreSignStatus
    ---@param err? string
    local function finish(status, err)
        if not on_done then
            return
        end
        vim.schedule(function()
            on_done(status, err)
        end)
    end

    if not is_macos() then
        return finish("skipped", "not macOS")
    end
    if vim.fn.executable("codesign") ~= 1 then
        return finish("skipped", "codesign not found")
    end
    local path = opts.path or M.core_path()
    if not path then
        return finish("skipped", "kulala-core path not resolved")
    end
    local key = fingerprint(path)
    if not key then
        return finish("skipped", "kulala-core not installed: " .. path)
    end
    if verified[key] then
        return finish("valid")
    end
    if pending[key] then
        return finish("pending")
    end
    pending[key] = true

    vim.system({ "codesign", "--verify", "--strict", path }, { text = true }, function(verify)
        if verify.code == 0 then
            pending[key] = nil
            verified[key] = true
            return finish("valid")
        end
        vim.system({ "codesign", "--force", "--sign", "-", path }, { text = true }, function(sign)
            pending[key] = nil
            if sign.code ~= 0 then
                local err = vim.trim(sign.stderr or "")
                notify(string.format("Failed to re-sign %s: %s", path, err), vim.log.levels.ERROR)
                return finish("failed", err)
            end
            local resigned_key = fingerprint(path)
            if resigned_key then
                verified[resigned_key] = true
            end
            notify("Re-signed kulala-core: macOS rejected its code signature", vim.log.levels.INFO)
            finish("resigned")
        end)
    end)
end

--- Modification time of a libuv stat result in seconds.
---@param stat table|nil
---@return integer
local function mtime_of(stat)
    return stat and type(stat.mtime) == "table" and stat.mtime.sec or 0
end

--- Read a whole file.
---@param path string
---@return string|nil content nil when the file cannot be read
local function read_file(path)
    local file = io.open(path, "r")
    if not file then
        return nil
    end
    local content = file:read("*a")
    file:close()
    return content
end

--- Replace a file through a rename, so a concurrent curl never reads a half-written bundle.
---@param path string
---@param content string
---@return boolean ok
---@return string|nil err
local function write_file_atomic(path, content)
    local tmp = string.format("%s.%d.tmp", path, vim.uv.os_getpid())
    local file, open_err = io.open(tmp, "w")
    if not file then
        return false, open_err
    end
    local written, write_err = file:write(content)
    file:close()
    if not written then
        os.remove(tmp)
        return false, write_err
    end
    local renamed, rename_err = os.rename(tmp, path)
    if not renamed then
        os.remove(tmp)
        return false, rename_err
    end
    return true
end

--- First path of the list that exists.
---@param paths string[]
---@return string|nil
local function first_existing(paths)
    for _, path in ipairs(paths) do
        if vim.uv.fs_stat(path) then
            return path
        end
    end
    return nil
end

--- Resolve the CA file kulala-core's bundled curl should verify servers with.
--- macOS: a generated bundle, the public roots followed by every certificate of the keychain, regenerated when
--- it is missing or older than one of the two. Synchronous (the keychain export takes ~50 ms) because the path
--- has to be in the environment before the kulala-core daemon starts.
--- Elsewhere nothing is generated: a hand-made bundle at `opts.bundle` wins, else the distribution's CA file,
--- which already contains the locally installed CAs.
---@param opts? KulalaCaBundleOptions
---@return string|nil ca_file path for CURL_CA_BUNDLE; nil when there is none
---@return KulalaCaBundleStatus status
---@return string|nil err
function M.ensure_ca_bundle(opts)
    opts = opts or {}
    local bundle = opts.bundle or vim.fn.expand(DEFAULT_CA_BUNDLE)
    local roots = opts.roots or SYSTEM_ROOTS
    local keychain = opts.keychain or SYSTEM_KEYCHAIN

    local bundle_stat = vim.uv.fs_stat(bundle)
    local existing = bundle_stat and bundle or nil
    if not is_macos() then
        local system_ca = existing or first_existing(opts.system_ca_files or LINUX_CA_FILES)
        if not system_ca then
            return nil, "skipped", "no system CA file found"
        end
        return system_ca, "system"
    end
    if vim.fn.executable("security") ~= 1 then
        return existing, "skipped", "security not found"
    end
    local roots_stat, keychain_stat = vim.uv.fs_stat(roots), vim.uv.fs_stat(keychain)
    if not roots_stat or not keychain_stat then
        return existing, "skipped", "trust source not found: " .. (roots_stat and keychain or roots)
    end
    local newest_source = math.max(mtime_of(roots_stat), mtime_of(keychain_stat))
    if bundle_stat and not opts.force and mtime_of(bundle_stat) >= newest_source then
        return bundle, "fresh"
    end

    --- Keep whatever bundle is there and report why it was not regenerated.
    ---@param err string
    ---@return string|nil bundle
    ---@return KulalaCaBundleStatus status
    ---@return string err
    local function fail(err)
        notify(string.format("CA bundle %s not rebuilt: %s", bundle, err), vim.log.levels.WARN)
        return existing, "failed", err
    end

    local roots_pem = read_file(roots)
    if not roots_pem then
        return fail("cannot read " .. roots)
    end
    local export_cmd = { "security", "find-certificate", "-a", "-p", keychain }
    local export = vim.system(export_cmd, { text = true, timeout = 10000 }):wait()
    if export.code ~= 0 or not (export.stdout or ""):find(PEM_MARKER, 1, true) then
        return fail("keychain export failed: " .. vim.trim(export.stderr or ""))
    end
    vim.fn.mkdir(bundle:match("^(.*)/[^/]*$") or ".", "p")
    local ok, write_err = write_file_atomic(bundle, roots_pem .. "\n" .. export.stdout)
    if not ok then
        return fail(write_err or "write failed")
    end
    return bundle, "rebuilt"
end

--- Put curl's own failure line right under the header of a kulala request error.
--- kulala.nvim shows only the first lines of "Errors in request <url> at line: N\n<curl verbose trace>", and
--- the reason (`curl: (60) SSL certificate ...`) is at the end of the trace, so the notification never says why.
--- Certificate verification failures also get the hint to run :KulalaTrustRefresh.
---@param message any message passed to kulala.logger.error
---@return string|nil explained nil when the message is not a failed curl request
---@return integer|nil shown number of leading lines that carry the explanation
function M.explain_curl_error(message)
    if type(message) ~= "string" or message:sub(1, #REQUEST_ERROR_PREFIX) ~= REQUEST_ERROR_PREFIX then
        return nil
    end
    local header, trace = message:match("^([^\n]*)\n(.+)$")
    if not header then
        return nil
    end
    local reason, code
    for line in trace:gmatch("[^\r\n]+") do
        code = line:match("^curl: %((%d+)%)")
        if code then
            reason = line
            break
        end
    end
    if not reason then
        return nil
    end
    local lines = { header, reason }
    if TLS_TRUST_CODES[code] then
        table.insert(lines, TLS_TRUST_HINT)
    end
    local shown = #lines
    table.insert(lines, trace)
    return table.concat(lines, "\n"), shown
end

--- Wrap kulala.nvim's error logger so a failed request shows curl's reason instead of the first trace line.
--- kulala.nvim fires no event for failed requests, hence the wrap of an internal function; when the logger
--- changes shape this does nothing and kulala's own message is shown.
---@return boolean wrapped
function M.explain_request_errors()
    local ok, logger = pcall(require, "kulala.logger")
    if not ok or type(logger) ~= "table" or type(logger.error) ~= "function" then
        return false
    end
    if explained_logger == logger then
        return true
    end
    local original = logger.error
    logger.error = function(message, lines_no, opts)
        local explained, shown = M.explain_curl_error(message)
        if not explained then
            return original(message, lines_no, opts)
        end
        return original(explained, math.max(lines_no or 1, shown), opts)
    end
    explained_logger = logger
    return true
end

--- Bring the CA file up to date (macOS: rebuild the bundle now) and restart the kulala-core daemon, which keeps
--- the environment it was started with: the next request starts a daemon that sees the current CURL_CA_BUNDLE.
---@param opts? KulalaCaBundleOptions
---@return KulalaCaBundleStatus status
function M.refresh_trust(opts)
    local ca_file, status, err = M.ensure_ca_bundle(vim.tbl_extend("force", opts or {}, { force = true }))
    if status == "failed" then
        return status
    end
    if not ca_file then
        notify("No CA file for kulala-core's curl: " .. tostring(err), vim.log.levels.WARN)
        return status
    end
    vim.env.CURL_CA_BUNDLE = vim.env.CURL_CA_BUNDLE or ca_file
    vim.system({ "pkill", "-f", DAEMON_PATTERN }, { text = true }):wait()
    local used = vim.env.CURL_CA_BUNDLE
    local rebuilt = status == "rebuilt" and used == ca_file and " (rebuilt)" or ""
    local message = string.format("kulala-core restarted with CA file %s%s: send the request again", used, rebuilt)
    notify(message, vim.log.levels.INFO)
    return status
end

--- Make kulala-core's bundled curl trust what the system trusts: export CURL_CA_BUNDLE (unless already set),
--- explain failed requests and register :KulalaTrustRefresh. Call before `require("kulala").setup()`, i.e.
--- before the kulala-core daemon can start.
---@param opts? KulalaCaBundleOptions
function M.setup_ca_trust(opts)
    local ca_file = M.ensure_ca_bundle(opts)
    vim.env.CURL_CA_BUNDLE = vim.env.CURL_CA_BUNDLE or ca_file
    M.explain_request_errors()
    vim.api.nvim_create_user_command(TRUST_REFRESH_COMMAND, function()
        M.refresh_trust(opts)
    end, { desc = "Refresh the CA file of kulala-core's curl and restart its daemon" })
end

return M
