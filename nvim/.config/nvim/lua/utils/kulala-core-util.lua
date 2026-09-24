-- Keeps the auto-downloaded kulala-core binary runnable on macOS.
--
-- kulala-core releases ship a Bun-built, linker-signed ad-hoc binary. macOS 27+ rejects that
-- signature and SIGKILLs the process on launch (exit 137, no output), which kulala.nvim then
-- reports as "invalid kulala-core parse output" / "No requests found in the document".
-- Re-signing the file ad-hoc (`codesign --force --sign -`) makes it runnable again.

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

local NOTIFY_TITLE = "kulala-core"

---@type table<string, true> fingerprints of binaries verified in this session
local verified = {}
---@type table<string, true> fingerprints with a verification in flight
local pending = {}

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

return M
