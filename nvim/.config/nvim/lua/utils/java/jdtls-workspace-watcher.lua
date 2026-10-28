-- Patches a JDTLS client so that, once the workspace has settled after
-- ServiceReady, it verifies that the project's generated sources are in place
-- and dispatches update_projects_config (which forces m2e to re-run the APT
-- processors — MapStruct etc.) ONLY when that verification fails.
--
-- The refresh is sent through the specific client that became ready. This
-- matters when several Java workspaces are open: nvim-jdtls's public
-- update_projects_config helper uses the current buffer, but these callbacks
-- fire asynchronously and the current buffer can belong to another project.
--
-- • setup — patch a JDTLS client with the workspace watcher (idempotent)
-- • mark_recovery_refresh — make newly-restarted clients run a forced refresh
--   plus a full build after recovery (root-scoped when root dirs are supplied)
-- • find_refresh_reason — the verification the watcher runs; callable on demand
-- • config — timings and toggles (tests shrink the timers)
--
-- Why a refresh is sometimes needed: m2e's configurator wipes
-- target/generated-sources/annotations on project import. JDT's
-- GeneratedFileManager state thinks the impls already exist, so a normal full
-- build skips APT for them. Only update_projects_config resets that state,
-- causing APT to fire for all annotated sources.
--
-- Why it must not run unconditionally (measured 2026-10-28 on a warm
-- workspace): the config update itself removes and re-adds the APT source
-- folders, so JDT deletes and regenerates EVERY impl, and m2e re-runs the
-- "execute on configuration" plugins (openapi-generator). That was 13–35 s of
-- churn after the editor was already usable (~14 s), with every generated
-- file's diagnostics cleared and re-reported — and the old 3-second
-- "no $/progress" idle heuristic fired inside gaps of that update, saw the
-- transient MapStruct "No implementation was created" diagnostic and re-ran
-- the whole refresh a second time in most sessions.
--
-- Verification (`find_refresh_reason`), any hit = refresh:
--   1. a stale MapStruct APT diagnostic ("No implementation was created …
--      erroneous element") anywhere in this root;
--   2. the build barrier reported errors (WITHERROR) AND a file of this root
--      has an ERROR "… cannot be resolved" (generated type missing);
--   3. a jdtls project (java.project.getAll) has no `.classpath` yet (fresh
--      import) or a generated source entry of its `.classpath` does not exist
--      on disk (`mvn clean`, wiped target/);
--   4. a `@Mapper` source under a project's src/ has no `<Name>Impl.java`
--      under that project's generated source folders (ripgrep + glob; custom
--      `implementationName` and nested mappers count as missing, which only
--      means the refresh runs as it always did).
-- jdtls publishes the problem markers of every workspace file after a build
-- and Neovim stores them for unloaded buffers too (vim.diagnostic.get(nil)
-- returns them), so 1–2 are workspace-wide; 3–4 catch what leaves no marker
-- (an impl that is simply absent and unreferenced, a wiped generated folder).
--
-- Deterministic signals instead of idle timers:
--   • `java/buildWorkspace` — the handler calls IWorkspace.build() synchronously
--     and answers when the build is finished; used as the "initial build done"
--     barrier and after every refresh (timers are only safety nets);
--   • after a refresh, jdtls' update job ends with a `language/status` of type
--     ProjectStatus (message OK/WARNING/ERROR), sent before the follow-up
--     autobuild starts — that is the signal to build on. `language/eventNotification`
--     eventType 100 (ClasspathUpdated, from ClasspathUpdateHandler) only arrives
--     when the classpath CONTENT changed and is sent from INSIDE the update job
--     (traced 2026-10-28 on a real session: ClasspathUpdated → a build requested
--     right away answered in 1 s with the pre-refresh markers, the APT
--     regeneration ran only after "Projects updated"), so it is used as a hint
--     only: it shortens the ProjectStatus wait to `classpath_event_grace_ms`.
--
-- Lifecycle (per client):
--   waiting_ready → settling → initial_build → verifying
--     → healthy: healthy_recheck → done
--     → refresh:  refreshing → awaiting_update → building → verifying
--                 (any check still failing → up to MAX_APT_RETRIES more rounds)
--                 → watching → done
-- `settling` ends only after jdtls reported the end of its startup build jobs
-- (the "Register Watchers" job runs after JobHelpers.waitForBuildJobs(); the
-- didChangeWatchedFiles registration is the same moment) plus idle_ms of quiet;
-- an explicit build before that interrupts the running startup build (Eclipse
-- yields the autobuild to other workspace operations). Verifications run after
-- the publishDiagnostics burst is quiet, and a healthy verdict is re-checked
-- once (APT deletes orphaned impls and jdtls publishes markers a moment after
-- java/buildWorkspace answers — a verdict taken right away can be wrong).
-- Recovery (mark_recovery_refresh): the verification is skipped — the refresh
-- is forced and the build after it is a full one, as before.
-- After a refresh cycle settles clean, completion-facing caches (blink.cmp LSP
-- source, jdtls classpath cache, MapStruct engine caches) are cleared and
-- buffers that still show pre-refresh "cannot be resolved" errors get their
-- jdtls document replayed (detach + reattach — the automatic form of :e).
-- The watcher disposes itself (timer + autocmds) when it finishes or when the
-- client stops; restarted clients get a fresh watcher via their own LspAttach.
--
-- Idempotent: marks the client with _patched_workspace_watcher so subsequent
-- calls (e.g. additional buffers attaching) are no-ops.

local M = {}

-- Timings and toggles. Tests shrink the timers; users may flip the toggles.
M.config = {
    -- quiet time after ServiceReady (no $/progress) before the initial build barrier is requested
    idle_ms = 3000,
    -- publishDiagnostics for build markers arrive shortly after java/buildWorkspace answers
    verify_grace_ms = 750,
    -- max wait for the update-applied signal (ProjectStatus) after a refresh was dispatched; builds
    -- anyway afterwards
    update_event_timeout_ms = 15 * 1000,
    -- ClasspathUpdated arrives inside the update job, before it ends: keep waiting for ProjectStatus,
    -- but at most this long after the event
    classpath_event_grace_ms = 5 * 1000,
    -- after ServiceReady the barrier waits for jdtls' "Register Watchers" job (it runs once the startup
    -- build jobs are done; also visible as the didChangeWatchedFiles registration) plus idle_ms of quiet;
    -- without that signal the idle timer alone may only fire after settle_max_ms
    settle_max_ms = 60 * 1000,
    -- publishDiagnostics bursts lag the java/buildWorkspace answer: verify only after this much quiet
    -- (capped at diag_quiet_max_ms)
    diag_quiet_ms = 1500,
    diag_quiet_max_ms = 10 * 1000,
    -- re-verify once after a healthy verdict: APT deletes orphaned impls and jdtls publishes the
    -- markers a moment after the build answer (seen 2026-10-28 22:55)
    healthy_recheck_ms = 10 * 1000,
    -- safety net for a java/buildWorkspace request that never answers
    build_timeout_ms = 10 * 60 * 1000,
    recovery_mark_ttl_ms = 2 * 60 * 1000,
    projects_timeout_ms = 10 * 1000,
    refresh_retry_ms = 5 * 1000,
    max_refresh_attempts = 3,
    apt_watch_window_ms = 60 * 1000,
    max_apt_retries = 2,
    -- verification toggles (see header); all on by default
    check_stale_apt_diagnostics = true,
    check_unresolved_diagnostics = true,
    check_classpath_generated_dirs = true,
    check_mapper_impls = true,
    rg_bin = "rg",
}

-- org.eclipse.jdt.ls.core.internal.EventType (jdtls 1.61): ClasspathUpdated=100,
-- ProjectsImported=200, ProjectsDeleted=210, IncompatibleGradleJdkIssue=300,
-- UpgradeGradleWrapper=400, SourceInvalidated=500, PreviewFeaturesNotAllowed=600
local EVENT_CLASSPATH_UPDATED = 100

-- org.eclipse.jdt.ls.core.internal.BuildWorkspaceStatus
local BUILD_STATUS = {
    [0] = "FAILED",
    [1] = "SUCCEEDED",
    [2] = "WITHERROR",
    [3] = "CANCELLED",
}

-- MapStruct APT emits this when it ran against a classpath that still had
-- unresolved ("erroneous") elements — the impl was skipped, not rejected
-- because of user code. The text comes from the project's own MapStruct
-- processor (version pinned by the build), so it only drifts on a MapStruct
-- upgrade. Each signature is a list of substrings that must ALL be present in
-- the diagnostic message; a message matching ANY signature counts as stale.
-- If a future MapStruct version rewords the message, add its variant here.
local STALE_APT_DIAG_SIGNATURES = {
    -- wording as of MapStruct 1.x
    { "No implementation was created", "erroneous element" },
}

-- JDT reconciles an open buffer's working copy on didOpen/didChange. Buffers
-- opened before APT (re)generated the impls keep their pre-refresh
-- "X cannot be resolved to a type" errors until the document is replayed
-- (what :e does manually). An ERROR diagnostic containing this substring
-- marks the buffer for revalidation once the refresh cycle settles clean.
local STALE_RESOLUTION_DIAG_SUBSTRING = "cannot be resolved"

-- ripgrep pattern for MapStruct mapper declarations (`@MapperConfig` excluded by the word boundary)
local MAPPER_PATTERN = [[@(org\.mapstruct\.)?Mapper\b]]

local BUILD_FILES = { "pom.xml", "build.gradle", "build.gradle.kts" }

local log = require("utils.logging-util").new({
    name = "jdtls.status",
    filename = "jdtls-status.log",
    level = vim.log.levels.INFO,
})

local recovery_refresh = {
    until_at = 0,
    reason = nil,
    -- set of root_dirs the marker applies to; nil = any client (legacy fallback)
    roots = nil,
}

--- Mark soon-created JDTLS clients as recovery clients that need a forced refresh + full build.
--- When root_dirs is supplied the marker only applies to clients whose
--- config.root_dir matches, and each root is consumed on first claim so an
--- unrelated workspace starting within the TTL is not dragged into recovery.
---@param reason string|nil
---@param root_dirs string[]|nil
function M.mark_recovery_refresh(reason, root_dirs)
    recovery_refresh.until_at = vim.uv.now() + M.config.recovery_mark_ttl_ms
    recovery_refresh.reason = reason or "recovery"
    if type(root_dirs) == "table" and #root_dirs > 0 then
        recovery_refresh.roots = {}
        for _, root in ipairs(root_dirs) do
            recovery_refresh.roots[root] = true
        end
    else
        recovery_refresh.roots = nil
    end
end

--- Claim the active recovery refresh reason for a client, if the marker matches.
---@param client vim.lsp.Client
---@return string|nil
local function claim_recovery_reason(client)
    if vim.uv.now() > recovery_refresh.until_at then
        return nil
    end

    local roots = recovery_refresh.roots
    if roots then
        local root_dir = client.config and client.config.root_dir
        if not (root_dir and roots[root_dir]) then
            return nil
        end
        -- consume: one recovery build per restarted root
        roots[root_dir] = nil
    end

    return recovery_refresh.reason or "recovery"
end

--- Return whether a diagnostic carries the MapStruct stale-APT signature
--- (implementation skipped because of an erroneous classpath element).
---@param diag vim.Diagnostic
---@return boolean
local function diag_matches_stale_apt(diag)
    local message = diag and diag.message
    if type(message) ~= "string" then
        return false
    end
    for _, signature in ipairs(STALE_APT_DIAG_SIGNATURES) do
        local all_present = true
        for _, needle in ipairs(signature) do
            if not message:find(needle, 1, true) then
                all_present = false
                break
            end
        end
        if all_present then
            return true
        end
    end
    return false
end

--- Return a loaded Java buffer attached to the supplied client (nil when none is loaded).
---@param client vim.lsp.Client
---@param preferred_bufnr integer|nil
---@return integer|nil
local function client_java_buffer(client, preferred_bufnr)
    if
        preferred_bufnr
        and vim.api.nvim_buf_is_valid(preferred_bufnr)
        and vim.api.nvim_buf_is_loaded(preferred_bufnr)
        and vim.bo[preferred_bufnr].filetype == "java"
        and vim.lsp.buf_is_attached(preferred_bufnr, client.id)
    then
        return preferred_bufnr
    end

    for bufnr in pairs(client.attached_buffers or {}) do
        if
            vim.api.nvim_buf_is_valid(bufnr)
            and vim.api.nvim_buf_is_loaded(bufnr)
            and vim.bo[bufnr].filetype == "java"
        then
            return bufnr
        end
    end

    return nil
end

--- Return whether an absolute path belongs to a workspace root (nil/empty root matches everything).
--- Buffer names carry resolved directory symlinks (macOS /var → /private/var, symlinked checkouts),
--- so the root is compared both as given and resolved.
---@param root string|nil
---@param path string
---@return boolean
local realpath_cache = {}
local function path_in_root(root, path)
    if not root or root == "" then
        return true
    end
    local function under(prefix)
        return path == prefix or path:sub(1, #prefix + 1) == prefix .. "/"
    end
    if under(root) then
        return true
    end
    local real = realpath_cache[root]
    if real == nil then
        real = vim.uv.fs_realpath(root) or false
        realpath_cache[root] = real
    end
    return real and real ~= root and under(real) or false
end

--- Return whether a directory holds one of the known build files.
---@param dir string
---@return boolean
local function has_build_file(dir)
    for _, name in ipairs(BUILD_FILES) do
        if vim.uv.fs_stat(dir .. "/" .. name) then
            return true
        end
    end
    return false
end

--- Turn the URIs returned by java.project.getAll into existing module directories that
--- carry a build file (jdtls' invisible default project and vanished dirs are skipped).
---@param projects string[]
---@return string[]
local function project_roots_from_uris(projects)
    local roots = {}
    for _, uri in ipairs(projects or {}) do
        if type(uri) == "string" and uri:sub(1, 7) == "file://" then
            local ok, path = pcall(vim.uri_to_fname, uri)
            if ok and type(path) == "string" then
                path = path:gsub("/+$", "")
                local stat = vim.uv.fs_stat(path)
                if stat and stat.type == "directory" and has_build_file(path) then
                    table.insert(roots, path)
                end
            end
        end
    end
    return roots
end

--- Read the generated source folders of a project's `.classpath` (every `kind="src"` entry whose
--- path mentions "generated": m2e-apt output, openapi-generator output, …) as absolute paths.
---@param root string module directory
---@return string[]|nil dirs nil when the project has no .classpath yet
local function classpath_generated_dirs(root)
    local file = io.open(root .. "/.classpath", "r")
    if not file then
        return nil
    end
    local content = file:read("*a")
    file:close()

    local dirs = {}
    for entry in content:gmatch("<classpathentry[^>]->") do
        local kind = entry:match('kind="([^"]*)"')
        local path = entry:match('path="([^"]*)"')
        if kind == "src" and path and path:find("generated", 1, true) then
            table.insert(dirs, root .. "/" .. path)
        end
    end
    return dirs
end

--- Request the project URIs of a client (java.project.getAll) with a timeout.
---@param client vim.lsp.Client
---@param bufnr integer|nil
---@param done fun(projects: string[]|nil)
local function request_projects(client, bufnr, done)
    local responded = false
    local ok, req_id = client:request("workspace/executeCommand", {
        command = "java.project.getAll",
    }, function(err, projects)
        if responded then
            return
        end
        responded = true

        if err then
            log.fmt_warn("client_id=%d java.project.getAll failed: %s", client.id, vim.inspect(err))
            done(nil)
            return
        end
        if type(projects) ~= "table" or #projects == 0 then
            log.fmt_warn("client_id=%d java.project.getAll returned no projects", client.id)
            done(nil)
            return
        end
        done(projects)
    end, client_java_buffer(client, bufnr))

    if not ok then
        log.fmt_warn("client_id=%d could not request java.project.getAll", client.id)
        done(nil)
        return
    end

    vim.defer_fn(function()
        if responded then
            return
        end
        responded = true
        pcall(function()
            client:cancel_request(req_id)
        end)
        log.fmt_warn("client_id=%d java.project.getAll timed out after %dms", client.id, M.config.projects_timeout_ms)
        done(nil)
    end, M.config.projects_timeout_ms)
end

--- Notify the client to refresh the configuration of the given projects.
---@param client vim.lsp.Client
---@param projects string[]
---@return boolean sent
local function notify_projects_update(client, projects)
    local params = {
        identifiers = vim.tbl_map(function(project)
            return { uri = project }
        end, projects),
    }
    local notify_ok, notify_err = pcall(function()
        client:notify("java/projectConfigurationsUpdate", params)
    end)
    if not notify_ok then
        log.fmt_warn("client_id=%d project config notify failed: %s", client.id, tostring(notify_err))
        return false
    end
    log.fmt_info("client_id=%d dispatched project config refresh for %d projects", client.id, #projects)
    return true
end

--- Find a stale MapStruct APT diagnostic in any buffer (loaded or not) of the root.
---@param root string|nil
---@return vim.Diagnostic|nil diag
---@return string|nil file
local function find_stale_apt_diag(root)
    for _, diag in ipairs(vim.diagnostic.get(nil)) do
        if diag_matches_stale_apt(diag) and diag.bufnr and vim.api.nvim_buf_is_valid(diag.bufnr) then
            local file = vim.api.nvim_buf_get_name(diag.bufnr)
            if path_in_root(root, file) then
                return diag, file
            end
        end
    end
    return nil, nil
end

--- Find an ERROR "… cannot be resolved" diagnostic in any buffer (loaded or not) of the root.
---@param root string|nil
---@return string|nil file
local function find_unresolved_diag(root)
    for _, diag in ipairs(vim.diagnostic.get(nil, { severity = vim.diagnostic.severity.ERROR })) do
        if
            type(diag.message) == "string"
            and diag.message:find(STALE_RESOLUTION_DIAG_SUBSTRING, 1, true)
            -- only a generated implementation type counts ("FooMapperImpl cannot be resolved to a type",
            -- "The import x.FooMapperImpl cannot be resolved"); a user's own unresolved type is WIP
            and diag.message:find("%u%w*Impl%f[%W]")
            and diag.bufnr
            and vim.api.nvim_buf_is_valid(diag.bufnr)
        then
            local file = vim.api.nvim_buf_get_name(diag.bufnr)
            if path_in_root(root, file) then
                return file
            end
        end
    end
    return nil
end

--- List the `@Mapper` sources under `<root>/src` with ripgrep (asynchronous; empty list when
--- ripgrep is unavailable or the directory does not exist).
---@param root string
---@param cb fun(files: string[], err: string|nil)
local function find_mapper_sources(root, cb)
    local src = root .. "/src"
    if not vim.uv.fs_stat(src) then
        cb({}, nil)
        return
    end
    local cmd = { M.config.rg_bin, "-l", "--no-messages", "--glob", "*.java", "-e", MAPPER_PATTERN, src }
    local ok, err = pcall(vim.system, cmd, { text = true }, function(out)
        vim.schedule(function()
            -- exit code 1 = no match; anything above = ripgrep failed
            if out.code > 1 then
                cb({}, string.format("rg exited with %d: %s", out.code, vim.trim(out.stderr or "")))
                return
            end
            local files = {}
            for line in (out.stdout or ""):gmatch("[^\n]+") do
                table.insert(files, line)
            end
            cb(files, nil)
        end)
    end)
    if not ok then
        cb({}, tostring(err))
    end
end

--- Return the first mapper whose generated `<Name>Impl.java` is missing under the given folders.
---@param mapper_files string[]
---@param generated_dirs string[]
---@return string|nil mapper_file
local function first_mapper_without_impl(mapper_files, generated_dirs)
    if #mapper_files == 0 then
        return nil
    end
    local search_path = table.concat(generated_dirs, ",")
    for _, mapper in ipairs(mapper_files) do
        local name = vim.fn.fnamemodify(mapper, ":t:r")
        local found = search_path ~= "" and vim.fn.globpath(search_path, "**/" .. name .. "Impl.java", false, true)
            or {}
        if #found == 0 then
            return mapper
        end
    end
    return nil
end

--- Verify the generated sources of one project root (checks 3 and 4 of the header).
---@param root string
---@param cb fun(reason: string|nil)
local function verify_project_root(root, cb)
    local cfg = M.config
    local generated_dirs = classpath_generated_dirs(root)
    local short = vim.fn.fnamemodify(root, ":t")

    if cfg.check_classpath_generated_dirs then
        if not generated_dirs then
            cb(string.format("project %s has no .classpath yet", short))
            return
        end
        for _, dir in ipairs(generated_dirs) do
            if not vim.uv.fs_stat(dir) then
                cb(string.format("generated source folder missing: %s", vim.fn.fnamemodify(dir, ":~:.")))
                return
            end
        end
    end

    if not cfg.check_mapper_impls then
        cb(nil)
        return
    end
    find_mapper_sources(root, function(mappers, err)
        if err then
            log.fmt_warn("mapper scan skipped for %s: %s", root, err)
            cb(nil)
            return
        end
        local missing = first_mapper_without_impl(mappers, generated_dirs or {})
        if missing then
            cb(string.format("no generated impl for %s", vim.fn.fnamemodify(missing, ":t")))
            return
        end
        log.fmt_debug(
            "project %s: %d mapper(s) verified against %d generated folder(s)",
            short,
            #mappers,
            #(generated_dirs or {})
        )
        cb(nil)
    end)
end

--- Decide whether a workspace needs the project config refresh (see the header for the four
--- checks). Asynchronous because the mapper scan runs ripgrep.
---@param client vim.lsp.Client
---@param opts { bufnr?: integer, build_status?: string, projects?: string[] }
---@param callback fun(reason: string|nil, projects: string[]|nil) reason = nil means healthy
function M.find_refresh_reason(client, opts, callback)
    opts = opts or {}
    local cfg = M.config
    local root = client.config and client.config.root_dir

    if cfg.check_stale_apt_diagnostics then
        local _, stale_file = find_stale_apt_diag(root)
        if stale_file then
            callback("stale MapStruct APT diagnostic in " .. vim.fn.fnamemodify(stale_file, ":t"), opts.projects)
            return
        end
    end

    if cfg.check_unresolved_diagnostics and opts.build_status == "WITHERROR" then
        local file = find_unresolved_diag(root)
        if file then
            callback(
                "build has errors and " .. vim.fn.fnamemodify(file, ":t") .. " has unresolved types",
                opts.projects
            )
            return
        end
    end

    if not (cfg.check_classpath_generated_dirs or cfg.check_mapper_impls) then
        callback(nil, opts.projects)
        return
    end

    local function verify_roots(projects)
        local roots = project_roots_from_uris(projects)
        local index = 0
        local function next_root()
            index = index + 1
            local candidate = roots[index]
            if not candidate then
                callback(nil, projects)
                return
            end
            verify_project_root(candidate, function(reason)
                if reason then
                    callback(reason, projects)
                else
                    next_root()
                end
            end)
        end
        next_root()
    end

    if opts.projects then
        verify_roots(opts.projects)
    else
        request_projects(client, opts.bufnr, function(projects)
            if not projects then
                -- cannot enumerate the projects: fall back to the root itself
                projects = root and { vim.uri_from_fname(root) } or {}
            end
            verify_roots(projects)
        end)
    end
end

--- Attach the JDTLS workspace watcher patch to the client.
---@param client vim.lsp.Client|nil
---@param bufnr integer|nil
function M.setup(client, bufnr)
    if not client or client.name ~= "jdtls" then
        return
    end
    if client._patched_workspace_watcher then
        return
    end
    ---@diagnostic disable-next-line: inject-field
    client._patched_workspace_watcher = true

    local cfg = M.config
    local recovery_reason = claim_recovery_reason(client)
    log.fmt_debug(
        "attached client_id=%d bufnr=%s recovery_reason=%s — installing workspace-settled watcher",
        client.id,
        tostring(bufnr),
        tostring(recovery_reason)
    )

    client.handlers = client.handlers or {}

    -- Lifecycle state; see the header comment for the transitions. Mirrored on the client as
    -- `_workspace_watcher_state` for inspection (:lua =vim.lsp.get_clients({name="jdtls"})[1]._workspace_watcher_state).
    local state
    local function set_state(next_state)
        state = next_state
        ---@diagnostic disable-next-line: inject-field
        client._workspace_watcher_state = next_state
    end
    set_state("waiting_ready")
    local refresh_attempts = 0
    local apt_retries = 0
    local disposed = false
    local detach_autocmd
    local diag_autocmd
    -- one timer, re-armed for whichever wait the current state needs
    local timer = assert(vim.uv.new_timer())
    local timer_purpose = nil
    local projects = nil -- java.project.getAll result, reused across the cycle
    local cycle_reason = nil -- why the current refresh cycle runs
    local pending_build = nil -- { cb = fun(status) } while a java/buildWorkspace request is in flight
    local startup_done = false -- jdtls reported the end of its startup build jobs ("Register Watchers")
    local settle_started = nil -- uv.now() at ServiceReady
    local last_publish = 0 -- uv.now() of the last textDocument/publishDiagnostics
    local last_build_status = nil -- status of the initial build barrier (for the healthy re-check)

    local on_timer, dispatch_refresh, run_build_after_refresh

    --- Return whether the client can no longer serve requests.
    local function client_gone()
        if client.is_stopped and client:is_stopped() then
            return true
        end
        return client.rpc ~= nil and client.rpc.is_closing()
    end

    --- Tear down the watcher exactly once (timer + autocmds).
    local function dispose(why)
        if disposed then
            return
        end
        disposed = true
        set_state("done")
        timer:stop()
        timer:close()
        if detach_autocmd then
            pcall(vim.api.nvim_del_autocmd, detach_autocmd)
            detach_autocmd = nil
        end
        if diag_autocmd then
            pcall(vim.api.nvim_del_autocmd, diag_autocmd)
            diag_autocmd = nil
        end
        log.fmt_debug("client_id=%d watcher disposed (%s)", client.id, why)
    end

    --- (Re)arm the timer for a purpose; the purpose tells on_timer what expired.
    local function arm(delay_ms, purpose)
        if disposed then
            return
        end
        timer_purpose = purpose
        timer:start(delay_ms, 0, on_timer)
    end

    --- Root-scoped path check for this client.
    local function in_root(path)
        return path_in_root(client.config and client.config.root_dir, path)
    end

    --- Clear completion-facing caches so freshly generated APT output
    --- (MapStruct impls etc.) resolves without further manual pokes.
    --- Only caches of already-loaded modules are cleared: this must not pull
    --- the MapStruct engine into projects that never used it.
    local function refresh_completion_sources(why)
        local ok, recovery = pcall(require, "utils.java.jdtls-recovery")
        if ok and recovery.refresh_blink_lsp then
            recovery.refresh_blink_lsp(why)
        else
            log.fmt_warn("client_id=%d could not refresh blink (%s): %s", client.id, why, tostring(recovery))
        end

        local classpath_util = package.loaded["utils.java.jdtls-classpath-util"]
        if classpath_util and classpath_util.clear_cache then
            pcall(classpath_util.clear_cache)
        end

        local mapstruct_context = package.loaded["modules.java.mapstruct.context"]
        if mapstruct_context and mapstruct_context.clear_all_caches then
            pcall(mapstruct_context.clear_all_caches)
        end

        log.fmt_info("client_id=%d refreshed completion sources (%s)", client.id, why)
    end

    --- Replay the jdtls document (didClose + didOpen via detach/reattach) for
    --- attached buffers that still show "cannot be resolved" errors after the
    --- refresh cycle. Those diagnostics come from a working-copy reconcile
    --- that ran before the generated sources existed; the replay makes jdtls
    --- reconcile again — the automatic form of the manual :e fix, minus the
    --- disk reload (so modified buffers are safe too).
    ---@param why string
    local function revalidate_stale_buffers(why)
        local stale_bufs = {}
        for buf in pairs(client.attached_buffers or {}) do
            if vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_buf_is_loaded(buf) then
                for _, diag in ipairs(vim.diagnostic.get(buf, { severity = vim.diagnostic.severity.ERROR })) do
                    if
                        type(diag.message) == "string"
                        and diag.message:find(STALE_RESOLUTION_DIAG_SUBSTRING, 1, true)
                    then
                        table.insert(stale_bufs, buf)
                        break
                    end
                end
            end
        end

        -- Snapshot first: detach/attach mutates client.attached_buffers,
        -- which must not happen while iterating it.
        for _, buf in ipairs(stale_bufs) do
            pcall(vim.lsp.buf_detach_client, buf, client.id)
            pcall(vim.lsp.buf_attach_client, buf, client.id)
        end
        if #stale_bufs > 0 then
            log.fmt_info(
                "client_id=%d revalidated %d buffer(s) with stale resolution diagnostics (%s)",
                client.id,
                #stale_bufs,
                why
            )
        end
    end

    --- Request a workspace build (incremental or full) and hand the status to cb when it is
    --- finished. The jdtls handler builds synchronously, so the answer IS the completion signal;
    --- the timer is only a safety net.
    ---@param full boolean
    ---@param why string
    ---@param cb fun(status: string|nil)
    local function build_workspace(full, why, cb)
        if client_gone() then
            dispose("client stopped before build (" .. why .. ")")
            return
        end
        set_state(full and "building_full" or "building")
        log.fmt_info(
            "client_id=%d requesting %s workspace build (%s)",
            client.id,
            full and "full" or "incremental",
            why
        )

        local finished = false
        pending_build = { cb = cb }
        local ok, req_id = client:request("java/buildWorkspace", full, function(err, result)
            if finished or disposed then
                return
            end
            finished = true
            pending_build = nil
            timer:stop()
            if err then
                log.fmt_warn("client_id=%d build failed (%s): %s", client.id, why, vim.inspect(err))
                cb(nil)
                return
            end
            local status = BUILD_STATUS[result] or tostring(result)
            log.fmt_info("client_id=%d build finished (%s): %s", client.id, why, status)
            cb(status)
        end, client_java_buffer(client, bufnr))

        if not ok then
            pending_build = nil
            log.fmt_warn("client_id=%d could not request workspace build (%s)", client.id, why)
            cb(nil)
            return
        end
        -- (a synchronously answered request has already cleared pending_build)
        if pending_build then
            pending_build.req_id = req_id
            pending_build.finish = function()
                finished = true
            end
            arm(cfg.build_timeout_ms, "build")
        end
    end

    --- Create the DiagnosticChanged watch (once) that revives the refresh
    --- cycle when the stale-APT diagnostic is published only after the
    --- cycle settled. Active only in the "watching" state.
    local begin_apt_retry
    local function ensure_diag_watch()
        if diag_autocmd then
            return
        end
        diag_autocmd = vim.api.nvim_create_autocmd("DiagnosticChanged", {
            callback = function(args)
                if disposed or state ~= "watching" then
                    return
                end
                local file = vim.api.nvim_buf_get_name(args.buf)
                if not in_root(file) then
                    return
                end
                for _, diag in ipairs(args.data and args.data.diagnostics or {}) do
                    if diag_matches_stale_apt(diag) then
                        begin_apt_retry("stale APT diagnostic published late in " .. file)
                        return
                    end
                end
            end,
        })
    end

    --- Run fn once textDocument/publishDiagnostics has been quiet for diag_quiet_ms (capped at
    --- diag_quiet_max_ms): the markers of a build arrive after java/buildWorkspace answers.
    ---@param fn fun()
    local function after_diag_quiet(fn)
        local started = vim.uv.now()
        local function poll()
            if disposed then
                return
            end
            local now = vim.uv.now()
            if now - last_publish >= cfg.diag_quiet_ms or now - started >= cfg.diag_quiet_max_ms then
                fn()
            else
                vim.defer_fn(poll, 250)
            end
        end
        vim.defer_fn(poll, cfg.verify_grace_ms)
    end

    --- The workspace verified healthy: announce it, then re-check once after healthy_recheck_ms
    --- (APT file deletions and marker publishing can lag the build answer) before disposing.
    local function finish_healthy(why)
        log.fmt_info("client_id=%d workspace verified healthy (%s) — no project config refresh", client.id, why)
        vim.notify("🏄 JDTLS ready — generated sources verified, no project refresh needed")
        set_state("healthy_recheck")
        arm(cfg.healthy_recheck_ms, "recheck")
    end

    --- Finish a refresh cycle: refresh completion sources, clear stale buffer
    --- diagnostics, then watch for a late stale-APT diagnostic before disposing.
    ---@param why string
    local function finish_cycle(why)
        refresh_completion_sources(why)
        -- Any further apt-retry rounds only need the config refresh,
        -- not another recovery full build.
        recovery_reason = nil
        revalidate_stale_buffers(why)

        if apt_retries > 0 then
            vim.notify("🧩 MapStruct impls resolved after project config re-refresh")
        end
        if apt_retries >= cfg.max_apt_retries then
            dispose("finalized (apt retry budget exhausted): " .. why)
            return
        end

        set_state("watching")
        ensure_diag_watch()
        arm(cfg.apt_watch_window_ms, "watch")
        log.fmt_info(
            "client_id=%d cycle done (%s) — watching diagnostics for %dms",
            client.id,
            why,
            cfg.apt_watch_window_ms
        )
    end

    --- Re-enter the refresh cycle because MapStruct APT skipped impl
    --- generation (stale "No implementation was created" diagnostic).
    ---@param why string
    begin_apt_retry = function(why)
        apt_retries = apt_retries + 1
        refresh_attempts = 0 -- fresh transient-failure budget for the new round
        log.fmt_warn(
            "client_id=%d %s — re-running project config refresh (apt retry %d/%d)",
            client.id,
            why,
            apt_retries,
            cfg.max_apt_retries
        )
        vim.notify(
            string.format(
                "🧩 MapStruct impls missing — re-refreshing project config (%d/%d)",
                apt_retries,
                cfg.max_apt_retries
            )
        )
        dispatch_refresh()
    end

    --- After the post-refresh build: run the same verification as at startup (a stale APT
    --- marker, missing impls, missing generated folders); any hit re-runs the cycle (bounded),
    --- otherwise the cycle is finished. A cycle that leaves impls missing must not be called resolved
    --- (seen 2026-10-28: 17 impls missing after a "resolved" verdict based on the marker alone).
    ---@param status string|nil
    local function verify_after_refresh(status)
        set_state("verifying")
        after_diag_quiet(function()
            if disposed then
                return
            end
            M.find_refresh_reason(
                client,
                { bufnr = bufnr, build_status = status, projects = projects },
                function(reason, list)
                    if disposed then
                        return
                    end
                    projects = list or projects
                    if reason then
                        if apt_retries < cfg.max_apt_retries then
                            begin_apt_retry(reason)
                            return
                        end
                        log.fmt_warn(
                            "client_id=%d still unhealthy after %d refresh retries: %s",
                            client.id,
                            cfg.max_apt_retries,
                            reason
                        )
                        vim.notify(
                            string.format(
                                "MapStruct impls still missing after %d refresh retries (%s) — run <leader>jdu manually",
                                cfg.max_apt_retries,
                                reason
                            ),
                            vim.log.levels.WARN
                        )
                        dispose("unhealthy after " .. cfg.max_apt_retries .. " retries: " .. reason)
                        return
                    end
                    finish_cycle(string.format("refresh (%s) built: %s", tostring(cycle_reason), tostring(status)))
                end
            )
        end)
    end

    --- Build after the config update was applied (incremental; full when recovering).
    run_build_after_refresh = function(why)
        build_workspace(recovery_reason ~= nil, why, verify_after_refresh)
    end

    --- Dispatch the project config refresh, retrying on transient failures, then wait for the
    --- update-applied signal (ProjectStatus / ClasspathUpdated, with a timeout) before building.
    dispatch_refresh = function()
        set_state("refreshing")
        refresh_attempts = refresh_attempts + 1
        log.fmt_info(
            "client_id=%d dispatching project config refresh: %s (attempt %d)",
            client.id,
            tostring(cycle_reason),
            refresh_attempts
        )

        local function on_dispatched(sent)
            if disposed then
                return
            end
            if sent then
                set_state("awaiting_update")
                arm(cfg.update_event_timeout_ms, "update_event")
                return
            end
            if refresh_attempts < cfg.max_refresh_attempts then
                set_state("retry_wait")
                log.fmt_warn(
                    "client_id=%d project config refresh failed — retrying in %dms (attempt %d/%d)",
                    client.id,
                    cfg.refresh_retry_ms,
                    refresh_attempts,
                    cfg.max_refresh_attempts
                )
                arm(cfg.refresh_retry_ms, "retry")
            else
                log.fmt_warn(
                    "client_id=%d project config refresh failed after %d attempts — giving up",
                    client.id,
                    cfg.max_refresh_attempts
                )
                dispose("refresh failed after " .. cfg.max_refresh_attempts .. " attempts")
            end
        end

        local function send(list)
            if disposed then
                return
            end
            if not list then
                on_dispatched(false)
                return
            end
            projects = list
            on_dispatched(notify_projects_update(client, list))
        end

        if projects then
            send(projects)
        else
            request_projects(client, bufnr, send)
        end
    end

    --- Start a refresh cycle for a reason (verification hit or recovery).
    ---@param reason string
    local function start_refresh(reason)
        cycle_reason = reason
        vim.notify("🏄 JDTLS: " .. reason .. " — refreshing project config")
        dispatch_refresh()
    end

    --- After the initial build barrier: recovery forces the refresh, otherwise verify first.
    ---@param status string|nil
    local function verify_initial(status)
        last_build_status = status
        set_state("verifying")
        after_diag_quiet(function()
            if disposed then
                return
            end
            if recovery_reason then
                start_refresh("recovery (" .. recovery_reason .. ")")
                return
            end
            M.find_refresh_reason(client, { bufnr = bufnr, build_status = status }, function(reason, list)
                if disposed then
                    return
                end
                projects = list or projects
                if reason then
                    start_refresh(reason)
                else
                    finish_healthy("initial build: " .. tostring(status))
                end
            end)
        end)
    end

    on_timer = vim.schedule_wrap(function()
        if disposed then
            return
        end
        -- A $/progress queued between timer fire and this scheduled callback
        -- re-armed the timer; honor the newer deadline.
        if timer:is_active() then
            return
        end
        if client_gone() then
            dispose("client stopped")
            return
        end
        local purpose = timer_purpose
        timer_purpose = nil

        if purpose == "settle" and state == "settling" then
            local waited = vim.uv.now() - (settle_started or vim.uv.now())
            if startup_done or waited >= cfg.settle_max_ms then
                if not startup_done then
                    log.fmt_warn(
                        "client_id=%d no 'Register Watchers' signal within %dms after ServiceReady — building anyway",
                        client.id,
                        cfg.settle_max_ms
                    )
                end
                build_workspace(false, "initial build barrier", verify_initial)
            else
                -- quiet, but jdtls has not finished its startup build jobs yet: an explicit build now
                -- would interrupt them (Eclipse yields the autobuild to other operations)
                log.fmt_debug("client_id=%d quiet but startup jobs still running — waiting", client.id)
                arm(cfg.idle_ms, "settle")
            end
        elseif purpose == "recheck" and state == "healthy_recheck" then
            set_state("verifying")
            after_diag_quiet(function()
                if disposed then
                    return
                end
                M.find_refresh_reason(
                    client,
                    { bufnr = bufnr, build_status = last_build_status, projects = projects },
                    function(reason, list)
                        if disposed then
                            return
                        end
                        projects = list or projects
                        if reason then
                            log.fmt_warn("client_id=%d healthy re-check found: %s", client.id, reason)
                            start_refresh(reason)
                        else
                            dispose("workspace verified healthy (re-check)")
                        end
                    end
                )
            end)
        elseif purpose == "retry" and state == "retry_wait" then
            dispatch_refresh()
        elseif purpose == "update_event" and state == "awaiting_update" then
            log.fmt_warn(
                "client_id=%d no ProjectStatus signal within %dms after the refresh — building anyway",
                client.id,
                cfg.update_event_timeout_ms
            )
            run_build_after_refresh("refresh applied (no signal, timeout)")
        elseif purpose == "classpath_grace" and state == "awaiting_update" then
            log.fmt_info(
                "client_id=%d no ProjectStatus within %dms after ClasspathUpdated — building",
                client.id,
                cfg.classpath_event_grace_ms
            )
            run_build_after_refresh("refresh applied (ClasspathUpdated, no ProjectStatus)")
        elseif purpose == "build" and pending_build then
            log.fmt_warn("client_id=%d workspace build did not answer within %dms", client.id, cfg.build_timeout_ms)
            local build = pending_build
            pending_build = nil
            if build.finish then
                build.finish()
            end
            pcall(function()
                client:cancel_request(build.req_id)
            end)
            build.cb(nil)
        elseif purpose == "watch" and state == "watching" then
            dispose("diagnostic watch window expired clean")
        end
    end)

    --- The config update job has ended (ProjectStatus): build and verify.
    ---@param signal string
    local function on_update_applied(signal)
        if disposed or state ~= "awaiting_update" then
            return
        end
        timer:stop()
        log.fmt_info("client_id=%d %s — config refresh applied, building", client.id, signal)
        run_build_after_refresh("refresh applied (" .. signal .. ")")
    end

    --- ClasspathUpdated arrived while a refresh is pending: the update job is still running, so keep
    --- waiting for ProjectStatus but cap the wait at `classpath_event_grace_ms`.
    local function on_classpath_updated()
        if disposed or state ~= "awaiting_update" or timer_purpose == "classpath_grace" then
            return
        end
        log.fmt_info(
            "client_id=%d ClasspathUpdated during the refresh — waiting up to %dms for ProjectStatus",
            client.id,
            cfg.classpath_event_grace_ms
        )
        arm(cfg.classpath_event_grace_ms, "classpath_grace")
    end

    --- Return a dispatcher for the handler that was in place before this watcher wrapped `method`:
    --- the client's own handler if it had one, otherwise whatever `vim.lsp.handlers[method]` holds
    --- at call time (global overrides such as utils.lang.java.lsp-java-handlers' publishDiagnostics
    --- may be installed after this setup and must still receive the messages).
    ---@param method string
    ---@return fun(err: any, result: any, ctx: any): any
    local function resolve_handler(method)
        local own = client.handlers[method]
        return function(err, result, ctx)
            local h = own or vim.lsp.handlers[method]
            if h then
                return h(err, result, ctx)
            end
        end
    end

    --- jdtls finished its startup build jobs: the settle may end at the next quiet moment.
    ---@param signal string
    local function on_startup_jobs_done(signal)
        if startup_done then
            return
        end
        startup_done = true
        log.fmt_info("client_id=%d %s — startup build jobs done", client.id, signal)
        if state == "settling" then
            arm(cfg.idle_ms, "settle")
        end
    end

    local prev_status = resolve_handler("language/status")
    client.handlers["language/status"] = function(err, result, ctx)
        log.fmt_debug("STATUS type=%s message=%s", result and result.type or "nil", result and result.message or "nil")
        if prev_status then
            prev_status(err, result, ctx)
        end
        if disposed or not result then
            return
        end
        if state == "waiting_ready" and result.type == "ServiceReady" then
            set_state("settling")
            settle_started = vim.uv.now()
            log.info("ServiceReady — starting idle debounce") -- INFO #1 (per session)
            arm(cfg.idle_ms, "settle")
        elseif state == "awaiting_update" and result.type == "ProjectStatus" then
            on_update_applied("ProjectStatus " .. tostring(result.message))
        end
    end

    local prev_progress = resolve_handler("$/progress")
    client.handlers["$/progress"] = function(err, result, ctx)
        local val = result and result.value or {}
        log.fmt_debug(
            "PROGRESS kind=%s title=%s message=%s percentage=%s",
            val.kind or "nil",
            val.title or "nil",
            val.message or "nil",
            val.percentage and tostring(val.percentage) or "nil"
        )
        if prev_progress then
            prev_progress(err, result, ctx)
        end
        if disposed then
            return
        end
        -- jdtls' "Register Watchers" job runs after JobHelpers.waitForBuildJobs(): its end marks
        -- the end of the startup build jobs (traced 2026-10-28).
        if val.kind == "end" and (val.message == "Register Watchers" or val.title == "Register Watchers") then
            on_startup_jobs_done("'Register Watchers' finished")
        end
        -- The settle is time based on top of that signal: wait until the post-startup burst
        -- (Publish Diagnostics, Searching..., Background task) goes quiet before the build barrier.
        if state == "settling" then
            arm(cfg.idle_ms, "settle")
        end
    end

    -- Same signal without progress reports: jdtls registers the didChangeWatchedFiles watchers
    -- right after its startup build jobs.
    local prev_register = resolve_handler("client/registerCapability")
    client.handlers["client/registerCapability"] = function(err, result, ctx)
        if not disposed and result and type(result.registrations) == "table" then
            for _, reg in ipairs(result.registrations) do
                if reg.method == "workspace/didChangeWatchedFiles" then
                    on_startup_jobs_done("didChangeWatchedFiles registered")
                    break
                end
            end
        end
        if prev_register then
            return prev_register(err, result, ctx)
        end
        return vim.NIL
    end

    local prev_publish = resolve_handler("textDocument/publishDiagnostics")
    client.handlers["textDocument/publishDiagnostics"] = function(err, result, ctx)
        last_publish = vim.uv.now()
        if prev_publish then
            return prev_publish(err, result, ctx)
        end
    end

    local prev_event = resolve_handler("language/eventNotification")
    client.handlers["language/eventNotification"] = function(err, result, ctx)
        log.fmt_debug("EVENT type=%s", result and tostring(result.eventType) or "nil")
        if prev_event then
            prev_event(err, result, ctx)
        end
        if result and result.eventType == EVENT_CLASSPATH_UPDATED then
            on_classpath_updated()
        end
    end

    -- jdtls restart handling: when this client stops, close the timer and
    -- remove the autocmd so nothing leaks; the restarted client gets a fresh
    -- watcher via its own LspAttach.
    detach_autocmd = vim.api.nvim_create_autocmd("LspDetach", {
        callback = function(args)
            if args.data.client_id ~= client.id then
                return
            end
            -- Fires per buffer; dispose only when the client itself is going
            -- away, so buffer churn in a live session keeps the watcher alive.
            vim.schedule(function()
                if not disposed and client_gone() then
                    dispose("client stopped (LspDetach)")
                end
            end)
        end,
    })
end

return M
