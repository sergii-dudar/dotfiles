-- Handler for jdtls' non-standard `language/actionableNotification`: a message with buttons, each button an LSP
-- command. Neither Neovim nor nvim-jdtls implement the method, so the client silently dropped every such prompt.
-- The one that matters here is "A build file was modified. Do you want to synchronize the Java
-- classpath/configuration?", sent after a pom.xml change while `java.configuration.updateBuildConfiguration`
-- is `interactive` (the default): a dependency added to a pom never reached the classpath until <leader>jdu or a
-- restart (seen 2026-10-28 with pom edits made by external tools).
-- Since 2026-11-06 the config sets `updateBuildConfiguration = "disabled"` (the prompt at every pom save was
-- more annoying than useful; <leader>jdu / utils.java.jdtls-project-config syncs on demand), so jdtls does not send
-- that prompt at all; this handler is what makes `interactive` usable again when the setting is changed back.
--
-- Mirrors vscode-java (src/standardLanguageClient.ts, ActionableNotification handler): show the message with the
-- command titles as choices and run the chosen command — a client command when one is registered
-- (`client.commands`, then `vim.lsp.commands`), otherwise a server command through `workspace/executeCommand`.
-- The client commands jdtls 1.61 expects (org.eclipse.jdt.ls.core StandardProjectsManager / BuildPathHandler):
--   java.projectConfiguration.status [TextDocumentIdentifier, FeatureStatus]  Yes / Always / Never
--   java.compile.nullAnalysis.setMode [FeatureStatus]                           Enable / Disable
-- "Always", "Never", "Enable" and "Disable" change the setting of the running jdtls only; vscode persists them in
-- settings.json, here the notification names the key in jdtls-config-util.lua that makes the choice permanent.
local M = {}

local logger = require("utils.logging-util").new({
    name = "jdtls-actionable",
    filename = "jdtls-actionable.log",
})

-- lsp4j MessageType values
local MESSAGE_TYPE_LOG = 4
local NOTIFY_LEVEL = { [1] = vim.log.levels.ERROR, [2] = vim.log.levels.WARN, [3] = vim.log.levels.INFO }

-- org.eclipse.jdt.ls.core.internal.preferences.Preferences.FeatureStatus: gson serialises the enum by name, the
-- vscode client declares it as ordinals; accept both.
local FEATURE_STATUS = {
    disabled = "disabled",
    interactive = "interactive",
    automatic = "automatic",
    [0] = "disabled",
    [1] = "interactive",
    [2] = "automatic",
}

-- one prompt at a time; a message already shown or queued is not queued again (jdtls repeats the build-file
-- notification for every save of the pom)
local queue = {}
local queued = {}
local showing = false

--- Normalise a FeatureStatus argument (enum name or ordinal) to its name.
---@param status any
---@return string|nil
local function feature_status(status)
    if type(status) == "string" then
        return FEATURE_STATUS[status]
    elseif type(status) == "number" then
        return FEATURE_STATUS[status]
    end
    return nil
end

--- Merge `patch` into the client's settings and push the complete table to jdtls.
--- jdtls rebuilds its Preferences from the whole `java` object of every didChangeConfiguration, so a partial table
--- would reset every other preference to its default; the full settings are sent every time.
---@param client vim.lsp.Client
---@param patch table
local function change_settings(client, patch)
    client.settings = vim.tbl_deep_extend("force", client.settings or {}, patch)
    if client.config then
        client.config.settings = client.settings
    end
    client:notify("workspace/didChangeConfiguration", { settings = client.settings })
end

--- Client commands jdtls puts on its actionable notifications; registered as `commands` of the jdtls client.
---@type table<string, fun(command: lsp.Command, ctx: table)>
M.client_commands = {
    -- Build-file prompt: Yes (interactive) updates the project of the build file once; Always (automatic) and
    -- Never (disabled) also change `java.configuration.updateBuildConfiguration` for this jdtls.
    ["java.projectConfiguration.status"] = function(command, ctx)
        local client = vim.lsp.get_client_by_id(ctx.client_id)
        local args = command.arguments or {}
        local identifier, mode = args[1], feature_status(args[2])
        local uri = type(identifier) == "table" and identifier.uri or identifier
        if not client or not mode then
            logger.fmt_warn("projectConfiguration.status: unusable arguments %s", vim.inspect(args))
            return
        end
        if mode ~= "interactive" then
            change_settings(client, { java = { configuration = { updateBuildConfiguration = mode } } })
            vim.notify(
                string.format(
                    "JDTLS: build-file sync is '%s' until this jdtls stops; set java.configuration.updateBuildConfiguration in jdtls-config-util.lua to keep it",
                    mode
                ),
                vim.log.levels.INFO
            )
        end
        if mode ~= "disabled" and type(uri) == "string" then
            -- the singular java/projectConfigurationUpdate is a @JsonNotification in jdtls that nvim-jdtls sends
            -- as a request (lsp4j then returns an error); the plural notification is what <leader>jdu sends too
            client:notify("java/projectConfigurationsUpdate", { identifiers = { { uri = uri } } })
            logger.fmt_info("project configuration update requested for %s (%s)", uri, mode)
            local deps = package.loaded["modules.java.dependencies-search"]
            if type(deps) == "table" and type(deps.reset) == "function" then
                deps.reset()
            end
        end
    end,
    -- Null-analysis prompt ("Null annotation types have been detected ..."): Enable (automatic) / Disable.
    ["java.compile.nullAnalysis.setMode"] = function(command, ctx)
        local client = vim.lsp.get_client_by_id(ctx.client_id)
        local mode = feature_status((command.arguments or {})[1])
        if not client or not mode then
            logger.fmt_warn("nullAnalysis.setMode: unusable arguments %s", vim.inspect(command.arguments))
            return
        end
        change_settings(client, { java = { compile = { nullAnalysis = { mode = mode } } } })
        vim.notify(
            string.format(
                "JDTLS: null analysis is '%s' until this jdtls stops; set java.compile.nullAnalysis.mode in jdtls-config-util.lua to keep it",
                mode
            ),
            vim.log.levels.INFO
        )
    end,
}

--- Run one command of a notification: a client command when registered, otherwise a server command.
---@param client_id integer
---@param command lsp.Command
function M.run_command(client_id, command)
    local client = vim.lsp.get_client_by_id(client_id)
    if not client or client:is_stopped() then
        vim.notify(
            string.format("JDTLS: client is gone, '%s' not run", command.title or command.command),
            vim.log.levels.WARN
        )
        return
    end
    local bufnr = vim.lsp.get_buffers_by_client_id(client.id)[1]
    local ctx = { client_id = client.id, bufnr = bufnr }
    local fn = (client.commands or {})[command.command] or vim.lsp.commands[command.command]
    if fn then
        local ok, err = pcall(fn, command, ctx)
        if not ok then
            logger.fmt_error("client command %s failed: %s", command.command, tostring(err))
            vim.notify(
                string.format("JDTLS: '%s' failed: %s", command.title or command.command, tostring(err)),
                vim.log.levels.ERROR
            )
        end
        return
    end
    client:request(
        "workspace/executeCommand",
        { command = command.command, arguments = command.arguments },
        function(err)
            if err then
                logger.fmt_error("server command %s failed: %s", command.command, vim.inspect(err))
                vim.notify(
                    string.format(
                        "JDTLS: '%s' failed: %s",
                        command.title or command.command,
                        err.message or vim.inspect(err)
                    ),
                    vim.log.levels.ERROR
                )
            end
        end,
        bufnr
    )
end

--- Run `fn` once the editor is in normal mode, so a prompt never opens over insert-mode typing or a command line.
---@param fn fun()
local function when_in_normal_mode(fn)
    if vim.fn.mode():sub(1, 1) == "n" then
        fn()
        return
    end
    vim.api.nvim_create_autocmd("ModeChanged", {
        pattern = "*:n*",
        once = true,
        callback = function()
            vim.schedule(fn)
        end,
    })
end

local show_next

--- Show one queued notification with `vim.ui.select` and run the chosen command.
---@param entry { message: string, commands: lsp.Command[], client_id: integer }
local function show(entry)
    local titles = {}
    for i, command in ipairs(entry.commands) do
        titles[i] = command.title or command.command or ("choice " .. i)
    end
    local finished = false
    local function finish(idx)
        if finished then
            return
        end
        finished = true
        queued[entry.message] = nil
        showing = false
        if idx then
            local command = entry.commands[idx]
            logger.fmt_info("chose '%s' (%s) for: %s", titles[idx], command.command, entry.message)
            M.run_command(entry.client_id, command)
        else
            logger.fmt_info("dismissed: %s", entry.message)
        end
        show_next()
    end
    local ok, err = pcall(vim.ui.select, titles, { prompt = "JDTLS: " .. entry.message }, function(_, idx)
        finish(idx)
    end)
    if not ok then
        logger.fmt_error("vim.ui.select failed: %s", tostring(err))
        finish(nil)
    end
end

show_next = function()
    if showing or #queue == 0 then
        return
    end
    showing = true
    local entry = table.remove(queue, 1)
    when_in_normal_mode(function()
        show(entry)
    end)
end

--- `language/actionableNotification` handler (vim.lsp.Handler signature).
---@param _ lsp.ResponseError|nil
---@param result { severity: integer, message: string, commands: lsp.Command[]|nil }
---@param ctx table
function M.handler(_, result, ctx)
    if type(result) ~= "table" or type(result.message) ~= "string" then
        return
    end
    local commands = result.commands or {}
    if result.severity == MESSAGE_TYPE_LOG then
        logger.fmt_info("log notification: %s", result.message)
        return
    end
    if #commands == 0 then
        vim.notify("JDTLS: " .. result.message, NOTIFY_LEVEL[result.severity] or vim.log.levels.INFO)
        return
    end
    if queued[result.message] then
        logger.fmt_info("already queued: %s", result.message)
        return
    end
    queued[result.message] = true
    table.insert(queue, { message = result.message, commands = commands, client_id = ctx and ctx.client_id })
    logger.fmt_info("queued (%d choice(s)): %s", #commands, result.message)
    show_next()
end

--- Test seam: drop queued prompts and the shown flag.
function M._reset()
    queue = {}
    queued = {}
    showing = false
end

return M
