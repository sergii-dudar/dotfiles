local helper = require("tests.utils.spec_helper")

describe("utils.java.jdtls-actionable-notification", function()
    local M
    local client, sent, select_calls, choice, logged

    local BUILD_FILE_MESSAGE = "A build file was modified. Do you want to synchronize the Java classpath/configuration?"
    local POM = { uri = "file:///repo/module/pom.xml" }

    --- The notification jdtls 1.61 sends after a build file changed (StandardProjectsManager); `status` encodes the
    --- FeatureStatus either as the enum name (gson default) or as the ordinal (vscode-java's declaration).
    ---@param as_ordinal boolean
    local function build_file_notification(as_ordinal)
        local function status(name, ordinal)
            return as_ordinal and ordinal or name
        end
        return {
            severity = 3,
            message = BUILD_FILE_MESSAGE,
            commands = {
                {
                    title = "Yes",
                    command = "java.projectConfiguration.status",
                    arguments = { POM, status("interactive", 1) },
                },
                {
                    title = "Always",
                    command = "java.projectConfiguration.status",
                    arguments = { POM, status("automatic", 2) },
                },
                {
                    title = "Never",
                    command = "java.projectConfiguration.status",
                    arguments = { POM, status("disabled", 0) },
                },
            },
        }
    end

    local function notifications_of(method)
        local out = {}
        for _, call in ipairs(sent) do
            if call.method == method then
                table.insert(out, call)
            end
        end
        return out
    end

    before_each(function()
        helper.reset_vim()
        logged = {}
        helper.stub_module("utils.logging-util", {
            new = function()
                local function record(level)
                    return function(fmt, ...)
                        table.insert(logged, level .. ": " .. string.format(fmt, ...))
                    end
                end
                return {
                    fmt_info = record("info"),
                    fmt_warn = record("warn"),
                    fmt_error = record("error"),
                    info = record("info"),
                    warn = record("warn"),
                    error = record("error"),
                }
            end,
        })

        sent = {}
        client = {
            id = 7,
            name = "jdtls",
            settings = {
                java = {
                    configuration = { updateBuildConfiguration = "interactive" },
                    compile = { nullAnalysis = { mode = "disabled" } },
                    format = { settings = { url = "file:///formatter.xml" } },
                },
            },
            config = {},
            notify = function(_, method, params)
                table.insert(sent, { method = method, params = params })
                return true
            end,
            request = function(_, method, params, callback)
                table.insert(sent, { method = method, params = params, callback = callback })
                return true, #sent
            end,
            is_stopped = function()
                return false
            end,
        }
        client.config.settings = client.settings

        vim.lsp.commands = {}
        vim.lsp.get_client_by_id = function(id)
            if id == client.id then
                return client
            end
        end
        vim.lsp.get_buffers_by_client_id = function()
            return { 3 }
        end
        vim.fn.mode = function()
            return "n"
        end
        vim.api.nvim_create_autocmd = function()
            error("no autocmd expected in normal mode")
        end
        select_calls = {}
        choice = nil
        vim.ui = {
            select = function(items, opts, on_choice)
                table.insert(select_calls, { items = items, prompt = opts.prompt })
                if choice then
                    on_choice(items[choice], choice)
                else
                    on_choice(nil, nil)
                end
            end,
        }

        M = helper.reload("utils.java.jdtls-actionable-notification")
        client.commands = M.client_commands
    end)

    after_each(function()
        M._reset()
        helper.clear_stub_modules({ "utils.logging-util", "utils.java.jdtls-actionable-notification" })
    end)

    it("shows the message with the command titles as choices", function()
        M.handler(nil, build_file_notification(false), { client_id = 7 })

        assert.are.equal(1, #select_calls)
        assert.are.same({ "Yes", "Always", "Never" }, select_calls[1].items)
        assert.are.equal("JDTLS: " .. BUILD_FILE_MESSAGE, select_calls[1].prompt)
    end)

    it("'Yes' updates the build file's project once and changes no setting", function()
        choice = 1
        M.handler(nil, build_file_notification(false), { client_id = 7 })

        local updates = notifications_of("java/projectConfigurationsUpdate")
        assert.are.equal(1, #updates)
        assert.are.same({ identifiers = { { uri = POM.uri } } }, updates[1].params)
        assert.are.equal(0, #notifications_of("workspace/didChangeConfiguration"))
        assert.are.equal("interactive", client.settings.java.configuration.updateBuildConfiguration)
    end)

    it("'Always' switches the running jdtls to automatic with the full settings table, then updates", function()
        choice = 2
        M.handler(nil, build_file_notification(false), { client_id = 7 })

        local changes = notifications_of("workspace/didChangeConfiguration")
        assert.are.equal(1, #changes)
        local java = changes[1].params.settings.java
        assert.are.equal("automatic", java.configuration.updateBuildConfiguration)
        -- the rest of the settings travel along: jdtls rebuilds all preferences from this object
        assert.are.equal("file:///formatter.xml", java.format.settings.url)
        assert.are.equal("disabled", java.compile.nullAnalysis.mode)
        assert.are.equal("automatic", client.settings.java.configuration.updateBuildConfiguration)
        assert.are.equal(client.settings, client.config.settings)
        assert.are.equal(1, #notifications_of("java/projectConfigurationsUpdate"))
        assert.are.equal(1, #vim._test_state.notifications)
        assert.truthy(vim._test_state.notifications[1].message:find("updateBuildConfiguration", 1, true))
    end)

    it("'Never' disables the sync for the session and does not update", function()
        choice = 3
        M.handler(nil, build_file_notification(false), { client_id = 7 })

        assert.are.equal("disabled", client.settings.java.configuration.updateBuildConfiguration)
        assert.are.equal(1, #notifications_of("workspace/didChangeConfiguration"))
        assert.are.equal(0, #notifications_of("java/projectConfigurationsUpdate"))
    end)

    it("accepts the FeatureStatus as ordinal too", function()
        choice = 2
        M.handler(nil, build_file_notification(true), { client_id = 7 })

        assert.are.equal("automatic", client.settings.java.configuration.updateBuildConfiguration)
        assert.are.equal(1, #notifications_of("java/projectConfigurationsUpdate"))
    end)

    it("dismissing the prompt runs nothing and allows the next prompt", function()
        choice = nil
        M.handler(nil, build_file_notification(false), { client_id = 7 })
        assert.are.equal(0, #sent)

        choice = 1
        M.handler(nil, build_file_notification(false), { client_id = 7 })
        assert.are.equal(2, #select_calls)
        assert.are.equal(1, #notifications_of("java/projectConfigurationsUpdate"))
    end)

    it("shows a repeated message once while its prompt is open, one prompt at a time", function()
        local pending_choice
        vim.ui.select = function(items, _, on_choice)
            table.insert(select_calls, { items = items })
            pending_choice = on_choice -- keep the prompt open
        end
        M.handler(nil, build_file_notification(false), { client_id = 7 })
        M.handler(nil, build_file_notification(false), { client_id = 7 })
        local other = {
            severity = 3,
            message = "Null annotation types have been detected in the project. Do you wish to enable null analysis for this project?",
            commands = {
                { title = "Enable", command = "java.compile.nullAnalysis.setMode", arguments = { "automatic" } },
                { title = "Disable", command = "java.compile.nullAnalysis.setMode", arguments = { "disabled" } },
            },
        }
        M.handler(nil, other, { client_id = 7 })
        assert.are.equal(1, #select_calls) -- duplicate dropped, second message queued behind the open prompt

        pending_choice("Yes", 1) -- answer the first prompt
        assert.are.equal(2, #select_calls)
        assert.are.same({ "Enable", "Disable" }, select_calls[2].items)

        pending_choice("Enable", 1)
        assert.are.equal("automatic", client.settings.java.compile.nullAnalysis.mode)
    end)

    it("waits for normal mode before prompting", function()
        local mode_callback
        vim.fn.mode = function()
            return "i"
        end
        vim.api.nvim_create_autocmd = function(event, opts)
            assert.are.equal("ModeChanged", event)
            assert.is_true(opts.once)
            mode_callback = opts.callback
        end
        choice = 1
        M.handler(nil, build_file_notification(false), { client_id = 7 })
        assert.are.equal(0, #select_calls)

        -- the test double runs vim.schedule callbacks immediately
        mode_callback()
        assert.are.equal(1, #vim._test_state.schedules)
        assert.are.equal(1, #select_calls)
        assert.are.equal(1, #notifications_of("java/projectConfigurationsUpdate"))
    end)

    it("logs log-severity notifications and shows command-less ones as plain notifications", function()
        M.handler(nil, { severity = 4, message = "just logging" }, { client_id = 7 })
        assert.are.equal(0, #select_calls)
        assert.are.equal(0, #vim._test_state.notifications)

        M.handler(nil, { severity = 2, message = "Something to know" }, { client_id = 7 })
        assert.are.equal(0, #select_calls)
        assert.are.equal(1, #vim._test_state.notifications)
        assert.are.equal("JDTLS: Something to know", vim._test_state.notifications[1].message)
        assert.are.equal(vim.log.levels.WARN, vim._test_state.notifications[1].level)
    end)

    it("falls back to a server command for unknown command ids", function()
        choice = 1
        M.handler(nil, {
            severity = 3,
            message = "Gradle wrapper is outdated",
            commands = {
                { title = "Upgrade", command = "java.project.upgradeGradle", arguments = { "file:///repo", "8.9" } },
            },
        }, { client_id = 7 })

        local calls = notifications_of("workspace/executeCommand")
        assert.are.equal(1, #calls)
        assert.are.same(
            { command = "java.project.upgradeGradle", arguments = { "file:///repo", "8.9" } },
            calls[1].params
        )
        calls[1].callback({ message = "boom" })
        assert.are.equal(1, #vim._test_state.notifications)
        assert.truthy(vim._test_state.notifications[1].message:find("boom", 1, true))
    end)

    it("reports unusable arguments instead of acting", function()
        choice = 1
        M.handler(nil, {
            severity = 3,
            message = BUILD_FILE_MESSAGE,
            commands = { { title = "Yes", command = "java.projectConfiguration.status", arguments = { POM, "later" } } },
        }, { client_id = 7 })

        assert.are.equal(0, #sent)
        assert.truthy(logged[#logged]:find("unusable arguments", 1, true))
    end)

    it("does nothing when the client is gone", function()
        vim.lsp.get_client_by_id = function()
            return nil
        end
        choice = 1
        M.handler(nil, build_file_notification(false), { client_id = 7 })

        assert.are.equal(0, #sent)
        assert.are.equal(1, #vim._test_state.notifications)
        assert.truthy(vim._test_state.notifications[1].message:find("client is gone", 1, true))
    end)
end)
