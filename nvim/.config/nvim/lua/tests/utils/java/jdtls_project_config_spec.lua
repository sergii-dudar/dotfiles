local helper = require("tests.utils.spec_helper")

describe("utils.java.jdtls-project-config", function()
    local M
    local names, current, attached, all_clients, realpaths, commands, fail_save, resets

    local JAVA, POM, NO_FILE, FOREIGN_POM, NESTED_POM = 1, 2, 3, 4, 5

    --- Fake jdtls client recording its notifications.
    ---@param id integer
    ---@param root string
    local function make_client(id, root)
        local client = { id = id, name = "jdtls", root_dir = root, sent = {}, stopped = false }
        function client:notify(method, params)
            table.insert(self.sent, { method = method, params = params })
            return true
        end
        function client:is_stopped()
            return self.stopped
        end
        return client
    end

    before_each(function()
        helper.reset_vim()
        names = {
            [JAVA] = "/repo/svc/src/main/java/A.java",
            [POM] = "/repo/svc/pom.xml",
            [NO_FILE] = "",
            [FOREIGN_POM] = "/elsewhere/pom.xml",
            [NESTED_POM] = "/repo/svc/module/pom.xml",
        }
        current = JAVA
        attached, all_clients, realpaths, commands = {}, {}, {}, {}
        fail_save = false
        resets = 0

        vim.api.nvim_buf_get_name = function(bufnr)
            return names[bufnr] or ""
        end
        vim.api.nvim_get_current_buf = function()
            return current
        end
        vim.api.nvim_buf_call = function(_, fn)
            return fn()
        end
        vim.cmd = function(command)
            table.insert(commands, command)
            if fail_save then
                error("E45: 'readonly' option is set")
            end
        end
        vim.uv = {
            fs_realpath = function(path)
                return realpaths[path] or path
            end,
        }
        vim.fs = vim.fs or {}
        vim.fs.basename = function(path)
            return path:match("([^/]+)$") or path
        end
        vim.uri_from_bufnr = function(bufnr)
            return "file://" .. names[bufnr]
        end
        helper.stub_module("utils.lsp-util", {
            get_client_by_name = function(_, opts)
                return opts and attached[opts.bufnr] or nil
            end,
            get_clients_by_name = function()
                return all_clients
            end,
        })
        package.loaded["modules.java.dependencies-search"] = {
            reset = function()
                resets = resets + 1
            end,
        }
        M = helper.reload("utils.java.jdtls-project-config")
    end)

    after_each(function()
        package.loaded["modules.java.dependencies-search"] = nil
        helper.clear_stub_modules({ "utils.lsp-util", "utils.java.jdtls-project-config" })
    end)

    it("recognises Maven and Gradle build files", function()
        assert.is_true(M.is_build_file("/repo/svc/pom.xml"))
        assert.is_true(M.is_build_file("/repo/app/build.gradle.kts"))
        assert.is_false(M.is_build_file("/repo/svc/src/main/java/A.java"))
        assert.is_false(M.is_build_file("/repo/svc/pom.xml.bak"))
    end)

    it("uses the client attached to a Java buffer", function()
        local client = make_client(7, "/repo/svc")
        attached[JAVA] = client
        all_clients = { make_client(8, "/repo"), client }

        assert.is_true(M.update(JAVA))
        assert.are.same({
            {
                method = "java/projectConfigurationsUpdate",
                params = { identifiers = { { uri = "file:///repo/svc/src/main/java/A.java" } } },
            },
        }, client.sent)
        assert.are.same({}, commands) -- a Java buffer is never written
        assert.are.equal(1, resets)
        assert.truthy(vim._test_state.notifications[1].message:find("A.java", 1, true))
    end)

    it("finds the client of a build file through the workspace root, deepest root first", function()
        local outer, inner, other = make_client(1, "/repo"), make_client(2, "/repo/svc/"), make_client(3, "/repo/svc2")
        all_clients = { outer, inner, other }
        current = POM

        assert.is_true(M.update(0))
        assert.are.equal(0, #outer.sent)
        assert.are.equal(0, #other.sent)
        assert.are.same({ identifiers = { { uri = "file:///repo/svc/pom.xml" } } }, inner.sent[1].params)

        assert.are.equal(inner, M.find_client(NESTED_POM))
    end)

    it("matches a symlinked path against the resolved root", function()
        realpaths["/repo/svc/pom.xml"] = "/real/svc/pom.xml"
        local client = make_client(1, "/real/svc")
        all_clients = { client }
        assert.are.equal(client, M.find_client(POM))
    end)

    it("writes an unsaved build file before the update, never an unmodified one", function()
        local client = make_client(1, "/repo/svc")
        all_clients = { client }

        vim.bo[POM].modified = false
        assert.is_true(M.update(POM))
        assert.are.same({}, commands)

        vim.bo[POM].modified = true
        assert.is_true(M.update(POM))
        assert.are.same({ "silent update" }, commands)
        assert.are.equal(2, #client.sent)
    end)

    it("sends nothing when the build file cannot be saved", function()
        local client = make_client(1, "/repo/svc")
        all_clients = { client }
        vim.bo[POM].modified = true
        fail_save = true

        assert.is_false(M.update(POM))
        assert.are.equal(0, #client.sent)
        assert.are.equal(vim.log.levels.ERROR, vim._test_state.notifications[1].level)
    end)

    it("warns and sends nothing without a file, a matching client or a live client", function()
        all_clients = { make_client(1, "/repo/svc") }

        assert.is_false(M.update(NO_FILE))
        assert.is_false(M.update(FOREIGN_POM)) -- a jdtls of another project must not be used
        all_clients[1].stopped = true
        assert.is_false(M.update(POM))

        assert.are.equal(0, #all_clients[1].sent)
        assert.are.equal(3, #vim._test_state.notifications)
        assert.are.equal(0, resets)
    end)

    it("does not treat a sibling directory with a common prefix as inside the root", function()
        names[POM] = "/repo/svc-api/pom.xml"
        all_clients = { make_client(1, "/repo/svc") }
        assert.is_nil(M.find_client(POM))
    end)
end)
