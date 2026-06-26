local helper = require("tests.utils.spec_helper")

describe("utils.envs-util", function()
    local envs_util
    local files

    before_each(function()
        helper.reset_vim()
        files = {}
        helper.stub_module("lib.file", {
            read_file = function(path)
                return files[path]
            end,
        })
        envs_util = helper.reload("utils.envs-util")
    end)

    after_each(function()
        helper.clear_stub_modules({ "utils.envs-util", "lib.file" })
    end)

    it("parses key-value pairs while ignoring comments and empty lines", function()
        -- given
        files["/tmp/.env"] = table.concat({
            "# comment",
            "",
            " DB_HOST = localhost ",
            "TOKEN=abc=123",
            "EMPTY=",
        }, "\n")

        -- when
        local envs = envs_util.load_env_file("/tmp/.env")

        -- then
        assert.are.same({
            DB_HOST = "localhost",
            TOKEN = "abc=123",
            EMPTY = "",
        }, envs)
    end)

    it("returns an empty table when the env file cannot be read", function()
        -- given
        files["/tmp/missing.env"] = nil

        -- when
        local envs = envs_util.load_env_file("/tmp/missing.env")

        -- then
        assert.are.same({}, envs)
    end)

    it("loads a single variable from an env file", function()
        -- given
        files["/tmp/.env"] = "A=one\nB=two"

        -- when
        local value = envs_util.load_env_file_variable("/tmp/.env", "B")

        -- then
        assert.are.equal("two", value)
    end)

    it("loads private variables from HOME/private.env", function()
        -- given
        local private_path = os.getenv("HOME") .. "/private.env"
        files[private_path] = "SECRET=value"

        -- when
        local value = envs_util.load_private_var("SECRET")

        -- then
        assert.are.equal("value", value)
    end)
end)
