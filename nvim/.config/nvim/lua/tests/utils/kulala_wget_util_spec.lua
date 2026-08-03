local helper = require("tests.utils.spec_helper")

describe("utils.kulala-wget-util", function()
    local kulala_util
    local state

    before_each(function()
        _, state = helper.reset_vim()
        kulala_util = helper.reload("utils.kulala-wget-util")
    end)

    after_each(function()
        helper.clear_stub_modules({
            "utils.kulala-wget-util",
            "kulala.cmd.kulala_core_bridge",
            "kulala.db",
            "kulala.globals",
            "kulala.logger",
            "kulala.parser.document",
            "kulala.parser.env",
            "kulala.parser.string_variables_parser",
        })
    end)

    it("preserves Kulala's resolved bearer header in the wget command", function()
        -- given
        local curl =
            [[curl -X 'POST' -v -s --data-binary '{"message":"it'\''s ready","count":2}' -H "Authorization: Bearer token" -H "Content-Type: application/json" -A 'kulala-core/0.30.0' --cookie 'session=abc' --http1.1 'https://example.com/api?x=1&y=2']]

        -- when
        local wget, err = kulala_util.curl_to_wget(curl)

        -- then
        assert.is_nil(err)
        assert.are.equal(
            [[wget --quiet --output-document=- --method='POST' --header='Authorization: Bearer token' --header='Content-Type: application/json' --header='Cookie: session=abc' --user-agent='kulala-core/0.30.0' --body-data='{"message":"it'\''s ready","count":2}' 'https://example.com/api?x=1&y=2']],
            wget
        )
    end)

    it("normalizes cURL no-value headers for wget", function()
        -- given
        local curl = [[curl -H 'Content-Type;:' --header='X-Empty;' -H 'Cookie: session=abc;' 'https://example.com']]

        -- when
        local wget, err = kulala_util.curl_to_wget(curl)

        -- then
        assert.is_nil(err)
        assert.are.equal(
            [[wget --quiet --output-document=- --header='Content-Type:' --header='X-Empty:' --header='Cookie: session=abc;' 'https://example.com']],
            wget
        )
    end)

    it("can force insecure mode when converting cURL to wget", function()
        -- given
        local curl = [[curl 'https://self-signed.example']]

        -- when
        local wget, err = kulala_util.curl_to_wget(curl, { insecure = true })

        -- then
        assert.is_nil(err)
        assert.are.equal(
            [[wget --quiet --output-document=- --no-check-certificate 'https://self-signed.example']],
            wget
        )
    end)

    it("preserves file request bodies", function()
        -- given
        local curl = [[curl -X PUT --data-binary @payload.json https://example.com/upload]]

        -- when
        local wget = assert(kulala_util.curl_to_wget(curl))

        -- then
        assert.are.equal(
            [[wget --quiet --output-document=- --method='PUT' --body-file='payload.json' 'https://example.com/upload']],
            wget
        )
    end)

    it("rejects ambiguous shell commands and unsupported cURL options", function()
        -- when
        local _, shell_err = kulala_util.curl_to_wget("curl https://example.com | sh")
        local _, option_err = kulala_util.curl_to_wget("curl --parallel https://example.com")

        -- then
        assert.are.equal("Shell operators are not supported in imported commands", shell_err)
        assert.are.equal("Unsupported cURL option for wget conversion: --parallel", option_err)
    end)

    it("copies the current Kulala request as wget", function()
        -- given
        state.buffer_lines[1] = {
            "### Shared",
            "# @curl-insecure",
            "NOP",
            "",
            "### request",
            "GET https://example.com",
        }
        state.cursor = { 6, 0 }
        local user_agent
        helper.stub_module("kulala.cmd.kulala_core_bridge", {
            enabled = function()
                return true
            end,
            to_curl_at_cursor = function(_, value)
                user_agent = value
                return [[curl -H 'Accept: application/json' -H 'Authorization: Bearer active-token' 'https://example.com']]
            end,
        })
        helper.stub_module("kulala.globals", {
            NAME = "kulala.nvim",
            VERSION = "7.0.0",
        })
        local info_message
        helper.stub_module("kulala.logger", {
            error = function(message)
                error(message)
            end,
            info = function(message)
                info_message = message
            end,
        })

        -- when
        kulala_util.copy_as_wget()

        -- then
        assert.are.equal("kulala.nvim/7.0.0", user_agent)
        assert.are.equal(
            [[wget --quiet --output-document=- --header='Accept: application/json' --header='Authorization: Bearer active-token' --no-check-certificate 'https://example.com']],
            state.registers["+"]
        )
        assert.are.equal("Copied wget command to clipboard", info_message)
    end)

    it("forces insecure mode when copying with the option enabled", function()
        -- given
        state.buffer_lines[1] = {
            "GET https://example.com",
        }
        helper.stub_module("kulala.cmd.kulala_core_bridge", {
            enabled = function()
                return true
            end,
            to_curl_at_cursor = function()
                return [[curl 'https://example.com']]
            end,
        })
        helper.stub_module("kulala.globals", {
            NAME = "kulala.nvim",
            VERSION = "7.0.0",
        })
        helper.stub_module("kulala.logger", {
            error = function(message)
                error(message)
            end,
            info = function() end,
        })

        -- when
        kulala_util.copy_as_wget({ insecure = true })

        -- then
        assert.are.equal(
            [[wget --quiet --output-document=- --no-check-certificate 'https://example.com']],
            state.registers["+"]
        )
    end)

    it("fills an empty bearer header from Kulala's active request", function()
        -- given
        state.buffer_lines[1] = {
            "GET https://example.com",
            [[Authorization: Bearer {{$auth.token("example")}}]],
        }
        helper.stub_module("kulala.cmd.kulala_core_bridge", {
            enabled = function()
                return true
            end,
            to_curl_at_cursor = function()
                return [[curl -H 'Authorization: Bearer ' 'https://example.com']]
            end,
        })
        helper.stub_module("kulala.globals", {
            NAME = "kulala.nvim",
            VERSION = "7.0.0",
        })
        helper.stub_module("kulala.db", {
            set_current_buffer = function() end,
        })
        helper.stub_module("kulala.parser.document", {
            get_document = function()
                return { {} }
            end,
            get_request_at = function()
                return {
                    {
                        variables = {},
                        headers = {
                            Authorization = [[Bearer {{$auth.token("example")}}]],
                        },
                    },
                }
            end,
        })
        helper.stub_module("kulala.parser.env", {
            get_env = function()
                return {}
            end,
        })
        helper.stub_module("kulala.parser.string_variables_parser", {
            parse = function()
                return "Bearer live-token"
            end,
        })
        helper.stub_module("kulala.logger", {
            error = function(message)
                error(message)
            end,
            info = function() end,
        })

        -- when
        kulala_util.copy_as_wget()

        -- then
        assert.are.equal(
            [[wget --quiet --output-document=- --header='Authorization: Bearer live-token' 'https://example.com']],
            state.registers["+"]
        )
    end)

    it("does not copy an empty bearer header when no active token is available", function()
        -- given
        state.buffer_lines[1] = {
            "GET https://example.com",
            [[Authorization: Bearer {{$auth.token("example")}}]],
        }
        helper.stub_module("kulala.cmd.kulala_core_bridge", {
            enabled = function()
                return true
            end,
            to_curl_at_cursor = function()
                return [[curl -H 'Authorization: Bearer ' 'https://example.com']]
            end,
        })
        helper.stub_module("kulala.globals", {
            NAME = "kulala.nvim",
            VERSION = "7.0.0",
        })
        helper.stub_module("kulala.db", {
            set_current_buffer = function() end,
        })
        helper.stub_module("kulala.parser.document", {
            get_document = function()
                return { {} }
            end,
            get_request_at = function()
                return {
                    {
                        variables = {},
                        headers = {
                            Authorization = [[Bearer {{$auth.token("example")}}]],
                        },
                    },
                }
            end,
        })
        helper.stub_module("kulala.parser.env", {
            get_env = function()
                return {}
            end,
        })
        helper.stub_module("kulala.parser.string_variables_parser", {
            parse = function()
                return "Bearer "
            end,
        })
        local error_message
        helper.stub_module("kulala.logger", {
            error = function(message)
                error_message = message
            end,
            info = function() end,
        })

        -- when
        kulala_util.copy_as_wget()

        -- then
        assert.are.equal(
            "Active bearer token is unavailable; acquire or refresh it in Kulala, then retry",
            error_message
        )
        assert.is_nil(state.registers["+"])
    end)
end)
