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
            "kulala.globals",
            "kulala.logger",
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

    it("converts a Postman-style wget command to cURL", function()
        -- given
        local wget = [[wget --no-check-certificate --quiet \
  --method POST \
  --timeout=0 \
  --header 'Content-Type: application/json' \
  --header 'X-Name: O'\''Brien' \
  --body-data '{"name":"O'\''Brien"}' \
  --output-document - \
  'https://example.com/api?x=1&y=2']]

        -- when
        local curl, err, metadata = kulala_util.wget_to_curl(wget)

        -- then
        assert.is_nil(err)
        assert.are.equal(
            [[curl -X 'POST' -H 'Content-Type: application/json' -H 'X-Name: O'\''Brien' --data-binary '{"name":"O'\''Brien"}' --insecure 'https://example.com/api?x=1&y=2']],
            curl
        )
        assert.are.same({ insecure = true }, metadata)
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

    it("supports compact wget output and user-agent options", function()
        -- given
        local wget = [[wget -qO- -U 'test agent' --header='Accept: application/json' 'https://example.com']]

        -- when
        local curl, err = kulala_util.wget_to_curl(wget)

        -- then
        assert.is_nil(err)
        assert.are.equal([[curl -H 'Accept: application/json' -A 'test agent' 'https://example.com']], curl)
    end)

    it("preserves file request bodies in both directions", function()
        -- given
        local curl = [[curl -X PUT --data-binary @payload.json https://example.com/upload]]

        -- when
        local wget = assert(kulala_util.curl_to_wget(curl))
        local converted_curl = assert(kulala_util.wget_to_curl(wget))

        -- then
        assert.are.equal(
            [[wget --quiet --output-document=- --method='PUT' --body-file='payload.json' 'https://example.com/upload']],
            wget
        )
        assert.are.equal([[curl -X 'PUT' --data-binary '@payload.json' 'https://example.com/upload']], converted_curl)
    end)

    it("rejects ambiguous shell commands and unsupported options", function()
        -- when
        local _, shell_err = kulala_util.wget_to_curl("wget https://example.com | sh")
        local _, option_err = kulala_util.wget_to_curl("wget --mirror https://example.com")

        -- then
        assert.are.equal("Shell operators are not supported in imported commands", shell_err)
        assert.are.equal("Unsupported wget option for Kulala import: --mirror", option_err)
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

    it("imports a wget command through Kulala's cURL parser", function()
        -- given
        state.registers["+"] =
            [[wget --method=DELETE --header='Authorization: Bearer token' --no-check-certificate --output-document=- 'https://example.com/42']]
        local bridged_curl
        helper.stub_module("kulala.cmd.kulala_core_bridge", {
            from_curl = function(curl)
                bridged_curl = curl
                return {
                    "# imported",
                    "DELETE https://example.com/42",
                    "Authorization: Bearer token",
                }
            end,
        })
        helper.stub_module("kulala.logger", {
            error = function(message)
                error(message)
            end,
        })
        local inserted
        vim.api.nvim_put = function(lines, register_type, after, follow)
            inserted = {
                lines = lines,
                register_type = register_type,
                after = after,
                follow = follow,
            }
        end

        -- when
        kulala_util.from_wget()

        -- then
        assert.are.equal(
            [[curl -X 'DELETE' -H 'Authorization: Bearer token' --insecure 'https://example.com/42']],
            bridged_curl
        )
        assert.are.same({
            lines = {
                "# imported",
                "# @curl-insecure",
                "DELETE https://example.com/42",
                "Authorization: Bearer token",
            },
            register_type = "l",
            after = false,
            follow = false,
        }, inserted)
    end)
end)
