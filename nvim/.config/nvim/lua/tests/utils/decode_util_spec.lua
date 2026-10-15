local helper = require("tests.utils.spec_helper")

local B64_ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

--- Strict decoder mirroring vim.base64.decode: standard alphabet, padded length, errors otherwise.
local function strict_base64_decode(input)
    if #input % 4 ~= 0 or input:find("[^A-Za-z0-9+/=]") then
        error("Invalid input")
    end

    local out = {}
    for index = 1, #input, 4 do
        local value, pad = 0, 0
        for offset = 0, 3 do
            local char = input:sub(index + offset, index + offset)
            if char == "=" then
                pad = pad + 1
                value = value * 64
            else
                if pad > 0 then
                    error("Invalid input")
                end
                value = value * 64 + (B64_ALPHABET:find(char, 1, true) - 1)
            end
        end
        table.insert(out, string.char(math.floor(value / 65536) % 256))
        if pad < 2 then
            table.insert(out, string.char(math.floor(value / 256) % 256))
        end
        if pad < 1 then
            table.insert(out, string.char(value % 256))
        end
    end
    return table.concat(out)
end

local JWT_HEADER = '{"alg":"HS256","typ":"JWT"}'
local JWT_PAYLOAD = '{"sub":"1234567890","name":"John Doe","iat":1516239022}'
local JWT_HEADER_B64 = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"
local JWT_PAYLOAD_B64 = "eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ"
local JWT_SIGNATURE = "SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"
local JWT = JWT_HEADER_B64 .. "." .. JWT_PAYLOAD_B64 .. "." .. JWT_SIGNATURE

local JSON_FIXTURES = {
    [JWT_HEADER] = { alg = "HS256", typ = "JWT" },
    [JWT_PAYLOAD] = { sub = "1234567890", name = "John Doe", iat = 1516239022 },
    ['{"a":1}'] = { a = 1 },
}

describe("utils.decode-util", function()
    local decode_util
    local state

    before_each(function()
        _, state = helper.reset_vim()
        vim.base64 = { decode = strict_base64_decode }
        vim.json.decode = function(text)
            local decoded = JSON_FIXTURES[text]
            if decoded == nil then
                error("invalid json: " .. tostring(text))
            end
            return decoded
        end
        helper.stub_module("utils.buffer-util", {
            open_scratch_split = function(bufnr, opts)
                state.scratch = { bufnr = bufnr, opts = opts }
            end,
        })
        decode_util = helper.reload("utils.decode-util")
    end)

    after_each(function()
        helper.clear_stub_modules({ "utils.decode-util", "utils.buffer-util", "utils.json.normalize" })
    end)

    describe("decode_base64", function()
        it("decodes padded, unpadded, line-wrapped and URL-safe input", function()
            assert.are.equal("hello", decode_util.decode_base64("aGVsbG8="))
            assert.are.equal("hello", decode_util.decode_base64("aGVsbG8"))
            assert.are.equal("hello", decode_util.decode_base64("  aGVs\n  bG8=\n"))
            assert.are.equal("\251\255", decode_util.decode_base64("-_8"))
            assert.are.equal("\251\255", decode_util.decode_base64("+/8="))
        end)

        it("returns nil for text that cannot be base64", function()
            assert.is_nil(decode_util.decode_base64("not base64!!"))
            assert.is_nil(decode_util.decode_base64("hello")) -- length % 4 == 1
            assert.is_nil(decode_util.decode_base64("a=b="))
            assert.is_nil(decode_util.decode_base64(""))
            assert.is_nil(decode_util.decode_base64("===="))
        end)
    end)

    describe("decode_jwt", function()
        it("splits a JWS into decoded header, payload and raw signature", function()
            -- when
            local result = decode_util.decode_jwt(JWT)

            -- then
            assert.are.equal("jwt", result.kind)
            assert.are.same({ alg = "HS256", typ = "JWT" }, result.header)
            assert.are.equal(JWT_HEADER, result.header_text)
            assert.are.same({ sub = "1234567890", name = "John Doe", iat = 1516239022 }, result.payload)
            assert.are.equal(JWT_PAYLOAD, result.payload_text)
            assert.are.equal(JWT_SIGNATURE, result.signature)
        end)

        it("accepts an unsigned token with an empty signature part", function()
            local result = decode_util.decode_jwt(JWT_HEADER_B64 .. "." .. JWT_PAYLOAD_B64 .. ".")

            assert.are.equal("jwt", result.kind)
            assert.are.equal("", result.signature)
        end)

        it("keeps a non-JSON payload as raw text", function()
            local result = decode_util.decode_jwt(JWT_HEADER_B64 .. ".aGVsbG8.sig")

            assert.is_nil(result.payload)
            assert.are.equal("hello", result.payload_text)
        end)

        it("recognises a five-part JWE and decodes only the header", function()
            local result = decode_util.decode_jwt(JWT_HEADER_B64 .. "..aXY.Y2lwaGVy.dGFn")

            assert.are.equal("jwe", result.kind)
            assert.are.same({ alg = "HS256", typ = "JWT" }, result.header)
            assert.is_nil(result.payload_text)
        end)

        it("returns nil when the header is not JSON or the shape is wrong", function()
            assert.is_nil(decode_util.decode_jwt("aGVsbG8.aGVsbG8.sig"))
            assert.is_nil(decode_util.decode_jwt(JWT_HEADER_B64))
            assert.is_nil(decode_util.decode_jwt(JWT_HEADER_B64 .. "." .. JWT_PAYLOAD_B64))
            assert.is_nil(decode_util.decode_jwt("a.b.c.d"))
        end)
    end)

    describe("decode", function()
        it("strips quotes and a Bearer prefix before classifying a JWT", function()
            assert.are.equal("jwt", decode_util.decode('  "Bearer ' .. JWT .. '"  ').kind)
            assert.are.equal("jwt", decode_util.decode("Bearer " .. JWT).kind)
            assert.are.equal("jwt", decode_util.decode("'" .. JWT .. "'").kind)
        end)

        it("falls back to base64 and ignores embedded whitespace", function()
            local result = decode_util.decode("aGVs\nbG8=")

            assert.are.equal("base64", result.kind)
            assert.are.equal("hello", result.decoded)
        end)

        it("returns nil for empty text and text that is neither", function()
            assert.is_nil(decode_util.decode(""))
            assert.is_nil(decode_util.decode('""'))
            assert.is_nil(decode_util.decode("plain words here"))
            assert.is_nil(decode_util.decode("aGVs bG8=")) -- spaces inside a line are not base64 wrapping
        end)
    end)

    describe("time_claim_lines", function()
        it("prints iat / nbf / exp in UTC with a relative label", function()
            local now = 1516239022
            local lines = decode_util.time_claim_lines({ iat = now - 90061, nbf = now, exp = now + 3661 }, now)

            assert.are.same({
                "// iat: 2018-01-17T00:29:21Z (1d 1h ago)",
                "// nbf: 2018-01-18T01:30:22Z (now)",
                "// exp: 2018-01-18T02:31:23Z (expires in 1h 1m)",
            }, lines)
        end)

        it("flags an expired token and accepts millisecond stamps", function()
            local now = 1516239022 + 172805
            local lines = decode_util.time_claim_lines({ exp = 1516239022000, name = "x" }, now)

            assert.are.same({ "// exp: 2018-01-18T01:30:22Z (EXPIRED 2d ago)" }, lines)
        end)

        it("ignores non-numeric claims", function()
            assert.are.same({}, decode_util.time_claim_lines({ exp = "soon" }, 0))
        end)
    end)

    describe("render", function()
        it("renders a JWT as commented header, payload, time claims and signature", function()
            local lines, filetype = decode_util.render(decode_util.decode(JWT), 1516239022 + 60)

            assert.are.equal("json5", filetype)
            assert.are.same({
                "// header",
                JWT_HEADER,
                "// payload",
                JWT_PAYLOAD,
                "// iat: 2018-01-18T01:30:22Z (1m ago)",
                "// signature (43 chars)",
                JWT_SIGNATURE,
            }, lines)
        end)

        it("marks an unsigned token", function()
            local lines = decode_util.render(decode_util.decode(JWT_HEADER_B64 .. "." .. JWT_PAYLOAD_B64 .. "."), 0)

            assert.are.equal("// signature (0 chars)", lines[#lines - 1])
            assert.are.equal("// (none, unsigned token)", lines[#lines])
        end)

        it("renders a JWE header with an encrypted-payload note", function()
            local lines, filetype = decode_util.render(decode_util.decode(JWT_HEADER_B64 .. "..aXY.Y2lwaGVy.dGFn"))

            assert.are.equal("json5", filetype)
            assert.are.same({
                "// header",
                JWT_HEADER,
                "// JWE: the payload is encrypted, only the header can be decoded",
            }, lines)
        end)

        it("formats base64 JSON with jq when available", function()
            vim.fn.executable = function(command)
                return command == "jq" and 1 or 0
            end
            vim.fn.system = function(command, input)
                assert.are.same({ "jq", "." }, command)
                assert.are.equal('{"a":1}', input)
                vim.v.shell_error = 0
                return '{\n  "a": 1\n}\n'
            end

            local lines, filetype = decode_util.render(decode_util.decode("eyJhIjoxfQ=="))

            assert.are.equal("json", filetype)
            assert.are.same({ "{", '  "a": 1', "}" }, lines)
        end)

        it("renders base64 text line by line", function()
            local lines, filetype = decode_util.render({ kind = "base64", decoded = "one\ntwo" })

            assert.are.equal("text", filetype)
            assert.are.same({ "one", "two" }, lines)
        end)

        it("renders binary base64 as an xxd hex dump", function()
            local lines, filetype = decode_util.render({ kind = "base64", decoded = "\251\255\0hi" })

            assert.are.equal("xxd", filetype)
            assert.are.same({ "00000000: fbff 0068 69" .. string.rep(" ", 27) .. "  ...hi" }, lines)
        end)

        it("treats invalid UTF-8 without control bytes as binary", function()
            local lines, filetype = decode_util.render({ kind = "base64", decoded = "\251\255" })

            assert.are.equal("xxd", filetype)
            assert.are.same({ "00000000: fbff" .. string.rep(" ", 35) .. "  .." }, lines)
        end)

        it("keeps valid UTF-8 text as text", function()
            local lines, filetype = decode_util.render({ kind = "base64", decoded = "h\195\169llo" })

            assert.are.equal("text", filetype)
            assert.are.same({ "h\195\169llo" }, lines)
        end)
    end)

    describe("decode_selection", function()
        it("decodes the selected quoted token into a named scratch split", function()
            -- given
            state.buffer_lines[0] = { 'token = "' .. JWT .. '"' }
            state.positions["'<"] = { 0, 1, 9, 0 }
            state.positions["'>"] = { 0, 1, 2147483647, 0 }
            vim.o.lines = 40

            -- when
            decode_util.decode_selection()

            -- then
            assert.are.equal(501, state.scratch.bufnr)
            assert.are.equal("json5", state.scratch.opts.filetype)
            assert.are.equal(20, state.scratch.opts.max_height)
            assert.are.equal("// header", state.buffer_lines[501][1])
            assert.are.equal(JWT_SIGNATURE, state.buffer_lines[501][#state.buffer_lines[501]])
            assert.are.same({}, state.notifications)
        end)

        it("joins a multi-line charwise selection before decoding", function()
            state.buffer_lines[0] = { "x aGVs", "bG8= y" }
            state.positions["'<"] = { 0, 1, 3, 0 }
            state.positions["'>"] = { 0, 2, 4, 0 }

            decode_util.decode_selection()

            assert.are.equal("text", state.scratch.opts.filetype)
            assert.are.same({ "hello" }, state.buffer_lines[501])
        end)

        it("warns and opens nothing when the selection is neither", function()
            state.buffer_lines[0] = { "just some words" }
            state.positions["'<"] = { 0, 1, 1, 0 }
            state.positions["'>"] = { 0, 1, 2147483647, 0 }

            decode_util.decode_selection()

            assert.is_nil(state.scratch)
            assert.are.equal(1, #state.notifications)
            assert.are.equal(vim.log.levels.WARN, state.notifications[1].level)
            assert.matches("neither a JWT nor base64", state.notifications[1].message)
        end)
    end)
end)
