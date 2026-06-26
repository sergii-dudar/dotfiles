local helper = require("tests.utils.spec_helper")
local tmp_dir = helper.tmp_dir

describe("utils.logging-util", function()
    local logging_util
    local original_os_date
    local log_dir = tmp_dir .. "nvim-logging-util-spec"

    local function read_file(path)
        local file = assert(io.open(path, "r"))
        local content = file:read("*a")
        file:close()
        return content
    end

    local function write_file(path, content)
        local file = assert(io.open(path, "w"))
        file:write(content)
        file:close()
    end

    before_each(function()
        helper.reset_vim()
        original_os_date = os.date
        os.execute("mkdir -p " .. log_dir)
        logging_util = helper.reload("utils.logging-util")
    end)

    after_each(function()
        os.date = original_os_date
        helper.clear_stub_modules("utils.logging-util")
    end)

    it("converts log levels between strings and numbers", function()
        -- given
        local levels = vim.log.levels

        -- when
        local warn_name = logging_util.level_to_string(levels.WARN)
        local debug_number = logging_util.level_to_number("debug")
        local fallback_name = logging_util.level_to_string({})
        local fallback_number = logging_util.level_to_number({})

        -- then
        assert.are.equal("WARN", warn_name)
        assert.are.equal(levels.DEBUG, debug_number)
        assert.are.equal("INFO", fallback_name)
        assert.are.equal(levels.INFO, fallback_number)
    end)

    it("creates and caches loggers by name", function()
        -- given
        vim.fn.stdpath = function()
            return "/tmp/logs"
        end

        -- when
        local first = logging_util.new({ name = "test", filename = "one.log", level = "debug" })
        local second = logging_util.new({ name = "test", filename = "two.log", level = "error" })
        local via_getter = logging_util.get_logger("test", "three.log")

        -- then
        assert.are.equal(first, second)
        assert.are.equal(first, via_getter)
        assert.are.equal("/tmp/logs/one.log", first.filepath)
        assert.are.equal(vim.log.levels.DEBUG, first.get_level())
    end)

    it("updates all existing logger levels when the global level changes", function()
        -- given
        local first = logging_util.new({ name = "one", level = "trace" })
        local second = logging_util.new({ name = "two", level = "debug" })

        -- when
        logging_util.set_level("error")

        -- then
        assert.are.equal(vim.log.levels.ERROR, first.get_level())
        assert.are.equal(vim.log.levels.ERROR, second.get_level())
    end)

    it("writes formatted messages to the configured log file", function()
        -- given
        local log_path = log_dir .. "/unit.log"
        os.remove(log_path)
        os.date = function()
            return "2026-06-25 12:34:56"
        end
        vim.fn.stdpath = function()
            return log_dir
        end
        local logger = logging_util.new({ name = "unit", filename = "unit.log", level = "info" })

        -- when
        logger.info("hello", { b = 2, a = 1 })

        -- then
        assert.are.equal("[2026-06-25 12:34:56] [INFO] [unit] hello {a=1, b=2}\n", read_file(log_path))
    end)

    it("does not write messages below the logger level", function()
        -- given
        local log_path = log_dir .. "/below-level.log"
        os.remove(log_path)
        vim.fn.stdpath = function()
            return log_dir
        end
        local logger = logging_util.new({ name = "unit", filename = "below-level.log", level = "warn" })

        -- when
        logger.info("skip me")

        -- then
        assert.is_nil(io.open(log_path, "r"))
    end)

    it("supports formatted logging helpers", function()
        -- given
        local log_path = log_dir .. "/fmt.log"
        os.remove(log_path)
        os.date = function()
            return "2026-06-25 12:34:56"
        end
        vim.fn.stdpath = function()
            return log_dir
        end
        local logger = logging_util.new({ name = "fmt", filename = "fmt.log", level = "trace" })

        -- when
        logger.fmt_error("failed: %s", "boom")

        -- then
        assert.are.equal("[2026-06-25 12:34:56] [ERROR] [fmt] failed: boom\n", read_file(log_path))
    end)

    it("clears a log file by opening it in write mode", function()
        -- given
        local log_path = log_dir .. "/clear.log"
        write_file(log_path, "old content")
        vim.fn.stdpath = function()
            return log_dir
        end

        -- when
        logging_util.clear_log("clear.log")

        -- then
        assert.are.equal("", read_file(log_path))
    end)

    it("can disable file output", function()
        -- given
        local log_path = log_dir .. "/disabled.log"
        os.remove(log_path)
        vim.fn.stdpath = function()
            return log_dir
        end
        local logger = logging_util.new({ name = "disabled", filename = "disabled.log", level = "info" })

        -- when
        logging_util.use_file(false)
        logger.info("not written")

        -- then
        assert.is_nil(io.open(log_path, "r"))
    end)
end)
