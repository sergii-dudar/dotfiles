# Test recipes for the Neovim Lua/Busted suite.
#
# Run `make help` to see all targets.

BUSTED ?= busted
LUA ?= lua

TEST_ROOT := lua/tests
BUSTED_CONFIG := $(TEST_ROOT)/.busted
BUSTED_BASE := $(BUSTED) --config-file=$(BUSTED_CONFIG)
REPORT_DIR ?= /private/tmp/nvim-busted-report
REPORT_FILE := $(REPORT_DIR)/busted.ndjson
OUTPUT_HANDLER := lua/modules/lua/busted-test/helpers/output_handler.lua

.DEFAULT_GOAL := help

.PHONY: help
help: ## Show this help.
	@awk 'BEGIN {FS = ":.*##"; printf "Available targets:\n"} \
		/^[a-zA-Z0-9_.-]+:.*##/ { printf "  \033[36m%-28s\033[0m %s\n", $$1, $$2 }' $(MAKEFILE_LIST)

# -----------------------------------------------------------------------------
# Standard runs
# -----------------------------------------------------------------------------

.PHONY: test
test: ## Run all Lua Busted specs.
	$(BUSTED_BASE) $(TEST_ROOT)

.PHONY: test-verbose
test-verbose: ## Run all specs with verbose Busted output.
	$(BUSTED_BASE) --verbose $(TEST_ROOT)

.PHONY: test-utils
test-utils: ## Run all utils specs.
	$(BUSTED_BASE) $(TEST_ROOT)/utils

.PHONY: test-utils-root
test-utils-root: ## Run top-level utils specs only.
	$(BUSTED_BASE) $(TEST_ROOT)/utils/*_spec.lua

.PHONY: test-utils-json
test-utils-json: ## Run utils/json specs.
	$(BUSTED_BASE) $(TEST_ROOT)/utils/json

.PHONY: test-utils-java
test-utils-java: ## Run utils/java specs.
	$(BUSTED_BASE) $(TEST_ROOT)/utils/java

# -----------------------------------------------------------------------------
# Individual specs
# -----------------------------------------------------------------------------

.PHONY: test-json-normalize
test-json-normalize: ## Run utils/json/normalize_spec.lua.
	$(BUSTED_BASE) $(TEST_ROOT)/utils/json/normalize_spec.lua

.PHONY: test-java-tostring-parser
test-java-tostring-parser: ## Run utils/java/java_tostring_parser_spec.lua.
	$(BUSTED_BASE) $(TEST_ROOT)/utils/java/java_tostring_parser_spec.lua

.PHONY: test-logging-util
test-logging-util: ## Run utils/logging_util_spec.lua.
	$(BUSTED_BASE) $(TEST_ROOT)/utils/logging_util_spec.lua

# -----------------------------------------------------------------------------
# Output formats
# -----------------------------------------------------------------------------

.PHONY: test-color
test-color: ## Run with utfTerminal output.
	$(BUSTED_BASE) --output=utfTerminal $(TEST_ROOT)

.PHONY: test-plain
test-plain: ## Run with plainTerminal output.
	$(BUSTED_BASE) --output=plainTerminal $(TEST_ROOT)

.PHONY: test-json-output
test-json-output: ## Run with Busted JSON output.
	$(BUSTED_BASE) --output=json $(TEST_ROOT)

.PHONY: test-tap
test-tap: ## Run with TAP output.
	$(BUSTED_BASE) --output=TAP $(TEST_ROOT)

.PHONY: test-junit
test-junit: ## Write JUnit XML to test-results.xml.
	$(BUSTED_BASE) --output=junit $(TEST_ROOT) > test-results.xml
	@echo "JUnit XML written to test-results.xml"

.PHONY: test-overseer-output
test-overseer-output: ## Run with the Overseer NDJSON output handler.
	@mkdir -p $(REPORT_DIR)
	$(BUSTED_BASE) --output=$(OUTPUT_HANDLER) -Xoutput=$(REPORT_FILE) --verbose $(TEST_ROOT)
	@echo "NDJSON report written to $(REPORT_FILE)"

# -----------------------------------------------------------------------------
# Diagnostics
# -----------------------------------------------------------------------------

.PHONY: test-list
test-list: ## List discovered tests without running.
	$(BUSTED_BASE) --list $(TEST_ROOT)

.PHONY: test-shuffle
test-shuffle: ## Run specs in random order.
	$(BUSTED_BASE) --shuffle $(TEST_ROOT)

.PHONY: test-repeat
test-repeat: ## Repeat the suite five times.
	$(BUSTED_BASE) --repeat=5 $(TEST_ROOT)

.PHONY: test-filter
test-filter: ## Run tests matching FILTER='pattern'.
	@test -n "$(FILTER)" || (echo "Usage: make test-filter FILTER='pattern'" && exit 2)
	$(BUSTED_BASE) --filter='$(FILTER)' $(TEST_ROOT)

.PHONY: test-file
test-file: ## Run one spec file with FILE=lua/tests/path_spec.lua.
	@test -n "$(FILE)" || (echo "Usage: make test-file FILE=lua/tests/utils/string_util_spec.lua" && exit 2)
	$(BUSTED_BASE) $(FILE)

.PHONY: luac
luac: ## Parse-check all Lua test files.
	find $(TEST_ROOT) -type f -name '*.lua' -exec luac -p {} +

.PHONY: clean
clean: ## Remove generated test artifacts.
	rm -f test-results.xml luacov.stats.out luacov.report.out
	rm -rf $(REPORT_DIR)
