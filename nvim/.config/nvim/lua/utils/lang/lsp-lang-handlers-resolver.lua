-- Resolves and installs per-project-language LSP handlers.
--
-- Each language owns its handlers completely — there is NO shared shape, because
-- handler needs differ wildly per language/LSP (jdtls needs diagnostic + hover
-- post-processing; rust-analyzer today needs none). A language absent from the
-- table below simply gets no custom handlers.
--
-- - setup - detect the active project language and install its handlers (if any)

local lang_project = require("utils.lang.lang-project")

local handlers_by_lang = {
    java = "utils.lang.java.lsp-java-handlers",
}

local M = {}

--- Install LSP handler overrides for the detected project language.
--- Looks up the current primary project language and requires that language's
--- handler module when one is registered; languages without custom handlers are
--- intentionally left on Neovim's default LSP behavior.
function M.setup()
    local lang = lang_project.current()
    if not lang then
        return
    end
    local mod = handlers_by_lang[lang]
    if mod then
        require(mod).setup()
    end
end

return M
