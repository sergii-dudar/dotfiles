return {
    {
        "mfussenegger/nvim-lint",
        opts = {
            linters_by_ft = {
                --xml = { "" },
                --java = { "checkstyle" },
                kotlin = { "ktlint" },
                --sh = { "shellcheck" },
                python = { "ruff" },
                --lua = { "luacheck" },
                -- Use the "*" filetype to run linters on all filetypes.
                -- ['*'] = { 'global linter' },
                -- Use the "_" filetype to run linters on filetypes that don't have other linters configured.
                -- ['_'] = { 'fallback linter' },
                -- ["*"] = { "typos" },
                -- css = { "stylelint" },
            },
            -- LazyVim extension to easily override linter options
            -- or add custom linters.
            ---@type table<string,table>
            linters = {
                -- -- Example of using selene only when a selene.toml file is present
                -- selene = {
                --   -- `condition` is another LazyVim extension that allows you to
                --   -- dynamically enable/disable linters based on the context.
                --   condition = function(ctx)
                --     return vim.fs.find({ "selene.toml" }, { path = ctx.filename, upward = true })[1]
                --   end,
                -- },
            },
        },
    },
}
