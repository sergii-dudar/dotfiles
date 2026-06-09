local rustft = { "rust", "ron" }
local cargoft = { "toml" }

-- which-key has no `ft` field, and its `cond` is evaluated only once at registration time, so group
-- labels can't be switched per buffer declaratively. Register the filetype-specific labels
-- buffer-locally on FileType -- which-key's supported mechanism (the inherited `buffer` field).
local wk_augroup = vim.api.nvim_create_augroup("rust_config_which_key", { clear = true })
vim.api.nvim_create_autocmd("FileType", {
    group = wk_augroup,
    pattern = rustft,
    callback = function(ev)
        require("which-key").add({
            ---@diagnostic disable-next-line: undefined-field
            buffer = ev.buf,
            { "<leader>j", group = "+rust" },
            { "<leader>jc", group = "+rust code/compile" },
            { "<leader>jd", group = "+rust errors/diagnostics" },
            { "<leader>jo", group = "+rust open" },
        })
    end,
})
vim.api.nvim_create_autocmd("FileType", {
    group = wk_augroup,
    pattern = cargoft,
    callback = function(ev)
        require("which-key").add({
            ---@diagnostic disable-next-line: undefined-field
            buffer = ev.buf,
            { "<leader>j", group = "+cargo" },
            { "<leader>jo", group = "+cargo open" },
            { "<leader>ju", group = "+cargo update" },
            { "<leader>js", group = "+cargo show" },
        })
    end,
})

return {
    -- rust keymaps, code actions & lsp based extensions.
    {
        "mrcjkb/rustaceanvim",
        -- stylua: ignore
        keys = {
                -- actions
                { "<leader>cc", function()
                        local action_names = require("utils.lang.rust.lsp-rust").code_action_auto_resolve_match_names
                        require("utils.lsp-util").code_action.resolve_context(action_names)
                end, ft = rustft, desc = "Context Apply First Code Action [rust-analyzer]" },

                -- code / compile
                { "<leader>jcc", function() vim.cmd.RustLsp({ "flyCheck", "run" }) end, ft = rustft, desc = "Rust Compile [rust]" },
                -- diagnostics & errors
                { "<leader>jde", function() vim.cmd.RustLsp({ 'explainError', 'current' }) end, ft = rustft, desc = "Diagnostic [e]xplain [rust]" },
                { "<leader>jdd", function() vim.cmd.RustLsp({ 'renderDiagnostic', 'current' }) end, ft = rustft, desc = "[D]iagnostic render [rust]" },
                { "<leader>jdl", function() vim.cmd.RustLsp('relatedDiagnostics') end, ft = rustft, desc = "[D]iagnostic qflist (>1) [rust]" },

                -- open
                { "<leader>joc", function() vim.cmd.RustLsp('openCargo') end, ft = rustft, desc = "[O]pen [C]argo [rust]" },
                { "<leader>jop", function() vim.cmd.RustLsp('parentModule') end, ft = rustft, desc = "[O]pen [P]argo Module [rust]" },
                { "<leader>jod", function() vim.cmd.RustLsp('openDocs') end, ft = rustft, desc = "[O]pen [d]ocs.rs of symbol under the cursor [rust]" },
        },
        --[[ opts = function(_, opts)
            opts.server = opts.server or {}
            local prev_on_attach = opts.server.on_attach
            opts.server.on_attach = function(client, bufnr)
                if prev_on_attach then
                    prev_on_attach(client, bufnr)
                end
                vim.keymap.set("n", "<leader>cc", function()
                    local action_names = require("utils.lang.rust.lsp-rust").code_action_auto_resolve_match_names
                    require("utils.lsp-util").code_action.resolve_context(action_names)
                end, { buffer = bufnr, desc = "Context Apply First Code Action [rust-analyzer]" })
            end
            return opts
        end, ]]
    },
    -- crates.nvim keymaps, ft-scoped to Cargo.toml. Registered as lazy `keys` (not via
    -- top-level safe_keymap_set) so there are no import-time side effects and crates.nvim
    -- is loaded on demand.
    {
        "Saecki/crates.nvim",
        -- stylua: ignore
        keys = {
            { "<leader>joo", function() require("crates").open_documentation() end, ft = cargoft, desc = "[O]pen Documentation [crates]" },
            { "<leader>joc", function() require("crates").show_crate_popup() end, ft = cargoft, desc = "Open [C]rate Info [crates]" },
            { "<leader>juu", function() require("crates").upgrade_crate(true) end, ft = cargoft, desc = "[U]pdate Current Crate [crates]" },
            { "<leader>jua", function() require("crates").upgrade_all_crates(true) end, ft = cargoft, desc = "[U]pdate [A]ll Crates [crates]" },
            { "<leader>jsv", function() require("crates").show_versions_popup() end, ft = cargoft, desc = "[S]how Crate [V]ersions Popup [crates]" },
            { "<leader>jsd", function() require("crates").show_dependencies_popup() end, ft = cargoft, desc = "Show Crate [D]ependencies [crates]" },
        },
        opts = {
            popup = {
                autofocus = true,
            },
        },
    },
}
