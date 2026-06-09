local rustft = { "rust", "ron" }
local cargoft = { "toml" }

-- stylua: ignore start
local keymap = LazyVim.safe_keymap_set
keymap("n", "<leader>joo", function() require("crates").open_documentation() end, { ft = cargoft, silent = true, desc = "[O]pen Documentation [crates]" })
keymap("n", "<leader>joc", function() require("crates").show_crate_popup() end, { ft = cargoft, silent = true, desc = "Open [C]rate Info [crates]" })
keymap("n", "<leader>juu", function() require("crates").upgrade_crate(true) end, { ft = cargoft, silent = true, desc = "[U]pdate Current Crate [crates]" })
keymap("n", "<leader>jua", function() require("crates").upgrade_all_crates(true) end, { ft = cargoft, silent = true, desc = "[U]pdate [A]ll Crates [crates]" })
keymap("n", "<leader>jsv", function() require("crates").show_versions_popup() end, { ft = cargoft, silent = true, desc = "[S]how Crate [V]ersions Popup [crates]" })
keymap("n", "<leader>jsd", function() require("crates").show_dependencies_popup() end, { ft = cargoft, silent = true, desc = "Show Crate [D]ependencies [crates]" })
-- stylua: ignore end

return {
    -- {
    --     "folke/which-key.nvim",
    --     optional = true,
    --     opts = {
    --         spec = {
    --             { "<leader>j", group = "+rust", ft = rustft },
    --             { "<leader>jc", group = "+rust code/compile", ft = rustft },
    --             { "<leader>jd", group = "+rust errors/diagnostics", ft = rustft },
    --             { "<leader>jo", group = "+rust open", ft = rustft },
    --             { "<leader>j", group = "+cargo", ft = cargoft },
    --             { "<leader>jo", group = "+cargo open", ft = cargoft },
    --             { "<leader>ju", group = "+cargo update", ft = cargoft },
    --             { "<leader>js", group = "+rust show", ft = cargoft },
    --         },
    --     },
    -- },
    -- rust keymaps, code actions & lsp based extensions.
    {
        "mrcjkb/rustaceanvim",
        -- stylua: ignore
        keys = {
                { "<leader>jcc", function() vim.cmd.RustLsp { "flyCheck", "run" } end, ft = rustft, desc = "Rust Compile [rust]" },
                { "<leader>cc", function()
                        local action_names = require("utils.lang.rust.lsp-rust").code_action_auto_resolve_match_names
                        require("utils.lsp-util").code_action.resolve_context(action_names)
                end, ft = rustft, desc = "Context Apply First Code Action [rust-analyzer]" },

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
    -- LSP for Cargo.toml
    -- {
    --     "Saecki/crates.nvim",
    -- },
}
