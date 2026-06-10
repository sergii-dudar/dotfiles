local rustft = { "rust", "ron" }
local cargoft = { "toml" }

---@param bufnr integer
---@return boolean
local function is_cargo_toml(bufnr)
    return vim.fn.fnamemodify(vim.api.nvim_buf_get_name(bufnr), ":t") == "Cargo.toml"
end

---@param bufnr integer
---@param lhs string
---@param rhs function
---@param desc string
local function cargo_keymap(bufnr, lhs, rhs, desc)
    vim.keymap.set("n", lhs, rhs, { buffer = bufnr, desc = desc })
end

local wk_augroup = vim.api.nvim_create_augroup("rust_config_which_key", { clear = true })
vim.api.nvim_create_autocmd("FileType", {
    group = wk_augroup,
    pattern = rustft,
    callback = function(ev)
        ---@diagnostic disable-next-line: undefined-field
        if vim.bo[ev.buf].buftype ~= "" then
            return
        end
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
    -- stylua: ignore
    callback = function(ev)
        ---@diagnostic disable-next-line: undefined-field
        if vim.bo[ev.buf].buftype ~= "" or not is_cargo_toml(ev.buf) then
            return
        end
        require("which-key").add({
            ---@diagnostic disable-next-line: undefined-field
            buffer = ev.buf,
            { "<leader>j", group = "+cargo" },
            { "<leader>jo", group = "+cargo open" },
            { "<leader>ju", group = "+cargo update" },
            { "<leader>js", group = "+cargo show" },
        })
        cargo_keymap(ev.buf, "<leader>joo", function() require("crates").open_documentation() end, "[O]pen Documentation [crates]")
        cargo_keymap(ev.buf, "<leader>joc", function() require("crates").show_crate_popup() end, "Open [C]rate Info [crates]")
        cargo_keymap(ev.buf, "<leader>juu", function() require("crates").upgrade_crate(true) end, "[U]pdate Current Crate [crates]")
        cargo_keymap(ev.buf, "<leader>jua", function() require("crates").upgrade_all_crates(true) end, "[U]pdate [A]ll Crates [crates]")
        cargo_keymap(ev.buf, "<leader>jsv", function() require("crates").show_versions_popup() end, "[S]how Crate [V]ersions Popup [crates]")
        cargo_keymap(ev.buf, "<leader>jsd", function() require("crates").show_dependencies_popup() end, "Show Crate [D]ependencies [crates]")
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
                { "<leader>jcc", function() vim.cmd.RustLsp({ "flyCheck", "run" }) end, ft = rustft, desc = "Rust Compile [rust]" }, -- have effect only in case: checkOnSave = false

                -- diagnostics & errors
                { "<leader>jde", function() vim.cmd.RustLsp({ 'explainError', 'current' }) end, ft = rustft, desc = "Diagnostic [e]xplain [rust]" },
                { "<leader>jdd", function() vim.cmd.RustLsp({ 'renderDiagnostic', 'current' }) end, ft = rustft, desc = "[D]iagnostic render [rust]" },
                { "<leader>jdl", function() vim.cmd.RustLsp('relatedDiagnostics') end, ft = rustft, desc = "[D]iagnostic qflist (>1) [rust]" },

                -- open
                { "<leader>joc", function() vim.cmd.RustLsp('openCargo') end, ft = rustft, desc = "[O]pen [C]argo [rust]" },
                { "<leader>jop", function() vim.cmd.RustLsp('parentModule') end, ft = rustft, desc = "[O]pen [P]argo Module [rust]" },
                { "<leader>jod", function() vim.cmd.RustLsp('openDocs') end, ft = rustft, desc = "[O]pen [d]ocs.rs of symbol under the cursor [rust]" },
        },
        -- opts = {
        --     server = {
        --         default_settings = {
        --             ["rust-analyzer"] = {
        --                 checkOnSave = false,
        --             },
        --         },
        --     },
        -- },
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
    -- managing crates.io dependencies
    {
        "Saecki/crates.nvim",
        event = { "BufRead Cargo.toml" },
        opts = {
            popup = {
                autofocus = true,
            },
        },
    },
}
