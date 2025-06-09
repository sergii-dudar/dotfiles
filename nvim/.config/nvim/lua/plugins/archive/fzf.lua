return {
    "ibhagwan/fzf-lua",
    enabled = false,
    keys = {
        -- { "<leader>.", LazyVim.pick("files"), desc = "Find Files (Root Dir)" },
        -- { "<leader>/", LazyVim.pick("live_grep"), desc = "Grep (Root Dir)" },
        { "<leader>.", LazyVim.pick("files", { root = false }), desc = "Find Files (cwd)" },
        { "<leader>/", LazyVim.pick("live_grep", { root = false }), desc = "Grep (cwd)" },
        { "<leader>m", LazyVim.pick("oldfiles", { cwd = vim.uv.cwd() }), desc = "Recent (cwd)" },
    },
    opts = function(_, opts)
        return vim.tbl_deep_extend("force", opts, {
            fzf_opts = {
                ["--layout"] = "default", --default / reverse / reverse-list
            },
            files = {
                prompt = "Files❯ ",
                -- cwd_prompt = false,
                hidden = true,
                resume = true,
            },
            grep = {
                prompt = "Rg❯ ",
                input_prompt = "Grep For❯ ",
                resume = true,
                hidden = true,
                no_header = true, -- hide grep|cwd header?
                no_header_i = true, -- hide interactive header?
            },
            oldfiles = {
                prompt = "History❯ ",
            },
            buffers = {
                prompt = "Buffers❯ ",
            },
        })
    end,
}