return {
    --{
    --    "mfussenegger/nvim-dap",
    --    keys = {
    --        --{
    --        --    "<leader>dO",
    --        --    function()
    --        --        require("dap").step_out()
    --        --    end,
    --        --    desc = "Step Out",
    --        --},
    --        --{
    --        --    "<leader>do",
    --        --    function()
    --        --        require("dap").step_over()
    --        --    end,
    --        --    desc = "Step Over",
    --        --},
    --    },
    --},
    {
        "theHamsta/nvim-dap-virtual-text",
        opts = {
            virt_text_win_col = 80,
        },
    },
    -- {
    --     "Weissle/persistent-breakpoints.nvim",
    --     config = function()
    --         require("persistent-breakpoints").setup({
    --             load_breakpoints_event = { "BufReadPost" },
    --         })
    --     end,
    -- },
    -- debugging nvim lua plugins config ( real game changer for customizing nvim config )
    --[[ Quickstart:
        - Launch the server in the debuggee using <leader>dl
        - Open another Neovim instance with the source file
        - Place breakpoint with <leader>db
        - Connect using the DAP client with <leader>dc
        - Run your script/plugin in the debuggee ]]
    {
        "mfussenegger/nvim-dap",
        dependencies = {
            "jbyuki/one-small-step-for-vimkind",
        },
        lazy = false,
        config = function()
            local dap = require("dap")
            dap.configurations.lua = {
                {
                    type = "nlua",
                    request = "attach",
                    name = "Attach to running Neovim instance",
                },
            }

            dap.adapters.nlua = function(callback, config)
                callback({ type = "server", host = config.host or "127.0.0.1", port = config.port or 8086 })
            end

            vim.keymap.set("n", "<leader>dl", function()
                require("osv").launch({ port = 8086 })
            end, { noremap = true })
        end,
    },
}
