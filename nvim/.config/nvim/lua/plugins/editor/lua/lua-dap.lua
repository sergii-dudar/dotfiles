return {
    "jbyuki/one-small-step-for-vimkind",
    -- ft = { "lua" },
    keys = {
        {
            "<leader>dl", -- jbyuki/one-small-step-for-vimkind by lazyvim.plugins.extras.dap.nlua
            -- debugging nvim lua plugins config ( real game changer for customizing nvim config )
            --[[ Quickstart:
                    - Launch the server in the debuggee using <leader>dl
                    - Open another Neovim instance with the source file
                    - Place breakpoint with <leader>db
                    - Connect using the DAP client with <leader>dc
                    - Run your script/plugin in the debuggee ]]
            function()
                require("osv").launch({ port = 8086 })
            end,
            desc = "Start LUA(NVIM) debug server (port = 8086)",
        },
    },
}