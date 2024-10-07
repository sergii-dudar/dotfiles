return {
    "jbyuki/one-small-step-for-vimkind",
    dependencies = {
        "mfussenegger/nvim-dap"
    },
    ft = { "lua" },
    config = function()
        local dap = require("dap")
        dap.configurations.lua = {
            {
                type = 'nlua',
                request = 'attach',
                name = "Attach to running Neovim instance",
            }
        }

        dap.adapters.nlua = function(callback, config)
            callback({ type = 'server', host = config.host or "127.0.0.1", port = config.port or 8086 })
        end

        vim.keymap.set('n', '<F5>', function()
            require("osv").run_this()
        end, { noremap = true, silent = false, desc = "Start lua debug" })
    end
}