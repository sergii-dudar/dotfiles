local home = os.getenv('HOME')
local project_name = vim.fn.fnamemodify(vim.fn.getcwd(), ':p:h:t')
local workspace_dir = vim.env.HOME .. '/jdtls-workspace/' .. project_name

--local java_google_style_file = home .. "/dotfiles/work/formatter/eclipse-java-google-style.xml";

local system = 'linux'
--local mason = require('mason-registry')
--local jdtls_path = mason.get_package('jdtls'):get_install_path()
local jdtls_path = home .. "/.local/share/nvim/mason/packages/jdtls"
local lombok_path = jdtls_path .. '/lombok.jar'
local equinox_launcher_path = vim.fn.glob(jdtls_path .. '/plugins/org.eclipse.equinox.launcher_*.jar')
local config_path = vim.fn.glob(jdtls_path .. '/config_' .. system)
--print("config_path: "..config_path)
--print("config.cmd" .. vim.inspect(config.cmd))


local java21_dir = vim.fn.glob(home .. "/.sdkman/candidates/java/21.*-oracle")
local java21_bin = java21_dir .. "/bin/java";
local java_google_style_file = home .. "/dotfiles/work/formatter/intellij-java-google-style.xml";

local M = {}

M.java21_dir = java21_dir
M.java_google_style_file = java_google_style_file

return M