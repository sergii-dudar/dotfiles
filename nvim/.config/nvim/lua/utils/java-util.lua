local home = os.getenv('HOME')
local project_name = vim.fn.fnamemodify(vim.fn.getcwd(), ':p:h:t')
local workspace_dir = vim.env.HOME .. '/jdtls-workspace/' .. project_name

--local java_google_style_file = home .. "/dotfiles/work/formatter/eclipse-java-google-style.xml";

local system = 'linux'
--local jdtls_path = mason.get_package('jdtls'):get_install_path()
--local jdtls_path = home .. "/.local/share/nvim/mason/packages/jdtls"
--local lombok_path = jdtls_path .. '/lombok.jar'
--local equinox_launcher_path = vim.fn.glob(jdtls_path .. '/plugins/org.eclipse.equinox.launcher_*.jar')
--local config_path = vim.fn.glob(jdtls_path .. '/config_' .. system)
--print("config_path: "..config_path)
--print("config.cmd" .. vim.inspect(config.cmd))


local java21_dir = vim.fn.glob(home .. "/.sdkman/candidates/java/21.*-oracle")
local java21_bin = java21_dir .. "/bin/java"
--local java_google_style_file = home .. "/dotfiles/work/formatter/intellij-java-google-style.xml"
local java_google_style_file = home .. "/dotfiles/work/formatter/eclipse-java-google-style.xml"

local M = {}

M.is_installed = function(package_name, package_version)
    local mason_reg = require('mason-registry')
    local pkg = mason_reg.get_package(package_name)
    local is_installed = pkg:is_installed()

    if not is_installed then
        return false
    end

    local installed_version
    pkg:get_installed_version(function(ok, version)
        if not ok then
            return
        end

        installed_version = version
    end)

    return installed_version == package_version
end

M.install_pkgs = function(packages)
    local mason_reg = require('mason-registry')
    for _, dep in ipairs(packages) do
        if not M.is_installed(dep.name, dep.version) then
            local pkg = mason_reg.get_package(dep.name)

            pkg:install({
                version = dep.version,
                force = true,
            })
        end
    end
end

M.java21_dir = java21_dir
M.java_google_style_file = java_google_style_file
M.get_spring_boot_tools_path_ls_path = function()

    --M.install_pkgs({{ name = 'spring-boot-tools', version = '1.55.1' }})
    --local mason_reg = require('mason-registry')
    --local spring_boot_tools_path = mason_reg.get_package('spring-boot-tools'):get_install_path()
    --vim.notify(spring_boot_tools_path .. "/extension/language-server")
    --return spring_boot_tools_path .. "/extension/language-server"

    --local mason_reg = require('mason-registry')
    return home .. "/.local/share/nvim/mason/packages/spring-boot-tools/extension"
end

return M