-- some times during updates and compiling ts, it can crash nvim.
-- to fix, need madually remove all languages, and restart nvim
-- vim.cmd("TSUninstall all")
-- optionally run vim.cmd("TSUpdate"), after all
-- in vim.cmd, as calling input also can crash as using ts
return {
    "nvim-treesitter/nvim-treesitter",
    opts = {
        ensure_installed = {
            "bash",
            "html",
            "javascript",
            "json",
            "lua",
            "markdown",
            "markdown_inline",
            "python",
            "query",
            "regex",
            "tsx",
            "typescript",
            "vim",
            "yaml",
            "java",
            -- "kotlin",
            "xml",
            -- "strace",
            -- "groovy",
            "terraform",
            "haskell",
            "hyprlang",
            -- "commonlisp",
            -- "css",
            -- "latex",
            -- "norg",
            -- "scss",
            -- "svelte",
            -- "typst",
            -- "vue",
        },
        indent = { enable = true, disable = { "java" } },
    },
    {
        "nvim-treesitter/nvim-treesitter-context",
        opts = {
            multiline_threshold = 1,
            on_attach = function(bufnr)
                return vim.bo[bufnr].filetype ~= "java"
            end,
        },
    },
}

--[[ {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      local function add(lang)
        if type(opts.ensure_installed) == "table" then
          table.insert(opts.ensure_installed, lang)
        end
      end

      vim.filetype.add({
        extension = { rasi = "rasi", rofi = "rasi", wofi = "rasi" },
        filename = {
          ["vifmrc"] = "vim",
        },
        pattern = {
          [".*/waybar/config"] = "jsonc",
          [".*/mako/config"] = "dosini",
          [".*/kitty/.+%.conf"] = "kitty",
          [".*/hypr/.+%.conf"] = "hyprlang",
          ["%.env%.[%w_.-]+"] = "sh",
        },
      })
      vim.treesitter.language.register("bash", "kitty")

      add("git_config")

      if have("hypr") then
        add("hyprlang")
      end

      if have("fish") then
        add("fish")
      end

      if have("rofi") or have("wofi") then
        add("rasi")
      end
    end,
  }, ]]
