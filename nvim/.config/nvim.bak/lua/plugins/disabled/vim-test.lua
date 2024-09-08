return {
  "vim-test/vim-test",
  dependencies = {
    "preservim/vimux"
  },
  enabled = false,
  config = function()
   -- vim.keymap.set("n", "<leader>", ":TestNearest<CR>", {})
   -- vim.keymap.set("n", "<leader>", ":TestFile<CR>", {})
   -- vim.keymap.set("n", "<leader>", ":TestSuite<CR>", {})
   -- vim.keymap.set("n", "<leader>", ":TestLast<CR>", {})
   -- vim.keymap.set("n", "<leader>", ":TestVisit<CR>", {})
   -- vim.cmd("let test#strategy = 'vimux'")
  end,
}