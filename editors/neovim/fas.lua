vim.filetype.add({
  extension = { fas = "fas" },
})

vim.lsp.config("fas", {
  cmd = { "fas", "lsp" },
  filetypes = { "fas" },
  root_markers = { ".git" },
})

vim.lsp.enable("fas")

vim.api.nvim_create_autocmd("User", {
  pattern = "TSUpdate",
  callback = function()
    local ok, parsers = pcall(require, "nvim-treesitter.parsers")
    if not ok then
      return
    end
    parsers.fas = {
      install_info = {
        url = "https://github.com/Globexix/fas",
        location = "editors/tree-sitter-fas",
        files = { "src/parser.c", "src/scanner.c" },
        revision = "e4221917cf767d27e1b9d6ca14310c783cd0220e",
      },
      filetype = "fas",
    }
  end,
})
