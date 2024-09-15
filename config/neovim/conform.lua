require("conform").setup {
  formatters_by_ft = {
    python = { "ruff check --select I --fix", "ruff format" },
  },
  format_on_save = {
    lsp_format = "fallback",
    timeout_ms = 500,
  },
}
