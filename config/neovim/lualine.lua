local lualine = require('lualine')
local lsp_progress = require('lsp-progress')

local function lspStatus()
  if #vim.lsp.get_clients() > 0 then
    return lsp_progress.status()
  end

  return ''
end

lualine.setup {
  options = {
    theme = 'nord',
    component_separators = { left = '|', right = '|' },
    section_separators = { left = '', right = '' },
  },
  sections = {
    lualine_a = { 'mode' },
    lualine_b = { { 'branch', icon = '' }, 'diagnostics' },
    lualine_c = { 'filename', lspStatus },
    lualine_x = { 'encoding', 'fileformat' },
    lualine_y = { 'filetype' },
    lualine_z = { 'location' }
  }
}

-- listen lsp-progress event and refresh lualine
vim.api.nvim_create_augroup("lualine_augroup", { clear = true })
vim.api.nvim_create_autocmd("User", {
  group = "lualine_augroup",
  pattern = "LspProgressStatusUpdated",
  callback = require("lualine").refresh,
})
