local lualine = require('lualine')
--local lspstatus = require('lsp-status')

local function lspStatus()
  if #vim.lsp.buf_get_clients() > 0 then
    return require('lsp-status').status()
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
    lualine_a = {'mode'},
    lualine_b = {{'branch', icon = ''}, 'diff', 'diagnostics'},
    lualine_c = {'filename', lspStatus},
    lualine_x = {'encoding', 'fileformat', 'filetype'},
    lualine_y = {},
    lualine_z = {'location'}
  }
}
