local lualine = require('lualine')
local lspstatus = require('lsp-status')

lspstatus.config {
  diagnostics = false,
}

local function lspStatus()
  if #vim.lsp.get_clients() > 0 then
    -- TODO: one day customize this more
    return lspstatus.status()
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
    lualine_b = {{'branch', icon = ''}, 'diagnostics'},
    lualine_c = {'filename', lspStatus},
    lualine_x = {'encoding', 'fileformat'},
    lualine_y = {'filetype'},
    lualine_z = {'location'}
  }
}
