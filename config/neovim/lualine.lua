local lualine = require('lualine')
local lsp_progress = require('lsp-progress')

-- need to initialize progress otherwise things fail
lsp_progress.setup({
  client_format = function(client_name, spinner, series_messages)
    if #series_messages == 0 then
      return nil
    end
    return {
      name = client_name,
      body = spinner .. " " .. table.concat(series_messages, ", "),
    }
  end,
  format = function(client_messages)
    --- @param name string
    --- @param msg string?
    --- @return string
    local function stringify(name, msg)
      return msg and string.format("%s %s", name, msg) or name
    end

    local sign = "λ"
    local lsp_clients = vim.lsp.get_clients()
    local messages_map = {}
    for _, climsg in ipairs(client_messages) do
      messages_map[climsg.name] = climsg.body
    end

    if #lsp_clients > 0 then
      table.sort(lsp_clients, function(a, b)
        return a.name < b.name
      end)
      local builder = {}
      for _, cli in ipairs(lsp_clients) do
        if
            type(cli) == "table"
            and type(cli.name) == "string"
            and string.len(cli.name) > 0
        then
          if messages_map[cli.name] then
            table.insert(builder, stringify(cli.name, messages_map[cli.name]))
          else
            table.insert(builder, stringify(cli.name))
          end
        end
      end
      if #builder > 0 then
        --return sign .. " " .. table.concat(builder, ", ")
        return sign
      end
    end
    return ""
  end,
})

local function lspStatus()
  if #vim.lsp.get_clients() > 0 then
    return lsp_progress.progress()
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
