--local capabilities = vim.lsp.protocol.make_client_capabilities()
local capabilities = require('blink.cmp').get_lsp_capabilities()

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('my.lsp', {}),
  callback = function(ev)
    local client = assert(vim.lsp.get_client_by_id(ev.data.client_id))

    -- Enable completion triggered by <c-x><c-o>
    --buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- Mappings.
    local opts = { noremap = true, silent = true }

    -- See `:help vim.lsp.*` for documentation on any of the below functions
    vim.keymap.set('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
    vim.keymap.set('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
    vim.keymap.set('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
    vim.keymap.set('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
    vim.keymap.set('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
    vim.keymap.set('n', '<C-h>', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
    vim.keymap.set('i', '<C-h>', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
    vim.keymap.set('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
    vim.keymap.set('i', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
    -- error handling
    vim.keymap.set('n', '<leader>e', '<cmd>lua vim.diagnostic.open_float()<CR>', opts)
    vim.keymap.set('n', '<leader>Q', '<cmd>lua vim.diagnostic.setloclist()<CR>', opts)
    vim.keymap.set('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
    vim.keymap.set('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)

    -- TODO: see if I ever want these
    --vim.keymap.set('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
    --vim.keymap.set('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
    --vim.keymap.set('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
    --vim.keymap.set('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
    --vim.keymap.set('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
    --vim.keymap.set('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
    --vim.keymap.set('n', '<space>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
  end,
})

vim.lsp.enable({ 'ts_ls', 'eslint', 'gopls', 'hls', 'lua_ls', 'nil_ls', 'pyright', 'rust_analyzer', 'texlab', })

-- specific language configs
vim.lsp.config('pyright', {
  settings = {
    pyright = {
      -- I run isort on save
      disableOrganizeInputs = true,
    },
    python = {
      analysis = {
        autoSearchPaths = true,
        diagnosticMode = "workspace",
        typeCheckingMode = "strict",
        useLibraryCodeForTypes = true,
        diagnosticSeverityOverrides = {
          reportDuplicateImport = "error",
          reportConstantRedefinition = "error",
          reportIncompatibleMethodOverride = "warning",
          reportIncompatibleVariableOverride = "warning",
          reportPrivateUsage = "error",
          reportMissingParameterType = "warning",
          reportMatchNotExhaustive = "error",
          reportMissingTypeStubs = "information",
          reportUntypedBaseClass = "none",
          reportUntypedFunctionDecorator = "warning",
          reportUnusedVariable = "none", -- ruff will help with this
          reportUnusedImport = "none",   -- ruff will help with this
        },
      }
    }
  }
})

vim.lsp.config('ruff', {
  on_attach = function(client, bufnr)
    -- disable hover in favor of pyright
    client.server_capabilities.hoverProvider = false
  end,
  init_options = {
    settings = {
      args = { "--extend-select", "T", },
    },
  },
})
