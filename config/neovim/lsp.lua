local lsp_status = require('lsp-status')

--lsp_status.config({
--})
lsp_status.register_progress()

local nvim_lsp = require('lspconfig')
local util = nvim_lsp.util

--local capabilities = vim.lsp.protocol.make_client_capabilities()
--local capabilities = lsp_status.capabilities
local capabilities = require('cmp_nvim_lsp').default_capabilities(vim.lsp.protocol.make_client_capabilities())

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', '<C-h>', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('i', '<C-h>', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('i', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  -- error handling
  buf_set_keymap('n', '<leader>e', '<cmd>lua vim.diagnostic.open_float()<CR>', opts)
  buf_set_keymap('n', '<leader>Q', '<cmd>lua vim.diagnostic.setloclist()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)

  -- TODO: see if I ever want these
  --buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  --buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  --buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  --buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  --buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  --buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  --buf_set_keymap('n', '<space>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)

  -- lsp_status
  lsp_status.on_attach(client)
end

-- setup each language server here
-- julia
local julia_startup = [[
  using LanguageServer, LanguageServer.SymbolServer
  import Pkg

  depot_path = get(ENV, "JULIA_DEPOT_PATH", "")
  # figure out if there's a project to connect to
  # from https://github.com/neovim/nvim-lspconfig/blob/2dd9e060f21eecd403736bef07ec83b73341d955/lua/lspconfig/server_configurations/julials.lua#L19-L35
  project_path = let
    dirname(something(
      ## 1. Finds an explicitly set project (JULIA_PROJECT)
      Base.load_path_expand((
        p = get(ENV, "JULIA_PROJECT", nothing);
        p === nothing ? nothing : isempty(p) ? nothing : p
      )),
      ## 2. Look for a Project.toml file in the current working directory,
      ##    or parent directories, with $HOME as an upper boundary
      Base.current_project(),
      ## 3. First entry in the load path
      get(Base.load_path(), 1, nothing),
      ## 4. Fallback to default global environment,
      ##    this is more or less unreachable
      Base.load_path_expand("@v#.#"),
    ))
  end
  @info "Running language server" VERSION pwd() project_path depot_path
  server = LanguageServer.LanguageServerInstance(stdin, stdout, project_path, depot_path);
  server.runlinter = true;
  run(server);
]]
nvim_lsp['julials'].setup {
  cmd = { "julia", "--startup-file=no", "--history-file=no", "-e", julia_startup },
  on_attach = on_attach,
  capabilities = capabilities,
  flags = {
    debounce_text_changes = 150,
  }
}

nvim_lsp['tsserver'].setup {
  on_attach = on_attach,
  capabilities = capabilities,
}

nvim_lsp['eslint'].setup {
  on_attach = on_attach,
  capabilities = capabilities,
}

nvim_lsp['gopls'].setup {
  on_attach = on_attach,
  capabilities = capabilities,
}

nvim_lsp['hls'].setup {
  on_attach = on_attach,
  capabilities = capabilities,
}

nvim_lsp['nil_ls'].setup {
  on_attach = on_attach,
  capabilities = capabilities,
}

nvim_lsp['pyright'].setup {
  on_attach = on_attach,
  capabilities = capabilities,

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
        useLibraryCodeForTypes = true
      }
    }
  }
}

nvim_lsp['rust_analyzer'].setup {
  on_attach = on_attach,
  capabilities = capabilities,
}

nvim_lsp['texlab'].setup {
  on_attach = on_attach,
  capabilities = capabilities,
}

