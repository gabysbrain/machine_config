-- setup isn't needed until overriding default options
-- require('nvim-treesitter').setup

-- set up highlighting
vim.api.nvim_create_autocmd('FileType', {
  pattern = { '<filetype>' },
  callback = function() vim.treesitter.start() end,
})
