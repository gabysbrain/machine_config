require'todo-comments'.setup {
  sign_priority = 12,
  search = {
    command = "ag",
    args = {
      "--vimgrep",
    }
  }
}
