" slime/julia stuff
let g:slime_target = "tmux"
let g:slime_default_config = { "socket_name": "default", "target_pane": "" }
let g:slime_paste_file = tempname()
let g:slime_cell_delimiter = "#%%"
nmap <c-c><c-r> <Plug>SlimeSendCell

" needs to be a function for custom completion
function! TmuxPanes(txt,L,P)
  " basically copied from the plugin because those functions are private
  let format = '#{pane_id} #{session_name}:#{window_index}.#{pane_index} #{window_name}#{?window_active, (active),} #{pane_current_path}'
  let l:socket_option = b:slime_config["socket_name"][0] ==? "/" ? "-S" : "-L"
  let l:command = " list-panes -a -F " . shellescape(format)

  let l:panetxt = system("tmux " . l:socket_option . " " . shellescape(b:slime_config["socket_name"]) . " " . l:command)
  let l:panes = split(l:panetxt, "\n")

  if a:txt !~ '\s\+'
    "filter(l:panes, 'v:val =~ "' . a:txt . '"')
    let l:panes = filter(l:panes, 'v:val =~ "julia"')
  endif
  return l:panes
endfunction

" override slime's config function so it pops up a nice window for panes
function! SlimeOverrideConfig()
  let b:slime_config["socket_name"] = input("tmux socket name or absolute path: ", b:slime_config["socket_name"])

  let b:slime_config["target_pane"] = input("tmux target pane: ", "julia", "customlist,TmuxPanes")
  if b:slime_config["target_pane"] =~ '\s\+'
    let b:slime_config["target_pane"] = split(b:slime_config["target_pane"])[0]
  endif
endfunction

