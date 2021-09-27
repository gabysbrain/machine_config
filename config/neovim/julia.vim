" slime/julia stuff
let g:slime_target = "tmux"
let g:slime_default_config = {"socket_name": get(split($TMUX, ","), 0), "target_pane": ":.1"}
let g:slime_cell_delimiter = "#%%"
nmap <leader>r <Plug>SlimeSendCell

