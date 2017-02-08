
if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1


setlocal comments=:%,:# commentstring=%\ %s
setlocal iskeyword=-,+,a-z,A-Z,48-57

setlocal formatoptions-=t formatoptions+=croql

setl sw=4 ts=4 et


let b:undo_ftplugin = "setl com< cms< fo< isk<"
