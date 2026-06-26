" Custom Ukrainian insert-mode keymap overrides.
" default mapping `:echo globpath(&rtp, 'keymap/ukrainian-jcuken.vim')`
"   macos - /opt/homebrew/share/nvim/runtime/keymap/ukrainian-jcuken.vim
"   linux - /usr/share/nvim/runtime/keymap/ukrainian-jcuken.vim
scriptencoding utf-8

runtime keymap/ukrainian-jcuken.vim

let b:keymap_name = "uk-custom"

lnoremap <buffer> <Bslash> ʼ
lnoremap <buffer> ` ґ