noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>

syntax on
set hlsearch
set ignorecase
set incsearch
set noswapfile
set number
let mapleader = "'"
set number relativenumber
set clipboard=unnamedplus

vnoremap . :norm! .<CR>

"nnoremap <C-c> "+y
"vnoremap <C-c> "+y
"nnoremap <C-p> "+p
"vnoremap <C-p> "+p

"inoremap jj <esc>
"inoremap JJ <esc>

"cnoremap jj <C-c>
"cnoremap JJ <C-c>

nnoremap <SPACE> <Nop>
let mapleader = " "

noremap J 6jzz
noremap K 6kzz

"vertically navigation
noremap <C-u> <C-u>zz
noremap <C-d> <C-d>zz

"find
noremap n nzz
noremap N Nzz

"if !has('gui_running')
map <SPACE>n <A-n>
"endif

" Yank colored selection
"au TextYankPost * silent! lua vim.highlight.on_yank()
au TextYankPost * silent! lua vim.highlight.on_yank {higroup="IncSearch", timeout=300}

" Find files using Telescope command-line sugar.
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

" wildmenu
set wildmenu
set wildmode=longest:full,full



"noremap <S-C-n> <A-n>


" Plugins will be downloaded under the specified directory.
call plug#begin(has('nvim') ? stdpath('data') . '/plugged' : '~/.vim/plugged')

" Declare the list of plugins.
Plug 'terryma/vim-multiple-cursors'

" telescope
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim', { 'tag': '0.1.5' }

Plug 'easymotion/vim-easymotion'

Plug 'fladson/vim-kitty'

" List ends here. Plugins become visible to Vim after this call.
call plug#end()
