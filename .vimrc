set nocompatible
execute pathogen#infect()
filetype plugin indent on
set runtimepath+=~/.vim/bundle/powerline/powerline/bindings/vim

set t_Co=16
syntax enable
set background=dark
let g:solarized_termtrans=1
colorscheme solarized

set autoindent
set relativenumber
set showcmd
set smarttab
set wildmenu
set nofoldenable
set ignorecase
set smartcase
set incsearch
set nohlsearch
set laststatus=2
set noshowmode
set colorcolumn=80
set nobackup
set nowritebackup
set wildignore+=*.pyc
set cpoptions+=Z
" Do not leak ack.vim output to console
set shellpipe=>

let g:netrw_list_hide='.*\.swp$'
let g:netrw_banner=0
let g:vim_markdown_folding_disabled=1
let g:vim_json_syntax_conceal=0
let g:hardtime_default_on=1
let g:ctrlp_custom_ignore={'dir': '\v/(vendor|public|node_modules)$'}

let mapleader=','
nnoremap <C-h> :tabprevious<CR>
nnoremap <C-l> :tabnext<CR>
nnoremap <Tab> <C-w>w
nnoremap <S-Tab> <C-w>W
nnoremap <Esc>h <C-w>h
nnoremap <Esc>j <C-w>j
nnoremap <Esc>k <C-w>k
nnoremap <Esc>l <C-w>l
nnoremap <Esc>c <C-w>c
nnoremap <C-t> :tabe<Space>
nnoremap <Space> zz
nnoremap <C-n> :NERDTreeTabsToggle<CR>
nnoremap <Leader>a :Ack!<Space>
nnoremap <Leader>h :set hlsearch<CR>
nnoremap <Leader>nh :set nohlsearch<CR>
" Save and quit
nnoremap <C-s> :w<CR>
inoremap <C-s> <Esc>:w<CR>
vnoremap <C-s> <Esc>:w<CR>
nnoremap <Leader>q :q<CR>
nnoremap <C-q> :qa<CR>
inoremap <C-q> <Esc>:qa<CR>
vnoremap <C-q> <Esc>:qa<CR>
cnoremap w!! w !sudo tee % >/dev/null

set expandtab
set shiftwidth=4
autocmd FileType coffee,ruby setlocal shiftwidth=2
autocmd FileType eco,html,jade,less,stylus,sass,scss setlocal shiftwidth=2
autocmd BufWinEnter *.gpp setlocal shiftwidth=2
autocmd FileType make setlocal noexpandtab
autocmd FileType xdefaults setlocal commentstring=!\ %s
autocmd FileType gitcommit setlocal colorcolumn=50

highlight ExtraWhitespace ctermbg=red guibg=red
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

set langmap=ёйцукенгшщзхъфывапролджэячсмитьбю;`qwertyuiop[]asdfghjkl\;'zxcvbnm\\,.,ЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ;QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>

if ! has('gui_running')
    set ttimeoutlen=10
    augroup FastEscape
        autocmd!
        autocmd InsertEnter * set timeoutlen=0
        autocmd InsertLeave * set timeoutlen=1000
    augroup END
endif
