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
set wildmode=list:longest,full
set nofoldenable
set ignorecase
set smartcase
set incsearch
set nohlsearch
set laststatus=2
set noshowmode
set colorcolumn=80
set wildignore+=*.pyc
set cpoptions+=Z
set nomodeline
set backspace=indent,eol,start
set lazyredraw
set fileencoding=utf-8
set autoread
" Do not leak ack.vim output to console
set shellpipe=>
set nowritebackup
set directory=~/.vim/swap

let g:netrw_banner=0
let g:vim_markdown_folding_disabled=1
let g:vim_json_syntax_conceal=0
let g:ctrlp_custom_ignore={
    \ 'dir': '\v/(vendor|bower_components|public|node_modules)$'}
let g:hardtime_default_on=1
" Disable "-" for figutive
let g:list_of_normal_keys=[
    \ "h", "j", "k", "l", "+",
    \ "<UP>", "<DOWN>", "<LEFT>", "<RIGHT>"]

let mapleader=','
call togglebg#map("<Leader>tb")
nnoremap <C-h> :tabprevious<CR>
nnoremap <C-l> :tabnext<CR>
nnoremap <Tab> <C-w>w
nnoremap <S-Tab> <C-w>W
nnoremap <Esc>h <C-w>h
nnoremap <Esc>j <C-w>j
nnoremap <Esc>k <C-w>k
nnoremap <Esc>l <C-w>l
nnoremap <Esc>c <C-w>c
nnoremap <Space> zz
nnoremap Y y$
nnoremap <C-t> :tabe<Space>
nnoremap <Leader>h :set hlsearch<CR>
nnoremap <Leader>nh :set nohlsearch<CR>
nnoremap <Leader>so :source $MYVIMRC<CR>
nnoremap <C-n> :NERDTreeTabsToggle<CR>
nnoremap <Leader>cc :CtrlPClearCache<CR>
nnoremap <Leader>a :Ack!<Space>
nnoremap <Leader>gg :Git<Space>
nnoremap <Leader>sg :Silent Git<Space>
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gw :Gwrite<CR>
nnoremap <Leader>gd :Gdiff<CR>
nnoremap <Leader>gl :Glog<CR>
" Save and quit
nnoremap <C-s> :w<CR>
inoremap <C-s> <Esc>:w<CR>
vnoremap <C-s> <Esc>:w<CR>
nnoremap <Leader>q :q<CR>
nnoremap <C-q> :qa<CR>
inoremap <C-q> <Esc>:qa<CR>
vnoremap <C-q> <Esc>:qa<CR>
cnoremap w!! w !sudo dd of="%"
" Disable confusing mapping
nnoremap Q <Nop>
nnoremap K <Nop>

set expandtab
set shiftwidth=4
set tabstop=4
autocmd FileType coffee,ruby setlocal shiftwidth=2
autocmd FileType eco,html,jade,less,stylus,sass,scss,json setlocal shiftwidth=2
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

command! -nargs=1 Silent
    \ execute ':silent '.<q-args> | execute ':redraw!'
