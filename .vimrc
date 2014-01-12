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
set hlsearch
set laststatus=2
set noshowmode
set colorcolumn=81
set wildignore+=*.pyc
set cpoptions+=Z
set nomodeline
set backspace=indent,eol,start
set lazyredraw
set fileencoding=utf-8
set autoread
set sessionoptions-=options
set shellpipe=>  " Do not leak ack.vim output to console
set nowritebackup
set directory=~/.vim/swap
set viminfo=

let g:netrw_banner=0
let g:vim_markdown_folding_disabled=1
let g:vim_json_syntax_conceal=0
let g:ctrlp_use_caching=0
let g:ctrlp_custom_ignore={
    \ 'dir':
    \ '\v/(.*\.egg-info|vendor|bower_components|public|build|node_modules)$'}
let g:hardtime_default_on=1
" Disable "-" for figutive
let g:list_of_normal_keys=[
    \ "h", "j", "k", "l", "+",
    \ "<UP>", "<DOWN>", "<LEFT>", "<RIGHT>"]
let g:UltiSnipsEditSplit='horizontal'

" Basic hotkeys.
let mapleader=','
nnoremap Y y$
nnoremap <Space> zz
nnoremap <C-n> :NERDTreeTabsToggle<CR>
nnoremap <Leader>e :e<Space>
nnoremap <Leader>h :set hlsearch! hlsearch?<CR>
nnoremap <Leader>n :nohlsearch<CR>
nnoremap <Leader>so :source $MYVIMRC<CR>
nnoremap <Leader>a :Ack!<Space>
nnoremap <Leader>se :UltiSnipsEdit<CR>
call togglebg#map("<Leader>tb")  " Make solarized light or dark
" Tabs.
nnoremap <C-h> :tabprevious<CR>
nnoremap <C-l> :tabnext<CR>
nnoremap <Leader>te :tabe<Space>
nnoremap <Leader>tn :tabn<Space>
nnoremap <Leader>tm :tabm<Space>
" Windows.
nnoremap <Tab> <C-w>w
nnoremap <S-Tab> <C-w>W
" NOTE: A-<hjkl> doesn't work in all terminals so use hack with ESC
" instead to map M-<hjkl> hotkeys.
nnoremap <Esc>h <C-w>h
nnoremap <Esc>j <C-w>j
nnoremap <Esc>k <C-w>k
nnoremap <Esc>l <C-w>l
nnoremap <Esc>c <C-w>c
" Fugitive.
nnoremap <Leader>gg :Git<Space>
nnoremap <Leader>sg :Silent Git<Space>
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gw :Gwrite<CR>
nnoremap <Leader>gdf :Gdiff<CR>
nnoremap <Leader>gdd :Silent Git diff<CR>
nnoremap <Leader>gdc :Silent Git diff --cached<CR>
nnoremap <Leader>gdh :Silent Git diff HEAD<CR>
nnoremap <Leader>gl :Silent Git log -p<CR>
nnoremap <Leader>gh :Silent Git show<CR>
nnoremap <Leader>gc :Gcommit -m ''<LEFT>
nnoremap <Leader>gac :Gcommit -am ''<LEFT>
" Save & quit.
nnoremap <C-s> :w<CR>
inoremap <C-s> <Esc>:w<CR>
vnoremap <C-s> <Esc>:w<CR>
nnoremap <C-q> :qa<CR>
inoremap <C-q> <Esc>:qa<CR>
vnoremap <C-q> <Esc>:qa<CR>
nnoremap <Leader>q :q<CR>
" Disable confusing hotkeys.
nnoremap Q <Nop>
nnoremap K <Nop>

set expandtab
set shiftwidth=4
set tabstop=4
autocmd FileType coffee,ruby setlocal shiftwidth=2
autocmd FileType eco,html,jade,less,stylus,sass,scss,json setlocal shiftwidth=2
autocmd BufWinEnter *.gpp setlocal shiftwidth=2
autocmd FileType make,gitconfig setlocal noexpandtab
autocmd FileType gitcommit setlocal colorcolumn=51
autocmd FileType xdefaults setlocal commentstring=!\ %s
autocmd FileType c,cpp setlocal commentstring=//\ %s
autocmd BufNewFile,BufRead * setlocal formatoptions-=ro

highlight ExtraWhitespace ctermbg=red guibg=red
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

set langmap=ФИСВУАПРШОЛДЬТЩЗЙКЫЕГМЦЧНЯ;ABCDEFGHIJKLMNOPQRSTUVWXYZ,фисвуапршолдьтщзйкыегмцчня;abcdefghijklmnopqrstuvwxyz

" Update statusline immediately.
" Source: <https://powerline.readthedocs.org/en/latest/tipstricks.html>.
if ! has('gui_running')
    set ttimeoutlen=10
    augroup FastEscape
        autocmd!
        autocmd InsertEnter * set timeoutlen=0
        autocmd InsertLeave * set timeoutlen=1000
    augroup END
endif

" Redraw window after executing provided command in silent mode.
" Without redraw ':silent' often corrupts the screen.
command! -nargs=1 Silent
    \ execute ':silent '.<q-args> | execute ':redraw!'
