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
set scrolloff=4
set nobackup
set nowritebackup
set wildignore+=*.pyc

let g:netrw_list_hide='.*\.swp$'
let g:netrw_banner=0
let g:netrw_browse_split=3
let g:vim_markdown_folding_disabled=1
let g:vim_json_syntax_conceal=0
let g:hardtime_default_on=1
let g:ctrlp_custom_ignore='node_modules$'

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
nnoremap <C-n> :NERDTreeTabsToggle<CR>
nnoremap <C-t> :tabe<Space>
nnoremap <Space> zz
nnoremap <Leader>w :w<CR>
nnoremap <Leader>q :q<CR>
nnoremap <Leader>a :Ack<Space>

autocmd BufWinEnter * set expandtab | set shiftwidth=4
autocmd BufWinEnter *.coffee set shiftwidth=2
autocmd BufWinEnter *.html set shiftwidth=2
autocmd BufWinEnter *.gpp set shiftwidth=2
autocmd BufWinEnter *.jade set shiftwidth=2
autocmd BufWinEnter *.styl set shiftwidth=2
autocmd BufWinEnter *.less set shiftwidth=2
autocmd BufWinEnter *.eco set shiftwidth=2
autocmd BufWinEnter *.rb set shiftwidth=2
autocmd BufWinEnter Gemfile set shiftwidth=2
autocmd BufWinEnter Rakefile set shiftwidth=2
autocmd BufWinEnter Makefile set noexpandtab

autocmd FileType xdefaults set commentstring=!\ %s

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
