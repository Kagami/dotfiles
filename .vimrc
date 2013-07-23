set nocompatible
execute pathogen#infect()
filetype plugin indent on
set rtp+=~/.vim/bundle/powerline/powerline/bindings/vim

set t_Co=16
syntax enable
set background=light
let g:solarized_termtrans=1
colorscheme solarized

set autoindent
set wildmenu
set nofoldenable
set ignorecase
set incsearch
set nohlsearch
set laststatus=2
set noshowmode
set colorcolumn=80
set nobackup
set nowritebackup
let g:netrw_list_hide='.*\.swp$'
let g:netrw_banner=0
let g:netrw_browse_split=3
let g:vim_markdown_folding_disabled=1
let g:vim_json_syntax_conceal=0
let g:NERDTreeWinSize=29
let g:hardtime_default_on=1

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
nnoremap <leader>a :Ack<Space>

au BufWinEnter * set expandtab | set tabstop=4 | set shiftwidth=4
au BufWinEnter *.coffee set tabstop=2 | set shiftwidth=2
au BufWinEnter *.html set tabstop=2 | set shiftwidth=2
au BufWinEnter *.gpp set tabstop=2 | set shiftwidth=2
au BufWinEnter *.jade set tabstop=2 | set shiftwidth=2
au BufWinEnter *.styl set tabstop=2 | set shiftwidth=2
au BufWinEnter *.less set tabstop=2 | set shiftwidth=2
au BufWinEnter *.eco set tabstop=2 | set shiftwidth=2
au BufWinEnter *.rb set tabstop=2 | set shiftwidth=2
au BufWinEnter Gemfile set tabstop=2 | set shiftwidth=2
au BufWinEnter Rakefile set tabstop=2 | set shiftwidth=2
au BufWinEnter Makefile set noexpandtab

highlight ExtraWhitespace ctermbg=red guibg=red
au BufWinEnter * match ExtraWhitespace /\s\+$/
au InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
au InsertLeave * match ExtraWhitespace /\s\+$/
au BufWinLeave * call clearmatches()

set langmap=ёйцукенгшщзхъфывапролджэячсмитьбю;`qwertyuiop[]asdfghjkl\;'zxcvbnm\\,.,ЙЦУКЕHГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ;QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>

if ! has('gui_running')
    set ttimeoutlen=10
    augroup FastEscape
        autocmd!
        au InsertEnter * set timeoutlen=0
        au InsertLeave * set timeoutlen=1000
    augroup END
endif
