set nocompatible
execute pathogen#infect()
filetype plugin indent on
set rtp+=~/.vim/bundle/powerline/powerline/bindings/vim

syntax enable
set background=light
let g:solarized_termtrans=1
let g:solarized_termcolors=256
colorscheme solarized

set autoindent
set wildmenu
set nofoldenable
set incsearch
set nohlsearch
set laststatus=2
set noshowmode
set nobackup
set nowritebackup
let g:netrw_list_hide='.*\.swp$'
let g:netrw_banner=0
let g:netrw_browse_split=3
let g:vim_markdown_folding_disabled=1

map <C-j> <C-e>j
map <C-k> <C-y>k
map <C-h> :tabprevious<CR>
map <C-l> :tabnext<CR>
map <Tab> <C-w>w
map <S-Tab> <C-w>W
map <A-h> <C-w>h
map <A-j> <C-w>j
map <A-k> <C-w>k
map <A-l> <C-w>l
map <C-n> :NERDTreeTabsToggle<CR>

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
