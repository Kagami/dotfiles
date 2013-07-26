[ -f ~/.zprofile ] && . ~/.zprofile
setopt NO_PROMPT_CR
HISTFILE=~/.zsh_history
HISTSIZE=50000
SAVEHIST=50000
setopt INC_APPEND_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS

WORDCHARS='*?_.-[]~=&;!#$%^(){}<>'
autoload -U compinit && compinit
zmodload zsh/complist
zstyle ':completion:*' menu yes select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*:default' list-colors '${LS_COLORS}'
zstyle ':completion:*:cd:*' ignore-parents parent pwd
setopt BRACE_CCL

bindkey -e
bindkey -M menuselect '^M' .accept-line
bindkey -M menuselect '/' accept-and-infer-next-history
bindkey -M menuselect '^H' accept-and-menu-complete
bindkey -M menuselect '^U' undo
bindkey -M menuselect 'h' backward-char
bindkey -M menuselect 'j' down-line-or-history
bindkey -M menuselect 'k' up-line-or-history
bindkey -M menuselect 'l' forward-char
bindkey '^T' history-incremental-search-forward

autoload -U colors && colors
blue=%{$fg_no_bold[blue]%}
green=%{$fg_no_bold[green]%}
redb=%{$fg_bold[red]%}
rc=%{$reset_color%}
host=`print -P %m | tr '[:lower:]' '[:upper:]'`
PROMPT="$blue╭($green%~$blue) ($redb$host$blue)
╰$ $rc"

function lcd() {
    cd "$1" && ls -F --color
}
function rcd() {
    cd "$1" && ls -rtF --color
}
function mcd() {
    mkdir "$1" && cd "$1"
}
function dus() {
    if [ -n "$1" ]; then
        files=`find "$1" -mindepth 1 -maxdepth 1`
    else
        files=`find -mindepth 1 -maxdepth 1`
    fi
    echo -n "$files" | sed 's#^\./##' | xargs -d '\n' du -sh | sort -h
}

alias ls='ls -F --color'
alias ls0='/bin/ls -F'
alias lsa='ls -A'
alias ll='ls -lh'
alias ll0='/bin/ls -Flh'
alias lla='ls -Alh'
alias lr='ls -rt'

alias l='vim -R'
alias v='vim'
alias mv='mv -i'
alias cp='cp -i'
alias df='df -h'
alias du='du -sh'
alias free='free -m'
alias cal='cal -m'
alias enc='enconv -L ru -x utf8'
alias c='clear'
alias scr='screen'
alias m='mplayer'
alias f='feh -drFSfilename'
alias o='exo-open'
alias yo='youtube-dl -t'
alias vv='vim ~/.vimrc'
alias vz='vim ~/.zshrc'
alias vx='vim ~/.xmonad/xmonad.hs'
alias z='source ~/.zshrc'
alias gdot='git --git-dir=$HOME/code/dotfiles.git --work-tree=$HOME'
alias gdup='gdot pull && gdot submodule init && gdot submodule update'
if ! which hd &>/dev/null; then
    alias hd='hexdump -C'
fi

alias apts='apt-cache search'
alias aptsh='apt-cache show'
alias apti='sudo apt-get install'
alias aptr='sudo apt-get remove'
alias aptp='sudo apt-get purge'
alias aptc='sudo apt-get clean'
alias aptu='sudo apt-get update'
alias aptauto='sudo apt-get autoremove'

alias e='eix'
alias eq='equery'
alias epv='emerge -pv'
alias eav='sudo emerge -av'
alias edel='sudo emerge -Ca'
alias ecl='sudo emerge -ca'
alias eworld='sudo emerge -uDNav world'

alias g='git'
alias ga='git add'
alias gc='git commit'
alias gs='git status'
alias gl='git log -3'
alias gd='git diff'
alias gco='git checkout'
alias gb='git branch'
alias gcl='git clone'
alias gush='git push'
alias gull='git pull'

alias -g L='|vim - -c "set nomodified"'
alias -g LL='|less'
alias -g H='|head'
alias -g T='|tail'
alias -g G='|grep'
alias -g W='|wc -l'

hdd=/media/hdd

###
# Special keys
# See <https://wiki.archlinux.org/index.php/Zsh#Key_Bindings>
###

# create a zkbd compatible hash;
# to add other keys to this hash, see: man 5 terminfo
typeset -A key

key[Home]=${terminfo[khome]}

key[End]=${terminfo[kend]}
key[Insert]=${terminfo[kich1]}
key[Delete]=${terminfo[kdch1]}
key[Up]=${terminfo[kcuu1]}
key[Down]=${terminfo[kcud1]}
key[Left]=${terminfo[kcub1]}
key[Right]=${terminfo[kcuf1]}
key[PageUp]=${terminfo[kpp]}
key[PageDown]=${terminfo[knp]}

# setup key accordingly
[[ -n "${key[Home]}"    ]]  && bindkey  "${key[Home]}"    beginning-of-line
[[ -n "${key[End]}"     ]]  && bindkey  "${key[End]}"     end-of-line
[[ -n "${key[Insert]}"  ]]  && bindkey  "${key[Insert]}"  overwrite-mode
[[ -n "${key[Delete]}"  ]]  && bindkey  "${key[Delete]}"  delete-char
[[ -n "${key[Up]}"      ]]  && bindkey  "${key[Up]}"      up-line-or-history
[[ -n "${key[Down]}"    ]]  && bindkey  "${key[Down]}"    down-line-or-history
[[ -n "${key[Left]}"    ]]  && bindkey  "${key[Left]}"    backward-char
[[ -n "${key[Right]}"   ]]  && bindkey  "${key[Right]}"   forward-char

# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
    function zle-line-init () {
        printf '%s' ${terminfo[smkx]}
    }
    function zle-line-finish () {
        printf '%s' ${terminfo[rmkx]}
    }
    zle -N zle-line-init
    zle -N zle-line-finish
fi
