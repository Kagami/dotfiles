# To prevent leaking temporary variables inside the shell.
function {

[ -f ~/.zprofile ] && . ~/.zprofile
# To make C-s/C-q work.
stty start undef
stty stop undef

# setopt EXTENDED_GLOB  # Too many false triggering: HEAD^, bootstrap#2.3.2
setopt AUTO_CD
setopt AUTO_PUSHD
setopt PUSHD_MINUS
setopt BRACE_CCL
setopt NO_PROMPT_CR
autoload -U zmv

HISTFILE=~/.zshhistory
HISTSIZE=100000
SAVEHIST=100000
setopt INC_APPEND_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS

autoload -U compinit && compinit
[ -f /usr/share/zsh/site-functions/go ] && . /usr/share/zsh/site-functions/go
zmodload zsh/complist
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

WORDCHARS='*?_.-[]~=&;!#$%^(){}<>'
zstyle ':completion:*' menu yes select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*:default' list-colors '${LS_COLORS}'
zstyle ':completion:*:cd:*' ignore-parents parent pwd

autoload -U colors && colors
reset=%{$reset_color%}
blue=%{$fg_no_bold[blue]%}
BLUE=%{$fg_bold[blue]%}
green=%{$fg_no_bold[green]%}
GREEN=%{$fg_bold[green]%}
yellow=%{$fg_no_bold[yellow]%}
YELLOW=%{$fg_bold[yellow]%}
red=%{$fg_no_bold[red]%}
RED=%{$fg_bold[red]%}
white=%{$fg_no_bold[white]%}
WHITE=%{$fg_bold[white]%}

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git*:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr "${green}M"
zstyle ':vcs_info:*' unstagedstr "${red}M"
zstyle ':vcs_info:git*' formats "$WHITE%b$reset:%c%u"

host=`print -P %m`
function precmd {
    vcs_info
    local -a parts
    parts=( "$blue╭($green%~$blue) ($RED${host:u}$blue)" )
    local vcs_part=${vcs_info_msg_0_%:}
    [ $vcs_part ] && parts+=( " ($vcs_part$blue)" )
    parts+=( $'\n' "╰$ $reset" )
    PROMPT=${(j::)parts}
}

alias ls='ls -F --color'
alias ls0='command ls -F'
alias lsa='ls -A'
alias ll='ls -lh'
alias ll0='command ls -Flh'
alias lla='ls -Alh'
alias lr='ls -rt'
alias lsd='ls -d'

alias v='vim'
alias sv='sudo -E vim'
alias mv='mv -i'
alias cp='cp -i'
alias rm='rm -I'
alias df='df -h'
alias du='du -sh'
alias free='free -m'
alias cal='cal -m'
alias enc='enconv -L ru -x utf8'
alias scr='screen'
alias am='alsamixer'
alias m='mpv'
alias f='feh -drFSfilename'
alias yo='noglob youtube-dl -t'
alias myo='noglob mplayer-youtube-dl'
alias vv='vim ~/.vimrc'
alias vz='vim ~/.zshrc'
alias vx='vim ~/.xmonad/xmonad.hs -c "cd ~/.xmonad"'
alias z='source ~/.zshrc'
alias gdot='git --git-dir=$HOME/code/dotfiles.git --work-tree=$HOME'
alias gdup='gdot pull && gdot submodule init && gdot submodule update'
alias brunch='node_modules/.bin/brunch'
alias grunt='node_modules/.bin/grunt'
alias bower='node_modules/.bin/bower'
alias bowls='bower list --offline'
alias bowlsp='bower list --paths'
alias dir='dirs -v'
alias psgrep='ps aux | grep'
alias zmv='noglob zmv -W'
alias net='netstat -anp L'

local LESS_VIM='vim -c "set nomodifiable" -c "nnoremap q :q<CR>"'
alias l="${LESS_VIM} -R"
alias -g L="|${LESS_VIM} -c 'set nomodified' -"

if which exo-open &>/dev/null; then
    alias o='exo-open'
else
    alias o='xdg-open'
fi
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
alias ei='eix -I'
alias em='sudo emerge'
alias eq='equery'
alias epv='emerge -pv'
alias eav='sudo emerge -av'
alias edel='sudo emerge -Ca'
alias ecl='sudo emerge -ca'
alias esync='sudo eix-sync -w'
alias eworld='sudo emerge -uDNav world'
alias edeep='sudo emerge -uDNav world --with-bdeps=y --complete-graph=y'

alias g='git'
alias ga='git add'
alias gc='git commit'
alias gs='git status'
alias gsh='git show'
alias gl='git log -3'
alias gd='git diff'
alias gdc='git diff --cached'
alias gdh='git diff HEAD'
alias gco='git checkout'
alias gb='git branch'
alias gcl='git clone'
alias gush='git push'
alias gull='git pull'
alias gr='git remote'
alias gch='git cherry-pick'
alias gm='git merge --no-ff'
alias gmt='git mergetool --no-prompt'
alias grb='git rebase'

alias -g LL='|less'
alias -g H='|head'
alias -g T='|tail'
alias -g G='|grep'
alias -g W='|wc -l'

hash -d hdd=/media/hdd

###
# Special keys. See
# <https://wiki.archlinux.org/index.php/Zsh#Key_Bindings> for details.
###

# create a zkbd compatible hash;
# to add other keys to this hash, see: man 5 terminfo
local -A key

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

}

###
# Helpers.
###

function lcd {
    cd $1 && ls
}

function rcd {
    cd $1 && ls -rt
}

function mcd {
    [ $1 ] && mkdir $1 && cd $1
}

# Info about occupied space.
function dus {
    find $1 -mindepth 1 -maxdepth 1 |\
        while read file; do
            command du -sh ${file#./}
        done |\
        sort -h
}

# Show package's ebuild.
function eqw {
    [ $1 ] || return
    local file=`equery which $1`
    [ $file ] && l $file
}

# Clear dynamic terminal title.
function rst {
    cd; echo -n '\e]0;Terminal\a'; clear
}

# Add sed-like functionality to ack.
function sack {
    if [[ $# -lt 2 ]]; then
        echo "Usage: sack <PERL EXPRESSION> <ACK OPTIONS>" >&2
        return 1
    fi
    local PERL_EXPR=$1
    shift
    local -a FILES_TO_REPLACE
    FILES_TO_REPLACE=("${(@f)$(ack -l $*)}")
    if [[ -z $FILES_TO_REPLACE ]]; then
        echo "ack doesn't find anything" >&2
        return 1
    fi
    perl -i -p -e $PERL_EXPR $FILES_TO_REPLACE
}

# Hack to run commands without closing the shell.
# See <http://superuser.com/q/91881> for details.
if [[ $1 == eval ]]; then
    "$@"
    set --
fi
