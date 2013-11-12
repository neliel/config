## .bashrc

# -----------------------------------------------------------------------------#
# Main conf

# Source global definitions
[ -f /etc/bashrc ] && . /etc/bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto --classify'
alias ll='ls --format=verbose --human-readable'

# -----------------------------------------------------------------------------#
## Main colors

txtblk='\e[0;30m' # Black - Regular
txtred='\e[0;31m' # Red
txtgrn='\e[0;32m' # Green
txtylw='\e[0;33m' # Yellow
txtblu='\e[0;34m' # Blue
txtpur='\e[0;35m' # Purple
txtcyn='\e[0;36m' # Cyan
txtwht='\e[0;37m' # White
bldblk='\e[1;30m' # Black - Bold
bldred='\e[1;31m' # Red
bldgrn='\e[1;32m' # Green
bldylw='\e[1;33m' # Yellow
bldblu='\e[1;34m' # Blue
bldpur='\e[1;35m' # Purple
bldcyn='\e[1;36m' # Cyan
bldwht='\e[1;37m' # White
unkblk='\e[4;30m' # Black - Underline
undred='\e[4;31m' # Red
undgrn='\e[4;32m' # Green
undylw='\e[4;33m' # Yellow
undblu='\e[4;34m' # Blue
undpur='\e[4;35m' # Purple
undcyn='\e[4;36m' # Cyan
undwht='\e[4;37m' # White
bakblk='\e[40m'   # Black - Background
bakred='\e[41m'   # Red
bakgrn='\e[42m'   # Green
bakylw='\e[43m'   # Yellow
bakblu='\e[44m'   # Blue
bakpur='\e[45m'   # Purple
bakcyn='\e[46m'   # Cyan
bakwht='\e[47m'   # White
txtrst='\e[0m'    # Text Reset

# -----------------------------------------------------------------------------#
## Main aliases

alias grep='grep --color=auto'

alias vi='vim'
alias del='rm -rf'

alias scr='screen -r'
alias ssh='ssh -C4'

# -----------------------------------------------------------------------------#
## Alt aliases

# Editors
alias gv='gvim'
alias emacs='/comp/emacs-24.3/bin/emacs'
alias emacsclient='/comp/emacs24.3/bin/emacsclient'
alias em='emacs'

# Applications
alias scr='screen -r'
alias lscrt='screen -S rtorrent rtorrent'
alias scrt='screen -r rtorrent'
alias fetchmail='fetchmail -v >> ~/Documents/log/fetchmail.log 2>&1'
alias setkb='setxkbmap fr bepo'

# Permissions
alias pshow='chmod u+rwx ~/Téléchargements/.restricted'
alias phide='chmod u-rwx ~/Téléchargements/.restricted'

# Places
alias cdv='cd ~/Workplace'
alias cdb='cd ~/Bureau'
alias cdt='cd ~/Téléchargements'

# -----------------------------------------------------------------------------#
## Main exports

[ -d ~/.opt/bin ] && PATH=$PATH:~/.opt/bin
export PATH

export EDITOR=vim
export PAGER=less

export SENDMAIL=msmtp

export HISTCONTROL=ignoredups # Ne pas mettre en double ds l'hist
export HISTSIZE=5000          # Lignes de l'hist par session bash
export HISTFILESIZE=20000     # Lignes de l'hist conservées

# -----------------------------------------------------------------------------#
## Alt exports

[ -d /comp/ghc-7.6.3 ] && PATH=$PATH:/comp/ghc-7.6.3/bin
[ -d /comp/haskell-platform-2013.2.0.0 ] && PATH=$PATH:/comp/haskell-platform-2013.2.0.0/bin
[ -d ~/.cabal ] && PATH=$PATH:~/.cabal/bin
export PATH

# -----------------------------------------------------------------------------#
## Main PS1

if [ -n "$SSH_CLIENT" ]; then
    PS1="\[$txtylw\][\u@\h \W]\$ \[$txtrst\]"
elif [ ${EUID} == 0 ]; then
    PS1="\[$txtred\][\u@\h \W]\$ \[$txtrst\]"
else
    PS1="\[$txtwht\]\$? \$(if [ \$? == 0 ]; then echo \"\[$txtgrn\]\342\234\223\"; else echo \"\[$txtred\]\342\234\227\"; fi) \[$txtcyn\]\u\[$txtrst\] ( \[$txtblu\]\w\[$txtrst\] ) $ "
fi

# -----------------------------------------------------------------------------#
#0
