## .bashrc

## Source global definitions
[ -f /etc/bashrc ] && . /etc/bashrc

## If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto --classify'
alias ll='ls --format=verbose --human-readable'

. ~/.bash_source
. ~/.bash_source_tm

#EOF
