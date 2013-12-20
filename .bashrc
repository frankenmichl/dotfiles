#
# ~/.bashrc
#
shopt -s histappend
PROMPT_COMMAND='history -a'

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
alias grep='grep --color=always'
alias ls='ls --color=auto'
alias emacs='emacs -nw'
export HISTIGNORE="&:ls:[bf]g:exit"
PS1='[\u@\h \W]\$ '

export EDITOR=vim
export VISUAL=vim
export LANG="en_US.UTF-8"

export ARFLAGS="rv"

#PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD}\007"'
