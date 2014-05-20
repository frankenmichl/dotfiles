#
# ~/.bashrc
#
shopt -s histappend
PROMPT_COMMAND='history -a'

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups
# ... and ignore same sucessive entries.
export HISTCONTROL=ignoreboth

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return



export HISTIGNORE="&:ls:[bf]g:exit"
PS1='[\u@\h \W]\$ '

export EDITOR=emacs
export VISUAL=emacs
export GIT_EDITOR=emacs
export PAGER=less
export SHELL=/bin/bash
export LANG="en_US.UTF-8"

# path definition
PATH=$PATH:/opt/GuitarPro6
PATH=$PATH:/opt/arm-2010q1/bin
PATH=$PATH:/usr/local/bin
PATH=$PATH:/opt/OSELAS.Toolchain-2013.12.2/arm-cortexm3-uclinuxeabi/gcc-4.8.2-uclibc-0.9.33.2-binutils-2.24-kernel-3.12-sanitized/bin
export PATH=$PATH:~/bin

PROMPT_COMMAND='echo -ne "\033]0;${USER}: ${PWD}\007"'

# aliases
alias gp6='schroot /opt/GuitarPro6/launcher.sh'
alias grep='grep --color=always'
alias ls='ls --color=auto --group-directories-first --quoting-style=shell'
alias emacs='emacs -nw'
