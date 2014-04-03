#
# ~/.bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# initialize keychain if existing on system
if [[ -e "/usr/bin/keychain" ]]
then
    keychain ~/.ssh/id_rsa
fi

shopt -s histappend
PROMPT_COMMAND='history -a'

alias grep='grep --color=always'
alias ls='ls --color=auto'
alias emacs='emacs -nw'

export HISTIGNORE="&:ls:[bf]g:exit"
PS1='[\u@\h \W]\$ '

export EDITOR=emacs
export VISUAL=emacs
export LANG="en_US.UTF-8"
