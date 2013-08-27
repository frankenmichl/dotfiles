
if [ "$TERM" != "dumb" ] && [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    alias dir='ls --color=auto --format=vertical'
    alias vdir='ls --color=auto --format=long'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases                                                                                                                                      
alias ls="ls --color=auto"
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'
alias emacs="emacs -nw"

alias rebuild="make distclean && ./configure --target-list=i386-softmmu && make -j8"