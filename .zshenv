# .zshenv
# initial setup file for both interactive and noninteractive zsh

limit coredumpsize unlimited
typeset -U path
path=($HOME/bin(N-/) $path /usr/*/bin(N-/) /usr/local/*/bin/(N-/) /var/*/bin/(N-/) $HOME/.cabal/bin(N-/) /usr/local/lib/idea/bin)
export RSYNC_RSH=ssh
export CVS_RSH=ssh
export EDITOR=emacsclient
export VISUAL=emacsclient
