# .zshenv
# initial setup file for both interactive and noninteractive zsh

limit coredumpsize 0
typeset -U path
path=($path /usr/*/bin(N-/) /usr/local/*/bin/(N-/) /var/*/bin/(N-/) $HOME/.cabal/bin(N-/))
export RSYNC_RSH=ssh
export CVS_RSH=ssh
export EDITOR=emacsclient
export VISUAL=emacsclient
