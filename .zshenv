# .zshenv
# initial setup file for both interactive and noninteractive zsh

path=(
    $HOME/bin(N-/)
    /usr/local/sbin(N-/)
    /usr/local/bin(N-/)
    /usr/local/*/bin(N-/)
    /usr/sbin(N-/)
    /usr/bin(N-/)
    /usr/*/bin(N-/)
    /sbin(N-/)
    /bin(N-/)
    $path
)

fpath=(
    $HOME/.zsh.d/*(N-/)
    $HOME/.zsh.d/functions/*(N-/)
    $HOME/.zsh.d/plugin/*(N-/)
    $fpath
)

cdpath=(
    ..
    $HOME
    $HOME/src
    $HOME/devel
    $cdpath
)

typeset -U path fpath cdpath manpath
fignore=('.elc' '.o' '~' '\#')

export RSYNC_RSH=ssh
export CVS_RSH=ssh
export TERM=xterm
export EDITOR=emacsclient
export VISUAL=emacsclient

limit coredumpsize unlimited
