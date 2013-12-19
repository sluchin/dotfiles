# .zshenv
# initial setup file for both interactive and noninteractive zsh

path=(
    $path
    $HOME/bin(N-/)
    /usr/local/sbin(N-/)
    /usr/local/bin(N-/)
    /usr/local/*/bin(N-/)
    /usr/sbin(N-/)
    /usr/bin(N-/)
    /usr/*/bin(N-/)
    /sbin(N-/)
    /bin(N-/)
)

fpath=(
    $fpath
    $HOME/.zsh.d/*(/N)
)

cdpath=(
    $cdpath
    ..
    $HOME
    $HOME/src
    $HOME/devel
)

typeset -U path fpath cdpath manpath
fignore=('.elc' '.o' '~' '\#')

export RSYNC_RSH=ssh
export CVS_RSH=ssh
export EDITOR=emacsclient
export VISUAL=emacsclient
export TERM=xterm

limit coredumpsize unlimited
