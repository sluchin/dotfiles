# .zshenv
# initial setup file for both interactive and noninteractive zsh

typeset -U path fpath cdpath manpath

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
    $HOME/.zsh.d/plugin/*/*(N-/)
    $HOME/.zsh.d/plugin/*/*/*(N-/)
    $fpath
)

cdpath=(
    $HOME/devel/*(N-/)
    $HOME/src/*(N-/)
    $HOME
    $cdpath
)

manpath=(
    /usr/*/man(N-/)
    /usr/local/*/man(N-/)
    $manpath
)

fignore=('.elc' '.o' '~' '\#')

export RSYNC_RSH=ssh
export CVS_RSH=ssh
export TERM=xterm
export EDITOR=emacsclient
export VISUAL=emacsclient
export FCEDIT=emacsclient

limit coredumpsize unlimited

ZSHENV_LOCAL=$HOME/.zshenv.local
[[ -f $ZSHENV_LOCAL ]] && source $ZSHENV_LOCAL
