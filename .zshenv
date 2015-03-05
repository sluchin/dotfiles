### .zshenv -*- mode: Shell-script; coding: utf-8; indent-tabs-mode: nil -*-
# initial setup file for both interactive and noninteractive zsh

typeset -T PYTHONPATH pythonpath
typeset -U path fpath cdpath manpath pythonpath

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
    /usr/local/share/zsh/*/functions*(N-/)
    /usr/share/zsh/*/functions*(N-/)
    $HOME/.zsh.d/*(N-/)
    $HOME/.zsh.d/functions/*(N-/)
    $HOME/.zsh.d/completion*(N-/)
    $HOME/.zsh.d/completion/zsh-completions/src*(N-/)
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

fignore=('.elc' '.o' '~' '\#')

pythonpath=(
    .
    /usr/lib/python*/site-packages(N-/)
    /usr/local/lib/python*/site-packages(N-/)
    /usr/share/pyshared(N-/)
    $pythonpath
)

export RSYNC_RSH=ssh
export CVS_RSH=ssh
export TERM=xterm
export EDITOR=emacsclient
export VISUAL=emacsclient
export FCEDIT=emacsclient

limit coredumpsize unlimited

LANG=ja_JP.UTF-8
export LANG

ZSHENV_LOCAL=$HOME/.zshenv.local
[[ -f $ZSHENV_LOCAL ]] && source $ZSHENV_LOCAL
