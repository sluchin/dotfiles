### .zshenv -*- mode: Shell-script; coding: utf-8; indent-tabs-mode: nil -*-
# initial setup file for both interactive and noninteractive zsh

typeset -xT LD_LIBRARY_PATH ld_library_path
typeset -xT PYTHONPATH pythonpath
typeset -U path fpath cdpath manpath pythonpath

path=(
    $HOME/bin(N-/)
    $HOME/bin/*(N-/)
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

ld_library_path=(
    .
    $HOME/lib(N-/)
    $HOME/lib/*(N-/)
    /usr/local/lib(N-/)
    /usr/local/*/lib(N-/)
    /usr/lib(N-/)
    /usr/lib/*(N-/)
    /usr/*/lib/(N-/)
    $ld_library_path
)

fignore=('.elc' '.o' '~' '\#')

pythonpath=(
    .
    /usr/local/lib/python*/site-packages(N-/)
    /usr/lib/python*/site-packages(N-/)
    /usr/share/pyshared(N-/)
    $pythonpath
)

export LANG=ja_JP.UTF-8
export RSYNC_RSH=ssh
export CVS_RSH=ssh
export TERM=xterm
export EDITOR=emacsclient
export VISUAL=$EDITOR
export FCEDIT=$EDITOR

limit coredumpsize unlimited

ZSHENV_LOCAL=$HOME/.zshenv.local
[[ -f $ZSHENV_LOCAL ]] && source $ZSHENV_LOCAL
