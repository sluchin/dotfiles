### .zlogin -*- mode: Shell-script; coding: utf-8; indent-tabs-mode: nil -*-
# Read in after the .zshrc file when you log in.

typeset -U manpath

manpath=(
    /usr/*/man(N-/)
    /usr/local/*/man(N-/)
    $manpath
)

export JLESSCHARSET=japanese
