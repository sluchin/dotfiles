# curl -L https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh | sh
# Path to your oh-my-zsh configuration.
ZSH_DIR=$HOME/.zsh.d
ZSH=$ZSH_DIR/oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# robbyrussell agnoster bira bureau candy blinks
ZSH_THEME="bureau"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git github perl symfony2)

if [ -f $ZSH/oh-my-zsh.sh ]; then
    source $ZSH/oh-my-zsh.sh
fi

# curl -O https://raw.github.com/git/git/master/contrib/completion/git-completion.bash
# curl -O https://raw.github.com/git/git/master/contrib/completion/git-completion.zsh
if [ -d $ZSH_DIR/plugin ]; then
    source $ZSH_DIR/plugin/*
fi

if [ -d $ZSH_DIR/functions ]; then
    source $ZSH_DIR/functions/*
fi

# Customize to your needs...
path=(
    $path
    $HOME/bin
    /usr/local/sbin
    /usr/local/bin
    /usr/sbin
    /usr/bin
    /sbin
    /bin
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

# autoload
autoload -U compinit
compinit -u
autoload run-help
autoload -U colors
colors

HISTSIZE=100000
SAVEHIST=100000
HISTFILE=~/.zhistory

# shell options
setopt auto_cd
setopt auto_remove_slash
setopt auto_name_dirs
setopt auto_pushd
setopt auto_menu
setopt auto_param_keys
setopt extended_history
setopt hist_ignore_dups
setopt hist_ignore_space
setopt share_history
setopt prompt_subst
setopt extended_glob
setopt list_types
setopt list_packed
setopt no_beep
setopt always_last_prompt
setopt cdable_vars
setopt sh_word_split
setopt pushd_ignore_dups
setopt magic_equal_subst
setopt complete_aliases
setopt no_clobber
setopt numeric_glob_sort
setopt extended_glob
setopt notify
unsetopt auto_param_slash

# alias
alias pu=pushd
alias po=popd
alias dirs='dirs -v'
alias ls='ls --color=auto'
alias ll='ls --color=auto -l ^*~'
alias la='ls --color=auto -a ^*~'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias g='git --no-pager'
alias e='emacsclient'
alias kille="emacsclient -e '(kill-emacs)'"
alias gnome-terminal='gnome-terminal --geometry=180x60'

alias -s log='tail -f'
alias -s c='emacsclient'
alias -s h='emacsclient'
alias -s cpp='emacsclient'
alias -s php='emacsclient'
alias -s yml='emacsclient'
alias -s el='emacsclient'

alias -g L='| less'
alias -g H='| head'
alias -g T='| tail'
alias -g G='| grep'
alias -g W='| wc'
alias -g S='| sed'
alias -g A='| awk'
alias -g W='| wc'

# stty
stty stop undef

# bindkey
bindkey -e
bindkey "^P" history-beginning-search-backward
bindkey "^N" history-beginning-search-forward

# prompt
local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"
RPS1="${return_code} $RPS1"

# mysql prompt
# https://github.com/tetsujin/zsh-function-mysql
# mysql client user
typeset -A mysql_prompt_style_client_user
mysql_prompt_style_client_user=(
    'root'     $fg_bold[red]
    '*'        $fg_bold[green]
)
# mysql client host
typeset -A mysql_prompt_style_client_host
mysql_prompt_style_client_host=(
    '*.local.*'     "$fg_bold[green]"
    '*.dev.*'       "$fg_bold[yellow]"
    '*'             "$fg_bold[red]"
)
# mysql server user
typeset -A mysql_prompt_style_server_user
mysql_prompt_style_server_user=(
    'root'          "$bg_bold[red]$fg_bold[yellow]"
    '*'             "$fg_bold[blue]"
)
# mysql server host
typeset -A mysql_prompt_style_server_host
mysql_prompt_style_server_host=(
    '*master*'      "$bg_bold[red]$fg_bold[yellow]"  # Master Server
    '*slave*'       "$bg[yellow]$fg[black]" # Slvae Server
    '*'             "$fg_bold[blue]"
)
# mysql prompt style (Should use single quoted string.)
mysql_prompt='${style_client_host}${USER}@${HOST}${fg_bold[white]} -> '
mysql_prompt=$mysql_prompt'${style_server_user}\u${reset_color}${fg_bold[white]}@${style_server_host}\h${reset_color}${fg_bold[white]}:${fg[magenta]}\d ${fg_bold[white]}\v${reset_color}\n'

# tmux
#PS1="$PS1"'$([ -n "$TMUX" ] && tmux setenv TMUXPWD_$(tmux display -p "#D" | tr -d %) "$PWD")'

# tmux自動起動
if [ -n `which tmux` -a -z "$TMUX" -a -z "$STY" ]; then
    if type tmux >/dev/null 2>&1; then
        if tmux has-session && tmux list-sessions | /usr/bin/grep -qE '.*]$'; then
            tmux attach && echo "tmux attached session "
        else
            tmux new-session && echo "tmux created new session"
        fi
    fi
fi

# emacsclient
if [ -n `which emacs` -a -n `pgrep emacs >/dev/null 2>&1` ]; then
    echo "Emacs server is already running..."
else
    `emacs --daemon`
fi
