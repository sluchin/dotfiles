# .zshrc
# initial setup file for only interactive zsh

HISTSIZE=100000
SAVEHIST=100000
HISTFILE=~/.zhistory
PROMPT='%m{%n}%% '
RPROMPT='[%~] '

# shell options
setopt auto_cd
setopt auto_remove_slash
setopt auto_name_dirs
setopt extendid_history
setopt hist_ignore_dups
setopt hist_ignore_space
setopt prompt_subst
setopt extended_glob
setopt list_types
setopt no_beep
setopt always_last_prompt
setopt cdable_vars
setopt sh_word_split
setopt auto_param_keys
setopt pushd_ignore_dups

# alias
alias pu=pushd
alias popd=po
alias dirs='dirs -v'
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

bindkey -e

zstyle ':completion:*' format '%BCompleting %d%b'
zstyle ':completion:*' group-name ''

autoload -U compinit && compinit
