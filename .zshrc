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
    if [ -f $ZSH_DIR/plugin/zaw/zaw.zsh ]; then
        source $ZSH_DIR/plugin/zaw/zaw.zsh
    fi
    if [ -f $ZSH_DIR/plugin/autojump/bin/autojump.zsh ]; then
        source $ZSH_DIR/plugin/autojump/bin/autojump.zsh
    fi
fi

if [ -d $ZSH_DIR/functions ]; then
    source $ZSH_DIR/functions/*
fi

# autoload
autoload -Uz compinit
compinit -u
autoload -Uz run-help
autoload -Uz colors
colors

HISTSIZE=100000
SAVEHIST=100000
HISTFILE=$HOME/.zhistory

# shell options
setopt auto_cd              # ディレクトリ名の入力のみで移動する
setopt auto_remove_slash
setopt auto_name_dirs
setopt auto_pushd           # cd -[TAB] でこれまでに移動したディレクトリ一覧を表示
setopt auto_menu            # 補完キー連打で補完候補を順に表示する
#setopt auto_list            # 補完候補を一覧で表示する
setopt pushd_to_home        # 引数なし pushd で $HOME に戻る(直前 dir へは cd - で)
setopt pushd_ignore_dups    # ディレクトリスタックに重複する物は古い方を削除
setopt extended_history     # コマンドの開始時刻と経過時間を登録
setopt hist_ignore_dups     # 直前のコマンドと同一ならば登録しない
setopt hist_ignore_all_dups # 登録済コマンド行は古い方を削除
setopt hist_reduce_blanks   # 余分な空白は詰めて登録(空白数違い登録を防ぐ)
setopt share_history        # ヒストリの共有(append系と異なり再読み込み不要)
setopt hist_no_store        # history コマンドは登録しない
setopt hist_ignore_space    # コマンド行先頭が空白の時登録しない(直後ならば呼べる)
setopt list_packed          # 補完候補リストを詰めて表示
setopt list_types           # 補完候補にファイルの種類も表示する
setopt print_eight_bit      # 補完候補リストの日本語を適正表示
setopt no_clobber           # 上書きリダイレクトの禁止
setopt no_hup               # logout時にバックグラウンドジョブを kill しない
setopt no_beep              # コマンド入力エラーでBEEPを鳴らさない
setopt extended_glob        # 拡張グロブ
setopt numeric_glob_sort    # 数字を数値と解釈して昇順ソートで出力
setopt auto_param_keys
setopt prompt_subst
setopt interactive_comments # コマンド入力中のコメントを認める
setopt always_last_prompt
setopt cdable_vars
setopt sh_word_split
setopt magic_equal_subst
setopt complete_aliases
setopt notify                # バックグラウンドジョブの状態変化を即時報告する
setopt globdots
setopt check_jobs
setopt magic_equal_subst     # =以降も補完する(--prefix=/usrなど)
unsetopt auto_param_slash

zstyle ':completion:*' verbose yes
# 矢印で補完を選択
zstyle ':completion:*:default' menu select=2
zstyle ':completion:*' menu select
zstyle ':completion:*' keep-prefix
zstyle ':completion:*' \
    completer \
    _oldlist \
    _complete \
    _match \
    _ignored \
    #_approximate \
    _list \
    _history
zstyle ':completion:*' group-name ''
# オブジェクトファイルは補完しない
zstyle ':completion:*:*files' ignored-patterns '*?.elc' '*?.o' '*?~' '*\#'
# カレントディレクトリに候補がない場合のみ cdpath 上のディレクトリを候補
zstyle ':completion:*:cd:*' tag-order local-directories path-directories
# 大文字小文字の区別をしない
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
# セパレータ
zstyle ':completion:*' list-separator '=>'
zstyle ':completion:*' format '%F{white}%d%f'

# apt-get や dpkg を速くする
if [ ! -d $ZSH_DIR/cache ]; then
    mkdir -p $ZSH_DIR/cache
fi
zstyle ':completion:*' cache-path $ZSH_DIR/cache
zstyle ':completion:*' use-cache yes
zstyle ':completion:*:date:*' fake \
    '+%Y-%m-%d: 西暦-月-日' \
    '+%Y-%m-%d %H\:%M\:%S: 西暦-月-日 時\:分\:秒'

# alias
alias pu=pushd
alias po=popd
alias dirs='dirs -v'
alias ls='ls --color=auto'
alias ll='ls --color=auto -l ^*~'
alias la='ls --color=auto -a ^*~'
if [ -n "`which trash-put`" ]; then
    alias rm='trash-put'
fi
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias g='git --no-pager'
alias t='tail -f'
alias gterm='gnome-terminal --geometry=130x40'
alias emd='emacs --daemon'
alias emc='emacsclient -t'
alias emn='emacs -nw'
alias emw='emacs'
alias emq='emacs -q --no-site-file'
alias ekill="emacsclient -e '(progn (defun yes-or-no-p (p) t) (kill-emacs))'"
unalias history
alias ha='history -E 1'

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
alias -g S='| sed'
alias -g A='| awk'
alias -g W='| wc'

# 常にバックグラウンドで起動
function gimp() { command gimp $* & }
function firefox() { command firefox $* & }
function xdvi() { command xdvi $* & }
function xpdf() { command xpdf $* & }
function evince() { command evince $* & }
function vlc() { command vlc $* & }
function gitg() { command gitg $* & }

# stty
stty stop undef

# bindkey
bindkey -e
bindkey "^P" history-beginning-search-backward
bindkey "^N" history-beginning-search-forward
#bindkey "^I" menu-complete
bindkey "^I" complete-word
bindkey "\e[Z" reverse-menu-complete  # S-Tabで補完候補を逆順する

# zaw
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-max 5000
zstyle ':chpwd:*' recent-dirs-default yes
zstyle ':completion:*' recent-dirs-insert both
zstyle ':filter-select' case-insensitive yes # 絞り込みをcase-insensitiveに
#bindkey "^@" zaw-cdr
bindkey "^h" zaw-history

# prompt
local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"
RPS1="${return_code} $RPS1"

# SSH ログイン時のプロンプト
[ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
  PROMPT="%{${fg[white]}%}${HOST%%.*} ${PROMPT}";

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

# tmux自動起動
if [ -n "`which tmux`" ]; then
    if [ -z "$TMUX" -a -z "$STY" ]; then
        if type tmux >/dev/null 2>&1; then
            if tmux has-session && tmux list-sessions | /usr/bin/grep -qE '.*]$'; then
                tmux attach && echo "tmux attached session "
            else
                tmux new-session && echo "tmux created new session"
            fi
        fi
    fi
fi
