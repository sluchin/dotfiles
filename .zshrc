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

# curl -O https://raw.github.com/git/git/master/contrib/completion/git-completion.bash
# curl -O https://raw.github.com/git/git/master/contrib/completion/git-completion.zsh

# autoload
autoload -Uz compinit
compinit -u
autoload -Uz run-help
autoload -Uz colors
colors
autoload -Uz zmv

plugins=(git github perl symfony2)

if [ -f $ZSH/oh-my-zsh.sh ]; then
    source $ZSH/oh-my-zsh.sh
fi

# zaw
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs

if [ -d $ZSH_DIR ]; then
    files=(
        #$ZSH_DIR/plugin/incr-0.2.zsh
        $ZSH_DIR/plugin/zaw/zaw.zsh
        $ZSH_DIR/plugin/autojump/bin/autojump.zsh
        $ZSH_DIR/functions/mysql
    )
    for file in $files
    do
        if [ -f $file ]; then
            #echo $file
            source $file
        fi
    done
fi

# zaw 関数上書き
function zaw-src-cdr () {
    setopt local_options extended_glob
    : ${(A)candidates::=${${(f)"$(cdr -l | awk '{ print $2 }')"}##<-> ##}}
    actions=(zaw-src-cdr-cd zaw-src-cdr-insert zaw-src-cdr-prune)
    act_descriptions=("cd" "insert" "prune")
    options+=(-m)
}

zle -N zaw-src-cdr

HISTSIZE=100000
SAVEHIST=100000
HISTFILE=$HOME/.zhistory

# shell options
setopt auto_cd              # ディレクトリ名の入力のみで移動する
setopt auto_remove_slash    # スラッシュの削除
setopt auto_pushd           # cd -[TAB] でこれまでに移動したディレクトリ一覧を表示
setopt auto_name_dirs       # 代入直後から名前付きディレクトリにする
setopt auto_menu            # 補完キー連打で補完候補を順に表示する
setopt cdable_vars          # チルダ省略
setopt pushd_to_home        # 引数なし pushd で $HOME に戻る(直前 dir へは cd - で)
setopt pushd_ignore_dups    # ディレクトリスタックに重複する物は古い方を削除
setopt extended_history     # コマンドの開始時刻と経過時間を登録
setopt hist_ignore_dups     # 直前のコマンドと同一ならば登録しない
setopt hist_ignore_all_dups # 登録済コマンド行は古い方を削除
setopt hist_reduce_blanks   # 余分な空白は詰めて登録(空白数違い登録を防ぐ)
setopt share_history        # ヒストリの共有(append 系と異なり再読み込み不要)
setopt hist_no_store        # history コマンドは登録しない
setopt hist_ignore_space    # コマンド行先頭が空白の時登録しない(直後ならば呼べる)
setopt inc_append_history   # すぐにヒストリファイルに追記する
setopt list_packed          # 補完候補リストを詰めて表示
setopt list_types           # 補完候補にファイルの種類も表示する
setopt print_eight_bit      # 補完候補リストの日本語を適正表示
setopt no_clobber           # 上書きリダイレクトの禁止
setopt no_hup               # logout 時にバックグラウンドジョブを kill しない
setopt no_beep              # コマンド入力エラーで BEEP を鳴らさない
setopt glob_complete        # glob を展開しないで候補の一覧から補完する
setopt extended_glob        # 拡張グロブ
setopt numeric_glob_sort    # 数字を数値と解釈して昇順ソートで出力
setopt prompt_subst         # PROMPT 変数を変数展開
setopt interactive_comments # コマンド入力中のコメントを認める
setopt always_last_prompt   # 元のプロンプロに留まる
setopt complete_aliases     # エイリアスには別の補完規則を適用する
setopt notify               # バックグラウンドジョブの状態変化を即時報告する
setopt globdots             # 明確なドットの指定なしで.から始まるファイルをマッチ
setopt check_jobs           # ジョブが残っているとき警告を出す
setopt auto_param_keys      # カッコの対応などを自動的に補完
setopt complete_in_word     # 語の途中でもカーソル位置で補完
setopt long_list_jobs       # jobs でプロセスID も出力する
setopt magic_equal_subst    # = 以降も補完する(--prefix=/usr など)
unsetopt auto_param_keys    # 変数名の後ろに空白を挿入
unsetopt auto_param_slash   # ディレクトリの後ろスラッシュを挿入
unsetopt sh_word_split      # クオートしていない変数を単語分割する

# 矢印で補完を選択
zstyle ':completion:*:default' menu select=2
zstyle ':completion:*:processes' menu yes select=2
zstyle ':completion:*' menu select
zstyle ':completion:*' keep-prefix
zstyle ':completion:*' completer \
    _oldlist \
    _complete \
    _match \
    _ignored \
    #_approximate \
    _prefix \
    _list \
    _history
# オブジェクトファイルは補完しない
zstyle ':completion:*:*:(^(rm|unlink|mv)):*:*' \
    ignored-patterns '*?.elc' '*?.o' '*?~' '*\#'
# カレントディレクトリに候補がない場合のみ cdpath 上のディレクトリを候補
zstyle ':completion:*:cd:*' tag-order local-directories path-directories
# 大文字小文字の区別をしない
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z} r:|[-_.]=**'
# セパレータ
zstyle ':completion:*' list-separator '=>'

zstyle ':completion:*' format '%F{white}%d%f'
zstyle ':completion:*' group-name ''

# apt-get や dpkg を速くする
ZSH_CACHE_DIR=$ZSH_DIR/cache
if [ ! -d $ZSH_CACHE_DIR ]; then
    mkdir -p $ZSH_CACHE_DIR
fi
zstyle ':completion:*' cache-path $ZSH_CACHE_DIR
zstyle ':completion:*' use-cache yes
zstyle ':completion:*' verbose yes    # 詳細な情報を使う
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:sudo:*' environ PATH="$SUDO_PATH:$PATH" # sudo 時には sudo 用のパスも使う

# fake
zstyle ':completion:*:date:*' fake \
    '+%Y-%m-%d: 西暦-月-日' \
    '+%Y-%m-%d %H\:%M\:%S: 西暦-月-日 時\:分\:秒'

# zaw
zstyle ':chpwd:*' recent-dirs-max 5000
zstyle ':chpwd:*' recent-dirs-default yes
zstyle ':completion:*' recent-dirs-insert both
zstyle ':filter-select' case-insensitive yes

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
alias emd='command emacs --daemon'
alias emn='command emacs -nw'
alias emc='command emacsclient -t'
alias emq='command emacs -q --no-site-file'
alias ekill="command emacsclient -e '(progn (defun yes-or-no-p (p) t) (kill-emacs))'"
alias ha='fc -lDE 1'
alias comps='echo ${(F)${(uo@)_comps}}'
alias zmv='noglob zmv -W'

alias -s log='tail -f'
alias -s {el,c,h,cpp,php,yml}='emacsclient'

alias -g L='| less'
alias -g H='| head'
alias -g T='| tail'
alias -g G='| grep -n'
alias -g S='| sed'
alias -g A='| awk'
alias -g W='| wc'
alias -g N='> /dev/null 2>&1'

# 常にバックグラウンドで起動
function emacs() { command emacs $* &! }
function gimp() { command gimp $* &! }
function firefox() { command firefox $* &! }
function xdvi() { command xdvi $* &! }
function xpdf() { command xpdf $* &! }
function evince() { command evince $* &! }
function vlc() { command vlc $* &! }
function gitg() { command gitg $* &! }

# stty
stty stop undef

# bindkey
bindkey -e
bindkey "^P" history-beginning-search-backward
bindkey "^N" history-beginning-search-forward
bindkey '^i' expand-or-complete
bindkey '^[^i' reverse-menu-complete
bindkey '^[i' menu-expand-or-complete

# zaw
bindkey "^X@" zaw-cdr
bindkey "^H" zaw-history
bindkey "^Xo" zaw-open-file

# 自動的に消費時間の統計情報を表示する(3秒以上)
REPORTTIME=3
# 全てのユーザのログイン・ログアウトを監視する
watch="all"
# / も単語区切りとみなす
WORDCHARS=${WORDCHARS:s,/,,}

# prompt
RPROMPT="!%!(%(?|%?|%{$fg_bold[red]%}%?%{$reset_color%}))$RPS1"

# SSH ログイン時のプロンプト
[ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
  PROMPT="%{${fg[white]}%}${HOST%%.*} ${PROMPT}";

# mysql prompt
# https://github.com/tetsujin/zsh-function-mysql
# mysql client user
typeset -A mysql_prompt_style_client_user
mysql_prompt_style_client_user=(
    'root' "$fg_bold[red]"
    '*'    "$fg_bold[green]"
)

# mysql client host
typeset -A mysql_prompt_style_client_host
mysql_prompt_style_client_host=(
    '*.local.*' "$fg_bold[green]"
    '*.dev.*'   "$fg_bold[yellow]"
    '*'         "$fg_bold[red]"
)
# mysql server user
typeset -A mysql_prompt_style_server_user
mysql_prompt_style_server_user=(
    'root' "$bg_bold[red]$fg_bold[yellow]"
    '*'    "$fg_bold[blue]"
)
# mysql server host
typeset -A mysql_prompt_style_server_host
mysql_prompt_style_server_host=(
    '*master*' "$bg_bold[red]$fg_bold[yellow]" # Master Server
    '*slave*'  "$bg[yellow]$fg[black]"         # Slave Server
    '*'        "$fg_bold[blue]"
)
# mysql prompt style (Should use single quoted string.)
mysql_prompt='${style_client_host}${USER}@${HOST}${fg_bold[white]} -> '
mysql_prompt=$mysql_prompt'${style_server_user}\u${reset_color}${fg_bold[white]}@${style_server_host}\h${reset_color}${fg_bold[white]}:${fg[magenta]}\d ${fg_bold[white]}\v${reset_color}\n'

# tmux 自動起動
if [ -z "$TMUX" -a -z "$STY" ]; then
    if type tmux >/dev/null 2>&1; then
        if tmux has-session && tmux list-sessions | grep -qE '.*]$'; then
            tmux attach && echo "tmux attached session "
        else
            tmux new-session && echo "tmux created new session"
        fi
    fi
fi

function cdup() {
    cd .. && zle reset-prompt
}

zle -N cdup

bindkey '^X\^' cdup
bindkey '^XU' cdup

function dired () {
    dir=$1
    if [ ! -d $dir ]; then
        dir="$PWD/$dir"
    fi
    if [ -d $dir ]; then
        emacsclient -e "(dired \"$dir\")"
    else
        echo "no directory"
    fi
}

function cde () {
    EMACS_CWD=`emacsclient -e "
(expand-file-name
(with-current-buffer
(nth 1
(assoc 'buffer-list
(nth 1 (nth 1 (current-frame-configuration)))))
default-directory))" | sed 's/^"\(.*\)"$/\1/'`
    echo "chdir to $EMACS_CWD"
    cd "$EMACS_CWD"
}

zle -N dired
zle -N cde
