### .zshrc -*- mode: Shell-script; coding: utf-8; indent-tabs-mode: nil -*-

# curl -L https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh | sh
# Path to your oh-my-zsh configuration.
ZSH_DIR=$HOME/.zsh.d
ZSH=$ZSH_DIR/oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.  # robbyrussell agnoster bira bureau candy blinks ZSH_THEME="bureau" 
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

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

plugins=(
    adb cp gas gem git github perl pod postgres python
    rsync ruby scala sudo svn symfony2
    vagrant tmux yum
)

if [ -f $ZSH/oh-my-zsh.sh ]; then
    source $ZSH/oh-my-zsh.sh
fi

# autoload
autoload -Uz compinit
compinit -u
autoload -Uz run-help
autoload -Uz colors
colors
autoload -Uz zmv
autoload -Uz select-word-style
select-word-style default

# cdr
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs

if [ -d $ZSH_DIR ]; then
    files=(
        $ZSH_DIR/plugin/zaw/zaw.zsh
        $ZSH_DIR/plugin/autojump/bin/autojump.zsh
        $ZSH_DIR/plugin/notify.zsh
        $ZSH_DIR/functions/mysql
    )
    for file in $files
    do
        if [ -f $file ]; then
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
LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

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

compctl -f -x 'C[-1,-[ef]]' -k '(mew batch-byte-compile trr)' -- emacs
compctl -x 'S[C]' -k '(CC CFLAGS CPPFLAGS CXXFLAGS)' -S = -- make
compctl -x 'q[d][s]' -k '(%Y-%m-%d\ %H:%M:%S %Y/%m/%d)' \
        -P '+' -- date

# 矢印で補完を選択
zstyle ':completion:*:default' menu select=2
zstyle ':completion:*:processes' menu yes select=2
zstyle ':completion:*' menu select interactive
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

# ファイル補完候補に色を付ける
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# cd, pushed では補完しない
zstyle ':completion::*:(cd|pushed):*:directories' \
    ignored-patterns '.git' '.svn' '.deps' '(*/)#(CVS|RCS)'

# カレントディレクトリに候補がない場合のみ cdpath 上のディレクトリを補完
zstyle ':completion::*:(cd|pushed):*:directories' \
    tag-order local-directories path-directories

# 親ディレクトリからカレントディレクトリを補完しない
zstyle ':completion::*:(cd|pushed):*:directories' \
    ignore-parents parent pwd

# rm, unlink, mv 以外ではオブジェクトファイルを補完しない
zstyle ':completion::*:(^(rm|unlink|mv)):*:*files' \
    ignored-patterns '?*.elc' '?*.o' '?*~' '?*.bak'

# 大文字小文字の区別をしない
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z} r:|[-_.]=**'

# セパレータ
zstyle ':completion:*' list-separator '-->'
zstyle ':completion:*' format '%F{white}%d%f'
zstyle ':completion:*' group-name ''

# apt-get や dpkg を速くする
ZSH_CACHE_DIR=$ZSH_DIR/cache
if [ ! -d $ZSH_CACHE_DIR ]; then
    mkdir -p $ZSH_CACHE_DIR
fi
zstyle ':completion:*' cache-path $ZSH_CACHE_DIR
zstyle ':completion:*' use-cache yes  # apt-get update キャッシュを使う
zstyle ':completion:*' verbose yes    # 詳細な情報を使う
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:sudo:*' environ PATH="$SUDO_PATH:$PATH" # sudo 時には sudo 用のパスも使う
zstyle ':completion:*' format '%B%d%b'

# 補完方法毎にグループ化する。
zstyle ':completion:*' format '%B%F{blue}%d%f%b'
zstyle ':completion:*' group-name ''

# 変数の添字の補完
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# man の補完をセクション番号別に表示させる
zstyle ':completion:*:manuals' separate-sections true

# ディレクトリで区切る
zstyle ':zle:*' word-chars " /=;@:{},|"
zstyle ':zle:*' word-style unspecified

# date コマンド補完
zstyle ':completion:*:date:*' fake \
    '+%Y-%m-%d: 西暦-月-日' \
    '+%Y-%m-%d %H\:%M\:%S: 西暦-月-日 時\:分\:秒'

compdef _gnu_generic emacs emacsclient \
        emacs-snapshot emacsclient.emacs-snapshot

# zaw
zstyle ':chpwd:*' recent-dirs-max 100000
zstyle ':chpwd:*' recent-dirs-default yes
zstyle ':completion:*' recent-dirs-insert both
zstyle ':filter-select' case-insensitive yes

# alias
alias pu=pushd
alias po=popd
alias dirs='dirs -v'
alias d='dirs'
alias q='echo $?'
alias eject='eject -v'
alias out='eject'
alias in='eject -t'
alias ls='ls --color=always'
alias ll='ls --color=always -ld ^*~'
alias la='ls --color=always -ad ^*~'
if type trash-put >/dev/null 2>&1; then
    alias rm='trash-put'
fi
alias grep='grep --color=always -dskip --binary-files=without-match --exclude="*~" --exclude-dir={.git,.svn,.deps}'
alias fgrep='fgrep --color=always -dskip --binary-files=without-match --exclude="*~" --exclude-dir={.git,.svn,.deps}'
alias egrep='egrep --color=always -dskip --binary-files=without-match --exclude="*~" --exclude-dir={.git,.svn,.deps}'
alias g='git --no-pager'
alias t='tail -f'
alias gterm='gnome-terminal --geometry=130x40'
alias emd='emacs --daemon'
alias emn='emacs -nw'
alias emc='emacsclient'
alias emq='emacs -q --no-site-file'
alias ha='fc -lDE 1'
alias comps='echo ${(F)${(uo@)_comps}}'
alias zmv='noglob zmv -W'
alias lxterminal='lxterminal --geometry=100x40'

alias -s log='tail -f'
alias -s {el,c,h,cpp,,pm,java,php,yml}='emacsclient'

alias -g L='| less'
alias -g H='| head'
alias -g T='| tail'
alias -g G='| grep'
alias -g S='| sed'
alias -g A='| awk'
alias -g W='| wc -l'
alias -g N='> /dev/null 2>&1'


zstyle ':completion::*:shnsplit::' \
    file-patterns '?*.flac'
alias shnsplit='shnsplit -o flac -t %n'

# 常にバックグラウンドで起動
function xpdf() { command xpdf $* &! }
function evince() { command evince $* &! }
function qpdfview() { command qpdfview $* &! }
function emacsclient() { command emacsclient $* &! }
function gimp() { command gimp $* &! }
function firefox() { command firefox $* &! }
function vlc() { command vlc $* &! }
function gitg() { command gitg $* &! }
function goldendict() { command goldendict $* &! }
function jd() { command jd $* &! }
function exaile() { command exaile $* &! }
function konqueror() { command konqueror $* &! }
function easytag() { command easytag $* &! }
function asunder() { command asunder $* &! }
function soundkonverter() { command soundkonverter $* &! }
function ghb() { command ghb $* &! }
function nixnote() { command nixnote $* &! }
function nautilus() { command nautilus $* &! }
function pterm() { command pterm $* &! }
function gnome-calculator() { command gnome-calculator $* &! }
function edwin() { command mit-scheme --edit $* &! }

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
bindkey "^z^z" zaw-cdr
bindkey "^z^h" zaw-history
bindkey "^z^o" zaw-open-file
bindkey "^z^a" zaw-applications
bindkey "^z^x" zaw-tmux

# 自動的に消費時間の統計情報を表示する
REPORTTIME=3
# 全てのユーザのログイン・ログアウトを監視する
watch="all"

# 右プロンプト
RPROMPT="!%!(%(?|%?|%{$fg_bold[red]%}%?%{$reset_color%}))$RPS1"

# SSH ログイン時のプロンプト
[ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
    PROMPT="%{${fg[white]}%}${HOST%%.*} ${PROMPT}";

# mysql プロンプト
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
mysql_prompt=$mysql_prompt'${style_server_user}\u${reset_color}${fg_bold[white]}@${style_server_host}\h${reset_color}${fg_bold[white]}:${fg[magenta]}\d ${fg_bold[white]}\v${reset_color}\n$ '

# notify-send
__timetrack_threshold=20 # seconds
read -r -d '' __timetrack_ignore_progs <<EOF
less more
emacs vi vim
ssh mosh telnet nc netcat
gdb
top htop run-help man woman e
EOF

export __timetrack_threshold
export __timetrack_ignore_progs

function is_exists() { type "$1" >/dev/null 2>&1; return $?; }
function is_osx() { [[ $OSTYPE == darwin* ]]; }
function is_screen_running() { [ ! -z "$STY" ]; }
function is_tmux_runnning() { [ ! -z "$TMUX" ]; }
function is_screen_or_tmux_running() { is_screen_running || is_tmux_runnning; }
function shell_has_started_interactively() { [ ! -z "$PS1" ]; }
function is_ssh_running() { [ ! -z "$SSH_CONECTION" ]; }

function tmux_automatically_attach_session()
{
    if is_screen_or_tmux_running; then
        ! is_exists 'tmux' && return 1

        if is_tmux_runnning; then
            echo "${fg_bold[red]} _____ __  __ _   ___  __ ${reset_color}"
            echo "${fg_bold[red]}|_   _|  \/  | | | \ \/ / ${reset_color}"
            echo "${fg_bold[red]}  | | | |\/| | | | |\  /  ${reset_color}"
            echo "${fg_bold[red]}  | | | |  | | |_| |/  \  ${reset_color}"
            echo "${fg_bold[red]}  |_| |_|  |_|\___//_/\_\ ${reset_color}"
        elif is_screen_running; then
            echo "This is on screen."
        fi
    else
        if shell_has_started_interactively && ! is_ssh_running; then
            if ! is_exists 'tmux'; then
                echo 'Error: tmux command not found' 2>&1
                return 1
            fi

            if tmux has-session >/dev/null 2>&1 && tmux list-sessions | grep -qE '.*]$'; then
                # detached session exists
                tmux list-sessions
                echo -n "Tmux: attach? (y/N/num) "
                read
                if [[ "$REPLY" =~ ^[Yy]$ ]] || [[ "$REPLY" == '' ]]; then
                    tmux attach-session
                    if [ $? -eq 0 ]; then
                        echo "$(tmux -V) attached session"
                        return 0
                    fi
                elif [[ "$REPLY" =~ ^[0-9]+$ ]]; then
                    tmux attach -t "$REPLY"
                    if [ $? -eq 0 ]; then
                        echo "$(tmux -V) attached session"
                        return 0
                    fi
                fi
            fi

            if is_osx && is_exists 'reattach-to-user-namespace'; then
                # on OS X force tmux's default command
                # to spawn a shell in the user's namespace
                tmux_config=$(cat $HOME/.tmux.conf < \
                              (echo 'set-option -g default-command \
                                "reattach-to-user-namespace -l $SHELL"'))
                tmux -f < \
                     (echo "$tmux_config") new-session && \
                    echo "$(tmux -V) created new session supported OS X"
            else
                tmux new-session && echo "tmux created new session"
            fi
        fi
    fi
}

DOT_TMUX_AUTO=$HOME/.tmux_autostart
[[ -f $DOT_TMUX_AUTO ]] && tmux_automatically_attach_session

if type peco >/dev/null 2>&1; then
    function peco-buffer()
    {
        BUFFER=$(eval ${BUFFER} | peco)
        CURSOR=0
    }
    zle -N peco-buffer
    bindkey '^p' peco-buffer

    function peco-history()
    {
        local tac
        if type tac > /dev/null 2>&1; then
            tac="tac"
        else
            tac="tail -r"
        fi
        BUFFER=$(\history -n 1 | egrep -v "^cd" | eval $tac | \
            peco --prompt="history >" --query "$LBUFFER")
        CURSOR=$#BUFFER
        zle clear-screen
    }
    zle -N peco-history
    bindkey '^r' peco-history

    function peco-cdr ()
    {
        local dir=$(cdr -l | sed 's/^[0-9]\+ \+//' | \
            peco --prompt="cdr >" --query "$LBUFFER")
        if [ -n "$dir" ]; then
            BUFFER="cd ${dir}"
            zle accept-line
        fi
    }
    zle -N peco-cdr
    bindkey '^s' peco-cdr

    MAXDEPTH=""
    function peco-path()
    {
        if [[ -n $RBUFFER ]]; then
            (( CURSOR++ ))
            return 0;
        fi
        local filepath=$(find . -maxdepth ${MAXDEPTH:-5} | \
            grep -v '/\.' | peco --prompt 'path >' --query "$LBUFFER")
        if [ -n "$filepath" ]; then
            if [ -d "$filepath" ]; then
                BUFFER+="cd '$filepath'"
            elif [ -f "$filepath" ]; then
                BUFFER="emacsclient '$filepath'"
            fi
            zle accept-line
        else
            return 1
        fi
        CURSOR=$#BUFFER
    }
    zle -N peco-path
    bindkey '^f' peco-path

    function peco-kill()
    {
        local pid=$(ps aux --sort=-pid | peco --prompt 'kill >' | awk '{ print $2 }')
        if [ ${#pid} -ne 0 ]; then
            kill "$@" $pid
            echo "kill $@ ${pid}"
        fi
    }
    zle -N peco-kill
    alias pk="peco-kill"
    alias pkill="peco-kill"

    function peco-grep() {
        res=$(grep --color=never "$@" | \
            peco --prompt 'grep >' | \
            awk -F: '{ print $2" "$1 }')
        if [ ${#res} -ne 0 ]; then
            local file=$(echo $res | awk -F" " '{ print $2 }')
            local nu=$(echo $res | awk -F" " '{ print $1 }')
            emacsclient +${nu:=0} ${file}
            echo "emacsclient +${nu} ${file}"
        fi
    }
    zle -N peco-grep
    alias pg="peco-grep"
    alias pgrep="peco-grep"
else
    bindkey "^r" zaw-history
    bindkey '^s' zaw-cdr
fi

function extract()
{
    case $1 in
        *.tar.gz|*.tgz) tar xzvf $1;;
        *.tar.xz) tar Jxvf $1;;
        *.zip) unzip $1;;
        *.lzh) lha e $1;;
        *.tar.bz2|*.tbz) tar xjvf $1;;
        *.tar.Z) tar zxvf $1;;
        *.gz) gzip -d $1;;
        *.bz2) bzip2 -dc $1;;
        *.Z) uncompress $1;;
        *.tar) tar xvf $1;;
        *.arj) unarj $1;;
    esac
}

alias -s {gz,tgz,zip,lzh,bz2,tbz,Z,tar,arj,xz}=extract

function splitflac() {

    [[ $# -eq 2 ]] ||
        { echo >&2 "Usage: ${0:t} cuefile flacfile"; return 1; }

    type shnsplit > /dev/null 2>&1 ||
        { echo >&2 "I require shnsplit but it's not installed. Aborting."; return 1; }

    [[ ! -d split ]] && mkdir split

    local cuefile=$1
    local flacfile=$2

    echo shnsplit -d split -t "%n - %p - %t" -o flac -f "$cuefile" "$flacfile"
    shnsplit -d split -t "%n - %p - %t" -o flac -f "$cuefile" "$flacfile"
    [[ $? -ne 0 ]] && return 1
    type cuetag > /dev/null 2>&1 ||
        { echo >&2 "I require cuetag but it's not installed. Aborting."; return 1; }

    local zerofile=$(find . -type f | grep split/00)
    [[ -n $zerofile ]] && rm split/00*

    local save=$IFS
    IFS='   '
    echo cuetag "$cuefile" ./split/*.flac
    cuetag "$cuefile" ./split/*.flac
    [[ $? -ne 0 ]] && return 1

    while true; do
        read ans\?"Do you delete $flacfile ? [Y/n] "
        case $ans in
        [Yy]* )
            rm $flacfile
            break ;;
        [Nn]* )
            break ;;
        esac
    done
    \mv split/*.flac .
    \rm -rf split

    IFS=$save

    return 0
}

function zman() {
    kind=$1
    search=$2
    PAGER="less -g -s '+/^       "$search"'" man $kind
}

function cdup ()
{
    cd .. && zle reset-prompt
    echo `pwd`
}
zle -N cdup
bindkey '^U' cdup

function dired ()
{
    dir=${1:-"$PWD"}
    [ ! -d $dir ] && dir="$PWD/$dir"
    echo "$0 $dir"
    if [ -d $dir ]; then
        emacsclient -e "(dired \"$dir\")"
        dir=""
    else
        echo "no directory: $dir"
    fi
}
zle -N dired

function woman ()
{
    topic=$1
    echo "$0 $topic"
    emacsclient -e "(woman \"$topic\")"
}
zle -N woman

function rgrep ()
{
    regex=$1
    files=${2:-"\*"}
    dir=${3:-"$PWD"}
    [ ! -d $dir ] && dir="$PWD/$dir"
    echo "$0 $regex $files $dir"
    if [ -d "$dir" ]; then
        emacsclient -e "(setq grep-find-template \"find . <X> -type f <F> -exec grep <C> -nH -e <R> {} +\")"
        emacsclient -e "(rgrep \"$regex\" \"$files\" \"$dir\" nil)"
        dir=""
    else
        echo "no directory: $dir"
    fi
}
zle -N rgrep

function magit-status ()
{
    dir=${1:-"$PWD"}
    [ ! -d $dir ] && dir="$PWD/$dir"
    echo "$0 $dir"
    if [ -d "$dir" ]; then
        emacsclient -e "(magit-status \"$dir\")"
        dir=""
    else
        echo "no directory: $dir"
    fi
}
zle -N magit-status

function cde ()
{
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
zle -N cde

function e ()
{
    emacsclient $* ||
    emacs $*
}
zle -N e

function ekill ()
{
    emacsclient -e "(progn (defun yes-or-no-p (p) t) (kill-emacs))"
}
zle -N ekill

function tmuxload ()
{
    file=$1
    if [ -r "$file" ]; then
        tac $file | while read line; \
            do echo $line | tr -d "\n" | tmux load-buffer -; \
            done
    else
        echo "tmuxload file"
    fi
}
zle -N tmuxload

function print_known_hosts ()
{
    if [ -f $HOME/.ssh/known_hosts ]; then
        cat $HOME/.ssh/known_hosts | tr ',' ' ' | cut -d' ' -f1
    fi
}
_cache_hosts=($(print_known_hosts) $_cache_hosts)

function zload ()
{
    local file files dir dirs
    typeset -U dirs
    if [ "${#}" -le 0 ]; then
        files=($ZSH_DIR/completion/*[^~](.))
    else
        files="$@"
    fi

    dirs=()
    for file in $files
    do
        if (($+functions["${file:t}"])) ; then
            echo "unfunction ${file:t}"
            unfunction "${file:t}"
        fi
        echo "autoload ${file:t}"
        FPATH="${file:h:a}" autoload -Uz +X "${file:t}"

        if [[ "${file:t}" == _* ]]; then
            dirs=($dirs "${file:h:a}")
        fi
    done

    if [ "${#dirs}" -gt 0 ]; then
        echo $dirs | sed -e"s# #\n#g"
        echo "compinit"
        fpath=($dirs $fpath) compinit
    fi
}
zle -N zload

function git-branchall ()
{
    for remote_branch in `git branch -a | grep remotes | grep -v HEAD | grep -v master`; do
        remote_name="$(echo -n "$remote_branch" | cut -d/ -f2)"
        branch_name="$(echo -n "$remote_branch" | cut -d/ -f3)"
        git checkout -b $branch_name $remote_name/$branch_name
    done
    git checkout master
}
zle -N git-branchall

function docfx {
  mono $HOME/bin/docfx/docfx.exe
}
zle -N docfx

function docfx-serve {
  mono $HOME/bin/docfx/docfx.exe serve _site
}
zle -N docfx-serve

