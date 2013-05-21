# .zshrc
# initial setup file for only interactive zsh

autoload -U compinit && compinit
autoload -Uz vcs_info
autoload -Uz add-zsh-hook
autoload -Uz is-at-least
autoload -Uz colors && colors

case ${UID} in
    0) PROMPT_MARK="#" ;;
    *) PROMPT_MARK="%%" ;;
esac

PROMPT="%{$fg[red]%}[%*] %n${PROMPT_MARK}%{$reset_color%} "
RPROMPT="%1(v|%{$fg[green]%}(%1v%3(v|%{$fg[red]%}:%3v|)%2(v|%{$fg[yellow]%}⚡|)%{$fg[green]%})%{$reset_color%}|)%{$bg[magenta]$fg[white]%}[ %~ ]%{$reset_color%}"
SPROMPT="%{$fg[red]%}%r is correct? [n,y,a,e]:%{$reset_color%} "

[ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
    PROMPT="%{$fg[white]%}${HOST%%.*}%{$reset_color%} ${PROMPT} "

HISTSIZE=100000
SAVEHIST=100000
HISTFILE=~/.zhistory

# shell options
setopt auto_cd
setopt auto_remove_slash
setopt auto_name_dirs
setopt extended_history
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
setopt magic_equal_subst

# alias
alias pu=pushd
alias po=popd
alias dirs='dirs -v'
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# stty
stty stop undef

# bindkey
bindkey -e

zstyle ':completion:*' format '%BCompleting %d%b'
zstyle ':completion:*' group-name ''
zstyle ':vcs_info:*' max-exports 3
zstyle ':vcs_info:*' enable git svn hg bzr
zstyle ':vcs_info:*' formats '%s:%b' '%m'
zstyle ':vcs_info:*' actionformats '%s:%b' '%m' '%a'
zstyle ':vcs_info:(svn|bzr):*' branchformat '%b:r%r'
zstyle ':vcs_info:bzr:*' use-simple true

if is-at-least 4.3.11; then
    # git のときはフック関数を設定する

    # formats '(%s)-[%b]' '%c%u %m' , actionformats '(%s)-[%b]' '%c%u %m' '<!%a>'
    # のメッセージを設定する直前のフック関数
    # 今回の設定の場合はformat の時は2つ, actionformats の時は3つメッセージがあるので
    # 各関数が最大3回呼び出される。
    zstyle ':vcs_info:git+set-message:*' hooks \
        git-hook-begin \
        git-push-status

    # フックの最初の関数
    # git の作業コピーのあるディレクトリのみフック関数を呼び出すようにする
    # (.git ディレクトリ内にいるときは呼び出さない)
    # .git ディレクトリ内では git status --porcelain などがエラーになるため
    function +vi-git-hook-begin() {
        if [[ $(command git rev-parse --is-inside-work-tree 2> /dev/null) != 'true' ]]; then
            # 0以外を返すとそれ以降のフック関数は呼び出されない
            return 1
        fi

        return 0
    }

    # push していないコミットの件数表示
    # リモートリポジトリに push していないコミットの件数を
    # pN という形式で misc (%m) に表示する
    function +vi-git-push-status() {
        # zstyle formats, actionformats の2番目のメッセージのみ対象にする
        if [[ "$1" != "1" ]]; then
            return 0
        fi

        # push していないコミット数を取得する
        local ahead
        ahead=$(command git rev-list origin/${hook_com[branch]}..${hook_com[branch]} 2>/dev/null \
            | wc -l \
            | tr -d ' ')

        if [[ "$ahead" -gt 0 ]]; then
            # misc (%m) に追加
            # hook_com[misc]+="(p${ahead})"
            hook_com[misc]+="${ahead}"
        fi
    }
fi


function _precmd_vcs_info() {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
    [[ -n "$vcs_info_msg_1_" ]] && psvar[2]="$vcs_info_msg_1_"
    [[ -n "$vcs_info_msg_2_" ]] && psvar[3]="$vcs_info_msg_2_"
}
add-zsh-hook precmd _precmd_vcs_info
