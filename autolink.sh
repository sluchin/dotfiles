#!/bin/sh

# emacs
EMACS_CONF=.emacs.el
EMACS_DIR=.emacs.d

# zsh
OHMYZSH=.oh-my-zsh
ZSH_PLUGIN=.zsh.d
ZSH_RC=.zshrc
ZSH_ENV=.zshenv
ZSH_PROFILE=.zlogin

# tmux
TMUX_DIR=.tmux
TMUX_CONF=.tmux.conf

#git
GIT_CONF=.gitconfig
GIT_IGNORE=.gitignore.local
GIT_DIFF=diff-highlight
GIT_MELD=git-meld.pl

# mysql
MYSQL=.my.cnf

DOTFILES=$HOME/dotfiles
BAK_DIR=$HOME/backup

if [ ! -d $DOTFILES ]; then
    echo "$DOTFILES no exist"
    exit 1
fi

if [ ! -e $BAK_DIR ]; then
    mkdir $BAK_DIR
    echo "mkdir $BAK_DIR"
else
    if [ ! -d $BAK_DIR ]; then
        echo "$BAK_DIR exist"
        exit 1
    fi
fi

autolink()
{
    src=$1
    dst=$2
    link=$3
    if [ -e "$dst/$link" -o -L "$dst/$link" ]; then
        if [ ! -e "$BAK_DIR/$link" -a -e "$dst/$link" ]; then
            mv $dst/$link $BAK_DIR/.; retval=$?
            echo "mv $link[$retval]"
        else
            if [ -L "$dst/$link" ]; then
                rm $dst/$link; retval=$?
                echo "rm $link[$retval]"
            else
                echo "Can not make $link"
                exit 1
            fi
        fi
    fi

    ln -s $src/$link $dst/.; retval=$?
    echo "ln -s $link[$retval]"
}

# emacs
autolink $DOTFILES $HOME $EMACS_CONF
autolink $DOTFILES $HOME $EMACS_DIR

# zsh
autolink $DOTFILES $HOME $ZSH_RC
autolink $DOTFILES $HOME $ZSH_ENV
autolink $DOTFILES $HOME $ZSH_PROFILE
autolink $DOTFILES $HOME $OHMYZSH
autolink $DOTFILES $HOME $ZSH_PLUGIN

# tmux
autolink $DOTFILES $HOME $TMUX_DIR
autolink $DOTFILES $HOME $TMUX_CONF

# git
autolink $DOTFILES $HOME $GIT_CONF
autolink $DOTFILES $HOME $GIT_IGNORE
if [ ! -d $HOME/bin ]; then
    mkdir $HOME/bin; retval=$?
    echo "mkdir $HOME/bin[$retval]"
fi
autolink $DOTFILES/bin $HOME/bin $GIT_DIFF
autolink $DOTFILES/bin $HOME/bin $GIT_MELD

# mysql
autolink $DOTFILES $HOME $MYSQL

exit 0
