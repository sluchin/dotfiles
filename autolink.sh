#!/bin/sh

# emacs
EMACS_CONF=.emacs.el
EMACS_DIR=.emacs.d

# zsh
OHMYZSH=.oh-my-zsh
ZSH_RC=.zshrc
ZSH_ENV=.zshenv
ZSH_PROFILE=.zlogin

#git
GIT_CONF=.gitconfig
GIT_IGNORE=.gitignore.local

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
    link=$1
    echo "$HOME/$link"
    if [ -e "$HOME/$link" -o -L "$HOME/$link" ]; then
        if [ ! -e "$BAK_DIR/$link" -a -e "$HOME/$link" ]; then
            mv $HOME/$link $BAK_DIR/.; retval=$?
            echo "mv $link[$retval]"
        else
            if [ -L "$HOME/$link" ]; then
                rm $HOME/$link; retval=$?
                echo "rm $link[$retval]"
            else
                echo "Can not make $link"
                exit 1
            fi
        fi
    fi

    ln -s $DOTFILES/$link $HOME/.; retval=$?
    echo "ln -s $link[$retval]"
}

# emacs
autolink $EMACS_CONF
autolink $EMACS_DIR

# zsh
autolink $ZSH_RC
autolink $ZSH_ENV
autolink $ZSH_PROFILE
autolink $OHMYZSH

# git
autolink $GIT_CONF
autolink $GIT_IGNORE

exit 0
