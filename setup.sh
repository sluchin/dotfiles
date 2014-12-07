#!/bin/sh

# Xmodmap
XMODMAP=.Xmodmap

# aspell
ASPELL_CONF=.aspell.conf

# emacs
EMACS_CONF=.emacs.el
EMACS_DIR=.emacs.d
EMACS_SRC=emacs

# zsh
ZSH_DIR=.zsh.d
ZSH_RC=.zshrc
ZSH_ENV=.zshenv
ZSH_LOGIN=.zlogin

# tmux
TMUX_DIR=.tmux
TMUX_CONF=.tmux.conf
TMUX_STATUS1=.tmux.powerline.status
TMUX_STATUS2=.tmux.status
TMUX_RC=.tmux-powerlinerc
TMUX_CPU=get_cpu_usage.sh
TMUX_MEM=get_mem_usage.sh

# git
GIT_CONF=.gitconfig
GIT_IGNORE=.gitignore.local
GIT_DIFF=diff-highlight
GIT_MELD=git-meld.pl

# mysql
MYSQL_CONF=.my.cnf

# application
APP=.local/share/applications
APPSYS=/usr/share/applications
GNOME_TERMINAL=gnome-terminal.desktop
EMACSCLIENT=emacsclient.desktop

# lisp
GUILE=.guile
SBCL=.sbclrc
CMUCL=.cmucl-init.lisp
MIT_SCHEME=.scheme.init

# font
FONTS=.fonts

DOTFILES=$HOME/dotfiles
BAK_DIR=$HOME/backup
SRC_DIR=$HOME/src

if [ ! -d $DOTFILES ]; then
    echo "$DOTFILES no exist"
    exit 1
fi

if [ ! -e $BAK_DIR ]; then
    mkdir $BAK_DIR
    echo "mkdir $BAK_DIR"
else
    if [ ! -d $BAK_DIR ]; then
        echo "$BAK_DIR file exist"
        exit 1
    fi
fi

if [ ! -e $SRC_DIR ]; then
    mkdir $SRC_DIR
    echo "mkdir $SRC_DIR"
else
    if [ ! -d $SRC_DIR ]; then
        echo "$SRC_DIR file exist"
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

# Xmodmap
autolink $DOTFILES $HOME $XMODMAP

# aspell
autolink $DOTFILES $HOME $ASPELL_CONF

# emacs
autolink $DOTFILES $HOME $EMACS_CONF
autolink $DOTFILES $HOME $EMACS_DIR
autolink $DOTFILES $SRC_DIR $EMACS_SRC

# zsh
autolink $DOTFILES $HOME $ZSH_RC
autolink $DOTFILES $HOME $ZSH_ENV
autolink $DOTFILES $HOME $ZSH_LOGIN
autolink $DOTFILES $HOME $ZSH_DIR

# tmux
autolink $DOTFILES $HOME $TMUX_DIR
autolink $DOTFILES $HOME $TMUX_CONF
autolink $DOTFILES $HOME $TMUX_STATUS1
autolink $DOTFILES $HOME $TMUX_STATUS2
autolink $DOTFILES $HOME $TMUX_RC
autolink $DOTFILES/bin $HOME/bin $TMUX_CPU
autolink $DOTFILES/bin $HOME/bin $TMUX_MEM

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
autolink $DOTFILES $HOME $MYSQL_CONF

# application
autolink $DOTFILES/$APP $HOME/$APP $GNOME_TERMINAL
autolink $DOTFILES/$APP $HOME/$APP $EMACSCLIENT
#if [ `whoami` = 'root' ]; then
    #autolink $DOTFILES/$APP $APPSYS $GNOME_TERMINAL
    #autolink $DOTFILES/$APP $APPSYS $EMACSCLIENT
#fi

# lisp
autolink $DOTFILES $HOME $GUILE
autolink $DOTFILES $HOME $SBCL
autolink $DOTFILES $HOME $CMUCL
autolink $DOTFILES $HOME $MIT_SCHEME

# fonts
cd $DOTFILES/.fonts
for font in *.tar.gz
do
    tar xvfz $font
done
cd $OLDPWD
autolink $DOTFILES $HOME $FONTS

if [ `whoami` = 'root' ]; then
    FONTS_DIR=/usr/local/share/fonts/truetype
    RICTY_DIR=$FONTS_DIR/ricty
    if [ ! -d $RICTY_DIR ]; then
        mkdir -p $RICTY_DIR; retval=$?
        echo "mkdir $RICTY_DIR[$retval]"
    fi

    for ttf in $DOTFILES/.fonts/*.ttf
    do
        file=`basename $ttf`
        autolink $DOTFILES/.fonts $RICTY_DIR $file
    done
fi

if [ -n `which fc-cache` ]; then
    fc-cache -vf
fi

# xdg-mime query filetype .zshrc
if [ -n `which xdg-mime` ]; then
    xdg-mime default emacsclient.desktop text/plain
fi

# gconftool-2 --set /apps/gnome-terminal/profiles/Default/font --type string "Ricty Regular 10"

exit 0
