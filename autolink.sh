#!/bin/sh

# Xmodmap
XMODMAP=.Xmodmap

# aspell
ASPELL_CONF=.aspell.conf

# emacs
EMACS_CONF=.emacs.el
EMACS_DIR=.emacs.d

# zsh
ZSH_DIR=.zsh.d
ZSH_RC=.zshrc
ZSH_ENV=.zshenv
ZSH_LOGIN=.zlogin

# tmux
TMUX_DIR=.tmux
TMUX_CONF=.tmux.conf
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

# font
FONTS=.fonts

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
        echo "$BAK_DIR file exist"
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

# zsh
autolink $DOTFILES $HOME $ZSH_RC
autolink $DOTFILES $HOME $ZSH_ENV
autolink $DOTFILES $HOME $ZSH_LOGIN
autolink $DOTFILES $HOME $ZSH_DIR

# tmux
autolink $DOTFILES $HOME $TMUX_DIR
autolink $DOTFILES $HOME $TMUX_CONF
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

# gconftool-2 --set /apps/gnome-terminal/profiles/Default/font --type string "Ricty Regular 10"

exit 0
