dotfiles
========

**.emacs.el**  

**.emacs.d**  
  - auto-install  
  - howm 1.4.0  
  - navi2ch 1.8.4  
  - magit  
  - bm  
  - emacs-w3m  
  - twittering-mode  
  - evernote-mode  
  - gist  
  - auto-complete  
  - yasnippet  
  - yasnippet-java-mode  
  - ajc-java-complete  



Quick start
-----------

  `git clone git://github.com/sluchin/dotfiles.git`  
  
  `cd dotfiles`  
  
  `./setup.sh`  
  
  `git submodule init`  
  
  `git submodule update`  
  
  `(install-el-get)`  
  
  `(el-get-install-all)`  
  
Fonts
-----

Emacs は, 半角と全角の幅が 1:2 (桁がそろう) のプログラミング用フォント
[HackGen (白源)](https://github.com/yuru7/HackGen) を使う.
同じ作者の [PlemolJP](https://github.com/yuru7/PlemolJP) と
[UDEV Gothic](https://github.com/yuru7/udev-gothic) も入れておく (切り替えて試すため).
どれも SIL Open Font License 1.1.

Ricty (`.fonts/Ricty.tar.gz`) は開発が終わったので, 使わない.

### インストール

フォントは `~/.local/share/fonts/` に入れる (ユーザーごとのフォントの場所).
版は, 各リポジトリの Releases の最新に合わせて変える.

```sh
cd /tmp
curl -LO https://github.com/yuru7/HackGen/releases/download/v2.10.0/HackGen_v2.10.0.zip
curl -LO https://github.com/yuru7/HackGen/releases/download/v2.10.0/HackGen_NF_v2.10.0.zip
curl -LO https://github.com/yuru7/PlemolJP/releases/download/v3.1.0/PlemolJP_v3.1.0.zip
curl -LO https://github.com/yuru7/PlemolJP/releases/download/v3.1.0/PlemolJP_NF_v3.1.0.zip
curl -LO https://github.com/yuru7/udev-gothic/releases/download/v2.2.0/UDEVGothic_v2.2.0.zip
curl -LO https://github.com/yuru7/udev-gothic/releases/download/v2.2.0/UDEVGothic_NF_v2.2.0.zip

for font in HackGen PlemolJP UDEVGothic; do
    mkdir -p ~/.local/share/fonts/$font
    for zip in ${font}_*.zip; do
        unzip -j -o "$zip" '*.ttf' -d ~/.local/share/fonts/$font
    done
done
fc-cache -f ~/.local/share/fonts

# 入ったことを確かめる
fc-list : family | grep -E '^(HackGen|PlemolJP|UDEV Gothic)' | sort -u
```

- `_NF` は Nerd Fonts 版 (アイコンの文字を含む). `_HS` 版 (全角の空白を見えなくしたもの) は入れない.
- 名前に `35` の付くもの (`HackGen35` など) は, 半角と全角が 3:5 で, 桁がそろわない. 使わない.
- `Console` の付くもの (`HackGen Console` など) は, 罫線などの記号を半角にしたもの. Emacs は, こちらを使う.

### Emacs

`.emacs.el` で `HackGen Console NF` の大きさ 110 にしている.
大きさは, 全角の幅がちょうど半角の 2 倍になるものにする (画面の点に丸めるため, 120 では桁がずれる).
そろう大きさは 100, 110, 130, 140, 160.

### ターミナル (Ptyxis)

システムのフォント (Ubuntu Sans Mono) のまま使う. HackGen と UDEV Gothic は, ターミナルでは字の間が離れて見えた
(ターミナルは, 文字の升目をフォントの幅より狭くできない).
HackGen にするときは, 次のようにする (`use-system-font true` で元に戻る).

```sh
gsettings set org.gnome.Ptyxis use-system-font false
gsettings set org.gnome.Ptyxis font-name 'HackGen Console NF 11'
```

Author
------

**Tetsuya Higashi**  

http://www.facebook.com/bennysluchin  

