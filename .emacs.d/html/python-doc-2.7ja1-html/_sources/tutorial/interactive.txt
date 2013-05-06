.. _tut-interacting:

**************************
対話入力編集とヒストリ置換
**************************

あるバージョンの Python インタプリタでは、Korn シェルや GNU Bash シェルに
見られる機能に似た、現在の入力行に対する編集機能やヒストリ置換機能を
サポートしています。この機能は `GNU Readline`_ ライブラリを使って
実装されています。このライブラリは Emacs スタイルと vi スタイルの編集を
サポートしています。ライブラリには独自のドキュメントがあり、ここでそれを
繰り返すつもりはありません。とはいえ、基本について簡単に解説することにします。
ここで述べる対話的な編集とヒストリについては、 Unix 版と Cygwin
版のインタプリタでオプションとして利用することができます。

この章では、Mark Hammond の PythonWin パッケージや、 Python とともに配布される
Tk ベースの環境である IDLE にある編集機能については解説 *しません* 。
NT 上の DOS ボックスやその他の DOS および Windows 類で働くコマンド行
ヒストリ呼出しもまた別のものです。


.. _tut-lineediting:

行編集
======

入力行の編集がサポートされている場合、インタプリタが一次または二次プロンプトを
出力している際にはいつでも有効になっています。現在の行は、慣例的な Emacs
制御文字を使って編集することができます。そのうち最も重要なものとして、次の
ようなキーがあります。 :kbd:`C-A` (Control-A)はカーソルを行の先頭へ
移動させます。 :kbd:`C-E` は末尾へ移動させます。 :kbd:`C-B` は逆方向へ
一つ移動させます。 :kbd:`C-F` は順方向へ移動させます。 Backspace は逆方向に
向かって文字を消します。 :kbd:`C-D` は順方向に向かって消します。 :kbd:`C-K`
は順方向に向かって行の残りを kill し (消し) ます、 :kbd:`C-Y` は最後に kill
された文字列を再び yank し (取り出し) ます。
:kbd:`C-underscore` 最後の変更を元に戻します。
これは、繰り返してどんどんさかのぼることができます。


.. _tut-history:

ヒストリ置換
============

ヒストリ置換は次のように働きます。入力された行のうち、空行でない実行された
行はすべてヒストリバッファに保存されます。そして、プロンプトが提示される
ときには、ヒストリバッファの最も下の新たな行に移動します。
:kbd:`C-P` はヒストリバッファの中を一行だけ上に移動し (戻し) ます。
:kbd:`C-N` は 1 行だけ下に移動します。ヒストリバッファのどの行も
編集することができます。行が編集されると、それを示すためにプロンプトの前に
アスタリスクが表示されます  [#]_ 。
:kbd:`Return` キーを押すと現在行がインタプリタへ渡されます。 :kbd:`C-R`
はインクリメンタルな逆方向サーチ (reverse search) を開始し、 :kbd:`C-S`
は順方向サーチ (forward search)を開始します。


.. _tut-keybindings:

キー割り当て
============

Readline ライブラリのキー割り当て (key binding) やその他のパラメタは、
:file:`~/.inputrc` という初期化ファイル [#]_ にコマンドを置くことで
カスタマイズできます。キー割り当ての形式は

::

   key-name: function-name

または

::

   "string": function-name

で、オプションの設定方法は

::

   set option-name value

です。例えば、以下のように設定します。

::

   # vi スタイルの編集を選択する:
   set editing-mode vi

   # 一行だけを使って編集する:
   set horizontal-scroll-mode On

   # いくつかのキーを再束縛する:
   Meta-h: backward-kill-word
   "\C-u": universal-argument
   "\C-x\C-r": re-read-init-file

Python では、 :kbd:`Tab` に対するデフォルトの割り当ては TAB の挿入です。
Readline のデフォルトであるファイル名補完関数ではないので注意してください。
もし、どうしても Readline のデフォルトを割り当てたいのなら、
:file:`~/.inputrc` に

::

   Tab: complete

を入れれば設定を上書きすることができます。 (もちろん、 :kbd:`Tab` を使って
インデントするのに慣れている場合、この設定を行うとインデントされた継続行を
入力しにくくなります。)

.. index::
   module: rlcompleter
   module: readline

変数名とモジュール名の自動的な補完がオプションとして利用できます。補完を
インタプリタの対話モードで有効にするには、以下の設定をスタートアップファイルに
追加します。 [#]_

::

   import rlcompleter, readline
   readline.parse_and_bind('tab: complete')

この設定は、 :kbd:`Tab` キーを補完関数に束縛します。従って、 :kbd:`Tab` キーを
二回たたくと補完候補が示されます。
補完機能は Python の文の名前、現在のローカル変数、および利用可能な
モジュール名を検索します。 ``string.a`` のようなドットで区切られた式に
ついては、最後の ``'.'`` までの式を評価し、結果として得られたオブジェクトの
属性から補完候補を示します。 :meth:`__getattr__` メソッドを持ったオブジェクトが
式に含まれている場合、 :meth:`__getattr__` がアプリケーション定義のコードを
実行するかもしれないので注意してください。

より良くできたスタートアップファイルは以下例のようになります。
この例では、作成した名前が不要になると削除されるのに注目してください。
これは、スタートアップファイルが対話コマンドと同じ名前空間で実行されているので、
不要な名前を除去して対話環境に副作用を生まないようにするためです。
import されたモジュールのうち、 :mod:`os` のようなインタプリタのほとんどの
セッションで必要なものについては、残しておくと便利に思うかもしれません。

::

   # Add auto-completion and a stored history file of commands to your Python
   # interactive interpreter. Requires Python 2.0+, readline. Autocomplete is
   # bound to the Esc key by default (you can change it - see readline docs).
   #
   # Store the file in ~/.pystartup, and set an environment variable to point
   # to it:  "export PYTHONSTARTUP=~/.pystartup" in bash.

   import atexit
   import os
   import readline
   import rlcompleter

   historyPath = os.path.expanduser("~/.pyhistory")

   def save_history(historyPath=historyPath):
       import readline
       readline.write_history_file(historyPath)

   if os.path.exists(historyPath):
       readline.read_history_file(historyPath)

   atexit.register(save_history)
   del os, atexit, readline, rlcompleter, save_history, historyPath


.. _tut-commentary:

インタラクティブインタプタの代替
================================

この機能は、初期の版のインタプリタに比べれば大きな進歩です。とはいえ、
まだいくつかの要望が残されています。例えば、行を継続するときに正しい
インデントが提示されたら快適でしょう (パーサは次の行でインデントトークンが
必要かどうかを知っています)。
補完機構がインタプリタのシンボルテーブルを使ってもよいかもしれません。
括弧やクォートなどの対応をチェックする (あるいは指示する) コマンドも
有用でしょう。

より優れたインタラクティブインタプリタの代替の一つに `IPython`_ があります。
このインタプリタは、様々なところで使われていて、タブ補完、オブジェクト探索や
先進的な履歴管理といった機能を持っています。
他のアプリケーションにカスタマイズされたり、組込まれこともあります。
別の優れたインタラクティブ環境としては `bpython`_ があります。


.. rubric:: 注記

.. [#] 訳注: これはデフォルト設定の Readline では現れません。
   ``set mark-modified-lines on`` という行を :file:`~/.inputrc` または環境変数
   :envvar:`INPUTRC` が指定するファイルに置くことによって現れるようになります。

.. [#] 訳注: このファイル名は環境変数 :envvar:`INPUTRC` がもしあればその指定が
   優先されます。

.. [#] Python は、対話インタプリタを開始する時に :envvar:`PYTHONSTARTUP`
   環境変数が指定するファイルの内容を実行します。

.. _GNU Readline: http://tiswww.case.edu/php/chet/readline/rltop.html
.. _IPython: http://ipython.scipy.org/
.. _bpython: http://www.bpython-interpreter.org/
