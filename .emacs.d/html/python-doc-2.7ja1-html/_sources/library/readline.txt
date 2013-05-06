:mod:`readline` --- GNU readline のインタフェース
=================================================

.. module:: readline
   :platform: Unix
   :synopsis: Python のための GNU readline サポート。
.. sectionauthor:: Skip Montanaro <skip@pobox.com>


:mod:`readline` モジュールでは、補完をしやすくしたり、ヒストリファイルを Python インタプリタから読み書きできるように
するためのいくつかの関数を定義しています。このモジュールは直接使うことも :mod:`rlcompleter` モジュールを介して使うこともできます。
このモジュールで利用される設定はインタプリタの対話プロンプトの振舞い、
組み込みの :func:`raw_input` と :func:`input` 関数の振舞いに影響します。

.. notes:

  MacOS X では :mod:`readline` モジュールは GNU readline の代わりに ``libedit`` ライブラリを使って実装されています。

  ``libedit`` の設定ファイルは GNU readline と異なります。プログラム上から設定文字列を読む場合、
  :const:`readline.__doc__` の "libedit" テキストで GNU readline と libedit の違いをチェックできます。

:mod:`readline` モジュールでは以下の関数を定義しています:


.. function:: parse_and_bind(string)

   readline 初期化ファイルの行を一行解釈して実行します。


.. function:: get_line_buffer()

   行編集バッファの現在の内容を返します。


.. function:: insert_text(string)

   コマンドラインにテキストを挿入します。


.. function:: read_init_file([filename])

   readline 初期化ファイルを解釈します。標準のファイル名設定は最後に使われたファイル名です。


.. function:: read_history_file([filename])

   readline ヒストリファイルを読み出します。標準のファイル名設定は :file:`~/.history` です。


.. function:: write_history_file([filename])

   readline ヒストリファイルを保存します。標準のファイル名設定は :file:`~/.history` です。


.. function:: clear_history()

   現在のヒストリをクリアします。 (注意:インストールされている GNU readline がサポートしていない場合、この関数は利用できません)

   .. versionadded:: 2.4


.. function:: get_history_length()

   ヒストリファイルに必要な長さを返します。負の値はヒストリファイルのサイズに制限がないことを示します。


.. function:: set_history_length(length)

   ヒストリファイルに必要な長さを設定します。この値は :func:`write_history_file` がヒストリを保存する際にファイルを
   切り詰めるために使います。負の値はヒストリファイルのサイズを制限しないことを示します。


.. function:: get_current_history_length()

   現在のヒストリ行数を返します(この値は :func:`get_history_length` で取
   得する異なります。 :func:`get_history_length` はヒストリファイルに書き出される最大行数を返します)。

   .. versionadded:: 2.3


.. function:: get_history_item(index)

   現在のヒストリから、 *index* 番目の項目を返します。

   .. versionadded:: 2.3


.. function:: remove_history_item(pos)

   ヒストリから指定した位置にあるヒストリを削除します。

   .. versionadded:: 2.4


.. function:: replace_history_item(pos, line)

   指定した位置にあるヒストリを、指定した line で置き換えます。

   .. versionadded:: 2.4


.. function:: redisplay()

   画面の表示を、現在のヒストリ内容によって更新します。

   .. versionadded:: 2.3


.. function:: set_startup_hook([function])

   startup_hook 関数を設定または除去します。 *function* が指定されていれば、新たな startup_hook 関数として用いられます;
   省略されるか ``None`` になっていれば、現在インストールされているフック関数は除去されます。 startup_hook 関数は readline
   が最初のプロンプトを出力する直前に引数なしで呼び出されます。


.. function:: set_pre_input_hook([function])

   pre_input_hook 関数を設定または除去します。 *function* が指定されていれば、新たな pre_input_hook
   関数として用いられます;  省略されるか ``None`` になっていれば、現在インストールされているフック関数は除去されます。 pre_input_hook
   関数は readline が最初のプロンプトを出力した後で、かつ readline が入力された文字を読み込み始める直前に引数なしで呼び出されます。


.. function:: set_completer([function])

   completer 関数を設定または除去します。 *function* が指定されていれば、新たな completer 関数として用いられます;  省略されるか
   ``None`` になっていれば、現在インストールされている completer 関数は除去されます。 completer 関数は
   ``function(text, state)`` の形式で、関数が文字列でない値を返すまで *state* を ``0``, ``1``, ``2``,
   ..., にして呼び出します。この関数は *text* から始まる文字列の補完結果として可能性のあるものを返さなくてはなりません。


.. function:: get_completer()

   completer 関数を取得します。completer 関数が設定されていなければ ``None`` を返します。

   .. versionadded:: 2.3


.. function:: get_completion_type()

   .. Get the type of completion being attempted.

   実行中の補完のタイプを取得します。

   .. versionadded:: 2.6

.. function:: get_begidx()

   readline タブ補完スコープの先頭のインデクスを取得します。


.. function:: get_endidx()

   readline タブ補完スコープの末尾のインデクスを取得します。


.. function:: set_completer_delims(string)

   タブ補完のための readline 単語区切り文字を設定します。


.. function:: get_completer_delims()

   タブ補完のための readline 単語区切り文字を取得します。

.. function:: set_completion_display_matches_hook([function])

   .. Set or remove the completion display function.  If *function* is
      specified, it will be used as the new completion display function;
      if omitted or ``None``, any completion display function already
      installed is removed.  The completion display function is called as
      ``function(substitution, [matches], longest_match_length)`` once
      each time matches need to be displayed.

   補完表示関数を設定あるいは解除します。
   *function* が指定された場合、それが新しい補完表示関数として利用されます。
   省略されたり、 ``None`` が渡された場合、既に設定されていた補完表示関数が解除されます。
   補完表示関数は、マッチの表示が必要になるたびに、
   ``function(substitution, [matches], longest_match_length)``
   という形で呼び出されます。

   .. versionadded:: 2.6

.. function:: add_history(line)

   1 行をヒストリバッファに追加し、最後に打ち込まれた行のようにします。

.. seealso::

   Module :mod:`rlcompleter`
      対話的プロンプトで Python 識別子を補完する機能。


.. _readline-example:

例
--

以下の例では、ユーザのホームディレクトリにある :file:`.pyhist` という
名前のヒストリファイルを自動的に読み書きするために、 :mod:`readline` モジュールによるヒストリの読み書き関数をどのように使うかを例示しています。
以下のソースコードは通常、対話セッションの中で :envvar:`PYTHONSTARTUP` ファイルから読み込まれ自動的に実行されることになります。 ::

   import os
   import readline
   histfile = os.path.join(os.path.expanduser("~"), ".pyhist")
   try:
       readline.read_history_file(histfile)
   except IOError:
       pass
   import atexit
   atexit.register(readline.write_history_file, histfile)
   del os, histfile

次の例では :class:`code.InteractiveConsole` クラスを拡張し、ヒストリの保存・復旧をサポートします。 ::

   import code
   import readline
   import atexit
   import os

   class HistoryConsole(code.InteractiveConsole):
       def __init__(self, locals=None, filename="<console>",
                    histfile=os.path.expanduser("~/.console-history")):
           code.InteractiveConsole.__init__(self, locals, filename)
           self.init_history(histfile)

       def init_history(self, histfile):
           readline.parse_and_bind("tab: complete")
           if hasattr(readline, "read_history_file"):
               try:
                   readline.read_history_file(histfile)
               except IOError:
                   pass
               atexit.register(self.save_history, histfile)

       def save_history(self, histfile):
           readline.write_history_file(histfile)

