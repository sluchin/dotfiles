
:mod:`tabnanny` --- あいまいなインデントの検出
==============================================

.. module:: tabnanny
   :synopsis: ディレクトリツリー内の Python のソースファイルで問題となる空白を検出するツール。
.. moduleauthor:: Tim Peters <tim_one@users.sourceforge.net>
.. sectionauthor:: Peter Funk <pf@artcom-gmbh.de>


.. rudimentary documentation based on module comments

差し当たり、このモジュールはスクリプトとして呼び出すことを意図しています。
しかし、IDE 上にインポートして下で説明する関数 :func:`check` を使うことができます。

.. note::

   このモジュールが提供する API を将来のリリースで変更する確率が高いです。
   このような変更は後方互換性がないかもしれません。


.. function:: check(file_or_dir)

   *file_or_dir* がディレクトリであってシンボリックリンクでないときに、
   *file_or_dir* という名前のディレクトリツリーを再帰的に下って行き、
   この通り道に沿ってすべての :file:`.py` ファイルを変更します。
   *file_or_dir* が通常の Python ソースファイルの場合には、
   問題のある空白をチェックします。
   診断メッセージは print 文を使って標準出力に書き込まれます。


.. data:: verbose

   冗長なメッセージをプリントするかどうかを示すフラグ。
   スクリプトとして呼び出された場合は、 ``-v`` オプションによって増加します。


.. data:: filename_only

   問題のある空白を含むファイルのファイル名のみをプリントするかどうかを示すフラグ。
   スクリプトとして呼び出された場合は、 ``-q`` オプションによって真に設定されます。


.. exception:: NannyNag

   あいまいなインデントを検出した場合に :func:`tokeneater` によって発生させられます。
   :func:`check` で捕捉され処理されます。


.. function:: tokeneater(type, token, start, end, line)

   この関数は関数 :func:`tokenize.tokenize` へのコールバックパラメータとして
   :func:`check` によって使われます。

.. XXX document errprint, format_witnesses, Whitespace, check_equal, indents,
   reset_globals


.. seealso::

   :mod:`tokenize` モジュール
      Pythonソースコードの字句解析器。
