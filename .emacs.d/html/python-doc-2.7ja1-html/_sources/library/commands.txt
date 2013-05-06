
:mod:`commands` --- コマンド実行ユーティリティ
==============================================

.. module:: commands
   :platform: Unix
   :synopsis: 外部コマンドを実行するためのユーティリティです。
   :deprecated:

.. deprecated:: 2.6
   :mod:`commands` モジュールは Python 3.0 で削除されました。
   代わりに :mod:`subprocess` モジュールを使ってください。

.. sectionauthor:: Sue Williams <sbw@provis.com>


:mod:`commands` は、システムへコマンド文字列を渡して実行する
:func:`os.popen` のラッパー関数を含んでいるモジュールです。
外部で実行したコマンドの結果や、その終了ステータスを扱います。

:mod:`subprocess` がプロセスを生成してその結果を取得するためのより強力な\
手段を提供しています。 :mod:`subprocess` モジュールを使う方が :mod:`commands`
モジュールを使うより好ましいです。

.. note::

   Python 3.x において、 :func:`getstatus` および二つの隠し関数(:func:`mk2arg` と
   :func:`mkarg`) は削除されました。また、 :func:`getstatusoutput` と
   :func:`getoutput` は :mod:`subprocess` モジュールに移動されました。

:mod:`commands` モジュールは以下の関数を定義しています。


.. function:: getstatusoutput(cmd)

   文字列 *cmd* を :func:`os.popen` を使いシェル上で実行し、
   タプル ``(status, output)`` を返します。
   実際には ``{ cmd; } 2>&1`` と実行されるため、標準出力とエラー出力が混合されます。
   また、出力の最後の改行文字は取り除かれます。
   コマンドの終了ステータスはC言語関数の :c:func:`wait` の規則に従って\
   解釈することができます。


.. function:: getoutput(cmd)

   :func:`getstatusoutput` に似ていますが、終了ステータスは無視され、\
   コマンドの出力のみを返します。


.. function:: getstatus(file)

   ``ls -ld file`` の出力を文字列で返します。
   この関数は :func:`getoutput` を使い、引数内の
   バックスラッシュ記号「\\」とドル記号「$」を適切にエスケープします。

   .. deprecated:: 2.6
      この関数は明らかでないですし役立たずです。名前も :func:`getstatusoutput`
      の前では誤解を招くものです。


例::

   >>> import commands
   >>> commands.getstatusoutput('ls /bin/ls')
   (0, '/bin/ls')
   >>> commands.getstatusoutput('cat /bin/junk')
   (256, 'cat: /bin/junk: No such file or directory')
   >>> commands.getstatusoutput('/bin/junk')
   (256, 'sh: /bin/junk: not found')
   >>> commands.getoutput('ls /bin/ls')
   '/bin/ls'
   >>> commands.getstatus('/bin/ls')
   '-rwxr-xr-x  1 root        13352 Oct 14  1994 /bin/ls'


.. seealso::

   :mod:`subprocess` モジュール
      サブプロセスの生成と管理のためのモジュール。
