:mod:`trace` --- Python ステートメント実行のトレースと追跡
==========================================================

.. module:: trace
   :synopsis: Python ステートメント実行のトレースと追跡


:mod:`trace` モジュールはプログラム実行のトレースを可能にし、
generate ステートメントのカバレッジリストを注釈付きで生成して、
呼び出し元/呼び出し先の関連やプログラム実行中に実行された関数のリストを出力します。
これは別個のプログラム中またはコマンドラインから利用することができます。

.. seealso::

   最新バージョンの `trace module Python source code
   <http://svn.python.org/view/python/branches/release27-maint/Lib/trace.py?view=markup>`_

.. _trace-cli:

コマンドラインからの利用
------------------------

:mod:`trace` モジュールはコマンドラインから起動することができます。
これは次のように単純です。 ::

   python -m trace --count -C . somefile.py ...

これで、 :file:`somefile.py` の実行中に import された Python 
モジュールの注釈付きリストがカレントディレクトリに生成されます。


.. program:: trace

.. cmdoption:: --help

   使い方を表示して終了します。

.. cmdoption:: --version

   モジュールのバージョンを表示して終了します。

主要なオプション
^^^^^^^^^^^^^^^^

:mod:`trace` を実行する時、以下のオプションのうち少なくとも1つを指定しなければなりません。
:option:`--listfuncs <-l>` オプションは :option:`--trace <-t>` および
:option:`--counts <-c>` オプションと互いに排他的です。
つまり :option:`--listfuncs <-l>` オプションを指定した場合 :option:`--trace <-t>`
や :option:`--counts <-c>` は指定できず、逆も然りです。

.. program:: trace

.. cmdoption:: -c, --count

   プログラム完了時に、それぞれのステートメントが何回実行されたかを示す
   注釈付きリストのファイルを生成します。
   下記の :option:`--coverdir <-C>`, :option:`--file <-f>`, :option:`--no-report <-R>` も参照。

.. cmdoption:: -t, --trace

   実行されるままに行を表示します。

.. cmdoption:: -l, --listfuncs

   プログラム実行の際に実行された関数を表示します。

.. cmdoption:: -r, --report

   :option:`--count <-c>` と :option:`--file <-f>` 引数を使った、過去のプログラム実行結果から
   注釈付きリストのファイルを生成します。コードを実行するわけではありません。

.. cmdoption:: -T, --trackcalls

   プログラム実行によって明らかになった呼び出しの関連を表示します。

修飾的オプション
^^^^^^^^^^^^^^^^

.. program:: trace

.. cmdoption:: -f, --file=<file>

   複数回にわたるトレース実行についてカウント(count)を蓄積するファイルに名前をつけます。
   :option:`--count <-c>` オプションと一緒に使って下さい。

.. cmdoption:: -C, --coverdir=<dir>

   レポートファイルを保存するディレクトリを指定します。
   ``package.module`` についてのカバレッジレポートは
   :file:`{dir}/{package}/{module}.cover` に書き込まれます。

.. cmdoption:: -m, --missing

   注釈付きリストの生成時に、実行されなかった行に ``>>>>>>`` の印を付けます。

.. cmdoption:: -s, --summary

   :option:`--count <-c>` または :option:`--report <-r>` の利用時に、
   処理されたファイルそれぞれの簡潔なサマリを標準出力(stdout)に書き出します。

.. cmdoption:: -R, --no-report

   注釈付きリストを生成しません。これは :option:`--count <-c>` を何度か走らせてから
   最後に単一の注釈付きリストを生成するような場合に便利です。

.. cmdoption:: -g, --timing

   各行の先頭にプログラム開始からの時間を付けます。トレース中にだけ使われます。


フィルターオプション
^^^^^^^^^^^^^^^^^^^^

これらのオプションは複数回指定できます。

.. program:: trace

.. cmdoption:: --ignore-module=<mod>

   指定されたモジュールと（パッケージだった場合は）そのサブモジュールを無視します。
   引数はカンマ区切りのモジュール名リストです。

.. cmdoption:: --ignore-dir=<dir>

   指定されたディレクトリとサブディレクトリ中のモジュールとパッケージを全て無視します。
   引数は :data:`os.pathsep` で区切られたディレクトリのリストです。


.. _trace-api:

プログラミングインターフェース
------------------------------


.. class:: Trace([count=1[, trace=1[, countfuncs=0[, countcallers=0[, ignoremods=()[, ignoredirs=()[, infile=None[, outfile=None[, timing=False]]]]]]]]])

   文(statement)や式(expression)の実行をトレースするオブジェクトを作成します。
   全てのパラメタがオプションです。
   *count* は行数を数えます。
   *trace* は行実行のトレースを行います。
   *countfuncs* は実行中に呼ばれた関数を列挙します。
   *countcallers* は呼び出しの関連の追跡を行います。
   *ignoremods* は無視するモジュールやパッケージのリストです。
   *ignoredirs* は無視するパッケージやモジュールを含むディレクトリのリストです。
   *infile* は保存された集計(count)情報を読むファイルの名前です。
   *outfile* は更新された集計(count)情報を書き出すファイルの名前です。
   *timing* は、タイムスタンプをトレース開始時点からの相対秒数で表示します。


    .. method:: run(cmd)

       コマンドを実行して、現在のトレースパラメータに基づいてその実行から
       統計情報を集めます。
       *cmd* は、 :func:`exec` に渡せるような文字列か code オブジェクトです。


    .. method:: runctx(cmd[, globals=None[, locals=None]])

       指定された globals と locals 環境下で、コマンドを実行して、現在の
       トレースパラメータに基づいてその実行から統計情報を集めます。
       *cmd* は、 :func:`exec` に渡せるような文字列か code オブジェクトです。
       定義しない場合、 *globals* と *locals* はデフォルトで空の辞書となります。


    .. method:: runfunc(func, *args, **kwds)

       与えられた引数の *func* を、 :class:`Trace` オブジェクトのコントロール下で
       現在のトレースパラメタのもとに呼び出します。

    .. method:: results()

       与えられた :class:`Trace` インスタンスの ``run``, ``runctx``, ``runfunc``
       の以前の呼び出しについて集計した結果を納めた :class:`CoverageResults`
       オブジェクトを返します。蓄積されたトレース結果はリセットしません。

.. class:: CoverageResults

   カバレッジ結果のコンテナで、 :meth:`Trace.results` で生成されるものです。
   ユーザーが直接生成するものではありません。

    .. method:: update(other)

       別の :class:`CoverageResults` オブジェクトのデータを統合します。

    .. method:: write_results([show_missing=True[, summary=False[, coverdir=None]]])

       カバレッジ結果を書き出します。
       ヒットしなかった行も出力するには *show_missing* を指定します。
       モジュールごとのサマリーを出力に含めるには *summary* を指定します。
       *coverdir* に指定するのは結果ファイルを出力するディレクトリです。
       ``None`` の場合は各ソースファイルごとの結果がそれぞれのディレクトリに置かれます。

簡単な例でプログラミングインターフェースの使い方を見てみましょう ::

   import sys
   import trace

   # Trace オブジェクトを、無視するもの、トレースや行カウントのいずれか
   # または両方を行うか否かを指定して作成します。
   tracer = trace.Trace(
       ignoredirs=[sys.prefix, sys.exec_prefix],
       trace=0,
       count=1)

   # 与えられたトレーサを使って、コマンドを実行します。
   tracer.run('main()')

   # 出力先を /tmp としてレポートを作成します。
   r = tracer.results()
   r.write_results(show_missing=True, coverdir="/tmp")

