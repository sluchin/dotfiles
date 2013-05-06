
:mod:`linecache` --- テキストラインにランダムアクセスする
=========================================================

.. module:: linecache
   :synopsis: このモジュールによりテキストファイルの各行にランダムアクセスできます。
.. sectionauthor:: Moshe Zadka <moshez@zadka.site.co.il>


:mod:`linecache` モジュールは、キャッシュ (一つのファイルから何行も読んでおくのが一般的です)
を使って、内部で最適化を図りつつ、任意のファイルの任意の行を取得するのを可能にします。
:mod:`traceback` モジュールは、整形されたトレースバックにソースコードを含めるためにこの
モジュールを利用しています。

.. seealso::

   最新バージョンの `linecache module Python ソースコード
   <http://svn.python.org/view/python/branches/release27-maint/Lib/linecache.py?view=markup>`_

:mod:`linecache` モジュールでは次の関数が定義されています:


.. function:: getline(filename, lineno[, module_globals])

   *filename* という名前のファイルから *lineno* 行目を取得します。
   この関数は決して例外を発生させません --- エラーの際には ``''``
   を返します。 (行末の改行文字は、見つかった行に含まれます。)

   .. index:: triple: module; search; path

   *filename* という名前のファイルが見つからなかった場合、モジュールの、つまり、
   ``sys.path`` でそのファイルを探します。
   zipfileやその他のファイルシステムでないimport元に対応するためまず *modules_globals*
   の :pep:`302` ``__loader__`` をチェックし、そのあと ``sys.path`` を探索します。

   .. versionadded:: 2.5
      パラメータ *module_globals* の追加.


.. function:: clearcache()

   キャッシュをクリアします。それまでに :func:`getline` を使って読み込んだファイルの行が必要でなくなったら、この関数を使ってください。


.. function:: checkcache([filename])

   キャッシュが有効かチェックします。キャッシュしたファイルにディスク上で変更があったかもしれなくて、更新
   が必要なときにこの関数を使ってください。もし *filename* がなければ、全てのキャッシュエントリをチェックします。

サンプル::

   >>> import linecache
   >>> linecache.getline('/etc/passwd', 4)
   'sys:x:3:3:sys:/dev:/bin/sh\n'

