
:mod:`hotshot` --- ハイパフォーマンス・ロギング・プロファイラ
=============================================================

.. module:: hotshot
   :synopsis: コードの大半が C で書かれたハイパフォーマンス・ロギング・プロファイラ
.. moduleauthor:: Fred L. Drake, Jr. <fdrake@acm.org>
.. sectionauthor:: Anthony Baxter <anthony@interlink.com.au>


.. versionadded:: 2.2

このモジュールは :mod:`_hotshot` C モジュールへのより良いインターフェースを提供します。Hotshot は既存の
:mod:`profile` に置き換わるものです。その大半が C で書かれているため、 :mod:`profile` に比べパフォー
マンス上の影響がはるかに少なく済みます。

.. note::

   :mod:`hotshot` は C モジュールでプロファイル中のオーバーヘッドを極力小さくすることに焦点を絞っており、その代わりに後処理時間の長さという
   つけを払います。通常の使用法についてはこのモジュールではなく :mod:`cProfile` を使うことを推奨します。 :mod:`hotshot` は保守され
   ておらず、将来的には標準ライブラリから外されるかもしれません。

.. versionchanged:: 2.5
   以前より意味のある結果が得られているはずです。かつては時間計測の中核部分に致命的なバグがありました.

.. note::

   :mod:`hotshot` プロファイラはまだスレッド環境ではうまく動作しません。測定したいコード上でプロファイラを実行するためにスレッドを使わない版の
   スクリプトを使う方法が有用です。


.. class:: Profile(logfile[, lineevents[, linetimings]])

   プロファイラ・オブジェクト。引数 *logfile* はプロファイル・データのログを保存するファイル名です。引数 *lineevents* はソースコー
   ドの1 行ごとにイベントを発生させるか、関数の呼び出し/リターンのときだけ発生させるかを指定します。デフォルトの値は ``0`` (関数の呼び出し/
   リターンのときだけログを残す)です。引数 *linetimings* は時間情報を記録するかどうかを指定します。デフォルトの値は ``1`` (時間情報を記
   録する)です。


.. _hotshot-objects:

プロファイル・オブジェクト
--------------------------

プロファイル・オブジェクトは以下のメソッドを持っています。


.. method:: Profile.addinfo(key, value)

   プロファイル出力の際、任意のラベル名を追加します。


.. method:: Profile.close()

   ログファイルを閉じ、プロファイラを終了します。


.. method:: Profile.fileno()

   プロファイラのログファイルのファイル・ディスクリプタを返します。


.. method:: Profile.run(cmd)

   スクリプト環境で :keyword:`exec` 互換文字列のプロファイルをおこないます。 :mod:`__main__`
   モジュールのグローバル変数は、スクリプトのグローバル変数、ローカル変数の両方に使われます。


.. method:: Profile.runcall(func, *args, **keywords)

   単一の呼び出し可能オブジェクトのプロファイルをおこないます。位置依存引数やキーワード引数を追加して呼び出すオブジェクトに渡すこともできます。
   呼び出しの結果はそのまま返されます。例外が発生したときはプロファイリングが無効になり、例外をそのまま伝えるようになっています。


.. method:: Profile.runctx(cmd, globals, locals)

   指定した環境で :keyword:`exec` 互換文字列の評価をおこないます。文字列のコンパイルはプロファイルを開始する前におこなわれます。


.. method:: Profile.start()

   プロファイラを開始します。


.. method:: Profile.stop()

   プロファイラを停止します。


hotshot データの利用
--------------------

.. module:: hotshot.stats
   :synopsis: Hotshot の統計分析


.. versionadded:: 2.2

このモジュールは hotshot プロファイル・データを標準の :mod:`pstats` オブジェクトにロードします。


.. function:: load(filename)

   *filename* から hotshot データを読み込み、 :class:`pstats.Stats` クラスのインスタンスを返します。


.. seealso::

   Module :mod:`profile`
      :mod:`profile` モジュールの :class:`Stats` クラス


.. _hotshot-example:

使用例
------

これは Python の"ベンチマーク" pystone を使った例です。実行にはやや時間がかかり、巨大な出力ファイルを生成するので注意してください。 ::

   >>> import hotshot, hotshot.stats, test.pystone
   >>> prof = hotshot.Profile("stones.prof")
   >>> benchtime, stones = prof.runcall(test.pystone.pystones)
   >>> prof.close()
   >>> stats = hotshot.stats.load("stones.prof")
   >>> stats.strip_dirs()
   >>> stats.sort_stats('time', 'calls')
   >>> stats.print_stats(20)
            850004 function calls in 10.090 CPU seconds

      Ordered by: internal time, call count

      ncalls  tottime  percall  cumtime  percall filename:lineno(function)
           1    3.295    3.295   10.090   10.090 pystone.py:79(Proc0)
      150000    1.315    0.000    1.315    0.000 pystone.py:203(Proc7)
       50000    1.313    0.000    1.463    0.000 pystone.py:229(Func2)
    .
    .
    .

