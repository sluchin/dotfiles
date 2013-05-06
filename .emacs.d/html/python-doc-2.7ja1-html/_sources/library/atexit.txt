
:mod:`atexit` --- 終了ハンドラ
==============================

.. module:: atexit
   :synopsis: 後始末関数の登録と実行。
.. moduleauthor:: Skip Montanaro <skip@pobox.com>
.. sectionauthor:: Skip Montanaro <skip@pobox.com>


.. versionadded:: 2.0

:mod:`atexit` モジュールでは、後始末関数を登録するための関数を一つだけ定義しています。この関数を使って登録した後始末関数は、インタプリタが
終了するときに自動的に実行されます。

.. seealso::

   最新バージョンの `atexit Python ソースコード
   <http://svn.python.org/view/python/branches/release27-maint/Lib/atexit.py?view=markup>`_

.. note::

   プログラムが Python で処理されないシグナルで停止させられたとき、Python の致命的な内部エラーが
   検出されたとき、あるいは :func:`os._exit` が呼び出されたときには、このモジュールを通して
   登録した関数は呼び出されません。

.. index:: single: exitfunc (in sys)

このモジュールは、 ``sys.exitfunc`` 変数の提供している機能の代用となるインタフェースです。

.. note::

   ``sys.exitfunc`` を設定する他のコードとともに使用した場合には、このモジュールは正しく動作しないでしょう。特に、他のコア Python
   モジュールでは、プログラマの意図を知らなくても :mod:`atexit` を自由に使えます。 ``sys.exitfunc`` を使っている人は、代わりに
   :mod:`atexit` を使うコードに変換してください。 ``sys.exitfunc`` を設定するコードを変換するには、 :mod:`atexit` を
   import し、 ``sys.exitfunc`` へ束縛されていた関数を登録するのが最も簡単です。


.. function:: register(func[, *args[, **kargs]])

   終了時に実行される関数として *func* を登録します。すべての *func* へ渡すオプションの引数を、
   :func:`register` へ引数としてわたさなければなりません。

   通常のプログラムの終了時、例えば :func:`sys.exit` が呼び出されるとき、あるいは、メインモジュールの実行が完了したときに、登録された全ての
   関数を、最後に登録されたものから順に呼び出します。通常、より低レベルのモジュールはより高レベルのモジュールより前に import されるので、
   後で後始末が行われるという仮定に基づいています。

   終了ハンドラの実行中に例外が発生すると、(:exc:`SystemExit` 以外の場合は)トレースバックを表示して、例外の情報を保存します。
   全ての終了ハンドラに動作するチャンスを与えた後に、最後に送出された例外を再送出します。

   .. versionchanged:: 2.6
      この関数をデコレータとして利用できるように、 *func* を返すようになりました。
      以前は ``None`` を返していたので、デコレータとして利用しようとすると、関数名の変数に
      ``None`` が代入されてしまっていました。

      .. This function now returns *func* which makes it possible to use it as a
         decorator without binding the original name to ``None``.

.. seealso::

   Module :mod:`readline`
      :mod:`readline` ヒストリファイルを読み書きするための :mod:`atexit` の有用な例です。


.. _atexit-example:

:mod:`atexit` の例
-------------------

次の簡単な例では、あるモジュールを import した時にカウンタを初期化しておき、プログラムが終了するときにアプリケーションがこのモジュールを明
示的に呼び出さなくてもカウンタが更新されるようにする方法を示しています。 ::

   try:
       _count = int(open("/tmp/counter").read())
   except IOError:
       _count = 0

   def incrcounter(n):
       global _count
       _count = _count + n

   def savecounter():
       open("/tmp/counter", "w").write("%d" % _count)

   import atexit
   atexit.register(savecounter)

:func:`register` に指定した固定引数とキーワードパラメタは登録した関数を呼び出す際に渡されます。 ::

   def goodbye(name, adjective):
       print 'Goodbye, %s, it was %s to meet you.' % (name, adjective)

   import atexit
   atexit.register(goodbye, 'Donny', 'nice')

   # or:
   atexit.register(goodbye, adjective='nice', name='Donny')

.. Usage as a :term:`decorator`

デコレータ(:term:`decorator`)として利用する例::

   import atexit

   @atexit.register
   def goodbye():
       print "You are now leaving the Python sector."

.. This obviously only works with functions that don't take arguments.

もちろん、デコレータとして利用できるのは、その関数が引数を受け取らない場合に限られます。
