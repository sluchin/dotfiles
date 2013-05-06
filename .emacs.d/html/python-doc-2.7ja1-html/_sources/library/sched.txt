:mod:`sched` --- イベントスケジューラ
=====================================

.. module:: sched
   :synopsis: 一般的な目的のためのイベントスケジューラ
.. sectionauthor:: Moshe Zadka <moshez@zadka.site.co.il>

.. index:: single: event scheduling

:mod:`sched` モジュールは一般的な目的のためのイベントスケジューラを実装するクラスを定義します:

.. seealso::

   最新バージョンの `sched モジュールの Python ソースコード
   <http://svn.python.org/view/python/branches/release27-maint/Lib/sched.py?view=markup>`_

.. class:: scheduler(timefunc, delayfunc)

   :class:`scheduler` クラスはイベントをスケジュールするための一般的なインターフェースを定義します。それは"外部世界"を実際に扱うための
   2つの関数を必要とします --- *timefunc* は引数なしで呼出し可能であるべきで、そして数(それは"time"です, どんな単位でもかまいません)
   を返すようにします。 *delayfunc* は1つの引数(*timefunc* の出力と互換)で呼出し可能であり、その時間だけ遅延しなければいけません。
   各々のイベントが、マルチスレッドアプリケーションの中で他のスレッドが実行する機会の許可を実行した後に、 *delayfunc* は引数 ``0`` で呼
   ばれるでしょう。

例::

   >>> import sched, time
   >>> s = sched.scheduler(time.time, time.sleep)
   >>> def print_time(): print "From print_time", time.time()
   ...
   >>> def print_some_times():
   ...     print time.time()
   ...     s.enter(5, 1, print_time, ())
   ...     s.enter(10, 1, print_time, ())
   ...     s.run()
   ...     print time.time()
   ...
   >>> print_some_times()
   930343690.257
   From print_time 930343695.274
   From print_time 930343700.273
   930343700.276

.. In multi-threaded environments, the :class:`scheduler` class has limitations
.. with respect to thread-safety, inability to insert a new task before
.. the one currently pending in a running scheduler, and holding up the main
.. thread until the event queue is empty.  Instead, the preferred approach
.. is to use the :class:`threading.Timer` class instead.

マルチスレッド環境において、 :class:`scheduler` クラスにはスレッドセーフのための制限があります。
現在実行中のスケジューラに対して、中断中のタスクよりも前に新しいタスクを挿入することはできません。
もし挿入しようとすると、メインスレッドはイベントキューが空になるまで動作しなくなります。
代わりに、より推奨される方法として、 :class:`threading.Timer` クラスを利用してください。

.. Example

例::

    >>> import time
    >>> from threading import Timer
    >>> def print_time():
    ...     print "From print_time", time.time()
    ...
    >>> def print_some_times():
    ...     print time.time()
    ...     Timer(5, print_time, ()).start()
    ...     Timer(10, print_time, ()).start()
    ...     time.sleep(11)  # sleep while time-delay events execute
    ...     print time.time()
    ...
    >>> print_some_times()
    930343690.257
    From print_time 930343695.274
    From print_time 930343700.273
    930343701.301



.. _scheduler-objects:

スケジューラオブジェクト
------------------------

:class:`scheduler` インスタンスは以下のメソッドと属性を持っています:


.. method:: scheduler.enterabs(time, priority, action, argument)

   新しいイベントをスケジュールします。引数 *time* は、コンストラクタへ渡された *timefunc* の戻り値と互換な数値型でなければいけません。
   同じ *time* によってスケジュールされたイベントは、それらの *priority* によって実行されるでしょう。

   イベントを実行することは、 ``action(*argument)`` を実行することを意味します。
   *argument* は *action* のためのパラメータを保持するシーケンスでなければいけません。

   戻り値は、イベントのキャンセル後に使われるかもしれないイベントです (:meth:`cancel` を見よ)。


.. method:: scheduler.enter(delay, priority, action, argument)

   時間単位以上の *delay* でイベントをスケジュールします。そのとき、その他の関連時間、その他の引数、効果、戻り値は、
   :meth:`enterabs` に対するものと同じです。


.. method:: scheduler.cancel(event)

   キューからイベントを消去します。もし *event* がキューにある現在のイベントでないならば、
   このメソッドは :exc:`ValueError` を送出します。


.. method:: scheduler.empty()

   もしイベントキューが空ならば、Trueを返します。


.. method:: scheduler.run()

   すべてのスケジュールされたイベントを実行します。この関数は次のイベントを(コンストラクタへ渡された関数
   :func:`delayfunc` を使うことで)待ち、そしてそれを実行し、イベントがスケジュールされなくなるまで同じことを繰り返します。

   *action* あるいは *delayfunc* は例外を投げることができます。いずれの場合も、スケジューラは一貫した状態を維持し、例外を伝播するでしょう。
   例外が *action* によって投げられる場合、イベントは :meth:`run` への呼出しを未来に行なわないでしょう。

   イベントのシーケンスが、次イベントの前に、利用可能時間より実行時間が長いと、スケジューラは単に遅れることになるでしょう。イベントが落ちることはありません;
   呼出しコードはもはや適切でないキャンセルイベントに対して責任があります。

.. .. attribute:: scheduler.queue
..
..    Read-only attribute returning a list of upcoming events in the order they
..    will be run.  Each event is shown as a :term:`named tuple` with the
..    following fields:  time, priority, action, argument.
..
..    .. versionadded:: 2.6

.. attribute:: scheduler.queue

   読み込み専用の属性で、これからのイベントが実行される順序で格納されたリストを返します。
   各イベントは、次の属性 time, priority, action, argument を持った名前付きタプル (:term:`named tuple`) の形式になります。

   .. versionadded:: 2.6
