:mod:`queue` --- 同期キュークラス
=================================

.. module:: Queue
   :synopsis: 同期キュークラス

.. note::
    :mod:`Queue` モジュールは Python 3.0 で :mod:`queue` という名前に変更されました。
    :term:`2to3` ツールは、自動的に import を変換します。

:mod:`Queue` モジュールは、多生産者-多消費者(multi-producer, multi-consumer)キューを実装します。
これは、複数のスレッドの間で情報を安全に交換しなければならないときのマルチスレッドプログラミングで特に有益です。
このモジュールの :class:`Queue` クラスは、必要なすべてのロックセマンティクスを実装しています。
これはPythonのスレッドサポートの状況に依存します。 :mod:`threading` モジュールを参照してください。

.. Implements three types of queue whose only difference is the order that
   the entries are retrieved.  In a FIFO queue, the first tasks added are
   the first retrieved. In a LIFO queue, the most recently added entry is
   the first retrieved (operating like a stack).  With a priority queue,
   the entries are kept sorted (using the :mod:`heapq` module) and the
   lowest valued entry is retrieved first.

3種類のキューが実装されていて、キューから取り出されるエントリの順番だけが違います。
FIFOキューでは、最初に追加されたエントリが最初に取り出されます。
LIFOキューでは、最後に追加されたエントリが最初に取り出されます(スタックのように振る舞います)。
優先順位付きキュー(priority queue)では、エントリは(:mod:`heapq` モジュールを利用して)ソートされ、
最も低い値のエントリが最初に取り出されます。

.. seealso::

   最新バージョンの `queue モジュールの Python ソースコード
   <http://svn.python.org/view/python/branches/release27-maint/Lib/Queue.py?view=markup>`_.

:mod:`Queue` モジュールは以下のクラスと例外を定義します:

.. class:: Queue(maxsize=0)

   FIFOキューのコンストラクタです。
   *maxsize* はキューに置くことのできる要素数の上限を設定する整数です。
   いったんこの大きさに達したら、挿入はキューの要素が消費されるまでブロックされます。もし *maxsize* が0以下であるならば、キューの大きさは無限です。

.. class:: LifoQueue(maxsize=0)

   LIFOキューのコンストラクタです。
   *maxsize* はキューに置くことのできる要素数の上限を設定する整数です。
   いったんこの大きさに達したら、挿入はキューの要素が消費されるまでブロックされます。もし *maxsize* が0以下であるならば、キューの大きさは無限です。

   .. versionadded:: 2.6

.. class:: PriorityQueue(maxsize=0)

   優先順位付きキューのコンストラクタです。
   *maxsize* はキューに置くことのできる要素数の上限を設定する整数です。
   いったんこの大きさに達したら、挿入はキューの要素が消費されるまでブロックされます。もし *maxsize* が0以下であるならば、キューの大きさは無限です。

.. The lowest valued entries are retrieved first (the lowest valued entry is the
   one returned by ``sorted(list(entries))[0]``).  A typical pattern for entries
   is a tuple in the form: ``(priority_number, data)``.

   最も低い値のエントリが最初に取り出されます。(最も低い値のエントリとは、 ``sorted(list(entries))[0]``
   の結果の事です)
   一般的なエントリの形式は、 ``(優先順位を表す値, data)`` になります。

   .. versionadded:: 2.6


.. exception:: Empty

   空な :class:`Queue` オブジェクトで、非ブロックメソッドとして :meth:`get` (または :meth:`get_nowait`)
   が呼ばれたとき、送出される例外です。


.. exception:: Full

   満杯な :class:`Queue` オブジェクトで、非ブロックメソッドとして :meth:`put` (または :meth:`put_nowait`)
   が呼ばれたとき、送出される例外です。

.. .. seealso::
..
..    :class:`collections.deque` is an alternative implementation of unbounded
..    queues with fast atomic :func:`append` and :func:`popleft` operations that
..    do not require locking.
..

.. seealso::

   :class:`collections.deque` は、ロックなしで :func:`popleft` や :func:`append`
   といったアトミック操作が可能なキューの実装です。


.. _queueobjects:

キューオブジェクト
------------------

.. Queue objects (:class:`Queue`, :class:`LifoQueue`, or :class:`PriorityQueue`)
   provide the public methods described below.

キューオブジェクト(:class:`Queue`, :class:`LifoQueue`, :class:`PriorityQueue`)は、
以下のpublicメソッドを提供しています。

.. method:: Queue.qsize()

.. Return the approximate size of the queue.  Note, qsize() > 0 doesn't
.. guarantee that a subsequent get() will not block, nor will qsize() < maxsize
.. guarantee that put() will not block.

   キューの大まかなサイズを返します。
   qsize() > 0 でも、次の get() がブロックしないことは保証されないので、注意してください。
   同じく、 qsize() < maxsize でも、次の put() がブロックしないことは保証されません。

.. method:: Queue.empty()

.. Return ``True`` if the queue is empty, ``False`` otherwise.  If empty()
.. returns ``True`` it doesn't guarantee that a subsequent call to put()
.. will not block.  Similarly, if empty() returns ``False`` it doesn't
.. guarantee that a subsequent call to get() will not block.

   キューが空なら ``True`` を返し、そうでないなら ``False`` を返します。
   empty() が True を返しても、次の put() がブロックしないことは保証されません。
   同じく、 empty() が False を返しても、次の get() がブロックしないことは保証されません。


.. method:: Queue.full()

.. Return ``True`` if the queue is full, ``False`` otherwise.  If full()
.. returns ``True`` it doesn't guarantee that a subsequent call to get()
.. will not block.  Similarly, if full() returns ``False`` it doesn't
.. guarantee that a subsequent call to put() will not block.

   キューが満杯なら ``True`` を返し、そうでないなら ``False`` を返します。
   full() が ``True`` を返しても、続く get() がブロックしないことは保証されません。
   同じく、 full() が ``False`` を返しても、続く put() がブロックしないことは保証されません。

.. method:: Queue.put(item[, block[, timeout]])

   *item* をキューに入れます。もしオプション引数 *block* がTrueで *timeout* がNone(デフォルト)ならば、
   フリースロットが利用可能になるまでブロックします。 *timeout* が正の値の場合、最大で *timeout* 秒間ブロックし、
   その時間内に空きスロットが利用可能にならなければ、例外 :exc:`Full` を送出します。
   他方(*block* がFalse)、直ちにフリースロットが利用できるならば、キューにアイテムを置きます。できないならば、例外 :exc:`Full` を送出します
   (この場合 *timeout* は無視されます)。

   .. versionadded:: 2.3
      *timeout* 引数が追加されました。


.. method:: Queue.put_nowait(item)

   ``put(item, False)`` と同じ意味です。


.. method:: Queue.get([block[, timeout]])

   キューからアイテムを取り除き、それを返します。もしオプション引数 *block* がTrueで *timeout* がNone(デフォルト)ならば、
   アイテムが利用可能になるまでブロックします。もし *timeout* が正の値の場合、最大で *timeout* 秒間ブロックし、
   その時間内でアイテムが利用可能にならなければ、例外 :exc:`Empty` を送出します。
   他方(*block* がFalse)、直ちにアイテムが利用できるならば、それを返します。できないならば、例外 :exc:`Empty` を送出します
   (この場合 *timeout* は無視されます)。

   .. versionadded:: 2.3
      *timeout* 引数が追加されました。


.. method:: Queue.get_nowait()

   ``get(False)`` と同じ意味です。

キューに入れられたタスクが全て消費者スレッドに処理されたかどうかを追跡するために 2つのメソッドが提供されます。


.. method:: Queue.task_done()

   過去にキューに入れられたタスクが完了した事を示します。キューの消費者スレッドに利用されます。タスクの取り出しに使われた、各 :meth:`get`
   に対して、それに続く :meth:`task_done` の呼び出しは、取り出したタスクに対する処理が完了した事をキューに教えます。

   :meth:`join` がブロックされていた場合、全itemが処理された (キューに :meth:`put` された全てのitemに対して
   :meth:`task_done` が呼び出されたことを意味します) 時に復帰します。

   キューにあるよりitemの個数よりも多く呼び出された場合、 :exc:`ValueError` が送出されます。

   .. versionadded:: 2.5


.. method:: Queue.join()

   キューの中の全アイテムが処理される間でブロックします。

   キューにitemが追加される度に、未完了タスクカウントが増やされます。消費者スレッドが :meth:`task_done`
   を呼び出して、itemを受け取ってそれに対する処理が完了した事を知らせる度に、未完了タスクカウントが減らされます。
   未完了タスクカウントが0になったときに、join() のブロックが解除されます。

   .. versionadded:: 2.5

キューに入れたタスクが完了するのを待つ例::

   def worker():
       while True:
           item = q.get()
           do_work(item)
           q.task_done()

   q = Queue()
   for i in range(num_worker_threads):
        t = Thread(target=worker)
        t.daemon = True
        t.start()

   for item in source():
       q.put(item)

   q.join()       # 全タスクが完了するまでブロック
