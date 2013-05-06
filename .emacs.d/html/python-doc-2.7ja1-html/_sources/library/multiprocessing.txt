:mod:`multiprocessing` --- プロセスベースの "並列処理" インタフェース
=====================================================================

.. module:: multiprocessing
   :synopsis: Process-based "threading" interface.

.. versionadded:: 2.6


はじめに
--------

:mod:`multiprocessing` はPythonの標準ライブラリのパッケージで
:mod:`threading` とよく似た API を使ってプロセスを生成することができます。
:mod:`multiprocessing` パッケージを使用すると、
ローカルとリモート両方の並列制御を行うことができます。
また、このパッケージはスレッドの代わりにサブプロセスを使用することにより、
グローバルインタプリタロック ( :term:`Global Interpreter Lock` )
の問題を避ける工夫が行われています。
このような特徴があるため :mod:`multiprocessing` モジュールを使うことで、
マルチプロセッサマシンの性能を最大限に活用することができるでしょう。
なお、このモジュールは Unix と Windows で動作します。

.. warning::

    このパッケージに含まれる機能には、ホストとなるオペレーティングシステム上で
    動作している共有セマフォ (shared semaphore) を使用しているものがあります。
    これが使用できない場合には、 :mod:`multiprocessing.synchronize` モジュールが無効になり、
    このモジュールのインポート時に :exc:`ImportError` が発生します。
    詳細は :issue:`3770` を参照してください。

.. note::

    このパッケージに含まれる機能を使用するためには、子プロセスから
    ``__main__`` モジュールを呼び出せる必要があります。
    このことについては :ref:`multiprocessing-programming` で触れていますが、
    ここであらためて強調しておきます。何故かというと、いくつかのサンプルコード、例えば
    :class:`multiprocessing.Pool` のサンプルはインタラクティブシェル上では動作しないからです。
    以下に例を示します。 ::

        >>> from multiprocessing import Pool
        >>> p = Pool(5)
        >>> def f(x):
        ...     return x*x
        ...
        >>> p.map(f, [1,2,3])
        Process PoolWorker-1:
        Process PoolWorker-2:
        Process PoolWorker-3:
        Traceback (most recent call last):
        Traceback (most recent call last):
        Traceback (most recent call last):
        AttributeError: 'module' object has no attribute 'f'
        AttributeError: 'module' object has no attribute 'f'
        AttributeError: 'module' object has no attribute 'f'

    (このサンプルを試すと、3つのトレースバック全てがほぼランダムに交互に重なって表示されます。
    そうなったら、なんとかしてマスタープロセスを止めましょう。)

:class:`Process` クラス
~~~~~~~~~~~~~~~~~~~~~~~~~~

:mod:`multiprocessing` モジュールでは、プロセスは以下の手順によって生成されます。
はじめに :class:`Process`  のオブジェクトを作成し、
続いて :meth:`~Process.start` メソッドを呼び出します。
この :class:`Process` クラスは :class:`threading.Thread` クラスと同様の API を持っています。
まずは、簡単な例をもとにマルチプロセスを使用したプログラムについてみていきましょう。

::

    from multiprocessing import Process

    def f(name):
        print 'hello', name

    if __name__ == '__main__':
        p = Process(target=f, args=('bob',))
        p.start()
        p.join()

実行された個々のプロセス ID を表示するために拡張したサンプルコードを以下に例を示します。

::

    from multiprocessing import Process
    import os

    def info(title):
        print title
        print 'module name:', __name__
        print 'parent process:', os.getppid()
        print 'process id:', os.getpid()

    def f(name):
        info('function f')
        print 'hello', name

    if __name__ == '__main__':
        info('main line')
        p = Process(target=f, args=('bob',))
        p.start()
        p.join()

(Windows 環境で) ``if __name__ == '__main__'`` という文が必要な理由については、
:ref:`multiprocessing-programming` を参照してください。



プロセス間でのオブジェクト交換
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:mod:`multiprocessing` モジュールでは、プロセス間通信の手段が2つ用意されています。
それぞれ以下に詳細を示します。

**キュー (Queue)**

   :class:`Queue` クラスは :class:`Queue.Queue` クラスとほとんど同じように使うことができます。
   以下に例を示します。 ::

      from multiprocessing import Process, Queue

      def f(q):
          q.put([42, None, 'hello'])

      if __name__ == '__main__':
          q = Queue()
          p = Process(target=f, args=(q,))
          p.start()
          print q.get()    # "[42, None, 'hello']" を表示
          p.join()

   キューはスレッドセーフであり、プロセスセーフです。

**パイプ (Pipe)**

   :func:`Pipe` 関数は接続用オブジェクトのペアを返します。デフォルトでは、
   このオブジェクトを介して、親子間でパイプを使った双方向通信をおこなうことができます。
   以下に例を示します。 ::

      from multiprocessing import Process, Pipe

      def f(conn):
          conn.send([42, None, 'hello'])
          conn.close()

      if __name__ == '__main__':
          parent_conn, child_conn = Pipe()
          p = Process(target=f, args=(child_conn,))
          p.start()
          print parent_conn.recv()   # "[42, None, 'hello']" を表示
          p.join()

   2つのコネクション用オブジェクトが :func:`Pipe` 関数から返され、
   親側の入出力、子側の入出力といったように、それぞれパイプの両端となります。
   (他プロセスと通信する方法として) 各接続用オブジェクトには、
   :meth:`~Connection.send` メソッドと :meth:`~Connection.recv` メソッドがあります。
   データを破壊してしまうような使い方に注意する必要があります。
   それは、2つのプロセス (もしくはスレッド) が同時に *同じ* パイプに対して、
   読み書きをおこなった場合に起こります。
   もちろん、異なったパイプを使用していれば、同時に読み書きをおこなっても
   データが破壊されてしまう危険性はありません。


プロセス間の同期
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:mod:`multiprocessing` は :mod:`threading` モジュールと同じプロセス間同期の仕組みを
備えています。以下の例では、ロックを使用して、一度に1つのプロセスしか
標準出力に書き込まないようにしています。 ::

   from multiprocessing import Process, Lock

   def f(l, i):
       l.acquire()
       print 'hello world', i
       l.release()

   if __name__ == '__main__':
       lock = Lock()

       for num in range(10):
           Process(target=f, args=(lock, num)).start()

ロックを使用しないで標準出力に書き込んだ場合は、
各プロセスからの出力がごちゃまぜになってしまいます。


プロセス間での状態の共有
~~~~~~~~~~~~~~~~~~~~~~~~

これまでの話の流れで触れたとおり、並列プログラミングをする時には、
出来る限り状態を共有しないというのが定石です。
複数のプロセッサを使用するときは特にそうでしょう。

しかし、どうしてもプロセス間のデータ共有が必要な場合のために
:mod:`multiprocessing` モジュールにはその方法が用意されています。

**共有メモリ (Shared memory)**

   データを共有メモリ上に保持するために :class:`Value` クラス、
   もしくは :class:`Array` クラスを使用することができます。
   以下のサンプルコードを使って、この機能についてみていきましょう。 ::

      from multiprocessing import Process, Value, Array

      def f(n, a):
          n.value = 3.1415927
          for i in range(len(a)):
              a[i] = -a[i]

      if __name__ == '__main__':
          num = Value('d', 0.0)
          arr = Array('i', range(10))

          p = Process(target=f, args=(num, arr))
          p.start()
          p.join()

          print num.value
          print arr[:]

   このサンプルコードを実行すると以下のように表示されます。 ::

      3.1415927
      [0, -1, -2, -3, -4, -5, -6, -7, -8, -9]

   ``num`` と ``arr`` を生成するときに使用されている ``'d'`` と ``'i'`` の引数は
   :mod:`array` モジュールにより使用される種別の型コードです。
   ここで使用されている ``'d'`` は倍精度浮動小数、 ``'i'`` は符号付整数を表します。
   これらの共有オブジェクトは、プロセスセーフでありスレッドセーフです。

   共有メモリを使用して、さらに柔軟なプログラミングを行うには
   :mod:`multiprocessing.sharedctypes` モジュールを使用します。
   このモジュールは共有メモリから割り当てられた任意の ctypes オブジェクトの
   生成をサポートします。

**サーバプロセス (Server process)**

   :func:`Manager` 関数により生成されたマネージャオブジェクトはサーバプロセスを管理します。
   マネージャオブジェクトは Python のオブジェクトを保持して、他のプロセスが
   プロキシ経由でその Python オブジェクトを操作することができます。

   :func:`Manager` 関数が返すマネージャは :class:`list` 、 :class:`dict` 、
   :class:`Namespace` 、 :class:`Lock` 、 :class:`RLock` 、 :class:`Semaphore` 、
   :class:`BoundedSemaphore` 、 :class:`Condition` 、 :class:`Event` 、
   :class:`Queue` 、 :class:`Value` 、 :class:`Array` をサポートします。
   以下にサンプルコードを示します。 ::

      from multiprocessing import Process, Manager

      def f(d, l):
          d[1] = '1'
          d['2'] = 2
          d[0.25] = None
          l.reverse()

      if __name__ == '__main__':
          manager = Manager()

          d = manager.dict()
          l = manager.list(range(10))

          p = Process(target=f, args=(d, l))
          p.start()
          p.join()

          print d
          print l

   このサンプルコードを実行すると以下のように表示されます。 ::

       {0.25: None, 1: '1', '2': 2}
       [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]

   サーバプロセスのマネージャオブジェクトは共有メモリのオブジェクトよりも
   柔軟であるといえます。それは、どのような型のオブジェクトでも使えるからです。
   また、1つのマネージャオブジェクトはネットワーク経由で他のコンピュータ上の
   プロセスによって共有することもできます。
   しかし、共有メモリより動作が遅いという欠点があります。


ワーカープロセスのプールを使用
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:class:`~multiprocessing.pool.Pool` クラスは、ワーカープロセスをプールする機能を備えています。
このクラスには、いくつかの方法で開放されたワーカープロセスへタスクを割り当てるメソッドがあります。

以下に例を示します。 ::

   from multiprocessing import Pool

   def f(x):
       return x*x

   if __name__ == '__main__':
       pool = Pool(processes=4)              # 4つのワーカープロセスで開始
       result = pool.apply_async(f, [10])    # 非同期で "f(10)" を評価
       print result.get(timeout=1)           # あなたのコンピュータが *かなり* 遅くない限りは "100" を表示
       print pool.map(f, range(10))          # "[0, 1, 4,..., 81]" を表示


リファレンス
------------

:mod:`multiprocessing` パッケージは :mod:`threading` モジュールの API とほとんど同じです。

:class:`Process` クラスと例外
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. class:: Process([group[, target[, name[, args[, kwargs]]]]])

   Process オブジェクトは各プロセスの振る舞いを表します。
   :class:`Process` クラスは :class:`threading.Thread` クラスの全てのメソッドと同じインタフェースを提供します。

   コンストラクタは必ずキーワード引数で呼び出すべきです。
   引数 *group* には必ず ``None`` を渡してください。
   この引数は :class:`threading.Thread` クラスとの互換性のためだけに残されています。
   引数 *target* には、呼び出し可能オブジェクト (Collable Object) を渡します。
   このオブジェクトは :meth:`run()` メソッドから呼び出されます。
   この引数はデフォルトで ``None`` となっており、何も呼び出されません。
   引数 *name* にはプロセス名を渡します。
   デフォルトでは、自動でユニークな名前が割り当てられます。
   命名規則は、 'Process-N\ :sub:`1`:N\ :sub:`2`:...:N\ :sub:`k`' となります。
   ここで N\ :sub:`1`,N\ :sub:`2`,...,N\ :sub:`k` は整数の数列で、
   *作成した* プロセス数に対応します。
   引数 *args* は target で指定された呼び出し可能オブジェクトへの引数を渡します。
   同じく、引数 *kwargs* はキーワード引数を渡します。
   デフォルトでは、 *target* には引数が渡されないようになっています。

   サブクラスがコンストラクタをオーバーライドする場合は、
   そのプロセスに対する処理を行う前に基底クラスのコンストラクタ
   (:meth:`Process.__init__`) を実行しなければなりません。

   .. method:: run()

      プロセスが実行する処理を表すメソッドです。

      このメソッドはサブクラスでオーバーライドすることができます。
      標準の :meth:`run` メソッドは呼び出し可能オブジェクトを呼び出します。
      この呼び出されるオブジェクトはコンストラクタの target 引数として渡されます。
      もしコンストラクタに *args* もしくは *kwargs* 引数が渡されていれば、
      呼び出すオブジェクトにこれらの引数を渡します。

   .. method:: start()

      プロセスの処理を開始するためのメソッドです。

      このメソッドをプロセスごとに呼び出す必要があります。
      各プロセスの :meth:`run` メソッドを呼び出す準備が完了します。

   .. method:: join([timeout])

      :meth:`join` されたプロセスが terminate を呼び出すまで、もしくは
      オプションで指定したタイムアウトが発生するまで呼び出し側のスレッドを
      ブロックします。

      *timeout* が ``None`` ならタイムアウトは設定されません。

      1つのプロセスは何回も join することができます。

      プロセスは自分自身を join することはできません。それはデッドロックを引き起こすからです。
      プロセスが start される前に join しようとするとエラーが発生します。

   .. attribute:: name

      プロセス名です。

      この名前は文字列で、プロセスの識別にのみ使用されます。特別な命名規則はありません。
      複数のプロセスが同じ名前を持つ場合もあります。
      また、この名前はコンストラクタにより初期化されます。

   .. method:: is_alive

      プロセスが実行中かを判別します。

      おおまかに言って、プロセスオブジェクトは :meth:`start` メソッドを呼び出してから子プロセス終了までの期間が実行中となります。

   .. attribute:: daemon

      デーモンプロセスであるかどうかのフラグであり、ブール値を設定します。
      この属性は :meth:`start` が呼び出される前に設定する必要があります。

      初期値は作成するプロセスから継承します。

      プロセスが終了するとき、その子プロセスのデーモンプロセス全てを終了させようとします。

      デーモンプロセスは子プロセスを作成できないことに注意してください。
      もしそうでなければ、そのデーモンの親プロセスが終了したときに子プロセスが孤児になってしまう場合があるからです。
      加えて Unix デーモンまたはサービスで **ない** 場合には、
      非デーモンプロセスが終了したとき、普通のプロセスとして (join されずに)
      終了します。

   :class:`Threading.Thread` クラスのAPIに加えて :class:`Process` クラスのオブジェクトには
   以下の属性およびメソッドがあります。

   .. attribute:: pid

      プロセスIDを返します。プロセスの生成前は ``None`` が設定されています。

   .. attribute:: exitcode

      子プロセスの終了コードです。
      子プロセスがまだ終了していない場合は ``None`` が返されます。
      負の値 *-N* は子プロセスがシグナル *N* で終了したことを表します。

   .. attribute:: authkey

      プロセスの認証キーです (バイト文字列です)。

      :mod:`multiprocessing` モジュールがメインプロセスにより初期化される場合には、
      :func:`os.random` 関数を使用してランダムな値が設定されます。

      :class:`Process` クラスのオブジェクトの作成時にその親プロセスから認証キーを継承します。
      もしくは :attr:`authkey` に別のバイト文字列を設定することもできます。

      詳細は :ref:`multiprocessing-auth-keys` を参照してください。

   .. method:: terminate()

      プロセスを終了します。Unix 環境では ``SIGTERM`` シグナルを、
      Windows 環境では :c:func:`TerminateProcess` を使用して終了させます。
      終了ハンドラや finally 節などは、実行されないことに注意してください。

      このメソッドにより終了するプロセスの子孫プロセスは、終了 *しません* 。
      そういった子孫プロセスは単純に孤児になります。

      .. warning::

         このメソッドの使用時に、関連付けられたプロセスがパイプやキューを使用している場合には、
         使用中のパイプやキューが破損して他のプロセスから使用できなくなる可能性があります。
         同様に、プロセスがロックやセマフォなどを取得している場合には、
         このプロセスが終了してしまうと
         他のプロセスのデッドロックの原因になるでしょう。

   プロセスオブジェクトが作成したプロセスのみが :meth:`start`, :meth:`join`,
   :meth:`is_alive` と :attr:`exit_code` のメソッドを呼び出すべきです。

   以下の例では :class:`Process` のメソッドの使い方を示しています。

   .. doctest::

       >>> import multiprocessing, time, signal
       >>> p = multiprocessing.Process(target=time.sleep, args=(1000,))
       >>> print p, p.is_alive()
       <Process(Process-1, initial)> False
       >>> p.start()
       >>> print p, p.is_alive()
       <Process(Process-1, started)> True
       >>> p.terminate()
       >>> time.sleep(0.1)
       >>> print p, p.is_alive()
       <Process(Process-1, stopped[SIGTERM])> False
       >>> p.exitcode == -signal.SIGTERM
       True


.. exception:: BufferTooShort

   この例外は :meth:`Connection.recv_bytes_into()` によって発生し、
   バッファオブジェクトが小さすぎてメッセージが読み込めないことを示します。

   ``e`` が :exc:`BufferTooShort` のインスタンスとすると、
   ``e.args[0]`` はバイト文字列でそのメッセージを取得できます。


パイプ (Pipe) とキュー (Queue)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

マルチプロセス環境では、
一般的にメッセージパッシングをプロセス間通信のために使用し、
ロックのような同期プリミティブの使用しないようにします。

メッセージのやりとりのために :func:`Pipe` (2つのプロセス間の通信用)、
もしくはキュー (複数プロセスがメッセージを生成、消費する通信用) を使用することができます。

:class:`Queue` と :class:`JoinableQueue` は複数プロセスから生成/消費を行う FIFO キューです。
これらのキューは標準ライブラリの :class:`Queue.Queue` を模倣しています。
:class:`Queue` には Python 2.5 の :class:`Queue.Queue` クラスで導入された
:meth:`~Queue.Queue.task_done` と :meth:`~Queue.Queue.join` メソッドがないことが違う点です。

もし :class:`JoinableQueue` を使用するなら、キューから削除される各タスクのために
:meth:`JoinableQueue.task_done` を呼び出さなければ **なりません** 。
もしくは、ある例外を発生させてオーバーフローする可能性がある未終了タスクを
数えるためにセマフォが使用されます。

管理オブジェクトを使用することで共有キューを作成できることも覚えておいてください。
詳細は :ref:`multiprocessing-managers` を参照してください。

.. note::

   :mod:`multiprocessing` は通常の :exc:`Queue.Empty` と、
   タイムアウトのシグナルを送るために :exc:`Queue.Full` 例外を使用します。
   それらは :mod:`Queue` からインポートする必要があるので :mod:`multiprocessing` の名前空間では利用できません。

.. warning::

   :class:`Queue` を利用しようとしている最中にプロセスを
   :meth:`Process.terminate` や :func:`os.kill` で終了させる場合、
   キューにあるデータは破損し易くなります。
   終了した後で他のプロセスがキューを利用しようとすると、例外を発生させる可能性があります。

.. warning::

   上述したように、もし子プロセスがキューへ要素を追加するなら
   (そして :meth:`JoinableQueue.cancel_join_thread` を使用しない)
   そのプロセスはバッファされた全ての要素がパイプへフラッシュされるまで終了しません。

   これは、そのプロセスを join しようとする場合、
   キューに追加された全ての要素が消費されるのを確認しない限り、
   デッドロックを発生させる可能性があることを意味します。
   似たような現象で、子プロセスが非デーモンプロセスの場合、
   親プロセスは終了時に非デーモンの全ての子プロセスを join しようとして
   ハングアップする可能性があります。

   Manager を使用して作成されたキューではこの問題はありません。
   詳細は :ref:`multiprocessing-programming` を参照してください。

プロセス間通信におけるキューの使用方法は :ref:`multiprocessing-examples` を参照してください。

.. function:: Pipe([duplex])

   パイプの終端を表す :class:`Connection` オブジェクトのタプル ``(conn1, conn2)`` を返します。

   *duplex* が ``True`` (デフォルト) なら、双方向性パイプです。
   *duplex* が ``False`` なら、パイプは一方向性です。
   ``conn1`` はメッセージの受信専用に ``conn2`` はメッセージの送信専用として使用されます。

.. class:: Queue([maxsize])

   パイプや2～3個のロック/セマフォを使用して実装されたプロセス共有キューを返します。
   あるプロセスが最初に要素をキューへ追加するとき、
   バッファからパイプの中へオブジェクトを転送する供給スレッドが開始されます。

   標準ライブラリの :mod:`Queue` モジュールからの通常の :exc:`Queue.Empty` や
   :exc:`Queue.Full` 例外はタイムアウトのシグナルを送るために発生します。

   :class:`Queue` は :meth:`~Queue.Queue.task_done` や :meth:`~Queue.Queue.join` を除く
   :class:`Queue.Queue` の全てのメソッドを実装します。

   .. method:: qsize()

      おおよそのキューのサイズを返します。
      マルチスレッディング/マルチプロセスの特性上、この数値は信用できません。

      これは ``sem_getvalue()`` が実装されていない Mac OS X のような Unix プラットホーム上で
      :exc:`NotImplementedError` を発生させる可能性があることを覚えておいてください。

   .. method:: empty()

      キューが空っぽなら ``True`` を、そうでなければ ``False`` を返します。
      マルチスレッディング/マルチプロセスの特性上、これは信用できません。

   .. method:: full()

      キューがいっぱいなら ``True`` を、そうでなければ ``False`` を返します。
      マルチスレッディング/マルチプロセスの特性上、これは信用できません。

   .. method:: put(item[, block[, timeout]])

      キューの中へ要素を追加します。オプションの引数 *block* が ``True`` (デフォルト) 且つ
      *timeout* が ``None`` (デフォルト) なら、空きスロットが利用可能になるまで必要であればブロックします。
      *timeout* が正の数なら、最大 *timeout* 秒ブロックして、
      その時間内に空きスロットが利用できなかったら :exc:`Queue.Full` 例外を発生させます。
      それ以外 ( *block* が ``False`` ) で、
      空きスロットがすぐに利用可能な場合はキューに要素を追加します。
      そうでなければ :exc:`Queue.Full` 例外が発生します(その場合 *timeout* は無視されます)。

   .. method:: put_nowait(item)

      ``put(item, False)`` と等価です。

   .. method:: get([block[, timeout]])

      キューから要素を取り出して削除します。オプションの引数 *block* が ``True`` (デフォルト) 且つ
      *timeout* が ``None`` (デフォルト) なら、要素が取り出せるまで必要であればブロックします。
      *timeout* が正の数なら、最大 *timeout* 秒ブロックして、
      その時間内に要素が取り出せなかったら :exc:`Queue.Empty` 例外を発生させます。
      それ以外 ( *block* が ``False`` ) で、
      要素がすぐに取り出せる場合は要素を返します。
      そうでなければ :exc:`Queue.Empty` 例外が発生します(その場合 *timeout* は無視されます)。

   .. method:: get_nowait()
               get_no_wait()

      ``get(False)`` と等価です。

   :class:`multiprocessing.Queue` は :class:`Queue.Queue` にはない追加メソッドがあります。
   これらのメソッドは通常、ほとんどのコードに必要ありません。

   .. method:: close()

      カレントプロセスからこのキューへそれ以上データが追加されないことを表します。
      バックグラウンドスレッドはパイプへバッファされた全てのデータをフラッシュするとすぐに終了します。
      これはキューがガベージコレクトされるときに自動的に呼び出されます。

   .. method:: join_thread()

      バックグラウンドスレッドを join します。
      このメソッドは :meth:`close` が呼び出された後でのみ使用されます。
      バッファされた全てのデータがパイプへフラッシュされるのを保証した上で、
      バックグラウンドスレッドが終了するまでブロックします。

      デフォルトでは、あるプロセスがキューを作成していない場合、
      終了時にキューのバックグラウンドスレッドを join しようとします。
      そのプロセスは :meth:`join_thread` が何もしないように :meth:`cancel_join_thread` を呼び出すことができます。

   .. method:: cancel_join_thread()

      :meth:`join_thread` がブロッキングするのを防ぎます。
      特にこれはバックグラウンドスレッドがそのプロセスの終了時に
      自動的に join されるのを防ぎます。
      詳細は :meth:`join_thread` を参照してください。

.. class:: JoinableQueue([maxsize])

   :class:`JoinableQueue` は :class:`Queue` のサブクラスであり、
   :meth:`task_done` や :meth:`join` メソッドが追加されているキューです。

   .. method:: task_done()

      以前にキューへ追加されたタスクが完了したことを表します。キュー消費スレッドによって使用されます。
      タスクをフェッチするために使用されるそれぞれの :meth:`~Queue.get` では、
      次に :meth:`task_done` を呼び出してタスクの処理が完了したことをキューへ伝えます。

      もし :meth:`~Queue.join` がブロッキング状態なら、
      全ての要素が処理されたときに復帰します( :meth:`task_done` 呼び出しが
      全ての要素からキュー内へ :meth:`~Queue.put` されたと受け取ったことを意味します)。

      キューにある要素より多く呼び出された場合 :exc:`ValueError` が発生します。

   .. method:: join()

      キューにある全ての要素が取り出されて処理されるまでブロッキングします。

      キューに要素が追加されると未終了タスク数が増えます。
      キューの要素が取り出されて全て処理が完了したことを表す
      :meth:`task_done` を消費スレッドが呼び出すと数が減ります。
      未終了タスク数がゼロになると :meth:`~Queue.join` はブロッキングを解除します。

その他の関数(Miscellaneous)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. function:: active_children()

   カレントプロセスのアクティブな子プロセスの全てのリストを返します。

   これを呼び出すと "join" して既に終了しているプロセスには副作用があります。

.. function:: cpu_count()

   システムの CPU 数を返します。
   もしかしたら :exc:`NotImplementedError` が発生するかもしれません。

.. function:: current_process()

   カレントプロセスに対応する :class:`Process` オブジェクトを返します。

   :func:`threading.current_thread` とよく似た関数です。

.. function:: freeze_support()

   :mod:`multiprocessing` を使用するプログラムが固まったときに
   ウィンドウを実行状態にすることをサポートします。
   ( **py2exe** , **PyInstaller** や **cx_Freeze** でテストされています。)

   メインモジュールの ``if __name__ == '__main__'`` の後で
   この関数を連続的に呼び出す必要があります。
   以下に例を示します。 ::

      from multiprocessing import Process, freeze_support

      def f():
          print 'hello world!'

      if __name__ == '__main__':
          freeze_support()
          Process(target=f).start()

   もし ``freeze_support()`` の行がない場合、固まった実行状態で
   実行しようとして :exc:`RuntimeError` を発生させます。

   そのモジュールが Python インタプリタによって普通に実行されるなら
   :func:`freeze_support` は何の影響もありません。

.. function:: set_executable()

   子プロセスを開始するときに使用する Python インタプリタのパスを設定します。
   (デフォルトでは :data:`sys.executable` が使用されます)。
   コードに組み込むときは、おそらく次のようにする必要があります。
   ::

      setExecutable(os.path.join(sys.exec_prefix, 'pythonw.exe'))

    子プロセスを生成する前に行います。 (Windows 専用)

.. note::

   :mod:`multiprocessing` には
   :func:`threading.active_count`, :func:`threading.enumerate`,
   :func:`threading.settrace`, :func:`threading.setprofile`,
   :class:`threading.Timer` や :class:`threading.local` のような関数はありません。

Connection オブジェクト
~~~~~~~~~~~~~~~~~~~~~~~

Connection オブジェクトは pickle でシリアライズ可能なオブジェクトか文字列を送ったり、受け取ったりします。
そういったオブジェクトはメッセージ指向の接続ソケットと考えられます。

Connection オブジェクトは通常は :func:`Pipe` を使用して作成されます。
詳細は :ref:`multiprocessing-listeners-clients` も参照してください。

.. class:: Connection

   .. method:: send(obj)

      コネクションの向こう側へ :meth:`recv` を使用して読み込むオブジェクトを送ります。

      オブジェクトは pickle でシリアライズ可能でなければなりません。
      pickle が極端に大きすぎる (OS にも依りますが、およそ 32 MB+) と、
      ValueError 例外が送出されることがあります。

   .. method:: recv()

      コネクションの向こう側から :meth:`send` を使用して送られたオブジェクトを返します。
      何も受け取らずにコネクションの向こう側でクローズされた場合 :exc:`EOFError` が発生します。

   .. method:: fileno()

      コネクションが使用するハンドラか、ファイルディスクリプタを返します。

   .. method:: close()

      コネクションをクローズします。

      コネクションがガベージコレクトされるときに自動的に呼び出されます。

   .. method:: poll([timeout])

      読み込み可能なデータがあるかどうかを返します。

      *timeout* が指定されていなければすぐに返します。
      *timeout* に数値を指定すると、最大指定した秒数をブロッキングします。
      *timeout* に ``None`` を指定するとタイムアウトせずにずっとブロッキングします。

   .. method:: send_bytes(buffer[, offset[, size]])

      バッファインタフェースをサポートするオブジェクトから完全なメッセージとしてバイトデータを送ります。

      *offset* が指定されると *buffer* のその位置からデータが読み込まれます。
      *size* が指定されると大量データがバッファから読み込まれます。

   .. method:: recv_bytes([maxlength])

      文字列のようにコネクションの向こう側から送られたバイトデータの完全なメッセージを返します。
      何も受け取らずにコネクションの向こう側でクローズされた場合 :exc:`EOFError` が発生します。

      *maxlength* を指定して、且つ *maxlength* よりメッセージが長い場合、
      :exc:`IOError` を発生させて、それ以上はコネクションから読み込めなくなります。

   .. method:: recv_bytes_into(buffer[, offset])

      コネクションの向こう側から送られたバイトデータを *buffer* に読み込み、メッセージのバイト数を返します。
      何も受け取らずにコネクションの向こう側でクローズされた場合 :exc:`EOFError` が発生します。

      *buffer* は書き込み可能なバッファインタフェースを備えたオブジェクトでなければなりません。
      *offset* が与えられたら、その位置からバッファへメッセージが書き込まれます。
      オフセットは *buffer* バイトよりも小さい正の数でなければなりません。

      バッファがあまりに小さいと :exc:`BufferTooShort` 例外が発生します。
      ``e`` が例外インスタンスとすると完全なメッセージは ``e.args[0]`` で確認できます。

例:

.. doctest::

    >>> from multiprocessing import Pipe
    >>> a, b = Pipe()
    >>> a.send([1, 'hello', None])
    >>> b.recv()
    [1, 'hello', None]
    >>> b.send_bytes('thank you')
    >>> a.recv_bytes()
    'thank you'
    >>> import array
    >>> arr1 = array.array('i', range(5))
    >>> arr2 = array.array('i', [0] * 10)
    >>> a.send_bytes(arr1)
    >>> count = b.recv_bytes_into(arr2)
    >>> assert count == len(arr1) * arr1.itemsize
    >>> arr2
    array('i', [0, 1, 2, 3, 4, 0, 0, 0, 0, 0])


.. warning::

    :meth:`Connection.recv` メソッドは受信したデータを自動的に unpickle 化します。
    それはメッセージを送ったプロセスが信頼できる場合を除いてセキュリティリスクになります。

    そのため :func:`Pipe` を使用してコネクションオブジェクトを生成する場合を除いて、
    何らかの認証処理を実行した後で :meth:`~Connection.recv` や
    :meth:`~Connection.send` メソッドのみを使用すべきです。
    詳細は :ref:`multiprocessing-auth-keys` を参照してください。

.. warning::

    もしプロセスがパイプの読み込み又は書き込み中に kill されると、
    メッセージの境界が正しいかどうか分からないので、
    そのパイプのデータは破壊されたようになります。


同期プリミティブ
~~~~~~~~~~~~~~~~


一般的に同期プリミティブはマルチスレッドプログラムのようにマルチプロセスプログラムでは必要ありません。
詳細は :mod:`threading` モジュールのドキュメントを参照してください。

マネージャオブジェクトを使用して同期プリミティブを作成できることも覚えておいてください。
詳細は :ref:`multiprocessing-managers` を参照してください。

.. class:: BoundedSemaphore([value])

   束縛されたセマフォオブジェクト: :class:`threading.BoundedSemaphore` のクローンです。

   (Mac OS X では ``sem_getvalue()`` が実装されていないので :class:`Semaphore` と区別がつきません。)

.. class:: Condition([lock])

   状態変数: :class:`threading.Condition` のクローンです。

   *lock* を指定するなら :mod:`multiprocessing` の :class:`Lock` か :class:`RLock` オブジェクトにすべきです。

.. class:: Event()

   :class:`threading.Event` のクローンです。
   このメソッドは、終了時の内部セマフォの状態を返すので、タイムアウトが
   与えられ、実際にオペレーションがタイムアウトしたのでなければ、
   必ず ``True`` を返します。

   .. versionchanged:: 2.7
      以前は、このメソッドは必ず ``None`` を返していました。

.. class:: Lock()

   非再帰的なロックオブジェクト: :class:`threading.Lock` のクローンです。

.. class:: RLock()

   再帰的なロックオブジェクト: :class:`threading.RLock` のクローンです。

.. class:: Semaphore([value])

   セマフォオブジェクト: :class:`threading.Semaphore` のクローンです。

.. note::

   :class:`BoundedSemaphore`, :class:`Lock`, :class:`RLock` と :class:`Semaphore`
   の :meth:`acquire` メソッドは :mod:`threading` ではサポートされていない
   タイムアウトパラメータを取ります。その引数はキーワード引数で受け取れる
   ``acquire(block=True, timeout=None)`` です。
   *block* が ``True`` 且つ *timeout* が ``None`` ではないなら、
   タイムアウトが秒単位で設定されます。
   *block* が ``False`` なら *timeout* は無視されます。

   Mac OS X では ``sem_timedwait`` がサポートされていないので、
   ``acquire()`` タイムアウトを与えて呼ぶと、
   擬似的なスリーピングループ関数を実行することになります。

.. note::

   メインスレッドが :meth:`BoundedSemaphore.acquire`, :meth:`Lock.acquire`,
   :meth:`RLock.acquire`, :meth:`Semaphore.acquire`, :meth:`Condition.acquire`
   又は :meth:`Condition.wait` を呼び出してブロッキング状態のときに Ctrl-C で
   生成される SIGINT シグナルを受け取ると、その呼び出しはすぐに中断されて
   :exc:`KeyboardInterrupt` が発生します。

   これは同等のブロッキング呼び出しが実行中のときに SIGINT が無視される
   :mod:`threading` の振る舞いとは違っています。

..
    Shared :mod:`ctypes` Objects
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

共有 :mod:`ctypes` オブジェクト
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

子プロセスにより継承される共有メモリを使用する共有オブジェクトを作成することができます。

.. function:: Value(typecode_or_type, *args[, lock])

   共有メモリから割り当てられた :mod:`ctypes` オブジェクトを返します。
   デフォルトでは、返り値は実際のオブジェクトの同期ラッパーです。

   *typecode_or_type* は返されるオブジェクトの型を決めます。
   それは ctypes の型か :mod:`array` モジュールで使用されるような1文字の型コードかのどちらか一方です。
   *\*args* は型のコンストラクタへ渡されます。

   *lock* が ``True`` (デフォルト) なら、値へ同期アクセスするために新たな
   ロックオブジェクトが作成されます。 *lock* が :class:`Lock` か :class:`RLock`
   なら値への同期アクセスに使用されます。 *lock* が ``False`` なら、返された
   オブジェクトへのアクセスはロックにより自動的に保護されません。
   そのため、必ずしも "プロセスセーフ" ではありません。

   *lock* はキーワード引数でのみ指定することに注意してください。

.. function:: Array(typecode_or_type, size_or_initializer, *, lock=True)

   共有メモリから割り当てられた ctypes 配列を返します。
   デフォルトでは、返り値は実際の配列の同期ラッパーです。

   *typecode_or_type* は返される配列の要素の型を決めます。
   それは ctypes の型か :mod:`array` モジュールで使用されるような1文字の型コードかのどちらか一方です。
   *size_or_initializer* が整数なら、配列の長さを決定し、その配列はゼロで初期化されます。
   別の使用方法として *size_or_initializer* は配列の初期化に使用されるシーケンスになり、
   そのシーケンス長が配列の長さを決定します。

   *lock* が ``True`` (デフォルト) なら、値へ同期アクセスするために新たな
   ロックオブジェクトが作成されます。 *lock* が :class:`Lock` か :class:`RLock`
   なら値への同期アクセスに使用されます。 *lock* が ``False`` なら、返された
   オブジェクトへのアクセスはロックにより自動的に保護されません。
   そのため、必ずしも "プロセスセーフ" ではありません。

   *lock* はキーワード引数でのみ指定することに注意してください。

   :data:`ctypes.c_char` の配列は文字列を格納して取り出せる
   *value* と *raw* 属性を持っていることを覚えておいてください。

..
    The :mod:`multiprocessing.sharedctypes` module
    >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

:mod:`multiprocessing.sharedctypes` モジュール
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

..
   :synopsis: Allocate ctypes objects from shared memory.

.. module:: multiprocessing.sharedctypes
   :synopsis: 共有メモリから ctypes オブジェクトを割り当てる

:mod:`multiprocessing.sharedctypes` モジュールは子プロセスに継承される共有メモリの
:mod:`ctypes` オブジェクトを割り当てる関数を提供します。

.. note::

   共有メモリのポインタを格納することは可能ではありますが、特定プロセスの
   アドレス空間の位置を参照するということを覚えておいてください。
   しかし、そのポインタは別のプロセスのコンテキストにおいて無効になる確率が高いです。
   そして、別のプロセスからそのポインタを逆参照しようとするとクラッシュを引き起こす可能性があります。

.. function:: RawArray(typecode_or_type, size_or_initializer)

   共有メモリから割り当てられた ctypes 配列を返します。

   *typecode_or_type* は返される配列の要素の型を決めます。
   それは ctypes の型か :mod:`array` モジュールで使用されるような1文字の型コードかのどちらか一方です。
   *size_or_initializer* が整数なら、配列の長さを決定し、その配列はゼロで初期化されます。
   別の使用方法として *size_or_initializer* は配列の初期化に使用されるシーケンスになり、
   そのシーケンス長が配列の長さを決定します。

   要素を取得したり設定したりすることは潜在的に非アトミックであることに注意してください。
   ロックを使用して自動的に同期されたアクセスを保証するには :func:`Array` を使用してください。

.. function:: RawValue(typecode_or_type, *args)

   共有メモリから割り当てられた ctypes オブジェクトを返します。

   *typecode_or_type* は返される型を決めます。
   それは ctypes の型か :mod:`array` モジュールで使用されるような1文字の型コードかのどちらか一方です。
   *\*args* は型のコンストラクタへ渡されます。

   値を取得したり設定したりすることは潜在的に非アトミックであることに注意してください。
   ロックを使用して自動的に同期されたアクセスを保証するには :func:`Value` を使用してください。

   :data:`ctypes.c_char` の配列は文字列を格納して取り出せる
   *value* と *raw* 属性を持っていることを覚えておいてください。
   詳細は :mod:`ctypes` を参照してください。

.. function:: Array(typecode_or_type, size_or_initializer, *args[, lock])

   *lock* の値に依存する点を除けば :func:`RawArray` と同様です。
   プロセスセーフな同期ラッパーが raw ctypes 配列の代わりに返されるでしょう。

   *lock* が ``True`` (デフォルト) なら、値へ同期アクセスするために新たな
   ロックオブジェクトが作成されます。 *lock* が :class:`Lock` か :class:`RLock`
   なら値への同期アクセスに使用されます。 *lock* が ``False`` なら、返された
   オブジェクトへのアクセスはロックにより自動的に保護されません。
   そのため、必ずしも "プロセスセーフ" ではありません。

   *lock* はキーワード引数でのみ指定することに注意してください。

.. function:: Value(typecode_or_type, *args[, lock])

   *lock* の値に依存する点を除けば :func:`RawValue` と同様です。
   プロセスセーフな同期ラッパーが ctypes オブジェクトの代わりに返されるでしょう。

   *lock* が ``True`` (デフォルト) なら、値へ同期アクセスするために新たな
   ロックオブジェクトが作成されます。 *lock* が :class:`Lock` か :class:`RLock`
   なら値への同期アクセスに使用されます。 *lock* が ``False`` なら、返された
   オブジェクトへのアクセスはロックにより自動的に保護されません。
   そのため、必ずしも "プロセスセーフ" ではありません。

   *lock* はキーワード引数でのみ指定することに注意してください。

.. function:: copy(obj)

   共有メモリから割り当てられた ctypes オブジェクト *obj* をコピーしたオブジェクトを返します。

.. function:: synchronized(obj[, lock])

   同期アクセスに *lock* を使用する ctypes オブジェクトのためにプロセスセーフな
   ラッパーオブジェクトを返します。 *lock* が ``None`` (デフォルト) なら、
   :class:`multiprocessing.RLock` オブジェクトが自動的に作成されます。

   同期ラッパーがラップするオブジェクトに加えて2つのメソッドがあります。
   :meth:`get_obj` はラップされたオブジェクトを返します。
   :meth:`get_lock` は同期のために使用されるロックオブジェクトを返します。

   ラッパー経由で ctypes オブジェクトにアクセスすることは
   raw ctypes オブジェクトへアクセスするよりずっと遅くなることに注意してください。

次の表は通常の ctypes 構文で共有メモリから共有 ctypes オブジェクトを作成するための
構文を比較します。
( ``MyStruct`` テーブル内には :class:`ctypes.Structure` のサブクラスがあります。)

==================== ============================ ================================
ctypes               type を使用する sharedctypes typecode を使用する sharedctypes
==================== ============================ ================================
c_double(2.4)        RawValue(c_double, 2.4)      RawValue('d', 2.4)
MyStruct(4, 6)       RawValue(MyStruct, 4, 6)
(c_short * 7)()      RawArray(c_short, 7)         RawArray('h', 7)
(c_int * 3)(9, 2, 8) RawArray(c_int, (9, 2, 8))   RawArray('i', (9, 2, 8))
==================== ============================ ================================


以下に子プロセスが多くの ctypes オブジェクトを変更する例を紹介します。

::

   from multiprocessing import Process, Lock
   from multiprocessing.sharedctypes import Value, Array
   from ctypes import Structure, c_double

   class Point(Structure):
       _fields_ = [('x', c_double), ('y', c_double)]

   def modify(n, x, s, A):
       n.value **= 2
       x.value **= 2
       s.value = s.value.upper()
       for a in A:
           a.x **= 2
           a.y **= 2

   if __name__ == '__main__':
       lock = Lock()

       n = Value('i', 7)
       x = Value(c_double, 1.0/3.0, lock=False)
       s = Array('c', 'hello world', lock=lock)
       A = Array(Point, [(1.875,-6.25), (-5.75,2.0), (2.375,9.5)], lock=lock)

       p = Process(target=modify, args=(n, x, s, A))
       p.start()
       p.join()

       print n.value
       print x.value
       print s.value
       print [(a.x, a.y) for a in A]


.. highlightlang:: none

結果は以下のように表示されます。

::

    49
    0.1111111111111111
    HELLO WORLD
    [(3.515625, 39.0625), (33.0625, 4.0), (5.640625, 90.25)]

.. highlightlang:: python


.. _multiprocessing-managers:

Managers
~~~~~~~~

Manager は別のプロセス間で共有されるデータの作成方法を提供します。
マネージャオブジェクトは *共有オブジェクト* を管理するサーバプロセスを制御します。
他のプロセスはプロキシ経由で共有オブジェクトへアクセスすることができます。

.. function:: multiprocessing.Manager()

   プロセス間で共有オブジェクトのために使用される
   :class:`~multiprocessing.managers.SyncManager` オブジェクトを返します。
   返されたマネージャオブジェクトは生成される子プロセスに対応して、
   共有オブジェクトを作成するメソッドを持ち、応答プロキシを返します。

.. module:: multiprocessing.managers
   :synopsis: Share data between process with shared objects.

マネージャプロセスは親プロセスが終了するか、ガベージコレクトされると停止します。
マネージャクラスは :mod:`multiprocessing.managers` モジュールで定義されています。

.. class:: BaseManager([address[, authkey]])

   BaseManager オブジェクトを作成します。

   作成後、マネージャオブジェクトが開始されたマネージャプロセスの参照を保証するために
   :meth:`start` か ``get_server().serve_forever()`` を呼び出します。

   *address* はマネージャプロセスが新たなコネクションを待ち受けるアドレスです。
   *address* が ``None`` の場合、任意のアドレスが設定されます。

   *authkey* はサーバプロセスへ接続しようとするコネクションの正当性を検証するために
   使用される認証キーです。
   *authkey* が ``None`` の場合 ``current_process().authkey`` が使用されます。
   *authkey* を使用する場合は文字列でなければなりません。

   .. method:: start([initializer[, initargs]])

      マネージャを開始するためにサブプロセスを開始します。
      *initializer* が ``None`` でなければ、サブプロセスは開始時に
      ``initializer(*initargs)`` を呼び出します。

   .. method:: get_server()

      マネージャの制御下にある実際のサーバに相当する :class:`Server` オブジェクトを返します。
      :class:`Server` オブジェクトは :meth:`serve_forever` メソッドをサポートします。
      
      ::

      >>> from multiprocessing.managers import BaseManager
      >>> manager = BaseManager(address=('', 50000), authkey='abc'))
      >>> server = manager.get_server()
      >>> server.serve_forever()

      :class:`Server` はさらに :attr:`address` 属性も持っています。

   .. method:: connect()

      ローカルからリモートのマネージャオブジェクトへ接続します。
      
      ::

      >>> from multiprocessing.managers import BaseManager
      >>> m = BaseManager(address=('127.0.0.1', 5000), authkey='abc))
      >>> m.connect()

   .. method:: shutdown()

      マネージャが使用するプロセスを停止します。これはサーバプロセスを
      開始するために :meth:`start` が使用された場合のみ有効です。

      これは複数回呼び出すことができます。

   .. method:: register(typeid[, callable[, proxytype[, exposed[, method_to_typeid[, create_method]]]]])

      マネージャクラスで呼び出し可能オブジェクト(callable)や型を登録するために使用されるクラスメソッドです。

      *typeid* は特に共有オブジェクトの型を識別するために使用される "型識別子" です。
      これは文字列でなければなりません。

      *callable* はこの型識別子のオブジェクトを作成するために使用される呼び出し可能オブジェクトです。
      マネージャインスタンスが :meth:`from_address` クラスメソッドを使用して作成されるか、
      *create_method* 引数が ``False`` の場合は ``None`` でも構いません。

      *proxytype* はこの *typeid* で共有オブジェクトのプロキシを作成するために使用される
      :class:`BaseProxy` のサブクラスです。 ``None`` の場合、プロキシクラスは自動的に作成されます。

      *exposed* は :meth:`BaseProxy._callMethod` を使用してアクセスされる
      この typeid のプロキシになるメソッド名のシーケンスを指定するために使用されます。
      ( *exposed* が ``None`` の場合 :attr:`proxytype._exposed_` が存在すれば、
      それが代わりに使用されます。) exposed リストが指定されない場合は、
      共有オブジェクトの全ての "パブリックメソッド" にアクセスされます。
      (ここで言う "パブリックメソッド" は :meth:`__call__` メソッドを持ち、
      ``'_'`` で始まらない名前の属性を意味します。)

      *method_to_typeid* はプロキシが返す exposed メソッドの型を指定するために
      使用されるマッピングです。それは typeid 文字列に対してメソッド名をマップします。
      ( *method_to_typeid* が ``None`` の場合 :attr:`proxytype._method_to_typeid_`
      が存在すれば、それが代わりに使用されます。) メソッド名がこのマッピングのキー
      ではないか、マッピングが ``None`` の場合、そのメソッドによって返される
      オブジェクトが値によってコピーされます。

      *create_method* は、新たな共有オブジェクトを作成するためにサーバプロセスへ
      伝えるのに使用されるメソッドを *typeid* の名前で作成し、そのための
      プロキシを返すかを決定します。デフォルトでは ``True`` です。

   :class:`BaseManager` インスタンスも読み取り専用属性を1つ持っています。

   .. attribute:: address

      マネージャが使用するアドレスです。

.. class:: SyncManager

   プロセス間の同期のために使用される :class:`BaseManager` のサブクラスです。
   :func:`multiprocessing.Manager` はこの型のオブジェクトを返します。

   また共有リストやディクショナリの作成もサポートします。

   .. method:: BoundedSemaphore([value])

      共有 :class:`threading.BoundedSemaphore` オブジェクトを作成して、そのプロキシを返します。

   .. method:: Condition([lock])

      共有 :class:`threading.Condition` オブジェクトを作成して、そのプロキシを返します。

      *lock* が提供される場合 :class:`threading.Lock` か
      :class:`threading.RLock` オブジェクトのためのプロキシになります。

   .. method:: Event()

      共有 :class:`threading.Event` オブジェクトを作成して、そのプロキシを返します。

   .. method:: Lock()

      共有 :class:`threading.Lock` オブジェクトを作成して、そのプロキシを返します。

   .. method:: Namespace()

      共有 :class:`Namespace` オブジェクトを作成して、そのプロキシを返します。

   .. method:: Queue([maxsize])

      共有 :class:`Queue.Queue` オブジェクトを作成して、そのプロキシを返します。

   .. method:: RLock()

      共有 :class:`threading.RLock` オブジェクトを作成して、そのプロキシを返します。

   .. method:: Semaphore([value])

      共有 :class:`threading.Semaphore` オブジェクトを作成して、そのプロキシを返します。

   .. method:: Array(typecode, sequence)

      配列を作成して、そのプロキシを返します。

   .. method:: Value(typecode, value)

      書き込み可能な ``value`` 属性を作成して、そのプロキシを返します。

   .. method:: dict()
               dict(mapping)
               dict(sequence)

      共有 ``dict`` オブジェクトを作成して、そのプロキシを返します。

   .. method:: list()
               list(sequence)

      共有 ``list`` オブジェクトを作成して、そのプロキシを返します。

   .. note::

      プロキシには、ミュータブルな値や、辞書とリストのプロキシの要素が、
      いつ変えられたのかを知る方法がないため、これらの値や要素の変化は
      マネージャを通して伝播しません。要素などを変化させるために、
      コンテナプロキシに変化されたオブジェクトを再代入できます。

         # create a list proxy and append a mutable object (a dictionary)
         lproxy = manager.list()
         lproxy.append({})
         # now mutate the dictionary
         d = lproxy[0]
         d['a'] = 1
         d['b'] = 2
         # at this point, the changes to d are not yet synced, but by
         # reassigning the dictionary, the proxy is notified of the change
         lproxy[0] = d


Namespace オブジェクト
>>>>>>>>>>>>>>>>>>>>>>

Namespace オブジェクトはプライベートなメソッドを持っていますが、書き込み属性を持ちます。
そのオブジェクト表現はその属性の値を表示します。

しかし、Namespace オブジェクトのためにプロキシを使用するとき
``'_'`` が先頭に付く属性はプロキシの属性になり、参照対象の属性にはなりません。

.. doctest::

   >>> manager = multiprocessing.Manager()
   >>> Global = manager.Namespace()
   >>> Global.x = 10
   >>> Global.y = 'hello'
   >>> Global._z = 12.3    # これはプロキシの属性です
   >>> print Global
   Namespace(x=10, y='hello')


カスタマイズされたマネージャ
>>>>>>>>>>>>>>>>>>>>>>>>>>>>

独自のマネージャを作成するために :class:`BaseManager` のサブクラスを作成して、
マネージャクラスで呼び出し可能なオブジェクトか新たな型を登録するために
:meth:`~BaseManager.register` クラスメソッドを使用します。

::

   from multiprocessing.managers import BaseManager

   class MathsClass(object):
       def add(self, x, y):
           return x + y
       def mul(self, x, y):
           return x * y

   class MyManager(BaseManager):
       pass

   MyManager.register('Maths', MathsClass)

   if __name__ == '__main__':
       manager = MyManager()
       manager.start()
       maths = manager.Maths()
       print maths.add(4, 3)         # 7 を表示
       print maths.mul(7, 8)         # 56 を表示


リモートマネージャを使用する
>>>>>>>>>>>>>>>>>>>>>>>>>>>>

あるマシン上でマネージャサーバを実行して、
他のマシンからそのサーバを使用するクライアントを
持つことができます(ファイアウォールを通過できることが前提)。

次のコマンドを実行することでリモートクライアントからアクセスを
受け付ける1つの共有キューのためにサーバを作成します。

::

   >>> from multiprocessing.managers import BaseManager
   >>> import Queue
   >>> queue = Queue.Queue()
   >>> class QueueManager(BaseManager): pass
   >>> QueueManager.register('get_queue', callable=lambda:queue)
   >>> m = QueueManager(address=('', 50000), authkey='abracadabra')
   >>> s = m.get_server()
   >>> s.serve_forever()

あるクライアントからサーバへのアクセスは次のようになります。

::

   >>> from multiprocessing.managers import BaseManager
   >>> class QueueManager(BaseManager): pass
   >>> QueueManager.register('get_queue')
   >>> m = QueueManager(address=('foo.bar.org', 50000), authkey='abracadabra')
   >>> m.connect()
   >>> queue = m.get_queue()
   >>> queue.put('hello')

別のクライアントもそれを使用することができます。

::

   >>> from multiprocessing.managers import BaseManager
   >>> class QueueManager(BaseManager): pass
   >>> QueueManager.register('get_queue')
   >>> m = QueueManager(address=('foo.bar.org', 50000), authkey='abracadabra')
   >>> m.connect()
   >>> queue = m.get_queue()
   >>> queue.get()
   'hello'

ローカルプロセスもそのキューへアクセスすることができます。
クライアント上で上述のコードを使用してアクセスします。

::

    >>> from multiprocessing import Process, Queue
    >>> from multiprocessing.managers import BaseManager
    >>> class Worker(Process):
    ...     def __init__(self, q):
    ...         self.q = q
    ...         super(Worker, self).__init__()
    ...     def run(self):
    ...         self.q.put('local hello')
    ...
    >>> queue = Queue()
    >>> w = Worker(queue)
    >>> w.start()
    >>> class QueueManager(BaseManager): pass
    ...
    >>> QueueManager.register('get_queue', callable=lambda: queue)
    >>> m = QueueManager(address=('', 50000), authkey='abracadabra')
    >>> s = m.get_server()
    >>> s.serve_forever()

Proxy オブジェクト
~~~~~~~~~~~~~~~~~~

プロキシは別のプロセスで(おそらく)有効な共有オブジェクトを *参照する* オブジェクトです。
共有オブジェクトはプロキシの *参照対象* になると言うことができます。
複数のプロキシオブジェクトが同じ参照対象を持つ可能性もあります。

プロキシオブジェクトはその参照対象が持つ対応メソッドを実行するメソッドを持ちます。
(そうは言っても、参照対象の全てのメソッドが必ずしもプロキシ経由で利用可能ではありません)
プロキシは通常その参照対象ができることと同じ方法で使用されます。

.. doctest::

   >>> from multiprocessing import Manager
   >>> manager = Manager()
   >>> l = manager.list([i*i for i in range(10)])
   >>> print l
   [0, 1, 4, 9, 16, 25, 36, 49, 64, 81]
   >>> print repr(l)
   <ListProxy object, typeid 'list' at 0x...>
   >>> l[4]
   16
   >>> l[2:5]
   [4, 9, 16]

プロキシに :func:`str` を適用すると参照対象のオブジェクト表現を返すのに対して、
:func:`repr` を適用するとプロキシのオブジェクト表現を返すことに注意してください。

プロキシオブジェクトの重要な機能はプロセス間で受け渡し可能な pickle 化ができることです。
しかし、プロキシが対応するマネージャプロセスに対して送信される場合、
そのプロキシを unpickle するとその参照対象を生成することを覚えておいてください。
例えば、これはある共有オブジェクトに別の共有オブジェクトが含められることを意味します。

.. doctest::

   >>> a = manager.list()
   >>> b = manager.list()
   >>> a.append(b)         # a の参照対象に b の参照対象を含める
   >>> print a, b
   [[]] []
   >>> b.append('hello')
   >>> print a, b
   [['hello']] ['hello']

.. note::

   :mod:`multiprocessing` のプロキシ型は値による比較に対して何もサポートしません。
   そのため、インスタンスでは、

   .. doctest::

       >>> manager.list([1,2,3]) == [1,2,3]
       False

   比較を行いたいときは参照対象のコピーを使用してください。

.. class:: BaseProxy

   プロキシオブジェクトは :class:`BaseProxy` のサブクラスのインスタンスです。

   .. method:: _callmethod(methodname[, args[, kwds]])

      プロキシの参照対象のメソッドの実行結果を返します。

      ``proxy`` がプロキシで、プロキシ内の参照対象が ``obj`` なら、

      ::

         proxy._callmethod(methodname, args, kwds)

      は、

      ::

         getattr(obj, methodname)(*args, **kwds)

      マネージャプロセス内のこの式を評価します。

      返される値はその呼び出し結果のコピーか、新たな共有オブジェクトに対するプロキシになります。
      詳細は :meth:`BaseManager.register` の *method_to_typeid* 引数のドキュメントを参照してください。

      例外がその呼び出しによって発生する場合 :meth:`_callmethod` によって再発生させます。
      もし他の例外がマネージャプロセスで発生するなら、
      :exc:`RemoteError` 例外に変換されて :meth:`_callmethod` によって発生させます。

      特に *methodname* が *公開* されていない場合は例外が発生することに注意してください。

      :meth:`_callmethod` の使用例になります。

      .. doctest::

         >>> l = manager.list(range(10))
         >>> l._callmethod('__len__')
         10
         >>> l._callmethod('__getslice__', (2, 7))   # `l[2:7]` と等価
         [2, 3, 4, 5, 6]
         >>> l._callmethod('__getitem__', (20,))     # `l[20]` と等価
         Traceback (most recent call last):
         ...
         IndexError: list index out of range

   .. method:: _getvalue()

      参照対象のコピーを返します。

      参照対象が unpickle 化できるなら例外を発生します。

   .. method:: __repr__

      プロキシオブジェクトのオブジェクト表現を返します。

   .. method:: __str__

      参照対象のオブジェクト表現を返します。

クリーンアップ
>>>>>>>>>>>>>>

プロキシオブジェクトは弱参照(weakref)コールバックを使用します。
プロキシオブジェクトがガベージコレクトされるときに
その参照対象が所有するマネージャからその登録を取り消せるようにするためです。

共有オブジェクトはプロキシが参照しなくなったときにマネージャプロセスから削除されます。

プロセスプール
~~~~~~~~~~~~~~

.. module:: multiprocessing.pool
   :synopsis: Create pools of processes.

:class:`Pool` クラスでタスクを実行するプロセスのプールを作成することができます。

.. class:: multiprocessing.Pool([processes[, initializer[, initargs[, maxtasksperchild]]]])

   プロセスプールオブジェクトはジョブが実行されるようにワーカープロセスのプールを制御します。
   タイムアウトやコールバックで非同期の実行をサポートして、並列 map 実装を持ちます。

   *processes* は使用するワーカープロセスの数です。 *processes* が ``None`` の場合
   :func:`cpu_count` が返す数を使用します。 *initializer* が ``None`` の場合、
   各ワーカープロセスが開始時に ``initializer(*initargs)`` を呼び出します。

   .. versionadded:: 2.7
      *maxtasksperchild* は、使われないリソースの開放のために
      ワーカープロセスが退出して、新しい
      ワーカープロセスで置き換えられるまでに完了できるタスクの数です。
      デフォルトの *maxtasksperchild* は None で、ワーカープロセスが
      プールと同じだけ残ることを意味します。

   .. note::

      :class:`Pool` 中のワーカープロセスは典型的に、プールのワークキューの
      存続期間とちょうど同じだけ残ります。(Apache, mod_wsgi, 等のような)
      他のシステムによく見られるパターンは、ワーカーが設定された量だけの
      ワークを完了させるまでプールに置いたら退出させ、新しいプロセスが
      産まれて古いプロセスを置き換えるものです。
      :class:`Pool` の *maxtasksperchild* 引数は、この能力をエンドユーザに開放します。

   .. method:: apply(func[, args[, kwds]])

      :func:`apply` 組み込み関数と同じです。その結果を返せるようになるまでブロックします。
      これらのブロックが与えられる場合、 :meth:`apply_async` の方が
      うまく並列実行を処理します。
      さらに関数はプールの中の一つのワーカーにのみ渡されます。

   .. method:: apply_async(func[, args[, kwds[, callback]]])

      :meth:`apply` メソッドの一種で結果オブジェクトを返します。

      *callback* が指定された場合、1つの引数を受け取って呼び出されます。
      その結果を返せるようになったときに *callback* が結果オブジェクトに対して
      (その呼び出しが失敗しない限り)適用されます。
      その結果を扱う別スレッドはブロックされるので *callback* はすぐに終了します。

   .. method:: map(func, iterable[, chunksize])

      並列な :func:`map` 組み込み関数と同じです( *iterable* な引数を1つだけサポートします)。
      その結果を返せるようになるまでブロックします。

      このメソッドは独立したタスクのようにプロセスプールに対して実行するチャンク数に分割します。
      チャンク(概算)サイズは *chunksize* に正の整数を指定することで行います。

   .. method:: map_async(func, iterable[, chunksize[, callback]])

      :meth:`map` メソッドの一種で結果オブジェクトを返します。

      *callback* が指定された場合、1つの引数を受け取って呼び出されます。
      その結果を返せるようになったときに *callback* が結果オブジェクトに対して
      (その呼び出しが失敗しない限り)適用されます。
      その結果を扱う別スレッドはブロックされるので *callback* はすぐに終了します。

   .. method:: imap(func, iterable[, chunksize])

      :func:`itertools.imap` と同じです。

      *chunksize* 引数は :meth:`map` メソッドで使用されるものと同じです。
      引数 iterable がとても大きいなら *chunksize* に大きな値を指定して使用する方が
      デフォルト値の ``1`` を使用するよりもジョブの完了が **かなり** 速くなります。

      また *chunksize* が ``1`` の場合 :meth:`imap` メソッドが返すイテレータの
      :meth:`next` メソッドはオプションで *timeout* パラメータを持ちます。
      ``next(timeout)`` は、その結果が *timeout* 秒以内に返されないときに
      :exc:`multiprocessing.TimeoutError` を発生させます。

   .. method:: imap_unordered(func, iterable[, chunksize])

      イテレータが返す結果の順番が任意の順番で良いと見なされることを除けば :meth:`imap` と同じです。
      (ワーカープロセスが1つしかない場合のみ "正しい" 順番になることが保証されます。)

   .. method:: close()

      これ以上プールでタスクが実行されないようにします。
      全てのタスクが完了した後でワーカープロセスが終了します。

   .. method:: terminate()

      実行中の処理を完了させずにワーカープロセスをすぐに停止します。
      プールオブジェクトがガベージコレクトされるときに :meth:`terminate` が呼び出されます。

   .. method:: join()

      ワーカープロセスが終了するのを待ちます。 :meth:`join` を使用する前に
      :meth:`close` か :meth:`terminate` を呼び出さなければなりません。

.. class:: AsyncResult

   :meth:`Pool.apply_async` や :meth:`Pool.map_async` で返される結果のクラスです。

   .. method:: get([timeout])

      結果を受け取ったときに返します。 *timeout* が ``None`` ではなくて、
      その結果が *timeout* 秒以内に受け取れない場合
      :exc:`multiprocessing.TimeoutError` が発生します。
      リモートの呼び出しが例外を発生させる場合、その例外は :meth:`get` が再発生させます。

   .. method:: wait([timeout])

      その結果が有効になるか *timeout* 秒経つまで待ちます。

   .. method:: ready()

      その呼び出しが完了しているかどうかを返します。

   .. method:: successful()

      その呼び出しが例外を発生させることなく完了したかどうかを返します。
      その結果が返せる状態でない場合 :exc:`AssertionError` が発生します。

次の例はプールの使用例を紹介します。

::

   from multiprocessing import Pool

   def f(x):
       return x*x

   if __name__ == '__main__':
       pool = Pool(processes=4)              # 4つのワーカープロセスで開始

       result = pool.apply_async(f, (10,))   # 非同期で "f(10)" を評価
       print result.get(timeout=1)           # あなたのコンピュータが *かなり* 遅くない限りは "100" を表示

       print pool.map(f, range(10))          # "[0, 1, 4,..., 81]" を表示

       it = pool.imap(f, range(10))
       print it.next()                       # "0" を表示
       print it.next()                       # "1" を表示
       print it.next(timeout=1)              # あなたのコンピュータが *かなり* 遅くない限りは "4" を表示

       import time
       result = pool.apply_async(time.sleep, (10,))
       print result.get(timeout=1)           # TimeoutError を発生


.. _multiprocessing-listeners-clients:

Listeners and Clients
~~~~~~~~~~~~~~~~~~~~~

.. module:: multiprocessing.connection
   :synopsis: API for dealing with sockets.

通常、プロセス間でメッセージを渡すにはキューを使用するか
:func:`Pipe` が返す :class:`Connection` オブジェクトを使用します。

しかし :mod:`multiprocessing.connection` モジュールはさらに柔軟な仕組みがあります。
基本的にはソケットもしくは Windows の名前付きパイプを扱う
高レベルのメッセージ指向 API を提供して :mod:`hmac` モジュールを
使用して *ダイジェスト認証* もサポートします。

.. function:: deliver_challenge(connection, authkey)

   ランダム生成したメッセージをコネクションの相手側へ送信して応答を待ちます。

   その応答がキーとして *authkey* を使用するメッセージのダイジェストと一致する場合、
   コネクションの相手側へ歓迎メッセージを送信します。
   そうでなければ :exc:`AuthenticationError` を発生させます。

.. function:: answerChallenge(connection, authkey)

   メッセージを受信して、そのキーとして *authkey* を使用するメッセージの
   ダイジェストを計算し、ダイジェストを送り返します。

   歓迎メッセージを受け取れない場合 :exc:`AuthenticationError` が発生します。

.. function:: Client(address[, family[, authenticate[, authkey]]])

   *address* で渡したアドレスを使用するリスナーに対してコネクションを
   確立しようとして :class:`~multiprocessing.Connection` を返します。

   コネクション種別は *family* 引数で決定しますが、一般的には *address* の
   フォーマットから推測できるので、これは指定されません。
   ( :ref:`multiprocessing-address-formats` を参照してください)

   *authenticate* が ``True`` か *authkey* が文字列の場合、
   ダイジェスト認証が使用されます。認証に使用されるキーは *authkey* 、
   又は *authkey* が ``None`` の場合は ``current_process().authkey`` のどちらかです。
   認証が失敗した場合 :exc:`AuthenticationError` が発生します。
   :ref:`multiprocessing-auth-keys` を参照してください。

.. class:: Listener([address[, family[, backlog[, authenticate[, authkey]]]]])

   コネクションを '待ち受ける' 束縛されたソケットか Windows の名前付きパイプのラッパです。

   *address* はリスナーオブジェクトの束縛されたソケットか名前付きパイプが使用するアドレスです。

   .. note::

      '0.0.0.0' のアドレスを使用する場合、Windows 上の終点へ接続することができません。
      終点へ接続したい場合は '127.0.0.1' を使用すべきです。

   *family* は使用するソケット(名前付きパイプ)の種別です。
   これは ``'AF_INET'`` (TCP ソケット), ``'AF_UNIX'`` (Unix ドメインソケット)
   又は ``'AF_PIPE'`` (Windows 名前付きパイプ) という文字列のどれか1つになります。
   これらのうち ``'AF_INET'`` のみが利用可能であることが保証されています。
   *family* が ``None`` の場合 *address* のフォーマットから推測されたものが使用されます。
   *address* も ``None`` の場合はデフォルトが選択されます。
   詳細は :ref:`multiprocessing-address-formats` を参照してください。
   *family* が ``'AF_UNIX'`` で *address* が ``None`` の場合 :func:`tempfile.mkstemp` を
   使用して作成されたプライベートな一時ディレクトリにソケットが作成されます。

   リスナーオブジェクトがソケットを使用する場合、ソケットに束縛されるときに
   *backlog* (デフォルトでは1つ) がソケットの :meth:`listen` メソッドに対して渡されます。

   *authenticate* が ``True``  (デフォルトでは ``False`` ) か
   *authkey* が ``None`` ではない場合、ダイジェスト認証が使用されます。

   *authkey* が文字列の場合、認証キーとして使用されます。そうでない場合は *None* でなければいけません。

   *authkey* が ``None`` 且つ *authenticate* が ``True`` の場合
   ``current_process().authkey`` が認証キーとして使用されます。
   *authkey* が ``None`` 且つ *authentication* が ``False`` の場合、認証は行われません。
   もし認証が失敗した場合 :exc:`AuthenticationError` が発生します。
   詳細 :ref:`multiprocessing-auth-keys` を参照してください。

   .. method:: accept()

      リスナーオブジェクトの名前付きパイプか束縛されたソケット上でコネクションを
      受け付けて :class:`Connection` オブジェクトを返します。
      認証が失敗した場合 :exc:`AuthenticationError` が発生します。

   .. method:: close()

      リスナーオブジェクトの名前付きパイプか束縛されたソケットをクローズします。
      これはリスナーがガベージコレクトされるときに自動的に呼ばれます。
      そうは言っても、明示的に close() を呼び出す方が望ましいです。

   リスナーオブジェクトは次の読み取り専用属性を持っています。

   .. attribute:: address

      リスナーオブジェクトが使用中のアドレスです。

   .. attribute:: last_accepted

      最後にコネクションを受け付けたアドレスです。
      有効なアドレスがない場合は ``None`` になります。

このモジュールは2つの例外を定義します。

.. exception:: AuthenticationError

   認証エラーが起こったときに例外が発生します。

**例**

次のサーバコードは認証キーとして ``'secret password'`` を使用するリスナーを作成します。
このサーバはコネクションを待ってクライアントへデータを送信します。

::

   from multiprocessing.connection import Listener
   from array import array

   address = ('localhost', 6000)     # family is deduced to be 'AF_INET'
   listener = Listener(address, authkey='secret password')

   conn = listener.accept()
   print 'connection accepted from', listener.last_accepted

   conn.send([2.25, None, 'junk', float])

   conn.send_bytes('hello')

   conn.send_bytes(array('i', [42, 1729]))

   conn.close()
   listener.close()

次のコードはサーバへ接続して、サーバからデータを受信します。

::

   from multiprocessing.connection import Client
   from array import array

   address = ('localhost', 6000)
   conn = Client(address, authkey='secret password')

   print conn.recv()                 # => [2.25, None, 'junk', float]

   print conn.recv_bytes()            # => 'hello'

   arr = array('i', [0, 0, 0, 0, 0])
   print conn.recv_bytes_into(arr)     # => 8
   print arr                         # => array('i', [42, 1729, 0, 0, 0])

   conn.close()


.. _multiprocessing-address-formats:

アドレスフォーマット
>>>>>>>>>>>>>>>>>>>>

* ``'AF_INET'`` アドレスは ``(hostname, port)`` のタプルになります。
  *hostname* は文字列で *port* は整数です。

* ``'AF_UNIX'`` アドレスはファイルシステム上のファイル名の文字列です。

* ``'AF_PIPE'`` アドレスは :samp:`r'\\\\.\\pipe\\{PipeName}'` の文字列です。
  *ServerName* というリモートコンピュータ上の名前付きパイプに接続するために
  :func:`Client` を使用するには、代わりに
  :samp:`r'\\\\{ServerName}\\pipe\\{PipeName}'` のアドレスを使用すべきです。

デフォルトでは、2つのバックスラッシュで始まる文字列は ``'AF_UNIX'`` よりも
``'AF_PIPE'`` として推測されることに注意してください。

.. _multiprocessing-auth-keys:

認証キー
~~~~~~~~

:meth:`Connection.recv` を使用するとき、データは自動的に unpickle されて受信します。
信頼できない接続元からのデータを unpickle することはセキュリティリスクがあります。
そのため :class:`Listener` や :func:`Client` はダイジェスト認証を提供するために
:mod:`hmac` モジュールを使用します。

認証キーはパスワードとして見なされる文字列です。
コネクションが確立すると、双方の終点で正しい接続先であることを証明するために
知っているお互いの認証キーを要求します。
(双方の終点が同じキーを使用して通信しようとしても、
コネクション上でそのキーを送信することは **できません** 。)

認証が要求されて認証キーが指定されている場合
``current_process().authkey`` の返す値が使用されます。
(詳細は :class:`~multiprocessing.Process` を参照してください。)
この値はカレントプロセスを作成する :class:`~multiprocessing.Process`
オブジェクトによって自動的に継承されます。
これは(デフォルトでは)複数プロセスのプログラムの全プロセスが相互にコネクションを
確立するときに使用される1つの認証キーを共有することを意味します。

適当な認証キーを :func:`os.urandom` を使用して生成することもできます。

ロギング
~~~~~~~~

ロギングのために幾つかの機能が利用可能です。しかし :mod:`logging` パッケージは、
(ハンドラ種別に依存して)違うプロセスからのメッセージがごちゃ混ぜになるので、
プロセスの共有ロックを使用しないことに注意してください。

.. currentmodule:: multiprocessing
.. function:: get_logger()

   :mod:`multiprocessing` が使用するロガーを返します。必要に応じて新たなロガーを作成します。

   最初に作成するとき、ロガーはレベルに :data:`logging.NOTSET` が設定されていて
   デフォルトハンドラがありません。
   このロガーへ送られるメッセージはデフォルトではルートロガーへ伝播されません。

   Windows 上では子プロセスが親プロセスのロガーレベルを継承しないことに注意してください。
   さらにその他のロガーのカスタマイズ内容も全て継承されません。

.. currentmodule:: multiprocessing
.. function:: log_to_stderr()

   この関数は :func:`get_logger` に対する呼び出しを実行しますが、
   get_logger によって作成されるロガーを返すことに加えて、
   ``'[%(levelname)s/%(processName)s] %(message)s'`` のフォーマットを使用して
   :data:`sys.stderr` へ出力を送るハンドラを追加します。

以下にロギングを有効にした例を紹介します。

::

    >>> import multiprocessing, logging
    >>> logger = multiprocessing.log_to_stderr()
    >>> logger.setLevel(logging.INFO)
    >>> logger.warning('doomed')
    [WARNING/MainProcess] doomed
    >>> m = multiprocessing.Manager()
    [INFO/SyncManager-...] child process calling self.run()
    [INFO/SyncManager-...] created temp directory /.../pymp-...
    [INFO/SyncManager-...] manager serving at '/.../listener-...'
    >>> del m
    [INFO/MainProcess] sending shutdown message to manager
    [INFO/SyncManager-...] manager exiting with exitcode 0

これらの2つのロギング関数があることに加えて、
multiprocessing モジュールも2つの追加ロギングレベル属性を提供します。
それは :const:`SUBWARNING` と :const:`SUBDEBUG` です。
次の表は通常のレベル階層にうまく適合していることを表します。

+----------------+----------------+
| Level          | Numeric value  |
+================+================+
| ``SUBWARNING`` | 25             |
+----------------+----------------+
| ``SUBDEBUG``   | 5              |
+----------------+----------------+

完全なロギングレベルの表については :mod:`logging` モジュールを参照してください。

こういった追加のロギングレベルは主に multiprocessing モジュールの
信頼できるデバッグメッセージのために使用されます。
以下に上述の例に :const:`SUBDEBUG` を有効にしたものを紹介します。

::

    >>> import multiprocessing, logging
    >>> logger = multiprocessing.log_to_stderr()
    >>> logger.setLevel(multiprocessing.SUBDEBUG)
    >>> logger.warning('doomed')
    [WARNING/MainProcess] doomed
    >>> m = multiprocessing.Manager()
    [INFO/SyncManager-...] child process calling self.run()
    [INFO/SyncManager-...] created temp directory /.../pymp-...
    [INFO/SyncManager-...] manager serving at '/.../pymp-.../listener-...'
    >>> del m
    [SUBDEBUG/MainProcess] finalizer calling ...
    [INFO/MainProcess] sending shutdown message to manager
    [DEBUG/SyncManager-1] manager received shutdown message
    [SUBDEBUG/SyncManager-...] calling <Finalize object, callback=unlink, ...
    [SUBDEBUG/SyncManager-...] finalizer calling <built-in function unlink> ...
    [SUBDEBUG/SyncManager-...] calling <Finalize object, dead>
    [SUBDEBUG/SyncManager-...] finalizer calling <function rmtree at 0x5aa730> ...
    [INFO/SyncManager-...] manager exiting with exitcode 0

:mod:`multiprocessing.dummy` モジュール
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. module:: multiprocessing.dummy
   :synopsis: Dumb wrapper around threading.

:mod:`multiprocessing.dummy` は :mod:`multiprocessing` の API を複製しますが
:mod:`threading` モジュールのラッパーでしかありません。

.. _multiprocessing-programming:

プログラミングガイドライン
--------------------------

:mod:`multiprocessing` を使用するときに守るべき確かなガイドラインとイディオムです。

全てのプラットホーム
~~~~~~~~~~~~~~~~~~~~

共有状態を避ける

    できるだけプロセス間で巨大なデータを移動することは避けるようにすべきです。

    :mod:`threading` モジュールのプリミティブな低レベルの同期を使用するよりも、
    キューかパイプをプロセス間通信に使用することがおそらく最善の方法です。

pickle 機能

    プロキシのメソッドへの引数は pickle 化できることを保証します。

プロキシのスレッドセーフ

    プロキシオブジェクトをロックで保護しない限り1つ以上のスレッドから使用してはいけません。

    (違うプロセスで *同じ* プロキシを使用することは問題ではありません。)

ゾンビプロセスを join する

    Unix 上ではプロセスが終了したときに join しないと、そのプロセスはゾンビになります。
    新たなプロセスが開始する(又は :func:`active_children` が呼ばれる)ときに、
    join されていない全ての完了プロセスが join されるので、
    あまり多くにはならないでしょう。また、終了したプロセスの
    :meth:`Process.is_alive` はそのプロセスを join します。
    そうは言っても、自分で開始した全てのプロセスを明示的に join することは
    おそらく良いプラクティスです。

pickle/unpickle より継承する方が良い

    Windows 上では :mod:`multiprocessing` の多くの型を子プロセスが使用するために
    pickle 化する必要があります。しかし、パイプやキューを使用する他のプロセスへ
    共有オブジェクトを送ることは一般的に避けるべきです。その代わり、どこかに
    作成された共有リソースへアクセスが必要なプロセスは他のプロセスから
    継承できるようにそのプログラムを修正すべきです。

プロセスを強制終了させることを避ける

    あるプロセスを停止するために :meth:`Process.terminate` メソッドを使用すると、
    そのプロセスが現在使用されている(ロック、セマフォ、パイプやキューのような)共有リソースを
    破壊したり他のプロセスから利用できない状態を引き起こし易いです。

    そのため、共有リソースを使用しないプロセスでのみ :meth:`Process.terminate` を
    使用するように考慮することがおそらく最善の方法です。

キューを使用するプロセスを join する

    キューに要素を追加するプロセスは、全てのバッファされた要素が "feeder" スレッドによって
    下位層のパイプに対してフィードされるまで終了を待つということを覚えておいてください。
    (子プロセスはこの動作を避けるためにキューの :meth:`Queue.cancel_join_thread`
    メソッドを呼ぶことができます。)

    これはキューを使用するときに、キューに追加された全ての要素が最終的に
    そのプロセスが join される前に削除されていることを確認する必要があることを意味します。
    そうしないと、そのキューに要素が追加したプロセスの終了を保証できません。
    デーモンではないプロセスは自動的に join されることも覚えておいてください。

    次の例はデッドロックを引き起こします。

    ::

        from multiprocessing import Process, Queue

        def f(q):
            q.put('X' * 1000000)

        if __name__ == '__main__':
            queue = Queue()
            p = Process(target=f, args=(queue,))
            p.start()
            p.join()                    # これはデッドロックします
            obj = queue.get()

    修正するには最後の2行を入れ替えます(または単純に ``p.join()`` の行を削除します)。

明示的に子プロセスへリソースを渡す

    Unix 上では子プロセスはグローバルなリソースを使用する親プロセスが作成した
    共有リソースを使用することができます。しかし、引数としてそのオブジェクトを
    子プロセスのコンストラクタへ渡す方が良いです。

    (潜在的に) Windows 互換なコードを作成することは別として、さらにこれは
    子プロセスが生き続ける限り、そのオブジェクトは親プロセスでガベージコレクト
    されないことも保証します。これは親プロセスでそのオブジェクトがガベージコレクト
    されるときにリソースが開放される場合に重要になるでしょう。

    そのため、例えば、

    ::

        from multiprocessing import Process, Lock

        def f():
            ... do something using "lock" ...

        if __name__ == '__main__':
           lock = Lock()
           for i in range(10):
                Process(target=f).start()

    次のように書き直すべきです。

    ::

        from multiprocessing import Process, Lock

        def f(l):
            ... do something using "l" ...

        if __name__ == '__main__':
           lock = Lock()
           for i in range(10):
                Process(target=f, args=(lock,)).start()

sys.stdin をファイル形式のオブジェクトへ置き換えることに注意してください

    :mod:`muliprocessing` は元々 :meth:`muliprocessing.Process.__bootstrap` の
    中で無条件に

    ::

        os.close(sys.stdin.fileno())

    を呼び出していました --- これはプロセス間で問題が起こしてしまいます。
    そこで、これは以下のように変更されました。
    
    ::

        sys.stdin.close()
        sys.stdin = open(os.devnull)

    これによってプロセス間同士が衝突して bad file descripter エラーを起こすという
    基本的な問題は解決しました、しかし、アプリケーションの出力バッファーを
    :func:`sys.stdin` からファイル形式のオブジェクトに置き換えるという潜在的危険を
    持ち込みます。
    複数のプロセスがファイル形式オブジェクトの :func:`close()` を呼び出した場合、
    オブジェクトに同じデータが何度もフラッシュされ、衝突が起きる危険があります。

    もし、ファイル形式オブジェクトを書いて、独自のキャッシュ実装する場合、
    キャッシュする時に常に pid を記録しておく、
    pid が変わったらキュッシュを捨てることでフォークセーフにできます。
    例::

        @property
        def cache(self):
            pid = os.getpid()
            if pid != self._pid:
                self._pid = pid
                self._cache = []
            return self._cache

    より詳しい情報は :issue:`5155` 、 :issue:`5351` 、 :issue:`5331` を見てください。

Windows
~~~~~~~

Windows では :func:`os.fork` がないので幾つか追加制限があります。

さらなる pickle 機能

    :meth:`Process.__init__` へ渡す全ての引数は pickle 化できることを保証します。
    これは特に束縛、又は非束縛メソッドが Windows 上の ``target`` 引数として
    直接的に使用できないことを意味します。その代わり、まさに関数を定義してください。

    また :class:`Process` をサブクラス化する場合、そのインスタンスが
    :meth:`Process.start` メソッドが呼ばれたときに pickle 化できることを保証します。

グローバル変数

    子プロセスで実行されるコードがグローバル変数にアクセスしようとする場合、
    子プロセスが見るその値は :meth:`Process.start` が呼ばれたときの親プロセスの
    その値と同じではない可能性があります。

    しかし、単にモジュールレベルの定数であるグローバル変数なら問題にはなりません。

メインモジュールの安全なインポート

    新たに Python インタプリタによって、意図しない副作用(新たなプロセスを開始する等)
    を起こさずにメインモジュールを安全にインポートできることを保証します。

    例えば Windows で次のモジュールを実行しようとすると :exc:`RuntimeError` で失敗します。

    ::

        from multiprocessing import Process

        def foo():
            print 'hello'

        p = Process(target=foo)
        p.start()

    代わりに、次のように ``if __name__ == '__main__':`` を使用して
    プログラムの "エントリポイント" を保護すべきです。

    ::

       from multiprocessing import Process, freeze_support

       def foo():
           print 'hello'

       if __name__ == '__main__':
           freeze_support()
           p = Process(target=foo)
           p.start()

    ( ``freeze_support()`` 行はプログラムが固まらずに実行されるなら通常は取り除かれます。)

    これは新たに生成された Python インタプリタがそのモジュールを安全にインポートして、
    モジュールの ``foo()`` 関数を実行します。

    プール又はマネージャがメインモジュールで作成される場合に似たような制限が適用されます。

.. _multiprocessing-examples:

例
--

カスタマイズされたマネージャやプロキシの作成方法と使用方法を紹介します。

.. literalinclude:: ../includes/mp_newtype.py


:class:`Pool` の使用例を紹介します。

.. literalinclude:: ../includes/mp_pool.py


ロック、コンディションやキューのような同期の例を紹介します。

.. literalinclude:: ../includes/mp_synchronize.py


ワーカープロセスのコレクションに対するタスクをフィードするキューの使用方法と
その結果をまとめる方法の例をを紹介します。

.. literalinclude:: ../includes/mp_workers.py


ワーカープロセスのプールが1つのソケットを共有して
それぞれの :class:`SimpleHTTPServer.HttpServer` インスタンスを
実行する方法の例を紹介します。

.. literalinclude:: ../includes/mp_webserver.py


:mod:`multiprocessing` と :mod:`threading` を比較した簡単なベンチマークです。

.. literalinclude:: ../includes/mp_benchmarks.py

