
:mod:`select` --- I/O 処理の完了を待機する
==========================================

.. module:: select
   :synopsis: 複数のストリームに対して I/O 処理の完了を待機します。


.. This module provides access to the :cfunc:`select` and :cfunc:`poll` functions
.. available in most operating systems, :cfunc:`epoll` available on Linux 2.5+ and
.. :cfunc:`kqueue` available on most BSD.
.. Note that on Windows, it only works for sockets; on other operating systems,
.. it also works for other file types (in particular, on Unix, it works on pipes).
.. It cannot be used on regular files to determine whether a file has grown since
.. it was last read.

このモジュールでは、ほとんどのオペレーティングシステムで利用可能な :c:func:`select` および :c:func:`poll` 関数、
Linux 2.5+ で利用可能な :c:func:`epoll` 、多くのBSDで利用可能な :c:func:`kqueue` 関数に対するアクセスを提供しています。
Windows 上ではソケットに対してしか動作しないので注意してください; その他のオペレーティングシステムでは、他のファイル形式でも
(特に Unixではパイプにも) 動作します。通常のファイルに対して適用し、最後にファイルを読み出した時から内容が増えているかを
決定するために使うことはできません。


.. The module defines the following:

このモジュールでは以下の内容を定義しています:


.. exception:: error

   .. The exception raised when an error occurs.  The accompanying value is a pair
   .. containing the numeric error code from :cdata:`errno` and the corresponding
   .. string, as would be printed by the C function :cfunc:`perror`.

   エラーが発生したときに送出される例外です。エラーに付属する値は、 :c:data:`errno` からとったエラーコードを表す数値とその
   エラーコードに対応する文字列からなるペアで、C 関数の :c:func:`perror` が出力するものと同様です。


.. function:: epoll([sizehint=-1])

   .. (Only supported on Linux 2.5.44 and newer.)  Returns an edge polling object,
   .. which can be used as Edge or Level Triggered interface for I/O events; see
   .. section :ref:`epoll-objects` below for the methods supported by epolling
   .. objects.

   (Linux 2.5.44 以降でのみサポート) エッジポーリング(edge polling)オブジェクトを返します。
   このオブジェクトは、 I/O イベントのエッジトリガもしくはレベルトリガインタフェースとして使うことができます。
   エッジポーリングオブジェクトが提供しているメソッドについては、 :ref:`epoll-objects` 節を参照してください。


   .. versionadded:: 2.6


.. function:: poll()

   .. (Not supported by all operating systems.)  Returns a polling object, which
   .. supports registering and unregistering file descriptors, and then polling them
   .. for I/O events; see section :ref:`poll-objects` below for the methods supported
   .. by polling objects.

   (全てのオペレーティングシステムでサポートされているわけではありません) ポーリングオブジェクトを返します。
   このオブジェクトはファイル記述子を登録したり登録解除したりすることができ、ファイル記述子に対する I/O イベント発生をポーリングすることができます;
   ポーリングオブジェクトが提供しているメソッドについては :ref:`poll-objects` 節を参照してください。


.. function:: kqueue()

   .. (Only supported on BSD.)  Returns a kernel queue object; see section
   .. :ref:`kqueue-objects` below for the methods supported by kqueue objects.

   (BSD でのみサポート) カーネルキュー(kernel queue)オブジェクトを返します。
   カーネルキューオブジェクトが提供しているメソッドについては、 :ref:`kqueue-objects` 節を参照してください。


   .. versionadded:: 2.6


.. function:: kevent(ident, filter=KQ_FILTER_READ, flags=KQ_EV_ADD, fflags=0, data=0, udata=0)

   .. (Only supported on BSD.)  Returns a kernel event object; see section
   .. :ref:`kevent-objects` below for the methods supported by kevent objects.

   (BSD でのみサポート) カーネルイベント(kernel event)オブジェクトを返します。
   カーネルイベントオブジェクトが提供しているメソッドについては、 :ref:`kevent-objects` 節を参照してください。


   .. versionadded:: 2.6


.. function:: select(rlist, wlist, xlist[, timeout])

   .. This is a straightforward interface to the Unix :cfunc:`select` system call.
   .. The first three arguments are sequences of 'waitable objects': either
   .. integers representing file descriptors or objects with a parameterless method
   .. named :meth:`fileno` returning such an integer:

   Unix の :c:func:`select` システムコールに対する直接的なインタフェースです。
   最初の 3 つの引数は '待機可能オブジェクト' からなるシーケンスです:
   待機可能オブジェクトとは、ファイル記述子を表す整数値か、引数なしで整数を返すメソッド :meth:`fileno` を持つオブジェクトです。


   .. * *rlist*: wait until ready for reading
   .. * *wlist*: wait until ready for writing
   .. * *xlist*: wait for an "exceptional condition" (see the manual page for what
   ..   your system considers such a condition)

   * *rlist*: 読み込み可能になるまで待つ
   * *wlist*: 書き込み可能になるまで待つ
   * *xlist*: "例外状態 (exceptional condition)" になるまで待つ
     ("例外状態" については、システムのマニュアルページを参照してください)


   .. Empty sequences are allowed, but acceptance of three empty sequences is
   .. platform-dependent. (It is known to work on Unix but not on Windows.)  The
   .. optional *timeout* argument specifies a time-out as a floating point number
   .. in seconds.  When the *timeout* argument is omitted the function blocks until
   .. at least one file descriptor is ready.  A time-out value of zero specifies a
   .. poll and never blocks.

   引数に空のシーケンスを指定してもかまいませんが、3 つの引数全てを空のシーケンスにしてもよいかどうかはプラットフォームに依存します
   (Unix では動作し、Windows では動作しないことが知られています)。
   オプションの *timeout* 引数にはタイムアウトまでの秒数を浮動小数点数で指定します。
   *timeout* 引数が省略された場合、関数は少なくとも一つのファイル記述子が何らかの準備完了状態になるまでブロックします。
   *timeout* に 0 を指定した場合は、ポーリングを行いブロックしないことを示します。


   .. The return value is a triple of lists of objects that are ready: subsets of the
   .. first three arguments.  When the time-out is reached without a file descriptor
   .. becoming ready, three empty lists are returned.

   戻り値は準備完了状態のオブジェクトからなる 3 つのリストです: したがってこのリストはそれぞれ関数の最初の 3 つの引数のサブセットに
   なります。ファイル記述子のいずれも準備完了にならないままタイムアウトした場合、3 つの空のリストが返されます。


   .. index::
      single: socket() (in module socket)
      single: popen() (in module os)


   .. Among the acceptable object types in the sequences are Python file objects (e.g.
   .. ``sys.stdin``, or objects returned by :func:`open` or :func:`os.popen`), socket
   .. objects returned by :func:`socket.socket`.  You may also define a :dfn:`wrapper`
   .. class yourself, as long as it has an appropriate :meth:`fileno` method (that
   .. really returns a file descriptor, not just a random integer).

   シーケンスの中に含めることのできるオブジェクトは Python ファイルオブジェクト
   (例えば ``sys.stdin`` や、 :func:`open` または :func:`os.popen` が返すオブジェクト)、
   :func:`socket.socket` が返すソケットオブジェクトです。
   ラッパー (:dfn:`wrapper`) クラスを自分で定義することもできます。
   この場合、適切な (単なる乱数ではなく本当のファイル記述子を返す) :meth:`fileno`  メソッドを持つ必要があります。


   .. note::

      .. index:: single: WinSock


      .. File objects on Windows are not acceptable, but sockets are.  On Windows,
      .. the underlying :cfunc:`select` function is provided by the WinSock
      .. library, and does not handle file descriptors that don't originate from
      .. WinSock.

      :func:`select` は Windows のファイルオブジェクトを受理しませんが、ソケットは受理します。
      Windows では、背後の :c:func:`select` 関数は WinSock ライブラリで提供されており、
      WinSock によって生成されたものではないファイル記述子を扱うことができないのです。

.. attribute:: select.PIPE_BUF

   :func:`select` や :func:`poll` などのインタフェースにより書き込み可能になったと
   報告されたファイルは、 :const:`PIPE_BUF` バイトまではブロックしないで書き込み
   できることが保証されます。
   この値は POSIX では最低でも 512 であることが保証されています。

   利用可能な環境: Unix

   .. versionadded:: 2.7

.. Edge and Level Trigger Polling (epoll) Objects

.. _epoll-objects:

エッジおよびレベルトリガポーリング (epoll) オブジェクト
-------------------------------------------------------

   http://linux.die.net/man/4/epoll

   *eventmask*

   +-----------------------+-----------------------------------------------+
   | 定数                  | 意味                                          |
   +=======================+===============================================+
   | :const:`EPOLLIN`      | 読み込み可能                                  |
   +-----------------------+-----------------------------------------------+
   | :const:`EPOLLOUT`     | 書き込み可能                                  |
   +-----------------------+-----------------------------------------------+
   | :const:`EPOLLPRI`     | 緊急の読み出しデータ                          |
   +-----------------------+-----------------------------------------------+
   | :const:`EPOLLERR`     | 設定された fd にエラー状態が発生した          |
   +-----------------------+-----------------------------------------------+
   | :const:`EPOLLHUP`     | 設定された fd がハングアップした              |
   +-----------------------+-----------------------------------------------+
   | :const:`EPOLLET`      | エッジトリガ動作に設定する。デフォルトでは    |
   |                       | レベルトリガ動作                              |
   +-----------------------+-----------------------------------------------+
   | :const:`EPOLLONESHOT` | 1ショット動作に設定する。1回イベントが取り出  |
   |                       | されたら、その fd が内部で無効になる。        |
   +-----------------------+-----------------------------------------------+
   | :const:`EPOLLRDNORM`  | :const:`EPOLLIN` と同じ                       |
   +-----------------------+-----------------------------------------------+
   | :const:`EPOLLRDBAND`  | priority data band を読み込める               |
   +-----------------------+-----------------------------------------------+
   | :const:`EPOLLWRNORM`  | :const:`EPOLLOUT` と同じ                      |
   +-----------------------+-----------------------------------------------+
   | :const:`EPOLLWRBAND`  | priority data に書き込みできる                |
   +-----------------------+-----------------------------------------------+
   | :const:`EPOLLMSG`     | 無視される                                    |
   +-----------------------+-----------------------------------------------+


.. method:: epoll.close()

   .. Close the control file descriptor of the epoll object.

   epoll オブジェクトの制御用ファイル記述子を閉じる。


.. method:: epoll.fileno()

   .. Return the file descriptor number of the control fd.

   制御用ファイル記述子の番号を返す。


.. method:: epoll.fromfd(fd)

   .. Create an epoll object from a given file descriptor.

   *fd* から epoll オブジェクトを作成する。


.. method:: epoll.register(fd[, eventmask])

   .. Register a fd descriptor with the epoll object.

   epoll オブジェクトにファイル記述子 *fd* を登録する。


   .. note::

     .. Registering a file descriptor that's already registered raises an
     .. IOError -- contrary to :ref:`poll-objects`'s register.

     :ref:`poll-objects` の register とは異なり、
     登録済みのファイル記述子を登録しようとすると IOError が発生します。

 
.. method:: epoll.modify(fd, eventmask)

   .. Modify a register file descriptor.

   ファイル記述子 *fd* の登録を変更する。


.. method:: epoll.unregister(fd)

   .. Remove a registered file descriptor from the epoll object.

   epoll オブジェクトから登録されたファイル記述子 *fd* を削除する。


.. method:: epoll.poll([timeout=-1[, maxevents=-1]])

   .. Wait for events. timeout in seconds (float)

   イベントを待つ。 *timeout* はタイムアウト時間で、単位は秒(float型) です。


.. _poll-objects:

ポーリングオブジェクト
----------------------

.. The :cfunc:`poll` system call, supported on most Unix systems, provides better
.. scalability for network servers that service many, many clients at the same
.. time. :cfunc:`poll` scales better because the system call only requires listing
.. the file descriptors of interest, while :cfunc:`select` builds a bitmap, turns
.. on bits for the fds of interest, and then afterward the whole bitmap has to be
.. linearly scanned again. :cfunc:`select` is O(highest file descriptor), while
.. :cfunc:`poll` is O(number of file descriptors).

:c:func:`poll` システムコールはほとんどの Unix システムでサポートされており、
非常に多数のクライアントに同時にサービスを提供するようなネットワークサーバが高いスケーラビリティを持てるようにしています。
:c:func:`poll` は対象のファイル記述子を列挙するだけでよいため、良くスケールします。
一方、 :c:func:`select` はビット対応表を構築し、対象ファイルの記述子に対応するビットを立て、その後全ての対応表の全てのビットを線形探索します。
:c:func:`select` は O(最大のファイル記述子番号) なのに対し、 :c:func:`poll` は O(対象とするファイル記述子の数) で済みます。


.. method:: poll.register(fd[, eventmask])

   .. Register a file descriptor with the polling object.  Future calls to the
   .. :meth:`poll` method will then check whether the file descriptor has any pending
   .. I/O events.  *fd* can be either an integer, or an object with a :meth:`fileno`
   .. method that returns an integer.  File objects implement :meth:`fileno`, so they
   .. can also be used as the argument.

   ファイル記述子をポーリングオブジェクトに登録します。
   これ以降の :meth:`poll` メソッド呼び出しでは、そのファイル記述子に処理待ち中の I/O イベントがあるかどうかを監視します。
   *fd* は整数か、整数値を返す :meth:`fileno` メソッドを持つオブジェクトを取ります。
   ファイルオブジェクトも :meth:`fileno` を実装しているので、引数として使うことができます。


   .. *eventmask* is an optional bitmask describing the type of events you want to
   .. check for, and can be a combination of the constants :const:`POLLIN`,
   .. :const:`POLLPRI`, and :const:`POLLOUT`, described in the table below.  If not
   .. specified, the default value used will check for all 3 types of events.

   *eventmask* はオプションのビットマスクで、どの種類の I/O イベントを監視したいかを記述します。
   この値は以下の表で述べる定数 :const:`POLLIN` 、 :const:`POLLPRI` 、および :const:`POLLOUT` の組み合わせにすることができます。
   ビットマスクを指定しない場合、標準の値が使われ、 3 種類のイベント全てに対して監視が行われます。


   .. +-------------------+------------------------------------------+
   .. | Constant          | Meaning                                  |
   .. +===================+==========================================+
   .. | :const:`POLLIN`   | There is data to read                    |
   .. +-------------------+------------------------------------------+
   .. | :const:`POLLPRI`  | There is urgent data to read             |
   .. +-------------------+------------------------------------------+
   .. | :const:`POLLOUT`  | Ready for output: writing will not block |
   .. +-------------------+------------------------------------------+
   .. | :const:`POLLERR`  | Error condition of some sort             |
   .. +-------------------+------------------------------------------+
   .. | :const:`POLLHUP`  | Hung up                                  |
   .. +-------------------+------------------------------------------+
   .. | :const:`POLLNVAL` | Invalid request: descriptor not open     |
   .. +-------------------+------------------------------------------+

   +-------------------+----------------------------------------------------------+
   | 定数              | 意味                                                     |
   +===================+==========================================================+
   | :const:`POLLIN`   | 読み出し可能なデータが存在する                           |
   +-------------------+----------------------------------------------------------+
   | :const:`POLLPRI`  | 緊急の読み出し可能なデータが存在する                     |
   +-------------------+----------------------------------------------------------+
   | :const:`POLLOUT`  | 書き出しの準備ができている: 書き出し処理がブロックしない |
   +-------------------+----------------------------------------------------------+
   | :const:`POLLERR`  | 何らかのエラー状態                                       |
   +-------------------+----------------------------------------------------------+
   | :const:`POLLHUP`  | ハングアップ                                             |
   +-------------------+----------------------------------------------------------+
   | :const:`POLLNVAL` | 無効な要求: 記述子が開かれていない                       |
   +-------------------+----------------------------------------------------------+


   .. Registering a file descriptor that's already registered is not an error, and has
   .. the same effect as registering the descriptor exactly once.

   登録済みのファイル記述子を登録してもエラーにはならず、一度だけ登録した場合と同じ効果になります。


.. method:: poll.modify(fd, eventmask)

   登録されているファイル記述子 *fd* を変更する。
   これは、 ``register(fd, eventmask)`` と同じ効果を持ちます。
   登録されていないファイル記述子に対してこのメソッドを呼び出すと、
   errno :const:`ENOENT` で :exc:`IOError` 例外が発生します。


   .. versionadded:: 2.6


.. method:: poll.unregister(fd)

   .. Remove a file descriptor being tracked by a polling object.  Just like the
   .. :meth:`register` method, *fd* can be an integer or an object with a
   .. :meth:`fileno` method that returns an integer.

   ポーリングオブジェクトによって追跡中のファイル記述子を登録解除します。
   :meth:`register` メソッドと同様に、 *fd* は整数か、整数値を返す :meth:`fileno` メソッドを持つオブジェクトを取ります。


   .. Attempting to remove a file descriptor that was never registered causes a
   .. :exc:`KeyError` exception to be raised.

   登録されていないファイル記述子を登録解除しようとすると :exc:`KeyError` 例外が送出されます。


.. method:: poll.poll([timeout])

   .. Polls the set of registered file descriptors, and returns a possibly-empty list
   .. containing ``(fd, event)`` 2-tuples for the descriptors that have events or
   .. errors to report. *fd* is the file descriptor, and *event* is a bitmask with
   .. bits set for the reported events for that descriptor --- :const:`POLLIN` for
   .. waiting input, :const:`POLLOUT` to indicate that the descriptor can be written
   .. to, and so forth. An empty list indicates that the call timed out and no file
   .. descriptors had any events to report. If *timeout* is given, it specifies the
   .. length of time in milliseconds which the system will wait for events before
   .. returning. If *timeout* is omitted, negative, or :const:`None`, the call will
   .. block until there is an event for this poll object.

   登録されたファイル記述子に対してポーリングを行い、報告すべき I/O イベントまたはエラーの発生したファイル記述子毎に
   2 要素のタプル ``(fd, event)`` からなるリストを返します。リストは空になることもあります。
   *fd* はファイル記述子で、 *event* は該当するファイル記述子について報告されたイベントを表すビットマスクです
   --- 例えば :const:`POLLIN` は入力待ちを示し、 :const:`POLLOUT` はファイル記述子に対する書き込みが可能を示す、などです。
   空のリストは呼び出しがタイムアウトしたか、報告すべきイベントがどのファイル記述子でも発生しなかったことを示します。
   *timeout* が与えられた場合、処理を戻すまで待機する時間の長さをミリ秒単位で指定します。
   *timeout* が省略されたり、負の値であったり、あるいは :const:`None` の場合、
   そのポーリングオブジェクトが監視している何らかのイベントが発生するまでブロックします。


.. _kqueue-objects:

kqueue オブジェクト
-------------------

.. method:: kqueue.close()

   .. Close the control file descriptor of the kqueue object.

   kqueue オブジェクトの制御用ファイル記述子を閉じる。


.. method:: kqueue.fileno()

   .. Return the file descriptor number of the control fd.

   制御用ファイル記述子の番号を返す。


.. method:: kqueue.fromfd(fd)

   .. Create a kqueue object from a given file descriptor.

   与えられたファイル記述子から、kqueue オブジェクトを作成する。


.. method:: kqueue.control(changelist, max_events[, timeout=None]) -> eventlist

   .. Low level interface to kevent

   kevent に対する低レベルのインタフェース


   .. - changelist must be an iterable of kevent object or None
   .. - max_events must be 0 or a positive integer
   .. - timeout in seconds (floats possible)

   - *changelist* は kevent オブジェクトの iterable または ``None``
   - *max_events* は 0 または正の整数
   - *timeout* はタイムアウト秒数 (float を利用可能)


.. Kevent Objects

.. _kevent-objects:

kevent オブジェクト
--------------------

http://www.freebsd.org/cgi/man.cgi?query=kqueue&sektion=2

.. attribute:: kevent.ident

   .. Value used to identify the event. The interpretation depends on the filter
   .. but it's usually the file descriptor. In the constructor ident can either
   .. be an int or an object with a fileno() function. kevent stores the integer
   .. internally.

   イベントを特定するための値。この値は、フィルタにもよりますが、大抵の場合はファイル記述子です。
   コンストラクタでは、 ident として、整数値か fileno() メソッドを持ったオブジェクトを渡せます。
   kevent は内部で整数値を保存します。


.. attribute:: kevent.filter

   .. Name of the kernel filter.

   カーネルフィルタの名前。


   .. +---------------------------+---------------------------------------------+
   .. | Constant                  | Meaning                                     |
   .. +===========================+=============================================+
   .. | :const:`KQ_FILTER_READ`   | Takes a descriptor and returns whenever     |
   .. |                           | there is data available to read             |
   .. +---------------------------+---------------------------------------------+
   .. | :const:`KQ_FILTER_WRITE`  | Takes a descriptor and returns whenever     |
   .. |                           | there is data available to write            |
   .. +---------------------------+---------------------------------------------+
   .. | :const:`KQ_FILTER_AIO`    | AIO requests                                |
   .. +---------------------------+---------------------------------------------+
   .. | :const:`KQ_FILTER_VNODE`  | Returns when one or more of the requested   |
   .. |                           | events watched in *fflag* occurs            |
   .. +---------------------------+---------------------------------------------+
   .. | :const:`KQ_FILTER_PROC`   | Watch for events on a process id            |
   .. +---------------------------+---------------------------------------------+
   .. | :const:`KQ_FILTER_NETDEV` | Watch for events on a network device        |
   .. |                           | [not available on Mac OS X]                 |
   .. +---------------------------+---------------------------------------------+
   .. | :const:`KQ_FILTER_SIGNAL` | Returns whenever the watched signal is      |
   .. |                           | delivered to the process                    |
   .. +---------------------------+---------------------------------------------+
   .. | :const:`KQ_FILTER_TIMER`  | Establishes an arbitrary timer              |
   .. +---------------------------+---------------------------------------------+

   +---------------------------+--------------------------------------------------------------------------+
   | 定数                      | 意味                                                                     |
   +===========================+==========================================================================+
   | :const:`KQ_FILTER_READ`   | 記述子を受け取り、読み込めるデータが存在する時に戻る                     |
   +---------------------------+--------------------------------------------------------------------------+
   | :const:`KQ_FILTER_WRITE`  | 記述子を受け取り、書き込み可能な時に戻る                                 |
   +---------------------------+--------------------------------------------------------------------------+
   | :const:`KQ_FILTER_AIO`    | AIO リクエスト                                                           |
   +---------------------------+--------------------------------------------------------------------------+
   | :const:`KQ_FILTER_VNODE`  | *fflag* で監視されたイベントが1つ以上発生したときに戻る                  |
   +---------------------------+--------------------------------------------------------------------------+
   | :const:`KQ_FILTER_PROC`   | プロセスID上のイベントを監視する                                         |
   +---------------------------+--------------------------------------------------------------------------+
   | :const:`KQ_FILTER_NETDEV` | ネットワークデバイス上のイベントを監視する (Mac OS X では利用不可)       |
   +---------------------------+--------------------------------------------------------------------------+
   | :const:`KQ_FILTER_SIGNAL` | 監視しているシグナルがプロセスに届いたときに戻る                         |
   +---------------------------+--------------------------------------------------------------------------+
   | :const:`KQ_FILTER_TIMER`  | 任意のタイマを設定する                                                   |
   +---------------------------+--------------------------------------------------------------------------+


.. attribute:: kevent.flags

   .. Filter action.

   フィルタアクション。


   .. +---------------------------+---------------------------------------------+
   .. | Constant                  | Meaning                                     |
   .. +===========================+=============================================+
   .. | :const:`KQ_EV_ADD`        | Adds or modifies an event                   |
   .. +---------------------------+---------------------------------------------+
   .. | :const:`KQ_EV_DELETE`     | Removes an event from the queue             |
   .. +---------------------------+---------------------------------------------+
   .. | :const:`KQ_EV_ENABLE`     | Permitscontrol() to returns the event       |
   .. +---------------------------+---------------------------------------------+
   .. | :const:`KQ_EV_DISABLE`    | Disablesevent                               |
   .. +---------------------------+---------------------------------------------+
   .. | :const:`KQ_EV_ONESHOT`    | Removes event after first occurrence        |
   .. +---------------------------+---------------------------------------------+
   .. | :const:`KQ_EV_CLEAR`      | Reset the state after an event is retrieved |
   .. +---------------------------+---------------------------------------------+
   .. | :const:`KQ_EV_SYSFLAGS`   | internal event                              |
   .. +---------------------------+---------------------------------------------+
   .. | :const:`KQ_EV_FLAG1`      | internal event                              |
   .. +---------------------------+---------------------------------------------+
   .. | :const:`KQ_EV_EOF`        | Filter specific EOF condition               |
   .. +---------------------------+---------------------------------------------+
   .. | :const:`KQ_EV_ERROR`      | See return values                           |
   .. +---------------------------+---------------------------------------------+

   +---------------------------+---------------------------------------------+
   | 定数                      | 意味                                        |
   +===========================+=============================================+
   | :const:`KQ_EV_ADD`        | イベントを追加または修正する                |
   +---------------------------+---------------------------------------------+
   | :const:`KQ_EV_DELETE`     | キューからイベントを取り除く                |
   +---------------------------+---------------------------------------------+
   | :const:`KQ_EV_ENABLE`     | control() がイベントを返すのを許可する      |
   +---------------------------+---------------------------------------------+
   | :const:`KQ_EV_DISABLE`    | イベントを無効にする                        |
   +---------------------------+---------------------------------------------+
   | :const:`KQ_EV_ONESHOT`    | イベントを最初の発生後無効にする            |
   +---------------------------+---------------------------------------------+
   | :const:`KQ_EV_CLEAR`      | イベントを受け取った後で状態をリセットする  |
   +---------------------------+---------------------------------------------+
   | :const:`KQ_EV_SYSFLAGS`   | 内部イベント                                |
   +---------------------------+---------------------------------------------+
   | :const:`KQ_EV_FLAG1`      | 内部イベント                                |
   +---------------------------+---------------------------------------------+
   | :const:`KQ_EV_EOF`        | フィルタ依存のEOF状態                       |
   +---------------------------+---------------------------------------------+
   | :const:`KQ_EV_ERROR`      | 戻り値を参照                                |
   +---------------------------+---------------------------------------------+


.. attribute:: kevent.fflags

   .. Filter specific flags.

   フィルタ依存のフラグ。


   .. :const:`KQ_FILTER_READ` and  :const:`KQ_FILTER_WRITE` filter flags:

   :const:`KQ_FILTER_READ` と :const:`KQ_FILTER_WRITE` フィルタのフラグ:


   .. +----------------------------+--------------------------------------------+
   .. | Constant                   | Meaning                                    |
   .. +============================+============================================+
   .. | :const:`KQ_NOTE_LOWAT`     | low water mark of a socket buffer          |
   .. +----------------------------+--------------------------------------------+

   +----------------------------+--------------------------------------------+
   | 定数                       | 意味                                       |
   +============================+============================================+
   | :const:`KQ_NOTE_LOWAT`     | ソケットバッファの最低基準値               |
   +----------------------------+--------------------------------------------+


   .. :const:`KQ_FILTER_VNODE` filter flags:

   :const:`KQ_FILTER_VNODE` フィルタのフラグ:


   .. +----------------------------+--------------------------------------------+
   .. | Constant                   | Meaning                                    |
   .. +============================+============================================+
   .. | :const:`KQ_NOTE_DELETE`    | *unlink()* was called                      |
   .. +----------------------------+--------------------------------------------+
   .. | :const:`KQ_NOTE_WRITE`     | a write occurred                           |
   .. +----------------------------+--------------------------------------------+
   .. | :const:`KQ_NOTE_EXTEND`    | the file was extended                      |
   .. +----------------------------+--------------------------------------------+
   .. | :const:`KQ_NOTE_ATTRIB`    | an attribute was changed                   |
   .. +----------------------------+--------------------------------------------+
   .. | :const:`KQ_NOTE_LINK`      | the link count has changed                 |
   .. +----------------------------+--------------------------------------------+
   .. | :const:`KQ_NOTE_RENAME`    | the file was renamed                       |
   .. +----------------------------+--------------------------------------------+
   .. | :const:`KQ_NOTE_REVOKE`    | access to the file was revoked             |
   .. +----------------------------+--------------------------------------------+

   +----------------------------+--------------------------------------------+
   | 定数                       | 意味                                       |
   +============================+============================================+
   | :const:`KQ_NOTE_DELETE`    | *unlink()* が呼ばれた                      |
   +----------------------------+--------------------------------------------+
   | :const:`KQ_NOTE_WRITE`     | 書き込みが発生した                         |
   +----------------------------+--------------------------------------------+
   | :const:`KQ_NOTE_EXTEND`    | ファイルのサイズが拡張された               |
   +----------------------------+--------------------------------------------+
   | :const:`KQ_NOTE_ATTRIB`    | 属性が変更された                           |
   +----------------------------+--------------------------------------------+
   | :const:`KQ_NOTE_LINK`      | リンクカウントが変更された                 |
   +----------------------------+--------------------------------------------+
   | :const:`KQ_NOTE_RENAME`    | ファイル名が変更された                     |
   +----------------------------+--------------------------------------------+
   | :const:`KQ_NOTE_REVOKE`    | ファイルアクセスが破棄された               |
   +----------------------------+--------------------------------------------+


   .. :const:`KQ_FILTER_PROC` filter flags:

   :const:`KQ_FILTER_PROC` フィルタフラグ:


   .. +----------------------------+--------------------------------------------+
   .. | Constant                   | Meaning                                    |
   .. +============================+============================================+
   .. | :const:`KQ_NOTE_EXIT`      | the process has exited                     |
   .. +----------------------------+--------------------------------------------+
   .. | :const:`KQ_NOTE_FORK`      | the process has called *fork()*            |
   .. +----------------------------+--------------------------------------------+
   .. | :const:`KQ_NOTE_EXEC`      | the process has executed a new process     |
   .. +----------------------------+--------------------------------------------+
   .. | :const:`KQ_NOTE_PCTRLMASK` | internal filter flag                       |
   .. +----------------------------+--------------------------------------------+
   .. | :const:`KQ_NOTE_PDATAMASK` | internal filter flag                       |
   .. +----------------------------+--------------------------------------------+
   .. | :const:`KQ_NOTE_TRACK`     | follow a process across *fork()*           |
   .. +----------------------------+--------------------------------------------+
   .. | :const:`KQ_NOTE_CHILD`     | returned on the child process for          |
   .. |                            | *NOTE_TRACK*                               |
   .. +----------------------------+--------------------------------------------+
   .. | :const:`KQ_NOTE_TRACKERR`  | unable to attach to a child                |
   .. +----------------------------+--------------------------------------------+

   +----------------------------+---------------------------------------------------+
   | 定数                       | 意味                                              |
   +============================+===================================================+
   | :const:`KQ_NOTE_EXIT`      | プロセスが終了した                                |
   +----------------------------+---------------------------------------------------+
   | :const:`KQ_NOTE_FORK`      | プロセスが *fork()* を呼び出した                  |
   +----------------------------+---------------------------------------------------+
   | :const:`KQ_NOTE_EXEC`      | プロセスが新しいプロセスを実行した                |
   +----------------------------+---------------------------------------------------+
   | :const:`KQ_NOTE_PCTRLMASK` | 内部フィルタフラグ                                |
   +----------------------------+---------------------------------------------------+
   | :const:`KQ_NOTE_PDATAMASK` | 内部フィルタフラグ                                |
   +----------------------------+---------------------------------------------------+
   | :const:`KQ_NOTE_TRACK`     | *fork()* の呼び出しを超えてプロセスを監視する     |
   +----------------------------+---------------------------------------------------+
   | :const:`KQ_NOTE_CHILD`     | *NOTE_TRACK* に対して子プロセスに渡される         |
   +----------------------------+---------------------------------------------------+
   | :const:`KQ_NOTE_TRACKERR`  | 子プロセスにアタッチできなかった                  |
   +----------------------------+---------------------------------------------------+


   .. :const:`KQ_FILTER_NETDEV` filter flags (not available on Mac OS X):

   :const:`KQ_FILTER_NETDEV` フィルタフラグ (Mac OS X では利用不可):


   .. +----------------------------+--------------------------------------------+
   .. | Constant                   | Meaning                                    |
   .. +============================+============================================+
   .. | :const:`KQ_NOTE_LINKUP`    | link is up                                 |
   .. +----------------------------+--------------------------------------------+
   .. | :const:`KQ_NOTE_LINKDOWN`  | link is down                               |
   .. +----------------------------+--------------------------------------------+
   .. | :const:`KQ_NOTE_LINKINV`   | link state is invalid                      |
   .. +----------------------------+--------------------------------------------+

   +----------------------------+--------------------------------------------+
   | 定数                       | 意味                                       |
   +============================+============================================+
   | :const:`KQ_NOTE_LINKUP`    | リンクアップしている                       |
   +----------------------------+--------------------------------------------+
   | :const:`KQ_NOTE_LINKDOWN`  | リンクダウンしている                       |
   +----------------------------+--------------------------------------------+
   | :const:`KQ_NOTE_LINKINV`   | リンク状態が不正                           |
   +----------------------------+--------------------------------------------+


.. attribute:: kevent.data

   .. Filter specific data.

   フィルタ固有のデータ。


.. attribute:: kevent.udata

   .. User defined value.

   ユーザー定義値。
