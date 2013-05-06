
:mod:`SocketServer` --- ネットワークサーバ構築のためのフレームワーク
=====================================================================

.. module:: SocketServer
   :synopsis: ネットワークサーバ構築のためのフレームワーク。

.. note::

   :mod:`SocketServer` モジュールは、Python 3では :mod:`socketserver` にリネームされました。
   :term:`2to3` ツールが、ソースコード内のimportを自動的にPython3用に修正します。


:mod:`SocketServer` モジュールはネットワークサーバを実装するタスクを単純化します。

このモジュールには 4 つのサーバクラスがあります: :class:`TCPServer` は、クライアントとサーバ間に継続的なデータ流路を提供
する、インターネット TCP プロトコルを使います。 :class:`UDPServer` は、順序通りに到着しなかったり、転送中に喪失して
しまってもかまわない情報の断続的なパケットである、データグラムを使います。 :class:`UnixStreamServer` および
:class:`UnixDatagramServer` クラスも同様ですが、Unix ドメインソケットを使います; 従って非 Unix
プラットフォームでは利用できません。ネットワークプログラミングについての詳細は、W. Richard Steven 著 UNIX Network
Programming や、 Ralph Davis 著 Win32 Network Programming のような書籍を参照してください。

これらの 4 つのクラスは要求を :dfn:`同期的に (synchronously)` 処理します;
各要求は次の要求を開始する前に完結していなければなりません。同期的な処理は、サーバで大量の計算を必要とする、あるいはクライアントが
処理するには時間がかかりすぎるような大量のデータを返す、といった理由によってリクエストに長い時間がかかる状況には向いていません。
こうした状況の解決方法は別のプロセスを生成するか、個々の要求を扱うスレッドを生成することです;  :class:`ForkingMixIn` および
:class:`ThreadingMixIn` 配合クラス (mix-in classes) を使えば、非同期的な動作をサポートできます。

サーバの作成にはいくつかのステップがあります。最初に、 :class:`BaseRequestHandler` クラスをサブクラス化して要求処理クラス
(request hander class) を生成し、その :meth:`handle` メソッドを上書きしなければなりません; このメソッドで入力される
要求を処理します。次に、サーバクラスのうち一つをインスタンス化して、サーバのアドレスと要求処理クラスを渡さなければなりません。最後に、サーバオブジェクトの
:meth:`handle_request` または  :meth:`serve_forever` メソッドを呼び出して、単一または多数の要求を処理します。

:class:`ThreadingMixIn` から継承してスレッドを利用した接続を行う場合、突発的な通信切断時の処理を明示的に指定する必要があります。
:class:`ThreadingMixIn` クラスには *daemon_threads* 属性があり、
サーバがスレッドの終了を待ち合わせるかどうかを指定する事ができます。スレッドが独自の処理を行う場合は、このフラグを明示的に指定します。
デフォルトは :const:`False` で、Pythonは :class:`ThreadingMixIn` クラス
が起動した全てのスレッドが終了するまで実行し続けます。

サーバクラス群は使用するネットワークプロトコルに関わらず、同じ外部メソッドおよび属性を持ちます。


サーバ生成に関するノート
------------------------

継承図にある五つのクラスのうち四つは四種類の同期サーバを表わしています。 ::

   +------------+
   | BaseServer |
   +------------+
         |
         v
   +-----------+        +------------------+
   | TCPServer |------->| UnixStreamServer |
   +-----------+        +------------------+
         |
         v
   +-----------+        +--------------------+
   | UDPServer |------->| UnixDatagramServer |
   +-----------+        +--------------------+

:class:`UnixDatagramServer` は :class:`UDPServer` から派生していて、
:class:`UnixStreamServer` からではないことに注意してください --- IP と Unix
ストリームサーバの唯一の違いはアドレスファミリーでそれは両方の Unix サーバクラスで単純に繰り返されています。

それぞれのタイプのサーバのフォークしたりスレッド実行したりするバージョンは :class:`ForkingMixIn` および
:class:`ThreadingMixIn` ミクシン(mix-in)クラスを使って作ることができます。たとえば、スレッド実行する UDP
サーバクラスは以下のようにして作られます。 ::

   class ThreadingUDPServer(ThreadingMixIn, UDPServer): pass

ミクシンクラスは :class:`UDPServer` で定義されるメソッドをオーバライドするために、
先に来なければなりません。様々なメンバ変数を設定することで元になるサーバ機構の振る舞いを変えられます。

サービスの実装には、 :class:`BaseRequestHandler` からクラスを派生させてその :meth:`handle`
メソッドを再定義しなければなりません。このようにすれば、サーバクラスと要求処理クラスを結合して様々なバージョンのサービスを
実行することができます。要求処理クラスはデータグラムサービスかストリームサービスかで異なることでしょう。この違いは処理サブクラス
:class:`StreamRequestHandler` または :class:`DatagramRequestHandler`
を使うという形で隠蔽できます。

もちろん、まだ頭を使わなければなりません! たとえば、サービスがリクエストによっては
書き換えられるようなメモリ上の状態を使うならば、フォークするサーバを使うのは馬鹿げています。
というのも子プロセスでの書き換えは親プロセスで保存されている初期状態にも親プロセスから分配される各子プロセスの状態にも届かないからです。この場合、
スレッド実行するサーバを使うことはできますが、共有データの一貫性を保つためにロックを使わなければならなくなるでしょう。

一方、全てのデータが外部に(たとえばファイルシステムに)保存される HTTP サーバを
作っているのだとすると、同期クラスではどうしても一つの要求が処理されている間サービスが「耳の聞こえない」状態を呈することになります --- この状態はもし
クライアントが要求した全てのデータをゆっくり受け取るととても長い時間続きかねません。こういう場合にはサーバをスレッド実行したりフォークすることが適切です。

ある場合には、要求の一部を同期的に処理する一方で、要求データに依って子プロセスを
フォークして処理を終了させる、といった方法も適当かもしれません。こうした処理方法は同期サーバを使って要求処理クラスの :meth:`handle`
メソッドの中で自分でフォークするようにして実装することができます。

スレッドも :func:`fork` もサポートされない環境で (もしくはサービスにとってそれらがあまりに高価についたり不適切な場合に)
多数の同時要求を捌くもう一つのアプローチは、部分的に処理し終えた要求のテーブルを自分で管理し、次にどの要求に対処するか
(または新しく入ってきた要求を扱うかどうか)を決めるのに :func:`select` を使う方法です。
これは(もしスレッドやサブプロセスが使えなければ)特にストリームサービスに対して重要で、そのようなサービスでは各クライアントが潜在的に長く接続し続けます。
この問題を管理する別の方法について、 :mod:`asyncore` モジュールを参照してください。

.. XXX should data and methods be intermingled, or separate?
   how should the distinction between class and instance variables be drawn?


Serverオブジェクト
------------------

.. class:: BaseServer

   .. This is the superclass of all Server objects in the module.  It defines the
      interface, given below, but does not implement most of the methods, which is
      done in subclasses.

   これは、このモジュールにある全てのサーバーオブジェクトの基底クラスです。
   このクラスは、ここから説明するインタフェースを定義していますが、そのほとんどを実装していません。
   実装はサブクラスで行われます。

.. method:: BaseServer.fileno()

   サーバが要求待ちを行っているソケットのファイル記述子を整数で返します。この関数は一般的に、同じプロセス中の複数のサーバを監視できるようにするために、
   :func:`select.select` に渡されます。


.. method:: BaseServer.handle_request()

   単一の要求を処理します。この関数は以下のメソッド: :meth:`get_request` 、 :meth:`verify_request` 、および
   :meth:`process_request` を順番に呼び出します。ハンドラ中でユーザによって提供された :meth:`handle` が例外
   を送出した場合、サーバの :meth:`handle_error` メソッドが呼び出されます。
   :attr:`self.timeout` 秒以内にリクエストが来なかった場合、 :meth:`handle_timeout` が呼ばれて、
   :meth:`handle_request` が終了します。


.. method:: BaseServer.serve_forever()

   .. Handle requests until an explicit :meth:`shutdown` request.  Polls for
      shutdown every *poll_interval* seconds.

   :meth:`shutdown` を呼ばれるまで、リクエストを処理し続けます。
   shutdown が呼ばれたかどうかを、 *poll_interval* 秒ごとにポーリングします。


.. method:: BaseServer.shutdown()

   .. Tells the :meth:`serve_forever` loop to stop and waits until it does.

   :meth:`serve_forever` ループに停止するように指示し、停止されるまで待ちます。

   .. versionadded:: 2.6


.. attribute:: BaseServer.address_family

   サーバのソケットが属しているプロトコルファミリです。
   一般的な値は :const:`socket.AF_INET` および :const:`socket.AF_UNIX`  です。


.. attribute:: BaseServer.RequestHandlerClass

   ユーザが提供する要求処理クラスです; 要求ごとにこのクラスのインスタンスが生成されます。


.. attribute:: BaseServer.server_address

   サーバが要求待ちを行うアドレスです。アドレスの形式はプロトコルファミリによって異なります。詳細は :mod:`socket` モジュールを参照してください。
   インターネットプロトコルでは、この値は例えば ``('127.0.0.1', 80)`` のようにアドレスを与える文字列と整数のポート番号を含むタプルです。


.. attribute:: BaseServer.socket

   サーバが入力の要求待ちを行うためのソケットオブジェクトです。

サーバクラスは以下のクラス変数をサポートします:

.. XXX should class variables be covered before instance variables, or vice versa?

.. attribute:: BaseServer.allow_reuse_address

   サーバがアドレスの再使用を許すかどうかを示す値です。この値は標準で :const:`False` で、サブクラスで再使用ポリシを変更するために
   設定することができます。


.. attribute:: BaseServer.request_queue_size

   要求待ち行列 (queue) のサイズです。単一の要求を処理するのに長時間かかる場合には、サーバが処理中に届いた要求は最大
   :attr:`request_queue_size` 個まで待ち行列に置かれます。待ち行列が一杯になると、それ以降のクライアントからの要求は "接続拒否
   (Connection denied)" エラーになります。標準の値は通常 5 ですが、この値はサブクラスで上書きすることができます。


.. attribute:: BaseServer.socket_type

   サーバが使うソケットの型です; 一般的な2つの値は、 :const:`socket.SOCK_STREAM` と
   :const:`socket.SOCK_DGRAM` です。


.. attribute:: BaseServer.timeout

   .. Timeout duration, measured in seconds, or :const:`None` if no timeout is
      desired.  If :meth:`handle_request` receives no incoming requests within the
      timeout period, the :meth:`handle_timeout` method is called.

   タイムアウト時間(秒)、もしくは、タイムアウトを望まない場合に :const:`None` 。
   :meth:`handle_request` がこの時間内にリクエストを受信しない場合、 :meth:`handle_timeout`
   メソッドが呼ばれます。

:class:`TCPServer` のような基底クラスのサブクラスで上書きできるサーバメソッドは多数あります; これらのメソッドはサーバオブジェクトの
外部のユーザにとっては役にたたないものです。

.. XXX should the default implementations of these be documented, or should
   it be assumed that the user will look at SocketServer.py?


.. method:: BaseServer.finish_request()

   :attr:`RequestHandlerClass` をインスタンス化し、 :meth:`handle` メソッドを呼び出して、実際に要求を処理します。


.. method:: BaseServer.get_request()

   ソケットから要求を受理して、クライアントとの通信に使われる *新しい* ソケットオブジェクト、およびクライアントのアドレスからなる、 2
   要素のタプルを返します。


.. method:: BaseServer.handle_error(request, client_address)

   この関数は :attr:`RequestHandlerClass` の :meth:`handle`
   メソッドが例外を送出した際に呼び出されます。標準の動作では標準出力へトレースバックを出力し、後続する要求を継続して処理します。


.. method:: BaseServer.handle_timeout()

   .. This function is called when the :attr:`timeout` attribute has been set to a
      value other than :const:`None` and the timeout period has passed with no
      requests being received.  The default action for forking servers is
      to collect the status of any child processes that have exited, while
      in threading servers this method does nothing.

   この関数は :attr:`timeout` 属性が :const:`None` 以外に設定されて、
   リクエストがないままタイムアウト秒数が過ぎたときに呼ばれます。
   fork型サーバーでのデフォルトの動作は、終了した子プロセスの情報を集めるようになっています。
   スレッド型サーバーではこのメソッドは何もしません。


.. method:: BaseServer.process_request(request, client_address)

   :meth:`finish_request` を呼び出して、 :meth:`RequestHandlerClass`
   のインスタンスを生成します。必要なら、この関数から新たなプロセスかスレッドを生成して要求を処理することができます; その処理は
   :class:`ForkingMixIn` または :class:`ThreadingMixIn`  クラスが行います。

.. Is there any point in documenting the following two functions?
   What would the purpose of overriding them be: initializing server
   instance variables, adding new network families?


.. method:: BaseServer.server_activate()

   サーバのコンストラクタによって呼び出され、サーバを活動状態にします。デフォルトではサーバのソケットを :meth:`listen` するだけです。
   このメソッドは上書きできます。


.. method:: BaseServer.server_bind()

   サーバのコンストラクタによって呼び出され、適切なアドレスにソケットをバインドします。このメソッドは上書きできます。


.. method:: BaseServer.verify_request(request, client_address)

   ブール値を返さなければなりません; 値が :const:`True` の場合には要求が処理され、 :const:`False` の場合には要求は拒否されます。
   サーバへのアクセス制御を実装するためにこの関数を上書きすることができます。標準の実装では常に :const:`True` を返します。


RequestHandlerオブジェクト
--------------------------

要求処理クラスでは、新たな :meth:`handle` メソッドを定義しなくてはならず、また以下のメソッドのいずれかを上書きすることができます。
各要求ごとに新たなインスタンスが生成されます。


.. method:: RequestHandler.finish()

   :meth:`handle` メソッドが呼び出された後、何らかの後始末を行うために呼び出されます。標準の実装では何も行いません。 :meth:`setup`
   または :meth:`handle` が例外を送出した場合には、この関数は呼び出されません。


.. method:: RequestHandler.handle()

   この関数では、クライアントからの要求を実現するために必要な全ての作業を行わなければなりません。デフォルト実装では何もしません。
   この作業の上で、いくつかのインスタンス属性を利用することができます; クライアントからの要求は :attr:`self.request` です;
   クライアントのアドレスは :attr:`self.client_address` です;  そしてサーバごとの情報にアクセスする場合には、サーバインスタンスを
   :attr:`self.server` で取得できます。

   :attr:`self.request` の型はサービスがデータグラム型かストリーム型かで
   異なります。ストリーム型では、 :attr:`self.request` はソケットオブジェクトです;
   データグラムサービスでは、 :attr:`self.request` は文字列とソケットのタプルになります。
   しかし、この違いは要求処理サブクラスの :class:`StreamRequestHandler` や
   :class:`DatagramRequestHandler` を使うことで隠蔽することができます。これらのクラスでは :meth:`setup` および
   :meth:`finish` メソッドを上書きしており、 :attr:`self.rfile` および :attr:`self.wfile` 属性を
   提供しています。 :attr:`self.rfile` および :attr:`self.wfile` は、要求データを取得したり
   クライアントにデータを返すために、それぞれ読み出し、書き込みを行うことができます。


.. method:: RequestHandler.setup()

   :meth:`handle`   メソッドより前に呼び出され、何らかの必要な初期化処理を行います。標準の実装では何も行いません。


例
--------

:class:`SocketServer.TCPServer` の例
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. This is the server side::

サーバーサイドの例です::

   import SocketServer

   class MyTCPHandler(SocketServer.BaseRequestHandler):
       """
       The RequestHandler class for our server.

       It is instantiated once per connection to the server, and must
       override the handle() method to implement communication to the
       client.
       """

       def handle(self):
           # self.request is the TCP socket connected to the client
           self.data = self.request.recv(1024).strip()
           print "%s wrote:" % self.client_address[0]
           print self.data
           # just send back the same data, but upper-cased
           self.request.send(self.data.upper())

   if __name__ == "__main__":
       HOST, PORT = "localhost", 9999

       # Create the server, binding to localhost on port 9999
       server = SocketServer.TCPServer((HOST, PORT), MyTCPHandler)

       # Activate the server; this will keep running until you
       # interrupt the program with Ctrl-C
       server.serve_forever()

.. An alternative request handler class that makes use of streams (file-like
   objects that simplify communication by providing the standard file interface)::

別の、ストリーム(標準のファイル型のインタフェースを利用して通信をシンプルにした
ファイルライクオブジェクト)を使うリクエストハンドラクラスの例です::

   class MyTCPHandler(SocketServer.StreamRequestHandler):

       def handle(self):
           # self.rfile is a file-like object created by the handler;
           # we can now use e.g. readline() instead of raw recv() calls
           self.data = self.rfile.readline().strip()
           print "%s wrote:" % self.client_address[0]
           print self.data
           # Likewise, self.wfile is a file-like object used to write back
           # to the client
           self.wfile.write(self.data.upper())

.. The difference is that the ``readline()`` call in the second handler will call
   ``recv()`` multiple times until it encounters a newline character, while the
   single ``recv()`` call in the first handler will just return what has been sent
   from the client in one ``send()`` call.

先ほどとの違いは、 ``readline()`` の呼び出しが、改行を受け取るまで ``recv()`` を複数回呼び出すことです。
1回の ``recv()`` の呼び出しは、クライアント側から1回の ``send()`` 呼び出しで送信された分しか受け取りません。

.. This is the client side::

クライアントサイドの例::

   import socket
   import sys

   HOST, PORT = "localhost", 9999
   data = " ".join(sys.argv[1:])

   # Create a socket (SOCK_STREAM means a TCP socket)
   sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

   # Connect to server and send data
   sock.connect((HOST, PORT))
   sock.send(data + "\n")

   # Receive data from the server and shut down
   received = sock.recv(1024)
   sock.close()

   print "Sent:     %s" % data
   print "Received: %s" % received


.. The output of the example should look something like this:

この例の出力は次のようになります。

サーバー::

   $ python TCPServer.py
   127.0.0.1 wrote:
   hello world with TCP
   127.0.0.1 wrote:
   python is nice

クライアント::

   $ python TCPClient.py hello world with TCP
   Sent:     hello world with TCP
   Received: HELLO WORLD WITH TCP
   $ python TCPClient.py python is nice
   Sent:     python is nice
   Received: PYTHON IS NICE


:class:`SocketServer.UDPServer` の例
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. This is the server side::

サーバーサイドの例です::

   import SocketServer

   class MyUDPHandler(SocketServer.BaseRequestHandler):
       """
       This class works similar to the TCP handler class, except that
       self.request consists of a pair of data and client socket, and since
       there is no connection the client address must be given explicitly
       when sending data back via sendto().
       """

       def handle(self):
           data = self.request[0].strip()
           socket = self.request[1]
           print "%s wrote:" % self.client_address[0]
           print data
           socket.sendto(data.upper(), self.client_address)

   if __name__ == "__main__":
       HOST, PORT = "localhost", 9999
       server = SocketServer.UDPServer((HOST, PORT), MyUDPHandler)
       server.serve_forever()

.. This is the client side::

クライアントサイドの例です::

   import socket
   import sys

   HOST, PORT = "localhost", 9999
   data = " ".join(sys.argv[1:])

   # SOCK_DGRAM is the socket type to use for UDP sockets
   sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

   # As you can see, there is no connect() call; UDP has no connections.
   # Instead, data is directly sent to the recipient via sendto().
   sock.sendto(data + "\n", (HOST, PORT))
   received = sock.recv(1024)

   print "Sent:     %s" % data
   print "Received: %s" % received

.. The output of the example should look exactly like for the TCP server example.

この例の出力は、TCPサーバーの例と全く同じようになります。

平行処理の Mix-in
~~~~~~~~~~~~~~~~~~~

.. To build asynchronous handlers, use the :class:`ThreadingMixIn` and
   :class:`ForkingMixIn` classes.

複数の接続を平行に処理するハンドラを作るには、 :class:`ThreadingMixIn`
か :class:`ForkingMixIn` クラスを利用します。

.. An example for the :class:`ThreadingMixIn` class::

:class:`ThreadingMixIn` クラスの利用例::

   import socket
   import threading
   import SocketServer

   class ThreadedTCPRequestHandler(SocketServer.BaseRequestHandler):

       def handle(self):
           data = self.request.recv(1024)
           cur_thread = threading.currentThread()
           response = "%s: %s" % (cur_thread.getName(), data)
           self.request.send(response)

   class ThreadedTCPServer(SocketServer.ThreadingMixIn, SocketServer.TCPServer):
       pass

   def client(ip, port, message):
       sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
       sock.connect((ip, port))
       sock.send(message)
       response = sock.recv(1024)
       print "Received: %s" % response
       sock.close()

   if __name__ == "__main__":
       # Port 0 means to select an arbitrary unused port
       HOST, PORT = "localhost", 0

       server = ThreadedTCPServer((HOST, PORT), ThreadedTCPRequestHandler)
       ip, port = server.server_address

       # Start a thread with the server -- that thread will then start one
       # more thread for each request
       server_thread = threading.Thread(target=server.serve_forever)
       # Exit the server thread when the main thread terminates
       server_thread.setDaemon(True)
       server_thread.start()
       print "Server loop running in thread:", server_thread.getName()

       client(ip, port, "Hello World 1")
       client(ip, port, "Hello World 2")
       client(ip, port, "Hello World 3")

       server.shutdown()

.. The output of the example should look something like this::

この例の出力は次のようになります::

   $ python ThreadedTCPServer.py
   Server loop running in thread: Thread-1
   Received: Thread-2: Hello World 1
   Received: Thread-3: Hello World 2
   Received: Thread-4: Hello World 3


.. The :class:`ForkingMixIn` class is used in the same way, except that the server
   will spawn a new process for each request.

:class:`ForkingMixIn` クラスは同じように利用することができます。
この場合、サーバーはリクエスト毎に新しいプロセスを作成します。
