
:mod:`asyncore` --- 非同期ソケットハンドラ
==========================================

.. module:: asyncore
   :synopsis: 非同期なソケット制御サービスのためのベースクラス
.. moduleauthor:: Sam Rushing <rushing@nightmare.com>
.. sectionauthor:: Christopher Petrilli <petrilli@amber.org>
.. sectionauthor:: Steve Holden <sholden@holdenweb.com>
.. heavily adapted from original documentation by Sam Rushing


このモジュールは、非同期ソケットサービスのクライアント・サーバを開発するための基盤として使われます。

CPUが一つしかない場合、プログラムが"二つのことを同時に"実行する方法は二つしかありません。
もっとも簡単で一般的なのはマルチスレッドを利用する方法ですが、これとはまったく異なる
テクニックで、一つのスレッドだけでマルチスレッドと同じような効果を得られるテクニックがあります。
このテクニックはI/O処理が中心である場合にのみ有効で、CPU負荷の高いプログラムでは効果が無く、
この場合にはプリエンプティブなスケジューリングが可能なスレッドが有効でしょう。
しかし、多くの場合、ネットワークサーバではCPU負荷よりはIO負荷が問題となります。

もしOSのI/Oライブラリがシステムコール :c:func:`select` をサポートしている場合
（ほとんどの場合はサポートされている）、I/O処理は"バックグラウンド"で実行し、
その間に他の処理を実行すれば、複数の通信チャネルを同時にこなすことができます。
一見、この戦略は奇妙で複雑に思えるかもしれませんが、いろいろな面でマルチスレッドよりも理解しやすく、制御も容易です。
:mod:`asyncore` は多くの複雑な問題を解決済みなので、洗練され、パフォーマンスにも優れた
ネットワークサーバとクライアントを簡単に開発することができます。
とくに、 :mod:`asynchat` のような、対話型のアプリケーションやプロトコルには非常に有効でしょう。

基本的には、この二つのモジュールを使う場合は一つ以上のネットワーク *チャネル* を
:class:`asyncore.dispatcher` クラス、または :class:`asynchat.async_chat`
のインスタンスとして作成します。作成されたチャネルはグローバルマップに登録され、
:func:`loop` 関数で参照されます。 :func:`loop` には、専用の *マップ* を渡す事も可能です。

チャネルを生成後、 :func:`loop` を呼び出すとチャネル処理が開始し、最後のチャネル
（非同期処理中にマップに追加されたチャネルを含む）が閉じるまで継続します。


.. function:: loop([timeout[, use_poll[, map[,count]]]])

   ポーリングループを開始し、count回が過ぎるか、全てのオープン済みチャネルがクローズ\
   された場合のみ終了します。
   全ての引数はオプションです。
   引数 *count* のデフォルト値はNoneで、ループは全てのチャネルがクローズされた場合のみ終了します。
   引数 *timeout* は :func:`select` または :func:`poll` の引数timeoutとして渡され、
   秒単位で指定します。デフォルト値は30秒です。
   引数 *use_poll* が真のとき、 :func:`select` ではなく :func:`poll` が使われます。
   デフォルト値は ``False`` です。

   引数 *map* には、監視するチャネルをアイテムとして格納した辞書を指定します。
   チャネルがクローズされた時に *map* からそのチャネルが削除されます。
   *map* が省略された場合、グローバルなマップが使用されます。
   チャネル (:class:`asyncore.dispatcher`, :class:`asynchat.async_chat` とその\
   サブクラス) は自由に混ぜて map に入れることができます。


.. class:: dispatcher()

   :class:`dispatcher` クラスは、低レベルソケットオブジェクトの薄いラッパーです。
   便宜上、非同期ループから呼び出されるイベント処理メソッドを追加していますが、\
   これ以外の点では、non-blockingなソケットと同様です。

   非同期ループ内で低レベルイベントが発生した場合、発生のタイミングやコネクション\
   の状態から特定の高レベルイベントへと置き換えることができます。
   例えばソケットを他のホストに接続する場合、最初の書き込み可能イベントが発生すれば\
   接続が完了した事が分かります(この時点で、ソケットへの書き込みは成功すると考えられる)。
   このように判定できる高レベルイベントを以下に示します：

   +----------------------+-----------------------------------------------------+
   | イベント             | 解説                                                |
   +======================+=====================================================+
   | ``handle_connect()`` | 最初にreadもしくはwriteイベントが発生した時         |
   +----------------------+-----------------------------------------------------+
   | ``handle_close()``   | 読み込み可能なデータなしでreadイベントが発生した時  |
   +----------------------+-----------------------------------------------------+
   | ``handle_accept()``  | listen中のソケットでreadイベントが発生した時        |
   +----------------------+-----------------------------------------------------+

   非同期処理中、マップに登録されたチャネルの :meth:`readable` メソッドと
   :meth:`writable` メソッドが呼び出され、 :c:func:`select` か
   :c:func:`poll` でread/writeイベントを検出するリストに登録するか否かを判定します。

   このようにして、チャネルでは低レベルなソケットイベントの種類より多くの種類のイベントを\
   検出する事ができます。
   以下にあげるイベントは、サブクラスでオーバライドすることが可能です：


   .. method:: handle_read()

      非同期ループで、チャネルのソケットの :meth:`read` メソッドの呼び出しが成功した時に呼び出されます。


   .. method:: handle_write()

      非同期ループで、書き込み可能ソケットが実際に書き込み可能になった時に呼び出される。
      このメソッドは、パフォーマンスの向上のためバッファリングを行う場合などに利用できます。
      例：  ::

         def handle_write(self):
             sent = self.send(self.buffer)
             self.buffer = self.buffer[sent:]


   .. method:: handle_expt()

      out of band (OOB)データが検出された時に呼び出されます。
      OOBはあまりサポートされておらず、また滅多に使われないので、
      :meth:`handle_expt` が呼び出されることはほとんどありません。


   .. method:: handle_connect()

      ソケットの接続が確立した時に呼び出されます。
      "welcome"バナーの送信、プロトコルネゴシエーションの初期化などを行います。


   .. method:: handle_close()

      ソケットが閉じた時に呼び出されます。


   .. method:: handle_error()

      捕捉されない例外が発生した時に呼び出されます。
      デフォルトでは、短縮したトレースバック情報が出力されます。


   .. method:: handle_accept()

      listen中のチャネルがリモートホストからの :meth:`connect`
      で接続され、接続が確立した時に呼び出されます。


   .. method:: readable()

      非同期ループ中に呼び出され、readイベントの監視リストに加えるか否かを決定します。
      デフォルトのメソッドでは ``True`` を返し、readイベントの発生を監視します。


   .. method:: writable()

      非同期ループ中に呼び出され、writeイベントの監視リストに加えるか否かを決定します。
      デフォルトのメソッドでは ``True`` を返し、writeイベントの発生を監視します。

   さらに、チャネルにはソケットのメソッドとほぼ同じメソッドがあり、チャネルはソケットの\
   メソッドの多くを委譲・拡張しており、ソケットとほぼ同じメソッドを持っています。


   .. method:: create_socket(family, type)

      引数も含め、通常のソケット生成と同じ。 :mod:`socket` モジュールを参照のこと。


   .. method:: connect(address)

      通常のソケットオブジェクトと同様、 *address* には一番目の値が接続先ホスト、\
      2番目の値がポート番号であるタプルを指定します。


   .. method:: send(data)

      リモート側の端点に *data* を送出します。


   .. method:: recv(buffer_size)

      リモート側の端点より、最大 *buffer_size* バイトのデータを読み込みます。
      長さ0の文字列が返ってきた場合、チャネルはリモートから切断された事を示します。


   .. method:: listen(backlog)

      ソケットへの接続を待つ。
      引数 *backlog* は、キューイングできるコネクションの最大数を指定します(1以上)。
      最大数はシステムに依存でします（通常は5)


   .. method:: bind(address)

      ソケットを *address* にバインドします。ソケットはバインド済みであってはなりません。
      (*address* の形式は、アドレスファミリに依存します。 :mod:`socket` モジュールを参照のこと。)
      ソケットを再利用可能にする (:const:`SO_REUSEADDR` オプションを設定する) には、 :class:`dispatcher` オブジェクトの :meth:`set_reuse_addr` メソッドを呼び出してください。


   .. method:: accept()

      接続を受け入れます。
      ソケットはアドレスにバインド済みであり、 :meth:`listen`
      で接続待ち状態でなければなりません。
      戻り値は ``None`` か ``(conn, address)`` のペアで、
      *conn* はデータの送受信を行う **新しい** ソケットオブジェクト、
      *address* は接続先ソケットがバインドされているアドレスです。
      ``None`` が返された場合、接続が起こらなかったことを意味します。
      その場合、サーバーはこのイベントを無視して後続の接続を待ち続けるべきです。


   .. method:: close()

      ソケットをクローズします。
      以降の全ての操作は失敗します。
      リモート端点では、キューに溜まったデータ以外、これ以降のデータ受信は行えません。
      ソケットはガベージコレクト時に自動的にクローズされます。


.. class:: dispatcher_with_send()

   :class:`dispatcher` のサブクラスで、シンプルなバッファされた出力を持ちます。
   シンプルなクライアントプログラムに適しています。
   もっと高レベルな場合には :class:`asynchat.async_chat` を利用してください。

.. class:: file_dispatcher()

   file_dispatcher はファイルディスクリプタかファイルオブジェクトとオプションとして
   map を引数にとって、 :c:func:`poll` か :c:func:`loop` 関数で利用できるようにラップします。
   与えられたファイルオブジェクトなどが :c:func:`fileno` メソッドを持っているとき、
   そのメソッドが呼び出されて戻り値が :class:`file_wrapper` のコンストラクタに\
   渡されます。
   利用できるプラットフォーム: UNIX

.. class:: file_wrapper()

   file_wrapper は整数のファイルディスクリプタを受け取って :func:`os.dup`
   を呼び出してハンドルを複製するので、元のハンドルは file_wrapper と独立して\
   close されます。
   このクラスは :class:`file_dispatcher` クラスが使うために必要なソケットを\
   エミュレートするメソッドを実装しています。
   利用できるプラットフォーム: UNIX


.. _asyncore-example-1:

asyncoreの例：簡単なHTTPクライアント
------------------------------------

基本的なサンプルとして、以下に非常に単純なHTTPクライアントを示します。こ
のHTTPクライアントは :class:`dispatcher` クラスでソケットを利用しています。 ::

   import asyncore, socket

   class HTTPClient(asyncore.dispatcher):

       def __init__(self, host, path):
           asyncore.dispatcher.__init__(self)
           self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
           self.connect( (host, 80) )
           self.buffer = 'GET %s HTTP/1.0\r\n\r\n' % path

       def handle_connect(self):
           pass

       def handle_close(self):
           self.close()

       def handle_read(self):
           print self.recv(8192)

       def writable(self):
           return (len(self.buffer) > 0)

       def handle_write(self):
           sent = self.send(self.buffer)
           self.buffer = self.buffer[sent:]

   client = HTTPClient('www.python.org', '/')
   asyncore.loop()

.. _asyncore-example-2:

基本的な echo サーバーの例
----------------------------------

この例の基本的な echoサーバーは、 :class:`dispatcher` を利用して接続を受けつけ、
接続をハンドラーにディスパッチします。 ::

    import asyncore
    import socket

    class EchoHandler(asyncore.dispatcher_with_send):

        def handle_read(self):
            data = self.recv(8192)
            if data:
                self.send(data)

    class EchoServer(asyncore.dispatcher):

        def __init__(self, host, port):
            asyncore.dispatcher.__init__(self)
            self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
            self.set_reuse_addr()
            self.bind((host, port))
            self.listen(5)

        def handle_accept(self):
            pair = self.accept()
            if pair is None:
                pass
            else:
                sock, addr = pair
                print 'Incoming connection from %s' % repr(addr)
                handler = EchoHandler(sock)

    server = EchoServer('localhost', 8080)
    asyncore.loop()

