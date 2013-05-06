
:mod:`asynchat` --- 非同期ソケットコマンド/レスポンスハンドラ
===============================================================

.. module:: asynchat
   :synopsis: 非同期コマンド/レスポンスプロトコルの開発サポート
.. moduleauthor:: Sam Rushing <rushing@nightmare.com>
.. sectionauthor:: Steve Holden <sholden@holdenweb.com>


:mod:`asynchat` を使うと、 :mod:`asyncore` を基盤とした非同期なサーバ・クライアントをより簡単に開発する事ができます。
:mod:`asynchat` では、プロトコルの要素が任意の文字列で終了するか、または可変長の文字列であるようなプロトコルを容易に制御できるようになってい
ます。 :mod:`asynchat` は、抽象クラス :class:`async_chat` を定義してお
り、 :class:`async_chat` を継承して :meth:`collect_incoming_data` メソッド
と :meth:`found_terminator` メソッドを実装すれば使うことができます。
:class:`async_chat` と :mod:`asyncore` は同じ非同期ループを使用してお
り、 :class:`asyncore.dispatcher` も :class:`asynchat.async_chat` も同じチャネ
ルマップに登録する事ができます。通常、 :class:`asyncore.dispatcher` はサーバチャネルとして使用し、リクエストの受け付け時に
:class:`asynchat.async_chat` オブジェクトを生成します。


.. class:: async_chat()

   このクラスは、 :class:`asyncore.dispatcher` から継承した抽象クラスです。
   使用する際には :class:`async_chat` のサブクラスを作成し、
   :meth:`collect_incoming_data` と :meth:`found_terminator` を定義し
   なければなりません。 :class:`asyncore.dispatcher` のメソッドを使用する事
   もできますが、メッセージ/レスポンス処理を中心に行う場合には使えないメソッドもあります。

   :class:`asyncore.dispatcher` と同様に、 :class:`async_chat` も
   :c:func:`select` 呼出し後のソケットの状態からイベントを生成します。ポーリングループ開始後、イベント処理フレームワークが自動的に
   :class:`async_chat` のメソッドを呼び出しますので、プログラマが処理を記述する必要はありません。

   .. Two class attributes can be modified, to improve performance, or possibly
      even to conserve memory.

   パフォーマンスの向上やメモリの節約のために、2つのクラス属性を調整することができます。

   .. data:: ac_in_buffer_size

      .. The asynchronous input buffer size (default ``4096``).

      非同期入力バッファサイズ (デフォルト値: ``4096``)


   .. data:: ac_out_buffer_size

      .. The asynchronous output buffer size (default ``4096``).

      非同期出力バッファサイズ (デフォルト値: ``4096``)

   :class:`asyncore.dispatcher` と違い、 :class:`async_chat` では *producer* の first-in-first-
   outキュー(fifo)を作成する事ができます。producerは :meth:`more` メソッドを必ず持ち、このメソッドで
   チャネル上に送出するデータを返します。producerが枯渇状態 (*i.e.* これ以上のデータを持たない状態)にある場合、
   :meth:`more` は空文字列を返します。この時、 :class:`async_chat` は枯渇
   状態にあるproducerをfifoから除去し、次のproducerが存在すればそのproducerを使用します。fifoにproducerが存在しない場合、
   :meth:`handle_write` は何もしません。リモート端点からの入力の終了や
   重要な中断点を検出する場合は、 :meth:`set_terminator` に記述します。

   :class:`async_chat` のサブクラスでは、入力メソッド
   :meth:`collect_incoming_data` と :meth:`found_terminator` を定義
   し、チャネルが非同期に受信するデータを処理します。これらのメソッドについては後ろで解説します。


.. method:: async_chat.close_when_done()

   producer fifoのトップに ``None`` をプッシュします。このproducerがポップされると、チャネルがクローズします。


.. method:: async_chat.collect_incoming_data(data)

   チャネルが受信した不定長のデータを *data* に指定して呼び出されます。このメソッドは必ずオーバライドする必要があり、デフォルトの実装では、
   :exc:`NotImplementedError` 例外を送出します。


.. method:: async_chat.discard_buffers()

   非常用のメソッドで、全ての入出力バッファとproducer fifoを廃棄します。


.. method:: async_chat.found_terminator()

   入力データストリームが、 :meth:`set_terminator` で指定した終了条件と一
   致した場合に呼び出されます。このメソッドは必ずオーバライドする必要があり、デフォルトの実装では、 :exc:`NotImplementedError`
   例外を送出します。入力データを参照する必要がある場合でも引数としては与えられないため、入力バッファをインスタンス属性として参照しなければなりません。


.. method:: async_chat.get_terminator()

   現在のチャネルの終了条件を返します。


.. method:: async_chat.push(data)

   チャネルの fifo にデータをプッシュして転送します。
   データをチャネルに書き出すために必要なのはこれだけですが、
   データの暗号化やチャンク化などを行う場合には独自の producer
   を使用する事もできます。


.. method:: async_chat.push_with_producer(producer)

   指定したproducerオブジェクトをチャネルのfifoに追加します。これより前にpushされたproducerが全て枯渇した後、チャネルはこのproducer
   から :meth:`more` メソッドでデータを取得し、リモート端点に送信します。


.. method:: async_chat.set_terminator(term)

   チャネルで検出する終了条件を設定します。 ``term`` は入力プロトコルデータの処理方式によって以下の3つの型の何れかを指定します。

   +-----------+------------------------------------------+
   | term      | 説明                                     |
   +===========+==========================================+
   | *string*  | 入力ストリーム中でstringが検出された時、 |
   |           | :meth:`found_terminator` を呼び出します。|
   +-----------+------------------------------------------+
   | *integer* | 指定された文字数が読み込まれた時、       |
   |           | :meth:`found_terminator` を呼び出します。|
   +-----------+------------------------------------------+
   | ``None``  | 永久にデータを読み込みます。             |
   +-----------+------------------------------------------+

   終了条件が成立しても、その後に続くデータは、 :meth:`found_terminator` の呼出し後に再びチャネルを読み込めば取得する事ができます。


asynchat - 補助クラス
---------------------------


.. class:: fifo([list=None])

   アプリケーションからプッシュされ、まだチャネルに書き出されていないデータを保持するための :class:`fifo` 。
   :class:`fifo` は必要になるまでデータと producer を保持するために使われるリストです。
   引数 *list* には、チャネルに出力する producer またはデータを指定する事ができます。


   .. method:: is_empty()

      fifoが空のとき(のみ)に ``True`` を返します。


   .. method:: first()

      fifoに :meth:`push` されたアイテムのうち、最も古いアイテムを返します。


   .. method:: push(data)

      データ(文字列またはproducerオブジェクト)をproducer fifoに追加します。


   .. method:: pop()

      fifoが空でなければ、 ``(True, first())`` を返し、ポップされたアイテムを削除します。
      fifoが空であれば ``(False, None)`` を返します。


.. _asynchat-example:

asynchat 使用例
---------------

以下のサンプルは、 :class:`async_chat` でHTTPリクエストを読み込む処理の一部です。Webサーバは、クライアントからの接続毎に
:class:`http_request_handler` オブジェクトを作成します。最初はチャネルの終
了条件に空行を指定してHTTPヘッダの末尾までを検出し、その後ヘッダ読み込み済みを示すフラグを立てています。

ヘッダ読み込んだ後、リクエストの種類がPOSTであればデータが入力ストリームに流れるため、 ``Content-Length:``
ヘッダの値を数値として終了条件に指定し、適切な長さのデータをチャネルから読み込みます。

必要な入力データを全て入手したら、チャネルの終了条件に ``None`` を指定して残りのデータを無視するようにしています。この後、
:meth:`handle_request` が呼び出されます。 ::

   class http_request_handler(asynchat.async_chat):

       def __init__(self, sock, addr, sessions, log):
           asynchat.async_chat.__init__(self, sock=sock)
           self.addr = addr
           self.sessions = sessions
           self.ibuffer = []
           self.obuffer = ""
           self.set_terminator("\r\n\r\n")
           self.reading_headers = True
           self.handling = False
           self.cgi_data = None
           self.log = log

       def collect_incoming_data(self, data):
           """Buffer the data"""
           self.ibuffer.append(data)

       def found_terminator(self):
           if self.reading_headers:
               self.reading_headers = False
               self.parse_headers("".join(self.ibuffer))
               self.ibuffer = []
               if self.op.upper() == "POST":
                   clen = self.headers.getheader("content-length")
                   self.set_terminator(int(clen))
               else:
                   self.handling = True
                   self.set_terminator(None)
                   self.handle_request()
           elif not self.handling:
               self.set_terminator(None) # browsers sometimes over-send
               self.cgi_data = parse(self.headers, "".join(self.ibuffer))
               self.handling = True
               self.ibuffer = []
               self.handle_request()
