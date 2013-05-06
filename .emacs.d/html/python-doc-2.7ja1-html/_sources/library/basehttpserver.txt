:mod:`BaseHTTPServer` --- 基本的な機能を持つ HTTP サーバ
========================================================

.. module:: BaseHTTPServer
   :synopsis: 基本的な機能を持つ HTTP サーバ  (SimpleHTTPServer および CGIHTTPServer の基底クラス)。

.. note::
   :mod:`BaseHTTPServer` モジュールは Python 3.0 では :mod:`http.server` に統合されました。
   ソースコードを 3.0 用に変換する時は、 :term:`2to3` ツールが自動的に import を修正します。


.. index::
   pair: WWW; server
   pair: HTTP; protocol
   single: URL
   single: httpd
   module: SimpleHTTPServer
   module: CGIHTTPServer

このモジュールでは、 HTTP サーバ (Web サーバ) を実装するための二つののクラスを定義しています。
通常、このモジュールが直接使用されることはなく、特定の機能を持つ Web サーバを構築するために使われます。
:mod:`SimpleHTTPServer` および :mod:`CGIHTTPServer` モジュールを参照してください。

最初のクラス, :class:`HTTPServer` は :class:`SocketServer.TCPServer`
のサブクラスで、従って :class:`SocketServer.BaseServer` インタフェースを実装しています。
:class:`HTTPServer` は HTTP ソケットを生成してリクエスト待ち (listen)
を行い、リクエストをハンドラに渡します。サーバを作成して動作させるためのコードは以下のようになります::

   def run(server_class=BaseHTTPServer.HTTPServer,
           handler_class=BaseHTTPServer.BaseHTTPRequestHandler):
       server_address = ('', 8000)
       httpd = server_class(server_address, handler_class)
       httpd.serve_forever()


.. class:: HTTPServer(server_address, RequestHandlerClass)

   このクラスは :class:`TCPServer` 型のクラスの上に構築されており、
   サーバのアドレスをインスタンス変数 :attr:`server_name`
   および :attr:`server_port` に記憶します。サーバはハンドラからアクセス可能で、通常 :attr:`server`
   インスタンス変数でアクセスします。


.. class:: BaseHTTPRequestHandler(request, client_address, server)

   このクラスはサーバに到着したリクエストを処理します。
   このメソッド自体では、実際のリクエストに応答することはできません; (GET や POST のような)
   各リクエストメソッドを処理するためにはサブクラス化しなければなりません。
   :class:`BaseHTTPRequestHandler` では、サブクラスで使うためのクラスやインスタンス変数、
   メソッド群を数多く提供しています。

   このハンドラはリクエストを解釈し、次いでリクエスト形式ごとに固有のメソッドを呼び出します。
   メソッド名はリクエストの名称から構成されます。
   例えば、リクエストメソッド ``SPAM`` に対しては、 :meth:`do_SPAM` メソッドが引数なしで呼び出されます。
   リクエストに関連する情報は全て、ハンドラのインスタンス変数に記憶されています。
   サブクラスでは :meth:`__init__` メソッドを上書きしたり拡張したりする必要はありません。

:class:`BaseHTTPRequestHandler` は以下のインスタンス変数を持っています:


   .. attribute:: client_address

      HTTP クライアントのアドレスを参照している、 ``(host, port)`` の形式をとるタプルが入っています。

   .. attribute:: server

      server インスタンスが入っています。


   .. attribute:: command

      HTTP 命令 (リクエスト形式) が入っています。例えば ``'GET'`` です。


   .. attribute:: path

      リクエストされたパスが入っています。


   .. attribute:: request_version

      リクエストのバージョン文字列が入っています。例えば ``'HTTP/1.0'`` です。


   .. attribute:: headers

      :attr:`MessageClass` クラス変数で指定されたクラスのインスタンスを保持しています。
      このインスタンスは HTTP リクエストのヘッダを解釈し、管理しています。


   .. attribute:: rfile

      入力ストリームが入っており、そのファイルポインタはオプション入力データ部の先頭を指しています。


   .. attribute:: wfile

      クライアントに返送する応答を書き込むための出力ストリームが入っています。
      このストリームに書き込む際には、HTTP プロトコルに従った形式をとらなければなりません。

   :class:`BaseHTTPRequestHandler` は以下のクラス変数を持っています:


   .. attribute:: server_version

      サーバのソフトウェアバージョンを指定します。
      この値は上書きする必要が生じるかもしれません。
      書式は複数の文字列を空白で分割したもので、各文字列はソフトウェア名[/バージョン] の形式をとります。
      例えば、 ``'BaseHTTP/0.2'`` です。


   .. attribute:: sys_version

      Python 処理系のバージョンが, :attr:`version_string` メソッドや :attr:`server_version`
      クラス変数で利用可能な形式で入っています。例えば ``'Python/1.4'`` です。


   .. attribute:: error_message_format

      クライアントに返すエラー応答を構築するための書式化文字列を指定します。この文字列は丸括弧で囲ったキー文字列で指定する形式を
      使うので、書式化の対象となる値は辞書でなければなりません。キー *code* は整数で、HTTP エラーコードを特定する数値です。 *message*
      は文字列で、何が発生したかを表す (詳細な)  エラーメッセージが入ります。 *explain* はエラーコード番号の説明です。 *message* および
      *explain* の標準の値は *response* クラス変数でみつけることができます。

   .. attribute:: error_content_type

      エラーレスポンスをクライアントに送信する時に使う Content-Type HTTP ヘッダを指定します。
      デフォルトでは ``'text/html'`` です。

      .. versionadded:: 2.6
         以前は、 Content-Type は常に ``'text/html'`` でした。


   .. attribute:: protocol_version

      この値には応答に使われる HTTP プロトコルのバージョンを指定します。
      ``'HTTP/1.1'`` に設定されると、サーバは持続的 HTTP 接続を許可します;
      しかしその場合、サーバは全てのクライアントに対する応答に、正確な値を持つ
      ``Content-Length`` ヘッダを (:meth:`send_header` を使って) 含め *なければなりません* 。
      以前のバージョンとの互換性を保つため、標準の設定値は ``'HTTP/1.0'`` です。


   .. attribute:: MessageClass

      .. index:: single: Message (in module mimetools)

      HTTP ヘッダを解釈するための :class:`rfc822.Message` 類似のクラスを指定します。
      通常この値が上書きされることはなく、標準の値 :class:`mimetools.Message` になっています。


   .. attribute:: responses

      この変数はエラーコードを表す整数を二つの要素をもつタプルに対応付けます。
      タプルには短いメッセージと長いメッセージが入っています。
      例えば、 ``{code: (shortmessage, longmessage)}`` といったようになります。
      *shortmessage* は通常、エラー応答における *message* キーの値として使われ、
      *longmessage* は *explain* キーの値として使われます
      (:attr:`error_message_format` クラス変数を参照してください) 。

   :class:`BaseHTTPRequestHandler` インスタンスは以下のメソッドを持っています:


   .. method:: handle()

      :meth:`handle_one_request` を一度だけ
      (持続的接続が有効になっている場合には複数回) 呼び出して、HTTPリクエストを処理します。
      このメソッドを上書きする必要はまったくありません; そうする代わりに適切な :meth:`do_\*` を実装してください。


   .. method:: handle_one_request()

      このメソッドはリクエストを解釈し、適切な :meth:`do_\*` メソッドに転送します。
      このメソッドを上書きする必要はまったくありません。


   .. method:: send_error(code[, message])

      完全なエラー応答をクライアントに送信し、ログ記録します。 *code* は数値型で、HTTP エラーコードを指定します。
      *message* はオプションで、より詳細なメッセージテキストです。
      完全なヘッダのセットが送信された後, :attr:`error_message_format` クラス変数を使って組み立てられたテキストが送られます。


   .. method:: send_response(code[, message])

      応答ヘッダを送信し、受理したリクエストをログ記録します。HTTP 応答行が送られた後、 *Server* および *Date* ヘッダが
      送られます。これら二つのヘッダはそれぞれ :meth:`version_string`  および :meth:`date_time_string`
      メソッドで取り出します。


   .. method:: send_header(keyword, value)

      出力ストリームに特定の HTTP ヘッダを書き込みます。
      *keyword* はヘッダのキーワードを指定し、 *value* にはその値を指定します。


   .. method:: end_headers()

      応答中の HTTP ヘッダの終了を示す空行を送信します。


   .. method:: log_request([code[, size]])

      受理された (成功した) リクエストをログに記録します。
      *code* にはこの応答に関連付けられた HTTP コード番号を指定します。
      応答メッセージの大きさを知ることができる場合、 *size* パラメタに渡すとよいでしょう。


   .. method:: log_error(...)

      リクエストを遂行できなかった際に、エラーをログに記録します。
      標準では、メッセージを :meth:`log_message` に渡します。従って同じ引数
      (*format* と追加の値) を取ります。


   .. method:: log_message(format, ...)

      任意のメッセージを ``sys.stderr`` にログ記録します。このメソッドは通常、カスタムのエラーログ記録機構を作成するために
      上書きされます。 *format* 引数は標準の printf 形式の書式化文字列で, :meth:`log_message` に渡された追加の引数は
      書式化の入力として適用されます。ログ記録される全てのメッセージには、クライアントのアドレスおよび現在の日付、時刻が先頭に付けられます。


   .. method:: version_string()

      サーバソフトウェアのバージョン文字列を返します。この文字列はクラス変数 :attr:`server_version` および
      :attr:`sys_version`  を組み合わせたものです。


   .. method:: date_time_string([timestamp])

      メッセージヘッダ向けに書式化された、 *timestamp* (:func:`time.time` のフォーマットである必要があります)で与えられた日時を返します。
      もし *timestamp* が省略された場合には、現在の日時が使われます。

      出力は ``'Sun, 06 Nov 1994 08:49:37 GMT'`` のようになります。

      .. versionadded:: 2.5
         *timestamp* パラメータ.


   .. method:: log_date_time_string()

      ログ記録向けに書式化された、現在の日付および時刻を返します。


   .. method:: address_string()

      ログ記録向けに書式化された、クライアントのアドレスを返します。このときクライアントの IP アドレスに対する名前解決を行います。


他の例
-------

永遠ではなく、何かの条件が満たされるまでの間実行するサーバーを作るには::

   def run_while_true(server_class=BaseHTTPServer.HTTPServer,
                      handler_class=BaseHTTPServer.BaseHTTPRequestHandler):
       """
       This assumes that keep_running() is a function of no arguments which
       is tested initially and after each request.  If its return value
       is true, the server continues.
       """
       server_address = ('', 8000)
       httpd = server_class(server_address, handler_class)
       while keep_running():
           httpd.handle_request()


.. seealso::

   Module :mod:`CGIHTTPServer`
      CGI スクリプトをサポートするように拡張されたリクエストハンドラ。

   Module :mod:`SimpleHTTPServer`
      ドキュメントルートの下にあるファイルに対する要求への応答のみに制限した基本リクエストハンドラ。

