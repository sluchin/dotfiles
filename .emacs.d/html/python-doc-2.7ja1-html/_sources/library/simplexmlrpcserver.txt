:mod:`SimpleXMLRPCServer` --- 基本的なXML-RPCサーバー
=====================================================

.. module:: SimpleXMLRPCServer
   :synopsis: 基本的なXML-RPCサーバーの実装。
.. moduleauthor:: Brian Quinlan <brianq@activestate.com>
.. sectionauthor:: Fred L. Drake, Jr. <fdrake@acm.org>

.. .. note::
   The :mod:`SimpleXMLRPCServer` module has been merged into
   :mod:`xmlrpc.server` in Python 3.0.  The :term:`2to3` tool will automatically
   adapt imports when converting your sources to 3.0.

.. note::
   :mod:`SimpleXMLRPCServer` モジュールは、Python 3では :mod:`xmlrpc.server`
   モジュールに統合されました。
   :term:`2to3` ツールが自動的にソースコード内のimportを修正します。

.. versionadded:: 2.2

:mod:`SimpleXMLRPCServer` モジュールはPythonで記述された基本的なXML-RPC
サーバーフレームワークを提供します。サーバーはスタンドアロンであるか、 :class:`SimpleXMLRPCServer`
を使うか、 :class:`CGIXMLRPCRequestHandler` を使って CGI 環境に組み込まれるかの、いずれかです。


.. class:: SimpleXMLRPCServer(addr[, requestHandler[, logRequests[, allow_none[, encoding[, bind_and_activate]]]]])

   新しくサーバーインスタンスを作成します。このクラスはXML-RPCプロトコルで呼ばれる関数の登録のためのメソッドを提供します。
   引数 *requestHandler* には、リクエストハンドラーインスタンスのファクトリーを設定します。デフォルトは :class:`SimpleXMLRPCRequestHandler` です。引数 *addr* と *requestHandler* は :class:`SocketServer.TCPServer` のコンストラクターに引き渡されます。もし引数 *logRequests* が真(true)であれば、(それがデフォルトですが、)リクエストはログに記録されます。偽(false)である場合にはログは記録されません。
   引数 *allow_none* と *encoding* は :mod:`xmlrpclib` に引き継がれ、サーバーから返されるXML-RPCレスポンスを制御します。
   *bind_and_activate* 引数は、コンストラクタの呼び出し直後に :meth:`server_bind` と :meth:`server_activate`
   を呼ぶかどうかを指定します。デフォルトでは ``True`` です。この引数に ``False``
   を指定することで、バインドする前に、 *allow_reuse_address* クラス変数を操作することができます。
   (訳注: 同じ名前のインスタンス変数を追加することで、クラス変数をオーバーライドすることができます。)

   .. versionchanged:: 2.5
      引数 *allow_none* と *encoding* が追加されました.

   .. versionchanged:: 2.6
      *bind_and_activate* 引数が追加されました。


.. class:: CGIXMLRPCRequestHandler([allow_none[, encoding]])

   CGI 環境における XML-RPC リクエストハンドラーを、新たに作成します。
   引数 *allow_none* と *encoding* は :mod:`xmlrpclib` に引き継がれ、サーバーから返されるXML-RPCレスポンスを制御します。

   .. versionadded:: 2.3

   .. versionchanged:: 2.5
      引数 *allow_none* と *encoding* が追加されました.


.. class:: SimpleXMLRPCRequestHandler()

   新しくリクエストハンドラーインスタンスを作成します。このリクエストハンドラーは ``POST`` リクエストを受け持ち、 :class:`SimpleXMLRPCServer` のコンストラクターの引数 *logRequests* に従ったログ出力を行います。


.. _simple-xmlrpc-servers:

SimpleXMLRPCServer オブジェクト
-------------------------------

:class:`SimpleXMLRPCServer` クラスは :class:`SocketServer.TCPServer`
のサブクラスで、基本的なスタンドアロンの XML-RPC サーバーを作成する手段を提供します。


.. method:: SimpleXMLRPCServer.register_function(function[, name])

   XML-
   RPCリクエストに応じる関数を登録します。引数 *name* が与えられている場合はその値が、関数 *function* に関連付けられます。これが与えられない場合は ``function.__name__`` の値が用いられます。引数 *name* は通常の文字列でもユニコード文字列でも良く、Pythonで識別子として正しくない文字("
   . "ピリオドなど )を含んでいても。


.. method:: SimpleXMLRPCServer.register_instance(instance[, allow_dotted_names])

   オブジェクトを登録し、そのオブジェクトの :meth:`register_function` で
   登録されていないメソッドを公開します。もし、 *instance* がメソッド
   :meth:`_dispatch` を定義していれば、 :meth:`_dispatch` が、リクエス
   トされたメソッド名とパラメータの組を引数として呼び出されます。そして、 :meth:`_dispatch` の返り値が結果としてクライアントに返されます。
   そのAPIは  ``def _dispatch(self, method, params)`` (注意:
   *params* は可変引数リストではありません)です。仕事をするために下位の関数を呼ぶ時には、その関数は ``func(*params)`` のように呼ばれ
   ます。 :meth:`_dispatch` の返り値はクライアントへ結果として返されます。もし、
   *instance* がメソッド :meth:`_dispatch` を定義していなければ、リク
   エストされたメソッド名がそのインスタンスに定義されているメソッド名から探されます。

   もしオプション引数 *allow_dotted_names* が真(true)で、インスタンスがメソッド :meth:`_dispatch` を定義していないとき、
   リクエストされたメソッド名がピリオドを含む場合は、（訳注：通常のPythonでのピリオドの解釈と同様に）階層的にオブジェクトを探索し
   ます。そして、そこで見つかったオブジェクトをリクエストから渡された引数で呼び出し、その返り値をクライアントに返します。

   .. warning::

      *allow_dotted_names* オプションを有効にすると、侵入者にあなたのモジュールの
      グローバル変数にアクセスすることを許し、あなたのコンピュータで任意のコードを実行する
      ことを許すことがあります。このオプションは安全な閉じたネットワークでのみお使い下さい。

   .. versionchanged:: 2.3.5, 2.4.1
      *allow_dotted_names* はセキュリティホールを塞ぐために追加されました。以前のバージョンは安全ではありません.


.. method:: SimpleXMLRPCServer.register_introspection_functions()

   XML-RPC
   のイントロスペクション関数、 ``system.listMethods`` 、 ``system.methodHelp`` 、 ``system.methodSignature``
   を登録します。

   .. versionadded:: 2.3

   .. % --


.. method:: SimpleXMLRPCServer.register_multicall_functions()

   XML-RPC における複数の要求を処理する関数 system.multicall を登録します。


.. attribute:: SimpleXMLRPCServer.rpc_paths

   この属性値はXML-RPCリクエストを受け付けるURLの正当なパス部分をリストするタプルで
   なければなりません。これ以外のパスへのリクエストは404「そのようなページはありません」
   HTTPエラーになります。このタプルが空の場合は全てのパスが正当であると見なされます。デフォルト値は ``('/', '/RPC2')`` です。

   .. versionadded:: 2.5

.. attribute:: SimpleXMLRPCRequestHandler.encode_threshold

   この属性が ``None`` でない場合、クライアントが受け付けるのであれば、
   この値よりも大きいサイズのレスポンスを *gzip* transfer encoding
   を利用して圧縮されます、
   デフォルト値は、だいたいTCPの1パケットに収まるように ``1400`` です。

   .. versionadded:: 2.7

.. _simplexmlrpcserver-example:

SimpleXMLRPCServer の例
^^^^^^^^^^^^^^^^^^^^^^^^^^
サーバーのコード::

   from SimpleXMLRPCServer import SimpleXMLRPCServer
   from SimpleXMLRPCServer import SimpleXMLRPCRequestHandler

   # Restrict to a particular path.
   class RequestHandler(SimpleXMLRPCRequestHandler):
       rpc_paths = ('/RPC2',)

   # Create server
   server = SimpleXMLRPCServer(("localhost", 8000),
                               requestHandler=RequestHandler)
   server.register_introspection_functions()

   # Register pow() function; this will use the value of
   # pow.__name__ as the name, which is just 'pow'.
   server.register_function(pow)

   # Register a function under a different name
   def adder_function(x,y):
       return x + y
   server.register_function(adder_function, 'add')

   # Register an instance; all the methods of the instance are
   # published as XML-RPC methods (in this case, just 'div').
   class MyFuncs:
       def div(self, x, y):
           return x // y

   server.register_instance(MyFuncs())

   # Run the server's main loop
   server.serve_forever()

以下のクライアントコードは上のサーバーで使えるようになったメソッドを呼び出します::

   import xmlrpclib

   s = xmlrpclib.ServerProxy('http://localhost:8000')
   print s.pow(2,3)  # Returns 2**3 = 8
   print s.add(2,3)  # Returns 5
   print s.div(5,2)  # Returns 5//2 = 2

   # Print list of available methods
   print s.system.listMethods()


CGIXMLRPCRequestHandler
-----------------------

:class:`CGIXMLRPCRequestHandler` クラスは、Python の CGI スクリプトに送られた XML-RPC
リクエストを処理するときに使用できます


.. method:: CGIXMLRPCRequestHandler.register_function(function[, name])

   XML-RPC リクエストに応じる関数を登録します。
   引数 *name* が与えられている場合はその値が、関数 *function* に関連付けられます。これが与えられない場合は ``function.__name__`` の値が用いられます。引数 *name* は通常の文字列でもユニコード文字列でも良く、Pythonで識別子として正しくない文字("
   . "ピリオドなど )を含んでもかまいません。


.. method:: CGIXMLRPCRequestHandler.register_instance(instance)

   オブジェクトを登録し、そのオブジェクトの :meth:`register_function` で登録されていないメソッドを公開します。もし、 *instance* がメソッド :meth:`_dispatch` を定義していれば、 :meth:`_dispatch` が、リクエストされたメソッド名とパラメータの組を引数として呼び出されます。そして、 :meth:`_dispatch` の返り値が結果としてクライアントに返されます。もし、 *instance* がメソッド :meth:`_dispatch` を定義していなければ、リクエストされたメソッド名がそのインスタンスに定義されているメソッド名から探されます。リクエストされたメソッド名がピリオドを含む場合は、（訳注：通常のPythonでのピリオドの解釈と同様に）階層的にオブジェクトを探索します。そして、そこで見つかったオブジェクトをリクエストから渡された引数で呼び出し、その返り値をクライアントに返します。

   .. % 原文で、引数名 instance は \var{} で囲まれていませんが、
   .. % SimpleXMLRPCServer.register_instance() の記述に合わせて \var{} で囲ん
   .. % であります。
   .. % 2003-07-25 ふるかわとおる


.. method:: CGIXMLRPCRequestHandler.register_introspection_functions()

   XML-RPC
   のイントロスペクション関数、 ``system.listMethods`` 、 ``system.methodHelp`` 、 ``system.methodSignature``
   を登録します。


.. method:: CGIXMLRPCRequestHandler.register_multicall_functions()

   XML-RPC における複数の要求を処理する関数 system.multicall を登録します。


.. method:: CGIXMLRPCRequestHandler.handle_request([request_text = None])

   XML-RPC リクエストを処理します。 *request_text* で渡されるのは、HTTP サーバーに提供された POST
   データです。何も渡されなければ標準入力からのデータが使われます。

以下に例を示します。 ::

   class MyFuncs:
       def div(self, x, y) : return x // y


   handler = CGIXMLRPCRequestHandler()
   handler.register_function(pow)
   handler.register_function(lambda x,y: x+y, 'add')
   handler.register_introspection_functions()
   handler.register_instance(MyFuncs())
   handler.handle_request()

