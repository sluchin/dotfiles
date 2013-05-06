:mod:`DocXMLRPCServer` --- セルフ-ドキュメンティング XML-RPC サーバ
===================================================================

.. module:: DocXMLRPCServer
   :synopsis: セルフ-ドキュメンティング XML-RPC サーバの実装。
.. moduleauthor:: Brian Quinlan <brianq@activestate.com>
.. sectionauthor:: Brian Quinlan <brianq@activestate.com>

.. .. note::
   The :mod:`DocXMLRPCServer` module has been merged into :mod:`xmlrpc.server`
   in Python 3.0.  The :term:`2to3` tool will automatically adapt imports when
   converting your sources to 3.0.

:mod:`DocXMLRPCServer` モジュールは、Python 3では :mod:`xmlrpc.server` モジュールに統合されました。
:term:`2to3` ツールは、ソースコード内のimportを自動的にPython 3用に修正します。

.. versionadded:: 2.3

:mod:`DocXMLRPCServer` モジュールは :mod:`SimpleXMLRPCServer` クラスを拡張し、HTTP GET
リクエストに対し HTML ドキュメントを返します。サーバは :class:`DocXMLRPCServer` を使ったスタンドアロン環境、
:class:`DocCGIXMLRPCRequestHandler` を使った CGI 環境の2つがあります。


.. class:: DocXMLRPCServer(addr[, requestHandler[, logRequests[, allow_none[,  encoding[, bind_and_activate]]]]])

   当たなサーバ・インスタンスを生成します。各パラメータの内容は :class:`SimpleXMLRPCServer.SimpleXMLRPCServer`
   のものと同じですが、 *requestHandler* のデフォルトは :class:`DocXMLRPCRequestHandler` になっています。


.. class:: DocCGIXMLRPCRequestHandler()

   CGI環境に XMR-RPC リクエスト・ハンドラの新たなインスタンスを生成します。


.. class:: DocXMLRPCRequestHandler()

   リクエスト・ハンドラの新たなインスタンスを生成します。このリクエスト・ハンドラは XML-RPC POST リクエスト、ドキュメントの GET、そして
   :class:`DocXMLRPCServer` コンストラクタに与えられた *logRequests*
   パラメータ設定を優先するため、ロギングの変更をサポートします。


.. _doc-xmlrpc-servers:

DocXMLRPCServer オブジェクト
----------------------------

:class:`DocXMLRPCServer` は :class:`SimpleXMLRPCServer.SimpleXMLRPCServer`
の派生クラスで、セルフ-  ドキュメンティングの手段と XML-RPC サーバ機能を提供します。HTTP POST  リクエストは XML-RPC
メソッドの呼び出しとして扱われます。HTTP GET リクエストは pydoc スタイルの HTML ドキュメント生成のリクエストとして扱わ
れます。これはサーバが自分自身のドキュメントを Web ベースで提供可能であることを意味します。


.. method:: DocXMLRPCServer.set_server_title(server_title)

   生成する HTML ドキュメントのタイトルをセットします。このタイトルは HTML の title 要素として使われます。


.. method:: DocXMLRPCServer.set_server_name(server_name)

   生成する HTML ドキュメントの名前をセットします。この名前は HTML 冒頭の h1 要素に使われます。


.. method:: DocXMLRPCServer.set_server_documentation(server_documentation)

   生成する HTML ドキュメントの本文をセットします。この本文はドキュメント中の名前の下にパラグラフとして出力されます。


DocCGIXMLRPCRequestHandler
--------------------------

:class:`DocCGIXMLRPCRequestHandler` は
:class:`SimpleXMLRPCServer.CGIXMLRPCRequestHandler` の派生クラスで、セルフ- ドキュメンティングの手段と
XML-RPC CGI スクリプト機能を提供します。HTTP POST リクエストは XML-RCP メソッドの呼び出しとして扱われます。 HTTP GET
リクエストは pydoc スタイルの HTML ドキュメント生成のリクエストとして扱われます。これはサーバが自分自身のドキュメントを Web ベース
で提供可能であることを意味します。


.. method:: DocCGIXMLRPCRequestHandler.set_server_title(server_title)

   生成する HTML ドキュメントのタイトルをセットします。このタイトルは HTML の title 要素として使われます。


.. method:: DocCGIXMLRPCRequestHandler.set_server_name(server_name)

   生成する HTML ドキュメントの名前をセットします。この名前は HTML 冒頭の h1 要素に使われます。


.. method:: DocCGIXMLRPCRequestHandler.set_server_documentation(server_documentation)

   生成する HTML ドキュメントの本文をセットします。この本文はドキュメント中の名前の下にパラグラフとして出力されます。

