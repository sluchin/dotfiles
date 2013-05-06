:mod:`CGIHTTPServer` ---  CGI 実行機能付き HTTP リクエスト処理機構
==================================================================

.. module:: CGIHTTPServer
   :synopsis: CGI スクリプトの実行機能を持つ HTTP サーバのためのリクエスト処理機構を提供します。
.. sectionauthor:: Moshe Zadka <moshez@zadka.site.co.il>

.. note::
   :mod:`BaseHTTPServer` モジュールは Python 3.0 では :mod:`http.server` に統合されました。
   ソースコードを 3.0 用に変換する時は、 :term:`2to3` ツールが自動的に import を修正します。



:mod:`CGIHTTPServer` モジュールでは、 :class:`BaseHTTPServer.BaseHTTPRequestHandler`
互換のインタフェースを持ち、 :class:`SimpleHTTPServer.SimpleHTTPRequestHandler` の動作を継承していますが
CGI スクリプトを動作することもできる、 HTTP 要求処理機構クラスを定義しています。

.. note::

   このモジュールは CGI スクリプトを Unix および Windows システム上で実行させることができます。

.. note::

   :class:`CGIHTTPRequestHandler` クラスで実行されるCGIスクリプトは HTTP コード200
   (スクリプトの出力が後に続く)を実行に先立って出力される (これがステータスコードになります)
   ため、リダイレクト(コード302)を行なうことができません。

:mod:`CGIHTTPServer` モジュールでは、以下のクラスを定義しています:


.. class:: CGIHTTPRequestHandler(request, client_address, server)

   このクラスは、現在のディレクトリかその下のディレクトリにおいて、ファイルか CGI
   スクリプト出力を提供するために使われます。
   HTTP 階層構造からローカルなディレクトリ構造への対応付けは
   :class:`SimpleHTTPServer.SimpleHTTPRequestHandler` と全く同じなので注意してください。

   このクラスでは、ファイルが CGI スクリプトであると推測された場合、
   これをファイルして提供する代わりにスクリプトを実行します。
   他の一般的なサーバ設定は特殊な拡張子を使って CGI スクリプトであることを示すのに対し、
   ディレクトリベースの CGI だけが使われます。

   :func:`do_GET` および :func:`do_HEAD` 関数は、HTTP 要求が ``cgi_directories``
   パス以下のどこかを指している場合、ファイルを提供するのではなく、CGI
   スクリプトを実行してその出力を提供するように変更されています。

    :class:`CGIHTTPRequestHandler` では以下のデータメンバを定義しています:


   .. attribute:: cgi_directories

      この値は標準で ``['/cgi-bin', '/htbin']`` であり、CGI
      スクリプトを含んでいることを示すディレクトリを記述します。

   :class:`CGIHTTPRequestHandler` では以下のメソッドを定義しています:


   .. method:: do_POST()

      このメソッドは、CGI スクリプトでのみ許されている ``'POST'``
      型の HTTP 要求に対するサービスを行います。 CGI でない url に対して
      POST を試みた場合、出力は Error 501, "Can only POST to CGI scripts" になります。

セキュリティ上の理由から、CGI スクリプトはユーザ nobody の UID で動作するので注意してください。
CGI スクリプトが原因で発生した問題は、Error 403 に変換されます。

使用例については、 :func:`test` 関数の実装を参照してください。


.. seealso::

   Module :mod:`BaseHTTPServer`
      Web サーバとリクエスト処理機構を実装した基底クラスです。

