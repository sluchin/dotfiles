:mod:`SimpleHTTPServer` --- 簡潔な HTTP リクエストハンドラ
==========================================================

.. module:: SimpleHTTPServer
   :synopsis: このモジュールは HTTP サーバに基本的なリクエストハンドラを提供します。
.. sectionauthor:: Moshe Zadka <moshez@zadka.site.co.il>

.. .. note::
   The :mod:`SimpleHTTPServer` module has been merged into :mod:`http.server` in
   Python 3.0.  The :term:`2to3` tool will automatically adapt imports when
   converting your sources to 3.0.

.. note::
   :mod:`SimpleHTTPServer` モジュールは、Python 3では :mod:`http.server` にリネームされました。
   :term:`2to3` ツールが、ソースコード内のimportを自動的にPython 3用に修正します。

.. The :mod:`SimpleHTTPServer` module defines a single class,
   :class:`SimpleHTTPRequestHandler`, which is interface-compatible with
   :class:`BaseHTTPServer.BaseHTTPRequestHandler`.

:mod:`SimpleHTTPServer` モジュールは、 :class:`SimpleHTTPRequestHandler` クラス1つを提供しています。
このクラスは、 :class:`BaseHTTPServer.BaseHTTPRequestHandler` に対して互換性のあるインタフェースを持っています。

:mod:`SimpleHTTPServer` モジュールでは以下のクラスを定義しています:


.. class:: SimpleHTTPRequestHandler(request, client_address, server)

   このクラスは、現在のディレクトリ以下にあるファイルを、HTTP リクエストにおけるディレクトリ構造に直接対応付けて提供します。

   リクエストの解釈のような、多くの作業は基底クラス :class:`BaseHTTPServer.BaseHTTPRequestHandler` で行われます。
   このクラスは関数 :func:`do_GET` および :func:`do_HEAD`  を実装しています。

   :class:`SimpleHTTPRequestHandler` では以下のメンバ変数を定義しています:


   .. attribute:: server_version

      この値は ``"SimpleHTTP/" + __version__`` になります。 ``__version__`` はこのモジュールで定義されている値です。


   .. attribute:: extensions_map

      拡張子を MIME 型指定子に対応付ける辞書です。標準の型指定は空文字列で表され、この値は ``application/octet-stream``
      と見なされます。対応付けは大小文字の区別をするので、小文字のキーのみを入れるべきです。

   :class:`SimpleHTTPRequestHandler` では以下のメソッドを定義しています:


   .. method:: do_HEAD()

      このメソッドは ``'HEAD'`` 型のリクエスト処理を実行します: すなわち、 ``GET`` リクエストの時に送信されるものと同じヘッダを送信します。
      送信される可能性のあるヘッダについての完全な説明は :meth:`do_GET`  メソッドを参照してください。


   .. method:: do_GET()

      リクエストを現在の作業ディレクトリからの相対的なパスとして解釈することで、リクエストをローカルシステム上のファイルと対応付けます。

      リクエストがディレクトリに対応付けられた場合、 ``index.html`` または ``index.htm`` をこの順序でチェックします。
      もしファイルを発見できればその内容を、そうでなければディレクトリ一覧を :meth:`list_directory` メソッドで生成して、返します。
      このメソッドは :func:`os.listdir` をディレクトリのスキャンに用いており、 :func:`listdir` が失敗した場合には ``404`` 応答
      が返されます。

      リクエストがファイルに対応付けられた場合、そのファイルを開いて内容を返します。要求されたファイルを開く際に何らかの :exc:`IOError` 例外
      が送出された場合、リクエストは ``404`` 、 ``'File not found'``  エラーに対応づけられます。そうでない場合、コンテントタイプが
      *extensions_map* 変数を用いて推測されます。

      出力は ``'Content-type:'`` と推測されたコンテントタイプで、その後にファイルサイズを示す ``'Content-Lenght;'``
      ヘッダと、ファイルの更新日時を示す ``'Last-Modified:'`` ヘッダが続きます。

      そしてヘッダの終了を示す空白行が続き、さらにその後にファイルの内容が続きます。このファイルはコンテントタイプが ``text/`` で始まっている場合
      はテキストモードで、そうでなければバイナリモードで開かれます。

      :mod:`SimpleHTTPServer` モジュールの :func:`test` 関数は
      :class:`SimpleHTTPRequestHandler` をハンドラとして使うサーバを作る例になっています。

      .. versionadded:: 2.5
         ``'Last-Modified'`` ヘッダ.

:mod:`SimpleHTTPServer` モジュールを使って現在のディレクトリ以下にあるファイルに
アクセスできるだけの、非常に初歩的な Web サーバを立ち上げる方法は以下の通りです。 ::

   import SimpleHTTPServer
   import SocketServer
 
   PORT = 8000
 
   Handler = SimpleHTTPServer.SimpleHTTPRequestHandler
 
   httpd = SocketServer.TCPServer(("", PORT), Handler)
 
   print "serving at port", PORT
   httpd.serve_forever()

インタプリタの ``-m`` スイッチで :mod:`SimpleHTTPServer` モジュールと ``ポート番号``
を指定して直接実行することもできます。
上の例と同じように、ここで立ち上がったサーバは現在のディレクトリ以下のファイルへの
アクセスを提供します。 ::

     python -m SimpleHTTPServer 8000

.. seealso::

   Module :mod:`BaseHTTPServer`
      Web サーバおよび要求ハンドラの基底クラス実装。

