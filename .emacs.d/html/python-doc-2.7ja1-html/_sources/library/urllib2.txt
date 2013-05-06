:mod:`urllib2` --- URL を開くための拡張可能なライブラリ
=======================================================

.. memo (@cocoatomo)

   HTTPErrorProcessor, AbstractHTTPHandler が文書化されてないことについて
   問い合わせ (2011.06.19)

.. module:: urllib2
   :synopsis: URLを開く次世代ライブラリ
.. moduleauthor:: Jeremy Hylton <jhylton@users.sourceforge.net>
.. sectionauthor:: Moshe Zadka <moshez@users.sourceforge.net>


.. note::
   :mod:`urllib2` モジュールは、Python 3.0で :mod:`urllib.request`, :mod:`urllib.error`
   に分割されました。
   :term:`2to3` ツールが自動的にソースコードのimportを修正します。


:mod:`urllib2` モジュールは基本的な認証、暗号化認証、リダイレクション、クッキー、その他の介在する複雑なアクセス環境において (大抵は HTTP
で)  URL を開くための関数とクラスを定義します。

:mod:`urllib2` モジュールでは以下の関数を定義しています:


.. function:: urlopen(url[, data][, timeout])

   URL *url* を開きます。 *url* は文字列でも :class:`Request` オブジェクトでもかまいません。

   .. warning::
      HTTPS のリクエストはサーバーの認証を一切行いません。

   *data* はサーバに送信する追加のデータを示す文字列か、そのようなデータが無ければ
   *None* を指定します。現時点でHTTPリクエストは *data* をサポートする唯一のリクエスト形式です;
   *data* パラメタが指定が指定された場合、HTTP リクエストは GET でなく POST になります。
   *data* は標準的な :mimetype:`application/x-www-form-urlencoded` 形式のバッファでなくてはなりません。
   :func:`urllib.urlencode` 関数はマップ型か2タプルのシーケンスを取り、この形式の文字列を返します。
   urllib2 モジュールは HTTP/1.1 リクエストを `Connection:close` ヘッダ付きで送信します。

   オプションの *timeout* 引数は、接続開始などのブロックする操作におけるタイムアウト時間を秒数で指定します。
   (指定されなかった場合、グローバルのデフォルトタイムアウト時間が利用されます)
   この引数は、 HTTP, HTTPS, FTP 接続でのみ有効です。

   この関数は以下の 2 つのメソッドを持つファイル類似のオブジェクトを返します:

   * :meth:`geturl` --- 取得されたリソースの URL を返します。
     主に、リダイレクトが発生したかどうかを確認するために利用します。

   * :meth:`info` --- 取得されたページのヘッダーなどのメタ情報を、 :class:`mimetools.Message`
     インスタンスとして返します。
     (`Quick Reference to HTTP Headers <http://www.cs.tut.fi/~jkorpela/http.html>`_ を参照してください)

   エラーが発生した場合 :exc:`URLError` を送出します。

   どのハンドラもリクエストを処理しなかった場合には ``None`` を返すことがあるので注意してください (デフォルトでインストールされる
   グローバルハンドラの :class:`OpenerDirector` は、 :class:`UnknownHandler`
   を使って上記の問題が起きないようにしています)。

   さらに、プロキシが設定されているときは、デフォルトでインストールされる :class:`ProxyHandler` がリクエストを処理するようになっています。

   .. versionchanged:: 2.6
      *timeout* 引数が追加されました。


.. function:: install_opener(opener)

   標準で URL を開くオブジェクトとして :class:`OpenerDirector` のインスタンスをインストールします。このコードは引数が本当に
   :class:`OpenerDirector` のインスタンスであるかどうかはチェックしないので、適切なインタフェースを持ったクラスは何でも動作します。


.. function:: build_opener([handler, ...])

   与えられた順番に URL ハンドラを連鎖させる :class:`OpenerDirector`  のインスタンスを返します。 *handler* は
   :class:`BaseHandler` または :class:`BaseHandler` のサブクラスのインスタンスのどちらかです
   (どちらの場合も、コンストラクトは引数無しで呼び出せるようになっていなければなりません) 。以下のクラス:

   :class:`ProxyHandler`, :class:`UnknownHandler`, :class:`HTTPHandler`,
   :class:`HTTPDefaultErrorHandler`, :class:`HTTPRedirectHandler`,
   :class:`FTPHandler`, :class:`FileHandler`, :class:`HTTPErrorProcessor`

   については、そのクラスのインスタンスか、そのサブクラスのインスタンスが *handler*  に含まれていない限り、 *handler* よりも先に連鎖します。

   Python が SSL をサポートするように設定してインストールされている場合 (すなわち、 :mod:`ssl` モジュールを
   import できる場合) :class:`HTTPSHandler` も追加されます。

   Python 2.3 からは、 :class:`BaseHandler` サブクラスでも  :attr:`handler_order`
   メンバ変数を変更して、ハンドラリスト内での場所を変更できるようになりました。

状況に応じて、以下の例外が送出されます:


.. exception:: URLError

   ハンドラが何らかの問題に遭遇した場合、この例外 (またはこの例外から派生した例外)を送出します。この例外は :exc:`IOError` のサブクラスです。

   .. attribute:: reason

      このエラーの原因。メッセージ文字列か、他の例外のインスタンス(リモートURLの場合は :exc:`socket.error`,
      ローカルURLの場合は :exc:`OSError`)。

.. exception:: HTTPError

   これは例外(:exc:`URLError` のサブクラス)ですが、このオブジェクトは例外でないファイル類似のオブジェクトとして返り値に使うことができます
   (:func:`urlopen` が返すのと同じものです)。
   この機能は、例えばサーバからの認証リクエストのように、変わった HTTP エラーを処理するのに役立ちます。

   .. attribute:: code

      `RFC 2616 <http://www.faqs.org/rfcs/rfc2616.html>`_ に定義されているHTTPステータスコード。
      この数値型の値は、 :attr:`BaseHTTPServer.BaseHTTPRequestHandler.responses`
      の辞書に登録されているコードに対応します。


以下のクラスが提供されています:


.. class:: Request(url[, data][, headers][, origin_req_host][, unverifiable])

   このクラスは URL リクエストを抽象化したものです。

   *url* は有効な URL を指す文字列でなくてはなりません。

   *data* はサーバに送信する追加のデータを示す文字列か、そのようなデータが無ければ *None* を指定します。現時点でHTTP リクエストは *data*
   をサポートする唯一のリクエスト形式です; *data* パラメタが指定が指定された場合、HTTP リクエストは GET でなく POST になります。
   *data* は標準的な :mimetype:`application/x-www-form-urlencoded` 形式のバッファでなくてはなりません。
   :func:`urllib.urlencode` 関数はマップ型か2タプルのシーケンスを取り、この形式の文字列を返します。

   *headers* は辞書でなくてはなりません。この辞書は :meth:`add_header` を辞書のキーおよび値を引数として呼び出した時と
   同じように扱われます。
   この引数はよく、ブラウザが何であるかを特定する ``User-Agent`` ヘッダを偽装するために用いられます。
   幾つかのHTTPサーバーが、スクリプトからのアクセスを禁止するために、一般的なブラウザの ``User-Agent``
   ヘッダーしか許可しないからです。
   例えば、 Mozilla Firefox は ``User-Agent`` に ``"Mozilla/5. (X11; U; Linux i686) Gecko/20071127 Firefox/2.0.0.11"``
   のように設定し、 :mod:`urllib2` はデフォルトで ``"Python-urllib/2.6"`` (Python 2.6の場合)と設定します。

   最後の二つの引数は、サードパーティの HTTP クッキーを正しく扱いたい場合にのみ関係してきます:

   *origin_req_host* は、 :rfc:`2965` で定義されている元のトランザクションにおけるリクエストホスト (request-host of
   the origin transaction) です。デフォルトの値は ``cookielib.request_host(self)`` です。
   この値は、ユーザによって開始された元々のリクエストにおけるホスト名や IP アドレスです。例えば、もしリクエストがある HTML
   ドキュメント内の画像を指していれば、この値は画像を含んでいるページへのリクエストにおけるリクエストホストになるはずです。

   *unverifiable* は、 :rfc:`2965` の定義において、該当するリクエストが証明不能 (unverifiable)
   であるかどうかを示します。デフォルトの値は False です。証明不能なリクエストとは、ユーザが受け入れの可否を選択できないような URL
   を持つリクエストのことです。例えば、リクエストが HTML ドキュメント中の画像であり、ユーザがこの画像を自動的に取得するか
   どうかを選択できない場合には、証明不能フラグは True になります。


.. class:: OpenerDirector()

   :class:`OpenerDirector` クラスは、 :class:`BaseHandler` の連鎖的に呼び出して URL
   を開きます。このクラスはハンドラをどのように連鎖させるか、またどのようにエラーをリカバリするかを管理します。


.. class:: BaseHandler()

   このクラスはハンドラ連鎖に登録される全てのハンドラがベースとしているクラスです -- このクラスでは登録のための単純なメカニズムだけを扱います。


.. class:: HTTPDefaultErrorHandler()

   HTTP エラー応答のための標準のハンドラを定義します; 全てのレスポンスに対して、例外 :exc:`HTTPError` を送出します。


.. class:: HTTPRedirectHandler()

   リダイレクションを扱うクラスです。


.. class:: HTTPCookieProcessor([cookiejar])

   HTTP Cookie を扱うためのクラスです。


.. class:: ProxyHandler([proxies])

   このクラスはプロキシを通過してリクエストを送らせます。引数 *proxies* を与える場合、プロトコル名からプロキシの URL
   へ対応付ける辞書でなくてはなりません。標準では、プロキシのリストを環境変数 :envvar:`<protocol>_proxy`  から読み出します。

   プロキシ環境変数が設定されていない場合は、 Windows 環境では、
   レジストリのインターネット設定セクションからプロキシ設定を手に入れ、
   Mac OS X 環境では、 OS X システム設定フレームワーク
   (System Configuration Framework) からプロキシ情報を取得します。
   
   自動検出されたproxyを無効にするには、空の辞書を渡してください。
   

.. class:: HTTPPasswordMgr()

   ``(realm, uri) -> (user, password)`` の対応付けデータベースを保持します。


.. class:: HTTPPasswordMgrWithDefaultRealm()

   ``(realm, uri) -> (user, password)``  の対応付けデータベースを保持します。レルム ``None``
   はその他諸々のレルムを表し、他のレルムが該当しない場合に検索されます。


.. class:: AbstractBasicAuthHandler([password_mgr])

   このクラスはHTTP 認証を補助するための混ぜ込みクラス (mixin class) です。遠隔ホストとプロキシの両方に対応しています。
   *password_mgr* を与える場合、 :class:`HTTPPasswordMgr` と互換性がなければなりません;
   互換性のためにサポートしなければならないインタフェースについての情報はセクション :ref:`http-password-mgr` を参照してください。


.. class:: HTTPBasicAuthHandler([password_mgr])

   遠隔ホストとの間での認証を扱います。 *password_mgr* を与える場合、 :class:`HTTPPasswordMgr` と互換性が
   なければなりません;  互換性のためにサポートしなければならないインタフェースについての情報はセクション :ref:`http-password-mgr`
   を参照してください。


.. class:: ProxyBasicAuthHandler([password_mgr])

   プロキシとの間での認証を扱います。 *password_mgr* を与える場合、 :class:`HTTPPasswordMgr` と互換性が
   なければなりません;  互換性のためにサポートしなければならないインタフェースについての情報はセクション :ref:`http-password-mgr`
   を参照してください。


.. class:: AbstractDigestAuthHandler([password_mgr])

   このクラスはHTTP 認証を補助するための混ぜ込みクラス (mixin class) です。遠隔ホストとプロキシの両方に対応しています。
   *password_mgr* を与える場合、 :class:`HTTPPasswordMgr` と互換性がなければなりません;
   互換性のためにサポートしなければならないインタフェースについての情報はセクション :ref:`http-password-mgr` を参照してください。


.. class:: HTTPDigestAuthHandler([password_mgr])

   遠隔ホストとの間での認証を扱います。 *password_mgr* を与える場合、 :class:`HTTPPasswordMgr` と互換性が
   なければなりません;  互換性のためにサポートしなければならないインタフェースについての情報はセクション :ref:`http-password-mgr`
   を参照してください。


.. class:: ProxyDigestAuthHandler([password_mgr])

   プロキシとの間での認証を扱います。 *password_mgr* を与える場合、 :class:`HTTPPasswordMgr` と互換性が
   なければなりません;  互換性のためにサポートしなければならないインタフェースについての情報はセクション :ref:`http-password-mgr`
   を参照してください。


.. class:: HTTPHandler()

   HTTP の URL を開きます。


.. class:: HTTPSHandler()

   HTTPS の URL を開きます。


.. class:: FileHandler()

   ローカルファイルを開きます。


.. class:: FTPHandler()

   FTP の URL を開きます。


.. class:: CacheFTPHandler()

   FTP の URL を開きます。遅延を最小限にするために、開かれている FTP  接続に対するキャッシュを保持します。


.. class:: UnknownHandler()

   その他諸々のためのクラスで、未知のプロトコルの URL を開きます。


.. _request-objects:

Request オブジェクト
--------------------

以下のメソッドは :class:`Request` の全ての公開インタフェースを記述します。
従ってサブクラスではこれら全てのメソッドをオーバライドしなければなりません。


.. method:: Request.add_data(data)

   :class:`Request` のデータを *data* に設定します。この値は HTTP  ハンドラ以外のハンドラでは無視されます。HTTP
   ハンドラでは、データはバイト文字列でなくてはなりません。このメソッドを使うとリクエストの形式が ``GET`` から ``POST`` に変更されます。


.. method:: Request.get_method()

   HTTP リクエストメソッドを示す文字列を返します。このメソッドは HTTP リクエストだけに対して意味があり、現状では常に ``'GET'`` か
   ``'POST'`` のいずれかの値を返します。


.. method:: Request.has_data()

   インスタンスが ``None`` でないデータを持つかどうかを返します。


.. method:: Request.get_data()

   インスタンスのデータを返します。


.. method:: Request.add_header(key, val)

   リクエストに新たなヘッダを追加します。ヘッダは HTTP ハンドラ以外のハンドラでは無視されます。HTTP ハンドラでは、引数はサーバに送信される
   ヘッダのリストに追加されます。同じ名前を持つヘッダを 2 つ以上持つことはできず、 *key* の衝突が生じた場合、後で追加したヘッダが前に
   追加したヘッダを上書きします。現時点では、この機能は HTTP の機能を損ねることはありません。というのは、複数回呼び出したときに意味を
   持つようなヘッダには、どれもただ一つのヘッダを使って同じ機能を果たすための (ヘッダ特有の) 方法があるからです。


.. method:: Request.add_unredirected_header(key, header)

   リダイレクトされたリクエストには追加されないヘッダを追加します。

   .. versionadded:: 2.4


.. method:: Request.has_header(header)

   インスタンスが名前つきヘッダであるかどうかを (通常のヘッダと非リダイレクトヘッダの両方を調べて) 返します。

   .. versionadded:: 2.4


.. method:: Request.get_full_url()

   コンストラクタで与えられた URL を返します。


.. method:: Request.get_type()

   URL のタイプ --- いわゆるスキーム (scheme) --- を返します。


.. method:: Request.get_host()

   接続を行う先のホスト名を返します。


.. method:: Request.get_selector()

   セレクタ --- サーバに送られる URL の一部分 --- を返します。


.. method:: Request.set_proxy(host, type)

   リクエストがプロキシサーバを経由するように準備します。 *host* および *type* はインスタンスのもとの設定と置き換えられ
   ます。インスタンスのセレクタはコンストラクタに与えたもともとの URL になります。


.. method:: Request.get_origin_req_host()

   :rfc:`2965` の定義よる、始原トランザクションのリクエストホストを返します。 :class:`Request` コンストラクタのドキュメントを
   参照してください。


.. method:: Request.is_unverifiable()

   リクエストが :rfc:`2965` の定義における証明不能リクエストであるかどうかを返します。 :class:`Request`
   コンストラクタのドキュメントを参照してください。


.. _opener-director-objects:

OpenerDirector オブジェクト
---------------------------

:class:`OpenerDirector` インスタンスは以下のメソッドを持っています:


.. method:: OpenerDirector.add_handler(handler)

   *handler* は :class:`BaseHandler` のインスタンスでなければなりません。以下のメソッドを使った検索が行われ、URL
   を取り扱うことが可能なハンドラの連鎖が追加されます (HTTP エラーは特別扱いされているので注意してください)。

   * :samp:`{protocol}_open` --- ハンドラが *protocol* の URL を開く方法を知っているかどうかを調べます。

   * :samp:`http_error_{type}` --- ハンドラが HTTP エラーコード *type* の処理方法を知っていることを示すシグナルです。

   * :samp:`{protocol}_error` --- ハンドラが (``http`` でない) *protocol* のエラー
     を処理する方法を知っていることを示すシグナルです。

   * :samp:`{protocol}_request` --- ハンドラが *protocol* リクエストのプリプロセス方法
     を知っていることを示すシグナルです。

   * :samp:`{protocol}_response` --- ハンドラが *protocol* リクエストのポストプロセス方法
     を知っていることを示すシグナルです。


.. method:: OpenerDirector.open(url[, data][, timeout])

   与えられた *url* (リクエストオブジェクトでも文字列でもかまいません) を開きます。オプションとして *data* を与えることができます。
   引数、返り値、および送出される例外は :func:`urlopen` と同じです (:func:`urlopen` の場合、標準でインストールされている
   グローバルな :class:`OpenerDirector` の :meth:`open` メソッドを呼び出します) 。
   オプションの *timeout* 引数は、接続開始のようなブロックする処理におけるタイムアウト時間を
   秒数で指定します。(指定しなかった場合は、グローバルのデフォルト設定が利用されます)
   タイムアウト機能は、 HTTP, HTTPS, FTP 接続でのみ有効です。

   .. versionchanged:: 2.6
      *timeout* 引数が追加されました

.. method:: OpenerDirector.error(proto[, arg[, ...]])

   与えられたプロトコルにおけるエラーを処理します。このメソッドは与えられたプロトコルにおける登録済みのエラーハンドラを (プロトコル固有の)
   引数で呼び出します。 HTTP プロトコルは特殊なケースで、特定のエラーハンドラを選び出すのに HTTP レスポンスコードを使います; ハンドラクラスの
   :meth:`http_error_\*` メソッドを参照してください。

   返り値および送出される例外は :func:`urlopen` と同じものです。

OpenerDirector オブジェクトは、以下の 3 つのステージに分けて URL を開きます:

各ステージで OpenerDirector オブジェクトのメソッドがどのような順で呼び出されるかは、ハンドラインスタンスの並び方で決まります。

#. :samp:`{protocol}_request` 形式のメソッドを持つ全てのハンドラに対してそのメソッドを呼び出し、リクエストの
   プリプロセスを行います。

#. :samp:`{protocol}_open` 形式のメソッドを持つハンドラを呼び出し、リクエストを処理します。
   このステージは、ハンドラが :const:`None` でない値 (すなわちレスポンス) を返すか、例外 (通常は :exc:`URLError`)
   を送出した時点で終了します。例外は伝播 (propagate) できます。

   実際には、上のアルゴリズムではまず :meth:`default_open` という名前のメソッドを呼び出します。このメソッドが全て
   :const:`None` を返す場合、同じアルゴリズムを繰り返して、今度は :samp:`{protocol}_open`
   形式のメソッドを試します。メソッドが全て :const:`None` を返すと、さらに同じアルゴリズムを繰り返して :meth:`unknown_open`
   を呼び出します。

   これらのメソッドの実装には、親となる :class:`OpenerDirector` インスタンスの :meth:`~OpenDirector.open`
   や :meth:`~OpenDirector.error` といったメソッド呼び出しが入る場合があるので注意してください。

#. :samp:`{protocol}_response` 形式のメソッドを持つ全てのハンドラに対してそのメソッドを呼び出し、リクエストの
   ポストプロセスを行います。


.. _base-handler-objects:

BaseHandler オブジェクト
------------------------

:class:`BaseHandler` オブジェクトは直接的に役に立つ 2 つのメソッドと、その他として派生クラスで使われることを想定したメソッドを
提供します。以下は直接的に使うためのメソッドです:


.. method:: BaseHandler.add_parent(director)

   親オブジェクトとして、 ``director`` を追加します。


.. method:: BaseHandler.close()

   全ての親オブジェクトを削除します。

以下のメンバおよびメソッドは :class:`BaseHandler` から派生したクラスでのみ使われます:

.. note::

   慣習的に、 :meth:`protocol_request` や :meth:`protocol_response` といったメソッドを定義している
   サブクラスは :class:`\ *Processor` と名づけ、その他は :class:`\* Handler` と名づけることになっています


.. attribute:: BaseHandler.parent

   有効な :class:`OpenerDirector` です。この値は違うプロトコルを使って URL を開く場合やエラーを処理する際に使われます。


.. method:: BaseHandler.default_open(req)

   このメソッドは :class:`BaseHandler` では定義されて *いません* 。しかし、全ての URL をキャッチさせたいなら、サブクラスで定義する
   必要があります。

   このメソッドが定義されていた場合、 :class:`OpenerDirector` から呼び出されます。このメソッドは
   :class:`OpenerDirector` のメソッド :meth:`open` が返す値について記述されているようなファイル類似の
   オブジェクトか、 ``None`` を返さなくてはなりません。このメソッドが送出する例外は、真に例外的なことが起きない限り、 :exc:`URLError`
   を送出しなければなりません (例えば、 :exc:`MemoryError` を :exc:`URLError` をマップしてはいけません)。

   このメソッドはプロトコル固有のオープンメソッドが呼び出される前に呼び出されます。


.. method:: BaseHandler.protocol_open(req)
   :noindex:

   ("protocol" は実際にはプロトコル名です)

   このメソッドは :class:`BaseHandler` では定義されて *いません* 。しかし *protocol* の URL
   をキャッチしたいなら、サブクラスで定義する必要があります。

   このメソッドが定義されていた場合、 :class:`OpenerDirector` から呼び出されます。戻り値は :meth:`default_open`
   と同じでなければなりません。


.. method:: BaseHandler.unknown_open(req)

   このメソッドは :class:`BaseHandler` では定義されて *いません* 。しかし URL を開くための特定のハンドラが登録されていないような
   URL をキャッチしたいなら、サブクラスで定義する必要があります。

   このメソッドが定義されていた場合、 :class:`OpenerDirector` から呼び出されます。戻り値は :meth:`default_open`
   と同じでなければなりません。


.. method:: BaseHandler.http_error_default(req, fp, code, msg, hdrs)

   このメソッドは :class:`BaseHandler` では定義されて *いません* 。しかしその他の処理されなかった HTTP
   エラーを処理する機能をもたせたいなら、サブクラスで定義する必要があります。このメソッドはエラーに遭遇した :class:`OpenerDirector`
   から自動的に呼び出されます。その他の状況では普通呼び出すべきではありません。

   *req* は :class:`Request` オブジェクトで、 *fp* は HTTP エラー本体を読み出せるようなファイル類似のオブジェクトに
   なります。 *code* は 3 桁の 10 進数からなるエラーコードで、 *msg* ユーザ向けのエラーコード解説です。 *hdrs* は
   エラー応答のヘッダをマップしたオブジェクトです。

   返される値および送出される例外は :func:`urlopen` と同じものでなければなりません。


.. method:: BaseHandler.http_error_nnn(req, fp, code, msg, hdrs)

   *nnn* は 3 桁の 10 進数からなる HTTP エラーコードでなくてはなりません。このメソッドも :class:`BaseHandler`
   では定義されていませんが、サブクラスのインスタンスで定義されていた場合、エラーコード *nnn* の HTTP エラーが発生した際に呼び出されます。

   特定の HTTP エラーに対する処理を行うためには、このメソッドをサブクラスでオーバライドする必要があります。

   引数、返される値、および送出される例外は :meth:`http_error_default` と同じものでなければなりません。


.. method:: BaseHandler.protocol_request(req)
   :noindex:

   ("protocol" は実際にはプロトコル名です)

   このメソッドは :class:`BaseHandler` では *定義されていません* が、サブクラスで特定の *protocol*
   のリクエストのプリプロセスを行いたい場合には定義する必要があります。

   このメソッドが定義されていると、親となる :class:`OpenerDirector` から呼び出されます。その際、 *req*
   は :class:`Request` オブジェクトになります。戻り値は :class:`Request` オブジェクトでなければなりません。


.. method:: BaseHandler.protocol_response(req, response)
   :noindex:

   ("protocol" は実際にはプロトコル名です)

   このメソッドは :class:`BaseHandler` では *定義されていません* が、サブクラスで特定の *protocol*
   のリクエストのポストプロセスを行いたい場合には定義する必要があります。

   このメソッドが定義されていると、親となる :class:`OpenerDirector` から呼び出されます。その際、 *req*
   は :class:`Request` オブジェクトになります。 *response* は :func:`urlopen` の戻り値と同じインタフェースを
   実装したオブジェクトになります。戻り値もまた、 :func:`urlopen` の戻り値と同じインタフェースを実装したオブジェクトでなければなりません。


.. _http-redirect-handler:

HTTPRedirectHandler オブジェクト
--------------------------------

.. note::

   HTTP リダイレクトによっては、このモジュールのクライアントコード側での処理を必要とします。その場合、 :exc:`HTTPError` が送出されます。
   様々なリダイレクトコードの厳密な意味に関する詳細は :rfc:`2616` を参照してください。


.. method:: HTTPRedirectHandler.redirect_request(req, fp, code, msg, hdrs, newurl)

   リダイレクトの通知に応じて、 :class:`Request` または ``None`` を返します。このメソッドは ``http_error_30*()``
   メソッドにおいて、リダイレクトの通知をサーバから受信した際に、デフォルトの実装として呼び出されます。リダイレクトを起こす場合、新たな
   :class:`Request` を生成して、 ``http_error_30*()`` が *newurl* へリダイレクトを実行できるようにします。
   そうでない場合、他のどのハンドラにもこの URL を処理させたくなければ :exc:`HTTPError` を送出し、
   リダイレクト処理を行うことはできないが他のハンドラなら可能かもしれない場合には ``None`` を返します。

   .. note::

      このメソッドのデフォルトの実装は、 :rfc:`2616` に厳密に従ったものではありません。 :rfc:`2616` では、 ``POST``
      リクエストに対する 301 および 302 応答が、ユーザの承認なく自動的にリダイレクトされてはならないと述べています。現実には、ブラウザは POST を
      ``GET`` に変更することで、これらの応答に対して自動的にリダイレクトを行えるようにしています。デフォルトの実装でも、この挙動を再現しています。


.. method:: HTTPRedirectHandler.http_error_301(req, fp, code, msg, hdrs)

   ``Location:`` か ``URI:`` のURL にリダイレクトします。このメソッドは HTTP  における 'moved permanently' レスポンスを取得した際に
   親オブジェクトとなる :class:`OpenerDirector` によって呼び出されます。


.. method:: HTTPRedirectHandler.http_error_302(req, fp, code, msg, hdrs)

   :meth:`http_error_301` と同じですが、'found' レスポンスに対して呼び出されます。


.. method:: HTTPRedirectHandler.http_error_303(req, fp, code, msg, hdrs)

   :meth:`http_error_301` と同じですが、'see other' レスポンスに対して呼び出されます。


.. method:: HTTPRedirectHandler.http_error_307(req, fp, code, msg, hdrs)

   :meth:`http_error_301` と同じですが、'temporary redirect'  レスポンスに対して呼び出されます。


.. _http-cookie-processor:

HTTPCookieProcessor オブジェクト
--------------------------------

.. versionadded:: 2.4

:class:`HTTPCookieProcessor` インスタンスは属性をひとつだけ持ちます:


.. attribute:: HTTPCookieProcessor.cookiejar

   クッキーの入っている :class:`cookielib.CookieJar` オブジェクトです。


.. _proxy-handler:

ProxyHandler オブジェクト
-------------------------


.. method:: ProxyHandler.protocol_open(request)
   :noindex:

   ("protocol" は実際にはプロトコル名です)

   :class:`ProxyHandler` は、コンストラクタで与えた辞書 *proxies* にプロキシが設定されているような *protocol*
   全てについて、メソッド  :samp:`{protocol}_open` を持つことになります。このメソッドは ``request.set_proxy()``
   を呼び出して、リクエストがプロキシを通過できるように修正します。その後連鎖するハンドラの中から次のハンドラを呼び出して実際にプロトコルを実行します。


.. _http-password-mgr:

HTTPPasswordMgr オブジェクト
----------------------------

以下のメソッドは :class:`HTTPPasswordMgr` および :class:`HTTPPasswordMgrWithDefaultRealm`
オブジェクトで利用できます。


.. method:: HTTPPasswordMgr.add_password(realm, uri, user, passwd)

   *uri* は単一の URI でも複数の URI からなるシーケンスでもかまいません。 *realm* 、 *user* および *passwd*
   は文字列でなくてはなりません。このメソッドによって、 *realm* と与えられた URI の上位 URI に対して ``(user, passwd)``
   が認証トークンとして使われるようになります。


.. method:: HTTPPasswordMgr.find_user_password(realm, authuri)

   与えられたレルムおよび URI に対するユーザ名またはパスワードがあればそれを取得します。該当するユーザ名／パスワードが存在しない場合、このメソッドは
   ``(None, None)`` を返します。

   :class:`HTTPPasswordMgrWithDefaultRealm` オブジェクトでは、与えられた *realm*
   に対して該当するユーザ名/パスワードが存在しない場合、レルム ``None`` が検索されます。


.. _abstract-basic-auth-handler:

AbstractBasicAuthHandler オブジェクト
-------------------------------------


.. method:: AbstractBasicAuthHandler.http_error_auth_reqed(authreq, host, req, headers)

   ユーザ名／パスワードを取得し、再度サーバへのリクエストを試みることで、サーバからの認証リクエストを処理します。 *authreq* はリクエストにおいて
   レルムに関する情報が含まれているヘッダの名前、 *host* は認証を行う対象の URL とパスを指定します、 *req* は (失敗した)
   :class:`Request` オブジェクト、そして *headers* はエラーヘッダでなくてはなりません。

   *host* は、オーソリティ (例 ``"python.org"``) か、オーソリティコンポーネントを含む URL (例
   ``"http://python.org"``) です。どちらの場合も、オーソリティはユーザ情報コンポーネントを含んではいけません
   (なので、 ``"python.org"`` や ``"python.org:80"`` は正しく、 ``"joe:password@python.org"``
   は不正です) 。


.. _http-basic-auth-handler:

HTTPBasicAuthHandler オブジェクト
---------------------------------


.. method:: HTTPBasicAuthHandler.http_error_401(req, fp, code,  msg, hdrs)

   認証情報がある場合、認証情報付きで再度リクエストを試みます。


.. _proxy-basic-auth-handler:

ProxyBasicAuthHandler オブジェクト
----------------------------------


.. method:: ProxyBasicAuthHandler.http_error_407(req, fp, code,  msg, hdrs)

   認証情報がある場合、認証情報付きで再度リクエストを試みます。


.. _abstract-digest-auth-handler:

AbstractDigestAuthHandler オブジェクト
--------------------------------------


.. method:: AbstractDigestAuthHandler.http_error_auth_reqed(authreq, host, req, headers)

   *authreq* はリクエストにおいてレルムに関する情報が含まれているヘッダの名前、 *host* は認証を行う対象のホスト名、 *req* は  (失敗した)
   :class:`Request` オブジェクト、そして *headers* はエラーヘッダでなくてはなりません。


.. _http-digest-auth-handler:

HTTPDigestAuthHandler オブジェクト
----------------------------------


.. method:: HTTPDigestAuthHandler.http_error_401(req, fp, code,  msg, hdrs)

   認証情報がある場合、認証情報付きで再度リクエストを試みます。


.. _proxy-digest-auth-handler:

ProxyDigestAuthHandler オブジェクト
-----------------------------------


.. method:: ProxyDigestAuthHandler.http_error_407(req, fp, code,  msg, hdrs)

   認証情報がある場合、認証情報付きで再度リクエストを試みます。


.. _http-handler-objects:

HTTPHandler オブジェクト
------------------------


.. method:: HTTPHandler.http_open(req)

   HTTP リクエストを送ります。 ``req.has_data()`` に応じて、 GET または POST のどちらでも送ることができます。


.. _https-handler-objects:

HTTPSHandler オブジェクト
-------------------------


.. method:: HTTPSHandler.https_open(req)

   HTTPS リクエストを送ります。 ``req.has_data()`` に応じて、 GET または POST のどちらでも送ることができます。


.. _file-handler-objects:

FileHandler オブジェクト
------------------------


.. method:: FileHandler.file_open(req)

   ホスト名がない場合、またはホスト名が ``'localhost'`` の場合にファイルをローカルでオープンします。そうでない場合、プロトコルを ``ftp``
   に切り替え、 :attr:`parent` を使って再度オープンを試みます。


.. _ftp-handler-objects:

FTPHandler オブジェクト
-----------------------


.. method:: FTPHandler.ftp_open(req)

   *req* で表されるファイルを FTP 越しにオープンします。ログインは常に空のユーザネームおよびパスワードで行われます。


.. _cacheftp-handler-objects:

CacheFTPHandler オブジェクト
----------------------------

:class:`CacheFTPHandler` オブジェクトは :class:`FTPHandler` オブジェクトに以下のメソッドを追加したものです:


.. method:: CacheFTPHandler.setTimeout(t)

   接続のタイムアウトを *t* 秒に設定します。


.. method:: CacheFTPHandler.setMaxConns(m)

   キャッシュ付き接続の最大接続数を *m* に設定します。


.. _unknown-handler-objects:

UnknownHandler オブジェクト
---------------------------


.. method:: UnknownHandler.unknown_open()

   例外 :exc:`URLError` を送出します。


.. _http-error-processor-objects:

HTTPErrorProcessor オブジェクト
-------------------------------

.. versionadded:: 2.4


.. method:: HTTPErrorProcessor.unknown_open()

   HTTP エラーレスポンスを処理します。

   エラーコード 200 の場合、レスポンスオブジェクトを即座に返します。

   200 以外のエラーコードの場合、 :meth:`OpenerDirector.error` を介して :samp:`{protocol}_error_code`
   メソッドに仕事を引き渡します。最終的にどのハンドラもエラーを処理しなかった
   場合、 :class:`urllib2.HTTPDefaultErrorHandler` が :exc:`HTTPError` を送出します。


.. _urllib2-examples:

例
--

以下の例では、 python.org のメインページを取得して、その最初の 100 バイト分を表示します::

   >>> import urllib2
   >>> f = urllib2.urlopen('http://www.python.org/')
   >>> print f.read(100)
   <!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
   <?xml-stylesheet href="./css/ht2html

今度は CGI の標準入力にデータストリームを送信し、CGI が返すデータを読み出します。この例は Python が SSL をサポートしている場合にのみ
動作することに注意してください。 ::

   >>> import urllib2
   >>> req = urllib2.Request(url='https://localhost/cgi-bin/test.cgi',
   ...                       data='This data is passed to stdin of the CGI')
   >>> f = urllib2.urlopen(req)
   >>> print f.read()
   Got Data: "This data is passed to stdin of the CGI"

上の例で使われているサンプルの CGI は以下のようになっています::

   #!/usr/bin/env python
   import sys
   data = sys.stdin.read()
   print 'Content-type: text-plain\n\nGot Data: "%s"' % data

以下はベーシック HTTP 認証の例です::

   import urllib2
   # ベーシック HTTP 認証をサポートする OpenerDirector を作成する...
   auth_handler = urllib2.HTTPBasicAuthHandler()
   auth_handler.add_password(realm='PDQ Application',
                             uri='https://mahler:8092/site-updates.py',
                             user='klem',
                             passwd='kadidd!ehopper')
   opener = urllib2.build_opener(auth_handler)
   # ...urlopen から利用できるよう、グローバルにインストールする
   urllib2.install_opener(opener)
   urllib2.urlopen('http://www.example.com/login.html')

:func:`build_opener` はデフォルトで沢山のハンドラを提供しており、その中に :class:`ProxyHandler`
があります。デフォルトでは、 :class:`ProxyHandler` は ``<scheme>_proxy`` という環境変数を使います。
ここで ``<scheme>`` は URL スキームです。例えば、 HTTP プロキシの URL を得るには、環境変数 :envvar:`http_proxy`
を読み出します。

この例では、デフォルトの :class:`ProxyHandler` を置き換えてプログラム的に作成したプロキシ URL を使うようにし、
:class:`ProxyBasicAuthHandler` でプロキシ認証サポートを追加します。 ::

   proxy_handler = urllib2.ProxyHandler({'http': 'http://www.example.com:3128/'})
   proxy_auth_handler = urllib2.ProxyBasicAuthHandler()
   proxy_auth_handler.add_password('realm', 'host', 'username', 'password')

   opener = urllib2.build_opener(proxy_handler, proxy_auth_handler)
   # 今回は OpenerDirector をインストールするのではなく直接使います:
   opener.open('http://www.example.com/login.html')

以下は HTTP ヘッダを追加する例です:

*headers* 引数を使って :class:`Request` コンストラクタを呼び出す方法の他に、以下のようにできます::

   import urllib2
   req = urllib2.Request('http://www.example.com/')
   req.add_header('Referer', 'http://www.python.org/')
   r = urllib2.urlopen(req)

:class:`OpenerDirector` は全ての :class:`Request` に :mailheader:`User-Agent`
ヘッダを自動的に追加します。これを変更するには::

   import urllib2
   opener = urllib2.build_opener()
   opener.addheaders = [('User-agent', 'Mozilla/5.0')]
   opener.open('http://www.example.com/')

のようにします。

また、 :class:`Request` が :func:`urlopen` (や :meth:`OpenerDirector.open`)
に渡される際には、いくつかの標準ヘッダ (:mailheader:`Content-Length`, :mailheader:`Content-Type`
および :mailheader:`Host`) も追加されることを忘れないでください。

