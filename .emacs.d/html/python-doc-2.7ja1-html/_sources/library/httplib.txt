:mod:`httplib` --- HTTP プロトコルクライアント
==============================================

.. module:: httplib
   :synopsis: HTTP および HTTPS プロトコルのクライアント  (ソケットを必要とします) 。

.. .. note::
   The :mod:`httplib` module has been renamed to :mod:`http.client` in Python
   3.0.  The :term:`2to3` tool will automatically adapt imports when converting
   your sources to 3.0.

.. note::
   :mod:`httplib` モジュールは、Python 3.0 では :mod:`http.client` にリネームされました。
   :term:`2to3` ツールが自動的にソースコードのimportを修正します。

.. index::
   pair: HTTP; protocol
   single: HTTP; httplib (standard module)

.. index:: module: urllib

このモジュールでは HTTP および HTTPS プロトコルのクライアント側を実装しているクラスを定義しています。通常、このモジュールは直接使いません
--- :mod:`urllib` モジュールが HTTP や HTTPS を使った URL を扱う上でこのモジュールを使います。

.. note::

   HTTPS のサポートは、SSL をサポートするように :mod:`socket` モジュールをコンパイルした場合にのみ利用できます。

このモジュールでは以下のクラスを提供しています:


.. class:: HTTPConnection(host[, port[, strict[, timeout[, source_address]]]])

   :class:`HTTPConnection` インスタンスは、HTTP サーバとの一回のトランザクションを表現します。インスタンスの生成はホスト名と
   オプションのポート番号を与えて行います。
   ポート番号を指定しなかった場合、ホスト名文字列が ``host:port``
   の形式であれば、ホスト名からポート番号を導き、そうでない場合には標準の HTTP ポート番号 (80) を使います。

   オプションの引数 *strict* に ``True`` が渡された場合(デフォルトでは ``False``)、
   ステータスラインが正しい(valid)　HTTP/1.0 もしくは 1.1 status line
   としてパースできなかった時に、 ``BadStatusLine`` 例外を発生させます。

   オプションの引数 *timeout* が渡された場合、ブロックする処理(コネクション接続など)
   のタイムアウト時間(秒数)として利用されます。(渡されなかった場合は、グローバルのデフォルト
   タイムアウト設定が利用されます。)

   オプションの引数 *source_address* を (host, port) という形式のタプルにすると HTTP 接続の接続元アドレスとして使用します。

   例えば、以下の呼び出しは全て同じサーバの同じポートに接続するインスタンスを生成します::

      >>> h1 = httplib.HTTPConnection('www.cwi.nl')
      >>> h2 = httplib.HTTPConnection('www.cwi.nl:80')
      >>> h3 = httplib.HTTPConnection('www.cwi.nl', 80)
      >>> h3 = httplib.HTTPConnection('www.cwi.nl', 80, timeout=10)

   .. versionadded:: 2.0

   .. versionchanged:: 2.6
      *timeout* 引数が追加されました

   .. versionchanged:: 2.7
      *source_address* 引数が追加されました

.. class:: HTTPSConnection(host[, port[, key_file[, cert_file[, strict[, timeout[, source_address]]]]]])

   :class:`HTTPConnection` のサブクラスで、セキュアなサーバと通信するために SSL を使います。標準のポート番号は ``443``
   です。
   *key_file* には、秘密鍵を格納したPEM形式ファイルのファイル名を指定します。 *cert_file* には、PEM形式の証明書チェーンファイルを指定します。

   .. warning::
      この関数はサーバ証明書の検査を行いません

   .. versionchanged:: 2.6
      *timeout* 引数が追加されました

   .. versionchanged:: 2.7
      *source_address* 引数が追加されました

.. class:: HTTPResponse(sock[, debuglevel=0][, strict=0])

   コネクションに成功したときに、このクラスのインスタンスが返されます。
   ユーザーから直接利用されることはありません。

   .. versionadded:: 2.0

.. class:: HTTPMessage

   .. An :class:`HTTPMessage` instance is used to hold the headers from an HTTP
      response. It is implemented using the :class:`mimetools.Message` class and
      provides utility functions to deal with HTTP Headers. It is not directly
      instantiated by the users.

   :class:`HTTPMessage` のインスタンスは、 HTTP レスポンスヘッダを格納する
   ために利用されます。 :class:`mimetools.Message` クラスを利用して実装されて
   いて、 HTTP ヘッダを扱うための便利な関数を提供しています。このクラスは
   ユーザーが直接インスタンス生成するものではありません。



必要に応じて以下の例外が送出されます:

.. exception:: HTTPException

   このモジュールにおける他の例外クラスの基底クラスです。 :exc:`Exception` のサブクラスです。

    .. versionadded:: 2.0

.. exception:: NotConnected

   :exc:`HTTPException` サブクラスです。

    .. versionadded:: 2.0

.. exception:: InvalidURL

   :exc:`HTTPException` のサブクラスです。ポート番号を指定したものの、その値が数字でなかったり空のオブジェクトであった場合に送出されます。

    .. versionadded:: 2.3

.. exception:: UnknownProtocol

   :exc:`HTTPException` のサブクラスです。


.. exception:: UnknownTransferEncoding

   :exc:`HTTPException` のサブクラスです。


.. exception:: IllegalKeywordArgument

   :exc:`HTTPException` のサブクラスです。


.. exception:: UnimplementedFileMode

   :exc:`HTTPException` のサブクラスです。


.. exception:: IncompleteRead

   :exc:`HTTPException` のサブクラスです。


.. exception:: ImproperConnectionState

   :exc:`HTTPException` のサブクラスです。


.. exception:: CannotSendRequest

   :exc:`ImproperConnectionState` のサブクラスです。


.. exception:: CannotSendHeader

   :exc:`ImproperConnectionState` のサブクラスです。


.. exception:: ResponseNotReady

   :exc:`ImproperConnectionState` のサブクラスです。


.. exception:: BadStatusLine

   :exc:`HTTPException` のサブクラスです。サーバが理解できない HTTP 状態コードで応答した場合に送出されます。

このモジュールで定義されている定数は以下の通りです:


.. data:: HTTP_PORT

   HTTP プロトコルの標準のポート (通常は ``80``) です。


.. data:: HTTPS_PORT

   HTTPS プロトコルの標準のポート (通常は ``443``) です。

また、整数の状態コードについて以下の定数が定義されています:

+------------------------------------------+---------+-----------------------------------------------------------------------+
| Constant                                 | Value   | Definition                                                            |
+==========================================+=========+=======================================================================+
| :const:`CONTINUE`                        | ``100`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.1.1                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.1.1>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`SWITCHING_PROTOCOLS`             | ``101`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.1.2                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.1.2>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`PROCESSING`                      | ``102`` | WEBDAV, `RFC 2518, Section 10.1                                       |
|                                          |         | <http://www.webdav.org/specs/rfc2518.html#STATUS_102>`_               |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`OK`                              | ``200`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.2.1                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.2.1>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`CREATED`                         | ``201`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.2.2                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.2.2>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`ACCEPTED`                        | ``202`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.2.3                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.2.3>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`NON_AUTHORITATIVE_INFORMATION`   | ``203`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.2.4                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.2.4>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`NO_CONTENT`                      | ``204`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.2.5                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.2.5>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`RESET_CONTENT`                   | ``205`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.2.6                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.2.6>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`PARTIAL_CONTENT`                 | ``206`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.2.7                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.2.7>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`MULTI_STATUS`                    | ``207`` | WEBDAV `RFC 2518, Section 10.2                                        |
|                                          |         | <http://www.webdav.org/specs/rfc2518.html#STATUS_207>`_               |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`IM_USED`                         | ``226`` | Delta encoding in HTTP,                                               |
|                                          |         | :rfc:`3229`, Section 10.4.1                                           |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`MULTIPLE_CHOICES`                | ``300`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.3.1                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.3.1>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`MOVED_PERMANENTLY`               | ``301`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.3.2                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.3.2>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`FOUND`                           | ``302`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.3.3                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.3.3>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`SEE_OTHER`                       | ``303`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.3.4                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.3.4>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`NOT_MODIFIED`                    | ``304`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.3.5                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.3.5>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`USE_PROXY`                       | ``305`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.3.6                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.3.6>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`TEMPORARY_REDIRECT`              | ``307`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.3.8                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.3.8>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`BAD_REQUEST`                     | ``400`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.4.1                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.1>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`UNAUTHORIZED`                    | ``401`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.4.2                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.2>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`PAYMENT_REQUIRED`                | ``402`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.4.3                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.3>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`FORBIDDEN`                       | ``403`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.4.4                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.4>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`NOT_FOUND`                       | ``404`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.4.5                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.5>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`METHOD_NOT_ALLOWED`              | ``405`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.4.6                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.6>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`NOT_ACCEPTABLE`                  | ``406`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.4.7                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.7>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`PROXY_AUTHENTICATION_REQUIRED`   | ``407`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.4.8                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.8>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`REQUEST_TIMEOUT`                 | ``408`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.4.9                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.9>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`CONFLICT`                        | ``409`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.4.10                                                               |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.10>`_ |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`GONE`                            | ``410`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.4.11                                                               |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.11>`_ |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`LENGTH_REQUIRED`                 | ``411`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.4.12                                                               |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.12>`_ |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`PRECONDITION_FAILED`             | ``412`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.4.13                                                               |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.13>`_ |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`REQUEST_ENTITY_TOO_LARGE`        | ``413`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.4.14                                                               |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.14>`_ |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`REQUEST_URI_TOO_LONG`            | ``414`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.4.15                                                               |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.15>`_ |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`UNSUPPORTED_MEDIA_TYPE`          | ``415`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.4.16                                                               |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.16>`_ |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`REQUESTED_RANGE_NOT_SATISFIABLE` | ``416`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.4.17                                                               |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.17>`_ |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`EXPECTATION_FAILED`              | ``417`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.4.18                                                               |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.18>`_ |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`UNPROCESSABLE_ENTITY`            | ``422`` | WEBDAV, `RFC 2518, Section 10.3                                       |
|                                          |         | <http://www.webdav.org/specs/rfc2518.html#STATUS_422>`_               |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`LOCKED`                          | ``423`` | WEBDAV `RFC 2518, Section 10.4                                        |
|                                          |         | <http://www.webdav.org/specs/rfc2518.html#STATUS_423>`_               |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`FAILED_DEPENDENCY`               | ``424`` | WEBDAV, `RFC 2518, Section 10.5                                       |
|                                          |         | <http://www.webdav.org/specs/rfc2518.html#STATUS_424>`_               |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`UPGRADE_REQUIRED`                | ``426`` | HTTP Upgrade to TLS,                                                  |
|                                          |         | :rfc:`2817`, Section 6                                                |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`INTERNAL_SERVER_ERROR`           | ``500`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.5.1                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.5.1>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`NOT_IMPLEMENTED`                 | ``501`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.5.2                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.5.2>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`BAD_GATEWAY`                     | ``502`` | HTTP/1.1 `RFC 2616, Section                                           |
|                                          |         | 10.5.3                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.5.3>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`SERVICE_UNAVAILABLE`             | ``503`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.5.4                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.5.4>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`GATEWAY_TIMEOUT`                 | ``504`` | HTTP/1.1 `RFC 2616, Section                                           |
|                                          |         | 10.5.5                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.5.5>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`HTTP_VERSION_NOT_SUPPORTED`      | ``505`` | HTTP/1.1, `RFC 2616, Section                                          |
|                                          |         | 10.5.6                                                                |
|                                          |         | <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.5.6>`_  |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`INSUFFICIENT_STORAGE`            | ``507`` | WEBDAV, `RFC 2518, Section 10.6                                       |
|                                          |         | <http://www.webdav.org/specs/rfc2518.html#STATUS_507>`_               |
+------------------------------------------+---------+-----------------------------------------------------------------------+
| :const:`NOT_EXTENDED`                    | ``510`` | An HTTP Extension Framework,                                          |
|                                          |         | :rfc:`2774`, Section 7                                                |
+------------------------------------------+---------+-----------------------------------------------------------------------+


.. data:: responses

   このディクショナリは、HTTP 1.1ステータスコードをW3Cの名前にマップしたものです。

   たとえば ``httplib.responses[httplib.NOT_FOUND]`` は ``'Not Found'`` となります。

   .. versionadded:: 2.5


.. _httpconnection-objects:

HTTPConnection オブジェクト
---------------------------

:class:`HTTPConnection` インスタンスには以下のメソッドがあります:


.. method:: HTTPConnection.request(method, url[, body[, headers]])

   このメソッドは、 HTTP 要求メソッド *method* およびセレクタ *url* を使って、要求をサーバに送ります。
   *body* 引数を指定する場合、ヘッダが終了した後に送信する文字列データでなければなりません。
   もしくは、開いているファイルオブジェクトを *body* に渡すこともできます。その場合、そのファイルの内容が送信されます。
   このファイルオブジェクトは、 ``fileno()`` と ``read()`` メソッドをサポートしている必要があります。
   ヘッダの Content-Length は自動的に正しい値に設定されます。 *headers*
   引数は要求と同時に送信される拡張 HTTP ヘッダの内容からなるマップ型でなくてはなりません。

   .. versionchanged:: 2.6
      *body* にファイルオブジェクトを渡せるようになりました

.. method:: HTTPConnection.getresponse()

   サーバに対して HTTP 要求を送り出した後に呼び出されなければりません。要求に対する応答を取得します。 :class:`HTTPResponse`
   インスタンスを返します。

   .. note::

      すべての応答を読み込んでからでなければ新しい要求をサーバに送ることはできないことに注意しましょう。


.. method:: HTTPConnection.set_debuglevel(level)

   デバッグレベル (印字されるデバッグ出力の量) を設定します。標準のデバッグレベルは ``0`` で、デバッグ出力を全く印字しません。

.. method:: HTTPConnection.set_tunnel(host,port=None, headers=None)

   HTTP トンネリング接続のホスト名とポート番号を設定します。
   通常はプロキシサーバを通して HTTP 接続を行うときに必要になります。

   ヘッダのパラメータは CONNECT リクエストで送信するために他の HTTP ヘッダにマッピングされます。

   .. versionadded:: 2.7

.. method:: HTTPConnection.connect()

   オブジェクトを生成するときに指定したサーバに接続します。


.. method:: HTTPConnection.close()

   サーバへの接続を閉じます。

上で説明した :meth:`request` メソッドを使うかわりに、以下の4つの関数を使用して要求をステップバイステップで送信することもできます。


.. method:: HTTPConnection.putrequest(request, selector[, skip_host[, skip_accept_encoding]])

   サーバへの接続が確立したら、最初にこのメソッドを呼び出さなくてはなりません。このメソッドは *request* 文字列、 *selector* 文字列、そして
   HTTP バージョン (``HTTP/1.1``) からなる一行を送信します。 ``Host:`` や ``Accept-Encoding:``
   ヘッダの自動送信を無効にしたい場合 (例えば別のコンテンツエンコーディングを受け入れたい場合) には、 *skip_host* や
   *skip_accept_encoding* を偽でない値に設定してください。


.. method:: HTTPConnection.putheader(header, argument[, ...])

   :rfc:`822` 形式のヘッダをサーバに送ります。この処理では、 *header* 、コロンとスペース、そして最初の引数からなる 1 行をサーバに送ります。
   追加の引数を指定した場合、継続して各行にタブ一つと引数の入った引数行が送信されます。


.. method:: HTTPConnection.endheaders()

   サーバに空行を送り、ヘッダ部が終了したことを通知します。


.. method:: HTTPConnection.send(data)

   サーバにデータを送ります。このメソッドは :meth:`endheaders`  が呼び出された直後で、かつ :meth:`getreply` が呼び出される
   前に使わなければなりません。


.. _httpresponse-objects:

HTTPResponse オブジェクト
-------------------------

:class:`HTTPResponse` インスタンスは以下のメソッドと属性を持ちます:


.. method:: HTTPResponse.read([amt])

   応答の本体全体か、 *amt* バイトまで読み出して返します。


.. method:: HTTPResponse.getheader(name[, default])

   ヘッダ *name* の内容を取得して返すか、該当するヘッダがない場合には *default* を返します。


.. method:: HTTPResponse.getheaders()

   (header, value) のタプルからなるリストを返します。

   .. versionadded:: 2.4

.. method:: HTTPResponse.fileno()

   ソケットの ``fileno`` を返します。

.. attribute:: HTTPResponse.msg

   応答ヘッダを含む :class:`mimetools.Message` インスタンスです。


.. attribute:: HTTPResponse.version

   サーバが使用した HTTP プロトコルバージョンです。10 は HTTP/1.0 を、 11 は HTTP/1.1 を表します。


.. attribute:: HTTPResponse.status

   サーバから返される状態コードです。


.. attribute:: HTTPResponse.reason

   サーバから返される応答の理由文です。


.. _httplib-examples:

例
--

以下は ``GET`` リクエストの送信方法を示した例です。 ::

   >>> import httplib
   >>> conn = httplib.HTTPConnection("www.python.org")
   >>> conn.request("GET", "/index.html")
   >>> r1 = conn.getresponse()
   >>> print r1.status, r1.reason
   200 OK
   >>> data1 = r1.read()
   >>> conn.request("GET", "/parrot.spam")
   >>> r2 = conn.getresponse()
   >>> print r2.status, r2.reason
   404 Not Found
   >>> data2 = r2.read()
   >>> conn.close()

.. Here is an example session that uses ``HEAD`` method. Note that ``HEAD`` method
   never returns any data. :

次の例のセッションでは、 ``HEAD`` メソッドを利用しています。
``HEAD`` メソッドは全くデータを返さないことに注目してください。 ::

   >>> import httplib
   >>> conn = httplib.HTTPConnection("www.python.org")
   >>> conn.request("HEAD","/index.html")
   >>> res = conn.getresponse()
   >>> print res.status, res.reason
   200 OK
   >>> data = res.read()
   >>> print len(data)
   0
   >>> data == ''
   True

以下は ``POST`` リクエストの送信方法を示した例です::

   >>> import httplib, urllib
   >>> params = urllib.urlencode({'spam': 1, 'eggs': 2, 'bacon': 0})
   >>> headers = {"Content-type": "application/x-www-form-urlencoded",
   ...            "Accept": "text/plain"}
   >>> conn = httplib.HTTPConnection("musi-cal.mojam.com:80")
   >>> conn.request("POST", "/cgi-bin/query", params, headers)
   >>> response = conn.getresponse()
   >>> print response.status, response.reason
   200 OK
   >>> data = response.read()
   >>> conn.close()

