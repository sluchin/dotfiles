:mod:`ssl` --- ソケットオブジェクトに対する TLS/SSL ラッパー
==============================================================

.. module:: ssl
   :synopsis: ソケットオブジェクトに対する TLS/SSL ラッパー

.. moduleauthor:: Bill Janssen <bill.janssen@gmail.com>

.. versionadded:: 2.6

.. sectionauthor::  Bill Janssen <bill.janssen@gmail.com>


.. index:: single: OpenSSL; (use in module ssl)

.. index:: TLS, SSL, Transport Layer Security, Secure Sockets Layer

このモジュールは Transport Layer Security (よく "Secure Sockets Layer"
という名前で知られています) 暗号化と、クライアントサイド、サーバーサイド
両方のネットワークソケットのためのピア認証の仕組みを提供しています。
このモジュールはOpenSSLライブラリを利用しています。
OpenSSLは、全てのモダンなUnixシステム、Windows、Mac OS X、その他幾つかの
OpenSSLがインストールされているプラットフォームで利用できます。

.. note::

   OSのソケットAPIに対して実装されているので、幾つかの挙動はプラットフォーム依存に
   なるかもしれません。
   インストールされているOpenSSLのバージョンの違いも挙動の違いの原因になるかもしれません。


このセクションでは、 ``ssl`` モジュールのオブジェクトと関数の解説します。
TLS, SSL, certificates に関するより一般的な情報は、末尾にある "See Also"
のセクションを参照してください。

このモジュールは1つのクラス、 :class:`ssl.SSLSocket` を提供します。
このクラスは :class:`socket.socket` クラスを継承していて、ソケットで
通信されるデータをSSLで暗号化・復号するソケットに似たラッパーになります。
また、このクラスは追加で、 :meth:`read` と :meth:`write` メソッド、
接続の相手側からの証明書を取得する :meth:`getpeercert` メソッド、
セキュア接続で使うための暗号方式を取得する :meth:`cipher`
メソッドをサポートしています。


関数、定数、例外
----------------

.. exception:: SSLError

   下層のSSL実装からのエラーを伝えるための例外です。
   このエラーは、低レベルなネットワークの上に載っている、高レベルな暗号化と認証レイヤーでの
   問題を通知します。
   このエラーは :exc:`socket.error` のサブタイプで、 :exc:`socket.error` は
   :exc:`IOError` のサブタイプです。


.. function:: wrap_socket (sock, keyfile=None, certfile=None, server_side=False, cert_reqs=CERT_NONE, ssl_version={see docs}, ca_certs=None, do_handshake_on_connect=True, suppress_ragged_eofs=True, ciphers=None)

   :class:`socket.socket` のインスタンス ``sock`` を受け取り、 :class:`socket.socket` のサブタイプである
   :class:`ssl.SSLSocket` のインスタンスを返します。 :class:`ssl.SSLSocket` は低レイヤの
   ソケットをSSLコンテキストでラップします。
   クライアントサイドソケットにおいて、コンテキストの生成は遅延されます。
   つまり、低レイヤのソケットがまだ接続されていない場合、コンテキストの生成はそのソケットの
   :meth:`connect` メソッドが呼ばれた後に行われます。
   サーバーサイドソケットの場合、そのソケットに接続先が居なければそれは listen 用ソケットだと
   判断されます。 :meth:`accept` メソッドで生成されるクライアント接続に対してのサーバーサイド
   SSLラップは自動的に行われます。そのクライアント接続に対して :func:`wrap_socket` を実行すると
   :exc:`SSLError` が発生します。

   オプションの ``keyfile`` と ``certfile`` 引数は、接続のこちら側を識別するために利用される
   証明書を含むファイルを指定します。
   証明書がどのように ``certfile`` に格納されるかについてのより詳しい情報は、 :ref:`ssl-certificates`
   を参照してください。

   多くの場合、証明書と同じファイルに秘密鍵も格納されています。この場合、 ``certfile``
   引数だけが必要とされます。
   秘密鍵が証明書と別のファイルに格納されている場合、両方の引数を指定しなければなりません。
   秘密鍵が ``certfile`` に格納されている場合、秘密鍵は証明書チェインの最初の証明書よりも先に
   ないといけません。 ::

      -----BEGIN RSA PRIVATE KEY-----
      ... (private key in base64 encoding) ...
      -----END RSA PRIVATE KEY-----
      -----BEGIN CERTIFICATE-----
      ... (certificate in base64 PEM encoding) ...
      -----END CERTIFICATE-----

   ``server_side`` 引数は真偽値で、このソケットがサーバーサイドとクライアントサイドのどちらの
   動作をするのかを指定します。

   ``cert_reqs`` 引数は、接続の相手側からの証明書を必要とするかどうかと、
   それを検証(validate)するかどうかを指定します。
   これは次の3つの定数のどれかで無ければなりません:
   :const:`CERT_NONE` (証明書は無視されます), :const:`CERT_OPTIONAL`
   (必要としないが、提供された場合は検証する), :const:`CERT_REQUIRED`
   (証明書を必要とし、検証する)。
   もしこの引数が :const:`CERT_NONE` 以外だった場合、 ``ca_certs`` 引数はCA証明書ファイルを
   指定していなければなりません。

   ``ca_certs`` ファイルは、接続の相手側から渡された証明書を検証するために使う、
   一連のCA証明書を結合したものを含んでいます。
   このファイル内にどう証明書を並べるかについての詳しい情報は :ref:`ssl-certificates`
   を参照してください。

   ``ssl_version`` 引数は、使用するSSLプロトコルのバージョンを指定します。
   通常、サーバー側が特定のプロトコルバージョンを選び、クライアント側は
   サーバーの選んだプロトコルを受け入れなければなりません。
   ほとんどのバージョンは他のバージョンと互換性がありません。
   もしこの引数が指定されなかった場合、クライアントサイドでは、デフォルトの
   SSLバージョンは SSLv3 になります。サーバーサイドでは SSLv23 です。
   これらのバージョンは、できるだけの互換性を確保するように選ばれています。

   次のテーブルは、どのクライアント側のバージョンがどのサーバー側のバージョンに
   接続できるかを示しています。

     .. table::

       ========================  =========  =========  ==========  =========
        *client* / **server**    **SSLv2**  **SSLv3**  **SSLv23**  **TLSv1**
       ------------------------  ---------  ---------  ----------  ---------
        *SSLv2*                    yes        no         yes         no
        *SSLv3*                    yes        yes        yes         no
        *SSLv23*                   yes        no         yes         no
        *TLSv1*                    no         no         yes         yes
       ========================  =========  =========  ==========  =========

   .. note::

      どの接続が成功するかは、 OpenSSL のバージョンに依存して大きく変わります。
      例えば、いくつか古めのバージョンの OpenSSL (OS X 10.4 の 0.9.7l など)
      では、 SSLv2 クライアントは SSLv23 サーバーに接続できませんでした。
      また、 OpenSSL 1.0.0 の初期では、 SSLv23 クライアントは明示的に SSLv2
      cipher を有効にしない限りは SSLv2 接続を試みずませんでした。
      このバージョンで SSLv2 を有効にするには、 *ciphers* 引数に ``"ALL"`` か
      ``"SSLv2"`` を指定することができます。

   *ciphers* 引数はこの SSL オブジェクトで利用可能な cipher を指定します。
   これは、 `OpenSSL cipher list format
   <http://www.openssl.org/docs/apps/ciphers.html#CIPHER_LIST_FORMAT>`_
   にある文字列のどれかでなければなりません。

   ``do_handshake_on_connect`` 引数は、 :meth:`socket.connect` の後に自動的に
   SSLハンドシェイクを行うか、それともアプリケーションが明示的に :meth:`SSLSocket.do_handshake`
   メソッドを実行するかを指定します。
   :meth:`SSLSocket.do_handshake` を明示的に呼びだすことで、ハンドシェイクによる
   ソケットI/Oのブロッキング動作を制御できます。

   ``suppress_ragged_eofs`` 引数は、 :meth:`SSLSocket.read` メソッドが、接続先から
   予期しないEOFを受け取った時に通知する方法を指定します。
   :const:`True` (デフォルト) の場合、下位のソケットレイヤーから予期せぬEOFエラーが来た場合、
   通常のEOFを返します。 :const:`False` の場合、呼び出し元に例外を投げて通知します。

   .. versionchanged:: 2.7
      新しいオプション引数 *ciphers*

.. function:: RAND_status()

   SSL 擬似乱数生成器が十分なランダム性(randomness)を受け取っている時に真を、
   それ以外の場合は偽を返します。
   :func:`ssl.RAND_egd` と :func:`ssl.RAND_add` を使って擬似乱数生成機に
   ランダム性を加えることができます。

.. function:: RAND_egd(path)

   もしエントロピー収集デーモン(EGD=entropy-gathering daemon)が動いていて、
   ``path`` がEGDへのソケットのパスだった場合、この関数はそのソケットから
   256バイトのランダム性を読み込み、SSL擬似乱数生成器にそれを渡すことで、
   生成される暗号鍵のセキュリティを向上させることができます。
   これは、より良いランダム性のソースが無いシステムでのみ必要です。

   エントロピー収集デーモンについては、
   http://egd.sourceforge.net/ や http://prngd.sourceforge.net/
   を参照してください。

.. function:: RAND_add(bytes, entropy)

   与えられた ``bytes`` をSSL擬似乱数生成器に混ぜます。
   ``entropy`` 引数(float値)は、その文字列に含まれるエントロピーの下限(lower bound)です。
   (なので、いつでも :const:`0.0` を使うことができます。)
   エントロピーのソースについてのより詳しい情報は、 :rfc:`1750` を参照してください。

.. function:: cert_time_to_seconds(timestring)

   証明書内の "notBefore" や "notAfter" で使われている日時の文字列表現 *timestring*
   から、通常のエポック秒を含むfloat値にして返します。

   例です。 ::

     >>> import ssl
     >>> ssl.cert_time_to_seconds("May  9 00:00:00 2007 GMT")
     1178694000.0
     >>> import time
     >>> time.ctime(ssl.cert_time_to_seconds("May  9 00:00:00 2007 GMT"))
     'Wed May  9 00:00:00 2007'
     >>>

.. function:: get_server_certificate (addr, ssl_version=PROTOCOL_SSLv3, ca_certs=None)

   SSLで保護されたサーバーのアドレス ``addr`` を (*hostname*, *port-number*)
   の形で受け取り、そのサーバーから証明書を取得し、それを PEMエンコードされた
   文字列として返します。
   ``ssl_version`` が指定された場合は、サーバーに接続を試みるときに
   そのバージョンのSSLプロトコルを利用します。
   ``ca_certs`` が指定された場合、それは :func:`wrap_socket` の同名の引数と同じフォーマットで、
   ルート証明書のリストを含むファイルでなければなりません。
   この関数はサーバー証明書をルート証明書リストに対して認証し、認証が失敗した
   場合にこの関数も失敗します。

.. function:: DER_cert_to_PEM_cert (DER_cert_bytes)

   DERエンコードされたバイト列として与えられた証明書から、
   PEMエンコードされたバージョンの同じ証明書を返します。

.. function:: PEM_cert_to_DER_cert (PEM_cert_string)

   PEM 形式のASCII文字列として与えられた証明書から、
   同じ証明書をDERエンコードしたバイト列を返します。

.. data:: CERT_NONE

   ソケット接続先からの証明書やその認証を必要としないときに、 :func:`sslobject`
   の ``cert_reqs`` 引数に指定する値。

.. data:: CERT_OPTIONAL

   ソケット接続先からの証明書を必要としないが、もし証明書があればそれを認証する場合に
   :func:`sslobject` の ``cert_reqs`` 引数に指定する値。
   この設定を利用するときは、 ``ca_certs`` 引数に有効な証明書認証ファイルが渡される
   必要があることに注意してください。

.. data:: CERT_REQUIRED

   ソケット接続先からの証明書とその認証が必要なときに :func:`sslobject` の
   ``cert_reqs`` 引数に指定する値。
   この設定を利用するときは、 ``ca_certs`` 引数に有効な証明書認証ファイルが渡される
   必要があることに注意してください。

.. data:: PROTOCOL_SSLv2

   チャンネル暗号化プロトコルに SSL バージョン2を選択する。

   このプロトコルは、 OpenSSL が OPENSSL_NO_SSL2 フラグが有効な状態で
   コンパイルされている場合には利用できません。

   .. warning::

      SSL version 2 は非セキュアです。
      このプロトコルは強く非推奨です。

.. data:: PROTOCOL_SSLv23

   .. Selects SSL version 2 or 3 as the channel encryption protocol.  This is a
      setting to use with servers for maximum compatibility with the other end of
      an SSL connection, but it may cause the specific ciphers chosen for the
      encryption to be of fairly low quality.

   チャンネル暗号化プロトコルとしてSSLバージョン2か3を選択します。
   これはサーバー側が相手側への最大限の互換性を確保するための設定です。
   しかし、この設定では非常に低い品質の暗号化が選ばれる可能性があります。

   .. todo::
      最後の一文が自信ない.

.. data:: PROTOCOL_SSLv3

   チャンネル暗号化プロトコルとしてSSLバージョン3をを選択します。
   クライアントにとって、これは最大限に互換性の高いSSLの種類です。

.. data:: PROTOCOL_TLSv1

   チャンネル暗号化プロトコルとしてTLSバージョン1を選択します。
   これは最も現代的で、接続の両サイドが利用できる場合は、たぶん最も安全な選択肢です。

.. data:: OPENSSL_VERSION

   インタプリタによってロードされた OpenSSL ライブラリのバージョン文字列::

    >>> ssl.OPENSSL_VERSION
    'OpenSSL 0.9.8k 25 Mar 2009'

   .. versionadded:: 2.7

.. data:: OPENSSL_VERSION_INFO

   OpenSSL ライブラリのバージョン情報を表す5つの整数のタプル::

    >>> ssl.OPENSSL_VERSION_INFO
    (0, 9, 8, 11, 15)

   .. versionadded:: 2.7

.. data:: OPENSSL_VERSION_NUMBER

   1つの整数の形式の、 OpenSSL ライブラリの生のバージョン番号::

    >>> ssl.OPENSSL_VERSION_NUMBER
    9470143L
    >>> hex(ssl.OPENSSL_VERSION_NUMBER)
    '0x9080bfL'

   .. versionadded:: 2.7


SSLSocket オブジェクト
----------------------

.. method:: SSLSocket.read([nbytes=1024])

   ``nbytes`` 以下のバイト列を SSL暗号化されたチャンネルから受信してそれを返します。

.. method:: SSLSocket.write(data)

   ``data`` をSSLチャンネルを使って暗号化した上で接続の相手側へ送ります。
   書き込めたバイト数を返します。

.. method:: SSLSocket.getpeercert(binary_form=False)

   接続先に証明書が無い場合、 ``None`` を返します。

   ``binary_form`` が :const:`False` で接続先から証明書を取得した場合、
   このメソッドは :class:`dict` のインスタンスを返します。
   証明書が認証されていない場合、辞書は空です。
   証明書が認証されていた場合、 ``subject`` (証明書が発行された principal),
   ``notafter`` (その証明書がそれ以降信頼できなくなる時間) が格納された辞書を返します。
   証明書は既に認証されているので、 ``notBefore`` と ``issuer`` フィールドは返されません。
   証明書が *Subject Alternative Name* 拡張(:rfc:`3280` を参照)のインスタンスを
   格納していた場合、 ``subjectAltName`` キーも辞書に含まれます。

   "subject" フィールドは、証明書の principal に格納されているRDN
   (relative distinguishued name)のシーケンスを格納したタプルで、各RDNは
   name-value ペアのシーケンスです。 ::

      {'notAfter': 'Feb 16 16:54:50 2013 GMT',
       'subject': ((('countryName', u'US'),),
                   (('stateOrProvinceName', u'Delaware'),),
                   (('localityName', u'Wilmington'),),
                   (('organizationName', u'Python Software Foundation'),),
                   (('organizationalUnitName', u'SSL'),),
                   (('commonName', u'somemachine.python.org'),))}

   ``binary_form`` 引数が :const:`True` だった場合、証明書が渡されていれば
   このメソッドはDERエンコードされた証明書全体をバイト列として返し、
   接続先が証明書を提示しなかった場合は :const:`None` を返します。
   この戻り値は認証とは独立しています。認証が要求されていた場合 (:const:`CERT_OPTIONAL`
   か :const:`CERT_REQUIRED`) その証明書は認証されますが、 :const:`CERT_NONE`
   が接続時に利用された場合、証明書があったとしても、それは認証されません。

.. method:: SSLSocket.cipher()

   利用されている暗号の名前、その暗号の利用を定義しているSSLプロトコルのバージョン、
   利用されている鍵のbit長の3つの値を含むタプルを返します。
   もし接続が確立されていない場合、 ``None`` を返します。

.. method:: SSLSocket.do_handshake()

   TLS/SSL ハンドシェイクを実施します。
   ノンブロッキングソケットで利用された場合、ハンドシェイクが完了するまでは
   :exc:`SSLError` の ``arg[0]`` に :const:`SSL_ERROR_WANT_READ` か
   :const:`SSL_ERROR_WANT_WRITE` が設定された例外が発生し、このメソッドを繰り返し
   実行しなければなりません。
   例えば、ブロッキングソケットを真似する場合は次のようになります。 ::

        while True:
            try:
                s.do_handshake()
                break
            except ssl.SSLError, err:
                if err.args[0] == ssl.SSL_ERROR_WANT_READ:
                    select.select([s], [], [])
                elif err.args[0] == ssl.SSL_ERROR_WANT_WRITE:
                    select.select([], [s], [])
                else:
                    raise

.. method:: SSLSocket.unwrap()

   SSLシャットダウンハンドシェイクを実行します。
   これは下位レイヤーのソケットからTLSレイヤーを取り除き、下位レイヤーの
   ソケットオブジェクトを返します。
   これは暗号化されたオペレーションから暗号化されていない接続に移行するときに利用されます。
   以降の通信には、このメソッドが返したソケットインスタンスを利用するべきです。
   元のソケットインスタンスは unwrap 後に正しく機能しないかもしれません。

.. index:: single: certificates

.. index:: single: X509 certificate

.. _ssl-certificates:

証明書
-------

証明書を大まかに説明すると、公開鍵/秘密鍵システムの一種です。
このシステムでは、各 *principal* (これはマシン、人、組織などです) は、
ユニークな2つの暗号鍵を割り当てられます。1つは公開され、 *公開鍵(public key)*
と呼ばれます。もう一方は秘密にされ、 *秘密鍵(private key)* と呼ばれます。
2つの鍵は関連しており、片方の鍵で暗号化したメッセージは、もう片方の鍵 **のみ**
で復号できます。

証明書は2つの principal の情報を含んでいます。
証明書は *subject* 名とその公開鍵を含んでいます。
また、もう一つの principal である *発行者(issuer)* からの、 subject が本人であることと、
その公開鍵が正しいことの宣言(statement)を含んでいます。
発行者からの宣言は、その発行者の秘密鍵で署名されています。発行者の秘密鍵は発行者しか
知りませんが、誰もがその発行者の公開鍵を利用して宣言を復号し、証明書内の別の情報と
比較することで認証することができます。
証明書はまた、その証明書が有効である期限に関する情報も含んでいます。
この期限は "notBefore" と "notAfter" と呼ばれる2つのフィールドで表現されています。

Python において証明書を利用する場合、クライアントもサーバーも自分を証明するために
証明書を利用することができます。ネットワーク接続の相手側に証明書の提示を要求する事ができ、
そのクライアントやサーバーが認証を必要とするならその証明書を認証することができます。
認証が失敗した場合、接続は例外を発生させます。
認証は下位層のOpenSSLフレームワークが自動的に行います。
アプリケーションは認証機構について意識する必要はありません。
しかし、アプリケーションは認証プロセスのために幾つかの証明書を提供する必要があるかもしれません。

Python は証明書を格納したファイルを利用します。そのファイルは "PEM" (:rfc:`1422` 参照)
フォーマットという、ヘッダー行とフッター行の間にbase-64エンコードされた形をとっている
必要があります。 ::

      -----BEGIN CERTIFICATE-----
      ... (certificate in base64 PEM encoding) ...
      -----END CERTIFICATE-----

Pythonが利用する証明書を格納したファイルは、ときには *証明書チェイン(certificate chain)*
と呼ばれる証明書のシーケンスを格納します。
このチェインは、まずクライアントやサーバー自体の principal の証明書で始まらなければなりません。
それ以降に続く証明書は、手前の証明書の発行者(issuer)の証明書になり、最後にsubject と発行者が
同じ *自己署名(self-signed)* 証明書で終わります。この最後の証明書は *ルート証明書(root certificate*
と呼ばれます。
これらの証明書チェインは1つの証明書ファイルに結合されなければなりません。
例えば、3つの証明書からなる証明書チェインがあるとします。私たちのサーバーの証明書から、
私たちのサーバーに署名した認証局の証明書、そして認証局の証明書を発行した機関のルート証明書です。 ::

      -----BEGIN CERTIFICATE-----
      ... (certificate for your server)...
      -----END CERTIFICATE-----
      -----BEGIN CERTIFICATE-----
      ... (the certificate for the CA)...
      -----END CERTIFICATE-----
      -----BEGIN CERTIFICATE-----
      ... (the root certificate for the CA's issuer)...
      -----END CERTIFICATE-----

もし相手から送られてきた証明書の認証をしたい場合、信頼している各発行者の
証明書チェインが入った "CA certs" ファイルを提供する必要があります。
繰り返しますが、このファイルは単純に、各チェインを結合しただけのものです。
認証のために、Pythonはそのファイルの中の最初にマッチしたチェインを利用します。

幾つかの "standard" ルート証明書が、幾つかの認証機関から入手できます:
`CACert.org <http://www.cacert.org/index.php?id=3>`_, `Thawte
<http://www.thawte.com/roots/>`_, `Verisign
<http://www.verisign.com/support/roots.html>`_, `Positive SSL
<http://www.PositiveSSL.com/ssl-certificate-support/cert_installation/UTN-USERFirst-Hardware.crt>`_
(python.org が利用しています), `Equifax and GeoTrust
<http://www.geotrust.com/resources/root_certificates/index.asp>`_.

一般的に、 SSL3 か TLS1 を利用している場合、"CA certs" ファイルに全てのチェインを
保存する必要はありません。接続先はそれ自身の証明書からルート証明書までの証明書チェインを
送ってくるはずで、"CA certs" にはルート証明書だけあれば充分なはずです。
証明書チェインを組み立てる方法についてのより詳しい情報は、 :rfc:`4158` を参照してください。

SSL暗号化接続サービスを提供するサーバーを建てる場合、適切な証明書を取得するには、
認証局から買うなどの幾つかの方法があります。また、自己署名証明書を作るケースもあります。
OpenSSLを使って自己署名証明書を作るには、次のようにします。 ::

  % openssl req -new -x509 -days 365 -nodes -out cert.pem -keyout cert.pem
  Generating a 1024 bit RSA private key
  .......++++++
  .............................++++++
  writing new private key to 'cert.pem'
  -----
  You are about to be asked to enter information that will be incorporated
  into your certificate request.
  What you are about to enter is what is called a Distinguished Name or a DN.
  There are quite a few fields but you can leave some blank
  For some fields there will be a default value,
  If you enter '.', the field will be left blank.
  -----
  Country Name (2 letter code) [AU]:US
  State or Province Name (full name) [Some-State]:MyState
  Locality Name (eg, city) []:Some City
  Organization Name (eg, company) [Internet Widgits Pty Ltd]:My Organization, Inc.
  Organizational Unit Name (eg, section) []:My Group
  Common Name (eg, YOUR name) []:myserver.mygroup.myorganization.com
  Email Address []:ops@myserver.mygroup.myorganization.com
  %

自己署名証明書の欠点は、それ自身がルート証明書であり、他の人はその証明書を持っていない
(そして信頼しない)ことです。

例
----

SSLサポートをテストする
^^^^^^^^^^^^^^^^^^^^^^^

インストールされているPythonがSSLをサポートしているかどうかをテストするために、
ユーザーコードは次のイディオムを利用することができます。 ::

   try:
       import ssl
   except ImportError:
       pass
   else:
       ... # do something that requires SSL support


クライアントサイドの処理
^^^^^^^^^^^^^^^^^^^^^^^^^

次の例では、SSLサーバーに接続し、サーバーのアドレスと証明書を表示し、
数バイト送信し、レスポンスの一部を読み込みます。 ::

   import socket, ssl, pprint

   s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

   # サーバーからの証明書を要求する
   ssl_sock = ssl.wrap_socket(s,
                              ca_certs="/etc/ca_certs_file",
                              cert_reqs=ssl.CERT_REQUIRED)

   ssl_sock.connect(('www.verisign.com', 443))

   print repr(ssl_sock.getpeername())
   print ssl_sock.cipher()
   print pprint.pformat(ssl_sock.getpeercert())

   # シンプルなHTTPリクエストを送信する。 -- 実際のコードではhttplibを利用してください。
   ssl_sock.write("""GET / HTTP/1.0\r
   Host: www.verisign.com\r\n\r\n""")

   # 1チャンクのデータを読む。
   # サーバーから返されたデータの全てを読み込むとは限らない。
   data = ssl_sock.read()

   # SSLSocketを閉じると下位レイヤーのソケットも閉じられることに注目してください。
   ssl_sock.close()

2007年9月時点で、このプログラムによって表示される証明書は次のようになります。 ::

      {'notAfter': 'May  8 23:59:59 2009 GMT',
       'subject': ((('serialNumber', u'2497886'),),
                   (('1.3.6.1.4.1.311.60.2.1.3', u'US'),),
                   (('1.3.6.1.4.1.311.60.2.1.2', u'Delaware'),),
                   (('countryName', u'US'),),
                   (('postalCode', u'94043'),),
                   (('stateOrProvinceName', u'California'),),
                   (('localityName', u'Mountain View'),),
                   (('streetAddress', u'487 East Middlefield Road'),),
                   (('organizationName', u'VeriSign, Inc.'),),
                   (('organizationalUnitName',
                     u'Production Security Services'),),
                   (('organizationalUnitName',
                     u'Terms of use at www.verisign.com/rpa (c)06'),),
                   (('commonName', u'www.verisign.com'),))}

これは不完全な形の ``subject`` フィールドです。


サーバーサイドの処理
^^^^^^^^^^^^^^^^^^^^^

サーバーサイドの処理では、通常、サーバー証明書と秘密鍵がそれぞれファイルに格納された形で必要です。
ソケットを開き、ポートにバインドし、そのソケットの :meth:`listen` を呼び、クライアントからの
接続を待ちます。 ::

   import socket, ssl

   bindsocket = socket.socket()
   bindsocket.bind(('myaddr.mydomain.com', 10023))
   bindsocket.listen(5)

誰かが接続してきた場合、 :meth:`accept` を呼んで新しいソケットを作成し、
:func:`wrap_socket` を利用してサーバーサイドSSLコンテキストを生成します。 ::

   while True:
       newsocket, fromaddr = bindsocket.accept()
       connstream = ssl.wrap_socket(newsocket,
                                    server_side=True,
                                    certfile="mycertfile",
                                    keyfile="mykeyfile",
                                    ssl_version=ssl.PROTOCOL_TLSv1)
       try:
           deal_with_client(connstream)
       finally:
           connstream.shutdown(socket.SHUT_RDWR)
           connstream.close()

そして、 ``connstream`` からデータを読み、クライアントと切断する(あるいはクライアントが
切断してくる)まで何か処理をします。 ::

   def deal_with_client(connstream):
       data = connstream.read()
       # null data means the client is finished with us
       while data:
           if not do_something(connstream, data):
               # we'll assume do_something returns False
               # when we're finished with client
               break
           data = connstream.read()
       # finished with client

そして新しいクライアント接続のために listen に戻ります。

.. seealso::

   Class :class:`socket.socket`
            下位レイヤーの :mod:`socket` クラスのドキュメント

   `TLS (Transport Layer Security) and SSL (Secure Socket Layer) <http://www3.rad.com/networks/applications/secure/tls.htm>`_
       Debby Koren

   `RFC 1422: Privacy Enhancement for Internet Electronic Mail: Part II: Certificate-Based Key Management <http://www.ietf.org/rfc/rfc1422>`_
       Steve Kent

   `RFC 1750: Randomness Recommendations for Security <http://www.ietf.org/rfc/rfc1750>`_
       D. Eastlake et. al.

   `RFC 3280: Internet X.509 Public Key Infrastructure Certificate and CRL Profile <http://www.ietf.org/rfc/rfc3280>`_
       Housley et. al.
