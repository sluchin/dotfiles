:mod:`xmlrpclib` --- XML-RPC クライアントアクセス
=================================================

.. module:: xmlrpclib
   :synopsis: XML-RPC client access.
.. moduleauthor:: Fredrik Lundh <fredrik@pythonware.com>
.. sectionauthor:: Eric S. Raymond <esr@snark.thyrsus.com>

.. note::
   :mod:`xmlrpclib` モジュールは、Python 3では :mod:`xmlrpc.client` にリネームされました。
   :term:`2to3` ツールは、自動的にソースコードのimportをPython 3用に修正します。


.. XXX Not everything is documented yet.  It might be good to describe
   Marshaller, Unmarshaller, getparser, dumps, loads, and Transport.

.. versionadded:: 2.2

XML-RPCはXMLを利用した遠隔手続き呼び出し(Remote Procedure Call)の一種で、HTTPをトランスポートとして使用します。XML-
RPCでは、クライアントはリモートサーバ(URIで指定されたサーバ)上のメソッドをパラメータを指定して呼
び出し、構造化されたデータを取得します。このモジュールは、XML-RPCクライアントの開発をサポートしており、Pythonオブジェクトに適合する転送用XMLの
変換の全てを行います。


.. class:: ServerProxy(uri[, transport[, encoding[, verbose[,  allow_none[, use_datetime]]]]])

   :class:`ServerProxy` は、リモートのXML-RPCサーバとの通信を管理するオブジェクトです。最初のパラメータはURI(Uniform
   Resource Indicator)で、通常はサーバのURLを指定します。2番目のパラメータにはトランスポート・ファクトリ
   を指定する事ができます。トランスポート・ファクトリを省略した場合、URLが https:
   ならモジュール内部の :class:`SafeTransport` インスタンスを使用し、それ以外の場合にはモジュール内部の
   :class:`Transport` インスタンスを使用します。オプションの 3 番目の引数はエンコード方法で、デフォルトでは
   UTF-8 です。オプションの 4 番目の引数はデバッグフラグです。 *allow_none* が真の場合、Python の定数 ``None`` は XML
   に翻訳されます; デフォルトの動作は ``None`` に対して :exc:`TypeError` を送出します。この仕様は XML-RPC
   仕様でよく用いられている拡張ですが、全てのクライアントやサーバでサポートされているわけではありません; 詳細記述については
   http://ontosys.com/xml-rpc/extensions.html を参照してください。
   *use_datetime* フラグは :class:`datetime.datetime` のオブジェクトとして日付/時刻を表現する時に使用し、デフォルトでは
   false に設定されています。
   呼び出しに :class:`datetime.datetime` のオブジェクトを渡すことができます。

   HTTP及びHTTPS通信の両方で、 ``http://user:pass@host:port/path`` のよう
   なHTTP基本認証のための拡張URL構文をサポートしています。 ``user:pass`` はbase64でエンコードしてHTTPの'Authorization
   'ヘッダとなり、XML-RPCメソッド呼び出し時に接続処理の一部としてリモートサーバに送信されます。リモート
   サーバが基本認証を要求する場合のみ、この機能を利用する必要があります。

   生成されるインスタンスはリモートサーバへのプロクシオブジェクトで、RPC呼び出しを行う為のメソッドを持ちます。リモートサーバがイントロスペクション
   APIをサポートしている場合は、リモートサーバのサポートするメソッドを検索 (サービス検索)やサーバのメタデータの取得なども行えます。

   :class:`ServerProxy` インスタンスのメソッドは引数としてPythonの基礎型とオ
   ブジェクトを受け取り、戻り値としてPythonの基礎型かオブジェクトを返します。
   以下の型をXMLに変換(XMLを通じてマーシャルする)する事ができます
   (特別な指定がない限り、逆変換でも同じ型として変換されます):

   +---------------------+-------------------------------------------------------------------------+
   | 名前                | 意味                                                                    |
   +=====================+=========================================================================+
   | boolean             | 定数 :const:`True` と :const:`False`                                    |
   +---------------------+-------------------------------------------------------------------------+
   | 整数                | そのまま                                                                |
   +---------------------+-------------------------------------------------------------------------+
   | 浮動小数点          | そのまま                                                                |
   +---------------------+-------------------------------------------------------------------------+
   | 文字列              | そのまま                                                                |
   +---------------------+-------------------------------------------------------------------------+
   | 配列                | 変換可能な要素を含むPythonシーケンス。戻り値はリスト。                  |
   +---------------------+-------------------------------------------------------------------------+
   | 構造体              | Pythonの辞書。キーは文字列のみ。全ての値は変換可能でなくてはならない。  |
   |                     | ユーザー定義型を渡すこともできます。 *__dict__*                         |
   |                     | の属性のみ転送されます。                                                |
   +---------------------+-------------------------------------------------------------------------+
   | 日付                | エポックからの経過秒数(:class:`DateTime` クラスのインスタンスとして     |
   |                     | 渡す) もしくは、 :class:`datetime.datetime` のインスタンス              |
   +---------------------+-------------------------------------------------------------------------+
   | バイナリ            | :class:`Binary` ラッパクラスのインスタンス                              |
   +---------------------+-------------------------------------------------------------------------+

   上記のXML-RPCでサポートする全データ型を使用することができます。メソッド呼び出し時、XML-
   RPCサーバエラーが発生すると :exc:`Fault` インスタンスを送出し、HTTP/HTTPSトランスポート層でエラーが発生した場合には
   :exc:`ProtocolError` を送出します。 :exc:`Error` をベースとする
   :exc:`Fault` と :exc:`ProtocolError` の両方が発生します。 Python 2.2以降では組み込み型のサ
   ブクラスを作成する事ができますが、現在のところxmlrpclibではそのようなサブクラスのインスタンスをマーシャルすることはできません。

   文字列を渡す場合、 ``<``, ``>``, ``&`` などのXMLで特殊な意味を持つ文字は自動的にエスケープされます。
   しかし、ASCII値0〜31の制御文字(もちろん、タブ'TAB',改行'LF',リターン'CR'は除く)などのXMLで使用することのできない文字を使用することはできず、
   使用するとそのXML-RPCリクエストはwell-formedなXMLとはなりません。
   そのような文字列を渡す必要がある場合は、後述の :class:`Binary` ラッパクラスを使用してください。

   :class:`Server` は、上位互換性の為に :class:`ServerProxy` の別名として残され
   ています。新しいコードでは :class:`ServerProxy` を使用してください。

   .. versionchanged:: 2.5
      *use_datetime* フラグが追加されました

   .. versionchanged:: 2.6
      ニュースタイルクラス(:term:`new-style class`)も、
      *__dict__* 属性を持っていて、特別な方法でマーシャルされている親クラスを
      持っていなければ、渡すことができます。


.. seealso::

   `XML-RPC HOWTO <http://www.tldp.org/HOWTO/XML-RPC-HOWTO/index.html>`_
      週種類のプログラミング言語で記述された XML-RPCの操作とクライアントソフトウェアの素晴らしい説明が掲載されています。
      XML- RPCクライアントの開発者が知っておくべきことがほとんど全て記載されています。

   `XML-RPC-Hacks page <http://xmlrpc-c.sourceforge.net/hacks.php>`_
      イントロスペクションとマルチコールをサポートしているオープンソースの拡張ライブラリについて説明しています。

   `XML-RPC Introspection <http://xmlrpc-c.sourceforge.net/introspection.html>`_
      インストロペクションをサポートする、 XML-RPC プロトコルの拡張を解説しています。

   `XML-RPC Specification <http://www.xmlrpc.com/spec>`_
      公式の仕様

   `Unofficial XML-RPC Errata <http://effbot.org/zone/xmlrpc-errata.htm>`_
      Fredrik Lundh による "unofficial errata, intended to clarify certain
      details in the XML-RPC specification, as well as hint at
      'best practices' to use when designing your own XML-RPC
      implementations."

.. _serverproxy-objects:

ServerProxy オブジェクト
------------------------

:class:`ServerProxy` インスタンスの各メソッドはそれぞれXML-RPCサーバの遠隔
手続き呼び出しに対応しており、メソッドが呼び出されると名前と引数をシグネチャとしてRPCを実行します(同じ名前のメソッドでも、異なる引数シグネチャに
よってオーバロードされます)。RPC実行後、変換された値を返すか、または
:class:`Fault` オブジェクトもしくは :class:`ProtocolError` オブジェクトでエラーを通知します。

予約メンバ :attr:`system` から、XMLイントロスペクションAPIの一般的なメソッドを利用する事ができます。


.. method:: ServerProxy.system.listMethods()

   XML-RPCサーバがサポートするメソッド名(system以外)を格納する文字列のリストを返します。


.. method:: ServerProxy.system.methodSignature(name)

   XML-RPCサーバで実装されているメソッドの名前を指定し、利用可能なシグネチャの配列を取得します。シグネチャは型のリストで、先頭の型は戻り値の型を示
   し、以降はパラメータの型を示します。

   XML-RPCでは複数のシグネチャ(オーバロード)を使用することができるので、単独のシグネチャではなく、シグネチャのリストを返します。

   シグネチャは、メソッドが使用する最上位のパラメータにのみ適用されます。例えばあるメソッドのパラメータが構造体の配列で戻り値が文字列の場合、シグネ
   チャは単に"文字列, 配列" となります。パラメータが三つの整数で戻り値が文字列の場合は"文字列, 整数, 整数, 整数"となります。

   メソッドにシグネチャが定義されていない場合、配列以外の値が返ります。 Pythonでは、この値はlist以外の値となります。


.. method:: ServerProxy.system.methodHelp(name)

   XML-RPCサーバで実装されているメソッドの名前を指定し、そのメソッドを解説する文書文字列を取得します。文書文字列を取得できない場合は空文字列を返し
   ます。文書文字列にはHTMLマークアップが含まれます


.. _boolean-objects:

Boolean オブジェクト
--------------------

このクラスは全てのPythonの値で初期化することができ、生成されるインスタンスは指定した値の真偽値によってのみ決まります。Booleanという名前から想像
される通りに各種のPython演算子を実装しており、 :meth:`__cmp__`, :meth:`__repr__`, :meth:`__int__`,
:meth:`__nonzero__` で定義される演算子を使用することができます。

以下のメソッドは、主に内部的にアンマーシャル時に使用されます:


.. method:: Boolean.encode(out)

   出力ストリームオブジェクト ``out`` に、XML-RPCエンコーディングのBoolean値を出力します。


.. A working example follows. The server code::

動作する例です。サーバー側::

   import xmlrpclib
   from SimpleXMLRPCServer import SimpleXMLRPCServer

   def is_even(n):
       return n%2 == 0

   server = SimpleXMLRPCServer(("localhost", 8000))
   print "Listening on port 8000..."
   server.register_function(is_even, "is_even")
   server.serve_forever()

.. The client code for the preceding server::

上記のサーバーに対するクライアント側::

   import xmlrpclib

   proxy = xmlrpclib.ServerProxy("http://localhost:8000/")
   print "3 is even: %s" % str(proxy.is_even(3))
   print "100 is even: %s" % str(proxy.is_even(100))


.. _datetime-objects:

DateTime オブジェクト
---------------------

このクラスは、エポックからの秒数、タプルで表現された時刻、ISO 8601形式の時間/日付文字列、
:class:`datetime.datetime`,
のインスタンスのいずれかで初期化することができます。
このクラスには以下のメソッドがあり、主にコードをマーシャル/アンマーシャルするための内部処理を行います。


.. method:: DateTime.decode(string)

   文字列をインスタンスの新しい時間を示す値として指定します。


.. method:: DateTime.encode(out)

   出力ストリームオブジェクト ``out`` に、XML-RPCエンコーディングの :class:`DateTime` 値を出力します。

また、 :meth:`__cmp__` と :meth:`__repr__` で定義される演算子を使用することができます。


.. _binary-objects:

Binary オブジェクト
-------------------

このクラスは、文字列(NULを含む)で初期化することができます。 :class:`Binary` の内容は、属性で参照します。


.. attribute:: Binary.data

   :class:`Binary` インスタンスがカプセル化しているバイナリデータ。このデータは8bitクリーンです。

以下のメソッドは、主に内部的にマーシャル/アンマーシャル時に使用されます:


.. method:: Binary.decode(string)

   指定されたbase64文字列をデコードし、インスタンスのデータとします。


.. method:: Binary.encode(out)

   バイナリ値をbase64でエンコードし、出力ストリームオブジェクト ``out`` に出力します。

   .. The encoded data will have newlines every 76 characters as per
      `RFC 2045 section 6.8 <http://tools.ietf.org/html/rfc2045#section-6.8>`_,
      which was the de facto standard base64 specification when the
      XML-RPC spec was written.

   エンコードされたデータは、
   `RFC 2045 section 6.8 <http://tools.ietf.org/html/rfc2045#section-6.8>`_
   にある通り、76文字ごとに改行されます。
   これは、XMC-RPC仕様が作成された時のデ・ファクト・スタンダードのbase64です。

また、 :meth:`__cmp__` で定義される演算子を使用することができます。

.. Example usage of the binary objects.  We're going to transfer an image over
   XMLRPC::

バイナリオブジェクトの使用例です。
XML-RPCごしに画像を転送します。 ::

   from SimpleXMLRPCServer import SimpleXMLRPCServer
   import xmlrpclib

   def python_logo():
        with open("python_logo.jpg", "rb") as handle:
            return xmlrpclib.Binary(handle.read())

   server = SimpleXMLRPCServer(("localhost", 8000))
   print "Listening on port 8000..."
   server.register_function(python_logo, 'python_logo')

   server.serve_forever()

.. The client gets the image and saves it to a file::

クライアント側は画像を取得して、ファイルに保存します。 ::

   import xmlrpclib

   proxy = xmlrpclib.ServerProxy("http://localhost:8000/")
   with open("fetched_python_logo.jpg", "wb") as handle:
       handle.write(proxy.python_logo().data)

.. _fault-objects:

Fault オブジェクト
------------------

:class:`Fault` オブジェクトは、XML-RPCのfaultタグの内容をカプセル化しており、以下のメンバを持ちます:


.. attribute:: Fault.faultCode

   失敗のタイプを示す文字列。


.. attribute:: Fault.faultString

   失敗の診断メッセージを含む文字列。


.. In the following example we're going to intentionally cause a :exc:`Fault` by
   returning a complex type object.  The server code::

以下のサンプルでは、複素数型のオブジェクトを返そうとして、故意に :exc:`Fault` を起こしています。 ::

   from SimpleXMLRPCServer import SimpleXMLRPCServer

   # A marshalling error is going to occur because we're returning a
   # complex number
   def add(x,y):
       return x+y+0j

   server = SimpleXMLRPCServer(("localhost", 8000))
   print "Listening on port 8000..."
   server.register_function(add, 'add')

   server.serve_forever()

.. The client code for the preceding server::

上記のサーバーに対するクライアント側のコード::

   import xmlrpclib

   proxy = xmlrpclib.ServerProxy("http://localhost:8000/")
   try:
       proxy.add(2, 5)
   except xmlrpclib.Fault, err:
       print "A fault occurred"
       print "Fault code: %d" % err.faultCode
       print "Fault string: %s" % err.faultString


.. _protocol-error-objects:

ProtocolError オブジェクト
--------------------------

:class:`ProtocolError` オブジェクトはトランスポート層で発生したエラー(URI で指定したサーバが見つからなかった場合に発生する404
'not found'など)の内容を示し、以下のメンバを持ちます:


.. attribute:: ProtocolError.url

   エラーの原因となったURIまたはURL。


.. attribute:: ProtocolError.errcode

   エラーコード。


.. attribute:: ProtocolError.errmsg

   エラーメッセージまたは診断文字列。


.. attribute:: ProtocolError.headers

   エラーの原因となったHTTP/HTTPSリクエストを含む文字列。


.. In the following example we're going to intentionally cause a :exc:`ProtocolError`
   by providing an invalid URI::

次の例では、XMLRPC サーバを指していない URI を利用して、故意に :exc:`ProtocolError` を発生させています。 ::

   import xmlrpclib

   # create a ServerProxy with an URI that doesn't respond to XMLRPC requests
   proxy = xmlrpclib.ServerProxy("http://www.google.com/")

   try:
       proxy.some_method()
   except xmlrpclib.ProtocolError, err:
       print "A protocol error occurred"
       print "URL: %s" % err.url
       print "HTTP/HTTPS headers: %s" % err.headers
       print "Error code: %d" % err.errcode
       print "Error message: %s" % err.errmsg

MultiCall オブジェクト
----------------------

.. versionadded:: 2.4

遠隔のサーバに対する複数の呼び出しをひとつのリクエストにカプセル化
する方法は、http://www.xmlrpc.com/discuss/msgReader%241208 で示されています。


.. class:: MultiCall(server)

   巨大な (boxcar) メソッド呼び出しに使えるオブジェクトを作成します。 *server* には最終的に呼び出しを行う対象を指定します。作成した
   MultiCall オブジェクトを使って呼び出しを行うと、即座に ``None`` を返し、呼び出したい手続き名とパラメタに保存するだけに留まります。
   オブジェクト自体を呼び出すと、それまでに保存しておいたすべての呼び出しを単一の ``system.multicall`` リクエストの形で伝送します。
   呼び出し結果はジェネレータ(:term:`generator`)になります。このジェネレータにわたってイテレーションを行うと、個々の呼び出し結果を返します。

以下にこのクラスの使い方を示します。

.. A usage example of this class follows.  The server code ::

このクラスの使用例です。サーバー側のコード::

   from SimpleXMLRPCServer import SimpleXMLRPCServer

   def add(x,y):
       return x+y

   def subtract(x, y):
       return x-y

   def multiply(x, y):
       return x*y

   def divide(x, y):
       return x/y

   # A simple server with simple arithmetic functions
   server = SimpleXMLRPCServer(("localhost", 8000))
   print "Listening on port 8000..."
   server.register_multicall_functions()
   server.register_function(add, 'add')
   server.register_function(subtract, 'subtract')
   server.register_function(multiply, 'multiply')
   server.register_function(divide, 'divide')
   server.serve_forever()

.. The client code for the preceding server::

このサーバーに対する、クライアント側のコード::

   import xmlrpclib

   proxy = xmlrpclib.ServerProxy("http://localhost:8000/")
   multicall = xmlrpclib.MultiCall(proxy)
   multicall.add(7,3)
   multicall.subtract(7,3)
   multicall.multiply(7,3)
   multicall.divide(7,3)
   result = multicall()

   print "7+3=%d, 7-3=%d, 7*3=%d, 7/3=%d" % tuple(result)


補助関数
--------


.. function:: boolean(value)

   Pythonの値を、XML-RPCのBoolean定数 ``True`` または ``False`` に変換します。


.. function:: dumps(params[, methodname[,  methodresponse[, encoding[, allow_none]]]])

   *params* を XML-RPC リクエストの形式に変換します。 *methodresponse* が真の場合、XML-RPC
   レスポンスの形式に変換します。 *params* に指定できるのは、引数からなるタプルか :exc:`Fault` 例外クラスのインスタンスです。
   *methodresponse* が真の場合、単一の値だけを返します。従って、 *params* の長さも 1 でなければなりません。 *encoding*
   を指定した場合、生成される XML のエンコード方式になります。デフォルトは UTF-8 です。 Python の :const:`None` は標準の
   XML-RPC には利用できません。 :const:`None` を使えるようにするには、 *allow_none* を真にして、拡張機能つきにしてください。


.. function:: loads(data[, use_datetime])

   XML-RPC リクエストまたはレスポンスを ``(params, methodname)`` の形式をとる Python オブジェクトにします。
   *params* は引数のタプルです。 *methodname* は文字列で、パケット中にメソッド名がない場合には ``None`` になります。
   例外条件を示す XML-RPC パケットの場合には、 :exc:`Fault` 例外を送出します。
   *use_datetime* フラグは :class:`datetime.datetime` のオブジェクトとして日付/時刻を表現する時に使用し、デフォルトでは
   false に設定されています。

   .. versionchanged:: 2.5
      *use_datetime* フラグを追加.


.. _xmlrpc-client-example:

クライアントのサンプル
----------------------

::

   # simple test program (from the XML-RPC specification)
   from xmlrpclib import ServerProxy, Error

   # server = ServerProxy("http://localhost:8000") # local server
   server = ServerProxy("http://betty.userland.com")

   print server

   try:
       print server.examples.getStateName(41)
   except Error, v:
       print "ERROR", v

XML-RPCサーバにプロキシを経由して接続する場合、カスタムトランスポートを定義する必要があります。以下に例を示します:

.. Example taken from http://lowlife.jp/nobonobo/wiki/xmlrpcwithproxy.html

::

   import xmlrpclib, httplib

   class ProxiedTransport(xmlrpclib.Transport):
       def set_proxy(self, proxy):
           self.proxy = proxy
       def make_connection(self, host):
           self.realhost = host
           h = httplib.HTTP(self.proxy)
           return h
       def send_request(self, connection, handler, request_body):
           connection.putrequest("POST", 'http://%s%s' % (self.realhost, handler))
       def send_host(self, connection, host):
           connection.putheader('Host', self.realhost)

   p = ProxiedTransport()
   p.set_proxy('proxy-server:8080')
   server = xmlrpclib.Server('http://time.xmlrpc.com/RPC2', transport=p)
   print server.currentTime.getCurrentTime()


.. Example of Client and Server Usage

クライアントとサーバーの利用例
----------------------------------

:ref:`simplexmlrpcserver-example` を参照してください。


