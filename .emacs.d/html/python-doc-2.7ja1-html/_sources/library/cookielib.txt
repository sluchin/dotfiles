:mod:`cookielib` --- HTTP クライアント用の Cookie 処理
======================================================

.. module:: cookielib
   :synopsis: HTTP cookie を自動的に処理するためのクラスライブラリ
.. moduleauthor:: John J. Lee <jjl@pobox.com>
.. sectionauthor:: John J. Lee <jjl@pobox.com>

.. .. note::
   The :mod:`cookielib` module has been renamed to :mod:`http.cookiejar` in
   Python 3.0.  The :term:`2to3` tool will automatically adapt imports when
   converting your sources to 3.0.

.. note::
   :mod:`cookielib` モジュールは、Python 3では :mod:`http.cookiejar` にリネームされました。
   :term:`2to3` ツールは自動的にソースコード内のimportをPython 3用に修正します。

.. versionadded:: 2.4



:mod:`cookielib` モジュールは HTTP クッキーの自動処理をおこなうクラスを定義します。これは小さなデータの断片 --
:dfn:`クッキー` --  を要求する web サイトにアクセスする際に有用です。クッキーとは web サーバの HTTP
レスポンスによってクライアントのマシンに設定され、のちの HTTP リクエストをおこなうさいにサーバに返されるものです。

標準的な Netscape クッキープロトコルおよび :rfc:`2965` で定義されているプロトコルの両方を処理できます。RFC 2965
の処理はデフォルトではオフになっています。 :rfc:`2109` のクッキーは Netscape クッキーとして解析され、のちに有効な 'ポリシー'
に従って Netscapeまたは RFC 2965 クッキーとして処理されます。但し、インターネット上の大多数のクッキーは Netscapeクッキーです。
:mod:`cookielib` はデファクトスタンダードの Netscape クッキープロトコル  (これは元々 Netscape
が策定した仕様とはかなり異なっています) に従うようになっており、RFC 2109 で導入された ``max-age`` や ``port`` などの
クッキー属性にも注意を払います。

.. note::

   :mailheader:`Set-Cookie` や :mailheader:`Set-Cookie2` ヘッダに現れる多種多様なパラメータの名前
   (``domain`` や ``expires`` など) は便宜上 :dfn:`属性` と呼ばれますが、ここでは Python
   の属性と区別するため、かわりに :dfn:`クッキー属性` と呼ぶことにします。

このモジュールは以下の例外を定義しています:


.. exception:: LoadError

   この例外は :class:`FileCookieJar` インスタンスがファイルからクッキーを読み込むのに失敗した場合に発生します。

以下のクラスが提供されています:


.. class:: CookieJar(policy=None)

   *policy* は :class:`CookiePolicy` インターフェイスを実装するオブジェクトです。

   :class:`CookieJar` クラスには HTTP クッキーを保管します。これは HTTP リクエストに応じてクッキーを取り出し、それを HTTP
   レスポンスの中で返します。必要に応じて、 :class:`CookieJar` インスタンスは保管されているクッキーを
   自動的に破棄します。このサブクラスは、クッキーをファイルやデータベースに格納したり取り出したりする操作をおこなう役割を負っています。


.. class:: FileCookieJar(filename, delayload=None, policy=None)

   *policy* は :class:`CookiePolicy` インターフェイスを実装するオブジェクトです。
   これ以外の引数については、該当する属性の説明を参照してください。

   :class:`FileCookieJar` はディスク上のファイルからのクッキーの読み込み、
   もしくは書き込みをサポートします。実際には、 :meth:`load` または  :meth:`revert` のどちらかのメソッドが呼ばれるまでクッキーは
   指定されたファイルからはロード **されません** 。このクラスのサブクラスは :ref:`file-cookie-jar-classes` 節で説明します。


.. class:: CookiePolicy()

   このクラスは、あるクッキーをサーバから受け入れるべきか、そしてサーバに返すべきかを決定する役割を負っています。


.. class:: DefaultCookiePolicy( blocked_domains=None, allowed_domains=None, netscape=True, rfc2965=False, rfc2109_as_netscape=None, hide_cookie2=False, strict_domain=False, strict_rfc2965_unverifiable=True, strict_ns_unverifiable=False, strict_ns_domain=DefaultCookiePolicy.DomainLiberal, strict_ns_set_initial_dollar=False, strict_ns_set_path=False )

   コンストラクタはキーワード引数しか取りません。 *blocked_domains* はドメイン名からなるシーケンスで、ここからは
   決してクッキーを受けとらないし、このドメインにクッキーを返すこともありません。 *allowed_domains* が :const:`None`
   でない場合、これはこのドメインのみからクッキーを受けとり、返すという指定になります。これ以外の引数については :class:`CookiePolicy`
   および :class:`DefaultCookiePolicy` オブジェクトの説明をごらんください。

   :class:`DefaultCookiePolicy` は Netscape および RFC 2965 クッキーの標準的な許可 /
   拒絶のルールを実装しています。デフォルトでは、RFC 2109 のクッキー (:mailheader:`Set-Cookie` の version
   クッキー属性が 1 で受けとられるもの) は RFC 2965 のルールで扱われます。しかし、RFC 2965処理が無効に設定されているか
   :attr:`rfc2109_as_netscape` が Trueの場合、RFC 2109クッキーは :class:`CookieJar` インスタンスによって
   :class:`Cookie` のインスタンスの :attr:`version` 属性を 0に設定する事で Netscapeクッキーに「ダウングレード」されます。
   また :class:`DefaultCookiePolicy` にはいくつかの細かいポリシー設定をおこなうパラメータが用意されています。


.. class:: Cookie()

   このクラスは Netscape クッキー、RFC 2109 のクッキー、および RFC 2965 のクッキーを表現します。 :mod:`cookielib`
   のユーザが自分で :class:`Cookie` インスタンスを作成することは想定されていません。かわりに、必要に応じて :class:`CookieJar`
   インスタンスの :meth:`make_cookies` を呼ぶことになっています。


.. seealso::

   Module :mod:`urllib2`
      クッキーの自動処理をおこない URL を開くモジュールです。

   Module :mod:`Cookie`
      HTTP のクッキークラスで、基本的にはサーバサイドのコードで有用です。 :mod:`cookielib` および :mod:`Cookie` モジュールは
      互いに依存してはいません。

   http://wp.netscape.com/newsref/std/cookie_spec.html
      元祖 Netscape のクッキープロトコルの仕様です。今でもこれが主流のプロトコルですが、現在のメジャーなブラウザ (と
      :mod:`cookielib`) が実装している「Netscape クッキープロトコル」は ``cookie_spec.html`` で述べられているものと
      おおまかにしか似ていません。

   :rfc:`2109` - HTTP State Management Mechanism
      RFC 2965 によって過去の遺物になりました。 :mailheader:`Set-Cookie` の version=1 で使います。

   :rfc:`2965` - HTTP State Management Mechanism
      Netscape プロトコルのバグを修正したものです。 :mailheader:`Set-Cookie` のかわりに
      :mailheader:`Set-Cookie2` を使いますが、普及してはいません。

   http://kristol.org/cookie/errata.html
      RFC 2965 に対する未完の正誤表です。

   :rfc:`2964` - Use of HTTP State Management

.. _cookie-jar-objects:

CookieJar および FileCookieJar オブジェクト
-------------------------------------------

:class:`CookieJar` オブジェクトは保管されている :class:`Cookie` オブジェクトを
ひとつずつ取り出すための、イテレータ(:term:`iterator`)・プロトコルをサポートしています。

:class:`CookieJar` は以下のようなメソッドを持っています:


.. method:: CookieJar.add_cookie_header(request)

   *request* に正しい :mailheader:`Cookie` ヘッダを追加します。

   ポリシーが許すようであれば (:class:`CookieJar` の :class:`CookiePolicy` インスタンスにある
   属性のうち、 :attr:`rfc2965` および :attr:`hide_cookie2` がそれぞれ真と偽であるような場合)、必要に応じて
   :mailheader:`Cookie2` ヘッダも追加されます。

   *request* オブジェクト (通常は :class:`urllib2.Request` インスタンス) は、 :mod:`urllib2`
   のドキュメントに記されているように、 :meth:`get_full_url`, :meth:`get_host`, :meth:`get_type`,
   :meth:`unverifiable`, :meth:`get_origin_req_host`, :meth:`has_header`,
   :meth:`get_header`, :meth:`header_items` および :meth:`add_unredirected_header`
   の各メソッドをサポートしている必要があります。


.. method:: CookieJar.extract_cookies(response, request)

   HTTP *response* からクッキーを取り出し、ポリシーによって許可されていればこれを :class:`CookieJar` 内に保管します。

   :class:`CookieJar` は *response* 引数の中から許可されている :mailheader:`Set-Cookie` および
   :mailheader:`Set-Cookie2` ヘッダを探しだし、適切に (:meth:`CookiePolicy.set_ok`
   メソッドの承認におうじて)  クッキーを保管します。

   *response* オブジェクト (通常は :meth:`urllib2.urlopen` あるいはそれに類似する呼び出しによって得られます) は
   :meth:`info` メソッドをサポートしている必要があります。これは :meth:`getallmatchingheaders` メソッドのある
   オブジェクト (通常は :class:`mimetools.Message` インスタンス) を返すものです。

   *request* オブジェクト (通常は :class:`urllib2.Request` インスタンス) は :mod:`urllib2`
   のドキュメントに記されているように、 :meth:`get_full_url`, :meth:`get_host`, :meth:`unverifiable`
   および :meth:`get_origin_req_host` の各メソッドをサポートしている必要があります。この request
   はそのクッキーの保存が許可されているかを検査するとともに、クッキー属性のデフォルト値を設定するのに使われます。


.. method:: CookieJar.set_policy(policy)

   使用する :class:`CookiePolicy` インスタンスを指定します。


.. method:: CookieJar.make_cookies(response, request)

   *response* オブジェクトから得られた :class:`Cookie` オブジェクトからなるシーケンスを返します。

   *response* および *request* 引数で要求されるインスタンスについては、 :meth:`extract_cookies`
   の説明を参照してください。


.. method:: CookieJar.set_cookie_if_ok(cookie, request)

   ポリシーが許すのであれば、与えられた :class:`Cookie` を設定します。


.. method:: CookieJar.set_cookie(cookie)

   与えられた :class:`Cookie` を、それが設定されるべきかどうかのポリシーのチェックを行わずに設定します。


.. method:: CookieJar.clear([domain[, path[, name]]])

   いくつかのクッキーを消去します。

   引数なしで呼ばれた場合は、すべてのクッキーを消去します。引数がひとつ与えられた場合、その *domain* に属するクッキーのみを消去します。
   ふたつの引数が与えられた場合、指定された *domain* と URL *path* に属するクッキーのみを消去します。引数が 3つ与えられた場合、
   *domain*, *path* および *name* で指定されるクッキーが消去されます。

   与えられた条件に一致するクッキーがない場合は :exc:`KeyError` を発生させます。


.. method:: CookieJar.clear_session_cookies()

   すべてのセッションクッキーを消去します。

   保存されているクッキーのうち、 :attr:`discard` 属性が真になっているものすべてを消去します (通常これは ``max-age`` または
   ``expires`` のどちらのクッキー属性もないか、あるいは明示的に ``discard`` クッキー属性が
   指定されているものです)。対話的なブラウザの場合、セッションの終了はふつうブラウザのウィンドウを閉じることに相当します。

   注意: *ignore_discard* 引数に真を指定しないかぎり、 :meth:`save` メソッドはセッションクッキーは保存しません。

さらに :class:`FileCookieJar` は以下のようなメソッドを実装しています:


.. method:: FileCookieJar.save(filename=None, ignore_discard=False, ignore_expires=False)

   クッキーをファイルに保存します。

   この基底クラスは  :exc:`NotImplementedError` を発生させます。サブクラスはこのメソッドを実装しないままにしておいてもかまいません。

   *filename* はクッキーを保存するファイルの名前です。 *filename* が指定されない場合、 :attr:`self.filename`
   が使用されます (このデフォルト値は、それが存在する場合は、コンストラクタに渡されています)。 :attr:`self.filename` も
   :const:`None` の場合は :exc:`ValueError` が発生します。

   *ignore_discard* : 破棄されるよう指示されていたクッキーでも保存します。 *ignore_expires* :
   期限の切れたクッキーでも保存します。

   ここで指定されたファイルがもしすでに存在する場合は上書きされるため、以前にあったクッキーはすべて消去されます。保存したクッキーはあとで
   :meth:`load` または :meth:`revert` メソッドを使って復元することができます。


.. method:: FileCookieJar.load(filename=None, ignore_discard=False, ignore_expires=False)

   ファイルからクッキーを読み込みます。

   それまでのクッキーは新しいものに上書きされない限り残ります。

   ここでの引数の値は :meth:`save` と同じです。

   名前のついたファイルはこのクラスがわかるやり方で指定する必要があります。さもないと :exc:`LoadError` が発生します。
   さらに、例えばファイルが存在しないような時に :exc:`IOError` が発生する場合があります。

   .. note::

      (:exc:`IOError` を発行する)Python 2.4との後方互換性のために、 :exc:`LoadError` は
      :exc:`IOError` のサブクラスです。


.. method:: FileCookieJar.revert(filename=None, ignore_discard=False, ignore_expires=False)

   すべてのクッキーを破棄し、保存されているファイルから読み込み直します。

   :meth:`revert` は :meth:`load` と同じ例外を発生させる事ができます。失敗した場合、オブジェクトの状態は変更されません。

:class:`FileCookieJar` インスタンスは以下のような公開の属性をもっています:


.. attribute:: FileCookieJar.filename

   クッキーを保存するデフォルトのファイル名を指定します。この属性には代入することができます。


.. attribute:: FileCookieJar.delayload

   真であれば、クッキーを読み込むさいにディスクから遅延読み込み (lazy) します。この属性には代入することができません。この情報は単なるヒントであり、
   (ディスク上のクッキーが変わらない限りは) インスタンスのふるまいには影響を与えず、パフォーマンスのみに影響します。 :class:`CookieJar`
   オブジェクトはこの値を無視することもあります。標準ライブラリに含まれている :class:`FileCookieJar` クラスで遅延読み込みを
   おこなうものはありません。


.. _file-cookie-jar-classes:

FileCookieJar のサブクラスと web ブラウザとの連携
-------------------------------------------------

クッキーの読み書きのために、以下の :class:`CookieJar` サブクラスが提供されています。

.. class:: MozillaCookieJar(filename, delayload=None, policy=None)

   Mozilla の ``cookies.txt`` ファイル形式 (この形式はまた Lynx と Netscape ブラウザによっても使われています)
   でディスクにクッキーを読み書きするための :class:`FileCookieJar` です。

   .. .. note::
      Version 3 of the Firefox web browser no longer writes cookies in the
      ``cookies.txt`` file format.

   .. note::

      Firefox 3 は、 cookie を ``cookies.txt`` ファイルフォーマットで保存しません。

   .. .. note::

      This loses information about RFC 2965 cookies, and also about newer or
      non-standard cookie-attributes such as ``port``.

   .. note::

      このクラスは RFC 2965 クッキーに関する情報を失います。また、より新しいか、標準でない ``port`` などの
      クッキー属性についての情報も失います。

   .. warning::

      もしクッキーの損失や欠損が望ましくない場合は、クッキーを保存する前にバックアップを取っておくようにしてください (ファイルへの読み込み /
      保存をくり返すと微妙な変化が生じる場合があります)。

   また、 Mozilla の起動中にクッキーを保存すると、 Mozilla によって内容が破壊されてしまうことにも注意してください。


.. class:: LWPCookieJar(filename, delayload=None, policy=None)

   libwww-perl のライブラリである ``Set-Cookie3`` ファイル形式でディスクにクッキーを読み書きするための
   :class:`FileCookieJar` です。これはクッキーを人間に可読な形式で保存するのに向いています。


.. _cookie-policy-objects:

CookiePolicy オブジェクト
-------------------------

:class:`CookiePolicy` インターフェイスを実装するオブジェクトは以下のようなメソッドを持っています:


.. method:: CookiePolicy.set_ok(cookie, request)

   クッキーがサーバから受け入れられるべきかどうかを表わす boolean 値を返します。

   *cookie* は :class:`cookielib.Cookie` インスタンスです。 *request* は
   :meth:`CookieJar.extract_cookies` の説明で定義されているインターフェイスを実装するオブジェクトです。


.. method:: CookiePolicy.return_ok(cookie, request)

   クッキーがサーバに返されるべきかどうかを表わす boolean 値を返します。

   *cookie* は :class:`cookielib.Cookie` インスタンスです。 *request* は
   :meth:`CookieJar.add_cookie_header` の説明で定義されているインターフェイスを実装するオブジェクトです。


.. method:: CookiePolicy.domain_return_ok(domain, request)

   与えられたクッキーのドメインに対して、そこにクッキーを返すべきでない場合には false を返します。

   このメソッドは高速化のためのものです。これにより、すべてのクッキーをある特定のドメインに対してチェックする
   (これには多数のファイル読みこみを伴なう場合があります) 必要がなくなります。 :meth:`domain_return_ok` および
   :meth:`path_return_ok` の両方から true が返された場合、すべての決定は :meth:`return_ok` に委ねられます。

   もし、このクッキードメインに対して :meth:`domain_return_ok` が true を返すと、つぎにそのクッキーのパス名に対して
   :meth:`path_return_ok` が呼ばれます。そうでない場合、そのクッキードメインに対する :meth:`path_return_ok` および
   :meth:`return_ok` は決して呼ばれることはありません。 :meth:`path_return_ok` が true を返すと、
   :meth:`return_ok` がその :class:`Cookie` オブジェクト自身の全チェックのために
   呼ばれます。そうでない場合、そのクッキーパス名に対する :meth:`return_ok` は決して呼ばれることはありません。

   注意: :meth:`domain_return_ok` は *request* ドメインだけではなく、すべての *cookie*
   ドメインに対して呼ばれます。たとえば request ドメインが ``"www.example.com"`` だった場合、この関数は
   ``".example.com"`` および ``"www.example.com"`` の両方に対して呼ばれることがあります。同じことは
   :meth:`path_return_ok` にもいえます。

   *request* 引数は :meth:`return_ok` で説明されているとおりです。


.. method:: CookiePolicy.path_return_ok(path, request)

   与えられたクッキーのパス名に対して、そこにクッキーを返すべきでない場合には false を返します。

   :meth:`domain_return_ok` の説明を参照してください。

上のメソッドの実装にくわえて、 :class:`CookiePolicy` インターフェイスの実装では
以下の属性を設定する必要があります。これはどのプロトコルがどのように使われるべきかを示すもので、これらの属性にはすべて代入することが許されています。


.. attribute:: CookiePolicy.netscape

   Netscape プロトコルを実装していることを示します。


.. attribute:: CookiePolicy.rfc2965

   RFC 2965 プロトコルを実装していることを示します。


.. attribute:: CookiePolicy.hide_cookie2

   :mailheader:`Cookie2` ヘッダをリクエストに含めないようにします (このヘッダが存在する場合、私たちは RFC 2965
   クッキーを理解するということをサーバに示すことになります)。

もっとも有用な方法は、 :class:`DefaultCookiePolicy` をサブクラス化した :class:`CookiePolicy`
クラスを定義して、いくつか (あるいはすべて) のメソッドをオーバーライドすることでしょう。 :class:`CookiePolicy` 自体は
どのようなクッキーも受け入れて設定を許可する「ポリシー無し」ポリシーとして使うこともできます (これが役に立つことはあまりありませんが)。


.. _default-cookie-policy-objects:

DefaultCookiePolicy オブジェクト
--------------------------------

クッキーを受けつけ、またそれを返す際の標準的なルールを実装します。

RFC 2965 クッキーと Netscape クッキーの両方に対応しています。デフォルトでは、RFC 2965 の処理はオフになっています。

自分のポリシーを提供するいちばん簡単な方法は、このクラスを継承して、自分用の追加チェックの前にオーバーライドした元のメソッドを呼び出すことです::

   import cookielib
   class MyCookiePolicy(cookielib.DefaultCookiePolicy):
       def set_ok(self, cookie, request):
           if not cookielib.DefaultCookiePolicy.set_ok(self, cookie, request):
               return False
           if i_dont_want_to_store_this_cookie(cookie):
               return False
           return True

:class:`CookiePolicy` インターフェイスを実装するのに必要な機能に加えて、このクラスではクッキーを受けとったり設定したりするドメインを
許可したり拒絶したりできるようになっています。ほかにも、 Netscape プロトコルのかなり緩い規則をややきつくするために、いくつかの
厳密性のスイッチがついています (いくつかの良性クッキーをブロックする危険性もありますが)。

ドメインのブラックリスト機能やホワイトリスト機能も提供されています (デフォルトではオフになっています)。
ブラックリストになく、(ホワイトリスト機能を使用している場合は) ホワイトリストにあるドメインのみがクッキーを設定したり返したりすることを許可されます。
コンストラクタの引数 *blocked_domains* 、および :meth:`blocked_domains` と
:meth:`set_blocked_domains` メソッドを使ってください (*allowed_domains*
に関しても同様の対応する引数とメソッドがあります)。ホワイトリストを設定した場合は、それを :const:`None` にすることで
ホワイトリスト機能をオフにすることができます。

ブラックリストあるいはホワイトリスト中にあるドメインのうち、ドット (.) で始まっていないものは、正確にそれと一致する
ドメインのクッキーにしか適用されません。たとえばブラックリスト中のエントリ ``"example.com"`` は、 ``"example.com"``
にはマッチしますが、 ``"www.example.com"`` にはマッチしません。一方ドット (.)
で始まっているドメインは、より特化されたドメインともマッチします。たとえば、 ``".example.com"``
は、 ``"www.example.com"`` と ``"www.coyote.example.com"`` の両方にマッチします
(が、 ``"example.com"`` 自身にはマッチしません)。IP アドレスは例外で、つねに正確に一致する必要があります。たとえば、かりに
*blocked_domains* が ``"192.168.1.2"`` と ``".168.1.2"`` を含んでいたとして、192.168.1.2
はブロックされますが、 193.168.1.2 はブロックされません。

:class:`DefaultCookiePolicy` は以下のような追加メソッドを実装しています:


.. method:: DefaultCookiePolicy.blocked_domains()

   ブロックしているドメインのシーケンスを (タプルとして) 返します。


.. method:: DefaultCookiePolicy.set_blocked_domains(blocked_domains)

   ブロックするドメインを設定します。


.. method:: DefaultCookiePolicy.is_blocked(domain)

   *domain* がクッキーを授受しないブラックリストに載っているかどうかを返します。


.. method:: DefaultCookiePolicy.allowed_domains()

   :const:`None` あるいは明示的に許可されているドメインを (タプルとして) 返します。


.. method:: DefaultCookiePolicy.set_allowed_domains(allowed_domains)

   許可するドメイン、あるいは :const:`None` を設定します。


.. method:: DefaultCookiePolicy.is_not_allowed(domain)

   *domain* がクッキーを授受するホワイトリストに載っているかどうかを返します。

:class:`DefaultCookiePolicy` インスタンスは以下の属性をもっています。
これらはすべてコンストラクタから同じ名前の引数をつかって初期化することができ、代入してもかまいません。


.. attribute:: DefaultCookiePolicy.rfc2109_as_netscape

   Trueの場合、 :class:`CookieJar` のインスタンスに RFC 2109 クッキー (即ち
   :mailheader:`Set-Cookie` ヘッダのVersion cookie属性の値が1のクッキー)を
   Netscapeクッキーへ、 :class:`Cookie` インスタンスのversion属性を0に設定する事で
   ダウングレードするように要求します。デフォルトの値は :const:`None` であり、この場合 RFC 2109 クッキーは RFC 2965
   処理が無効に設定されている場合に限りダウングレードされます。それ故に RFC 2109 クッキーはデフォルトではダウングレードされます。

   .. versionadded:: 2.5

一般的な厳密性のスイッチ:


.. attribute:: DefaultCookiePolicy.strict_domain

   サイトに、国別コードとトップレベルドメインだけからなるドメイン名 (``.co.uk``, ``.gov.uk``, ``.co.nz`` など)
   を設定させないようにします。これは完璧からはほど遠い実装であり、いつもうまくいくとは限りません!

RFC 2965 プロトコルの厳密性に関するスイッチ:


.. attribute:: DefaultCookiePolicy.strict_rfc2965_unverifiable

   検証不可能なトランザクション (通常これはリダイレクトか、別のサイトがホスティングしているイメージの読み込み要求です) に関する RFC 2965
   の規則に従います。この値が偽の場合、検証可能性を基準にしてクッキーがブロックされることは *決して* ありません。

Netscape プロトコルの厳密性に関するスイッチ:


.. attribute:: DefaultCookiePolicy.strict_ns_unverifiable

   検証不可能なトランザクションに関する RFC 2965 の規則を Netscape クッキーに対しても適用します。


.. attribute:: DefaultCookiePolicy.strict_ns_domain

   Netscape クッキーに対するドメインマッチングの規則をどの程度厳しくするかを指示するフラグです。とりうる値については下の説明を見てください。


.. attribute:: DefaultCookiePolicy.strict_ns_set_initial_dollar

   Set-Cookie: ヘッダで、 ``'$'`` で始まる名前のクッキーを無視します。


.. attribute:: DefaultCookiePolicy.strict_ns_set_path

   要求した URI にパスがマッチしないクッキの設定を禁止します。

:attr:`strict_ns_domain` はいくつかのフラグの集合です。これはいくつかの値を or することで構成します (たとえば
``DomainStrictNoDots|DomainStrictNonDomain`` は両方のフラグが設定されていることになります)。


.. attribute:: DefaultCookiePolicy.DomainStrictNoDots

   クッキーを設定するさい、ホスト名のプレフィクスにドットが含まれるのを禁止します (例: ``www.foo.bar.com`` は ``.bar.com``
   のクッキーを設定することはできません、なぜなら ``www.foo`` はドットを含んでいるからです)。


.. attribute:: DefaultCookiePolicy.DomainStrictNonDomain

   ``domain`` クッキー属性を明示的に指定していないクッキーは、そのクッキーを設定したドメインと同一のドメインだけに返されます (例:
   ``example.com`` からのクッキーに ``domain`` クッキー属性がない場合、そのクッキーが ``spam.example.com``
   に返されることはありません)。


.. attribute:: DefaultCookiePolicy.DomainRFC2965Match

   クッキーを設定するさい、RFC 2965 の完全ドメインマッチングを要求します。

以下の属性は上記のフラグのうちもっともよく使われる組み合わせで、便宜をはかるために提供されています。


.. attribute:: DefaultCookiePolicy.DomainLiberal

   0 と同じです (つまり、上述の Netscape のドメイン厳密性フラグがすべてオフにされます)。


.. attribute:: DefaultCookiePolicy.DomainStrict

   ``DomainStrictNoDots|DomainStrictNonDomain`` と同じです。


.. _cookielib-cookie-objects:

Cookie オブジェクト
-------------------

:class:`Cookie` インスタンスは、さまざまなクッキーの標準で規定されている標準的なクッキー属性とおおまかに対応する Python
属性をもっています。しかしデフォルト値を決める複雑なやり方が存在しており、また ``max-age`` および ``expires`` クッキー属性は
同じ値をもつことになっているので、また RFC 2109クッキーは :mod:`cookielib` によって version 1から version 0
(Netscape)クッキーへ 'ダウングレード' される場合があるため、この対応は 1対 1 ではありません。

:class:`CookiePolicy` メソッド内でのごくわずかな例外を除けば、これらの属性に代入する必要はないはずです。このクラスは
内部の一貫性を保つようにはしていないため、代入するのは自分のやっていることを理解している場合のみにしてください。


.. attribute:: Cookie.version

   整数または :const:`None` 。 Netscape クッキーはバージョン 0 であり、 RFC 2965 および RFC 2109 クッキーは
   バージョン 1 です。しかし、 :mod:`cookielib` は RFC 2109クッキーを Netscapeクッキー (:attr:`version` が
   0)に'ダウングレード'する場合がある事に注意して下さい。


.. attribute:: Cookie.name

   クッキーの名前 (文字列)。


.. attribute:: Cookie.value

   クッキーの値 (文字列)、あるいは :const:`None` 。


.. attribute:: Cookie.port

   ポートあるいはポートの集合をあらわす文字列 (例: '80' または '80,8080')、あるいは :const:`None` 。


.. attribute:: Cookie.path

   クッキーのパス名 (文字列、例: ``'/acme/rocket_launchers'``)。


.. attribute:: Cookie.secure

   そのクッキーを返せるのが安全な接続のみならば真を返します。


.. attribute:: Cookie.expires

   クッキーの期限が切れる日時をあわらす整数 (エポックから経過した秒数)、あるいは :const:`None` 。 :meth:`is_expired`
   も参照してください。


.. attribute:: Cookie.discard

   これがセッションクッキーであれば真を返します。


.. attribute:: Cookie.comment

   このクッキーの働きを説明する、サーバからのコメント文字列、あるいは :const:`None` 。


.. attribute:: Cookie.comment_url

   このクッキーの働きを説明する、サーバからのコメントのリンク URL、あるいは :const:`None` 。


.. attribute:: Cookie.rfc2109

   RFC 2109クッキー(即ち :mailheader:`Set-Cookie` ヘッダにあり、かつVersion
   cookie属性の値が1のクッキー)の場合、Trueを返します。 :mod:`cookielib` が RFC 2109クッキーを Netscapeクッキー
   (:attr:`version` が 0)に'ダウングレード'する場合があるので、この属性が提供されています。

   .. versionadded:: 2.5


.. attribute:: Cookie.port_specified

   サーバがポート、あるいはポートの集合を (:mailheader:`Set-Cookie` / :mailheader:`Set-Cookie2` ヘッダ内で)
   明示的に指定していれば真を返します。


.. attribute:: Cookie.domain_specified

   サーバがドメインを明示的に指定していれば真を返します。


.. attribute:: Cookie.domain_initial_dot

   サーバが明示的に指定したドメインが、ドット (``'.'``) で始まっていれば真を返します。

クッキーは、オプションとして標準的でないクッキー属性を持つこともできます。これらは以下のメソッドでアクセスできます:


.. method:: Cookie.has_nonstandard_attr(name)

   そのクッキーが指定された名前のクッキー属性をもっている場合には真を返します。


.. method:: Cookie.get_nonstandard_attr(name, default=None)

   クッキーが指定された名前のクッキー属性をもっていれば、その値を返します。そうでない場合は *default* を返します。


.. method:: Cookie.set_nonstandard_attr(name, value)

   指定された名前のクッキー属性を設定します。

:class:`Cookie` クラスは以下のメソッドも定義しています:


.. method:: Cookie.is_expired([now=None])

   サーバが指定した、クッキーの期限が切れるべき時が過ぎていれば真を返します。 *now* が指定されているときは (エポックから経過した秒数です)、
   そのクッキーが指定された時間において期限切れになっているかどうかを判定します。


.. _cookielib-examples:

使用例
------

はじめに、もっとも一般的な :mod:`cookielib` の使用例をあげます::

   import cookielib, urllib2
   cj = cookielib.CookieJar()
   opener = urllib2.build_opener(urllib2.HTTPCookieProcessor(cj))
   r = opener.open("http://example.com/")

以下の例では、 URL を開く際に Netscape や Mozilla または Lynx のクッキーを使う方法を示しています (クッキーファイルの位置は
Unix/Netscape の慣例にしたがうものと仮定しています)::

   import os, cookielib, urllib2
   cj = cookielib.MozillaCookieJar()
   cj.load(os.path.join(os.path.expanduser("~"), ".netscape", "cookies.txt"))
   opener = urllib2.build_opener(urllib2.HTTPCookieProcessor(cj))
   r = opener.open("http://example.com/")

つぎの例は :class:`DefaultCookiePolicy` の使用例です。 RFC 2965 クッキーをオンにし、Netscape
クッキーを設定したり返したりするドメインに対してより厳密な規則を適用します。そしていくつかのドメインから
クッキーを設定あるいは返還するのをブロックしています::

   import urllib2
   from cookielib import CookieJar, DefaultCookiePolicy
   policy = DefaultCookiePolicy(
       rfc2965=True, strict_ns_domain=DefaultCookiePolicy.DomainStrict,
       blocked_domains=["ads.net", ".ads.net"])
   cj = CookieJar(policy)
   opener = urllib2.build_opener(urllib2.HTTPCookieProcessor(cj))
   r = opener.open("http://example.com/")

