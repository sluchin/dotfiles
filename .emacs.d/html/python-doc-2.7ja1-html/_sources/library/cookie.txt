:mod:`Cookie` --- HTTPの状態管理
================================

.. module:: Cookie
   :synopsis: HTTP状態管理(cookies)のサポート。
.. moduleauthor:: Timothy O'Malley <timo@alum.mit.edu>
.. sectionauthor:: Moshe Zadka <moshez@zadka.site.co.il>

.. note::
   Python 3.0 では :mod:`Cookie` モジュールは :mod:`http.cookies` にリネームされました。
   ソースコードを 3.0 用に変換する時は、 :term:`2to3` ツールが自動的に import を修正します。

:mod:`Cookie` モジュールはHTTPの状態管理機能であるcookieの概念を抽象
化、定義しているクラスです。単純な文字列のみで構成されるcookieのほか、
シリアル化可能なあらゆるデータ型でクッキーの値を保持するための機能も備
えています。

このモジュールは元々 :rfc:`2109` と :rfc:`2068` に定義されている構文解析の規則を厳密に守っていました。しかし、MSIE
3.0xがこれらのRFCで定義された文字の規則に従っていないことが判明したため、結局、
やや厳密さを欠く構文解析規則にせざるを得ませんでした。

.. note::

   正しくない cookie に遭遇した場合、 :exc:`CookieError` 例外を送出します。
   なので、ブラウザから持ってきた cookie データを parse するときには常に
   :exc:`CookieError` 例外を catch して不正な cookie に備えるべきです。

.. exception:: CookieError

   属性や :mailheader:`Set-Cookie` ヘッダが正しくないなど、 :rfc:`2109` に合致していないときに発生する例外です。


.. class:: BaseCookie([input])

   このクラスはキーが文字列、値が :class:`Morsel` インスタンスで構成される辞書風オブジェクトです。
   値に対するキーを設定するときは、値がキーと値を含む :class:`Morsel` に変換されることに注意してください。

   *input* が与えられたときは、そのまま :meth:`load` メソッドへ渡されます。


.. class:: SimpleCookie([input])

   このクラスは :class:`BaseCookie` の派生クラスで、 :meth:`value_decode`
   は与えられた値の正当性を確認するように、 :meth:`value_encode` は :func:`str` で文字列化するようにそれぞれオーバライドします。


.. class:: SerialCookie([input])

   このクラスは :class:`BaseCookie` の派生クラスで、 :meth:`value_decode`
   と :meth:`value_encode` をそれぞれ :func:`pickle.loads` と
   :func:`pickle.dumps` を実行するようにオーバーライドします。

   .. deprecated:: 2.3
      このクラスを使ってはいけません! 信頼できないcookieのデータから pickle 化された値を読み込むことは、あなたのサーバ上で任意のコードを
      実行するために pickle 化した文字列の作成が可能であることを意味し、重大なセキュリティホールとなります。


.. class:: SmartCookie([input])

   このクラスは :class:`BaseCookie` の派生クラスで、 :meth:`value_decode`  を、値が pickle
   化されたデータとして正当なときは :func:`pickle.loads` を実行、そうでないときはその値自体を返すよう
   にオーバーライドします。また :meth:`value_encode` を、値が文字列以外
   のときは :func:`pickle.dumps` を実行、文字列のときはその値自体を返すようにオーバーライドします。

   .. deprecated:: 2.3
      :class:`SerialCookie` と同じセキュリティ上の注意が当てはまります。

関連して、さらなるセキュリティ上の注意があります。後方互換性のため、 :mod:`Cookie` モジュールは :class:`Cookie` というクラス名を
:class:`SmartCookie` のエイリアスとしてエクスポートしています。これはほ
ぼ確実に誤った措置であり、将来のバージョンでは削除することが適当と思わ
れます。アプリケーションにおいて :class:`SerialCookie` クラスを使うべきで
ないのと同じ理由で :class:`Cookie` クラスを使うべきではありません。


.. seealso::

   Module :mod:`cookielib`
      Web *クライアント* 向けの HTTP クッキー処理です。 :mod:`cookielib` と :mod:`Cookie` は互いに独立しています。

   :rfc:`2109` - HTTP State Management Mechanism
      このモジュールが実装しているHTTPの状態管理に関する規格です。

.. % \subsection{Cookie Objects \label{cookie-objects}}


.. _cookie-objects:

Cookieオブジェクト
------------------

.. method:: BaseCookie.value_decode(val)

   文字列表現を値にデコードして返します。戻り値の型はどのようなものでも許
   されます。このメソッドは :class:`BaseCookie` において何も実行せず、オーバーライドされるためにだけ存在します。


.. method:: BaseCookie.value_encode(val)

   エンコードした値を返します。元の値はどのような型でもかまいませんが、戻
   り値は必ず文字列となります。このメソッドは :class:`BaseCookie` において何も実行せず、オーバーライドされるためにだけ存在します。

   通常 :meth:`value_encode` と :meth:`value_decode` はともに
   *value_decode* の処理内容から逆算した範囲に収まっていなければなりません。


.. method:: BaseCookie.output([attrs[, header[, sep]]])

   HTTPヘッダ形式の文字列表現を返します。 *attrs* と *header* はそれ
   ぞれ :class:`Morsel` の :meth:`output` メソッドに送られます。 *sep*
   はヘッダの連結に用いられる文字で、デフォルトは ``'\r\n'`` (CRLF)となっています。

   .. versionchanged:: 2.5
      デフォルトのセパレータを ``'\n'`` 　から、クッキーの使用にあわせた.


.. method:: BaseCookie.output([attrs[, header[, sep]]])

   HTTPヘッダ形式の文字列表現を返します。


.. method:: BaseCookie.js_output([attrs])

   ブラウザがJavaScriptをサポートしている場合、HTTPヘッダを送信した場合と同様に動作する埋め込み可能なJavaScript
   snippetを返します。

   *attrs* の意味は :meth:`output` と同じです。


.. method:: BaseCookie.load(rawdata)

   *rawdata* が文字列であれば、 ``HTTP_COOKIE`` として処理し、その値
   を :class:`Morsel` として追加します。辞書の場合は次と同様の処理をおこないます。 ::

      for k, v in rawdata.items():
          cookie[k] = v


.. _morsel-objects:

Morselオブジェクト
------------------


.. class:: Morsel

   :rfc:`2109` の属性をキーと値で保持するabstractクラスです。

   Morselは辞書風のオブジェクトで、キーは次のような :rfc:`2109` 準拠の定数となっています。

   * ``expires``
   * ``path``
   * ``comment``
   * ``domain``
   * ``max-age``
   * ``secure``
   * ``version``
   * ``httponly``

   :attr:`httponly` 属性は、 cookie が HTTP リクエストでのみ送信されて、
   JavaScript からのはアクセスできない事を示します。これはいくつかの
   クロスサイトスクリプティングの脅威を和らげることを意図しています。

   キーの大小文字は区別されません。

   .. versionadded:: 2.6
      :attr:`httponly` 属性が追加されました。


.. attribute:: Morsel.value

   クッキーの値。


.. attribute:: Morsel.coded_value

   実際に送信する形式にエンコードされたcookieの値。


.. attribute:: Morsel.key

   cookieの名前。


.. method:: Morsel.set(key, value, coded_value)

   メンバ *key* 、 *value* 、 *coded_value* に値をセットします。


.. method:: Morsel.isReservedKey(K)

   *K* が :class:`Morsel` のキーであるかどうかを判定します。


.. method:: Morsel.output([attrs[, header]])

   MoselをHTTPヘッダ形式の文字列表現にして返します。 *attrs* を指定しない場合、デフォルトですべての属性を含めます。 *attrs* を指定する場合、
   属性をリストで渡さなければなりません。 *header* のデフォルトは ``"Set-Cookie:"`` です。


.. method:: Morsel.js_output([attrs])

   ブラウザがJavaScriptをサポートしている場合、HTTPヘッダを送信した場合と同様に動作する埋め込み可能なJavaScript
   snippetを返します。

   *attrs* の意味は :meth:`output` と同じです。


.. method:: Morsel.OutputString([attrs])

   Moselの文字列表現をHTTPやJavaScriptで囲まずに出力します。

   *attrs* の意味は :meth:`output` と同じです。


.. _cookie-example:

例
--

次の例は :mod:`Cookie` の使い方を示したものです。

.. doctest::
   :options: +NORMALIZE_WHITESPACE

   >>> import Cookie
   >>> C = Cookie.SimpleCookie()
   >>> C["fig"] = "newton"
   >>> C["sugar"] = "wafer"
   >>> print C # generate HTTP headers
   Set-Cookie: fig=newton
   Set-Cookie: sugar=wafer
   >>> print C.output() # same thing
   Set-Cookie: fig=newton
   Set-Cookie: sugar=wafer
   >>> C = Cookie.SimpleCookie()
   >>> C["rocky"] = "road"
   >>> C["rocky"]["path"] = "/cookie"
   >>> print C.output(header="Cookie:")
   Cookie: rocky=road; Path=/cookie
   >>> print C.output(attrs=[], header="Cookie:")
   Cookie: rocky=road
   >>> C = Cookie.SimpleCookie()
   >>> C.load("chips=ahoy; vienna=finger") # load from a string (HTTP header)
   >>> print C
   Set-Cookie: chips=ahoy
   Set-Cookie: vienna=finger
   >>> C = Cookie.SimpleCookie()
   >>> C.load('keebler="E=everybody; L=\\"Loves\\"; fudge=\\012;";')
   >>> print C
   Set-Cookie: keebler="E=everybody; L=\"Loves\"; fudge=\012;"
   >>> C = Cookie.SimpleCookie()
   >>> C["oreo"] = "doublestuff"
   >>> C["oreo"]["path"] = "/"
   >>> print C
   Set-Cookie: oreo=doublestuff; Path=/
   >>> C["twix"] = "none for you"
   >>> C["twix"].value
   'none for you'
   >>> C = Cookie.SimpleCookie()
   >>> C["number"] = 7 # equivalent to C["number"] = str(7)
   >>> C["string"] = "seven"
   >>> C["number"].value
   '7'
   >>> C["string"].value
   'seven'
   >>> print C
   Set-Cookie: number=7
   Set-Cookie: string=seven
   >>> # SerialCookie と SmartCookie は非推奨です。
   >>> # これらを使うとセキュリティーホールができることがあります。
   >>> C = Cookie.SerialCookie()
   >>> C["number"] = 7
   >>> C["string"] = "seven"
   >>> C["number"].value
   7
   >>> C["string"].value
   'seven'
   >>> print C
   Set-Cookie: number="I7\012."
   Set-Cookie: string="S'seven'\012p1\012."
   >>> C = Cookie.SmartCookie()
   >>> C["number"] = 7
   >>> C["string"] = "seven"
   >>> C["number"].value
   7
   >>> C["string"].value
   'seven'
   >>> print C
   Set-Cookie: number="I7\012."
   Set-Cookie: string=seven

