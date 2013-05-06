
:mod:`xml.sax` --- SAX2 パーサのサポート
========================================

.. module:: xml.sax
   :synopsis: SAX2 基底クラスと有用な関数のパッケージ
.. moduleauthor:: Lars Marius Garshol <larsga@garshol.priv.no>
.. sectionauthor:: Fred L. Drake, Jr. <fdrake@acm.org>
.. sectionauthor:: Martin v. Löwis <martin@v.loewis.de>


.. versionadded:: 2.0

:mod:`xml.sax` パッケージは Python 用の Simple API for XML (SAX) インターフェースを実装した数多くのモジュールを提供しています。
またパッケージには SAX 例外と SAX API 利用者が頻繁に利用するであろう有用な関数群も含まれています。

その関数群は以下の通りです:


.. function:: make_parser([parser_list])

   SAX :class:`XMLReader` オブジェクトを作成して返します。
   パーサには最初に見つかったものが使われます。
   *parser_list* を指定する場合は、 :func:`create_parser`
   関数を含んでいるモジュール名のシーケンスを与える必要があります。
   *parser_list* のモジュールはデフォルトのパーサのリストに優先して使用されます。


.. function:: parse(filename_or_stream, handler[, error_handler])

   SAX パーサを作成してドキュメントをパースします。
   *filename_or_stream* として指定するドキュメントはファイル名、ファイル・オブジェクトのいずれでもかまいません。
   *handler* パラメータには SAX :class:`ContentHandler` のインスタンスを指定します。
   *error_handler* には SAX :class:`ErrorHandler` のインスタンスを指定します。
   これが指定されていないときは、すべてのエラーで :exc:`SAXParseException` 例外が発生します。
   関数の戻り値はなく、すべての処理は *handler* に渡されます。


.. function:: parseString(string, handler[, error_handler])

   :func:`parse` に似ていますが、こちらはパラメータ *string* で指定されたバッファをパースします。

典型的な SAX アプリケーションでは3種類のオブジェクト(リーダ、ハンドラ、入力元)が用いられます(ここで言うリーダとはパーサを指しています)。
言い換えると、プログラムはまず入力元からバイト列、あるいは文字列を読み込み、一連のイベントを発生させます。
発生したイベントはハンドラ・オブジェクトによって振り分けられます。
さらに言い換えると、リーダがハンドラのメソッドを呼び出すわけです。
つまり SAX アプリケーションには、リーダ・オブジェクト、(作成またはオープンされる)入力元のオブジェクト、ハンドラ・オブジェクト、そしてこれら3つのオブジェクトを連携させることが必須なのです。
前処理の最後の段階でリーダは入力をパースするために呼び出されます。
パースの過程で入力データの構造、構文にもとづいたイベントにより、ハンドラ・オブジェクトのメソッドが呼び出されます。

これらのオブジェクトは(通常アプリケーション側でインスタンスを作成しない)インターフェースに相当するものです。
Python はインターフェースという明確な概念を提供していないため、形としてはクラスが用いられています。
しかし提供されるクラスを継承せずに、アプリケーション側で独自に実装することも可能です。
:class:`InputSource` 、 :class:`Locator` 、 :class:`Attributes` 、
:class:`AttributesNS` 、 :class:`XMLReader` の各インターフェースは :mod:`xml.sax.xmlreader`
モジュールで定義されています。
ハンドラ・インターフェースは :mod:`xml.sax.handler` で定義されています。
しばしばアプリケーション側で直接インスタンスが作成される :class:`InputSource` とハンドラ・クラスは利便性のため :mod:`xml.sax`
にも含まれています。
これらのインターフェースに関しては後に解説します。

このほかに :mod:`xml.sax` は次の例外クラスも提供しています。


.. exception:: SAXException(msg[, exception])

   XML エラーと警告をカプセル化します。
   このクラスには XML パーサとアプリケーションで発生するエラーおよび警告の基本的な情報を持たせることができます。
   また機能追加や地域化のためにサブクラス化することも可能です。
   なお :class:`ErrorHandler`
   で定義されているハンドラがこの例外のインスタンスを受け取ることに注意してください。
   実際に例外を発生させることは必須でなく、情報のコンテナとして利用されることもあるからです。

   インスタンスを作成する際 *msg* はエラー内容を示す可読データにしてください。
   オプションの *exception* パラメータは ``None`` もしくはパース用コードで補足、渡って来る情報でなければなりません。

   このクラスはSAX 例外の基底クラスになります。


.. exception:: SAXParseException(msg, exception, locator)

   パースエラー時に発生する :exc:`SAXException` のサブクラスです。
   パースエラーに関する情報として、このクラスのインスタンスが SAX
   :class:`ErrorHandler` インターフェースのメソッドに渡されます。
   このクラスは :class:`SAXException` 同様 SAX
   :class:`Locator` インターフェースもサポートしています。


.. exception:: SAXNotRecognizedException(msg[, exception])

   SAX :class:`XMLReader` が認識できない機能やプロパティに遭遇したとき発生させる :exc:`SAXException` のサブクラスです。
   SAX アプリケーションや拡張モジュールにおいて同様の目的にこのクラスを利用することもできます。


.. exception:: SAXNotSupportedException(msg[, exception])

   SAX :class:`XMLReader` が要求された機能をサポートしていないとき発生させる :exc:`SAXException` のサブクラスです。
   SAX アプリケーションや拡張モジュールにおいて同様の目的にこのクラスを利用することもできます。


.. seealso::

   `SAX: The Simple API for XML <http://www.saxproject.org/>`_
      SAX API 定義に関し中心となっているサイトです。
      Java による実装とオンライン・ドキュメントが提供されています。
      実装と SAX API の歴史に関する情報のリンクも掲載されています。

   Module :mod:`xml.sax.handler`
      アプリケーションが提供するオブジェクトのインターフェース定義

   Module :mod:`xml.sax.saxutils`
      SAX アプリケーション向けの有用な関数群

   Module :mod:`xml.sax.xmlreader`
      パーサが提供するオブジェクトのインターフェース定義


.. _sax-exception-objects:

SAXException オブジェクト
-------------------------

:class:`SAXException` 例外クラスは以下のメソッドをサポートしています。


.. method:: SAXException.getMessage()

   エラー状態を示す可読メッセージを返します。


.. method:: SAXException.getException()

   カプセル化した例外オブジェクトまたは ``None`` を返します。
