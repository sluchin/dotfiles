
:mod:`xml.sax.xmlreader` --- XML パーサのインタフェース
=========================================================

.. module:: xml.sax.xmlreader
   :synopsis: SAX 準拠の XML パーサが実装すべきインタフェースです。
.. moduleauthor:: Lars Marius Garshol <larsga@garshol.priv.no>
.. sectionauthor:: Martin v. Löwis <martin@v.loewis.de>


.. versionadded:: 2.0

各 SAX パーサは Python モジュールとして :class:`XMLReader` インタフェース\
を実装しており、関数 :func:`create_parser` を提供しています。この関数は\
新たなパーサ・オブジェクトを生成する際、
:func:`xml.sax.make_parser` から引き数なしで呼び出されます。


.. class:: XMLReader()

   SAX パーサが継承可能な基底クラスです。


.. class:: IncrementalParser()

   入力ソースをパースする際、すべてを一気に処理しないで、途中でドキュメ\
   ントのチャンクを取得したいことがあります。SAX リーダは通常、ファイル\
   全体を一気に読み込まずチャンク単位で処理するのですが、全体の処理が終\
   わるまで :meth:`parse` は return しません。つまり、\
   IncrementalParser インタフェースは :meth:`parse` にこのような排\
   他的挙動を望まないときに使われます。

   パーサのインスタンスが作成されると、feed メソッドを通じてすぐに、デー\
   タを受け入れられるようになります。close メソッドの呼出しでパースが終\
   わると、パーサは新しいデータを受け入れられるように、reset メソッドを\
   呼び出されなければなりません。

   これらのメソッドをパース処理の途中で呼び出すことはできません。つまり、\
   パースが実行された後で、パーサから return する前に呼び出す必要がある\
   のです。

   なお、SAX 2.0 ドライバを書く人のために、XMLReader インタフェースの \
   parse メソッドがデフォルトで、IncrementalParser
   の feed、close、reset メソッドを使って実装されています。


.. class:: Locator()

   SAX イベントとドキュメントの位置を関連付けるインタフェースです。\
   locator オブジェクトは DocumentHandler メソッドを呼び出している間\
   だけ正しい情報を返し、それ以外とのときに呼び出すと、予測できない結果\
   が返ります。情報を取得できない場合、メソッドは ``None`` を返すこ\
   ともあります。


.. class:: InputSource([systemId])

   :class:`XMLReader` がエンティティを読み込むために必要な情報をカプセ\
   ル化します。

   このクラスには公開識別子、システム識別子、(場合によっては文字エンコー\
   ディング情報を含む)バイト・ストリーム、そしてエンティティの文字スト\
   リームなどの情報が含まれます。

   アプリケーションは :meth:`XMLReader.parse` メソッドに渡す引き数、\
   または EntityResolver.resolveEntity の戻り値としてこのオブジェトを作\
   成します。

   :class:`InputSource` はアプリケーション側に属します。
   :class:`XMLReader` はアプリケーションから渡された
   :class:`InputSource` オブジェクトの変更を許していませんが、\
   コピーを作り、それを変更することは可能です。


.. class:: AttributesImpl(attrs)

   :class:`Attributes` インタフェース (
   :ref:`attributes-objects` 参照)の実装です。辞書風のオブジェクトで、
   :meth:`startElement` 内で要素の属性表示をおこないます。多くの辞書\
   風オブジェクト操作に加え、ほかにもインタフェースに記述されているメ\
   ソッドを、多数サポートしています。このクラスのオブジェクトはリーダ\
   によってインスタンスを作成しなければなりません。また、 *attrs* は\
   属性名と属性値を含む辞書風オブジェクトでなければなりません。


.. class:: AttributesNSImpl(attrs, qnames)

   :class:`AttributesImpl` を名前空間認識型に改良したクラスで、
   :meth:`startElementNS` に渡されます。 :class:`AttributesImpl` の派\
   生クラスですが、 *namespaceURI* と *localname* 、この2つのタプ\
   ルを解釈します。さらに、元のドキュメントに出てくる修飾名を返す多くの\
   メソッドを提供します。このクラスは :class:`AttributesNS`
   インタフェース (:ref:`attributes-ns-objects` 参照) の実装です。


.. _xmlreader-objects:

XMLReader オブジェクト
----------------------

:class:`XMLReader` は次のメソッドをサポートします。:


.. method:: XMLReader.parse(source)

   入力ソースを処理し、SAX イベントを発生させます。 *source* オブジェクトにはシステム識別子(入力ソースを特定する文字列 -- 一般にファイル\
   名やURL)、ファイル風オブジェクト、または :class:`InputSource` オブジェクトを指定できます。 :meth:`parse` から
   return された段階で、入力データの処理は完了、パーサ・オブジェクトは破棄ないしリセットされます。
   なお、現在の実装はバイト・ストリームのみをサポートしており、文字ストリームの処理は将来の課題になっています。


.. method:: XMLReader.getContentHandler()

   現在の :class:`ContentHandler` を返します。


.. method:: XMLReader.setContentHandler(handler)

   現在の :class:`ContentHandler` をセットします。 :class:`ContentHandler`
   がセットされていない場合、コンテント・イベントは破棄されます。


.. method:: XMLReader.getDTDHandler()

   現在の :class:`DTDHandler` を返します。


.. method:: XMLReader.setDTDHandler(handler)

   現在の :class:`DTDHandler` をセットします。 :class:`DTDHandler` がセットされていない場合、DTD
   イベントは破棄されます。


.. method:: XMLReader.getEntityResolver()

   現在の :class:`EntityResolver` を返します。


.. method:: XMLReader.setEntityResolver(handler)

   現在の :class:`EntityResolver` をセットします。 :class:`EntityResolver`
   がセットされていない場合、外部エンティティとして解決されるべきものが、システム識別子として解釈されてしまうため、該当するものがなければ結果
   的にエラーとなります。


.. method:: XMLReader.getErrorHandler()

   現在の :class:`ErrorHandler` を返します。


.. method:: XMLReader.setErrorHandler(handler)

   現在のエラー・ハンドラをセットします。 :class:`ErrorHandler` がセットされていない場合、エラーは例外を発生し、警告が表示されます。


.. method:: XMLReader.setLocale(locale)

   アプリケーションにエラーや警告のロカール設定を許可します。

   SAX パーサにとって、エラーや警告の地域化は必須ではありません。しかし、パーサは要求されたロカールをサポートしていない場合、SAX 例外を発生さ\
   せなければなりません。アプリケーションはパースの途中でロカールを変更することもできます。


.. method:: XMLReader.getFeature(featurename)

   機能 *featurename* の現在の設定を返します。その機能が認識できないときは、 :exc:`SAXNotRecognizedException`
   を発生させます。広く使われている機能名の一覧はモジュール :mod:`xml.sax.handler` に書かれています。


.. method:: XMLReader.setFeature(featurename, value)

   機能名 *featurename* に値 *value* をセットします。その機能が\
   認識できないときは、 :exc:`SAXNotRecognizedException` を発生させ\
   ます。また、パーサが指定された機能や設定をサポートしていないときは、
   :exc:`SAXNotSupportedException` を発生させます。


.. method:: XMLReader.getProperty(propertyname)

   属性名 *propertyname* の現在の値を返します。その属性が認識できないときは、
   :exc:`SAXNotRecognizedException` を発生させます。
   広く使われている属性名の一覧はモジュール :mod:`xml.sax.handler` に書かれています。


.. method:: XMLReader.setProperty(propertyname, value)

   属性名 *propertyname* に値 *value* をセットします。その機能\
   が認識できないときは、 :exc:`SAXNotRecognizedException` を発生さ\
   せます。また、パーサが指定された機能や設定をサポートしていないときは、
   :exc:`SAXNotSupportedException` を発生させます。


.. _incremental-parser-objects:

IncrementalParser オブジェクト
------------------------------

:class:`IncrementalParser` のインスタンスは次の追加メソッドを提供します。:


.. method:: IncrementalParser.feed(data)

   *data* のチャンクを処理します。


.. method:: IncrementalParser.close()

   ドキュメントの終わりを決定します。終わりに達した時点でドキュメントが整形式であるかどうかを判別、ハンドラを起動後、パース時に使用した資源を解放します。


.. method:: IncrementalParser.reset()

   このメソッドは close が呼び出された後、次のドキュメントをパース可能にするため、パーサのリセットするのに呼び出されます。close 後、reset
   を呼び出さずに parse や feed を呼び出した場合の戻り値は未定義です。


.. _locator-objects:

Locator オブジェクト
--------------------

:class:`Locator` のインスタンスは次のメソッドを提供します。:


.. method:: Locator.getColumnNumber()

   現在のイベントが終了する列番号を返します。


.. method:: Locator.getLineNumber()

   現在のイベントが終了する行番号を返します。


.. method:: Locator.getPublicId()

現在の文書イベントの公開識別子を返します。


.. method:: Locator.getSystemId()

   現在のイベントのシステム識別子を返します。


.. _input-source-objects:

InputSource オブジェクト
------------------------


.. method:: InputSource.setPublicId(id)

   この :class:`InputSource` の公開識別子をセットします。


.. method:: InputSource.getPublicId()

   この :class:`InputSource` の公開識別子を返します。


.. method:: InputSource.setSystemId(id)

   この :class:`InputSource` のシステム識別子をセットします。


.. method:: InputSource.getSystemId()

   この :class:`InputSource` のシステム識別子を返します。


.. method:: InputSource.setEncoding(encoding)

   この :class:`InputSource` の文字エンコーディングをセットします。

   指定するエンコーディングは XML エンコーディング宣言として定義された文字列でなければなりません(セクション 4.3.3 の XML 勧告を参照)。

   :class:`InputSource` のエンコーディング属性は、 :class:`InputSource` が\
   たとえ文字ストリームを含んでいたとしても、無視されます。


.. method:: InputSource.getEncoding()

   この :class:`InputSource` の文字エンコーディングを取得します。


.. method:: InputSource.setByteStream(bytefile)

   この入力ソースのバイトストリーム(Python のファイル風オブジェクトですが、バイト列と文字の相互変換はサポートしません)を設定します。

   なお、文字ストリームが指定されてもSAX パーサは無視し、バイト・ストリームを使って指定された URI に接続しようとします。

   アプリケーション側でバイト・ストリームの文字エンコーディングを知っている場合は、setEncoding メソッドを使って指定する必要があります。


.. method:: InputSource.getByteStream()

   この入力ソースのバイトストリームを取得します。

   getEncoding メソッドは、このバイト・ストリームの文字エンコーディングを返します。認識できないときは None を返します。


.. method:: InputSource.setCharacterStream(charfile)

   この入力ソースの文字ストリームをセットします(ストリームは Python 1.6 の Unicode-wrapped
   なファイル風オブジェクトで、ユニコード文字列への変換をサポートしていなければなりません)。

   なお、文字ストリームが指定されても SAX パーサは無視、システム識別子とみなし、バイト・ストリームを使って URI に接続しようとします。


.. method:: InputSource.getCharacterStream()

   この入力ソースの文字ストリームを取得します。


.. _attributes-objects:

The :class:`Attributes` インタフェース
----------------------------------------

:class:`Attributes` オブジェクトは :meth:`copy`, :meth:`get`,
:meth:`has_key`, :meth:`items`, :meth:`keys`, :meth:`values`
などを含む、マッピング・プロトコルの一部を実装したものです。さらに次のメソッドも提供されています。:


.. method:: Attributes.getLength()

   属性の数を返す。


.. method:: Attributes.getNames()

   属性の名前を返す。


.. method:: Attributes.getType(name)

   属性名 *name* のタイプを返す。通常は ``'CDATA'``


.. method:: Attributes.getValue(name)

   属性 *name* の値を返す。

.. getValueByQName, getNameByQName, getQNameByName, getQNames available
.. here already, but documented only for derived class.


.. _attributes-ns-objects:

:class:`AttributesNS` インタフェース
--------------------------------------

このインタフェースは :class:`Attributes` インタフェース
(:ref:`attributes-objects` 参照) のサブタイプです。
Attributes インタフェースがサポートしているすべてのメソッドは :class:`AttributesNS` オブジェクトでも利用可能です。

そのほか、次のメソッドがサポートされています。:


.. method:: AttributesNS.getValueByQName(name)

   修飾名の値を返す。


.. method:: AttributesNS.getNameByQName(name)

   修飾名 *name* に対応する ``(namespace, localname)`` のペアを返す。


.. method:: AttributesNS.getQNameByName(name)

   ``(namespace, localname)`` のペアに対応する修飾名を返す。


.. method:: AttributesNS.getQNames()

   すべての属性の修飾名を返す。

