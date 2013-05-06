
:mod:`xml.dom` --- 文書オブジェクトモデル (DOM) API
===================================================

.. module:: xml.dom
   :synopsis: Python のための文書オブジェクトモデル API。
.. sectionauthor:: Paul Prescod <paul@prescod.net>
.. sectionauthor:: Martin v. Löwis <loewis@informatik.hu-berlin.de>


.. versionadded:: 2.0

文書オブジェクトモデル、または "DOM" は、ワールドワイドウェブコンソーシアム (World Wide Web Consortium, W3C)
による、XML ドキュメントにアクセスしたり変更を加えたりするための、プログラミング言語間共通の API です。DOM 実装によって、XML ドキュメントは
ツリー構造として表現されます。また、クライアントコード側でツリー構造をゼロから構築できるようになります。さらに、
前述の構造に対して、よく知られたインタフェースをもつ一連のオブジェクトを通したアクセス手段も提供します。

DOM はランダムアクセスを行うアプリケーションで非常に有用です。 SAX では、一度に閲覧することができるのはドキュメントのほんの一部分です。ある SAX
要素に注目している際には、別の要素をアクセスすることはできません。またテキストノードに注目しているときには、
その中に入っている要素をアクセスすることができません。 SAX によるアプリケーションを書くときには、プログラムがドキュメント内の
どこを処理しているのかを追跡するよう、コードのどこかに記述する必要があります。SAX 自体がその作業を行ってくれることはありません。さらに、XML
ドキュメントに対する先読み (look ahead) が必要だとすると不運なことになります。

アプリケーションによっては、ツリーにアクセスできなければイベント駆動モデルを実現できません。もちろん、何らかのツリーを SAX
イベントに応じて自分で構築することもできるでしょうが、 DOM ではそのようなコードを書かなくてもよくなります。 DOM は XML
データに対する標準的なツリー表現なのです。

文書オブジェクトモデルは、W3C によっていくつかの段階、W3C の用語で言えば "レベル (level)" で定義されています。 Python においては、
DOM API への対応付けは実質的には DOM レベル 2 勧告に基づいています。

.. XXX PyXML はお亡くなりに...
.. 現在はドラフト形式でのみ入手できるレベル 3 仕様への対応付けは、
   `Python XML 分科会 (Special Interest Group) <http://www.python.org/sigs/xml-sig/>`_
   により、 `PyXML パッケージ <http://pyxml.sourceforge.net/>`_ の一部として開発中です。 DOM レベル 3
   サポートの現在の状態についての情報は、 PyXML パッケージに同梱されているドキュメントを参照してください。

.. What if your needs are somewhere between SAX and the DOM?  Perhaps
   you cannot afford to load the entire tree in memory but you find the
   SAX model somewhat cumbersome and low-level.  There is also a module
   called xml.dom.pulldom that allows you to build trees of only the
   parts of a document that you need structured access to.  It also has
   features that allow you to find your way around the DOM.
   See http://www.prescod.net/python/pulldom

DOM アプリケーションは、普通は XML を DOM に解析するところから始まります。どのようにして解析を行うかについては DOM レベル 1 では全く
カバーしておらず、レベル 2 では限定的な改良だけが行われました: レベル 2 では :class:`Document` を生成するメソッドを提供する
:class:`DOMImplementation` オブジェクトクラスがありますが、実装に依存しない方法で XML
リーダ(reader)/パーザ(parser)/文書ビルダ (Document builder) にアクセスする方法はありません。また、既存の
:class:`Document` オブジェクトなしにこれらのメソッドにアクセスするような、よく定義された方法もありません。 Python では、各々の
DOM 実装で :func:`getDOMImplementation` が定義されているはずです。 DOM レベル 3
ではロード(Load)/ストア(Store) 仕様が追加され、リーダのインタフェースにを定義していますが、Python 標準ライブラリではまだ
利用することができません。

DOM 文書オブジェクトを生成したら、そのプロパティとメソッドを使って XML 文書の一部にアクセスできます。これらのプロパティは DOM
仕様で定義されています; 本リファレンスマニュアルでは、 Python において DOM 仕様がどのように解釈されているかを記述しています。

W3C から提供されている仕様は、 DOM API を Java、ECMAScript、および OMG IDL で定義しています。ここで定義されている
Python での対応づけは、大部分がこの仕様の IDL 版に基づいていますが、厳密な準拠は必要とされていません (実装で IDL
の厳密な対応付けをサポートするのは自由ですが)。API への対応付けに関する詳細な議論は :ref:`dom-conformance` を参照してください。


.. seealso::

   `Document Object Model (DOM) Level 2 Specification <http://www.w3.org/TR/DOM-Level-2-Core/>`_
      Python DOM API が準拠している W3C 勧告。

   `Document Object Model (DOM) Level 1 Specification <http://www.w3.org/TR/REC-DOM-Level-1/>`_
      :mod:`xml.dom.minidom` でサポートされている W3C の DOM に関する勧告。

   `Python Language Mapping Specification <http://www.omg.org/spec/PYTH/1.2/PDF>`_
      このドキュメントでは OMG IDL から Python への対応付けを記述しています。


モジュールの内容
----------------

:mod:`xml.dom` には、以下の関数が収められています:


.. function:: registerDOMImplementation(name, factory)

   ファクトリ関数 (factory function) *factory* を名前 *name* で登録します。ファクトリ関数は
   :class:`DOMImplementation` インタフェースを実装するオブジェクトを返さなければなりません。ファクトリ関数は
   毎回同じオブジェクトを返すこともでき、呼び出されるたびに、特定の実装 (例えば実装が何らかのカスタマイズをサポートしている場合) における、
   適切な新たなオブジェクトを返すこともできます。


.. function:: getDOMImplementation([name[, features]])

   適切な DOM 実装を返します *name* は、よく知られた DOM 実装のモジュール名か、 ``None`` になります。 ``None`` でない場合、
   対応するモジュールを import して、import が成功した場合 :class:`DOMImplementation`
   オブジェクトを返します。 *name* が与えられておらず、環境変数 :envvar:`PYTHON_DOM` が設定されていた場合、 DOM
   実装を見つけるのに環境変数が使われます。

   *name* が与えられない場合、利用可能な実装を調べて、指定された機能 (feature) セットを持つものを探します。実装が見つからなければ
   :exc:`ImportError` を送出します。 *features* のリストは ``(feature, version)`` の
   ペアからなるシーケンスで、利用可能な :class:`DOMImplementation` オブジェクトの :meth:`hasFeature`
   メソッドに渡されます。

いくつかの便利な定数も提供されています:


.. data:: EMPTY_NAMESPACE

   DOM 内のノードに名前空間が何も関連づけられていないことを示すために使われる値です。この値は通常、ノードの :attr:`namespaceURI` の値
   として見つかったり、名前空間特有のメソッドに対する *namespaceURI* パラメタとして使われます。

   .. versionadded:: 2.2


.. data:: XML_NAMESPACE

   `Namespaces in XML <http://www.w3.org/TR/REC-xml-names/>`_ (4 節)
   で定義されている、予約済みプレフィクス (reserved prefix) ``xml`` に関連付けられた名前空間 URI です。

   .. versionadded:: 2.2


.. data:: XMLNS_NAMESPACE

   `Document Object Model (DOM) Level 2 Core Specification
   <http://www.w3.org/TR/DOM-Level-2-Core/core.html>`_ (1.1.8 節)
   で定義されている、名前空間宣言への名前空間 URI です。

   .. versionadded:: 2.2


.. data:: XHTML_NAMESPACE

   `XHTML 1.0: The Extensible HyperText Markup Language
   <http://www.w3.org/TR/xhtml1/>`_ (3.1.1 節) で定義されている、XHTML 名前空間 URI です。

   .. versionadded:: 2.2

加えて、 :mod:`xml.dom` には基底となる :class:`Node` クラスと DOM
例外クラスが収められています。このモジュールで提供されている :class:`Node` クラスは DOM 仕様で定義されているメソッドや属性は
何ら実装していません; これらは具体的な DOM 実装において提供しなければなりません。このモジュールの一部として提供されている :class:`Node`
クラスでは、具体的な :class:`Node` オブジェクトの :attr:`nodeType` 属性として使う定数を提供しています; これらの
定数は、DOM 仕様に適合するため、クラスではなくモジュールのレベルに配置されています。

.. Should the Node documentation go here?


.. _dom-objects:

DOM 内のオブジェクト
--------------------

DOM について最も明確に限定しているドキュメントは W3C による DOM 仕様です。

DOM 属性は単純な文字列としてだけではなく、ノードとして操作されるかもしれないので注意してください。とはいえ、そうしなければならない
場合はかなり稀なので、今のところ記述されていません。

+--------------------------------+-----------------------------------+--------------------------------------------------------------------+
| インタフェース                 | 節                                | 目的                                                               |
+================================+===================================+====================================================================+
| :class:`DOMImplementation`     | :ref:`dom-implementation-objects` | 根底にある実装へのインタフェース。                                 |
|                                |                                   |                                                                    |
+--------------------------------+-----------------------------------+--------------------------------------------------------------------+
| :class:`Node`                  | :ref:`dom-node-objects`           | ドキュメント内の大部分のオブジェクトのに対する基底インタフェース。 |
+--------------------------------+-----------------------------------+--------------------------------------------------------------------+
| :class:`NodeList`              | :ref:`dom-nodelist-objects`       | ノードの列に対するインタフェース。                                 |
+--------------------------------+-----------------------------------+--------------------------------------------------------------------+
| :class:`DocumentType`          | :ref:`dom-documenttype-objects`   | ドキュメントを処理するために必要な宣言についての情報。             |
+--------------------------------+-----------------------------------+--------------------------------------------------------------------+
| :class:`Document`              | :ref:`dom-document-objects`       | ドキュメント全体を表現するオブジェクト。                           |
+--------------------------------+-----------------------------------+--------------------------------------------------------------------+
| :class:`Element`               | :ref:`dom-element-objects`        | ドキュメント階層内の要素ノード。                                   |
+--------------------------------+-----------------------------------+--------------------------------------------------------------------+
| :class:`Attr`                  | :ref:`dom-attr-objects`           | 階層ノード上の属性値。                                             |
+--------------------------------+-----------------------------------+--------------------------------------------------------------------+
| :class:`Comment`               | :ref:`dom-comment-objects`        | ソースドキュメント内のコメント表現。                               |
+--------------------------------+-----------------------------------+--------------------------------------------------------------------+
| :class:`Text`                  | :ref:`dom-text-objects`           | ドキュメント内のテキスト記述を含むノード。                         |
+--------------------------------+-----------------------------------+--------------------------------------------------------------------+
| :class:`ProcessingInstruction` | :ref:`dom-pi-objects`             | 処理命令 (processing instruction)                                  |
|                                |                                   | 表現。                                                             |
+--------------------------------+-----------------------------------+--------------------------------------------------------------------+

さらに追加の節として、 Python で DOM を利用するために定義されている例外について記述しています。


.. _dom-implementation-objects:

DOMImplementation オブジェクト
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:class:`DOMImplementation` インタフェースは、利用している DOM 実装において特定の機能が利用可能かどうかを決定するための方法を
アプリケーションに提供します。DOM レベル 2 では、 :class:`DOMImplementation` を使って新たな :class:`Document`
オブジェクトや :class:`DocumentType` オブジェクトを生成する機能も追加しています。


.. method:: DOMImplementation.hasFeature(feature, version)

   機能名 *feature* とバージョン番号 *version* で識別される機能（feature）が実装されていればtrueを返します。


.. method:: DOMImplementation.createDocument(namespaceUri, qualifiedName, doctype)

   新たな(DOMのスーパークラスである) :class:`Document` クラスのオブジェクトを返します。
   このクラスは *namespaceUri* と *qualifiedName* が設定された子クラス :class:`Element` のオブジェクトを所有しています。
   *doctype* は :meth:`createDocumentType` によって生成された :class:`DocumentType` クラスのオブジェクト、
   または ``None`` である必要があります。 Python DOM APIでは、子クラスである :class:`Element` を作成しないことを
   示すために、はじめの２つの引数を ``None`` に設定することができます。


.. method:: DOMImplementation.createDocumentType(qualifiedName, publicId, systemId)

   新たな :class:`DocumentType` クラスのオブジェクトを返します。このオブジェクトは *qualifiedName* 、 *publicId* 、そして
   *systemId* 文字列をふくんでおり、XML文書の形式情報を表現しています。


.. _dom-node-objects:

Node オブジェクト
^^^^^^^^^^^^^^^^^

XML 文書の全ての構成要素は :class:`Node` のサブクラスです。


.. attribute:: Node.nodeType

   ノード (node) の型を表現する整数値です。型に対応する以下のシンボル定数: :const:`ELEMENT_NODE` 、
   :const:`ATTRIBUTE_NODE` 、 :const:`TEXT_NODE` 、 :const:`CDATA_SECTION_NODE` 、
   :const:`ENTITY_NODE` 、 :const:`PROCESSING_INSTRUCTION_NODE` 、
   :const:`COMMENT_NODE` 、 :const:`DOCUMENT_NODE` 、 :const:`DOCUMENT_TYPE_NODE` 、
   :const:`NOTATION_NODE` 、が :class:`Node` オブジェクトで定義されています。読み出し専用の属性です。


.. attribute:: Node.parentNode

   現在のノードの親ノードか、文書ノードの場合には ``None`` になります。この値は常に :class:`Node` オブジェクトか ``None``
   になります。 :class:`Element` ノードの場合、この値はルート要素 (root element) の場合を除き親要素 (parent
   element) となり、ルート要素の場合には :class:`Document` オブジェクトとなります。 :class:`Attr`
   ノードの場合、この値は常に ``None`` となります。読み出し専用の属性です。


.. attribute:: Node.attributes

   属性オブジェクトの :class:`NamedNodeMap` です。要素だけがこの属性に実際の値を持ちます; その他のオブジェクトでは、この属性を
   ``None`` にします。読み出し専用の属性です。


.. attribute:: Node.previousSibling

   このノードと同じ親ノードを持ち、直前にくるノードです。例えば、 *self* 要素のの開始タグの直前にくる終了タグを持つ要素です。もちろん、XML
   文書は要素だけで構成されているだけではないので、直前にくる兄弟関係にある要素 (sibling) はテキストやコメント、
   その他になる可能性があります。このノードが親ノードにおける先頭の子ノードである場合、属性値は ``None`` になります。読み出し専用の属性です。


.. attribute:: Node.nextSibling

   このノードと同じ親ノードを持ち、直後にくるノードです。例えば、 :attr:`previousSibling` も参照してください。
   このノードが親ノードにおける末尾頭の子ノードである場合、属性値は ``None`` になります。読み出し専用の属性です。


.. attribute:: Node.childNodes

   このノード内に収められているノードからなるリストです。読み出し専用の属性です。


.. attribute:: Node.firstChild

   このノードに子ノードがある場合、その先頭のノードです。そうでない場合 ``None`` になります。読み出し専用の属性です。


.. attribute:: Node.lastChild

   このノードに子ノードがある場合、その末尾のノードです。そうでない場合 ``None`` になります。読み出し専用の属性です。


.. attribute:: Node.localName

   :attr:`tagName` にコロンがあれば、コロン以降の部分に、なければ :attr:`tagName` 全体になります。値は文字列です。


.. attribute:: Node.prefix

   :attr:`tagName` のコロンがあれば、コロン以前の部分に、なければ空文字列になります。値は文字列か、 ``None`` になります。


.. attribute:: Node.namespaceURI

   要素名に関連付けられた名前空間です。文字列か ``None`` になります。読み出し専用の属性です。


.. attribute:: Node.nodeName

   この属性はノード型ごとに異なる意味を持ちます; 詳しくは DOM 仕様を参照してください。この属性で得られることになる情報は、全てのノード型では
   :attr:`tagName` 、属性では :attr:`name` プロパティといったように、常に他のプロパティで得ることができます。全てのノード型で、
   この属性の値は文字列か ``None`` になります。読み出し専用の属性です。


.. attribute:: Node.nodeValue

   この属性はノード型ごとに異なる意味を持ちます; 詳しくは DOM 仕様を参照してください。その序今日は :attr:`nodeName` と似ています。
   この属性の値は文字列か ``None`` になります。


.. method:: Node.hasAttributes()

   ノードが何らかの属性を持っている場合に真を返します。


.. method:: Node.hasChildNodes()

   ノードが何らかの子ノードを持っている場合に真を返します。


.. method:: Node.isSameNode(other)

   *other* がこのノードと同じノードを参照している場合に真を返します。このメソッドは、何らかのプロキシ (proxy) 機構を利用するような DOM
   実装で特に便利です (一つ以上のオブジェクトが同じノードを参照するかもしれないからです)。

   .. note::

      このメソッドは DOM レベル 3 API で提案されており、まだ "ワーキングドラフト(working draft)" の段階です。しかし、
      このインタフェースだけは議論にはならないと考えられます。 W3C による変更は必ずしも Python DOM インタフェースにおける
      このメソッドに影響するとは限りません (ただしこのメソッドに対する何らかの新たな W3C API もサポートされるかもしれません)。


.. method:: Node.appendChild(newChild)

   現在のノードの子ノードリストの末尾に新たな子ノードを追加し、 *newChild* を返します。
   もしノードが既にツリーにあれば、最初に削除されます。


.. method:: Node.insertBefore(newChild, refChild)

   新たな子ノードを既存の子ノードの前に挿入します。 *refChild* は現在のノードの子ノードである場合に限られます; そうでない場合、
   :exc:`ValueError` が送出されます。 *newChild* が返されます。
   もし *refChild* が ``None`` なら、 *newChild* を子ノードリストの最後に挿入します。


.. method:: Node.removeChild(oldChild)

   子ノードを削除します。 *oldChild* はこのノードの子ノードでなければなりません。そうでない場合、 :exc:`ValueError` が送出されます。
   成功した場合 *oldChild* が返されます。 *oldChild* をそれ以降使わない場合、 :meth:`unlink` メソッドを
   呼び出さなければなりません。


.. method:: Node.replaceChild(newChild, oldChild)

   既存のノードと新たなノードを置き換えます。この操作は *oldChild* が現在のノードの子ノードである場合に限られます; そうでない場合、
   :exc:`ValueError` が送出されます。


.. method:: Node.normalize()

   一続きのテキスト全体を一個の :class:`Text` インスタンスとして保存するために隣接するテキストノードを結合します。これにより、多くの
   アプリケーションで DOM ツリーからのテキスト処理が簡単になります。

   .. versionadded:: 2.1


.. method:: Node.cloneNode(deep)

   このノードを複製 (clone) します。 *deep* を設定すると、子ノードも同様に複製することを意味します。複製されたノードを返します。


.. _dom-nodelist-objects:

NodeList オブジェクト
^^^^^^^^^^^^^^^^^^^^^

:class:`NodeList` は、ノードからなるシーケンスを表現します。これらのオブジェクトは DOM コア勧告 (DOM Core
recommendation) において、二通りに使われています: :class:`Element` オブジェクトでは、子ノードのリストを提供するのに
:class:`NodeList` を利用します。また、このインタフェースにおける :class:`Node` の
:meth:`getElementsByTagName` および :meth:`getElementsByTagNameNS` メソッドは、クエリに対する結果
を表現するのに :class:`NodeList` を利用します。

DOM レベル 2 勧告では、これらのオブジェクトに対し、メソッドと属性を一つづつ定義しています:


.. method:: NodeList.item(i)

   シーケンスに *i* 番目の要素がある場合にはその要素を、そうでない場合には ``None`` を返します。 *i* はゼロよりも小さくてはならず、
   シーケンスの長さ以上であってはなりません。


.. attribute:: NodeList.length

   シーケンス中のノードの数です。

この他に、Python の DOM インタフェースでは、 :class:`NodeList` オブジェクトを Python
のシーケンスとして使えるようにするサポートが追加されていることが必要です。 :class:`NodeList` の実装では、全て :meth:`__len__`
と :meth:`__getitem__` をサポートしなければなりません; このサポートにより、 :keyword:`for` 文内で
:class:`NodeList` にわたる繰り返しと、組み込み関数 :func:`len` の適切なサポートができるようになります。

DOM 実装が文書の変更をサポートしている場合、 :class:`NodeList` の実装でも :meth:`__setitem__` および
:meth:`__delitem__` メソッドをサポートしなければなりません。


.. _dom-documenttype-objects:

DocumentType オブジェクト
^^^^^^^^^^^^^^^^^^^^^^^^^

文書で宣言されている記法 (notation) やエンティティ (entity) に関する (外部サブセット(external subset)
がパーザから利用でき、情報を提供できる場合にはそれも含めた) 情報は、 :class:`DocumentType`
オブジェクトから手に入れることができます。文書の :class:`DocumentType` は、 :class:`Document` オブジェクトの
:attr:`doctype` 属性で入手することができます; 文書の ``DOCTYPE`` 宣言がない場合、文書の :attr:`doctype`
属性は、このインタフェースを持つインスタンスの代わりに ``None`` に設定されます。

:class:`DocumentType` は :class:`Node` を特殊化したもので、以下の属性を加えています:


.. attribute:: DocumentType.publicId

   文書型定義 (document type definition) の外部サブセットに対する公開識別子 (public identifier)
   です。文字列または ``None`` になります。


.. attribute:: DocumentType.systemId

   文書型定義 (document type definition) の外部サブセットに対するシステム識別子 (system identifier)
   です。文字列の URI または ``None`` になります。


.. attribute:: DocumentType.internalSubset

   ドキュメントの完全な内部サブセットを与える文字列です。サブセットを囲むブラケットは含みません。ドキュメントが内部サブセットを持たない場合、この値は
   ``None`` です。


.. attribute:: DocumentType.name

   ``DOCTYPE`` 宣言でルート要素の名前が与えられている場合、その値になります。


.. attribute:: DocumentType.entities

   外部エンティティの定義を与える :class:`NamedNodeMap` です。複数回定義されているエンティティに対しては、最初の定義だけが提供されます
   (その他は XML 勧告での要求仕様によって無視されます)。パーザによって情報が提供されないか、エンティティが定義されていない場合には、この値は
   ``None`` になることがあります。


.. attribute:: DocumentType.notations

   記法の定義を与える :class:`NamedNodeMap` です。複数回定義されている記法名に対しては、最初の定義だけが提供されます (その他は XML
   勧告での要求仕様によって無視されます)。パーザによって情報が提供されないか、エンティティが定義されていない場合には、この値は ``None``
   になることがあります。


.. _dom-document-objects:

Document オブジェクト
^^^^^^^^^^^^^^^^^^^^^

:class:`Document` は XML ドキュメント全体を表現し、その構成要素である要素、属性、処理命令、コメント等が入っています。
:class:`Document` は :class:`Node` からプロパティを継承していることを思い出してください。


.. attribute:: Document.documentElement

   ドキュメントの唯一無二のルート要素です。


.. method:: Document.createElement(tagName)

   新たな要素ノードを生成して返します。要素は、生成された時点ではドキュメント内に挿入されません。 :meth:`insertBefore` や
   :meth:`appendChild` のような他のメソッドの一つを使って明示的に挿入を行う必要があります。


.. method:: Document.createElementNS(namespaceURI, tagName)

   名前空間を伴う新たな要素ノードを生成して返します。 *tagName* にはプレフィクス (prefix) があってもかまいません。要素は、生成された
   時点では文書内に挿入されません。 :meth:`insertBefore` や :meth:`appendChild` のような他のメソッドの一つを使って
   明示的に挿入を行う必要があります。 :meth:`appendChild`.


.. method:: Document.createTextNode(data)

   パラメタで渡されたデータの入ったテキストノードを生成して返します。他の生成 (create) メソッドと同じく、このメソッドは生成された
   ノードをツリーに挿入しません。


.. method:: Document.createComment(data)

   パラメタで渡されたデータの入ったコメントノードを生成して返します。他の生成 (create) メソッドと同じく、このメソッドは生成された
   ノードをツリーに挿入しません。


.. method:: Document.createProcessingInstruction(target, data)

   パラメタで渡された *target* および *data* の入った処理命令ノードを生成して返します。他の生成 (create) メソッドと同じく、
   このメソッドは生成されたノードをツリーに挿入しません。


.. method:: Document.createAttribute(name)

   属性ノードを生成して返します。このメソッドは属性ノードを特定の要素に関連づけることはしません。新たに生成された属性インスタンスを使うには、適切な
   :class:`Element` オブジェクトの :meth:`setAttributeNode` を使わなければなりません。


.. method:: Document.createAttributeNS(namespaceURI, qualifiedName)

   名前空間を伴う新たな属性ノードを生成して返します。 *tagName* にはプレフィクス (prefix) があってもかまいません。
   このメソッドは属性ノードを特定の要素に関連づけることはしません。新たに生成された属性インスタンスを使うには、適切な :class:`Element`
   オブジェクトの :meth:`setAttributeNode` を使わなければなりません。


.. method:: Document.getElementsByTagName(tagName)

   全ての下位要素 (直接の子要素、子要素の子要素、等) から、特定の要素型名を持つものを検索します。


.. method:: Document.getElementsByTagNameNS(namespaceURI, localName)

   全ての下位要素 (直接の子要素、子要素の子要素、等) から、特定の名前空間 URI とローカル名 (local name) を持つものを検索します。
   ローカル名は名前空間におけるプレフィクス以降の部分です。


.. _dom-element-objects:

Element オブジェクト
^^^^^^^^^^^^^^^^^^^^

:class:`Element` は :class:`Node` のサブクラスです。このため :class:`Node` クラスの全ての属性を継承します。


.. attribute:: Element.tagName

   要素型名です。名前空間使用の文書では、要素型名中にコロンがあるかもしれません。値は文字列です。


.. method:: Element.getElementsByTagName(tagName)

   :class:`Document` クラス内における同名のメソッドと同じです。


.. method:: Element.getElementsByTagNameNS(namespaceURI, localName)

   :class:`Document` クラス内における同名のメソッドと同じです。


.. method:: Element.hasAttribute(name)

   指定要素に *name* で渡した名前の属性が存在していれば true を返します。


.. method:: Element.hasAttributeNS(namespaceURI, localName)

   指定要素に *namespaceURI* と *localName* で指定した名前の属性が存在していれば true を返します。


.. method:: Element.getAttribute(name)

   *name* で指定した属性の値を文字列として返します。もし、属性が存在しない、もしくは属性に値が設定されていない場合、空の文字列が返されます。


.. method:: Element.getAttributeNode(attrname)

   *attrname* で指定された属性の :class:`Attr` ノードを返します。


.. method:: Element.getAttributeNS(namespaceURI, localName)

   *namespaceURI* と *localName* によって指定した属性の値を文字列として返します。
   もし、属性が存在しない、もしくは属性に値が設定されていない場合、空の文字列が返されます。


.. method:: Element.getAttributeNodeNS(namespaceURI, localName)

   指定した *namespaceURI* および *localName* を持つ属性値をノードとして返します。


.. method:: Element.removeAttribute(name)

   名前で指定された属性を削除します。該当する属性がなければ、
   :exc:`NotFoundErr` が送出されます。


.. method:: Element.removeAttributeNode(oldAttr)

   *oldAttr* が属性リストにある場合、削除して返します。 *oldAttr* が存在しない場合、 :exc:`NotFoundErr` が送出されます。


.. method:: Element.removeAttributeNS(namespaceURI, localName)

   名前で指定された属性を削除します。このメソッドは *qname* ではなく *localName* を使うので注意してください。該当する
   属性がなくても例外は送出されません。


.. method:: Element.setAttribute(name, value)

   文字列を使って属性値を設定します。


.. method:: Element.setAttributeNode(newAttr)

   新たな属性ノードを要素に追加します。 :attr:`name` 属性が既存の属性に一致した場合、必要に応じて属性を置き換えます。
   置換が生じると、古い属性ノードが返されます。 *newAttr* がすでに使われていれば、 :exc:`InuseAttributeErr` が送出されます。


.. method:: Element.setAttributeNodeNS(newAttr)

   新たな属性ノードを要素に追加します。 :attr:`namespaceURI` および :attr:`localName`
   属性が既存の属性に一致した場合、必要に応じて属性を置き換えます。置換が生じると、古い属性ノードが返されます。 *newAttr* がすでに使われていれば、
   :exc:`InuseAttributeErr` が送出されます。


.. method:: Element.setAttributeNS(namespaceURI, qname, value)

   指定された *namespaceURI* および *qname* で与えられた属性の値を文字列で設定します。qname は属性の完全な名前であり、この点が
   上記のメソッドと違うので注意してください。


.. _dom-attr-objects:

Attr オブジェクト
^^^^^^^^^^^^^^^^^

:class:`Attr` は :class:`Node` を継承しており、全ての属性を受け継いでいます。


.. attribute:: Attr.name

   要素型名です。
   名前空間使用の文書では、要素型名中にコロンが含まれるかもしれません。


.. attribute:: Attr.localName

   名前にコロンがあればコロン以降の部分に、なければ名前全体になります。


.. attribute:: Attr.prefix

   名前にコロンがあればコロン以前の部分に、なければ空文字列になります。


.. attribute:: Attr.value

   .. The text value of the attribute.  This is a synonym for the
      :attr:`nodeValue` attribute.

   その要素の text value.
   これは :attr:`nodeValue` 属性の別名です。

.. _dom-attributelist-objects:

NamedNodeMap Objects
^^^^^^^^^^^^^^^^^^^^

:class:`NamedNodeMap` は :class:`Node` を継承して *いません* 。


.. attribute:: NamedNodeMap.length

   属性リストの長さです。


.. method:: NamedNodeMap.item(index)

   特定のインデクスを持つ属性を返します。属性の並び方は任意ですが、 DOM 文書が生成されている間は一定になります。各要素は属性ノードです。属性値はノードの
   :attr:`value` 属性で取得してください。

このクラスをよりマップ型的な動作ができるようにする実験的なメソッドもあります。そうしたメソッドを使うこともできますし、 :class:`Element`
オブジェクトに対して、標準化された :meth:`getAttribute\*` ファミリのメソッドを使うこともできます。


.. _dom-comment-objects:

Comment オブジェクト
^^^^^^^^^^^^^^^^^^^^

:class:`Comment` は XML 文書中のコメントを表現します。 :class:`Comment` は :class:`Node`
のサブクラスですが、子ノードを持つことはありません。


.. attribute:: Comment.data

   文字列によるコメントの内容です。この属性には、コメントの先頭にある ``<!-`` \ ``-`` と末尾にある ``-`` \ ``->`` 間の全ての文字
   が入っていますが、 ``<!-`` \ ``-`` と ``-`` \ ``->`` 自体は含みません。


.. _dom-text-objects:

Text オブジェクトおよび CDATASection オブジェクト
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:class:`Text` インタフェースは XML 文書内のテキストを表現します。パーザおよび DOM 実装が DOM の XML
拡張をサポートしている場合、 CDATA でマークされた区域 (section) に入れられている部分テキストは :class:`CDATASection`
オブジェクトに記憶されます。これら二つのインタフェースは同一のものですが、 :attr:`nodeType` 属性が異なります。

これらのインタフェースは :class:`Node` インタフェースを拡張したものです。しかし子ノードを持つことはできません。


.. attribute:: Text.data

   文字列によるテキストノードの内容です。

.. note::

   :class:`CDATASection` ノードの利用は、ノードが完全な CDATA マーク区域を表現するという意味ではなく、ノードの内容が CDATA
   区域の一部であるということを意味するだけです。単一の CDATA セクションは文書ツリー内で複数のノードとして表現されることがあります。二つの隣接する
   :class:`CDATASection` ノードが、異なる CDATA マーク区域かどうかを決定する方法はありません。


.. _dom-pi-objects:

ProcessingInstruction オブジェクト
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

XML 文書内の処理命令を表現します; :class:`Node` インタフェースを継承していますが、子ノードを持つことはできません。


.. attribute:: ProcessingInstruction.target

   最初の空白文字までの処理命令の内容です。読み出し専用の属性です。


.. attribute:: ProcessingInstruction.data

   最初の空白文字以降の処理命令の内容です。


.. _dom-exceptions:

例外
^^^^

.. versionadded:: 2.1

DOM レベル 2 勧告では、単一の例外 :exc:`DOMException` と、どの種のエラーが発生したかをアプリケーションが決定できるようにする
多くの定数を定義しています。 :exc:`DOMException` インスタンスは、特定の例外に関する適切な値を提供する :attr:`code`
属性を伴っています。

Python DOM インタフェースでは、上記の定数を提供していますが、同時に一連の例外を拡張して、DOM で定義されている各例外コードに対して
特定の例外が存在するようにしています。 DOM の実装では、適切な特定の例外を送出しなければならず、各例外は :attr:`code`
属性に対応する適切な値を伴わなければなりません。


.. exception:: DOMException

   全ての特定の DOM 例外で使われている基底例外クラスです。この例外クラスは直接インスタンス化することができません。


.. exception:: DomstringSizeErr

   指定された範囲のテキストが文字列に収まらない場合に送出されます。この例外は Python の DOM 実装で使われるかどうかは判っていませんが、 Python
   で書かれていない DOM 実装から送出される場合があります。


.. exception:: HierarchyRequestErr

   挿入できない型のノードを挿入しようと試みたときに送出されます。


.. exception:: IndexSizeErr

   メソッドに与えたインデクスやサイズパラメタが負の値や許容範囲の値を超えた際に送出されます。


.. exception:: InuseAttributeErr

   文書中にすでに存在する :class:`Attr` ノードを挿入しようと試みた際に送出されます。


.. exception:: InvalidAccessErr

   パラメタまたは操作が根底にあるオブジェクトでサポートされていない場合に送出されます。


.. exception:: InvalidCharacterErr

   この例外は、文字列パラメタが、現在使われているコンテキストで XML 1.0 勧告によって許可されていない場合に送出されます。例えば、要素型に空白の入った
   :class:`Element` ノードを生成しようとすると、このエラーが送出されます。


.. exception:: InvalidModificationErr

   ノードの型を変更しようと試みた際に送出されます。


.. exception:: InvalidStateErr

   定義されていないオブジェクトや、もはや利用できなくなったオブジェクトを使おうと試みた際に送出されます。


.. exception:: NamespaceErr

   `Namespaces in XML <http://www.w3.org/TR/REC-xml-names/>`_
   に照らして許可されていない方法でオブジェクトを変更しようと試みた場合、この例外が送出されます。


.. exception:: NotFoundErr

   参照しているコンテキスト中に目的のノードが存在しない場合に送出される例外です。例えば、 :meth:`NamedNodeMap.removeNamedItem`
   は渡されたノードがノードマップ中に存在しない場合にこの例外を送出します。


.. exception:: NotSupportedErr

   要求された方のオブジェクトや操作が実装でサポートされていない場合に送出されます。


.. exception:: NoDataAllowedErr

   データ属性をサポートしないノードにデータを指定した際に送出されます。

   .. XXX  a better explanation is needed!


.. exception:: NoModificationAllowedErr

   オブジェクトに対して (読み出し専用ノードに対する修正のように) 許可されていない修正を行おうと試みた際に送出されます。


.. exception:: SyntaxErr

   無効または不正な文字列が指定された際に送出されます。

   .. XXX  how is this different from InvalidCharacterErr ???


.. exception:: WrongDocumentErr

   ノードが現在属している文書と異なる文書に挿入され、かつある文書から別の文書へのノードの移行が実装でサポートされていない場合に送出されます。

DOM 勧告で定義されている例外コードは、以下のテーブルに従って上記の例外と対応付けられます:

+--------------------------------------+---------------------------------+
| 定数                                 | 例外                            |
+======================================+=================================+
| :const:`DOMSTRING_SIZE_ERR`          | :exc:`DomstringSizeErr`         |
+--------------------------------------+---------------------------------+
| :const:`HIERARCHY_REQUEST_ERR`       | :exc:`HierarchyRequestErr`      |
+--------------------------------------+---------------------------------+
| :const:`INDEX_SIZE_ERR`              | :exc:`IndexSizeErr`             |
+--------------------------------------+---------------------------------+
| :const:`INUSE_ATTRIBUTE_ERR`         | :exc:`InuseAttributeErr`        |
+--------------------------------------+---------------------------------+
| :const:`INVALID_ACCESS_ERR`          | :exc:`InvalidAccessErr`         |
+--------------------------------------+---------------------------------+
| :const:`INVALID_CHARACTER_ERR`       | :exc:`InvalidCharacterErr`      |
+--------------------------------------+---------------------------------+
| :const:`INVALID_MODIFICATION_ERR`    | :exc:`InvalidModificationErr`   |
+--------------------------------------+---------------------------------+
| :const:`INVALID_STATE_ERR`           | :exc:`InvalidStateErr`          |
+--------------------------------------+---------------------------------+
| :const:`NAMESPACE_ERR`               | :exc:`NamespaceErr`             |
+--------------------------------------+---------------------------------+
| :const:`NOT_FOUND_ERR`               | :exc:`NotFoundErr`              |
+--------------------------------------+---------------------------------+
| :const:`NOT_SUPPORTED_ERR`           | :exc:`NotSupportedErr`          |
+--------------------------------------+---------------------------------+
| :const:`NO_DATA_ALLOWED_ERR`         | :exc:`NoDataAllowedErr`         |
+--------------------------------------+---------------------------------+
| :const:`NO_MODIFICATION_ALLOWED_ERR` | :exc:`NoModificationAllowedErr` |
+--------------------------------------+---------------------------------+
| :const:`SYNTAX_ERR`                  | :exc:`SyntaxErr`                |
+--------------------------------------+---------------------------------+
| :const:`WRONG_DOCUMENT_ERR`          | :exc:`WrongDocumentErr`         |
+--------------------------------------+---------------------------------+


.. _dom-conformance:

適合性
------

この節では適合性に関する要求と、Python DOM API、W3C DOM 勧告、および OMG IDL の Python API
への対応付けとの間の関係について述べます。


.. _dom-type-mapping:

型の対応付け
^^^^^^^^^^^^

DOM 仕様で使われている基本的な IDL 型は、以下のテーブルに従って Python の型に対応付けられています。

+------------------+------------------------------------------------+
| IDL 型           | Python 型                                      |
+==================+================================================+
| ``boolean``      | ``IntegerType`` (値 ``0`` または ``1``) による |
+------------------+------------------------------------------------+
| ``int``          | ``IntegerType``                                |
+------------------+------------------------------------------------+
| ``long int``     | ``IntegerType``                                |
+------------------+------------------------------------------------+
| ``unsigned int`` | ``IntegerType``                                |
+------------------+------------------------------------------------+

さらに、勧告で定義されている :class:`DOMString` は、Python 文字列または Unicode
文字列に対応付けられます。アプリケーションでは、 DOM から文字列が返される際には常に Unicode を扱えなければなりません。

IDL の `null` 値は ``None`` に対応付けられており、 API で `null`
の使用が許されている場所では常に受理されるか、あるいは実装によって提供されるはずです。


.. _dom-accessor-methods:

アクセサメソッド
^^^^^^^^^^^^^^^^

OMG IDL から Python への対応付けは、 IDL `attribute` 宣言へのアクセサ関数の定義を、Java
による対応付けが行うのとほとんど同じように行います。

IDL 宣言の対応付け ::

   readonly attribute string someValue;
            attribute string anotherValue;

は、三つのアクセサ関数: :attr:`someValue` に対する "get" メソッド
(:meth:`_get_someValue`)、そして :attr:`anotherValue` に対する "get" および "set" メソッド
(:meth:`_get_anotherValue` および :meth:`_set_anotherValue`) を生み出します。
とりわけ、対応付けでは、IDL 属性が通常の Python 属性としてアクセス可能であることは必須ではありません: ``object.someValue``
が動作することは必須 *ではなく* 、 :exc:`AttributeError` を送出してもかまいません。

しかしながら、Python DOM API では、通常の属性アクセスが動作することが必須です。これは、Python IDL
コンパイラによって生成された典型的なサロゲーションはまず動作することはなく、DOM オブジェクトが CORBA
を解してアクセスされる場合には、クライアント上でラッパオブジェクトが必要であることを意味します。 CORBA DOM
クライアントでは他にもいくつか考慮すべきことがある一方で、 CORBA を介して DOM を使った経験を持つ実装者はこのことを問題視して
いません。 `readonly` であると宣言された属性は、全ての DOM 実装で書き込みアクセスを制限しているとは限りません。

Python DOM API では、アクセサ関数は必須ではありません。アクセサ関数が提供された場合、 Python IDL
対応付けによって定義された形式をとらなければなりませんが、属性は Python から直接アクセスすることができるので、それらのメソッドは
必須ではないと考えられます。 `readonly` であると宣言された属性に対しては、 "set" アクセサを提供してはなりません。

このIDLでの定義はW3C DOM APIの全ての要件を実装しているわけではありません。例えば、一部のオブジェクトの概念や
:meth:`getElementsByTagName` が"live"であることなどです。 Python DOM API
はこれらの要件を実装することを強制しません。

