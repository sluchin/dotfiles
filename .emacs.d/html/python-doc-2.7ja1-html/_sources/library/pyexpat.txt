
:mod:`xml.parsers.expat` --- Expat を使った高速な XML 解析
==========================================================

.. module:: xml.parsers.expat
   :synopsis: Expat による、検証を行わない XML パーザへのインタフェース
.. moduleauthor:: Paul Prescod <paul@prescod.net>


.. Markup notes:

   XMLParser オブジェクトの属性の多くはコールバックです。
   シグニチャ情報を書かないといけないので、method ディレクティヴを使ってます。
   これらはクライアントコードでセットされる属性なので、
   文中の参照は :member: ロールを使って書きます。

.. versionadded:: 2.0

.. index:: single: Expat

:mod:`xml.parsers.expat` モジュールは、検証 (validation) を行わない XML パーザ (parser,
解析器)、Expat への Python インタフェースです。モジュールは一つの拡張型 :class:`xmlparser` を提供します。これは
XMLパーザの現在の状況を表します。一旦 :class:`xmlparser` オブジェクトを生成すると、オブジェクトの様々な属性をハンドラ関数
(handler function) に設定できます。その後、XML 文書をパーザに入力すると、 XML文書の文字列とマークアップ
に応じてハンドラ関数が呼び出されます。

.. index:: module: pyexpat

このモジュールでは、Expatパーザへのアクセスを提供するために :mod:`pyexpat` モジュールを使用します。
:mod:`pyexpat` モジュールの直接使用は撤廃されています。

このモジュールは、例外を一つと型オブジェクトを一つ提供しています。


.. exception:: ExpatError

   Expat がエラーを報告したときに例外を送出します。
   Expatのエラーを解釈する上での詳細な情報は、 :ref:`expaterror-objects`
   を参照してください。


.. exception:: error

   :exc:`ExpatError` への別名です。


.. data:: XMLParserType

   :func:`ParserCreate` 関数から返された戻り値の型を示します。

:mod:`xml.parsers.expat` モジュールには以下の 2 つの関数が収められています:


.. function:: ErrorString(errno)

   与えられたエラー番号 *errno* を解説する文字列を返します。


.. function:: ParserCreate([encoding[, namespace_separator]])

   新しい :class:`xmlparser` オブジェクトを作成し、返します。
   *encoding* が指定されていた場合、XMLデータで使われている文字列のエンコード名でなければなりません。
   Expatは、Pythonのように多くのエンコードをサポートしておらず、またエンコーディングのレパートリを拡張することはできません;
   サポートするエンコードは、UTF-8, UTF-16, ISO-8859-1 (Latin1), ASCII です。
   *encoding* [1]_ が指定されると、文書に対する明示的、非明示的なエンコード指定を上書き (override) します。

   Expat はオプションで XML 名前空間の処理を行うことができます。
   これは引数 *namespace_separator* に値を指定することで有効になります。
   この値は、1文字の文字列でなければなりません;
   文字列が誤った長さを持つ場合には :exc:`ValueError` が送出されます
   (``None`` は値の省略と見なされます)。
   名前空間の処理が可能なとき、名前空間に属する要素と属性が展開されます。
   要素のハンドラである :attr:`StartElementHandler` と
   :attr:`EndElementHandler` に渡された要素名は、名前空間の
   URI、名前空間の区切り文字、要素名のローカル部を連結したものになります。
   名前空間の区切り文字が 0 バイト  (``chr(0)``)
   の場合、名前空間の URI とローカル部は区切り文字なしで連結されます。

   たとえば、 *namespace_separator* に空白文字(``' '``)がセットされ、次のような文書が解析されるとします。 ::

      <?xml version="1.0"?>
      <root xmlns    = "http://default-namespace.org/"
            xmlns:py = "http://www.python.org/ns/">
        <py:elem1 />
        <elem2 xmlns="" />
      </root>

   :attr:`StartElementHandler` は各要素ごとに次のような文字列を受け取ります。 ::

      http://default-namespace.org/ root
      http://www.python.org/ns/ elem1
      elem2


.. seealso::

   `The Expat XML Parser <http://www.libexpat.org/>`_
      Expatプロジェクトのホームページ


.. _xmlparser-objects:

XMLParser Objects
-----------------

:class:`xmlparser` オブジェクトは以下のようなメソッドを持ちます。


.. method:: xmlparser.Parse(data[, isfinal])

   文字列 *data* の内容を解析し、解析されたデータを処理するための適切な関数を呼び出します。このメソッドを最後に呼び出す時は *isfinal*
   を真にしなければなりません。 *data* は空の文字列を取ることもできます。


.. method:: xmlparser.ParseFile(file)

   *file* オブジェクトから読み込んだXMLデータを解析します。 *file* には `read(nbytes)` メソッドのみが必要です。
   このメソッドはデータがなくなった場合に空文字列を返さねばなりません。。


.. method:: xmlparser.SetBase(base)

   (XML) 宣言中のシステム識別子中の相対 URI を解決するための、基底 URI を設定します。相対識別子の解決はアプリケーションに任されます:
   この値は関数 :func:`ExternalEntityRefHandler` や
   :func:`NotationDeclHandler`, :func:`UnparsedEntityDeclHandler` に引数 *base*
   としてそのまま渡されます。


.. method:: xmlparser.GetBase()

   以前の :meth:`SetBase` によって設定された基底 URI を文字列の形で返します。 :meth:`SetBase` が呼ばれていないときには
   ``None`` を返します。


.. method:: xmlparser.GetInputContext()

   現在のイベントを発生させた入力データを文字列として返します。データはテキストの入っているエンティティが持っているエンコードになります。
   イベントハンドラがアクティブでないときに呼ばれると、戻り値は ``None`` となります。

   .. versionadded:: 2.1


.. method:: xmlparser.ExternalEntityParserCreate(context[, encoding])

   親となるパーザで解析された内容が参照している、外部で解析されるエンティティを解析するために使える "子の" パーザを作成します。 *context*
   パラメータは、以下に記すように :meth:`ExternalEntityRefHandler`
   ハンドラ関数に渡される文字列でなければなりません。子のパーザは
   :attr:`ordered_attributes`, :attr:`returns_unicode`, :attr:`specified_attributes`
   が現在のパーザの値に設定されて生成されます。

.. method:: xmlparser.SetParamEntityParsing(flag)

   パラメーターエンティティ (外部DTDサブセットを含む) の解析を制御します。
   *flag* の有効な値は、 :const:`XML_PARAM_ENTITY_PARSING_NEVER`,
   :const:`XML_PARAM_ENTITY_PARSING_UNLESS_STANDALONE`,
   :const:`XML_PARAM_ENTITY_PARSING_ALWAYS` です。
   flag の設定をしたら true を返します。

.. method:: xmlparser.UseForeignDTD([flag])

   *flag* の値をデフォルトのtrueにすると、Expatは代わりのDTDをロードするため、すべての引数に :const:`None` を設定して
   :attr:`ExternalEntityRefHandler` を呼び出します。XML文書が文書型定義を持っていなければ、
   :attr:`ExternalEntityRefHandler` が呼び出しますが、 :attr:`StartDoctypeDeclHandler` と
   :attr:`EndDoctypeDeclHandler` は呼び出されません。

   *flag* にfalseを与えると、メソッドが前回呼ばれた時のtrueの設定が解除されますが、他には何も起こりません。

   このメソッドは :meth:`Parse` または :meth:`ParseFile` メソッドが呼び出される前にだけ呼び出されます;これら2つのメソッドの
   どちらかが呼び出されたあとにメソッドが呼ばれると、 :attr:`code` に定数
   :const:`errors.XML_ERROR_CANT_CHANGE_FEATURE_ONCE_PARSING` が設定されて例外
   :exc:`ExpatError` が送出されます。

   .. versionadded:: 2.3

:class:`xmlparser` オブジェクトは次のような属性を持ちます:


.. attribute:: xmlparser.buffer_size

   :attr:`buffer_text` が真の時に使われるバッファのサイズです。
   この属性に新しい整数値を代入することで違うバッファサイズにできます。
   サイズが変えられるときにバッファはフラッシュされます。

   .. versionadded:: 2.3

   .. versionchanged:: 2.6
      バッファサイズが変えられるようになりました。

.. attribute:: xmlparser.buffer_text

   この値を真にすると、 :class:`xmlparser` オブジェクトが Expatから返された
   もとの内容をバッファに保持するようになります。これにより可能なときに何度も :meth:`CharacterDataHandler`
   を呼び出してしまうようなことを避けることができます。Expatは通常、文字列のデータを行末ごと大量に破棄
   するため、かなりパフォーマンスを改善できるはずです。この属性はデフォルトでは偽で、いつでも変更可能です。

   .. versionadded:: 2.3


.. attribute:: xmlparser.buffer_used

   :attr:`buffer_text` が利用可能なとき、バッファに保持されたバイト数です。
   これらのバイトはUTF-8でエンコードされたテキストを表します。この属性は :attr:`buffer_text` が偽の時には意味がありません。

   .. versionadded:: 2.3


.. attribute:: xmlparser.ordered_attributes

   この属性をゼロ以外の整数にすると、報告される(XMLノードの) 属性を辞書型ではなくリスト型にします。属性は文書のテキスト中の出現順で示されます。
   それぞれの属性は、2つのリストのエントリ: 属性名とその値、が与えられます。 (このモジュールの古いバージョンでも、同じフォーマットが使われています。)
   デフォルトでは、この属性はデフォルトでは偽となりますが、いつでも変更可能です。

   .. versionadded:: 2.1


.. attribute:: xmlparser.returns_unicode

   この属性をゼロ以外の整数にすると、ハンドラ関数に Unicode 文字列が渡されます。 :attr:`returns_unicode` が
   :const:`False` の時には、UTF-8でエンコードされたデータを含む 8 ビット文字列がハンドラに渡されます。Pythonがユニコード
   サポートつきでビルドされている場合、この値はデフォルトで :const:`True` です。

   .. versionchanged:: 1.6
      戻り値の型がいつでも変更できるように変更されたはずです.


.. attribute:: xmlparser.specified_attributes

   ゼロ以外の整数にすると、パーザは文書のインスタンスで特定される属性だけを報告し、属性宣言から導出された属性は報告しないようになります。
   この属性が指定されたアプリケーションでは、XMLプロセッサの振る舞いに関する標準に従うために必要とされる (文書型) 宣言によって、どのような
   付加情報が利用できるのかということについて特に注意を払わなければなりません。デフォルトで、この属性は偽となりますが、いつでも変更可能です。

   .. versionadded:: 2.1

以下の属性には、 :class:`xmlparser` オブジェクトで最も最近に起きたエラーに関する値が入っており、また :meth:`Parse` または
:meth:`ParseFile` メソッドが :exc:`xml.parsers.expat.ExpatError`
例外を送出した際にのみ正しい値となります。


.. attribute:: xmlparser.ErrorByteIndex

   エラーが発生したバイトのインデクスです。


.. attribute:: xmlparser.ErrorCode

   エラーを特定する数値によるコードです。この値は :func:`ErrorString` に渡したり、 ``errors``
   オブジェクトで定義された内容と比較できます。


.. attribute:: xmlparser.ErrorColumnNumber

   エラーの発生したカラム番号です。


.. attribute:: xmlparser.ErrorLineNumber

   エラーの発生した行番号です。

以下の属性は :class:`xmlparser` オブジェクトがその時パースしている位置に
関する値を保持しています。コールバックがパースイベントを報告している間、これらの値はイベントの生成した文字列の先頭の位置を指し示します。
コールバックの外から参照された時には、（対応するコールバックであるかにかかわらず）直前のパースイベントの位置を示します。

.. versionadded:: 2.4


.. attribute:: xmlparser.CurrentByteIndex

   パーサへの入力の、現在のバイトインデックス。


.. attribute:: xmlparser.CurrentColumnNumber

   パーサへの入力の、現在のカラム番号。


.. attribute:: xmlparser.CurrentLineNumber

   パーサへの入力の、現在の行番号。

以下に指定可能なハンドラのリストを示します。 :class:`xmlparser` オブジェクト *o*
にハンドラを指定するには、 ``o.handlername = func`` を使用します。
*handlername* は、以下のリストに挙げた値をとらねばならず、
また *func* は正しい数の引数を受理する呼び出し可能なオブジェクトでなければなりません。
引数は特に明記しない限り、すべて文字列となります。


.. method:: xmlparser.XmlDeclHandler(version, encoding, standalone)

   XML 宣言が解析された時に呼ばれます。XML宣言とは、XML勧告の適用バージョン (オプション)、文書テキストのエンコード、そしてオプションの
   "スタンドアロン" の宣言です。 *version* と *encoding* は :attr:`returns_unicode`
   属性によって指示された型を示す文字列となり、 *standalone* は、文書がスタンドアロンであると宣言される場合には
   ``1`` に、文書がスタンドアロンでない場合には ``0`` に、スタンドアロン宣言を省略する場合には ``-1`` になります。このハンドラは Expat
   のバージョン1.95.0以降のみ使用できます。

   .. versionadded:: 2.1


.. method:: xmlparser.StartDoctypeDeclHandler(doctypeName, systemId, publicId, has_internal_subset)

   Expatが文書型宣言 ``<!DOCTYPE ...``)を解析し始めたときに呼び出されます。 *doctypeName* は、与えられた値がそのまま
   Expat に提供されます。 *systemId* と *publicId* パラメタが指定されている場合、それぞれシステムと公開識別子を与えます。
   省略する時には ``None`` にします。文書が内部的な文書宣言のサブセット (internal document declaration subset)
   を持つか、サブセット自体の場合、 *has_internal_subset* は true になります。このハンドラには、Expat version
   1.2以上が必要です。


.. method:: xmlparser.EndDoctypeDeclHandler()

   Expatが文書型宣言の解析を終えたときに呼び出されます。このハンドラには、Expat version 1.2以上が必要です。


.. method:: xmlparser.ElementDeclHandler(name, model)

   それぞれの要素型宣言ごとに呼び出されます。 *name* は要素型の名前であり、 *model* は内容モデル (content model) の表現です。

.. % -------------


.. method:: xmlparser.AttlistDeclHandler(elname, attname, type, default, required)

   ひとつの要素型で宣言される属性ごとに呼び出されます。属性リストの宣言が 3つの属性を宣言したとすると、このハンドラはひとつの属性に1度づつ、
   3度呼び出されます。 *elname* は要素名であり、これに対して宣言が適用され、 *attname* が宣言された属性名となります。
   属性型は文字列で、 *type* として渡されます; 取りえる値は、 ``'CDATA'``, ``'ID'``, ``'IDREF'``, ... です。
   *default* は、属性が文書のインスタンスによって指定されていないときに使用されるデフォルト値を与えます。デフォルト値(``#IMPLIED``
   values)が存在しないときには ``None`` を与えます。文書のインスタンスによって属性値が
   与えられる必要のあるときには *required* がtrueになります。このメソッドはExpat version 1.95.0 以上が必要です。


.. method:: xmlparser.StartElementHandler(name, attributes)

   要素の開始を処理するごとに呼び出されます。 *name* は要素名を格納した文字列で、 *attributes* はその値に属性名を対応付ける辞書型です。


.. method:: xmlparser.EndElementHandler(name)

   要素の終端を処理するごとに呼び出されます。


.. method:: xmlparser.ProcessingInstructionHandler(target, data)

   Called for every processing instruction. 処理命令を処理するごとに呼び出されます。


.. method:: xmlparser.CharacterDataHandler(data)

   文字データを処理するときに呼びだされます。このハンドラは通常の文字データ、 CDATAセクション、無視できる空白文字列のために呼び出されます。
   これらを識別しなければならないアプリケーションは、要求された情報を収集するために :attr:`StartCdataSectionHandler`,
   :attr:`EndCdataSectionHandler`, and :attr:`ElementDeclHandler` コールバックメソッドを使用できます。


.. method:: xmlparser.UnparsedEntityDeclHandler(entityName, base, systemId, publicId, notationName)

   解析されていない (NDATA) エンティティ宣言を処理するために呼び出されます。このハンドラは Expat
   ライブラリのバージョン1.2のためだけに存在します; より最近のバージョンでは、代わりに :attr:`EntityDeclHandler` を使用してください
   (根底にある Expat ライブラリ内の関数は、撤廃されたものであると宣言されています)。


.. method:: xmlparser.EntityDeclHandler(entityName, is_parameter_entity, value, base, systemId, publicId, notationName)

   エンティティ宣言ごとに呼び出されます。パラメタと内部エンティティについて、 *value* はエンティティ宣言の宣言済みの内容を与える文字列となります;
   外部エンティティの時には ``None`` となります。解析済みエンティティの場合、 *notationName* パラメタは ``None`` となり、
   解析されていないエンティティの時には記法 (notation) 名となります。 *is_parameter_entity*
   は、エンティティがパラメタエンティティの場合真に、一般エンティティ (general entitiy) の場合には偽になります
   (ほとんどのアプリケーションでは、一般エンティティのことしか気にする必要がありません)。このハンドラは Expat ライブラリのバージョン1.95.0
   以降でのみ使用できます。

   .. versionadded:: 2.1


.. method:: xmlparser.NotationDeclHandler(notationName, base, systemId, publicId)

   記法の宣言 (notation declaration) で呼び出されます。 *notationName*, *base*, *systemId*, および
   *publicId* を与える場合、文字列にします。public な識別子が省略された場合、 *publicId* は ``None`` になります。


.. method:: xmlparser.StartNamespaceDeclHandler(prefix, uri)

   要素が名前空間宣言を含んでいる場合に呼び出されます。名前空間宣言は、宣言が配置されている要素に対して :attr:`StartElementHandler`
   が呼び出される前に処理されます。


.. method:: xmlparser.EndNamespaceDeclHandler(prefix)

   名前空間宣言を含んでいたエレメントの終了タグに到達したときに呼び出されます。このハンドラは、要素に関する名前空間宣言ごとに、
   :attr:`StartNamespaceDeclHandler` とは逆の順番で一度だけ呼び出され、各名前空間宣言のスコープが開始されたことを示します。
   このハンドラは、要素が終了する際、対応する :attr:`EndElementHandler` が呼ばれた後に呼び出されます。


.. method:: xmlparser.CommentHandler(data)

   コメントで呼び出されます。 *data* はコメントのテキストで、先頭の '``<!-`` \ ``-``' と末尾の '``-`` \ ``->``'
   を除きます。


.. method:: xmlparser.StartCdataSectionHandler()

   CDATA セクションの開始時に呼び出されます。CDATA セクションの構文的な開始と終了位置を識別できるようにするには、このハンドラと
   :attr:`EndCdataSectionHandler` が必要です。


.. method:: xmlparser.EndCdataSectionHandler()

   CDATA セクションの終了時に呼び出されます。


.. method:: xmlparser.DefaultHandler(data)

   XML 文書中で、適用可能なハンドラが指定されていない文字すべてに対して呼び出されます。この文字とは、検出されたことが
   報告されるが、ハンドラは指定されていないようなコンストラクト (construct) の一部である文字を意味します。


.. method:: xmlparser.DefaultHandlerExpand(data)

   :func:`DefaultHandler` と同じですが、内部エンティティの展開を禁止しません。エンティティ参照はデフォルトハンドラに渡されません。


.. method:: xmlparser.NotStandaloneHandler()

   XML 文書がスタンドアロンの文書として宣言されていない場合に呼び出されます。
   外部サブセットやパラメタエンティティへの参照が存在するが、XML 宣言が XML
   宣言中で standalone 変数を ``yes`` に設定していない場合に起きます。
   このハンドラが ``0`` を返すと、パーザは :const:`XML_ERROR_NOT_STANDALONE`
   を発生させます。このハンドラが設定されていなければ、パーザは前述の事態で
   例外を送出しません。


.. method:: xmlparser.ExternalEntityRefHandler(context, base, systemId, publicId)

   外部エンティティの参照時に呼び出されます。 *base* は現在の基底 (base) で、以前の :meth:`SetBase` で設定された値になっています。
   public、および system の識別子である、 *systemId* と *publicId* が指定されている場合、値は文字列です; public
   識別子が指定されていない場合、 *publicId* は ``None`` になります。 *context*
   の値は不明瞭なものであり、以下に記述するようにしか使ってはなりません。

   外部エンティティが解析されるようにするには、このハンドラを実装しなければなりません。このハンドラは、
   ``ExternalEntityParserCreate(context)`` を使って適切なコールバックを指定し、子パーザを生成して、
   エンティティを解析する役割を担います。このハンドラは整数を返さねばなりません; ``0`` を返した場合、パーザは
   :const:`XML_ERROR_EXTERNAL_ENTITY_HANDLING` エラーを送出します。そうでないばあい、解析を継続します。

   このハンドラが与えられておらず、 :attr:`DefaultHandler` コールバックが指定されていれば、
   外部エンティティは :attr:`DefaultHandler` で報告されます。


.. _expaterror-objects:

ExpatError 例外
---------------

.. sectionauthor:: Fred L. Drake, Jr. <fdrake@acm.org>


:exc:`ExpatError` 例外はいくつかの興味深い属性を備えています:


.. attribute:: ExpatError.code

   特定のエラーにおける Expat の内部エラー番号です。この値はこのモジュールの ``errors`` オブジェクトで定義されている
   定数のいずれかに一致します。

   .. versionadded:: 2.1


.. attribute:: ExpatError.lineno

   エラーが検出された場所の行番号です。最初の行の番号は ``1`` です。

   .. versionadded:: 2.1


.. attribute:: ExpatError.offset

   エラーが発生した場所の行内でのオフセットです。最初のカラムの番号は ``0`` です。

   .. versionadded:: 2.1


.. _expat-example:

例
--

以下のプログラムでは、与えられた引数を出力するだけの三つのハンドラを定義しています。 ::

   import xml.parsers.expat

   # 3 handler functions
   def start_element(name, attrs):
       print 'Start element:', name, attrs
   def end_element(name):
       print 'End element:', name
   def char_data(data):
       print 'Character data:', repr(data)

   p = xml.parsers.expat.ParserCreate()

   p.StartElementHandler = start_element
   p.EndElementHandler = end_element
   p.CharacterDataHandler = char_data

   p.Parse("""<?xml version="1.0"?>
   <parent id="top"><child1 name="paul">Text goes here</child1>
   <child2 name="fred">More text</child2>
   </parent>""")

このプログラムの出力は以下のようになります::

   Start element: parent {'id': 'top'}
   Start element: child1 {'name': 'paul'}
   Character data: 'Text goes here'
   End element: child1
   Character data: '\n'
   Start element: child2 {'name': 'fred'}
   Character data: 'More text'
   End element: child2
   Character data: '\n'
   End element: parent


.. _expat-content-models:

内容モデルの記述
----------------

.. sectionauthor:: Fred L. Drake, Jr. <fdrake@acm.org>


内容モデルは入れ子になったタプルを使って記述されています。各タプルには 4 つの値: 型、限定詞 (quantifier)、名前、そして子の
タプル、が収められています。子のタプルは単に内容モデルを記述したものです。

最初の二つのフィールドの値は :mod:`xml.parsers.expat` モジュールの ``model``
オブジェクトで定義されている定数です。これらの定数は二つのグループ: モデル型 (model type) グループと限定子 (quantifier)
グループ、に取りまとめられます。

以下にモデル型グループにおける定数を示します:


.. data:: XML_CTYPE_ANY
   :noindex:

   モデル名で指定された要素は ``ANY`` の内容モデルを持つと宣言されます。


.. data:: XML_CTYPE_CHOICE
   :noindex:

   指定されたエレメントはいくつかのオプションから選択できるようになっています; ``(A | B | C)`` のような内容モデルで用いられます。


.. data:: XML_CTYPE_EMPTY
   :noindex:

   ``EMPTY`` であると宣言されている要素はこのモデル型を持ちます。


.. data:: XML_CTYPE_MIXED
   :noindex:


.. data:: XML_CTYPE_NAME
   :noindex:


.. data:: XML_CTYPE_SEQ
   :noindex:

   順々に続くようなモデルの系列を表すモデルがこのモデル型で表されます。 ``(A, B, C)`` のようなモデルで用いられます。

限定子グループにおける定数を以下に示します:


.. data:: XML_CQUANT_NONE
   :noindex:

   修飾子 (modifier) が指定されていません。従って ``A`` のように、厳密に一つだけです。


.. data:: XML_CQUANT_OPT
   :noindex:

   このモデルはオプションです: ``A?`` のように、一つか全くないかです。


.. data:: XML_CQUANT_PLUS
   :noindex:

   このモデルは (``A+`` のように) 一つかそれ以上あります。


.. data:: XML_CQUANT_REP
   :noindex:

   このモデルは ``A*`` のようにゼロ回以上あります。


.. _expat-errors:

Expat エラー定数
----------------

以下の定数は :mod:`xml.parsers.expat` モジュールにおける ``errors`` オブジェクトで提供されています。これらの定数は、
エラーが発生した際に送出される :exc:`ExpatError` 例外オブジェクトのいくつかの属性を解釈する上で便利です。

``errors`` オブジェクトは以下の属性を持ちます:


.. data:: XML_ERROR_ASYNC_ENTITY
   :noindex:


.. data:: XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF
   :noindex:

   属性値中のエンティティ参照が、内部エンティティではなく外部エンティティを参照しました。


.. data:: XML_ERROR_BAD_CHAR_REF
   :noindex:

   文字参照が、XML では正しくない (illegal) 文字を参照しました (例えば ``0`` や '``&#0;``')。


.. data:: XML_ERROR_BINARY_ENTITY_REF
   :noindex:

   エンティティ参照が、記法 (notation) つきで宣言されているエンティティを参照したため、解析できません。


.. data:: XML_ERROR_DUPLICATE_ATTRIBUTE
   :noindex:

   一つの属性が一つの開始タグ内に一度より多く使われています。


.. data:: XML_ERROR_INCORRECT_ENCODING
   :noindex:


.. data:: XML_ERROR_INVALID_TOKEN
   :noindex:

   入力されたバイトが文字に適切に関連付けできない際に送出されます; 例えば、UTF-8 入力ストリームにおける NUL バイト (値 ``0``) などです。


.. data:: XML_ERROR_JUNK_AFTER_DOC_ELEMENT
   :noindex:

   空白以外の何かがドキュメント要素の後にあります。


.. data:: XML_ERROR_MISPLACED_XML_PI
   :noindex:

   入力データの先頭以外の場所に XML 定義が見つかりました。


.. data:: XML_ERROR_NO_ELEMENTS
   :noindex:

   このドキュメントには要素が入っていません (XML では全てのドキュメントは確実にトップレベルの要素を一つ持つよう要求しています)。


.. data:: XML_ERROR_NO_MEMORY
   :noindex:

   Expat が内部メモリを確保できませんでした。


.. data:: XML_ERROR_PARAM_ENTITY_REF
   :noindex:

   パラメタエンティティが許可されていない場所で見つかりました。


.. data:: XML_ERROR_PARTIAL_CHAR
   :noindex:

   入力に不完全な文字が見つかりました。


.. data:: XML_ERROR_RECURSIVE_ENTITY_REF
   :noindex:

   エンティティ参照中に、同じエンティティへの別の参照が入っていました; おそらく違う名前で参照しているか、間接的に参照しています。


.. data:: XML_ERROR_SYNTAX
   :noindex:

   何らかの仕様化されていない構文エラーに遭遇しました。


.. data:: XML_ERROR_TAG_MISMATCH
   :noindex:

   終了タグが最も内側で開かれている開始タグに一致しません。


.. data:: XML_ERROR_UNCLOSED_TOKEN
   :noindex:

   何らかの (開始タグのような) トークンが閉じられないまま、ストリームの終端や次のトークンに遭遇しました。


.. data:: XML_ERROR_UNDEFINED_ENTITY
   :noindex:

   定義されていないエンティティへの参照が行われました。


.. data:: XML_ERROR_UNKNOWN_ENCODING
   :noindex:

   ドキュメントのエンコードが Expat でサポートされていません。


.. data:: XML_ERROR_UNCLOSED_CDATA_SECTION
   :noindex:

   CDATAセクションが閉じられていません。


.. data:: XML_ERROR_EXTERNAL_ENTITY_HANDLING
   :noindex:


.. data:: XML_ERROR_NOT_STANDALONE
   :noindex:

   XML文書が"standalone"だと宣言されており :attr:`NotStandaloneHandler` が設定され ``0`` が
   返されているにもかかわらず、パーサは"standalone"ではないと判別しました。


.. data:: XML_ERROR_UNEXPECTED_STATE
   :noindex:


.. data:: XML_ERROR_ENTITY_DECLARED_IN_PE
   :noindex:


.. data:: XML_ERROR_FEATURE_REQUIRES_XML_DTD
   :noindex:

   その操作を完了するにはDTDのサポートが必要ですが、ExpatがDTDのサポートをしない設定になっています。これは
   :mod:`xml.parsers.expat` モジュールの標準的なビルドでは報告されません。


.. data:: XML_ERROR_CANT_CHANGE_FEATURE_ONCE_PARSING
   :noindex:

   パースが始まったあとで動作の変更が要求されました。これはパースが開始される前にのみ変更可能です。（現在のところ） :meth:`UseForeignDTD`
   によってのみ送出されます。


.. data:: XML_ERROR_UNBOUND_PREFIX
   :noindex:

   名前空間の処理を有効すると宣言されていないプレフィックスが見つかります。


.. data:: XML_ERROR_UNDECLARING_PREFIX
   :noindex:

   XML文書はプレフィックスに対応した名前空間宣言を削除しようとしました。


.. data:: XML_ERROR_INCOMPLETE_PE
   :noindex:

   パラメータエンティティは不完全なマークアップを含んでいます。


.. data:: XML_ERROR_XML_DECL
   :noindex:

   XML文書中に要素がありません。


.. data:: XML_ERROR_TEXT_DECL
   :noindex:

   外部エンティティ中のテキスト宣言にエラーがあります。


.. data:: XML_ERROR_PUBLICID
   :noindex:

   パブリックID中に許可されていない文字があります。


.. data:: XML_ERROR_SUSPENDED
   :noindex:

   要求された操作は一時停止されたパーサで行われていますが、許可されていない操作です。このエラーは追加の入力を
   行なおうとしている場合、もしくはパーサが停止しようとしている場合にも送出されます。


.. data:: XML_ERROR_NOT_SUSPENDED
   :noindex:

   パーサを一時停止しようとしましたが、停止されませんでした。


.. data:: XML_ERROR_ABORTED
   :noindex:

   Pythonアプリケーションには通知されません。


.. data:: XML_ERROR_FINISHED
   :noindex:

   要求された操作で、パース対象となる入力が完了したと判断しましたが、入力は受理されませんでした。このエラーは
   追加の入力を行なおうとしている場合、もしくはパーサが停止しようとしている場合に送出されます。


.. data:: XML_ERROR_SUSPEND_PE
   :noindex:


.. rubric:: 注記

.. [#] XML の出力に含まれるエンコーディング文字列は適切な標準に\
   適合していなければなりません。
   たとえば、"UTF-8" は正当ですが、"UTF8" は違います。
   http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-EncodingDecl
   と
   http://www.iana.org/assignments/character-sets
   を参照して下さい。
