
:mod:`xml.dom.minidom` --- 軽量な DOM 実装
==========================================

.. module:: xml.dom.minidom
   :synopsis: 軽量な文書オブジェクトモデルの実装。
.. moduleauthor:: Paul Prescod <paul@prescod.net>
.. sectionauthor:: Paul Prescod <paul@prescod.net>
.. sectionauthor:: Martin v. Löwis <loewis@informatik.hu-berlin.de>


.. versionadded:: 2.0

:mod:`xml.dom.minidom` は、軽量な文書オブジェクトモデルインタフェースの実装です。この実装では、完全な DOM よりも
単純で、かつ十分に小さくなるよう意図しています。

DOM アプリケーションは典型的に、XML を DOM に解析 (parse) することで開始します。 :mod:`xml.dom.minidom`
では、以下のような解析用の関数を介して行います::

   from xml.dom.minidom import parse, parseString

   dom1 = parse('c:\\temp\\mydata.xml') # parse an XML file by name

   datasource = open('c:\\temp\\mydata.xml')
   dom2 = parse(datasource)   # parse an open file

   dom3 = parseString('<myxml>Some data<empty/> some more data</myxml>')

:func:`parse` 関数はファイル名か、開かれたファイルオブジェクトを引数にとることができます。


.. function:: parse(filename_or_file[, parser[, bufsize]])

   与えられた入力から :class:`Document` を返します。 *filename_or_file*
   はファイル名でもファイルオブジェクトでもかまいません。 *parser* を指定する場合、SAX2 パーザオブジェクトでなければなりません。
   この関数はパーザの文書ハンドラを変更し、名前空間サポートを有効にします; (エンティティリゾルバ (entity resolver) のような)
   他のパーザ設定は前もっておこなわなければなりません。

XML データを文字列で持っている場合、 :func:`parseString` を代わりに使うことができます:


.. function:: parseString(string[, parser])

   *string* を表現する :class:`Document` を返します。このメソッドは文字列に対する :class:`StringIO`
   オブジェクトを生成して、そのオブジェクトを :func:`parse` に渡します。

これらの関数は両方とも、文書の内容を表現する :class:`Document` オブジェクトを返します。

:func:`parse` や :func:`parseString` といった関数が行うのは、 XML パーザを、何らかの SAX パーザからくる解析イベント
(parse event)  を受け取って DOM ツリーに変換できるような "DOM ビルダ (DOM builder)"
に結合することです。関数は誤解を招くような名前になっているかもしれませんが、インタフェースについて学んでいるときには理解しやすい
でしょう。文書の解析はこれらの関数が戻るより前に完結します; 要するに、これらの関数自体はパーザ実装を提供しないということです。

"DOM 実装" オブジェクトのメソッドを呼び出して :class:`Document` を
生成することもできます。このオブジェクトは、 :mod:`xml.dom`  パッケージ、または :mod:`xml.dom.minidom` モジュールの
:func:`getDOMImplementation` 関数を呼び出して取得できます。 :mod:`xml.dom.minidom`
モジュールの実装を使うと、常に minidom 実装の :class:`Document` インスタンスを返します。一方、 :mod:`xml.dom`
版の関数では、別の実装によるインスタンスを返すかもれません (`PyXML package <http://pyxml.sourceforge.net/>`_
がインストールされているとそうなるでしょう)。 :class:`Document` を取得したら、DOM を構成するために子ノードを追加していくことができます::

   from xml.dom.minidom import getDOMImplementation

   impl = getDOMImplementation()

   newdoc = impl.createDocument(None, "some_tag", None)
   top_element = newdoc.documentElement
   text = newdoc.createTextNode('Some textual content.')
   top_element.appendChild(text)

DOM 文書オブジェクトを手にしたら、XML 文書のプロパティやメソッドを使って、文書の一部にアクセスすることができます。これらのプロパティは DOM
仕様で定義されています。文書オブジェクトの主要なプロパティは :attr:`documentElement` プロパティです。このプロパティは XML
文書の主要な要素: 他の全ての要素を保持する要素、を与えます。以下にプログラム例を示します::

   dom3 = parseString("<myxml>Some data</myxml>")
   assert dom3.documentElement.tagName == "myxml"

DOMツリーを使い終えた後に、 :meth:`unlink` メソッドを呼び出すことで
利用されなくなったオブジェクトが早くクリーンアップされるように助けることができます。
:meth:`unlink` は、 DOM API に対する :mod:`xml.dom.minidom`  特有の拡張です。ノードに対して
:meth:`unlink` を呼び出した後は、ノードとその下位ノードは本質的には無意味なものとなります。
このメソッドを呼び出さなくても、 Python のガベージコレクタがいつかはツリーの
オブジェクトを後片付けします。

.. seealso::

   `Document Object Model (DOM) Level 1 Specification <http://www.w3.org/TR/REC-DOM-Level-1/>`_
      :mod:`xml.dom.minidom` でサポートされている DOM の W3C 勧告。


.. _minidom-objects:

DOM オブジェクト
----------------

Python の DOM API 定義は :mod:`xml.dom` モジュールドキュメント
の一部として与えられています。この節では、 :mod:`xml.dom` の API と :mod:`xml.dom.minidom`
との違いについて列挙します。


.. method:: Node.unlink()

   DOM との内部的な参照を破壊して、循環参照ガベージコレクションを持たないバージョンの Python でもガベージコレクションされるように
   します。循環参照ガベージコレクションが利用できても、このメソッドを使えば、大量のメモリをすぐに使えるようにできるため、必要なくなったらすぐにこのメソッドを
   DOM オブジェクトに対して呼ぶのが良い習慣です。このメソッドは :class:`Document` オブジェクトに対してだけ呼び出せば
   よいのですが、あるノードの子ノードを放棄するために子ノードに対して呼び出してもかまいません。


.. method:: Node.writexml(writer[, indent=""[, addindent=""[, newl=""]]])

   XML を *writer* オブジェクトに書き込みます。 *writer* は、ファイルオブジェクトインタフェースの :meth:`write` に該当する
   メソッドを持たなければなりません。 *indent* パラメタには現在のノードのインデントを指定します。 *addindent*
   パラメタには現在のノードの下にサブノードを追加する際のインデント増分を指定します。 *newl* には、改行時に行末を終端する文字列を指定します。

   :class:`Document` ノードでは、追加のキーワード引数 *encoding* を使って XML ヘッダの
   encoding フィールドを指定することができます。

   .. versionchanged:: 2.1
      美しい出力をサポートするため、新たなキーワード引数 *indent* 、 *addindent* 、および *newl* が追加されました.

   .. versionchanged:: 2.3
      :class:`Document` ノードに対して、追加のキーワード引数 *encoding* を使って、
      XML ヘッダの encoding フィールドを指定できるようになりました.


.. method:: Node.toxml([encoding])

   DOM が表現している XML を文字列にして返します。

   引数がなければ、 XML ヘッダは encoding を指定せず、文書内の全ての文字をデフォルトエンコード方式で表示できない場合、結果は Unicode
   文字列となります。この文字列を UTF-8 以外のエンコード方式でエンコードするのは不正であり、なぜなら UTF-8 が XML
   のデフォルトエンコード方式だからです。

   明示的な *encoding* [1]_ 引数があると、結果は指定されたエンコード方式によるバイト文字列となります。引数を常に指定するよう推奨します。
   表現不可能なテキストデータの場合に :exc:`UnicodeError` が送出されるのを避けるため、encoding 引数は "utf-8"
   に指定するべきです。

   .. versionchanged:: 2.3
      *encoding* が追加されました。
       :meth:`writexml` を参照して下さい。


.. method:: Node.toprettyxml([indent=""[, newl=""[, encoding=""]]])

   美しく出力されたバージョンの文書を返します。 *indent* はインデントを行うための文字で、デフォルトはタブです; *newl*
   には行末で出力される文字列を指定し、デフォルトは ``\n`` です。

   .. versionadded:: 2.1

   .. versionchanged:: 2.3
      encoding 引数が追加されました。
      :meth:`writexml` を参照して下さい。

以下の標準 DOM メソッドは、 :mod:`xml.dom.minidom` では特別な注意をする必要があります:


.. method:: Node.cloneNode(deep)

   このメソッドは Python 2.0 にパッケージされているバージョンの :mod:`xml.dom.minidom` にはありましたが、これには深刻な
   障害があります。以降のリリースでは修正されています。


.. _dom-example:

DOM の例
--------

以下のプログラム例は、かなり現実的な単純なプログラムの例です。特にこの例に関しては、DOM の柔軟性をあまり活用してはいません。

.. literalinclude:: ../includes/minidom-example.py


.. _minidom-and-dom:

minidom と DOM 標準
-------------------

:mod:`xml.dom.minidom` モジュールは、本質的には DOM 1.0 互換の DOM に、いくつかの DOM 2 機能 (主に名前空間機能)
を追加したものです。

Python における DOM インタフェースは率直なものです。以下の対応付け規則が適用されます:

* インタフェースはインスタンスオブジェクトを介してアクセスされます。アプリケーション自身から、クラスをインスタンス化してはなりません;
  :class:`Document` オブジェクト上で利用可能な生成関数 (creator function)
  を使わなければなりません。派生インタフェースでは基底インタフェースの全ての演算 (および属性) に加え、新たな演算をサポートします。

* 演算はメソッドとして使われます。DOM では :keyword:`in` パラメタのみを使うので、引数は通常の順番 (左から右へ) で渡されます。
  オプション引数はありません。 `void` 演算は ``None`` を返します。

* IDL 属性はインスタンス属性に対応付けられます。OMG IDL 言語における Python への対応付けとの互換性のために、属性 ``foo``
  はアクセサメソッド :meth:`_get_foo` および :meth:`_set_foo` でもアクセスできます。 `readonly`
  属性は変更してはなりません; とはいえ、これは実行時には強制されません。

* ``short int`` 、 ``unsigned int`` 、 ``unsigned long long`` 、および ``boolean``
  型は、全て Python 整数オブジェクトに対応付けられます。

* ``DOMString`` 型は Python 文字列型に対応付けられます。 :mod:`xml.dom.minidom` ではバイト文字列 (byte
  string) および Unicode 文字列のどちらかに対応づけられますが、通常 Unicode 文字列を生成します。 ``DOMString``
  型の値は、W3C の DOM 仕様で、IDL ``null`` 値になってもよいとされている場所では ``None`` になることもあります。

* `const` 宣言を行うと、 (``xml.dom.minidom.Node.PROCESSING_INSTRUCTION_NODE``
  のように) 対応するスコープ内の変数に対応付けを行います; これらは変更してはなりません。

* ``DOMException`` は現状では :mod:`xml.dom.minidom`
  でサポートされていません。その代わり、 :mod:`xml.dom.minidom`  は、 :exc:`TypeError` や
  :exc:`AttributeError` といった標準の Python 例外を使います。

* :class:`NodeList` オブジェクトは Python の組み込みリスト型を使って実装されています。 Python 2.2
  からは、これらのオブジェクトは DOM 仕様で定義されたインタフェースを提供していますが、それ以前のバージョンの Python では、公式の API
  をサポートしていません。しかしながら、これらの API は W3C 勧告で定義されたインタフェースよりも "Python 的な" ものになっています。

以下のインタフェースは :mod:`xml.dom.minidom` では全く実装されていません:

* :class:`DOMTimeStamp`

* :class:`DocumentType` (added in Python 2.1)

* :class:`DOMImplementation` (added in Python 2.1)

* :class:`CharacterData`

* :class:`CDATASection`

* :class:`Notation`

* :class:`Entity`

* :class:`EntityReference`

* :class:`DocumentFragment`

これらの大部分は、ほとんどの DOM のユーザにとって一般的な用途として有用とはならないような XML 文書内の情報を反映しています。

.. rubric:: 注記

.. [#] XML の出力に含まれるエンコーディング文字列は適切な標準に\
   適合していなければなりません。
   たとえば、"UTF-8" は正当ですが、"UTF8" は違います。
   http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-EncodingDecl
   と
   http://www.iana.org/assignments/character-sets
   を参照して下さい。
