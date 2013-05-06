
:mod:`xml.etree.ElementTree` --- ElementTree XML API
====================================================

.. module:: xml.etree.ElementTree
   :synopsis: Implementation of the ElementTree API.
.. moduleauthor:: Fredrik Lundh <fredrik@pythonware.com>


.. versionadded:: 2.5

:class:`Element` 型は柔軟性のあるコンテナオブジェクトで、階層的データ構造を
メモリーに格納するようにデザインされています。この型は言わばリストと辞書の
間の子のようなものです。

各エレメントは関連する多くのプロパティを具えています:

* このエレメントがどういう種類のデータを表現しているかを同定する文字列であるタグ(別の言い方をすればエレメントの型)

* 幾つもの属性(Python 辞書に収められます)

* テキスト文字列

* オプションの末尾文字列

* 幾つもの子エレメント(Python シーケンスに収められます)

element のインスタンスを作るには、 :class:`Element` コンストラクタや
:func:`SubElement` ファクトリー関数を使います。

:class:`ElementTree` クラスはエレメントの構造を包み込み、それと XML を行き来するのに使えます。

この API の C 実装である :mod:`xml.etree.cElementTree` も使用可能です。

チュートリアルその他のドキュメントへのリンクについては
http://effbot.org/zone/element-index.htm を参照して下さい。
Fredrik Lundh のページも xml.etree.ElementTree の開発バージョンの置き場所です。

.. versionchanged:: 2.7
   ElementTree API が 1.3 に更新されました。
   より詳しい情報については、
   `Introducing ElementTree 1.3 <http://effbot.org/zone/elementtree-13-intro.htm>`_
   を参照してください。


.. _elementtree-functions:

関数
----


.. function:: Comment(text=None)

   コメント・エレメントのファクトリーです。このファクトリー関数は、標準のシリアライザでは
   XML コメントにシリアライズされる特別な要素を作ります。コメント文字列は byte
   文字列でも Unicode 文字列でも構いません。
   *text* はそのコメント文字列を含んだ文字列です。
   コメントを表わすエレメントのインスタンスを返します。


.. function:: dump(elem)

   エレメントの木もしくはエレメントの構造を sys.stdout に書き込みます。この関数はデバグ目的でだけ使用してください。

   出力される形式の正確なところは実装依存です。このバージョンでは、通常の XML ファイルとして書き込まれます。

   *elem* はエレメントの木もしくは個別のエレメントです。


.. function:: fromstring(text)

   文字列定数で与えられた XML 断片を構文解析します。 :func:`XML` 関数と
   同じです。 *text* は XML データの文字列です。
   :class:`Element` インスタンスを返します。


.. function:: fromstringlist(sequence, parser=None)

   文字列のシーケンスからXMLドキュメントを解析します。
   *sequence* は XML データのフラグメントを格納した、 list かその他のシーケンスです。
   *parser* はオプションの parser インスタンスです。指定されなかった場合、
   標準の :class:`XMLParser` パーサーが利用されます。
   :class:`Elment` インスタンスを返します。

   .. versionadded:: 2.7


.. function:: iselement(element)

   オブジェクトが正当なエレメント・オブジェクトであるかをチェックします。 *element* はエレメント・インスタンスです。
   引数がエレメント・オブジェクトならば真値を返します。


.. function:: iterparse(source, events=None, parser=None)

   XML 断片を構文解析してエレメントの木を漸増的に作っていき、その間進行状況をユーザーに報告します。 *source* は XML
   データを含むファイル名またはファイル風オブジェクト。 *events* は報告すべきイベントのリスト。
   省略された場合は "end" イベントだけが報告されます。
   *parser* はオプションの引数で、パーサーのインスタンスを指定します。
   指定されなかった場合は標準の :class:`XMLParser` が利用されます。
   ``(event, elem)`` ペアのイテレータ(:term:`iterator`)を返します。

   .. note::
      :func:`iterparse` は "start" イベントを送り出すとき\
      開始タグの ">" なる文字を見たことだけを保証しますので、
      アトリビュートは定義されますが、その時点ではテキストの内容も\
      テール・アトリビュートもまだ定義されていません。
      同じことは子エレメントにも言えて、その時点ではあるともないとも言えません。

      全部が揃ったエレメントが必要ならば、"end" イベントを探すようにして下さい。


.. function:: parse(source, parser=None)

   XML 断片を構文解析して element tree にします。
   *source* は XML データを含むファイル名またはファイル風オブジェクト。 *parser*
   はオプションの構文解析器インスタンスです。
   これが与えられない場合、標準の :class:`XMLParser` パーサーが使われます。
   :class:`ElementTree` インスタンスを返します。


.. function:: ProcessingInstruction(target, text=None)

   PI エレメントのファクトリー。このファクトリー関数は XML の処理命令(processing instruction)
   としてシリアライズされる特別なエレメントを作ります。 *target* は PI ターゲットを含んだ文字列です。 *text* は与えられるならば PI
   コンテンツを含んだ文字列です。
   PI を表わすエレメント・インスタンスを返します。


.. function:: register_namespace(prefix, uri)

   名前空間の prefix を登録します。レジストリはグローバルで、与えられた prefix
   か名前空間URI のどちらかの既存のマッピングは全て削除されます。
   *prefix* は名前空間の prefix です。 *uri* は名前空間のURIです。
   この名前空間のタグや属性は、可能な限り与えられた prefix をつけてシリアライズされます。

   .. versionadded:: 2.7


.. function:: SubElement(parent, tag[, attrib[, **extra]])

   子エレメントのファクトリー。この関数はエレメント・インスタンスを作り、それを既存のエレメントに追加します。

   エレメント名、アトリビュート名およびアトリビュート値は byte 文字列でも Unicode 文字列でも構いません。 *parent*
   は親エレメントです。 *tag* はエレメント名です。 *attrib* はオプションの辞書で、エレメントのアトリビュートを含んでいます。 *extra*
   は追加のアトリビュートで、キーワード引数として与えられたものです。
   エレメント・インスタンスを返します。


.. function:: tostring(element, encoding="us-ascii", method="xml")

   XML エレメントを全ての子エレメントを含めて表現する文字列を生成します。
   *element* は :class:`Element` のインスタンスです。 *encoding* [1]_
   は出力エンコーディング(デフォルトは US-ASCII)です。
   *method* は ``"xml"``, ``"html"``, ``"text"`` のいずれか(デフォルトは ``"xml"``) です。
   XML データを含んだエンコードされた文字列を返します。


.. function:: tostringlist(element, encoding="us-ascii", method="xml")

   XML エレメントを全ての子エレメントを含めて表現する文字列を生成します。
   *element* は :class:`Element` のインスタンスです。 *encoding* [1]_
   は出力エンコーディング(デフォルトは US-ASCII)です。
   *method* は ``"xml"``, ``"html"``, ``"text"`` のいずれか(デフォルトは ``"xml"``) です。
   XML データを含んだエンコードされた文字列のリストを返します。
   これは、 ``"".join(tostringlist(element)) == tostring(element)``
   であること以外、なにか特定のシーケンスになることは保証していません。

   .. versionadded:: 2.7


.. function:: XML(text, parser=None)

   文字列定数で与えられた XML 断片を構文解析します。この関数は Python コードに
   「XML リテラル」を埋め込むのに使えます。 *text* は XML データを含んだ文字列です。
   *parser* はオプションで、パーサーのインスタンスです。指定されなかった場合は、
   標準の :class:`XMLParser` パーサーを利用します。
   :class:`Element` のインスタンスを返します。


.. function:: XMLID(text, parser=None)

   文字列定数で与えられた XML 断片を構文解析し、エレメント ID からエレメント
   へのマッピングを与える辞書も同時に返します。 *text* は XMLデータを
   含んだ文字列です。
   *parser* はオプションで、パーサーのインスタンスです。指定されなかった場合は、
   標準の :class:`XMLParser` パーサーを利用します。
   :class:`Element` のインスタンスと辞書のタプルを返します。


.. _elementtree-element-objects:

Element オブジェクト
----------------------

.. function:: Element(tag[, attrib][, **extra])

   エレメントクラス。この関数は Element インタフェースを定義すると同時に、
   そのリファレンス実装を提供します。

   エレメント名、アトリビュート名およびアトリビュート値は bytes 文字列でも
   Unicode 文字列でも構いません。 *tag* はエレメント名です。
   *attrib* はオプションの辞書で、エレメントのアトリビュートを含んでいます。 *extra*
   は追加のアトリビュートで、キーワード引数として与えられたものです。
   エレメント・インスタンスを返します。



   .. attribute:: tag

      このエレメントが表すデータの種類を示す文字列です(言い替えると、エレメントの型です)。

   .. attribute:: text

      *text* アトリビュートはエレメントに結びつけられた付加的なデータを保持するのに使われます。
      名前が示唆しているようにこのアトリビュートはたいてい文字列ですが、
      アプリケーション固有のオブジェクトであって構いません。
      エレメントが XML ファイルから作られたものならば、このアトリビュートは
      エレメント・タグの間にあるテキストを丸ごと含みます。


   .. attribute:: tail

      *tail* アトリビュートはエレメントに結びつけられた付加的なデータを保持するのに使われます。
      このアトリビュートはたいてい文字列ですが、アプリケーション固有のオブジェクトであって構いません。
      エレメントが XML ファイルから作られたものならば、このアトリビュートはエレメントの
      終了タグと次のタグの直前までの間に見つかったテキストを丸ごと含みます。


   .. attribute:: attrib

      エレメントのアトリビュートを保持する辞書です。
      次のことに注意しましょう。
      *attrib* は普通の書き換え可能な Python の辞書ではあるのですが、
      ElementTree の実装によっては別の内部表現を選択して要求されたときにだけ辞書を作るようにするかもしれません。
      そうした実装の利益を享受するために、可能な限り下記の辞書メソッドを通じて使いましょう。

   以下の辞書風メソッドがエレメントのアトリビュートに対して働きます。

   .. method:: clear()

      エレメントをリセットします。全ての子孫エレメントを削除し、アトリビュートをクリアし、
      test と tail を ``None`` にセットします。


   .. method:: get(key, default=None)

      エレメントの *key* という名前のアトリビュートを取得します。

      アトリビュートの値、またはアトリビュートがない場合は *default* を返します。


   .. method:: items()

      エレメントのアトリビュートを (名前, 値) ペアのシーケンスとして返します。
      返されるアトリビュートの順番は決まっていません。


   .. method:: keys()

      エレメントのアトリビュート名をリストとして返します。
      返される名前の順番は決まっていません。

   .. method:: set(key, value)

      エレメントのアトリビュート *key* に *value* をセットします。

   以下のメソッドはエレメントの子(サブエレメント)に対して働きます。

   .. method:: append(subelement)

      エレメント *subelement* をこのエレメントの内部にあるサブエレメントの
      リストの最後に追加します。

   .. method:: extend(subelements)

      シーケンスオブジェクト *subelements* から 0個以上のサブエレメントを追加します。
      サブエレメントが有効なオブジェクトでない場合は :exc:`AssertionError`
      を発生させます。

      .. versionadded:: 2.7

   .. method:: find(match)

      *match* にマッチする最初のサブエレメントを探します。
      *match* はタグ名かパス(path)です。
      エレメント・インスタンスまたは ``None`` を返します。

   .. method:: findall(match)

      タグ名かパスにマッチする全てのサブエレメントを探します。
      全てのマッチするエレメントを、ドキュメント上の順序で含むリストを返します。

   .. method:: findtext(match, default=None)

      *match* にマッチする最初のサブエレメントのテキストを探します。
      *match* はタグ名かパスです。
      最初にマッチするエレメントの text を返すか、エレメントが見あたらなかった場合
      *default* を返します。
      マッチしたエレメントに text がなければ空文字列が返されるので気を付けましょう。

   .. method:: getchildren()

      .. deprecated:: 2.7
         ``list(elem)`` を使うか、 Element に対してイテレートしてください。

   .. method:: getiterator(tag=None)

      .. deprecated:: 2.7
         :meth:`Element.iter` メソッドを使ってください。

   .. method:: insert(index, element)

      サブエレメントをこのエレメントの与えられた位置に挿入します。


   .. method:: iter(tag=None)

      現在のエレメントを根とするツリーのイテレータ(:term:`iterator`)を作ります。
      イテレータは現在のエレメントとそれ以下の全てのエレメントを、
      文書中での出現順(深さ優先順)でイテレートします。
      *tag* が ``None`` または ``'*'`` でない場合は、
      与えられたタグに等しいものについてのみイテレータから返されます。
      イテレート中にツリー構造が変更された場合の結果は未定義です。


   .. method:: iterfind(match)

      タグ名かパスにマッチする全てのサブエレメントを探します。
      全てのマッチするエレメントをドキュメント上の順序で yield する
      イテレート可能オブジェクトを返します。

      .. versionadded:: 2.7


   .. method:: itertext()

      text のイテレータを作成します。
      このイテレータは、このエレメントと全てのサブエレメントをドキュメント上の
      順序で巡回し、全ての内部の text を返します。

      .. versionadded:: 2.7


   .. method:: makeelement(tag, attrib)

      現在のエレメントと同じ型の新しいエレメント・オブジェクトを作ります。
      このメソッドは呼び出さずに、 :func:`SubElement` ファクトリー関数を使って下さい。

   .. method:: remove(subelement)

      現在のエレメントから *subelement* を削除します。
      find\* メソッド群と違ってこのメソッドはエレメントをインスタンスの同一性で比較します。
      タグや内容では比較しません。

   :class:`Element` オブジェクトは以下のシーケンス型のメソッドを、サブエレメントを
   操作するためにサポートします:  :meth:`__delitem__`, :meth:`__getitem__`, :meth:`__setitem__`,
   :meth:`__len__`.

   注意: サブエレメントを持たないエレメントの真偽値は ``False`` になります。
   この挙動は将来のバージョンで変更されるかもしれません。
   直接真偽値をテストするのでなく、 ``len(elem)`` か ``elem is None`` を利用してください。 ::

     element = root.find('foo')

     if not element:  # careful!
         print "element not found, or element has no subelements"

     if element is None:
         print "element not found"


.. _elementtree-elementtree-objects:

ElementTree オブジェクト
------------------------


.. class:: ElementTree(element=None, file=None)

   ElementTree ラッパー・クラス。このクラスはエレメントの全階層を表現し、
   さらに標準 XML との相互変換を追加しています。

   *element* は根エレメントです。
   木はもし *file* が与えられればその XML の内容により初期化されます。


   .. method:: _setroot(element)

      この木の根エレメントを置き換えます。
      したがって現在の木の内容は破棄され、与えられたエレメントが代わりに使われます。
      注意して使ってください。 *element* はエレメント・インスタンスです。


   .. method:: find(match)

      *match* にマッチする最初のトップレベルのエレメントを探します。
      *match* はタグ名かパスです。
      getroot().find(path) と同じです。
      最初に条件に合ったエレメント、または見つからない時は ``None`` を返します。


   .. method:: findall(match)

      タグ名かパスにマッチする全てのサブエレメントを探します。
      getroot().findall(match) と同じです。 *match* はタグ名かパスです。
      条件に合った全てのエレメントを、ドキュメント上の順序で格納したリストを返します。


   .. method:: findtext(match, default=None)

      子孫エレメントの中で与えられたタグを持つ最初のもののテキストを見つけます。
      getroot().findtext(match) と同じです。 *match* はタグ名かパスです。
      *default* はエレメントが見つからなかった場合に返される値です。
      条件に合った最初のエレメントのテキスト、または見つからなかった場合にはデフォルト値を返します。
      もしエレメントが見つかったもののテキストがなかった場合には、
      このメソッドは空文字列を返すということに気をつけてください。


   .. method:: getiterator(tag=None)

      .. deprecated:: 2.7
         代わりに :meth:`ElementTree.iter` メソッドを利用してください。


   .. method:: getroot()

      この木の根エレメントを返します。


   .. method:: iter(tag=None)

      ルートエレメントに対する、ツリーを巡回するイテレータを返します。
      イテレータはツリーの全てのエレメントに渡ってセクション順にループします。
      *tag* は探したいタグです(デフォルトでは全てのエレメントを返します)。


   .. method:: iterfind(match)

      タグ名かパスにマッチする全てのサブエレメントを返します。
      getroot().iterfind(match) と同じです。
      全てのマッチするエレメントをドキュメント順に yield するイテレート可能
      オブジェクトを返します。

      .. versionadded:: 2.7


   .. method:: parse(source, parser=None)

      外部の XML 断片をこのエレメントの木に読み込みます。
      *source* は XML データを含むファイル名またはファイル風オブジェクト。
      *parser* はオプションの構文解析器インスタンスです。
      これが与えられない場合、標準の XMLParser パーサーが使われます。
      断片の根エレメントを返します。


   .. method:: write(file, encoding="us-ascii", xml_declaration=None, method="xml")

      エレメントの木をファイルに XML として書き込みます。
      *file* はファイル名またはファイル風オブジェクトで書き込み用に開かれたもの。
      *encoding* [1]_ は出力エンコーディング(デフォルトは US-ASCII)です。
      *xml_declaration* は、 XML 宣言がファイルに書かれるかどうかを制御します。
      False の場合は常に書かれず、 True の場合は常に書かれ、 None の場合は
      US-ASCII か UTF-8 以外の場合に書かれます (デフォルトは None です)。
      *method* は ``"xml"``, ``"html"``, ``"text"`` のいづれかです
      (デフォルトは ``"xml"`` です)。
      エンコードされた文字列を返します。


次に示すのがこれから操作する XML ファイルです::

    <html>
        <head>
            <title>Example page</title>
        </head>
        <body>
            <p>Moved to <a href="http://example.org/">example.org</a>
            or <a href="http://example.com/">example.com</a>.</p>
        </body>
    </html>

第1段落の全てのリンクの "target" アトリビュートを変更する例::

    >>> from xml.etree.ElementTree import ElementTree
    >>> tree = ElementTree()
    >>> tree.parse("index.xhtml")
    <Element 'html' at 0xb77e6fac>
    >>> p = tree.find("body/p")     # Finds first occurrence of tag p in body
    >>> p
    <Element 'p' at 0xb77ec26c>
    >>> links = list(p.iter("a"))   # Returns list of all links
    >>> links
    [<Element 'a' at 0xb77ec2ac>, <Element 'a' at 0xb77ec1cc>]
    >>> for i in links:             # Iterates through all found links
    ...     i.attrib["target"] = "blank"
    >>> tree.write("output.xhtml")


.. _elementtree-qname-objects:

QName オブジェクト
------------------


.. class:: QName(text_or_uri, tag=None)

   QName ラッパー。このクラスは QName アトリビュート値をラップし、出力時に
   真っ当な名前空間の扱いを得るために使われます。 *text_or_uri*
   は {uri}local という形式の QName 値を含む文字列、または tag 引数が与えられた場合には QName の URI 部分の文字列です。
   *tag* が与えられた場合、一つめの引数は URI と解釈され、この引数はローカル名と解釈されます。
   :class:`QName` インスタンスは不透明です。


.. _elementtree-treebuilder-objects:

TreeBuilder オブジェクト
------------------------


.. class:: TreeBuilder(element_factory=None)

   汎用のエレメント構造ビルダー。これは start, data, end の
   メソッド呼び出しの列を整形式のエレメント構造に変換します。
   このクラスを使うと、好みの XML 構文解析器、または他の XML に似た形式の
   構文解析器を使って、エレメント構造を作り出すことができます。 *element_factory*
   が与えられた場合には、新しい :class:`Element` インスタンスを作る際にこれを呼び出します。


   .. method:: close()

      ビルダーのバッファをフラッシュし、最上位の文書エレメントを返します。
      :class:`Element` インスタンスを返します。


   .. method:: data(data)

      現在のエレメントにテキストを追加します。 *data* は文字列です。
      bytes 文字列もしくは Unicode 文字列でなければなりません。


   .. method:: end(tag)

      現在のエレメントを閉じます。 *tag* はエレメントの名前です。
      閉じられたエレメントを返します。


   .. method:: start(tag, attrs)

      新しいエレメントを開きます。 *tag* はエレメントの名前です。
      *attrs* はエレメントのアトリビュートを保持した辞書です。
      開かれたエレメントを返します。

   加えて、カスタムの :class:`TreeBuilder` オブジェクトは以下のメソッドを
   提供できます。

   .. method:: doctype(name, pubid, system)

      doctype 宣言を処理します。 *name* は doctype 名です。
      *pubid* は公式の識別子です。 *system* はシステム識別子です。
      このメソッドはデフォルトの :class:`TreeBuilder` クラスには存在しません。

      .. versionadded:: 2.7


.. _elementtree-xmlparser-objects:

XMLParser オブジェクト
-----------------------

   XML ソースからエレメント構造を作るもので、expat 構文解析器に基づいています。 *html* は前もって定義された HTML
   エンティティです。このオプションは現在の実装ではサポートされていません。
   *target* はターゲットとなるオブジェクトです。省略された場合、標準の
   TreeBuilder クラスのインスタンスが使われます。

.. class:: XMLParser(html=0, target=None, encoding=None)

   expat パーサーを利用した、XML ソースデータからの :class:`Element` 構造ビルダー。
   *html* は定義済みの HTML エンティティです。このフラグは現在の実装では
   サポートされていません。
   *target* はターゲットオブジェクトです。省略された場合、ビルダーは
   標準の TreeBuilder クラスのインスタンスを利用します。
   *encoding* [1]_ はオプションで、与えられた場合は XML ファイルで指定された
   エンコーディングをオーバーライドします。


   .. method:: close()

      構文解析器にデータを供給するのを終わりにします。
      エレメント構造を返します。


   .. method:: doctype(name, pubid, system)

      .. deprecated:: 2.7
         カスタムの TreeBuilder target で :meth:`TreeBuilder.doctype` メソッドを
         定義してください。


   .. method:: feed(data)

      構文解析器にデータを供給します。
      *data* はエンコードされたデータです。

:meth:`XMLParser.feed` は *target* の :meth:`start` メソッドを
それぞれの開始タグに対して呼び、また :meth:`end` メソッドを終了タグに対して呼び、
そしてデータは :meth:`data` メソッドで処理されます。
:meth:`XMLParser.close` は *target* の :meth:`close` メソッドを呼びます。
:class:`XMLParser` は木構造を構築する以外にも使えます。
以下の例は、XML ファイルの最高の深さを数えます::

    >>> from xml.etree.ElementTree import XMLParser
    >>> class MaxDepth:                     # The target object of the parser
    ...     maxDepth = 0
    ...     depth = 0
    ...     def start(self, tag, attrib):   # Called for each opening tag.
    ...         self.depth += 1
    ...         if self.depth > self.maxDepth:
    ...             self.maxDepth = self.depth
    ...     def end(self, tag):             # Called for each closing tag.
    ...         self.depth -= 1
    ...     def data(self, data):
    ...         pass            # We do not need to do anything with data.
    ...     def close(self):    # Called when all data has been parsed.
    ...         return self.maxDepth
    ...
    >>> target = MaxDepth()
    >>> parser = XMLParser(target=target)
    >>> exampleXml = """
    ... <a>
    ...   <b>
    ...   </b>
    ...   <b>
    ...     <c>
    ...       <d>
    ...       </d>
    ...     </c>
    ...   </b>
    ... </a>"""
    >>> parser.feed(exampleXml)
    >>> parser.close()
    4


.. rubric:: 注記

.. [#] XML の出力に含まれるエンコーディング文字列は適切な標準に\
   適合していなければなりません。
   たとえば、"UTF-8" は正当ですが、"UTF8" は違います。
   http://www.w3.org/TR/2006/REC-xml11-20060816/#NT-EncodingDecl
   と
   http://www.iana.org/assignments/character-sets
   を参照して下さい。
