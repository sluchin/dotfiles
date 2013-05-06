
:mod:`xml.sax.saxutils` --- SAX ユーティリティ
==============================================

.. module:: xml.sax.saxutils
   :synopsis: SAX とともに使う有用な関数とクラスです。
.. moduleauthor:: Lars Marius Garshol <larsga@garshol.priv.no>
.. sectionauthor:: Martin v. Löwis <martin@v.loewis.de>


.. versionadded:: 2.0

モジュール :mod:`xml.sax.saxutils` には SAX アプリケーションの作成に\
役立つ多くの関数やクラスも含まれており、直接利用したり、基底クラスとして使うことができます。


.. function:: escape(data[, entities])

   文字列データ内の ``'&'``, ``'<'``, ``'>'`` をエスケープします。

   オプションの *entities* パラメータに辞書を渡すことで、そのほかの文字をエスケープさせることも可能です。辞書のキーと値はすべて文字列\
   で、キーに指定された文字は対応する値に置換されます。
   ``'&'``, ``'<'``, ``'>'`` は *entities* が与えられるかどうかに関わらず、常にエスケープします。

.. function:: unescape(data[, entities])

   エスケープされた文字列 ``'&amp;'``, ``'&lt;'``, ``'&gt;'`` を元の文字に戻します。

   オプションの *entities* パラメータに辞書を渡すことで、そのほかの文字をエスケープさせることも可能です。辞書のキーと値はすべて文字列\
   で、キーに指定された文字は対応する値に置換されます。
   ``'&amp;'``, ``'&lt;'``, ``'&gt;'`` は *entities* が与えられるかどうかに関わらず、常に元の文字に戻します。

   .. versionadded:: 2.3


.. function:: quoteattr(data[, entities])

   :func:`escape` に似ていますが、 *data* は属性値の作成に使われます。戻り値はクォート済みの *data* で、置換する文字の追加も可\
   能です。 :func:`quoteattr` はクォートすべき文字を *data* の文脈から判断し、クォートすべき文字を残さないように文字列をエンコード\
   します。

   *data* の中にシングル・クォート、ダブル・クォートがあれば、両方ともエンコードし、全体をダブルクォートで囲みます。戻り値の文字列はその\
   ままで属性値として利用できます。::

      >>> print "<element attr=%s>" % quoteattr("ab ' cd \" ef")
      <element attr="ab ' cd &quot; ef">

   この関数は参照具象構文を使って、 HTML や SGML の属性値を生成するのに便利です。

   .. versionadded:: 2.2


.. class:: XMLGenerator([out[, encoding]])

   このクラスは :class:`ContentHandler` インターフェースの実装で、SAX イベントを XML ドキュメントに書き戻します。つまり、
   :class:`XMLGenerator` をコンテント・ハンドラとして用いると、パースしたオリジナル・ドキュメントの複製が作れるのです。 *out*
   に指定するのはファイル風のオブジェクトで、デフォルトは *sys.stdout* です。 *encoding*
   は出力ストリームのエンコーディングで、デフォルトは ``'iso-8859-1'`` です。


.. class:: XMLFilterBase(base)

   このクラスは :class:`XMLReader` とクライアント・アプリケーションのイベント・ハンドラとの間に位置するものとして設計されています。デフォル\
   トでは何もせず、ただリクエストをリーダに、イベントをハンドラに、それぞれ加工せず渡すだけです。しかし、サブクラスでメソッドをオーバーライ\
   ドすると、イベント・ストリームやリクエストを加工してから渡すように変更可能です。


.. function:: prepare_input_source(source[, base])

   この関数は引き数に入力ソース、オプションとして URL を取り、読み取り可能な解決済み :class:`InputSource`
   オブジェクトを返します。入力ソースは文字列、ファイル風オブジェクト、 :class:`InputSource` のいずれでも\
   良く、この関数を使うことで、パーサは様々な *source* パラメータを :meth:`parse` に渡すことが可能になります。

