
.. _markup:

************************
構造化マークアップツール
************************

Python は様々な構造化データマークアップ形式を扱うための、
様々なモジュールをサポートしています。
これらは標準化一般マークアップ言語 (SGML)
およびハイパーテキストマークアップ言語 (HTML)、
そして可拡張性マークアップ言語 (XML) を扱うためのいくつかのインタフェースからなります。

注意すべき重要な点として、
:mod:`xml` パッケージは少なくとも一つの SAX に対応した XML パーザが利用可能でなければなりません。
Python 2.3 からは Expat パーザが Python に取り込まれているので、
:mod:`xml.parsers.expat` モジュールは常に利用できます。
また、 `PyXML 追加パッケージ <http://pyxml.sourceforge.net/>`_
についても知りたいと思うかもしれません;
このパッケージは Python 用の拡張された XML ライブラリセットを提供します。

:mod:`xml.dom` および :mod:`xml.sax` パッケージのドキュメントは
Python による DOM および SAX インタフェースへのバインディングに関する定義です。


.. toctree::

   htmlparser.rst
   sgmllib.rst
   htmllib.rst
   pyexpat.rst
   xml.dom.rst
   xml.dom.minidom.rst
   xml.dom.pulldom.rst
   xml.sax.rst
   xml.sax.handler.rst
   xml.sax.utils.rst
   xml.sax.reader.rst
   xml.etree.elementtree.rst
