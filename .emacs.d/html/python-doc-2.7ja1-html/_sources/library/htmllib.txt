:mod:`htmllib` --- HTML 文書の解析器
====================================

.. module:: htmllib
   :synopsis: HTML 文書の解析器。
   :deprecated:

.. deprecated:: 2.6
    :mod:`htmllib` モジュールは Python 3.0 で削除されました。


.. index::
   single: HTML
   single: hypertext

このモジュールでは、ハイパーテキスト記述言語 (HTML, HyperText Mark-up  Language)
形式で書式化されたテキストファイルを解析するための基盤として役立つクラスを定義しています。このクラスは I/O と直接的には接続されません ---
このクラスにはメソッドを介して文字列形式の入力を提供する必要があり、出力を生成するには "フォーマッタ (formatter)"
オブジェクトのメソッドを何度か呼び出さなくてはなりません。

.. index::
   module: sgmllib
   module: formatter
   single: SGMLParser (in module sgmllib)

:class:`HTMLParser` クラスは、機能を追加するために他のクラスの基底クラスとして利用するように設計されており、ほとんどのメソッドが拡張したり
上書きしたりできるようになっています。さらにこのクラスは :mod:`sgmllib` モジュールで定義されている :class:`SGMLParser`
クラスから派生しており、その機能を拡張しています。 :class:`HTMLParser` の実装は、 :rfc:`1866` で解説されている HTML
2.0 記述言語をサポートします。 :mod:`formatter` では 2 つのフォーマッタオブジェクト実装が提供されています;
フォーマッタのインタフェースについての情報は :mod:`formatter` モジュールのドキュメントを参照してください。

以下は :class:`sgmllib.SGMLParser` で定義されているインタフェースの概要です:

* インスタンスにデータを与えるためのインタフェースは :meth:`feed` メソッドで、このメソッドは文字列を引数に取ります。
  このメソッドに一度に与えるテキストは必要に応じて多くも少なくもできます; というのは ``p.feed(a);p.feed(b)`` は
  ``p.feed(a+b)``  と同じ効果を持つからです。与えられたデータが完全な HTML マークアップ文を含む場合、それらの文は即座に処理されます;
  不完全なマークアップ構造はバッファに保存されます。全ての未処理データを強制的に処理させるには、 :meth:`close`  メソッドを呼び出します。

  例えば、ファイルの全内容を解析するには::

     parser.feed(open('myfile.html').read())
     parser.close()

  のようにします。

* HTML タグに対して意味付けを定義するためのインタフェースはとても単純です: サブクラスを派生して、 :meth:`start_tag` 、
  :meth:`end_tag` 、あるいは :meth:`do_tag` といったメソッドを定義するだけです。
  パーザはこれらのメソッドを適切なタイミングで呼び出します:  :meth:`start_tag` や :meth:`do_tag` は  ``<tag
  ...>`` の形式の開始タグに遭遇した時に呼び出されます; :meth:`end_tag` は ``<tag>`` の形式の終了タグに
  遭遇した時に呼び出されます。 ``<H1>`` ... ``</H1>`` のように開始タグが終了タグと対応している必要がある場合、クラス中で
  :meth:`start_tag` が定義されていなければなりません; ``<P>`` のように終了タグが必要ない場合、クラス中では
  :meth:`do_tag` を定義しなければなりません。

このモジュールではパーザクラスと例外を一つづつ定義しています:


.. class:: HTMLParser(formatter)

   基底となる HTML パーザクラスです。XHTML 1.0 仕様  (http://www.w3.org/TR/xhtml1)
   勧告で要求されている全てのエンティティ名をサポートしています。
   また、全ての HTML 2.0 の要素および HTML 3.0、3.2 の多くの要素のハンドラを定義しています。


.. exception:: HTMLParseError

   :class:`HTMLParser` クラスがパーズ処理中にエラーに遭遇した場合に送出する例外です。

   .. versionadded:: 2.4


.. seealso::

   Module :mod:`formatter`
      抽象化された書式イベントの流れを writer オブジェクト上の特定の出力イベントに変換するためのインターフェース。

   Module :mod:`HTMLParser`
      HTML パーザのひとつです。やや低いレベルでしか入力を扱えませんが、XHTML を扱うことができるように設計されています。"広く知られている HTML
      (HTML as deployed)" では使われておらずかつ XHTML では正しくないとされる SGML 構文のいくつかは実装されていません。

   Module :mod:`htmlentitydefs`
      XHTML 1.0 エンティティに対する置換テキストの定義。

   Module :mod:`sgmllib`
      :class:`HTMLParser` の基底クラス。


.. _html-parser-objects:

HTMLParser オブジェクト
-----------------------

タグメソッドに加えて、 :class:`HTMLParser` クラスではタグメソッドで利用するためのいくつかのメソッドとインスタンス変数を提供しています。


.. attribute:: HTMLParser.formatter

   パーザに関連付けられているフォーマッタインスタンスです。


.. attribute:: HTMLParser.nofill

   ブール値のフラグで、空白文字を縮約したくないときには真、縮約するときには偽にします。一般的には、この値を真にするのは、 ``<PRE>`` 要素の
   中のテキストのように、文字列データが "書式化済みの (preformatted)"  場合だけです。標準の値は偽です。この値は
   :meth:`handle_data` および :meth:`save_end` の操作に影響します。


.. method:: HTMLParser.anchor_bgn(href, name, type)

   このメソッドはアンカー領域の先頭で呼び出されます。引数は  ``<A>`` タグの属性で同じ名前を持つものに対応します。
   標準の実装では、ドキュメント内のハイパーリンク  (``<A>`` タグの ``HREF`` 属性) を列挙したリスト
   を維持しています。ハイパーリンクのリストはデータ属性 :attr:`anchorlist` で手に入れることができます。


.. method:: HTMLParser.anchor_end()

   このメソッドはアンカー領域の末尾で呼び出されます。標準の実装では、テキストの注釈マーカを追加します。マーカは  :meth:`anchor_bgn`
   で作られたハイパーリンクリストのインデクス値です。


.. method:: HTMLParser.handle_image(source, alt[, ismap[, align[, width[, height]]]])

   このメソッドは画像を扱うために呼び出されます。標準の実装では、単に :meth:`handle_data` に *alt* の値を渡すだけです。


.. method:: HTMLParser.save_bgn()

   文字列データをフォーマッタオブジェクトに送らずにバッファに保存する操作を開始します。保存されたデータは :meth:`save_end` で取得してください。
   :meth:`save_bgn` / :meth:`save_end`  のペアを入れ子構造にすることはできません。


.. method:: HTMLParser.save_end()

   文字列データのバッファリングを終了し、以前 :meth:`save_bgn`  を呼び出した時点から保存されている全てのデータを返します。
   :attr:`nofill` フラグが偽の場合、空白文字は全てスペース文字一文字に置き換えられます。予め :meth:`save_bgn` を呼ばないで
   このメソッドを呼び出すと :exc:`TypeError` 例外が送出されます。


:mod:`htmlentitydefs` --- HTML 一般エンティティの定義
=====================================================

.. module:: htmlentitydefs
   :synopsis: HTML 一般エンティティの定義。
.. sectionauthor:: Fred L. Drake, Jr. <fdrake@acm.org>

.. note::

   Python 3.0 で :mod:`htmlentitydefs` モジュールは :mod:`html.entities`
   と改名されました。
   ソースを 3.0 用に変換する際には :term:`2to3` ツールが自動的に import
   を直してくれます。


このモジュールでは ``entitydefs`` 、 ``codepoint2name`` 、 ``entitydefs`` の三つの辞書を定義しています。
``entitydefs`` は :mod:`htmllib` モジュールで :class:`HTMLParser` クラスの :attr:`entitydefs`
メンバを定義するために使われます。このモジュールでは XHTML 1.0 で定義された全てのエンティティを提供しており、 Latin-1 キャラクタセット
(ISO-8859-1)の簡単なテキスト置換を行う事ができます。


.. data:: entitydefs

   各 XHTML 1.0 エンティティ定義について、ISO Latin-1 における置換テキストへの対応付けを行っている辞書です。


.. data:: name2codepoint

   HTMLのエンティティ名をUnicodeのコードポイントに変換するための辞書です。

   .. versionadded:: 2.3


.. data:: codepoint2name

   UnicodeのコードポイントをHTMLのエンティティ名に変換するための辞書です。

   .. versionadded:: 2.3

