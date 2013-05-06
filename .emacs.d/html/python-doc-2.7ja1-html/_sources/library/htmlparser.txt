
:mod:`HTMLParser` --- HTML および XHTML のシンプルなパーザ
==========================================================

.. module:: HTMLParser
   :synopsis: HTML と XHTML を扱えるシンプルなパーザ。

.. note::
    :mod:`HTMLParser` モジュールは Python 3.0 で :mod:`html.parser`
    に改名されました。
    ソースを 3.0 用に移行する際には :term:`2to3` がインポートを自動的に直してくれます。

.. versionadded:: 2.2

.. index::
   single: HTML
   single: XHTML

このモジュールでは :class:`HTMLParser` クラスを定義します。
このクラスは HTML  (ハイパーテキスト記述言語、 HyperText Mark-up Language)
および XHTML で書式化されているテキストファイルを解釈するための基礎となります。
:mod:`htmllib` にあるパーザと違って、このパーザは :mod:`sgmllib` の SGML
パーザに基づいてはいません。


.. class:: HTMLParser()

   :class:`HTMLParser` クラスは引数なしでインスタンス化します。

   :class:`HTMLParser` インスタンスに HTML データが入力されると、
   タグが開始したとき、及び終了したときに関数を呼び出します。
   :class:`HTMLParser` クラスは、ユーザが行いたい動作を提供する\
   ために上書きできるようになっています。

   :mod:`htmllib` のパーザと違い、このパーザは終了タグが開始タグと\
   一致しているか調べたり、外側のタグ要素が閉じるときに内側で明示的\
   に閉じられていないタグ要素のタグ終了ハンドラを呼び出したりはしません。

例外も定義されています:


.. exception:: HTMLParseError

   パーズ中にエラーに遭遇した場合に :class:`HTMLParser` クラスが送出する例外です。
   この例外は三つの属性を提供しています:
   :attr:`msg` はエラーの内容を説明する簡単なメッセージ、
   :attr:`lineno` は壊れたマークアップ構造を検出した場所の行番号、
   :attr:`offset` は問題のマークアップ構造の行内での開始位置を示す文字数です。

:class:`HTMLParser` インスタンスは以下のメソッドを提供します:


.. method:: HTMLParser.reset()

   インスタンスをリセットします。
   未処理のデータは全て失われます。
   インスタンス化の際に非明示的に呼び出されます。


.. method:: HTMLParser.feed(data)

   パーザにテキストを入力します。
   入力が完全なタグ要素で構成されている場合に限り処理が行われます;
   不完全なデータであった場合、新たにデータが入力されるか、
   :meth:`close` が呼び出されるまでバッファされます。


.. method:: HTMLParser.close()

   全てのバッファされているデータについて、その後にファイル終了マーク\
   が続いているとみなして強制的に処理を行います。このメソッドは\
   入力データの終端で行うべき追加処理を定義するために派生クラスで\
   上書きすることができますが、再定義を行ったクラスでは常に、
   :class:`HTMLParser` 基底クラスのメソッド :meth:`close`
   を呼び出さなくてはなりません。


.. method:: HTMLParser.getpos()

   現在の行番号およびオフセット値を返します。


.. method:: HTMLParser.get_starttag_text()

   最も最近開かれた開始タグのテキスト部分を返します。
   このテキストは必ずしも元データを構造化する上で必須ではありませんが、
   "広く知られている (as deployed)" HTML を扱ったり、
   入力を最小限の変更で再生成 (属性間の空白をそのままにする、など)
   したりする場合に便利なことがあります。


.. method:: HTMLParser.handle_starttag(tag, attrs)

   このメソッドはタグの開始部分を処理するために呼び出されます。
   派生クラスで上書きするためのメソッドです;
   基底クラスの実装では何も行いません。

   *tag* 引数はタグの名前で、小文字に変換されています。
   *attrs* 引数は ``(name, value)`` のペアからなるリストで、タグの
   ``<>`` 括弧内にある属性が収められています。
   *name* は小文字に変換され、
   *value* 内の引用符は取り除かれ、文字参照とエンティティ参照は置換されます。
   例えば、タグ ``<A HREF="http://www.cwi.nl/">``
   を処理する場合、このメソッドは
   ``handle_starttag('a', [('href', 'http://www.cwi.nl/')])``
   として呼び出されます。

   .. versionchanged:: 2.6
      属性中の :mod:`htmlentitydefs` の全てのエンティティ参照が\
      置換されるようになりました。


.. method:: HTMLParser.handle_startendtag(tag, attrs)

   :meth:`handle_starttag` と似ていますが、パーザが XHTML 形式の空タグ
   (``<a .../>``) に遭遇した場合に呼び出されます。
   この特定の語彙情報 (lexical information) が必要な場合、
   このメソッドをサブクラスで上書きすることができます;
   標準の実装では、単に :meth:`handle_starttag` および
   :meth:`handle_endtag` を呼ぶだけです。


.. method:: HTMLParser.handle_endtag(tag)

   このメソッドはあるタグ要素の終了タグを処理するために呼び出されます。
   派生クラスで上書きするためのメソッドです; 基底クラスの実装では何も行いません。
   *tag* 引数はタグの名前で、小文字に変換されています。


.. method:: HTMLParser.handle_data(data)

   このメソッドは、他のメソッドに当てはまらない任意のデータを処理するために\
   呼び出されます。
   派生クラスで上書きするためのメソッドです; 基底クラスの実装では何も行いません。


.. method:: HTMLParser.handle_charref(ref)

   このメソッドはタグ外の ``&#ref;`` 形式の文字参照 (character reference)
   を処理するために呼び出されます。
   *ref* には、先頭の ``&#`` および末尾の ``;`` は含まれません。
   派生クラスで上書きするためのメソッドです; 基底クラスの実装では何も行いません。


.. method:: HTMLParser.handle_entityref(name)

   このメソッドはタグ外の ``&name;`` 形式の一般のエンティティ参照
   (entity reference) *name* を処理するために呼び出されます。
   *name* には、先頭の ``&`` および末尾の ``;`` は含まれません。
   派生クラスで上書きするためのメソッドです; 基底クラスの実装では何も行いません。


.. method:: HTMLParser.handle_comment(data)

   このメソッドはコメントに遭遇した場合に呼び出されます。
   *comment* 引数は文字列で、 ``--`` および ``--`` デリミタ間の、
   デリミタ自体を除いたテキストが収められています。
   例えば、コメント ``<!--text-->`` があると、このメソッドは引数
   ``'text'`` で呼び出されます。
   派生クラスで上書きするためのメソッドです;  基底クラスの実装では何も行いません。


.. method:: HTMLParser.handle_decl(decl)

   パーザが SGML の ``doctype`` 宣言を読み出した際に呼び出されるメソッドです。
   *decl* パラメタは ``<!...>`` 記述内の宣言内容全体になります。
   派生クラスで上書きするためのメソッドです; 基底クラスの実装では何も行いません。


.. method:: HTMLParser.unknown_decl(data)

   パーザが認識できない SGML 宣言を読み出した際に呼び出されるメソッドです。
   *data* パラメタは ``<!...>`` 記述内の宣言内容全体になります。
   派生クラスで上書きすると良いかもしれません。
   基底クラスの実装では :exc:`HTMLParseError` を送出します。


.. method:: HTMLParser.handle_pi(data)

   処理指令に遭遇した場合に呼び出されます。
   *data* には、処理指令全体が含まれ、例えば ``<?proc color='red'>``
   という処理指令の場合、 ``handle_pi("proc color='red'")`` のように呼び出されます。
   このメソッドは派生クラスで上書きするためのメソッドです;
   基底クラスの実装では何も行いません。

   .. note::

      The :class:`HTMLParser` クラスでは、処理指令に SGML の構文を使用します。
      末尾に ``'?'`` がある XHTML の処理指令では、 ``'?'`` が *data*
      に含まれることになります。


.. exception:: HTMLParseError

   HTML の構文に沿わないパターンを発見したときに送出される例外です。
   HTML 構文法上の全てのエラーを発見できるわけではないので注意してください。


.. _htmlparser-example:

HTML パーザアプリケーションの例
-------------------------------

基礎的な例として、 :class:`HTMLParser` クラスを使い、
発見したタグを出力する、非常に基礎的な HTML パーザを以下に示します。 ::

   from HTMLParser import HTMLParser

   class MyHTMLParser(HTMLParser):

       def handle_starttag(self, tag, attrs):
           print "Encountered the beginning of a %s tag" % tag

       def handle_endtag(self, tag):
           print "Encountered the end of a %s tag" % tag

