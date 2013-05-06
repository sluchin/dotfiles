
:mod:`sgmllib` --- 単純な SGML パーザ
=====================================

.. module:: sgmllib
   :synopsis: HTML を解析するのに必要な機能だけを備えた SGML パーザ。
   :deprecated:

.. deprecated:: 2.6
    :mod:`sgmllib` は Python 3.0 で削除されました。


.. index:: single: SGML

このモジュールでは SGML (Standard Generalized Mark-up Language: 汎用マークアップ言語標準)
で書式化されたテキストファイルを解析するための基礎として働く :class:`SGMLParser` クラスを定義しています。実際には、このクラスは完全な
SGML パーザを提供しているわけではありません --- このクラスは HTML で用いられているような SGML だけを解析し、モジュール自体も
:mod:`htmllib` モジュールの基礎にするためだけに存在しています。XHTML をサポートし、少し異なったインタフェースを提供しているもう一つの
HTML パーザは、 :mod:`HTMLParser` モジュールで使うことができます。


.. class:: SGMLParser()

   :class:`SGMLParser` クラスは引数無しでインスタンス化されます。このパーザは以下の構成を認識するようにハードコードされています:

   * ``<tag attr="value" ...>`` と ``</tag>`` で表されるタグの開始部と終了部。

   * ``&#name;`` 形式をとる文字の数値参照。

   * ``&name;`` 形式をとるエンティティ参照。

   * ``<!--text-->`` 形式をとる SGML コメント。末尾の ``>`` とその直前にある
     ``--`` の間にはスペース、タブ、改行を入れることができます。

例外が以下のように定義されます:


.. exception:: SGMLParseError

   :class:`SGMLParser` クラスで構文解析中にエラーに出逢うとこの例外が発生します。

   .. versionadded:: 2.1

:class:`SGMLParser` インスタンスは以下のメソッドを持っています:


.. method:: SGMLParser.reset()

   インスタンスをリセットします。未処理のデータは全て失われます。このメソッドはインスタンス生成時に非明示的に呼び出されます。


.. method:: SGMLParser.setnomoretags()

   タグの処理を停止します。以降の入力をリテラル入力 (CDATA)  として扱います。(この機能は HTML タグ ``<PAINTEXT>`` を実装
   できるようにするためだけに提供されています)


.. method:: SGMLParser.setliteral()

   リテラルモード (CDATA モード) に移行します。


.. method:: SGMLParser.feed(data)

   テキストをパーザに入力します。入力は完全なエレメントから成り立つ場合に限り処理されます; 不完全なデータは追加のデータが入力されるか、
   :meth:`close` が呼び出されるまでバッファに蓄積されます。


.. method:: SGMLParser.close()

   バッファに蓄積されている全てのデータについて、直後にファイル終了記号が来た時のようにして強制的に処理します。このメソッドは派生クラスで
   再定義して、入力の終了時に追加の処理行うよう定義することができますが、このメソッドの再定義されたバージョンでは常に :meth:`close`
   を呼び出さなければなりません。


.. method:: SGMLParser.get_starttag_text()

   もっとも最近開かれた開始タグのテキストを返します。通常、構造化されたデータの処理をする上でこのメソッドは必要ありませんが、 "広く知られている (as
   deployed)" HTML を扱ったり、入力を最小限の変更で再生成 (属性間の空白をそのままにする、など) したりする場合に便利なことがあります。


.. method:: SGMLParser.handle_starttag(tag, method, attributes)

   このメソッドは :meth:`start_tag` か :meth:`do_tag` のどちらかのメソッドが定義されている開始タグを処理するために呼び出され
   ます。 *tag* 引数はタグの名前で、小文字に変換されています。 *method* 引数は開始タグの意味解釈をサポートするために用いられる
   バインドされたメソッドです。 *attributes* 引数は ``(name, value)`` のペアからなるリストで、タグの ``<>``
   括弧内にある属性が収められています。

   *name* は小文字に変換されます。 *value* 内の二重引用符とバックスラッシュも変換され、
   と同時に知られている文字参照および知られているエンティティ参照でセミコロンで終端されているものも変換されます(通常、エンティティ参照は任意の非英数文字
   で終端されてよいのですが、これを許すと非常に一般的な ``<A HREF="url?spam=1&eggs=2">`` 　において ``eggs`` が
   正当なエンティティ参照であるようなケースを破綻させます)。

   例えば、タグ  ``<A HREF="http://www.cwi.nl/">`` を処理する場合、このメソッドは
   ``unknown_starttag('a', [('href', 'http://www.cwi.nl/')])``
   として呼び出されます。基底クラスの実装では、単に *method*  を単一の引数 *attributes* と共に呼び出します。

   .. versionadded:: 2.5
      属性値中のエンティティおよび文字参照の扱い.


.. method:: SGMLParser.handle_endtag(tag, method)

   このメソッドは :meth:`end_tag` メソッドの定義されている終了タグを処理するために呼び出されます。 *tag*
   引数はタグの名前で、小文字に変換されており、 *method* 引数は終了タグの意味解釈をサポートするために使われる
   バインドされたメソッドです。 :meth:`end_tag` メソッドが終了エレメントとして定義されていない場合、ハンドラは一切呼び出され
   ません。基底クラスの実装では単に *method* を呼び出します。


.. method:: SGMLParser.handle_data(data)

   このメソッドは何らかのデータを処理するために呼び出されます。派生クラスで上書きするためのメソッドです; 基底クラスの実装では何も行いません。


.. method:: SGMLParser.handle_charref(ref)

   このメソッドは ``&#ref;`` 形式の文字参照 (character reference) を処理するために呼び出されます。
   基底クラスの実装は、 :meth:`convert_charref` を使って参照を文字列に変換します。もしそのメソッドが文字列を返せば
   :meth:`handle_data` を呼び出します。そうでなければ、エラーを処理するために ``unknown_charref(ref)``
   が呼び出されます。

   .. versionchanged:: 2.5
      ハードコードされた変換に代わり :meth:`convert_charref` を使います.


.. method:: SGMLParser.convert_charref(ref)

   文字参照を文字列に変換するか、 ``None`` を返します。 *ref* は文字列として渡される参照です。基底クラスでは *ref* は 0-255
   の範囲の十進数でなければなりません。そしてコードポイントをメソッド :meth:`convert_codepoint`  を使って変換します。もし *ref*
   が不正もしくは範囲外ならば、 ``None`` を返します。このメソッドはデフォルト実装の :meth:`handle_charref`
   から、あるいは属性値パーザから呼び出されます。

   .. versionadded:: 2.5


.. method:: SGMLParser.convert_codepoint(codepoint)

   コードポイントを :class:`str` の値に変換します。もしそれが適切ならばエンコーディングをここで扱うこともできますが、 :mod:`sgmllib`
   の残りの部分はこの問題に関知しません。

   .. versionadded:: 2.5


.. method:: SGMLParser.handle_entityref(ref)

   このメソッドは *ref* を一般エンティティ参照として、 ``&ref;`` 形式のエンティティ参照を処理するために呼び出されます。
   このメソッドは、 *ref* を :meth:`convert_entityref` に渡して変換します。変換結果が返された場合、変換された文字を引数にして
   :meth:`handle_data` を呼び出します; そうでない場合、 ``unknown_entityref(ref)`` を呼び出します。標準では
   :attr:`entitydefs` は ``&amp;`` 、 ``&apos`` 、 ``&gt;`` 、 ``&lt;`` 、および ``&quot;``
   の変換を定義しています。

   .. versionchanged:: 2.5
      ハードコードされた変換に代わり :meth:`convert_entityref` を使います.


.. method:: SGMLParser.convert_entityref(ref)

   名前付きエンティティ参照を :class:`str` の値に変換するか、または ``None`` を返します。変換結果は再パーズしません。 *ref*
   はエンティティの名前部分だけです。デフォルトの実装ではインスタンス(またはクラス)変数の :attr:`entitydefs`
   というエンティティ名から対応する文字列へのマッピングから *ref* を探します。もし *ref* に対応する文字列が見つからなければメソッドは
   ``None`` を返します。このメソッドは :meth:`handle_entityref`  のデフォルト実装からおよび属性値パーザから呼び出されます。

   .. versionadded:: 2.5


.. method:: SGMLParser.handle_comment(comment)

   このメソッドはコメントに遭遇した場合に呼び出されます。 *comment* 引数は文字列で、 ``<!--`` and ``-->`` デリミタ間の、
   デリミタ自体を除いたテキストが収められています。例えば、コメント ``<!--text-->`` があると、このメソッドは引数  ``'text'``
   で呼び出されます。基底クラスの実装では何も行いません。


.. method:: SGMLParser.handle_decl(data)

   パーザが SGML 宣言を読み出した際に呼び出されるメソッドです。実際には、 ``DOCTYPE`` は HTML だけに見られる宣言ですが、
   パーザは宣言間の相違 (や誤った宣言) を判別しません。 ``DOCTYPE`` の内部サブセット宣言はサポートされていません。 *decl* パラメタは
   ``<!``...\ ``>`` 記述内の宣言内容全体になります。基底クラスの実装では何も行いません。


.. method:: SGMLParser.report_unbalanced(tag)

   個のメソッドは対応する開始エレメントのない終了タグが発見された時に呼び出されます。


.. method:: SGMLParser.unknown_starttag(tag, attributes)

   未知の開始タグを処理するために呼び出されるメソッドです。派生クラスで上書きするためのメソッドです; 基底クラスの実装では何も行いません。


.. method:: SGMLParser.unknown_endtag(tag)

   This method is called to process an unknown end tag.
   未知の終了タグを処理するために呼び出されるメソッドです。派生クラスで上書きするためのメソッドです; 基底クラスの実装では何も行いません。


.. method:: SGMLParser.unknown_charref(ref)

   このメソッドは解決不能な文字参照数値を処理するために呼び出されます。標準で何が処理可能かは :meth:`handle_charref` を参照
   してください。派生クラスで上書きするためのメソッドです; 基底クラスの実装では何も行いません。


.. method:: SGMLParser.unknown_entityref(ref)

   未知のエンティティ参照を処理するために呼び出されるメソッドです。派生クラスで上書きするためのメソッドです; 基底クラスの実装では何も行いません。

上に挙げたメソッドを上書きしたり拡張したりするのとは別に、派生クラスでは以下の形式のメソッドを定義して、特定のタグを処理する
こともできます。入力ストリーム中のタグ名は大小文字の区別に依存しません; メソッド名中の *tag* は小文字でなければなりません:


.. method:: SGMLParser.start_tag(attributes)
   :noindex:

   このメソッドは開始タグ *tag* を処理するために呼び出されます。 :meth:`do_tag` よりも高い優先順位があります。 *attributes*
   引数は上の :meth:`handle_starttag` で記述されているのと同じ意味です。


.. method:: SGMLParser.do_tag(attributes)
   :noindex:

   このメソッドは :meth:`start_tag` メソッドが定義されていない開始タグ *tag* を処理するために呼び出されます。 *attributes*
   引数は上の :meth:`handle_starttag` で記述されているのと同じ意味です。


.. method:: SGMLParser.end_tag()
   :noindex:

   このメソッドは終了タグ *tag* を処理するために呼び出されます。

パーザは開始されたエレメントのうち、終了タグがまだ見つかっていないもののスタックを維持しているので注意してください。 :meth:`start_tag`
で処理されたタグだけがスタックにプッシュされます。are pushed on this stack.  Definition of an それらのタグに対する
:meth:`end_tag` メソッドの定義はオプションです。 :meth:`do_tag` や :meth:`unknown_tag`
で処理されるタグについては、 :meth:`end_tag` を定義してはいけません; 定義されていても使われることはありません。あるタグに対して
:meth:`start_tag` および :meth:`do_tag`  メソッドの両方が存在する場合、 :meth:`start_tag` が優先されます。

