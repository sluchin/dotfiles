:mod:`email`: 文字セットの表現
------------------------------

.. module:: email.charset
   :synopsis: 文字セット


このモジュールは文字セットを表現する :class:`Charset` クラスと電子メールメッセージにふくまれる文字セット間の変換、および
文字セットのレジストリとこのレジストリを操作するためのいくつかの便宜的なメソッドを提供します。
:class:`Charset` インスタンスは
:mod:`email` パッケージ中にあるほかのいくつかのモジュールで使用されます。

このクラスは :mod:`email.charset` モジュールからimportしてください。

.. versionadded:: 2.2.2


.. class:: Charset([input_charset])

   文字セットを email のプロパティに写像する。 Map character sets to their email properties.

   このクラスはある特定の文字セットに対し、電子メールに課される制約の情報を提供します。また、与えられた適用可能な codec
   をつかって、文字セット間の変換をおこなう便宜的なルーチンも提供します。またこれは、ある文字セットが与えられたときに、
   その文字セットを電子メールメッセージのなかでどうやって RFC に準拠したやり方で使用するかに関する、できうるかぎりの情報も提供します。

   文字セットによっては、それらの文字を電子メールのヘッダあるいはメッセージ本体で使う場合は quoted-printable 形式あるいは
   base64形式でエンコードする必要があります。またある文字セットはむきだしのまま変換する必要があり、電子メールの中では使用できません。

   以下ではオプション引数 *input_charset* について説明します。この値はつねに小文字に強制的に変換されます。
   そして文字セットの別名が正規化されたあと、この値は文字セットのレジストリ内を検索し、ヘッダのエンコーディングと
   メッセージ本体のエンコーディング、および出力時の変換に使われる codec をみつけるのに使われます。たとえば *input_charset* が
   ``iso-8859-1`` の場合、ヘッダおよびメッセージ本体は quoted-printable でエンコードされ、出力時の変換用 codec
   は必要ありません。もし *input_charset* が ``euc-jp`` ならば、ヘッダは base64 でエンコードされ、
   メッセージ本体はエンコードされませんが、出力されるテキストは ``euc-jp`` 文字セットから ``iso-2022-jp`` 文字セットに変換されます。

   :class:`Charset` インスタンスは以下のようなデータ属性をもっています:


   .. attribute:: input_charset

      最初に指定される文字セットです。一般に通用している別名は、
      *正式な* 電子メール用の名前に変換されます (たとえば、 ``latin_1`` は
      ``iso-8859-1`` に変換されます)。デフォルトは 7-bit の ``us-ascii`` です。


   .. attribute:: header_encoding

      この文字セットが電子メールヘッダに使われる前にエンコードされる必要がある場合、
      この属性は ``Charset.QP`` (quoted-printable エンコーディング)、
      ``Charset.BASE64`` (base64 エンコーディング)、あるいは\
      最短の QP または BASE64 エンコーディングである ``Charset.SHORTEST`` に\
      設定されます。そうでない場合、この値は ``None`` になります。


   .. attribute:: body_encoding

      *header_encoding* と同じですが、この値はメッセージ本体のための\
      エンコーディングを記述します。これはヘッダ用のエンコーディングとは\
      違うかもしれません。
      *body_encoding* では、 ``Charset.SHORTEST`` を使うことはできません。


   .. attribute:: output_charset

      文字セットによっては、電子メールのヘッダあるいはメッセージ本体に\
      使う前にそれを変換する必要があります。もし *input_charset* が\
      それらの文字セットのどれかをさしていたら、この *output_charset* 属性は\
      それが出力時に変換される文字セットの名前をあらわしています。
      それ以外の場合、この値は ``None`` になります。


   .. attribute:: input_codec

      *input_charset* を Unicode に変換するための Python 用 codec 名です。
      変換用の codec が必要ないときは、この値は ``None`` になります。


   .. attribute:: output_codec

      Unicode を *output_charset* に変換するための Python 用 codec 名です。
      変換用の codec が必要ないときは、この値は ``None`` になります。
      この属性は *input_codec* と同じ値をもつことになるでしょう。

   :class:`Charset` インスタンスは、以下のメソッドも持っています:


   .. method:: get_body_encoding()

      メッセージ本体のエンコードに使われる content-transfer-encoding の値を返します。

      この値は使用しているエンコーディングの文字列 ``quoted-printable`` または
      ``base64`` か、あるいは関数のどちらかです。後者の場合、これはエンコードされる
      Message オブジェクトを単一の引数として取るような関数である必要があります。
      この関数は変換後 :mailheader:`Content-Transfer-Encoding`
      ヘッダ自体を、なんであれ適切な値に設定する必要があります。

      このメソッドは *body_encoding* が ``QP`` の場合 ``quoted-printable``
      を返し、 *body_encoding* が ``BASE64`` の場合 ``base64`` を返します。
      それ以外の場合は文字列 ``7bit`` を返します。


   .. method:: convert(s)

      文字列 *s* を *input_codec* から *output_codec* に変換します。


.. method:: Charset.to_splittable(s)

   おそらくマルチバイトの文字列を、安全に split できる形式に変換します。
   *s* には split する文字列を渡します。

   これは *input_codec* を使って文字列を Unicode にすることで、
   文字と文字の境界で (たとえそれがマルチバイト文字であっても) 安全に
   split できるようにします。

   *input_charset* の文字列 *s* をどうやって Unicode に変換すればいいかが\
   不明な場合、このメソッドは与えられた文字列そのものを返します。

   Unicode に変換できなかった文字は、Unicode 置換文字
   (Unicode replacement character) ``'U+FFFD'``
   に置換されます。


   .. method:: from_splittable(ustr[, to_output])

      split できる文字列をエンコードされた文字列に変換しなおします。
      *ustr* は "逆split" するための Unicode 文字列です。

      このメソッドでは、文字列を Unicode からべつのエンコード形式に変換するために\
      適切な codec を使用します。与えられた文字列が Unicode ではなかった場合、
      あるいはそれをどうやって Unicode から変換するか不明だった場合は、
      与えられた文字列そのものが返されます。

      Unicode から正しく変換できなかった文字については、
      適当な文字 (通常は ``'?'``) に置き換えられます。

      *to_output* が ``True`` の場合 (デフォルト)、
      このメソッドは *output_codec* をエンコードの形式として使用します。
      *to_output* が ``False`` の場合、これは *input_codec* を使用します。


   .. method:: get_output_charset()

      出力用の文字セットを返します。

      これは *output_charset* 属性が ``None`` でなければその値になります。
      それ以外の場合、この値は *input_charset* と同じです。


   .. method:: encoded_header_len()

      エンコードされたヘッダ文字列の長さを返します。
      これは quoted-printable エンコーディングあるいは base64 エンコーディング\
      に対しても正しく計算されます。


   .. method:: header_encode(s[, convert])

      文字列 *s* をヘッダ用にエンコードします。

      *convert* が ``True`` の場合、
      文字列は入力用文字セットから出力用文字セットに自動的に変換されます。
      これは行の長さ問題のあるマルチバイトの文字セットに対しては役に立ちません
      (マルチバイト文字はバイト境界ではなく、文字ごとの境界で split
      する必要があります)。
      これらの問題を扱うには、高水準のクラスである :class:`~email.header.Header` クラスを\
      使ってください (:mod:`email.header` を参照)。
      *convert* の値はデフォルトでは ``False`` です。

      エンコーディングの形式 (base64 または quoted-printable) は、
      *header_encoding* 属性に基づきます。


   .. method:: body_encode(s[, convert])

      文字列 *s* をメッセージ本体用にエンコードします。

      *convert* が ``True`` の場合 (デフォルト)、
      文字列は入力用文字セットから出力用文字セットに自動的に変換されます。
      :meth:`header_encode` とは異なり、メッセージ本体にはふつう\
      バイト境界の問題やマルチバイト文字セットの問題がないので、
      これはきわめて安全におこなえます。

      エンコーディングの形式 (base64 または quoted-printable) は、
      *body_encoding* 属性に基づきます。

   :class:`Charset` クラスには、標準的な演算と組み込み関数をサポートする\
   いくつかのメソッドがあります。


   .. method:: __str__()

      *input_charset* を小文字に変換された文字列型として返します。
      :meth:`__repr__` は、 :meth:`__str__` の別名となっています。


   .. method:: __eq__(other)

      このメソッドは、2つの :class:`Charset` インスタンスが同じかどうかを\
      チェックするのに使います。


   .. method:: __ne__(other)

      このメソッドは、2つの :class:`Charset` インスタンスが異なるかどうかを\
      チェックするのに使います。

また、 :mod:`email.charset` モジュールには、
グローバルな文字セット、文字セットの別名(エイリアス) および codec 用のレジストリに\
新しいエントリを追加する以下の関数もふくまれています:


.. function:: add_charset(charset[, header_enc[, body_enc[, output_charset]]])

   文字の属性をグローバルなレジストリに追加します。

   *charset* は入力用の文字セットで、その文字セットの正式名称を指定する必要があります。

   オプション引数 *header_enc* および *body_enc* は quoted-printable
   エンコーディングをあらわす ``Charset.QP`` か、
   base64 エンコーディングをあらわす ``Charset.BASE64`` 、
   最短の quoted-printable または base64 エンコーディングをあらわす
   ``Charset.SHORTEST`` 、あるいはエンコーディングなしの ``None`` の\
   どれかになります。 ``SHORTEST`` が使えるのは *header_enc* だけです。
   デフォルトの値はエンコーディングなしの ``None`` になっています。

   オプション引数 *output_charset* には出力用の文字セットが入ります。
   :meth:`Charset.convert` が呼ばれたときの変換は\
   まず入力用の文字セットを Unicode に変換し、それから出力用の文字セットに\
   変換されます。デフォルトでは、出力は入力と同じ文字セットになっています。

   *input_charset* および *output_charset* は\
   このモジュール中の文字セット-codec 対応表にある Unicode codec エントリである\
   必要があります。モジュールがまだ対応していない codec を追加するには、
   :func:`add_codec` を使ってください。
   より詳しい情報については :mod:`codecs` モジュールの文書を参照してください。

   グローバルな文字セット用のレジストリは、モジュールの global 辞書 ``CHARSETS``
   内に保持されています。


.. function:: add_alias(alias, canonical)

   文字セットの別名 (エイリアス) を追加します。 *alias* はその別名で、
   たとえば ``latin-1`` のように指定します。 *canonical*
   はその文字セットの正式名称で、たとえば ``iso-8859-1`` のように指定します。

   文字セットのグローバルな別名用レジストリは、モジュールの global 辞書 ``ALIASES``
   内に保持されています。


.. function:: add_codec(charset, codecname)

   与えられた文字セットの文字と Unicode との変換をおこなう codec を追加します。

   *charset* はある文字セットの正式名称で、 *codecname* は Python 用 codec
   の名前です。これは組み込み関数 :func:`unicode` の第2引数か、
   あるいは Unicode 文字列型の :meth:`encode` メソッドに\
   適した形式になっていなければなりません。

