
:mod:`unicodedata` --- Unicode データベース
===========================================

.. module:: unicodedata
   :synopsis: Access the Unicode Database.
.. moduleauthor:: Marc-Andre Lemburg <mal@lemburg.com>
.. sectionauthor:: Marc-Andre Lemburg <mal@lemburg.com>
.. sectionauthor:: Martin v. Löwis <martin@v.loewis.de>


.. index::
   single: Unicode
   single: character
   pair: Unicode; database

このモジュールは、全ての Unicode 文字の属性を定義している Unicode
文字データベースへのアクセスを提供します。このデータベース内のデータは、ftp://ftp.unicode.org/ で公開されている
:file:`UnicodeData.txt` ファイルのバージョン 5.2.0 に基づいています。

このモジュールは、UnicodeData ファイルフォーマット 5.2.0
(http://www.unicode.org/reports/tr44/tr44-4.html を参照)
で定義されているものと、同じ名前と記号を使います。
このモジュールで定義されている関数は、以下のとおりです。


.. function:: lookup(name)

   名前に対応する文字を探します。その名前の文字が見つかった場合、その Unicode
   文字が返されます。見つからなかった場合には、 :exc:`KeyError` を発生させます。


.. function:: name(unichr[, default])

   Unicode 文字 *unichr* に付いている名前を、文字列で返します。名前が定義されていない場合には *default*
   が返されますが、この引数が与えられていなければ :exc:`ValueError` を発生させます。


.. function:: decimal(unichr[, default])

   Unicode 文字 *unichr* に割り当てられている十進数を、整数で返します。この値が定義されていない場合には *default*
   が返されますが、この引数が与えられていなければ :exc:`ValueError` を発生させます。


.. function:: digit(unichr[, default])

   Unicode 文字 *unichr* に割り当てられている二進数を、整数で返します。この値が定義されていない場合には *default*
   が返されますが、この引数が与えられていなければ :exc:`ValueError` を発生させます。


.. function:: numeric(unichr[, default])

   Unicode 文字 *unichr* に割り当てられている数値を、float 型で返します。この値が定義されていない場合には *default*
   が返されますが、この引数が与えられていなければ :exc:`ValueError` を発生させます。


.. function:: category(unichr)

   Unicode 文字 *unichr* に割り当てられた、汎用カテゴリを返します。


.. function:: bidirectional(unichr)

   Unicode 文字 *unichr* に割り当てられた、双方向カテゴリを返します。そのような値が定義されていない場合、空の文字列が返されます。


.. function:: combining(unichr)

   Unicode 文字 *unichr* に割り当てられた正規結合クラスを返します。結合クラス定義されていない場合、 ``0`` が返されます。


.. function:: east_asian_width(unichr)

   *unichr* as string. ユニコード文字 *unichr* に割り当てられたeast asian widthを文字列で返します。

   .. versionadded:: 2.4


.. function:: mirrored(unichr)

   Unicode 文字 *unichr* に割り当てられた、鏡像化のプロパティを返します。
   その文字が双方向テキスト内で鏡像化された文字である場合には
   ``1`` を、それ以外の場合には ``0`` を返します。


.. function:: decomposition(unichr)

   Unicode 文字 *unichr*
   に割り当てられた、文字分解マッピングを、文字列型で返します。そのようなマッピングが定義されていない場合、空の文字列が返されます。


.. function:: normalize(form, unistr)

   Unicode 文字列 *unistr* の正規形 *form* を返します。 *form* の有効な値は、'NFC'、'NFKC'、'NFD'、'NFKD'
   です。

   .. The Unicode standard defines various normalization forms of a Unicode string,
      based on the definition of canonical equivalence and compatibility equivalence.
      In Unicode, several characters can be expressed in various way. For example, the
      character U+00C7 (LATIN CAPITAL LETTER C WITH CEDILLA) can also be expressed as
      the sequence U+0327 (COMBINING CEDILLA) U+0043 (LATIN CAPITAL LETTER C).

   Unicode 規格は標準等価性 (canonical equivalence) と互換等価性 (compatibility equivalence)
   に基づいて、様々な Unicode文字列の正規形を定義します。Unicode では、複数の方法で表現できる文字があります。たとえば、文字 U+00C7
   (LATIN CAPITAL LETTER C WITH CEDILLA) は、U+0327 (COMBINING CEDILLA) U+0043 (LATIN CAPITAL LETTER C)
   というシーケンスとしても表現できます。

   各文字には2つの正規形があり、それぞれ正規形 C と正規形 D といいます。正規形 D (NFD) は標準分解 (canonical
   decomposition) としても知られており、各文字を分解された形に変換します。正規形 C (NFC) は標準分解を適用した後、結合済文字を再構成します。

   .. % For each character, there are two normal forms: normal form C and
   .. % normal form D. Normal form D (NFD) is also known as canonical
   .. % decomposition, and translates each character into its decomposed form.
   .. % Normal form C (NFC) first applies a canonical decomposition, then
   .. % composes pre-combined characters again.

   互換等価性に基づいて、2つの正規形が加えられています。Unicode では、一般に他の文字との統合がサポートされている文字があります。たとえば、U+2160
   (ROMAN NUMERAL ONE) は事実上 U+0049 (LATIN CAPITAL LETTER I) と同じものです。しかし、Unicode
   では、既存の文字集合 (たとえば gb2312) との互換性のために、これがサポートされています。

   .. % In addition to these two forms, there two additional normal forms
   .. % based on compatibility equivalence. In Unicode, certain characters are
   .. % supported which normally would be unified with other characters. For
   .. % example, U+2160 (ROMAN NUMERAL ONE) is really the same thing as U+0049
   .. % (LATIN CAPITAL LETTER I). However, it is supported in Unicode for
   .. % compatibility with existing character sets (e.g. gb2312).

   正規形 KD (NFKD) は、互換分解 (compatibility decomposition)
   を適用します。すなわち、すべての互換文字を、等価な文字で置換します。正規形 KC (NFKC) は、互換分解を適用してから、標準分解を適用します。

   .. % The normal form KD (NFKD) will apply the compatibility decomposition,
   .. % i.e. replace all compatibility characters with their equivalents. The
   .. % normal form KC (NFKC) first applies the compatibility decomposition,
   .. % followed by the canonical composition.

   .. Even if two unicode strings are normalized and look the same to
      a human reader, if one has combining characters and the other
      doesn't, they may not compare equal.

   2つのunicode文字列が正規化されていて人間の目に同じに見えても、
   片方が結合文字を持っていてもう片方が持っていない場合、それらは完全に同じではありません。

   .. versionadded:: 2.3

更に、本モジュールは以下の定数を公開します。


.. data:: unidata_version

   このモジュールで使われている Unicode データベースのバージョン。

   .. versionadded:: 2.3


.. data:: ucd_3_2_0

   これはモジュール全体と同じメソッドを具えたオブジェクトですが、Unicode データベースバージョン 3.2 を代わりに使っており、この特定のバージョンの
   Unicode データベースを必要とするアプリケーション(IDNA など)のためものです。

   .. versionadded:: 2.5

例:

   >>> import unicodedata
   >>> unicodedata.lookup('LEFT CURLY BRACKET')
   u'{'
   >>> unicodedata.name(u'/')
   'SOLIDUS'
   >>> unicodedata.decimal(u'9')
   9
   >>> unicodedata.decimal(u'a')
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
   ValueError: not a decimal
   >>> unicodedata.category(u'A')  # 'L'etter, 'u'ppercase
   'Lu'
   >>> unicodedata.bidirectional(u'\u0660') # 'A'rabic, 'N'umber
   'AN'

