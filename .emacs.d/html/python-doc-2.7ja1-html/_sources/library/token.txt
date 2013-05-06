
:mod:`token` --- Python 解析木と共に使われる定数
================================================

.. module:: token
   :synopsis: 解析木の終端ノードを表す定数。
.. sectionauthor:: Fred L. Drake, Jr. <fdrake@acm.org>


このモジュールは解析木の葉ノード(終端記号)の数値を表す定数を提供します。
言語の文法のコンテキストにおける名前の定義については、
Python ディストリビューションのファイル :file:`Grammar/Grammar` を参照してください。
名前がマップする特定の数値は Python のバージョン間で変わります。

このモジュールは、数値コードから名前へのマッピングと、いくつかの関数も提供しています。
関数は Python の C ヘッダファイルの定義を反映します。


.. data:: tok_name

   辞書はこのモジュールで定義されている定数の数値を名前の文字列へマップし、
   より人が読みやすいように解析木を表現します。


.. function:: ISTERMINAL(x)

   終端トークンの値に対して真を返します。


.. function:: ISNONTERMINAL(x)

   非終端トークンの値に対して真を返します。


.. function:: ISEOF(x)

   *x* が入力の終わりを示すマーカーならば、真を返します。


token の定数一覧:

.. data:: ENDMARKER
          NAME
          NUMBER
          STRING
          NEWLINE
          INDENT
          DEDENT
          LPAR
          RPAR
          LSQB
          RSQB
          COLON
          COMMA
          SEMI
          PLUS
          MINUS
          STAR
          SLASH
          VBAR
          AMPER
          LESS
          GREATER
          EQUAL
          DOT
          PERCENT
          BACKQUOTE
          LBRACE
          RBRACE
          EQEQUAL
          NOTEQUAL
          LESSEQUAL
          GREATEREQUAL
          TILDE
          CIRCUMFLEX
          LEFTSHIFT
          RIGHTSHIFT
          DOUBLESTAR
          PLUSEQUAL
          MINEQUAL
          STAREQUAL
          SLASHEQUAL
          PERCENTEQUAL
          AMPEREQUAL
          VBAREQUAL
          CIRCUMFLEXEQUAL
          LEFTSHIFTEQUAL
          RIGHTSHIFTEQUAL
          DOUBLESTAREQUAL
          DOUBLESLASH
          DOUBLESLASHEQUAL
          AT
          OP
          ERRORTOKEN
          N_TOKENS
          NT_OFFSET


.. seealso::

   :mod:`parser` モジュール
      :mod:`parser` モジュールの二番目の例で、
      :mod:`symbol` モジュールの使い方を示しています。

