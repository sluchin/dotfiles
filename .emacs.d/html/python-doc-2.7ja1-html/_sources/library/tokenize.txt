
:mod:`tokenize` --- Pythonソースのためのトークナイザ
====================================================

.. module:: tokenize
   :synopsis: Pythonソースコードのための字句解析器。
.. moduleauthor:: Ka Ping Yee
.. sectionauthor:: Fred L. Drake, Jr. <fdrake@acm.org>


:mod:`tokenize` モジュールでは、Python で実装された Python ソースコードの字句解析器を提供します。
さらに、このモジュールの字句解析器はコメントもトークンとして返します。
このため、このモジュールはスクリーン上で表示する際の色付け機能 (colorizers) を含む
"清書出力器 (pretty-printer)" を実装する上で便利です。

.. seealso::

   最新バージョンの `tokenize module Python ソースコード
   <http://svn.python.org/view/python/branches/release27-maint/Lib/tokenize.py?view=markup>`_

第一のエントリポイントはジェネレータ(:term:`generator`)です:

.. function:: generate_tokens(readline)

   :func:`generate_tokens` ジェネレータは一つの引数 *readline*
   を必要とします。この引数は呼び出し可能オブジェクトで、組み込みファイルオブジェクトに\
   おける :meth:`readline` メソッドと同じインタフェース\
   を提供していなければなりません ( :ref:`bltin-file-objects` 節を参照して\
   ください)。この関数は呼び出しのたびに入力内の一行を文字列で返さなければなりません。

   ジェネレータは 5 要素のタプルを返し、タプルは以下のメンバ: トークン型; トークン文字列; ソースコード中でトークンが始まる行と列を示す\
   整数の2要素のタプル ``(srow, scol)`` ; ソースコード中でトークンが終わる行と列を示す整数の2要素のタプル  ``(srow, scol)`` ;
   そして、トークンが見つかった行、からなります。渡される行は *論理* 行です; 連続する行は一行に含められます。

   .. versionadded:: 2.2

後方互換性のために古いエントリポイントが残されています:


.. function:: tokenize(readline[, tokeneater])

   :func:`tokenize` 関数は二つのパラメータを取ります:
   一つは入力ストリームを表し、もう一つは :func:`tokenize` のための\
   出力メカニズムを与えます。

   最初のパラメータ、 *readline* は、組み込みファイルオブジェクトの
   :meth:`readline` メソッドと同じインタフェイスを提供する呼び出し\
   可能オブジェクトでなければなりません ( :ref:`bltin-file-objects` 節を参照)。
   この関数は呼び出しのたびに入力内の一行を文字列で返さなければなりません。
   もしくは、 *readline* を呼び出し可能オブジェクトで
   :exc:`StopIteration` を送出することで補完を知らせるものとすることもできます。

   .. versionchanged:: 2.5
      :exc:`StopIteration` サポートの追加.

   二番目のパラメータ *tokeneater* も呼び出し可能オブジェクトでなければなりません。
   この関数は各トークンに対して一度だけ呼び出され、
   :func:`generate_tokens` が生成するタプルに対応する 5 つの引数をとります。

:mod:`token` モジュールの全ての定数は :mod:`tokenize` でも公開されており、これに加え、以下の二つのトークン値が
:func:`tokenize` の *tokeneater* 関数に渡される可能性があります:


.. data:: COMMENT

   コメントであることを表すために使われるトークン値です。


.. data:: NL

   終わりではない改行を表すために使われるトークン値。
   NEWLINE トークンは Pythonコードの論理行の終わりを表します。
   NLトークンはコードの論理行が複数の物理行にわたって続いているときに作られます。

もう一つの関数がトークン化プロセスを逆転するために提供されています。
これは、スクリプトを字句解析し、トークンのストリームに変更を加え、変更された\
スクリプトを書き戻すようなツールを作成する際に便利です。


.. function:: untokenize(iterable)

   トークンの列を Python ソースコードに変換します。 *iterable* は少なくとも\
   二つの要素、トークン型およびトークン文字列、からなるシーケンスを返します。
   その他のシーケンスの要素は無視されます。

   再構築されたスクリプトは一つの文字列として返されます。
   得られる結果はもう一度字句解析すると入力と一致することが保証されるので、
   変換がロスレスでありラウンドトリップできることは間違いありません。
   この保証はトークン型およびトークン文字列に対してのものでトークン間のスペース
   (コラム位置)のようなものは変わることがあり得ます。

   .. versionadded:: 2.5

スクリプト書き換えの例で、浮動小数点数リテラルを Decimal オブジェクトに変換します::

   def decistmt(s):
       """Substitute Decimals for floats in a string of statements.

       >>> from decimal import Decimal
       >>> s = 'print +21.3e-5*-.1234/81.7'
       >>> decistmt(s)
       "print +Decimal ('21.3e-5')*-Decimal ('.1234')/Decimal ('81.7')"

       >>> exec(s)
       -3.21716034272e-007
       >>> exec(decistmt(s))
       -3.217160342717258261933904529E-7

       """
       result = []
       g = generate_tokens(StringIO(s).readline)   # tokenize the string
       for toknum, tokval, _, _, _  in g:
           if toknum == NUMBER and '.' in tokval:  # replace NUMBER tokens
               result.extend([
                   (NAME, 'Decimal'),
                   (OP, '('),
                   (STRING, repr(tokval)),
                   (OP, ')')
               ])
           else:
               result.append((toknum, tokval))
       return untokenize(result)

