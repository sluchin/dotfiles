
:mod:`dis` --- Python バイトコードの逆アセンブラ
================================================

.. module:: dis
   :synopsis: Python バイトコードの逆アセンブラ。


:mod:`dis` モジュールは CPython バイトコード (:term:`bytecode`) を逆アセンブルすることでバイトコードの解析をサポートします。
このモジュールが入力として受け取る CPython バイトコードはファイル :file:`Include/opcode.h` に定義されており、
コンパイラとインタプリタが使用しています。

.. seealso::

   Latest version of the `dis module Python source code
   <http://svn.python.org/view/python/branches/release27-maint/Lib/dis.py?view=markup>`_


.. impl-detail::

   バイトコードは CPython インタプリタの実装詳細です! Python のバージョン
   間でバイトコードの追加や、削除、変更がないという保証はありません。この
   モジュールを使用することによって Python の異なる VM または異なるリリース
   の間で動作すると考えるべきではありません。


例: 以下の関数 :func:`myfunc` を考えると


::

   def myfunc(alist):
       return len(alist)


:func:`myfunc` の逆アセンブル結果を得るために次のコマンドを使うことができます


::

   >>> dis.dis(myfunc)
     2           0 LOAD_GLOBAL              0 (len)
                 3 LOAD_FAST                0 (alist)
                 6 CALL_FUNCTION            1
                 9 RETURN_VALUE


("2"は行番号です)。


:mod:`dis` モジュールは次の関数と定数を定義します:


.. function:: dis([bytesource])

   *bytesource* オブジェクトを逆アセンブルします。
   *bytesource* はモジュール、クラス、関数、あるいはコードオブジェクトのいずれかを示します。
   モジュールに対しては、すべての関数を逆アセンブルします。クラスに対しては、すべてのメソッドを逆アセンブルします。
   単一のコード列に対しては、バイトコード命令ごとに 1 行を出力します。
   オブジェクトが与えられない場合は、最後のトレースバックを逆アセンブルします。


.. function:: distb([tb])

   トレースバックのスタックの先頭の関数を逆アセンブルします。
   Noneが渡された場合は最後のトレースバックを使います。例外を引き起こした命令が表示されます。


.. function:: disassemble(code[, lasti])

   コードオブジェクトを逆アセンブルします。
   *lasti* が与えられた場合は、最後の命令を示します。出力は次のようなカラムに分割されます:


   #. 各行の最初の命令に対する行番号。
   #. 現在の命令。 ``-->`` として示されます。
   #. ラベル付けされた命令。 ``>>`` とともに表示されます。
   #. 命令のアドレス。
   #. 命令コード名。
   #. 命令パラメタ。
   #. パラメタの解釈を括弧で囲んだもの。

   パラメタの解釈は、ローカル変数とグローバル変数の名前、定数の値、
   分岐先、比較命令を認識します。


.. function:: disco(code[, lasti])

   :func:`disassemble` の別名。よりタイプしやすく、以前の
   Python リリースと互換性があります。


.. function:: findlinestarts(code)

   このジェネレータ関数は、コードオブジェクト *code* の ``co_firstlineno`` と
   ``co_lnotab`` 属性を使い、ソースコード内の行が始まる場所であるオフセットを
   求めます。これらは ``(offset, lineno)`` の対として生成されます。


.. function:: findlabels(code)

   ジャンプ先であるコードオブジェクト *code* のすべてのオフセットを
   求め、これらのオフセットのリストを返します。


.. data:: opname

   命令コード名のリスト。バイトコードをインデクスに使って参照できます。


.. data:: opmap

   命令コード名をバイトコードに対応づける辞書。


.. data:: cmp_op

   すべての比較命令の名前のリスト。


.. data:: hasconst

   定数パラメタを持つバイトコードのリスト。


.. data:: hasfree

   自由変数にアクセスするバイトコードのリスト。


.. data:: hasname

   名前によって属性にアクセスするバイトコードのリスト。


.. data:: hasjrel

   相対ジャンプ先を持つバイトコードのリスト。


.. data:: hasjabs

   絶対ジャンプ先を持つバイトコードのリスト。


.. data:: haslocal

   ローカル変数にアクセスするバイトコードのリスト。


.. data:: hascompare

   ブール命令のバイトコードのリスト。


.. _bytecodes:

Python バイトコード命令
-----------------------

現在 Python コンパイラは次のバイトコード命令を生成します。


.. opcode:: STOP_CODE ()

   コンパイラにコードの終わりを知らせます。インタプリタでは使われません。


.. opcode:: NOP ()

   なにもしないコード。バイトコードオプティマイザでプレースホルダとして使われます。


.. opcode:: POP_TOP ()

   スタックの先頭 (TOS) の要素を取り除きます。


.. opcode:: ROT_TWO ()

   スタックの先頭の 2 つの要素を入れ替えます。


.. opcode:: ROT_THREE ()

   スタックの二番目と三番目の要素の位置を 1 つ上げ、先頭を三番目へ下げます。


.. opcode:: ROT_FOUR ()

   スタックの二番目、三番目および四番目の位置を 1 つ上げ、先頭を四番目に下げます。


.. opcode:: DUP_TOP ()

   スタックの先頭にある参照の複製を作ります。


単項命令はスタックの先頭を取り出して操作を適用し、結果をスタックへプッシュし戻します。


.. opcode:: UNARY_POSITIVE ()

   ``TOS = +TOS`` に対応します。


.. opcode:: UNARY_NEGATIVE ()

   ``TOS = -TOS`` に対応します。


.. opcode:: UNARY_NOT ()

   ``TOS = not TOS`` に対応します。


.. opcode:: UNARY_CONVERT ()

   ``TOS = `TOS``` に対応します。


.. opcode:: UNARY_INVERT ()

   ``TOS = ~TOS`` に対応します。


.. opcode:: GET_ITER ()

   ``TOS = iter(TOS)`` に対応します。


二項命令はスタックの先頭 (TOS) と先頭から二番目の要素をスタックから取り除きます。
命令を実行し、スタックへ結果をプッシュし戻します。


.. opcode:: BINARY_POWER ()

   ``TOS = TOS1 ** TOS`` に対応します。


.. opcode:: BINARY_MULTIPLY ()

   ``TOS = TOS1 * TOS`` に対応します。


.. opcode:: BINARY_DIVIDE ()

   ``from __future__ import division`` が有効でないときの ``TOS = TOS1 / TOS`` に対応します。


.. opcode:: BINARY_FLOOR_DIVIDE ()

   ``TOS = TOS1 // TOS`` に対応します。


.. opcode:: BINARY_TRUE_DIVIDE ()

   ``from __future__ import division`` が有効なときの ``TOS = TOS1 / TOS`` に対応します。


.. opcode:: BINARY_MODULO ()

   ``TOS = TOS1 % TOS`` に対応します。


.. opcode:: BINARY_ADD ()

   ``TOS = TOS1 + TOS`` に対応します。


.. opcode:: BINARY_SUBTRACT ()

   ``TOS = TOS1 - TOS`` に対応します。


.. opcode:: BINARY_SUBSCR ()

   ``TOS = TOS1[TOS]`` に対応します。


.. opcode:: BINARY_LSHIFT ()

   ``TOS = TOS1 << TOS`` に対応します。


.. opcode:: BINARY_RSHIFT ()

   ``TOS = TOS1 >> TOS`` に対応します。


.. opcode:: BINARY_AND ()

   ``TOS = TOS1 & TOS`` に対応します。


.. opcode:: BINARY_XOR ()

   ``TOS = TOS1 ^ TOS`` に対応します。


.. opcode:: BINARY_OR ()

   ``TOS = TOS1 | TOS`` に対応します。


インプレース命令は TOS と TOS1 を取り除いて結果をスタックへプッシュするという点で二項命令と似ています。
しかし、TOS1 がインプレース命令をサポートしている場合には操作が直接 TOS1 に行われます。
また、操作結果の TOS は (常に同じというわけではありませんが) 元の TOS1 と同じオブジェクトになることが多いです。


.. opcode:: INPLACE_POWER ()

   インプレースの ``TOS = TOS1 ** TOS`` に対応します。


.. opcode:: INPLACE_MULTIPLY ()

   インプレースの ``TOS = TOS1 * TOS`` に対応します。


.. opcode:: INPLACE_DIVIDE ()

   ``from __future__ import division`` が有効でないときのインプレースの ``TOS = TOS1 / TOS`` に対応します。


.. opcode:: INPLACE_FLOOR_DIVIDE ()

   .. Implements in-place ``TOS = TOS1 // TOS``.

   インプレースの ``TOS = TOS1 // TOS`` に対応します。


.. opcode:: INPLACE_TRUE_DIVIDE ()

   ``from __future__ import division`` が有効なときのインプレースの ``TOS = TOS1 / TOS`` に対応します。


.. opcode:: INPLACE_MODULO ()

   インプレースの ``TOS = TOS1 % TOS`` に対応します。


.. opcode:: INPLACE_ADD ()

   インプレースの ``TOS = TOS1 + TOS`` に対応します。


.. opcode:: INPLACE_SUBTRACT ()

   インプレースの ``TOS = TOS1 - TOS`` に対応します。


.. opcode:: INPLACE_LSHIFT ()

   インプレースの ``TOS = TOS1 << TOS`` に対応します。


.. opcode:: INPLACE_RSHIFT ()

   インプレースの ``TOS = TOS1 >> TOS`` に対応します。


.. opcode:: INPLACE_AND ()

   インプレースの ``TOS = TOS1 & TOS`` に対応します。


.. opcode:: INPLACE_XOR ()

   インプレースの ``TOS = TOS1 ^ TOS`` に対応します。


.. opcode:: INPLACE_OR ()

   インプレースの ``TOS = TOS1 | TOS`` に対応します。


スライス命令コードは最大 3 つのパラメタを取ります。


.. opcode:: SLICE+0 ()

   ``TOS = TOS[:]`` に対応します。


.. opcode:: SLICE+1 ()

   ``TOS = TOS1[TOS:]`` に対応します。


.. opcode:: SLICE+2 ()

   ``TOS = TOS1[:TOS]`` に対応します。


.. opcode:: SLICE+3 ()

   ``TOS = TOS2[TOS1:TOS]`` に対応します。


スライス代入はさらにもう 1 つのパラメタを必要とします。
他の文と同じく、これらはスタックに何もプッシュしません。


.. opcode:: STORE_SLICE+0 ()

   ``TOS[:] = TOS1`` に対応します。


.. opcode:: STORE_SLICE+1 ()

   ``TOS1[TOS:] = TOS2`` に対応します。


.. opcode:: STORE_SLICE+2 ()

   ``TOS1[:TOS] = TOS2`` に対応します。


.. opcode:: STORE_SLICE+3 ()

   ``TOS2[TOS1:TOS] = TOS3`` に対応します。


.. opcode:: DELETE_SLICE+0 ()

   ``del TOS[:]`` に対応します。


.. opcode:: DELETE_SLICE+1 ()

   ``del TOS1[TOS:]`` に対応します。


.. opcode:: DELETE_SLICE+2 ()

   ``del TOS1[:TOS]`` に対応します。


.. opcode:: DELETE_SLICE+3 ()

   ``del TOS2[TOS1:TOS]`` に対応します。


.. opcode:: STORE_SUBSCR ()

   ``TOS1[TOS] = TOS2`` に対応します。


.. opcode:: DELETE_SUBSCR ()

   ``del TOS1[TOS]`` に対応します。


その他の命令コード。


.. opcode:: PRINT_EXPR ()

   対話モードのための式文に対応します。TOS はスタックから取り除かれ表示されます。
   非対話モードにおいては、式文は ``POP_STACK`` で終了しています。


.. opcode:: PRINT_ITEM ()

   ``sys.stdout`` に束縛されたファイル互換オブジェクトに対して TOS を出力します。
   :keyword:`print` 文の各要素に対してこのような命令が一つずつあります。


.. opcode:: PRINT_ITEM_TO ()

   ``PRINT_ITEM`` と似ていますが、TOS から二番目の要素を TOS にあるファイル互換オブジェクトへ出力します。
   これは拡張 print 文で使われます。


.. opcode:: PRINT_NEWLINE ()

   ``sys.stdout`` へ改行を表示します。
   これは :keyword:`print` 文がコンマで終わっていない場合に :keyword:`print` 文の最後の命令として生成されます。


.. opcode:: PRINT_NEWLINE_TO ()

   ``PRINT_NEWLINE`` と似ていますが、TOSのファイル互換オブジェクトに改行を表示します。
   これは拡張 print 文で使われます。


.. opcode:: BREAK_LOOP ()

   :keyword:`break` 文によってループを終了します。


.. opcode:: CONTINUE_LOOP (target)

   :keyword:`continue` 文によってループを継続します。
   *target* はジャンプするアドレスです (アドレスは ``FOR_ITER`` 命令でなければなりません)。


.. opcode:: LIST_APPEND (i)

   ``list.append(TOS[-i], TOS)`` を呼びます。リスト内包表記を実装するために使われます。
   要素が取り除かれる間、リストオブジェクトはスタックに残るので、ループの
   反復をさらに行えます。


.. opcode:: LOAD_LOCALS ()

   現在のスコープのローカルな名前空間 (locals) への参照をスタックにプッシュします。
   これはクラス定義のためのコードで使われます:
   クラス本体が評価された後、locals はクラス定義へ渡されます。


.. opcode:: RETURN_VALUE ()

   関数の呼び出し元へ TOS を返します。


.. opcode:: YIELD_VALUE ()

   ``TOS`` をポップし、それをジェネレータ (:term:`generator`) から yield します。


.. opcode:: IMPORT_STAR ()

   ``'_'`` で始まっていないすべてのシンボルをモジュール TOS から直接ローカル名前空間へロードします。
   モジュールはすべての名前をロードした後にポップされます。
   この命令コードは ``from module import *`` に対応します。


.. opcode:: EXEC_STMT ()

   ``exec TOS2,TOS1,TOS`` に対応します。コンパイラは指定されなかったオプションのパラメタを ``None`` で埋めます。


.. opcode:: POP_BLOCK ()

   ブロックスタックからブロックを一つ取り除きます。
   フレームごとにブロックのスタックがあり、ネストしたループや try 文などを表しています。


.. opcode:: END_FINALLY ()

   :keyword:`finally` 節を終了します。
   インタプリタは例外を再送出しなければならないかどうか、あるいは、
   関数から return して外側の次のブロックに続くかどうかを再度判断します。


.. opcode:: BUILD_CLASS ()

   新しいクラスオブジェクトを作成します。TOSはメソッド辞書、TOS1は基底クラスの名前のタプル、TOS2はクラス名です。


.. opcode:: SETUP_WITH (delta)

   この命令コードは、with ブロックが開始する前にいくつかの命令を行います。
   まず、コンテキストマネージャから :meth:`~object.__exit__` をロードし、
   後から :opcode:`WITH_CLEANUP` で使うためにスタックにプッシュします。
   そして、 :meth:`~object.__enter__` が呼び出され、 *delta* を指す
   finally ブロックがプッシュされます。最後に、enter メソッドを呼び出した
   結果がスタックにプッシュされます。次の命令コードはこれを無視
   (:opcode:`POP_TOP`) するか、変数に保存 (:opcode:`STORE_FAST`,
   :opcode:`STORE_NAME`, または :opcode:`UNPACK_SEQUENCE`) します。


.. opcode:: WITH_CLEANUP ()

   :keyword:`with` 式ブロックを抜けるときに、スタックをクリーンアップします。
   スタックの先頭は 1--3 個の値で、それらはなぜ/どのように finally 節に
   到達したかを表しています:


   * TOP = ``None``
   * (TOP, SECOND) = (``WHY_{RETURN,CONTINUE}``), retval
   * TOP = ``WHY_*``; no retval below it
   * (TOP, SECOND, THIRD) = exc_info()


   これらの値の下には、コンテキストマネージャーの :meth:`__exit__` 結合メソッド
   (bound method) である EXIT があります。


   最後のケースでは、 ``EXIT(TOP, SECOND, THIRD)`` が呼ばれ、それ以外では
   ``EXIT(None, None, None)`` が呼ばれます。


   EXIT はスタックから取り除かれ、その上の値は順序を維持したまま残されます。
   加えて、スタックが例外処理中であることを示し、 *かつ* 関数呼び出しが *true* 値を返した場合、
   ``END_FINALLY`` が例外を再送出することを防ぐため、この情報は削除されます ("zapped")。
   (しかし、 non-local goto は再開されます)


   .. XXX explain the WHY stuff!


以下の命令コードはすべて引数を必要とします。引数は 2 バイトで、最上位バイトが後になります。


   ``name = TOS`` に対応します。
   *namei* はコードオブジェクトの属性 :attr:`co_names` における *name* のインデクスです。
   コンパイラは可能ならば ``STORE_FAST`` または ``STORE_GLOBAL`` を使おうとします。


.. opcode:: DELETE_NAME (namei)

   ``del name`` に対応します。 *namei* はコードオブジェクトの :attr:`co_names` 属性へのインデクスです。


.. opcode:: UNPACK_SEQUENCE (count)

   TOS を *count* 個の個別の値にアンパックして、右から左の順にスタックに置きます。


.. opcode:: DUP_TOPX (count)

   *count* 個の要素を順番を保ちながら複製します。
   実装上の制限から、 *count* は 1以上 から 5 以下でなければなりません。


.. opcode:: STORE_ATTR (namei)

   ``TOS.name = TOS1`` に対応します。 *namei* は :attr:`co_names` における名前のインデクスです。


.. opcode:: DELETE_ATTR (namei)

   ``del TOS.name`` に対応します。 :attr:`co_names` へのインデクスとして *namei* を使います。


.. opcode:: STORE_GLOBAL (namei)

   ``STORE_NAME`` と同じように動作しますが、 name をグローバルとして保存します。


.. opcode:: DELETE_GLOBAL (namei)

   ``DELETE_NAME`` と同じように動作しますが、グローバルの name を削除します。


.. opcode:: LOAD_CONST (consti)

   ``co_consts[consti]`` をスタックにプッシュします。


.. opcode:: LOAD_NAME (namei)

   ``co_names[namei]`` に関連付けられた値をスタックにプッシュします。


.. opcode:: BUILD_TUPLE (count)

   スタックから *count* 個の要素を消費してタプルを作り出し、できたタプルをスタックにプッシュします。


.. opcode:: BUILD_LIST (count)

   ``BUILD_TUPLE`` と同じように動作しますが、リストを作り出します。


.. opcode:: BUILD_MAP (count)

   スタックに新しい辞書オブジェクトをプッシュします。
   辞書は *count* 個のエントリを持つサイズに設定されます。


.. opcode:: LOAD_ATTR (namei)

   TOS を ``getattr(TOS, co_names[namei])`` と入れ替えます。


.. opcode:: COMPARE_OP (opname)

   ブール命令を実行します。命令名は ``cmp_op[opname]`` にあります。


.. opcode:: IMPORT_NAME (namei)

   モジュール ``co_names[namei]`` をインポートします。
   TOS と TOS1 がポップされ、 :func:`__import__` の *fromlist* と *level* 引数になります。
   モジュールオブジェクトはスタックへプッシュされます。現在の名前空間は影響されません:
   適切な import 文のためには、後続の ``STORE_FAST`` 命令が名前空間を変更します。


.. opcode:: IMPORT_FROM (namei)

   TOS にあるモジュールから属性 ``co_names[namei]`` をロードします。
   作成されたオブジェクトはスタックにプッシュされ、後続の ``STORE_FAST`` 命令によって保存されます。


.. opcode:: JUMP_FORWARD (delta)

   バイトコードカウンタを *delta* だけ増加させます。


.. opcode:: POP_JUMP_IF_TRUE (target)

   TOS が真ならば、バイトコードカウンタを *target* に設定します。
   TOS はポップされます。


.. opcode:: POP_JUMP_IF_FALSE (target)

   TOS が偽ならば、バイトコードカウンタを *target* に設定します。
   TOS はポップされます。


.. opcode:: JUMP_IF_TRUE_OR_POP (target)

   TOS が真ならば、バイトコードカウンタを *target* に設定し、TOS は
   スタックに残されます。そうでない (TOS が偽) なら、TOS はポップされます。


.. opcode:: JUMP_IF_FALSE_OR_POP (target)

   TOS が偽ならば、バイトコードカウンタを *target* に設定し、TOS は
   スタックに残されます。そうでない (TOS が真) なら、TOS はポップされます。


.. opcode:: JUMP_ABSOLUTE (target)

   バイトコードカウンタを *target* に設定します。


.. opcode:: FOR_ITER (delta)

   ``TOS`` はイテレータです。その :meth:`!next` メソッドを呼び出します。
   新しい値が yield された場合は、それをスタックにプッシュします (イテレータはその下に残されます)。
   イテレータの呼び出しで要素が尽きたことが示された場合は、 ``TOS`` がポップされます。
   そして、バイトコードカウンタが *delta* だけ増やされます。


.. opcode:: LOAD_GLOBAL (namei)

   ``co_names[namei]`` という名前のグローバルをスタック上にロードします。


.. opcode:: SETUP_LOOP (delta)

   ループのためのブロックをブロックスタックにプッシュします。
   ブロックは現在の命令から *delta* バイトの大きさを占めます。


.. opcode:: SETUP_EXCEPT (delta)

   try-except 節から try ブロックをブロックスタックにプッシュします。
   *delta* は最初の except ブロックを指します。


.. opcode:: SETUP_FINALLY (delta)

   try-except 節から try ブロックをブロックスタックにプッシュします。
   *delta* は finally ブロックを指します。


.. opcode:: STORE_MAP ()

   key, value のペアを辞書に格納します。
   key と value をポップする一方、辞書はスタックに残されます。


.. opcode:: LOAD_FAST (var_num)

   ローカルな ``co_varnames[var_num]`` への参照をスタックにプッシュします。


.. opcode:: STORE_FAST (var_num)

   TOS をローカルな ``co_varnames[var_num]`` の中に保存します。


.. opcode:: DELETE_FAST (var_num)

   .. Deletes local ``co_varnames[var_num]``.

   ローカルな ``co_varnames[var_num]`` を削除します。


.. opcode:: LOAD_CLOSURE (i)

   セルと自由変数の記憶領域のスロット *i* に含まれるセルへの参照をプッシュします。
   *i* が *co_cellvars* の長さより小さければ、変数の名前は ``co_cellvars[i]`` です。
   そうでなければ ``co_freevars[i - len(co_cellvars)]`` です。


.. opcode:: LOAD_DEREF (i)

   セルと自由変数の記憶領域のスロット *i* に含まれるセルをロードします。
   セルが持つオブジェクトへの参照をスタックにプッシュします。


.. opcode:: STORE_DEREF (i)

   セルと自由変数の記憶領域のスロット *i* に含まれるセルへTOSを保存します。


.. opcode:: SET_LINENO (lineno)

      この命令コードは廃止されました。


.. opcode:: RAISE_VARARGS (argc)

   例外を発生させます。 *argc* は raise 文へ与えるパラメタの数を 0 から 3 の
   範囲で示します。
   ハンドラは TOS2 をトレースバック、TOS1 をパラメタ、TOS を例外として探します。


.. opcode:: CALL_FUNCTION (argc)

   関数を呼び出します。 *argc* の下位バイトは位置パラメタの数を、上位バイトはキーワードパラメタの数を示します。
   スタック上では、最初にキーワードパラメタが見つかります。
   それぞれのキーワード引数に対しては、値がキーより上に来ます。
   スタック上のキーワードパラメタの下に位置パラメタがあり、最も右のパラメタが先頭になります。
   スタック上のパラメタの下には、呼び出される関数オブジェクトが置かれます。
   すべての関数引数をポップし、関数自体もスタックから取り除き、戻り値をプッシュします。


.. opcode:: MAKE_FUNCTION (argc)

   新しい関数オブジェクトをスタックにプッシュします。
   TOS は関数に関連付けられたコードです。
   関数オブジェクトは TOS の下にある *argc* デフォルトパラメタをもつように定義されます。


.. opcode:: MAKE_CLOSURE (argc)

   新しい関数オブジェクトを作り出し、その *func_closure* スロットを設定し、スタックにプッシュします。
   TOS は関数に関連付けられたコードで、TOS1 はクロージャの自由変数に対するセルを格納したタプルです。
   関数はセルの前にある *argc* デフォルトパラメタも持っています。


.. opcode:: BUILD_SLICE (argc)

   .. index:: builtin: slice

   スライスオブジェクトをスタックにプッシュします。 *argc* は2あるいは3でなければなりません。
   2 ならば ``slice(TOS1, TOS)`` がプッシュされます。
   3 ならば ``slice(TOS2, TOS1, TOS)`` がプッシュされます。
   これ以上の情報については、 :func:`slice()` 組み込み関数を参照してください。


.. opcode:: EXTENDED_ARG (ext)

   デフォルトの 2 バイトに収まりきらない大きな引数を持つあらゆる命令コードの前に置かれます。
   *ext* は追加の 2 バイトを保持し、後続の命令コードの引数と組み合わされます。
   それらは 4 バイト引数を構成し、 *ext* はその最上位バイトです。


.. opcode:: CALL_FUNCTION_VAR (argc)

   関数を呼び出します。 *argc* は ``CALL_FUNCTION`` と同じように解釈されます。
   スタックの先頭の要素は可変引数リストを含んでおり、その後にキーワード引数と位置引数が続きます。


.. opcode:: CALL_FUNCTION_KW (argc)

   関数を呼び出します。 *argc* は ``CALL_FUNCTION`` と同じように解釈されます。
   スタックの先頭の要素はキーワード引数辞書を含んでおり、その後に明示的なキーワード引数と位置引数が続きます。


.. opcode:: CALL_FUNCTION_VAR_KW (argc)

   関数を呼び出します。 *argc* は ``CALL_FUNCTION`` と同じように解釈されます。
   スタックの先頭の要素はキーワード引数辞書を含んでおり、その後に変数引数のタプルが続き、
   さらに明示的なキーワード引数と位置引数が続きます。


.. opcode:: HAVE_ARGUMENT ()

   これは実際の命令コードではありません。引数を取らない命令コード ``< HAVE_ARGUMENT``  と、
   引数を取る命令コード ``>= HAVE_ARGUMENT`` の分割行を表します。
