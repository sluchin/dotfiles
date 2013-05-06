
:mod:`operator` --- 関数形式の標準演算子
========================================

.. module:: operator
   :synopsis: 標準演算子に対応する関数
.. sectionauthor:: Skip Montanaro <skip@automatrix.com>


.. testsetup::

   import operator
   from operator import itemgetter


:mod:`operator` モジュールは、Python 固有の各演算子に対応している
C 言語で実装された関数セットを提供します。例えば、
``operator.add(x, y)`` は式 ``x+y`` と等価です。
関数名は特殊なクラスメソッドとして扱われます; 便宜上、先頭と末尾の
``__``  を取り除いたものも提供されています。

これらの関数はそれぞれ、オブジェクトの比較、論理演算、数学演算、
シーケンス操作、および抽象型テストに分類されます。

オブジェクト比較関数は全てのオブジェクトで有効で、関数の名前はサポートする大小比較演算子からとられています:


.. function:: lt(a, b)
              le(a, b)
              eq(a, b)
              ne(a, b)
              ge(a, b)
              gt(a, b)
              __lt__(a, b)
              __le__(a, b)
              __eq__(a, b)
              __ne__(a, b)
              __ge__(a, b)
              __gt__(a, b)

   これらは  *a* および *b* の大小比較を行います。
   特に、 ``lt(a, b)`` は ``a < b`` 、 ``le(a, b)`` は ``a <= b`` 、
   ``eq(a, b)`` は ``a == b`` 、 ``ne(a, b)`` は ``a != b`` 、
   ``gt(a, b)`` は ``a > b`` 、そして ``ge(a, b)`` は ``a >= b`` と等価です。
   組み込み関数 :func:`cmp` と違って、これらの関数はどのような値を返してもよく、
   ブール代数値として解釈できてもできなくてもかまいません。
   大小比較の詳細については :ref:`comparisons` を参照してください。

   .. versionadded:: 2.2

論理演算もまた全てのオブジェクトに対して適用することができ、真値テスト、同一性テストおよびブール演算をサポートします:


.. function:: not_(obj)
              __not__(obj)

   :keyword:`not` *obj* の結果を返します。(オブジェクトのインスタンスには :meth:`__not__`
   メソッドは適用されないので注意してください; この操作を定義しているのはインタプリタコアだけです。結果は :meth:`__nonzero__` および
   :meth:`__len__` メソッドによって影響されます。)


.. function:: truth(obj)

   *obj* が真の場合 ``True`` を返し、そうでない場合 ``False``
   を返します。この関数は :class:`bool` のコンストラクタ呼び出しと同等です。


.. function:: is_(a, b)

   ``a is b`` を返します。オブジェクトの同一性をテストします。


.. function:: is_not(a, b)

   ``a is not b`` を返します。オブジェクトの同一性をテストします。

演算子で最も多いのは数学演算およびビット単位の演算です:


.. function:: abs(obj)
              __abs__(obj)

   *obj* の絶対値を返します。


.. function:: add(a, b)
              __add__(a, b)

   数値 *a* および *b* について ``a + b`` を返します。


.. function:: and_(a, b)
              __and__(a, b)

   *a* と *b* の論理積を返します。


.. function:: div(a, b)
              __div__(a, b)

   ``__future__.division`` が有効でない場合には ``a / b`` を返します。
   "古い(classic)" 除算としても知られています。


.. function:: floordiv(a, b)
              __floordiv__(a, b)

   ``a // b`` を返します。

   .. versionadded:: 2.2

.. function:: index(a)
              __index__(a)

   整数に変換された *a* を返します。 ``a.__index__()`` と同等です。

   .. versionadded:: 2.5

.. function:: inv(obj)
              invert(obj)
              __inv__(obj)
              __invert__(obj)

   *obj* のビット単位反転を返します。 ``~obj`` と同じです。

   .. versionadded:: 2.0
      名前 :func:`invert` および :func:`__invert__` が追加されました。


.. function:: lshift(a, b)
              __lshift__(a, b)

   *a* の *b* ビット左シフトを返します。


.. function:: mod(a, b)
              __mod__(a, b)

   ``a % b`` を返します。


.. function:: mul(a, b)
              __mul__(a, b)

   数値 *a* および *b* について ``a * b`` を返します。


.. function:: neg(obj)
              __neg__(obj)

   *obj* の符号反転 (``-obj``) を返します。


.. function:: or_(a, b)
              __or__(a, b)

   *a* と *b* の論理和を返します。


.. function:: pos(obj)
              __pos__(obj)

   *obj* の符号非反転 (``+obj``) を返します。


.. function:: pow(a, b)
              __pow__(a, b)

   数値 *a* および *b* について ``a ** b`` を返します。

   .. versionadded:: 2.3


.. function:: rshift(a, b)
              __rshift__(a, b)

   *a* の *b* ビット右シフトを返します。


.. function:: sub(a, b)
              __sub__(a, b)

   ``a - b`` を返します。


.. function:: truediv(a, b)
              __truediv__(a, b)

   ``__future__.division`` が有効な場合 ``a / b``  を返します。
   "真の"除算としても知られています。

   .. versionadded:: 2.2


.. function:: xor(a, b)
              __xor__(a, b)

   *a* および *b* の排他的論理和を返します。


シーケンスを扱う演算子（いくつかの演算子は map 型も扱います）には以下のようなものがあります:

.. function:: concat(a, b)
              __concat__(a, b)

   シーケンス *a* および *b* について ``a + b`` を返します。


.. function:: contains(a, b)
              __contains__(a, b)

   ``b in a`` を調べた結果を返します。演算対象が左右反転しているので注意してください。

   .. versionadded:: 2.0
      関数名 :func:`__contains__` が追加されました。


.. function:: countOf(a, b)

   *a* の中に *b* が出現する回数を返します。


.. function:: delitem(a, b)
              __delitem__(a, b)

   *a* でインデクスが *b* の要素を削除します。


.. function:: delslice(a, b, c)
              __delslice__(a, b, c)

   *a* でインデクスが *b* から *c-1* のスライス要素を削除します。

   .. deprecated:: 2.6
      この関数は Python 3.x で削除されます。
      :func:`delitem` をスライスインデクスで使って下さい。

.. function:: getitem(a, b)
              __getitem__(a, b)

   *a* でインデクスが *b* の要素を返します。


.. function:: getslice(a, b, c)
              __getslice__(a, b, c)

   *a* でインデクスが *b* から *c-1* のスライス要素を返します。

   .. deprecated:: 2.6
      この関数は Python 3.x で削除されます。
      :func:`getitem` をスライスインデクスで使って下さい。

.. function:: indexOf(a, b)

   *a* で最初に *b* が出現する場所のインデクスを返します。


.. function:: repeat(a, b)
              __repeat__(a, b)

   .. deprecated:: 2.7
      代わりに :func:`__mul__` を使って下さい。

   シーケンス *a* と整数 *b* について ``a * b`` を返します。


.. function:: sequenceIncludes(...)

   .. deprecated:: 2.0
      :func:`contains` を使ってください。

   :func:`contains` の別名です。


.. function:: setitem(a, b, c)
              __setitem__(a, b, c)

   *a* でインデクスが *b* の要素の値を *c* に設定します。


.. function:: setslice(a, b, c, v)
              __setslice__(a, b, c, v)

   *a* でインデクスが *b* から *c-1* のスライス要素の値をシーケンス *v* に設定します。

   .. deprecated:: 2.6
      この関数は Python 3.x で削除されます。
      :func:`setitem` をスライスインデクスで使って下さい。

operator の関数を使う例を挙げます::

    >>> # Elementwise multiplication
    >>> map(mul, [0, 1, 2, 3], [10, 20, 30, 40])
    [0, 20, 60, 120]

    >>> # Dot product
    >>> sum(map(mul, [0, 1, 2, 3], [10, 20, 30, 40]))
    200

多くの演算に「その場」バージョンがあります。
以下の関数はそうした演算子の通常の文法に比べてより素朴な呼び出し方を提供します。
たとえば、文(:term:`statement`) ``x += y`` は
``x = operator.iadd(x, y)`` と等価です。
別の言い方をすると、 ``z = operator.iadd(x, y)``
は複合文 ``z = x; z += y`` と等価です。


.. function:: iadd(a, b)
              __iadd__(a, b)

   ``a = iadd(a, b)`` は ``a += b`` と等価です。

   .. versionadded:: 2.5


.. function:: iand(a, b)
              __iand__(a, b)

   ``a = iand(a, b)`` は ``a &= b`` と等価です。

   .. versionadded:: 2.5


.. function:: iconcat(a, b)
              __iconcat__(a, b)

   ``a = iconcat(a, b)`` は二つのシーケンス *a* と *b* に対し ``a += b`` と等価です。

   .. versionadded:: 2.5


.. function:: idiv(a, b)
              __idiv__(a, b)

   ``a = idiv(a, b)`` は ``__future__.division`` が有効でないときに ``a /= b`` と等価です。

   .. versionadded:: 2.5


.. function:: ifloordiv(a, b)
              __ifloordiv__(a, b)

   ``a = ifloordiv(a, b)`` は ``a //= b`` と等価です。

   .. versionadded:: 2.5


.. function:: ilshift(a, b)
              __ilshift__(a, b)

   ``a = ilshift(a, b)`` は ``a <`` \ ``<= b`` と等価です。

   .. versionadded:: 2.5


.. function:: imod(a, b)
              __imod__(a, b)

   ``a = imod(a, b)`` は ``a %= b`` と等価です。

   .. versionadded:: 2.5


.. function:: imul(a, b)
              __imul__(a, b)

   ``a = imul(a, b)`` は ``a *= b`` と等価です。

   .. versionadded:: 2.5


.. function:: ior(a, b)
              __ior__(a, b)

   ``a = ior(a, b)`` は ``a |= b`` と等価です。

   .. versionadded:: 2.5


.. function:: ipow(a, b)
              __ipow__(a, b)

   ``a = ipow(a, b)`` は ``a **= b`` と等価です。

   .. versionadded:: 2.5


.. function:: irepeat(a, b)
              __irepeat__(a, b)

   .. deprecated:: 2.7
      代わりに :func:`__imul__` を使って下さい。

   ``a = irepeat(a, b)`` は *a* がシーケンスで *b* が整数であるとき ``a *= b`` と等価です。

   .. versionadded:: 2.5


.. function:: irshift(a, b)
              __irshift__(a, b)

   ``a = irshift(a, b)`` は ``a >>= b`` と等価です。

   .. versionadded:: 2.5


.. function:: isub(a, b)
              __isub__(a, b)

   ``a = isub(a, b)`` は ``a -= b`` と等価です。

   .. versionadded:: 2.5


.. function:: itruediv(a, b)
              __itruediv__(a, b)

   ``a = itruediv(a, b)`` は ``__future__.division`` が有効なときに ``a /= b`` と等価です。

   .. versionadded:: 2.5


.. function:: ixor(a, b)
              __ixor__(a, b)

   ``a = ixor(a, b)`` は ``a ^= b`` と等価です。

   .. versionadded:: 2.5

:mod:`operator` モジュールでは、オブジェクトの型を調べるための述語演算子も定義しています。
しかしながらこれらはいつでも信頼できるというわけではありません。
代わりに抽象基底クラスをテストするのが望ましい方法です
(詳しくは :mod:`collections` や :mod:`numbers` を参照して下さい)。


.. function:: isCallable(obj)

   .. deprecated:: 2.0
      :func:`isinstance(x, collections.Callable)` を使ってください。

   オブジェクト *obj* を関数のように呼び出すことができる場合真を返し、
   それ以外の場合偽を返します。関数、バインドおよび非バインドメソッド、
   クラスオブジェクト、および :meth:`__call__` メソッドをサポートするインスタンスオブジェクトは真を返します。


.. function:: isMappingType(obj)

   .. deprecated:: 2.7
      代わりに ``isinstance(x, collections.Mapping)`` を使って下さい。

   オブジェクト *obj* がマップ型インタフェースをサポートする場合に真を返します。
   辞書および :meth:`__getitem__`
   メソッドが定義された全てのインスタンスオブジェクトに対しては、この値は真になります。

.. function:: isNumberType(obj)

   .. deprecated:: 2.7
      代わりに ``isinstance(x, numbers.Number)`` を使って下さい。

   オブジェクト *obj* が数値を表現している場合に真を返します。
   C で実装された全ての数値型対して、この値は真になります。

.. function:: isSequenceType(obj)

   .. deprecated:: 2.7
      代わりに ``isinstance(x, collections.Sequence)`` を使って下さい。

   *obj* がシーケンス型プロトコルをサポートする場合に真を返します。
   シーケンス型メソッドを C で定義している全てのオブジェクトおよび
   :meth:`__getitem__` メソッドが定義された全てのインスタンスオブジェクトに対して、この値は真になります。

:mod:`operator` モジュールはアトリビュートとアイテムの汎用的な検索のための道具も定義しています。 :func:`map`,
:func:`sorted`, :meth:`itertools.groupby`,  や関数を引数に取るその他の関数に対して高速にフィールドを抽出する際に
引数として使うと便利です。


.. function:: attrgetter(attr[, args...])

   演算対象から *attr* を取得する呼び出し可能なオブジェクトを返します。二つ以上のアトリビュートを要求された場合には、アトリビュートのタプルを返します。
   ``f = attrgetter('name')`` とした後で、 ``f(b)`` を呼び出すと ``b.name`` を返します。
   ``f = attrgetter('name', 'date')`` とした後で、 ``f(b)`` を呼び出すと ``(b.name, b.date)``
   を返します。
   以下と等価です::

      def attrgetter(*items):
          if len(items) == 1:
              attr = items[0]
              def g(obj):
                  return resolve_attr(obj, attr)
          else:
              def g(obj):
                  return tuple(resolve_att(obj, attr) for attr in items)
          return g

      def resolve_attr(obj, attr):
          for name in attr.split("."):
              obj = getattr(obj, name)
          return obj


   アトリビュート名にドットを含んでも構いません。
   ``f = attrgetter('date.month')`` とした後で、 ``f(b)`` を呼び出すと ``b.date.month`` を返します。

   .. versionadded:: 2.4

   .. versionchanged:: 2.5
      複数のアトリビュートがサポートされました。

   .. versionchanged:: 2.6
      ドット付きアトリビュートがサポートされました。


.. function:: itemgetter(item[, args...])

   演算対象からその :meth:`__getitem__` メソッドを使って
   *item* を取得する呼び出し可能なオブジェクトを返します。
   二つ以上のアイテムを要求された場合には、アイテムのタプルを返します。
   以下のコードと等価です::

      def itemgetter(*items):
          if len(items) == 1:
              item = items[0]
              def g(obj):
                  return obj[item]
          else:
              def g(obj):
                  return tuple(obj[item] for item in items)
          return g

   アイテムは演算対象の :meth:`__getitem__` メソッドが受け付けるどんな型でも構いません。
   辞書ならば任意のハッシュ可能な値を受け付けますし、
   リスト、タプル、文字列などはインデクスかスライスを受け付けます:

      >>> itemgetter(1)('ABCDEFG')
      'B'
      >>> itemgetter(1,3,5)('ABCDEFG')
      ('B', 'D', 'F')
      >>> itemgetter(slice(2,None))('ABCDEFG')
      'CDEFG'

   .. versionadded:: 2.4

   .. versionchanged:: 2.5
      複数のアトリビュートがサポートされました.

   :func:`itemgetter` を使って特定のフィールドをタプルから取り出す例:

      >>> inventory = [('apple', 3), ('banana', 2), ('pear', 5), ('orange', 1)]
      >>> getcount = itemgetter(1)
      >>> map(getcount, inventory)
      [3, 2, 5, 1]
      >>> sorted(inventory, key=getcount)
      [('orange', 1), ('banana', 2), ('apple', 3), ('pear', 5)]


.. function:: methodcaller(name[, args...])

   引数の *name* メソッドを呼び出す呼び出し可能オブジェクトを返します。
   追加の引数および/またはキーワード引数が与えられると、
   これらもそのメソッドに引き渡されます。
   ``f = methodcaller('name')`` とした後で、 ``f(b)`` を呼び出すと ``b.name()`` を返します。
   ``f = methodcaller('name', 'foo', bar=1)`` とした後で、 ``f(b)`` を呼び出すと ``b.name('foo', bar=1)`` を返します。
   以下と等価です::

      def methodcaller(name, *args, **kwargs):
          def caller(obj):
              return getattr(obj, name)(*args, **kwargs)
          return caller


.. _operator-map:

演算子から関数への対応表
------------------------

下のテーブルでは、個々の抽象的な操作が、どのように Python 構文上の各演算子や :mod:`operator` モジュールの関数に対応しているか
を示しています。

+----------------------+-------------------------+----------------------------------------+
| 操作                 | 構文                    | 関数                                   |
+======================+=========================+========================================+
| 加算                 | ``a + b``               | ``add(a, b)``                          |
+----------------------+-------------------------+----------------------------------------+
| 結合                 | ``seq1 + seq2``         | ``concat(seq1, seq2)``                 |
+----------------------+-------------------------+----------------------------------------+
| 包含テスト           | ``obj in seq``          | ``contains(seq, obj)``                 |
+----------------------+-------------------------+----------------------------------------+
| 除算                 | ``a / b``               | ``div(a, b)``                          |
|                      |                         | (``__future__.division`` が無効な場合) |
+----------------------+-------------------------+----------------------------------------+
| 除算                 | ``a / b``               | ``truediv(a, b)``                      |
|                      |                         | (``__future__.division`` が有効な場合) |
+----------------------+-------------------------+----------------------------------------+
| 除算                 | ``a // b``              | ``floordiv(a, b)``                     |
+----------------------+-------------------------+----------------------------------------+
| 論理積               | ``a & b``               | ``and_(a, b)``                         |
+----------------------+-------------------------+----------------------------------------+
| 排他的論理和         | ``a ^ b``               | ``xor(a, b)``                          |
+----------------------+-------------------------+----------------------------------------+
| ビット反転           | ``~ a``                 | ``invert(a)``                          |
+----------------------+-------------------------+----------------------------------------+
| 論理和               | ``a | b``               | ``or_(a, b)``                          |
+----------------------+-------------------------+----------------------------------------+
| べき乗               | ``a ** b``              | ``pow(a, b)``                          |
+----------------------+-------------------------+----------------------------------------+
| インデクス指定の代入 | ``obj[k] = v``          | ``setitem(obj, k, v)``                 |
+----------------------+-------------------------+----------------------------------------+
| インデクス指定の削除 | ``del obj[k]``          | ``delitem(obj, k)``                    |
+----------------------+-------------------------+----------------------------------------+
| インデクス指定       | ``obj[k]``              | ``getitem(obj, k)``                    |
+----------------------+-------------------------+----------------------------------------+
| 左シフト             | ``a << b``              | ``lshift(a, b)``                       |
+----------------------+-------------------------+----------------------------------------+
| 剰余                 | ``a % b``               | ``mod(a, b)``                          |
+----------------------+-------------------------+----------------------------------------+
| 乗算                 | ``a * b``               | ``mul(a, b)``                          |
+----------------------+-------------------------+----------------------------------------+
| (算術)否             | ``- a``                 | ``neg(a)``                             |
+----------------------+-------------------------+----------------------------------------+
| (論理)否             | ``not a``               | ``not_(a)``                            |
+----------------------+-------------------------+----------------------------------------+
| 符号反転             | ``+ a``                 | ``pos(a)``                             |
+----------------------+-------------------------+----------------------------------------+
| 右シフト             | ``a >> b``              | ``rshift(a, b)``                       |
+----------------------+-------------------------+----------------------------------------+
| シーケンスの反復     | ``seq * i``             | ``repeat(seq, i)``                     |
+----------------------+-------------------------+----------------------------------------+
| スライス指定の代入   | ``seq[i:j] = values``   | ``setslice(seq, i, j, values)``        |
+----------------------+-------------------------+----------------------------------------+
| スライス指定の削除   | ``del seq[i:j]``        | ``delslice(seq, i, j)``                |
+----------------------+-------------------------+----------------------------------------+
| スライス指定         | ``seq[i:j]``            | ``getslice(seq, i, j)``                |
+----------------------+-------------------------+----------------------------------------+
| 文字列書式化         | ``s % obj``             | ``mod(s, obj)``                        |
+----------------------+-------------------------+----------------------------------------+
| 減算                 | ``a - b``               | ``sub(a, b)``                          |
+----------------------+-------------------------+----------------------------------------+
| 真値テスト           | ``obj``                 | ``truth(obj)``                         |
+----------------------+-------------------------+----------------------------------------+
| 順序付け             | ``a < b``               | ``lt(a, b)``                           |
+----------------------+-------------------------+----------------------------------------+
| 順序付け             | ``a <= b``              | ``le(a, b)``                           |
+----------------------+-------------------------+----------------------------------------+
| 等価性               | ``a == b``              | ``eq(a, b)``                           |
+----------------------+-------------------------+----------------------------------------+
| 不等性               | ``a != b``              | ``ne(a, b)``                           |
+----------------------+-------------------------+----------------------------------------+
| 順序付け             | ``a >= b``              | ``ge(a, b)``                           |
+----------------------+-------------------------+----------------------------------------+
| 順序付け             | ``a > b``               | ``gt(a, b)``                           |
+----------------------+-------------------------+----------------------------------------+

