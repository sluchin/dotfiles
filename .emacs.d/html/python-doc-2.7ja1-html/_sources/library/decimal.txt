
:mod:`decimal` --- 10進固定及び浮動小数点数の算術演算
=====================================================

.. module:: decimal
   :synopsis: 汎用10進数演算仕様 (General Decimal Arithmetic Specification) の実装。


.. moduleauthor:: Eric Price <eprice at tjhsst.edu>
.. moduleauthor:: Facundo Batista <facundo at taniquetil.com.ar>
.. moduleauthor:: Raymond Hettinger <python at rcn.com>
.. moduleauthor:: Aahz <aahz at pobox.com>
.. moduleauthor:: Tim Peters <tim.one at comcast.net>


.. sectionauthor:: Raymond D. Hettinger <python at rcn.com>

.. versionadded:: 2.4

.. import modules for testing inline doctests with the Sphinx doctest builder
.. testsetup:: *

   import decimal
   import math
   from decimal import *
   # make sure each group gets a fresh context
   setcontext(Context())

:mod:`decimal` モジュールは10進の浮動小数点算術をサポートします。
:mod:`decimal` には、 :class:`float`
データ型に比べて、以下のような利点があります:

* Decimal は「人々を念頭にデザインされた浮動小数点を元にしており、\
  必然的に最も重要な指針があります -- コンピュータは人々が学校で習った\
  算術と同じように動作する算術を提供しなければならない」 -- 10進数演算仕様より

* 10進数を正確に表現できます。 :const:`1.1` や :const:`2.2` のような数は、2 進数の\
  浮動小数点型では正しく表現できません。エンドユーザは普通、 2
  進数における ``1.1 + 2.2`` の近似値が :const:`3.3000000000000003`
  だからといって、そのように表示してほしいとは考えないものです。

* 値の正確さは算術にも及びます。10進の浮動小数点による計算では、
  ``0.1 + 0.1 + 0.1 - 0.3`` は厳密にゼロに等しくなります。 2
  進浮動小数点では :const:`5.5511151231257827e-017` になってしまいます。
  ゼロに近い値とはいえ、この誤差は数値間の等価性テストの信頼性を阻害します。
  また、誤差が蓄積されることもあります。こうした理由から、数値間の等価性を\
  厳しく保たねばならないようなアプリケーションを考えるなら、10進数による\
  数値表現が望ましいということになります。

* :mod:`decimal` モジュールでは、有効桁数の表記が取り入れられており、
  例えば ``1.30 + 1.20`` は :const:`2.50`
  になります。すなわち、末尾のゼロは有効数字を示すために残されます。
  こうした仕様は通貨計算を行うアプリケーションでは慣例です。
  乗算の場合、「教科書的な」アプローチでは、乗算の被演算子すべての桁数を使います。
  例えば、 ``1.3 * 1.2`` は :const:`1.56` になり、
  ``1.30 * 1.20`` は :const:`1.5600` になります。

* ハードウェアによる 2 進浮動小数点表現と違い、 :mod:`decimal`
  モジュールでは計算精度をユーザが変更できます(デフォルトでは 28
  桁です)。この桁数はほとんどの問題解決に十分な大きさです::

     >>> from decimal import *
     >>> getcontext().prec = 6
     >>> Decimal(1) / Decimal(7)
     Decimal('0.142857')
     >>> getcontext().prec = 28
     >>> Decimal(1) / Decimal(7)
     Decimal('0.1428571428571428571428571429')

* 2進と10進の浮動小数点は、いずれも広く公開されている標準仕様の\
  もとに実装されています。組み込みの浮動小数点型では、標準仕様で提唱されている\
  機能のほんのささやかな部分を利用できるにすぎませんが、 :mod:`decimal`
  では標準仕様が要求している全ての機能を利用できます。必要に応じて、\
  プログラマは値の丸めやシグナル処理を完全に制御できます。
  この中には全ての不正確な操作を例外でブロックして正確な算術を遵守させる\
  オプションもあります。

* decimal モジュールは「偏見なく、正確な丸めなしの十進算術\
  (固定小数点算術と呼ばれることもある)と\
  丸めありの浮動小数点数算術」(10進数演算仕様より引用)\
  をサポートするようにデザインされました。

このモジュールは、10進数型、算術コンテキスト (context for arithmetic)、
そしてシグナル (signal) という三つの概念を中心に設計されています、

10進数型は変更不可能な型です。この型には符号部、仮数部、そして指数部\
があります。有効桁数を残すために、仮数部の末尾にあるゼロの切り詰めは\
行われません。
:mod:`decimal` では、 :const:`Infinity`, :const:`-Infinity`, および
:const:`NaN` といった特殊な値も定義されています。
標準仕様では :const:`-0` と :const:`+0` も区別しています。

算術コンテキストとは、精度や値丸めの規則、指数部の制限を決めている
環境です。この環境では、演算結果を表すためのフラグや、演算上発生した
特定のシグナルを例外として扱うかどうかを決めるトラップイネーブラも
定義しています。丸め規則には :const:`ROUND_CEILING`,
:const:`ROUND_DOWN`, :const:`ROUND_FLOOR`, :const:`ROUND_HALF_DOWN`,
:const:`ROUND_HALF_EVEN`, :const:`ROUND_HALF_UP`, :const:`ROUND_UP`,
および :const:`ROUND_05UP` があります。

シグナルとは、演算の過程で生じる例外的条件です。個々のシグナルは、\
アプリケーションそれぞれの要求に従って、無視されたり、単なる情報と\
みなされたり、例外として扱われたりします。
:mod:`decimal` モジュールには、 :const:`Clamped`,
:const:`InvalidOperation`, :const:`DivisionByZero`, :const:`Inexact`,
:const:`Rounded`, :const:`Subnormal`, :const:`Overflow`,
および :const:`Underflow` といったシグナルがあります。

各シグナルには、フラグとトラップイネーブラがあります。演算上\
何らかのシグナルに遭遇すると、フラグは 1 にセットされて\
ゆきます。このとき、もしトラップイネーブラが 1 にセットされて\
いれば、例外を送出します。フラグの値は膠着型 (sticky) なので、\
演算によるフラグの変化をモニタしたければ、予めフラグをリセット\
しておかねばなりません。


.. seealso::

   * IBM による汎用10進演算仕様、 `The General Decimal Arithmetic Specification
     <http://speleotrove.com/decimal/>`_ 。

   * IEEE 標準化仕様 854-1987, `IEEE 854 に関する非公式のテキスト
     <http://754r.ucbtest.org/standards/854.pdf>`_ 。

.. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


.. _decimal-tutorial:

Quick-start Tutorial
--------------------

普通、 :mod:`decimal` を使うときには、モジュールを import し、現在の\
演算コンテキストを :func:`getcontext` で調べ、必要に応じて\
精度や丸めを設定し、演算エラーのトラップを有効にします::

   >>> from decimal import *
   >>> getcontext()
   Context(prec=28, rounding=ROUND_HALF_EVEN, Emin=-999999999, Emax=999999999,
           capitals=1, flags=[], traps=[Overflow, DivisionByZero,
           InvalidOperation])

   >>> getcontext().prec = 7       # 新たな精度を設定

:class:`Decimal` のインスタンスは、整数、文字列、浮動小数点数、またはタプルから構成
できます。整数や浮動小数点数からの構成は、整数や浮動小数点数の値を正確に
変換します。 :class:`Decimal` は
"数値ではない (Not a Number)" を表す :const:`NaN` や正負の
:const:`Infinity` (無限大)、 :const:`-0` といった特殊な値も表現できます。

   >>> getcontext().prec = 28
   >>> Decimal(10)
   Decimal('10')
   >>> Decimal('3.14')
   Decimal('3.14')
   >>> Decimal(3.14)
   Decimal('3.140000000000000124344978758017532527446746826171875')
   >>> Decimal((0, (3, 1, 4), -2))
   Decimal('3.14')
   >>> Decimal(str(2.0 ** 0.5))
   Decimal('1.41421356237')
   >>> Decimal(2) ** Decimal('0.5')
   Decimal('1.414213562373095048801688724')
   >>> Decimal('NaN')
   Decimal('NaN')
   >>> Decimal('-Infinity')
   Decimal('-Infinity')

新たな :class:`Decimal` 型数値の有効桁数は入力した数の桁数だけで決まります。
演算コンテキストにおける精度や値丸めの設定が影響するのは算術操作の中だけです。

.. doctest:: newcontext

   >>> getcontext().prec = 6
   >>> Decimal('3.0')
   Decimal('3.0')
   >>> Decimal('3.1415926535')
   Decimal('3.1415926535')
   >>> Decimal('3.1415926535') + Decimal('2.7182818285')
   Decimal('5.85987')
   >>> getcontext().rounding = ROUND_UP
   >>> Decimal('3.1415926535') + Decimal('2.7182818285')
   Decimal('5.85988')

:class:`Decimal` 型数値はほとんどの場面で Python の他の機能とうまく\
やりとりできます。 :class:`Decimal`
浮動小数点小劇場 (flying circus) を示しましょう:

.. doctest::
   :options: +NORMALIZE_WHITESPACE

   >>> data = map(Decimal, '1.34 1.87 3.45 2.35 1.00 0.03 9.25'.split())
   >>> max(data)
   Decimal('9.25')
   >>> min(data)
   Decimal('0.03')
   >>> sorted(data)
   [Decimal('0.03'), Decimal('1.00'), Decimal('1.34'), Decimal('1.87'),
    Decimal('2.35'), Decimal('3.45'), Decimal('9.25')]
   >>> sum(data)
   Decimal('19.29')
   >>> a,b,c = data[:3]
   >>> str(a)
   '1.34'
   >>> float(a)
   1.34
   >>> round(a, 1)     # round() は値をまず二進の浮動小数点数に変換します
   1.3
   >>> int(a)
   1
   >>> a * 5
   Decimal('6.70')
   >>> a * b
   Decimal('2.5058')
   >>> c % a
   Decimal('0.77')

いくつかの数学的関数も Decimal には用意されています:

   >>> getcontext().prec = 28
   >>> Decimal(2).sqrt()
   Decimal('1.414213562373095048801688724')
   >>> Decimal(1).exp()
   Decimal('2.718281828459045235360287471')
   >>> Decimal('10').ln()
   Decimal('2.302585092994045684017991455')
   >>> Decimal('10').log10()
   Decimal('1')

:meth:`quantize` メソッドは位を固定して数値を丸めます。このメソッドは、\
計算結果を固定の桁数で丸めることがよくある、通貨を扱うアプリケーションで\
便利です:

   >>> Decimal('7.325').quantize(Decimal('.01'), rounding=ROUND_DOWN)
   Decimal('7.32')
   >>> Decimal('7.325').quantize(Decimal('1.'), rounding=ROUND_UP)
   Decimal('8')

前述のように、 :func:`getcontext` 関数を使うと現在の演算コンテキスト\
にアクセスでき、設定を変更できます。ほとんどのアプリケーションはこの\
アプローチで十分です。

より高度な作業を行う場合、 :func:`Context` コンストラクタを使って\
別の演算コンテキストを作っておくと便利なことがあります。
別の演算コンテキストをアクティブにしたければ、 :func:`setcontext` を使います。

:mod:`Decimal` モジュールでは、標準仕様に従って、すぐ利用できる\
二つの標準コンテキスト、 :const:`BasicContext` および
:const:`ExtendedContext` を提供しています。後者はほとんどのトラップが\
有効になっており、とりわけデバッグの際に便利です:

.. doctest:: newcontext
   :options: +NORMALIZE_WHITESPACE

   >>> myothercontext = Context(prec=60, rounding=ROUND_HALF_DOWN)
   >>> setcontext(myothercontext)
   >>> Decimal(1) / Decimal(7)
   Decimal('0.142857142857142857142857142857142857142857142857142857142857')

   >>> ExtendedContext
   Context(prec=9, rounding=ROUND_HALF_EVEN, Emin=-999999999, Emax=999999999,
           capitals=1, flags=[], traps=[])
   >>> setcontext(ExtendedContext)
   >>> Decimal(1) / Decimal(7)
   Decimal('0.142857143')
   >>> Decimal(42) / Decimal(0)
   Decimal('Infinity')

   >>> setcontext(BasicContext)
   >>> Decimal(42) / Decimal(0)
   Traceback (most recent call last):
     File "<pyshell#143>", line 1, in -toplevel-
       Decimal(42) / Decimal(0)
   DivisionByZero: x / 0

演算コンテキストには、演算中に遭遇した例外的状況をモニタするための\
シグナルフラグがあります。フラグが一度セットされると、明示的に\
クリアするまで残り続けます。そのため、フラグのモニタを行いたいような\
演算の前には :meth:`clear_flags` メソッドでフラグをクリアして\
おくのがベストです。 :

   >>> setcontext(ExtendedContext)
   >>> getcontext().clear_flags()
   >>> Decimal(355) / Decimal(113)
   Decimal('3.14159292')
   >>> getcontext()
   Context(prec=9, rounding=ROUND_HALF_EVEN, Emin=-999999999, Emax=999999999,
           capitals=1, flags=[Rounded, Inexact], traps=[])

*flags* エントリから、 :const:`Pi` の有理数による近似値が丸められた
(コンテキスト内で決められた精度を超えた桁数が捨てられた) ことと、\
計算結果が厳密でない (無視された桁の値に非ゼロのものがあった) ことが\
わかります。

コンテキストの :attr:`traps` フィールドに入っている辞書を使うと、\
個々のトラップをセットできます:

.. doctest:: newcontext

   >>> setcontext(ExtendedContext)
   >>> Decimal(1) / Decimal(0)
   Decimal('Infinity')
   >>> getcontext().traps[DivisionByZero] = 1
   >>> Decimal(1) / Decimal(0)
   Traceback (most recent call last):
     File "<pyshell#112>", line 1, in -toplevel-
       Decimal(1) / Decimal(0)
   DivisionByZero: x / 0

ほとんどのプログラムでは、開始時に一度だけ現在の演算コンテキストを\
修正します。また、多くのアプリケーションでは、データから :class:`Decimal`
への変換はループ内で一度だけキャストして行います。コンテキストを設定し、
:class:`Decimal` オブジェクトを生成できたら、ほとんどのプログラムは\
他の Python 数値型と全く変わらないかのように :class:`Decimal` を操作できます。

.. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


.. _decimal-decimal:

Decimal オブジェクト
--------------------


.. class:: Decimal([value [, context]])

   *value* に基づいて新たな :class:`Decimal` オブジェクトを構築します。

   *value* は整数、文字列、タプル、 :class:`float` および他の :class:`Decimal`
   オブジェクトにできます。
   *value* を指定しない場合、 ``Decimal("0")`` を返します。
   *value* が文字列の場合、先頭と末尾の空白を取り除いた後には以下の
   10進数文字列の文法に従わねばなりません::

      sign           ::=  '+' | '-'
      digit          ::=  '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
      indicator      ::=  'e' | 'E'
      digits         ::=  digit [digit]...
      decimal-part   ::=  digits '.' [digits] | ['.'] digits
      exponent-part  ::=  indicator [sign] digits
      infinity       ::=  'Infinity' | 'Inf'
      nan            ::=  'NaN' [digits] | 'sNaN' [digits]
      numeric-value  ::=  decimal-part [exponent-part] | infinity
      numeric-string ::=  [sign] numeric-value | [sign] nan

   *value* をユニコード文字列にした場合、他のユニコード数字も上の ``digit``
   の場所に使うことができます。つまり各書記体系における(アラビア-インド系や
   デーヴァナーガリーなど)の数字や、全角数字０(``u'\uff10'``)から
   ９(``u'\uff19'``)までなどです。

   *value* を :class:`tuple` にする場合、タプルは三つの要素を持ち、
   それぞれ符号 (正なら :const:`0` 、負なら :const:`1`)、仮数部を
   表す数字のタプル、そして指数を表す整数でなければなりません。
   例えば、 ``Decimal((0, (1, 4, 1, 4), -3))`` は
   ``Decimal('1.414')`` を返します。

   *value* を :class:`float` にする場合、2進浮動小数点数値が損失なく
   正確に等価な Decimal に変換されます。この変換はしばしば 53 桁以上の精度を
   要求します。例えば、 ``Decimal(float('1.1'))`` は
   ``Decimal('1.100000000000000088817841970012523233890533447265625')``
   に変換されます。

   *context* に指定した精度 (precision) は、オブジェクトが記憶する\
   桁数には影響しません。桁数は *value* に指定した桁数だけから\
   決定されます。例えば、演算コンテキストに指定された精度が 3 桁しかなくても、\
   ``Decimal('3.00000')`` は 5 つのゼロを全て記憶します。

   *context* 引数の目的は、 *value* が正しくない形式の文字列\
   であった場合に行う処理を決めることにあります;
   演算コンテキストが :const:`InvalidOperation` をトラップするように\
   なっていれば、例外を送出します。それ以外の場合には、コンストラクタは\
   値が :const:`NaN` の :class:`Decimal` を返します。

   一度生成すると、 :class:`Decimal` オブジェクトは変更不能 (immutable)
   になります。

   .. versionchanged:: 2.6
      文字列から Decimal インスタンスを生成する際に先頭と末尾の空白が許\
      されることになりました。

   .. versionchanged:: 2.7
      このコンストラクタの引数は、 :class:`float` インスタンスにもできるようになりました。

   10進浮動小数点オブジェクトは、 :class:`float` や :class:`int` のような\
   他の組み込み型と多くの点で似ています。通常の数学演算や特殊メソッドを\
   適用できます。また、 :class:`Decimal` オブジェクトは\
   コピーでき、pickle 化でき、print で出力でき、辞書のキーにでき、
   集合の要素にでき、比較、保存、他の型 (:class:`float`
   や :class:`long`) への型強制を行えます。

   Decimal オブジェクトは一般に、算術演算で浮動小数点数と組み合わせることが
   できません。例えば、 :class:`Decimal` に :class:`float` を足そうとすると、
   :exc:`TypeError` が送出されます。ただしこの規則には例外があります。
   :class:`float` インスタンス ``x`` と :class:`Decimal` インスタンス ``y``
   を比較する比較演算子です。この例外がなかったとすると、 :class:`Decimal` と
   :class:`float` インスタンスの比較は、リファレンスマニュアルの
   :ref:`expressions` 節で記述されている、異なる型のオブジェクトを
   比較するときの一般の規則に従うことになり、紛らわしい結果につながります。

   .. versionchanged:: 2.7
      :class:`float` インスタンス ``x`` と :class:`Decimal` インスタンス ``y``
      の比較は、 ``x`` と ``y`` の値に基づく結果を返すようになりました。
      以前のバージョンでは、どんな :class:`float` インスタンス ``x`` と
      どんな :class:`Decimal` インスタンス ``y`` に対しても、
      ``x < y`` は同じ(任意の) 結果を返していました。

   こうした標準的な数値型の特性の他に、10進浮動小数点オブジェクトには\
   様々な特殊メソッドがあります:

   .. method:: adjusted()

      仮数部の先頭の一桁だけが残るように右側の数字を追い出す桁シフトを行い、
      その結果の指数部を返します:
      ``Decimal('321e+5').adjusted()`` なら 7 です。
      最上桁の小数点からの相対位置を調べる際に使います。


   .. method:: as_tuple()

      数値を表現するための名前付きタプル(:term:`named tuple`):
      ``(sign, digittuple, exponent)`` を返します。

      .. versionchanged:: 2.6
         名前付きタプルを使用するようになりました。

   .. method:: canonical()

      引数の標準的(canonical)エンコーディングを返します。現在のところ、
      :class:`Deciaml` インスタンスのエンコーディングは常に標準的なので、
      この操作は引数に手を加えずに返します。

      .. versionadded:: 2.6

   .. method:: compare(other[, context])

      二つの Decimal インスタンスを比較します。この演算は通常の比較メソッド
      :meth:`__cmp__` と同じように振る舞いますが、整数でなく Decimal
      インスタンスを返すところと、両方の引数が NaN だったときに結果としても
      NaN を返すところが異なります。::

         a or b is a NaN ==> Decimal("NaN")
         a < b           ==> Decimal("-1")
         a == b          ==> Decimal("0")
         a > b           ==> Decimal("1")

   .. method:: compare_signal(other[, context])

      この演算は :meth:`compare` とほとんど同じですが、全ての NaN が\
      シグナルを送るところが異なります。すなわち、どちらの比較対象も発信
      (signaling) NaN でないならば無言(quiet) NaN である比較対象が\
      あたかも発信 NaN であるかのように扱われます。

      .. versionadded:: 2.6

   .. method:: compare_total(other)

      二つの対象を数値によらず抽象表現によって比較します。 :meth:`compare`
      に似ていますが、結果は :class:`Decimal` に全順序を与えます。
      この順序づけによると、数値的に等しくても異なった表現を持つ二つの
      :class:`Decimal` インスタンスの比較は等しくなりません:

         >>> Decimal('12.0').compare_total(Decimal('12'))
         Decimal('-1')


      無言 NaN と発信 NaN もこの全順序に位置付けられます。
      この関数の結果は、もし比較対象が同じ表現を持つならば ``Decimal('0')``
      であり、一つめの比較対象が二つめより下位にあれば ``Decimal('-1')`` 、
      上位にあれば ``Decimal('1')`` です。全順序の詳細については仕様を参照してください。

      .. versionadded:: 2.6

   .. method:: compare_total_mag(other)

      二つの対象を :meth:`compare_total` のように数値によらず抽象表現によって\
      比較しますが、両者の符号を無視します。 ``x.compare_total_mag(y)``
      は ``x.copy_abs().compare_total(y.copy_abs())`` と等価です。

      .. versionadded:: 2.6

   .. method:: conjugate()

      self を返すだけです。このメソッドは十進演算仕様に適合するためだけのものです。

      .. versionadded:: 2.6

   .. method:: copy_abs()

      引数の絶対値を返します。
      この演算はコンテキストに影響されず、静かです。
      すなわち、フラグは変更されず、丸めは行われません。

      .. versionadded:: 2.6

   .. method:: copy_negate()

      引数の符号を変えて返します。
      この演算はコンテキストに影響されず、静かです。
      すなわち、フラグは変更されず、丸めは行われません。

      .. versionadded:: 2.6

   .. method:: copy_sign(other)

      最初の演算対象のコピーに二つめと同じ符号を付けて返します。たとえば:

         >>> Decimal('2.3').copy_sign(Decimal('-1.5'))
         Decimal('-2.3')

      この演算はコンテキストに影響されず、静かです。
      すなわち、フラグは変更されず、丸めは行われません。

      .. versionadded:: 2.6

   .. method:: exp([context])

      与えられた数での(自然)指数関数 ``e**x`` の値を返します。
      結果は :const:`ROUND_HALF_EVEN` 丸めモードで正しく丸められます。

      >>> Decimal(1).exp()
      Decimal('2.718281828459045235360287471')
      >>> Decimal(321).exp()
      Decimal('2.561702493119680037517373933E+139')

      .. versionadded:: 2.6

   .. method:: from_float(f)

      浮動小数点数を正確に小数に変換するクラスメソッドです。

      なお、 `Decimal.from_float(0.1)` は `Decimal('0.1')` と同じではありません。
      0.1 は二進浮動小数点数で正確に表せないので、その値は表現できる最も近い
      値、 `0x1.999999999999ap-4` として記憶されます。浮動小数点数での等価な値は
      `0.1000000000000000055511151231257827021181583404541015625` です。

      .. note:: Python 2.7 以降では、 :class:`Decimal` インスタンスは
         :class:`float` から直接構成することも出来ます。

      .. doctest::

          >>> Decimal.from_float(0.1)
          Decimal('0.1000000000000000055511151231257827021181583404541015625')
          >>> Decimal.from_float(float('nan'))
          Decimal('NaN')
          >>> Decimal.from_float(float('inf'))
          Decimal('Infinity')
          >>> Decimal.from_float(float('-inf'))
          Decimal('-Infinity')

      .. versionadded:: 2.7

   .. method:: fma(other, third[, context])

      融合積和(fused multiply-add)です。self*other+third を途中結果の積
      self*other で丸めを行わずに計算して返します。

      >>> Decimal(2).fma(3, 5)
      Decimal('11')

      .. versionadded:: 2.6

   .. method:: is_canonical()

      引数が標準的(canonical)ならば :const:`True` を返し、そうでなければ
      :const:`False` を返します。現在のところ、 :class:`Decimal` のインスタンスは\
      常に標準的なのでこのメソッドの結果はいつでも :const:`True` です。

      .. versionadded:: 2.6

   .. method:: is_finite()

      引数が有限の数値ならば :const:`True` を、無限大か NaN ならば :const:`False`
      を返します。

      .. versionadded:: 2.6

   .. method:: is_infinite()

      引数が正または負の無限大ならば :const:`True` を、そうでなければ :const:`False`
      を返します。

      .. versionadded:: 2.6

   .. method:: is_nan()

      引数が(無言か発信かは問わず) NaN であれば
      :const:`True` を、そうでなければ :const:`False` を返します。

      .. versionadded:: 2.6

   .. method:: is_normal()

      引数が *正規(normal)* のゼロでない有限数値で調整された指数が *Emin*
      以上ならば :const:`True` を返します。
      引数がゼロ、非正規(subnormal)、無限大または NaN であれば :const:`False`
      を返します。
      ここでの *正規* という用語は標準的な(canonical)値を作り出すために使われる
      :meth:`normalize` メソッドにおける意味合いとは異なりますので注意して下さい。

      .. versionadded:: 2.6

   .. method:: is_qnan()

      引数が無言 NaN であれば :const:`True` を、そうでなければ :const:`False`
      を返します。

      .. versionadded:: 2.6

   .. method:: is_signed()

      引数に負の符号がついていれば :const:`True` を、そうでなければ :const:`False`
      を返します。注意すべきはゼロや NaN なども符号を持ち得ることです。

      .. versionadded:: 2.6

   .. method:: is_snan()

      引数が発信 NaN であれば :const:`True` を、そうでなければ
      :const:`False` を返します。

      .. versionadded:: 2.6

   .. method:: is_subnormal()

      引数が非正規数(subnormal)であれば :const:`True` を、そうでなければ
      :const:`False` を返します。非正規な数値とは、ゼロでなく、有限で、
      調整された指数が *Emin* 未満のものを指します。

      .. versionadded:: 2.6

   .. method:: is_zero()

      引数が(正または負の)ゼロであれば :const:`True` を、そうでなければ
      :const:`False` を返します。

      .. versionadded:: 2.6

   .. method:: ln([context])

      演算対象の自然対数(底 e の対数)を返します。
      結果は :const:`ROUND_HALF_EVEN` 丸めモードで正しく丸められます。

      .. versionadded:: 2.6

   .. method:: log10([context])

      演算対象の常用対数(底 10 の対数)を返します。
      結果は :const:`ROUND_HALF_EVEN` 丸めモードで正しく丸められます。

      .. versionadded:: 2.6

   .. method:: logb([context])

      非零の数値については、 :class:`Decimal` インスタンスとして調整された\
      指数を返します。演算対象がゼロだった場合、 ``Decimal('-Infinity')``
      が返され :const:`DivisionByZero` フラグが送出されます。
      演算対象が無限大だった場合、 ``Decimal('Infinity')`` が返されます。

      .. versionadded:: 2.6

   .. method:: logical_and(other[, context])

      :meth:`logical_and` は二つの *論理引数* (:ref:`logical_operands_label`
      参照)を取る論理演算です。結果は二つの引数の数字ごとの ``and`` です。

      .. versionadded:: 2.6

   .. method:: logical_invert([context])

      :meth:`logical_invert` は論理演算です。
      結果は引数の数字ごとの反転です。

      .. versionadded:: 2.6

   .. method:: logical_or(other[, context])

      :meth:`logical_or` は二つの *論理引数* (:ref:`logical_operands_label`
      参照)を取る論理演算です。結果は二つの引数の数字ごとの ``or`` です。

      .. versionadded:: 2.6

   .. method:: logical_xor(other[, context])

      :meth:`logical_xor` は二つの *論理引数* (:ref:`logical_operands_label`
      参照)を取る論理演算です。結果は二つの引数の数字ごとの排他的論理和です。

      .. versionadded:: 2.6

   .. method:: max(other[, context])

      ``max(self, other)`` と同じですが、値を返す前に現在のコンテキストに\
      即した丸め規則を適用します。また、  :const:`NaN`
      に対して、(コンテキストの設定と、発信か無言どちらのタイプであるか\
      に応じて) シグナルを発行するか無視します。

   .. method:: max_mag(other[, context])

      :meth:`.max` メソッドに似ていますが、比較は絶対値で行われます。

      .. versionadded:: 2.6

   .. method:: min(other[, context])

      ``min(self, other)`` と同じですが、値を返す前に現在のコンテキストに\
      即した丸め規則を適用します。また、  :const:`NaN`
      に対して、(コンテキストの設定と、発信か無言どちらのタイプであるか\
      に応じて) シグナルを発行するか無視します。


   .. method:: min_mag(other[, context])

      :meth:`.min` メソッドに似ていますが、比較は絶対値で行われます。

      .. versionadded:: 2.6

   .. method:: next_minus([context])

      与えられたコンテキスト(またはコンテキストが渡されなければ現スレッ\
      ドのコンテキスト)において表現可能な、操作対象より小さい最大の数を\
      返します。

      .. versionadded:: 2.6

   .. method:: next_plus([context])

      与えられたコンテキスト(またはコンテキストが渡されなければ現スレッ\
      ドのコンテキスト)において表現可能な、操作対象より大きい最小の数を\
      返します。

      .. versionadded:: 2.6

   .. method:: next_toward(other[, context])

      二つの比較対象が等しくなければ、一つめの対象に最も近く二つめの対\
      象へ近付く方向の数を返します。もし両者が数値的に等しければ、二つ\
      めの対象の符号を採った一つめの対象のコピーを返します。

      .. versionadded:: 2.6

   .. method:: normalize([context])

      数値を正規化 (normalize) して、右端に連続しているゼロを除去し、
      :const:`Decimal('0')` と同じ結果はすべて
      :const:`Decimal('0e0')` に変換します。
      同じクラスの値から基準表現を生成する際に用います。たとえば、
      ``Decimal('32.100')`` と ``Decimal('0.321000e+2')`` の正規化は、いずれも同じ値
      ``Decimal('32.1')`` になります。


   .. method:: number_class([context])

      操作対象の *クラス* を表す文字列を返します。返されるのは以下の10種類のいずれかです。

      * ``"-Infinity"``, 負の無限大であることを示します。
      * ``"-Normal"``, 負の通常数であることを示します。
      * ``"-Subnormal"``, 負の非正規数であることを示します。
      * ``"-Zero"``, 負のゼロであることを示します。
      * ``"+Zero"``, 正のゼロであることを示します。
      * ``"+Subnormal"``, 正の非正規数であることを示します。
      * ``"+Normal"``, 正の通常数であることを示します。
      * ``"+Infinity"``, 正の無限大であることを示します。
      * ``"NaN"``, 無言(quiet) NaN (Not a Number) であることを示します。
      * ``"sNaN"``, 発信(signaling) NaN であることを示します。

      .. versionadded:: 2.6

   .. method:: quantize(exp [, rounding[, context[, watchexp]]])

      二つめの操作対象と同じ指数を持つように丸めを行った、\
      一つめの操作対象と等しい値を返します。

      >>> Decimal('1.41421356').quantize(Decimal('1.000'))
      Decimal('1.414')

      他の操作と違い、打ち切り(quantize)操作後の係数の長さが精度を越えた場合には、
      :const:`InvalidOperation` がシグナルされます。これにより\
      エラー条件がない限り打ち切られた指数が常に右側の引数と同じになることが\
      保証されます。

      同様に、他の操作と違い、 quantize は Underflow を、たとえ結果が\
      非正規になったり不正確になったとしても、シグナルしません。

      二つ目の演算対象の指数が一つ目のそれよりも大きければ丸めが必要かもしれません。
      この場合、丸めモードは以下のように決められます。
      ``rounding`` 引数が与えられていればそれが使われます。
      そうでなければ ``context`` 引数で決まります。
      どちらの引数も渡されなければ現在のスレッドのコンテキストの丸めモードが使われます。

      *watchexp* が (default) に設定されている場合、処理結果の指数\
      が :attr:`Emax` よりも大きい場合や :attr:`Etiny` よりも小さい\
      場合にエラーを返します。

   .. method:: radix()

      ``Decimal(10)`` つまり :class:`Decimal` クラスがその全ての算術を実行する\
      基数を返します。仕様との互換性のために取り入れられています。

      .. versionadded:: 2.6

   .. method:: remainder_near(other[, context])

      モジュロを計算し、正負のモジュロのうちゼロに近い値を返します。
      たとえば、 ``Decimal(10).remainder_near(6)`` は
      ``Decimal('4')`` よりもゼロに近い値 ``Decimal('-2')`` を返します。

      ゼロからの差が同じ場合には、 *self* と同じ符号を持った方を\
      返します。

   .. method:: rotate(other[, context])

      一つめの演算対象の数字を二つめので指定された量だけ巡回(rotate)した結果を返します。
      二つめの演算対象は -precision から precision までの範囲の整数でなければなりません。
      この二つめの演算対象の絶対値が何桁ずらすかを決めます。
      そしてもし正の数ならば巡回の方向は左に、そうでなければ右になります。
      一つめの演算対象の仮数部は必要ならば精度いっぱいまでゼロで埋められます。
      符号と指数は変えられません。

      .. versionadded:: 2.6

   .. method:: same_quantum(other[, context])

      *self* と *other* が同じ指数を持っているか、あるいは\
      双方とも :const:`NaN` である場合に真を返します。

   .. method:: scaleb(other[, context])

      二つめの演算対象で調整された指数の一つめの演算対象を返します。
      同じことですが、一つめの演算対象を ``10**other`` 倍したものを返します。
      二つめの演算対象は整数でなければなりません。

      .. versionadded:: 2.6

   .. method:: shift(other[, context])

      一つめの演算対象の数字を二つめので指定された量だけシフトした結果を返します。
      二つめの演算対象は -precision から precision までの範囲の整数でなければなりません。
      この二つめの演算対象の絶対値が何桁ずらすかを決めます。
      そしてもし正の数ならばシフトの方向は左に、そうでなければ右になります。
      一つめの演算対象の係数は必要ならば精度いっぱいまでゼロで埋められます。
      符号と指数は変えられません。

      .. versionadded:: 2.6

   .. method:: sqrt([context])

      平方根を精度いっぱいまで求めます。


   .. method:: to_eng_string([context])

      数値を工学で用いられる形式 (工学表記; enginnering notation)
      の文字列に変換します。

      工学表記では指数は 3 の倍数になります。従って、
      最大で 3 桁までの数字が基数の小数部に現れます。
      たとえば、 ``Decimal('123E+1')`` は
      ``Decimal('1.23E+3')`` に変換されます。

   .. method:: to_integral([rounding[, context]])

      :const:`Inexact` や :const:`Rounded` といったシグナルを出さずに\
      最近傍の整数に値を丸めます。 *rounding* が指定されていれば適用\
      されます; それ以外の場合、値丸めの方法は *context* の設定か現在の\
      コンテキストの設定になります。

   .. method:: to_integral_exact([rounding[, context]])

      最近傍の整数に値を丸め、丸めが起こった場合には :const:`Inexact`
      または :const:`Rounded` のシグナルを適切に出します。
      丸めモードは以下のように決められます。
      ``rounding`` 引数が与えられていればそれが使われます。
      そうでなければ ``context`` 引数で決まります。
      どちらの引数も渡されなければ現在のスレッドのコンテキストの丸めモードが使われます。

      .. versionadded:: 2.6

   .. method:: to_integral_value([rounding[, context]])

      :const:`Inexact` や :const:`Rounded` といったシグナルを出さずに\
      最近傍の整数に値を丸めます。 *rounding* が指定されていれば適用\
      されます; それ以外の場合、値丸めの方法は *context* の設定か現在の\
      コンテキストの設定になります。

      .. versionchanged:: 2.6
         ``to_integral`` から ``to_integral_value`` に改名されました。
         古い名前も互換性のために残されています。

.. _logical_operands_label:

論理引数
^^^^^^^^

:meth:`logical_and`, :meth:`logical_invert`, :meth:`logical_or`, および
:meth:`logical_xor` メソッドはその引数が *論理引数* であると想定しています。
*論理引数* とは :class:`Decimal` インスタンスで指数と符号は共にゼロであり、
各桁の数字が :const:`0` か :const:`1` であるものです。

.. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


.. _decimal-context:

Context オブジェクト
--------------------

コンテキスト (context) とは、算術演算における環境設定です。
コンテキストは計算精度を決定し、値丸めの方法を設定し、
シグナルのどれが例外になるかを決め、指数の範囲を制限しています。

多重スレッドで処理を行う場合には各スレッドごとに現在のコンテキストが\
あり、 :func:`getcontext` や :func:`setcontext` といった関数で\
アクセスしたり設定変更できます:


.. function:: getcontext()

   アクティブなスレッドの現在のコンテキストを返します。


.. function:: setcontext(c)

   アクティブなスレッドのコンテキストを *c* に設定します。

Python 2.5 から、 :keyword:`with` 文と :func:`localcontext` 関数を使っ\
て実行するコンテキストを一時的に変更することもできるようになりました。


.. function:: localcontext([c])

   with 文の入口でアクティブなスレッドのコンテキストを *c* のコピー\
   に設定し、with 文を抜ける時に元のコンテキストに復旧する、コンテキスト\
   マネージャを返します。コンテキストが指定されなければ、現在のコンテキ\
   ストのコピーが使われます。

   .. versionadded:: 2.5

   たとえば、以下のコードでは精度を42桁に設定し、計算を実行し、そして\
   元のコンテキストに復帰します。  ::

      from decimal import localcontext

      with localcontext() as ctx:
          ctx.prec = 42   # 高精度の計算を実行
          s = calculate_something()
      s = +s  # 最終的な結果をデフォルトの精度に丸める

新たなコンテキストは、以下で説明する :class:`Context` コンストラクタを\
使って生成できます。その他にも、 :mod:`decimal` モジュールでは\
作成済みのコンテキストを提供しています:


.. class:: BasicContext

   汎用10進演算仕様で定義されている標準コンテキストの一つです。
   精度は 9 桁に設定されています。丸め規則は :const:`ROUND_HALF_UP`
   です。すべての演算結果フラグはクリアされています。
   :const:`Inexact`, :const:`Rounded`, :const:`Subnormal`
   を除く全ての演算エラートラップが有効 (例外として扱う) になっています。

   多くのトラップが有効になっているので、デバッグの際に便利なコンテキストです。


.. class:: ExtendedContext

   汎用10進演算仕様で定義されている標準コンテキストの一つです。
   精度は 9 桁に設定されています。丸め規則は :const:`ROUND_HALF_EVEN`
   です。すべての演算結果フラグはクリアされています。トラップは全て無効\
   (演算中に一切例外を送出しない) になっています。

   トラップが無効になっているので、エラーの伴う演算結果を :const:`NaN` や
   :const:`Infinity` にし、例外を送出しないようにしたいアプリケーションに\
   向いたコンテキストです。このコンテキストを使うと、他の場合にはプログラム\
   が停止してしまうような状況があっても実行を完了させられます。


.. class:: DefaultContext

   :class:`Context` コンストラクタが新たなコンテキストを作成するさいに
   雛形にするコンテキストです。このコンテキストのフィールド (精度の設定など)
   を変更すると、 :class:`Context` コンストラクタが生成する新たなコンテキストに
   影響を及ぼします。

   このコンテキストは、主に多重スレッド環境で便利です。スレッドを開始する\
   前に何らかのフィールドを変更しておくと、システム全体のデフォルト設定\
   に効果を及ぼせます。スレッドを開始した後にフィールドを変更すると\
   競合条件を抑制するためにスレッドを同期化せねばならないので推奨しません。

   単一スレッドの環境では、このコンテキストを使わないよう薦めます。
   下で述べるように明示的にコンテキストを作成してください。

   デフォルトの値は精度 28 桁、丸め規則 :const:`ROUND_HALF_EVEN` で、トラップ
   :const:`Overflow`, :const:`InvalidOperation`, および :const:`DivisionByZero`
   が有効になっています。

上に挙げた三つのコンテキストに加え、 :class:`Context` コンストラクタを\
使って新たなコンテキストを生成できます。


.. class:: Context(prec=None, rounding=None, traps=None, flags=None, Emin=None, Emax=None, capitals=1)

   新たなコンテキストを生成します。あるフィールドが定義されていないか :const:`None`
   であれば、 :const:`DefaultContext` からデフォルト値をコピーします。
   *flags* フィールドが設定されていいか :const:`None` の場合には、
   全てのフラグがクリアされます。

   *prec* フィールドは正の整数で、コンテキストにおける算術演算の\
   計算精度を設定します。

   *rounding* は、

   * :const:`ROUND_CEILING` (:const:`Infinity` 寄りの値にする),
   * :const:`ROUND_DOWN` (ゼロ寄りの値にする),
   * :const:`ROUND_FLOOR` (:const:`-Infinity` 寄りの値にする),
   * :const:`ROUND_HALF_DOWN` (最近値のうちゼロ寄りの値にする),
   * :const:`ROUND_HALF_EVEN` (最近値のうち偶数値を優先する),
   * :const:`ROUND_HALF_UP` (最近値のうちゼロから遠い値にする),
   * :const:`ROUND_UP` (ゼロから遠い値にする), または
   * :const:`ROUND_05UP` (ゼロに向かって丸めた後の最小の桁が 0 か 5
      ならばゼロから遠い値にし、そうでなければゼロにする)

   のいずれかです。

   *traps* および *flags* フィールドには、セットしたい\
   シグナルを列挙します。一般的に、新たなコンテキストを作成するときには\
   トラップだけを設定し、フラグはクリアしておきます。

   *Emin* および *Emax* フィールドには、指数範囲の外側限界値を整数で\
   指定します。

   *capitals* フィールドは :const:`0` または :const:`1` (デフォルト)
   にします。 :const:`1` に設定すると、指数記号を大文字 :const:`E` で\
   出力します。それ以外の場合には  :const:`Decimal('6.02e+23')`
   のように :const:`e` を使います。

   .. versionchanged:: 2.6
      :const:`ROUND_05UP` 丸めモードが追加されました。

   :class:`Context` クラスでは、いくつかの汎用のメソッドの他、現在の\
   コンテキストで算術演算を直接行うためのメソッドを数多く定義しています。
   加えて、 :class:`Decimal` の各メソッドについて(:meth:`adjusted` および
   :meth:`as_tuple` メソッドを例外として)対応する :class:`Context`
   のメソッドが存在します。たとえば、 :class:`Context` インスタンス  ``C``
   と :class:`Decimal` インスタンス ``x`` に対して、 ``C.exp(x)`` は
   ``x.exp(context=C)`` と等価です。
   それぞれの :class:`Context` メソッドは、Decimal インスタンスが受け付けられる
   ところならどこでも、Python の整数 (:class:`int` または :class:`long` の
   インスタンス) を受け付けます。

   .. method:: clear_flags()

      フラグを全て :const:`0` にリセットします。


   .. method:: copy()

      コンテキストの複製を返します。

   .. method:: copy_decimal(num)

      Decimal インスタンス num のコピーを返します。

   .. method:: create_decimal(num)

      *self* をコンテキストとする新たな Decimal インスタンスを *num* から生成します。
      :class:`Decimal` コンストラクタと違い、
      数値を変換する際にコンテキストの精度、値丸め方法、フラグ、トラップ\
      を適用します。

      定数値はしばしばアプリケーションの要求よりも高い精度を持っているため、
      このメソッドが役に立ちます。また、値丸めを即座に行うため、
      例えば以下のように、入力値に値丸めを行わないために合計値にゼロの加算を\
      追加するだけで結果が変わってしまうといった、現在の精度
      よりも細かい値の影響が紛れ込む問題を防げるという恩恵もあります。
      以下の例は、丸められていない入力を使うということは和にゼロを加えると\
      結果が変わり得るという見本です :

      .. doctest:: newcontext

         >>> getcontext().prec = 3
         >>> Decimal('3.4445') + Decimal('1.0023')
         Decimal('4.45')
         >>> Decimal('3.4445') + Decimal(0) + Decimal('1.0023')
         Decimal('4.44')

      このメソッドは IBM 仕様の to-number 演算を実装したものです。
      引数が文字列の場合、前や後ろに余計な空白を付けることは許されません。

   .. method:: create_decimal_from_float(f)

      浮動小数点数 *f* から新しい Decimal インスタンスを生成しますが、
      *self* をコンテキストとして丸めます。 :meth:`Decimal.from_float`
      クラスメソッドとは違い、変換にコンテキストの精度、丸めメソッド、
      フラグ、そしてトラップが適用されます。

      .. doctest::

         >>> context = Context(prec=5, rounding=ROUND_DOWN)
         >>> context.create_decimal_from_float(math.pi)
         Decimal('3.1415')
         >>> context = Context(prec=5, traps=[Inexact])
         >>> context.create_decimal_from_float(math.pi)
         Traceback (most recent call last):
             ...
         Inexact: None

      .. versionadded:: 2.7

   .. method:: Etiny()

      ``Emin - prec + 1`` に等しい値を返します。
      演算結果の劣化が起こる桁の最小値です。アンダーフローが起きた場合、
      指数は :const:`Etiny` に設定されます。


   .. method:: Etop()

      ``Emax - prec + 1`` に等しい値を返します。

   :class:`Decimal` を使った処理を行う場合、通常は :class:`Decimal`
   インスタンスを生成して、算術演算を適用するというアプローチを\
   とります。演算はアクティブなスレッドにおける現在のコンテキストの\
   下で行われます。もう一つのアプローチは、コンテキストのメソッドを\
   使った特定のコンテキスト下での計算です。コンテキストのメソッドは
   :class:`Decimal` クラスのメソッドに似ているので、
   ここでは簡単な説明にとどめます。


   .. method:: abs(x)

      *x* の絶対値を返します。


   .. method:: add(x, y)

      *x* と *y* の和を返します。


   .. method:: canonical(x)

      同じ Decimal オブジェクト *x* を返します。

   .. method:: compare(x, y)

      二つの値を数値として比較します。

   .. method:: compare_signal(x, y)

      二つの演算対象の値を数値として比較します。

   .. method:: compare_total(x, y)

      二つの演算対象を抽象的な表現を使って比較します。

   .. method:: compare_total_mag(x, y)

      二つの演算対象を抽象的な表現を使い符号を無視して比較します。

   .. method:: copy_abs(x)

      *x* のコピーの符号を 0 にセットして返します。

   .. method:: copy_negate(x)

      *x* のコピーの符号を反転して返します。

   .. method:: copy_sign(x, y)

      *y* から *x* に符号をコピーします。

   .. method:: divide(x, y)

      *x* を *y* で除算した値を返します。

   .. method:: divide_int(x, y)

      *x* を *y* で除算した値を整数に切り捨てて返します。

   .. method:: divmod(x, y)

      二つの数値間の除算を行い、結果の整数部を返します。

      .. FIXME: this isn't a correct description

   .. method:: exp(x)

      `e ** x` を返します。

   .. method:: fma(x, y, z)

      *x* を *y* 倍したものに *z* を加えて返します。

   .. method:: is_canonical(x)

      *x* が標準的(canonical)ならば True を返します。そうでなければ False です。

   .. method:: is_finite(x)

      *x* が有限ならば True を返します。そうでなければ False です。

   .. method:: is_infinite(x)

      *x* が無限ならば True を返します。そうでなければ False です。

   .. method:: is_nan(x)

      *x* が NaN か sNaN であれば True を返します。そうでなければ False です。

   .. method:: is_normal(x)

      *x* が通常の数ならば True を返します。そうでなければ False です。

   .. method:: is_qnan(x)

      *x* が無言 NaN であれば True を返します。そうでなければ False です。

   .. method:: is_signed(x)

      *x* が負の数であれば True を返します。そうでなければ False です。

   .. method:: is_snan(x)

      *x* が発信 NaN であれば True を返します。そうでなければ False です。

   .. method:: is_subnormal(x)

      *x* が非正規数であれば True を返します。そうでなければ False です。

   .. method:: is_zero(x)

      *x* がゼロであれば True を返します。そうでなければ False です。

   .. method:: ln(x)

      *x* の自然対数(底 e の対数)を返します。

   .. method:: log10(x)

      *x* の底 10 の対数を返します。

   .. method:: logb(x)

      演算対象の MSD の大きさの指数部を返します。

   .. method:: logical_and(x, y)

      それぞれの桁に論理演算 *and* を当てはめます。

   .. method:: logical_invert(x)

      *x* の全ての桁を反転させます。

   .. method:: logical_or(x, y)

      それぞれの桁に論理演算 *or* を当てはめます。

   .. method:: logical_xor(x, y)

      それぞれの桁に論理演算 *xor* を当てはめます。

   .. method:: max(x, y)

      二つの値を数値として比較し、大きいほうを返します。


   .. method:: max_mag(x, y)

      値を符号を無視して数値として比較します。


   .. method:: min(x, y)

      二つの値を数値として比較し、小さいほうを返します。


   .. method:: min_mag(x, y)

      値を符号を無視して数値として比較します。


   .. method:: minus(x)

      Python における単項マイナス演算子に対応する演算です。


   .. method:: multiply(x, y)

      *x* と *y* の積を返します。

   .. method:: next_minus(x)

      *x* より小さい最大の表現可能な数を返します。


   .. method:: next_plus(x)

      *x* より大きい最小の表現可能な数を返します。


   .. method:: next_toward(x, y)

      *x* に *y* の方向に向かって最も近い数を返します。


   .. method:: normalize(x)

      *x* をもっとも単純な形にします。

   .. method:: number_class(x)

      *x* のクラスを指し示すものを返します。


   .. method:: plus(x)

      Python における単項のプラス演算子に対応する演算です。
      コンテキストにおける精度や値丸めを適用するので、
      等値 (identity) 演算とは *違います* 。


   .. method:: power(x, y[, modulo])

      ``x`` の ``y`` 乗を計算します。 *modulo* が指定されていればモジュロを取ります。

      二引数であれば ``x**y`` を計算します。 ``x`` が負であれば
      ``y`` は整でなければなりません。
      結果は ``y`` が整であって結果が有限になり 'precision'
      桁で正確に表現できるのでなければ不正確になります。
      その結果は現スレッドのコンテキストの丸めモードを使って正しく丸められます。

      三引数であれば ``(x**y) % modulo`` を計算します。
      この形式の場合、以下の制限が引数に掛かります:

         - 全ての引数は整
         - ``y`` は非負でなければならない
         - ``x`` と ``y`` の少なくともどちらかはゼロでない
         - ``modulo`` は非零で大きくても 'precision' 桁

      ``Context.power(x, y, modulo)`` で得られる値は ``(x**y) % modulo``
      を精度無制限で計算して得られるものと同じ値ですが、より効率的に計算されます。
      結果の指数は ``x``, ``y``, ``modulo`` の指数に関係なくゼロです。
      この計算は常に正確です。

      .. versionchanged:: 2.6
         ``x**y`` 形式で ``y`` が非整数で構わないことになった。
         三引数バージョンに対するより厳格な要求。


   .. method:: quantize(x, y)

      *x* に値丸めを適用し、指数を *y* にした値を返します。

   .. method:: radix()

      単に 10 を返します。何せ十進ですから :)


   .. method:: remainder(x, y)

      整数除算の剰余を返します。

      剰余がゼロでない場合、符号は割られる数の符号と同じになります。


   .. method:: remainder_near(x, y)

      ``x - y * n`` を返します。ここで *n* は ``x / y`` の正確な値に一番近い整数です
      (この結果が 0 ならばその符号は *x* の符号と同じです)。


   .. method:: rotate(x, y)

      *x* の *y* 回巡回したコピーを返します。


   .. method:: same_quantum(x, y)

      *self* と *other* が同じ指数を持っているか、あるいは\
      双方とも :const:`NaN` である場合に真を返します。


   .. method:: scaleb (x, y)

      一つめの演算対象の指数部に二つめの値を加えたものを返します。


   .. method:: shift(x, y)

      *x* を *y* 回シフトしたコピーを返します。


   .. method:: sqrt(x)

      *x* の平方根を精度いっぱいまで求めます。


   .. method:: subtract(x, y)

      *x* と *y* の間の差を返します。


   .. method:: to_eng_string()

      工学表記で文字列に変換します。


   .. method:: to_integral(x)

      最近傍の整数に値を丸めます。


   .. method:: to_sci_string(x)

      数値を科学表記で文字列に変換します。

.. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


.. _decimal-signals:

シグナル
--------

シグナルは、計算中に生じた様々なエラー条件を表現します。
各々のシグナルは一つのコンテキストフラグと一つのトラップイネーブラに\
対応しています。

コンテキストフラグは、該当するエラー条件に遭遇するたびにセットされます。
演算後にフラグを調べれば、演算に関する情報
(例えば計算が厳密だったかどうか) がわかります。
フラグを調べたら、次の計算を始める前にフラグを全てクリアするように\
してください。

あるコンテキストのトラップイネーブラがあるシグナルに対してセット\
されている場合、該当するエラー条件が生じると Python の例外を送出\
します。例えば、 :class:`DivisionByZero` が設定されていると、\
エラー条件が生じた際に :exc:`DivisionByZero`
例外を送出します。


.. class:: Clamped

   値の表現上の制限に沿わせるために指数部が変更されたことを通知します。

   通常、クランプ (clamp) は、指数部がコンテキストにおける指数桁の制限値
   :attr:`Emin` および :attr:`Emax` を越えた場合に発生します。
   可能な場合には、係数部にゼロを加えた表現に合わせて指数部を減らします。


.. class:: DecimalException

   他のシグナルの基底クラスで、 :exc:`ArithmeticError` の\
   サブクラスです。


.. class:: DivisionByZero

   有限値をゼロで除算したときのシグナルです。

   除算やモジュロ除算、数を負の値で累乗した場合に起きることがあります。
   このシグナルをトラップしない場合、演算結果は :const:`Infinity` または
   :const:`-Infinity` になり、その符号は演算に使った入力に基づいて決まります。


.. class:: Inexact

   値の丸めによって演算結果から厳密さが失われたことを通知します。

   このシグナルは値丸め操作中にゼロでない桁を無視した際に生じます。
   演算結果は値丸め後の値です。シグナルのフラグやトラップは、\
   演算結果の厳密さが失われたことを検出するために使えるだけです。


.. class:: InvalidOperation

   無効な演算が実行されたことを通知します。

   ユーザが有意な演算結果にならないような操作を要求したことを示します。
   このシグナルをトラップしない場合、 :const:`NaN` を返します。
   このシグナルの発生原因として考えられるのは、以下のような状況です::

      Infinity - Infinity
      0 * Infinity
      Infinity / Infinity
      x % 0
      Infinity % x
      x._rescale( non-integer )
      sqrt(-x) and x > 0
      0 ** 0
      x ** (non-integer)
      x ** Infinity


.. class:: Overflow

   数値オーバフローを示すシグナルです。

   このシグナルは、値丸めを行った後の指数部が :attr:`Emax` より大きいことを\
   示します。シグナルをトラップしない場合、演算結果は値丸めのモードにより、\
   表現可能な最大の数値になるように内側へ引き込んで丸めを行った値か、
   :const:`Infinity` になるように外側に丸めた値のいずれかになります。
   いずれの場合も、 :class:`Inexact` および :class:`Rounded` が同時に\
   シグナルされます。


.. class:: Rounded

   情報が全く失われていない場合も含み、値丸めが起きたときのシグナルです。

   このシグナルは、値丸めによって桁がなくなると常に発生します。
   なくなった桁がゼロ (例えば :const:`5.00` を丸めて :const:`5.0`
   になった場合) であってもです。このシグナルをトラップしなければ、\
   演算結果をそのまま返します。このシグナルは有効桁数の減少を検出\
   する際に使います。


.. class:: Subnormal

   値丸めを行う前に指数部が :attr:`Emin` より小さかったことを示す\
   シグナルです。

   演算結果が微小である場合 (指数が小さすぎる場合) に発生します。
   このシグナルをトラップしなければ、演算結果をそのまま返します。


.. class:: Underflow

   演算結果が値丸めによってゼロになった場合に生じる数値アンダフローです。

   演算結果が微小なため、値丸めによってゼロになった場合に発生します。
   :class:`Inexact` および :class:`Subnormal`
   シグナルも同時に発生します。

これらのシグナルの階層構造をまとめると、以下の表のようになります::

   exceptions.ArithmeticError(exceptions.StandardError)
       DecimalException
           Clamped
           DivisionByZero(DecimalException, exceptions.ZeroDivisionError)
           Inexact
               Overflow(Inexact, Rounded)
               Underflow(Inexact, Rounded, Subnormal)
           InvalidOperation
           Rounded
           Subnormal

.. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


.. _decimal-notes:

浮動小数点数に関する注意
------------------------


精度を上げて丸め誤差を抑制する
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

10進浮動小数点数を使うと、10進数表現による誤差を抑制できます
(:const:`0.1` を正確に表現できるようになります); しかし、ゼロでない\
桁が一定の精度を越えている場合には、演算によっては依然として値丸めによる\
誤差を引き起こします。 Knuth は、十分でない計算精度の下で値丸めを伴う\
浮動小数点演算を行った結果、加算の結合則や分配則における恒等性が崩れて\
しまう例を二つ示しています:

.. doctest:: newcontext

   # Examples from Seminumerical Algorithms, Section 4.2.2.
   >>> from decimal import Decimal, getcontext
   >>> getcontext().prec = 8

   >>> u, v, w = Decimal(11111113), Decimal(-11111111), Decimal('7.51111111')
   >>> (u + v) + w
   Decimal('9.5111111')
   >>> u + (v + w)
   Decimal('10')

   >>> u, v, w = Decimal(20000), Decimal(-6), Decimal('6.0000003')
   >>> (u*v) + (u*w)
   Decimal('0.01')
   >>> u * (v+w)
   Decimal('0.0060000')

:mod:`decimal` モジュールでは、最下桁を失わないように十分に計算精度を\
広げることで、上で問題にしたような恒等性をとりもどせます:

.. doctest:: newcontext

   >>> getcontext().prec = 20
   >>> u, v, w = Decimal(11111113), Decimal(-11111111), Decimal('7.51111111')
   >>> (u + v) + w
   Decimal('9.51111111')
   >>> u + (v + w)
   Decimal('9.51111111')
   >>>
   >>> u, v, w = Decimal(20000), Decimal(-6), Decimal('6.0000003')
   >>> (u*v) + (u*w)
   Decimal('0.0060000')
   >>> u * (v+w)
   Decimal('0.0060000')


特殊値
^^^^^^

:mod:`decimal` モジュールの数体系では、 :const:`NaN`, :const:`sNaN`,
:const:`-Infinity`, :const:`Infinity`, および二つのゼロ、 :const:`+0`
と :const:`-0` といった特殊な値を提供しています。

無限大 (Infinity) は ``Decimal('Infinity')`` で直接構築できます。
また、 :exc:`DivisionByZero` をトラップせずにゼロで除算を行った\
場合にも出てきます。同様に、 :exc:`Overflow` シグナルをトラップ\
しなければ、表現可能な最大の数値の制限を越えた値を丸めたときに出てきます。

無限大には符号があり (アフィン: affine であり)、算術演算に使用でき、\
非常に巨大で不確定の(indeterminate)値として扱われます。例えば、無限大に\
何らかの定数を加算すると、演算結果は別の無限大になります。

演算によっては結果が不確定になるものがあり、 :const:`NaN` を返します。
ただし、 :exc:`InvalidOperation` シグナルをトラップするように\
なっていれば例外を送出します。

例えば、 ``0/0`` は :const:`NaN` を返します。 :const:`NaN` は\
「非数値 (not a number)」を表します。このような :const:`NaN` は\
暗黙のうちに生成され、一度生成されるとそれを他の計算にも流れてゆき、\
関係する個々の演算全てが個別の :const:`NaN` を返すようになります。
この挙動は、たまに入力値が欠けるような状況で一連の計算を行う際に\
便利です --- 特定の計算に対しては無効な結果を示すフラグを立てつつ\
計算を進められるからです。

一方、 :const:`NaN` の変種である :const:`sNaN` は関係する全ての演算\
で演算後にシグナルを送出します。 :const:`sNaN` は、無効な演算結果\
に対して特別な処理を行うために計算を停止する必要がある場合に便利です。

Python の比較演算は :const:`NaN` が関わってくると少し驚くようなことがあります。
等価性のテストの一方の対象が無言または発信 :const:`NaN` である場合いつでも
:const:`False` を返し(たとえ ``Decimal('NaN')==Decimal('NaN')`` でも)、
一方で不等価をテストするといつでも :const:`True` を返します。
二つの Decimal を ``<``, ``<=``, ``>`` または ``>=`` を使って比較する試みは
一方が :const:`NaN` である場合には :exc:`InvalidOperation` シグナルを誘発し、
このシグナルをトラップしなければ結果は :const:`False` に終わります。
汎用10進演算仕様は直接の比較の振る舞いについて定めていないことに注意しておきましょう。
ここでの :const:`NaN` が関係する比較ルールは IEEE 854 標準から持ってきました
(section 5.7 の Table 3 を見て下さい)。
厳格に標準遵守を貫くなら、 :meth:`compare` および :meth:`compare-signal`
メソッドを代わりに使いましょう。

アンダフローの起きた計算は、符号付きのゼロ (signed zero) を返す\
ことがあります。符号は、より高い精度で計算を行った結果の\
符号と同じになります。
符号付きゼロの大きさはやはりゼロなので、正のゼロと負のゼロは\
等しいとみなされ、符号は単なる参考にすぎません。

二つの符号付きゼロが区別されているのに等価であることに加えて、
異なる精度におけるゼロの表現はまちまちなのに、値は等価と\
みなされるということがあります。これに慣れるには多少時間がかかります。
正規化浮動小数点表現に目が慣れてしまうと、以下の計算でゼロに\
等しい値が返っているとは即座に分かりません:

   >>> 1 / Decimal('Infinity')
   Decimal('0E-1000000026')

.. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


.. _decimal-threads:

スレッドを使った処理
--------------------

関数 :func:`getcontext` は、スレッド毎に別々の :class:`Context`
オブジェクトにアクセスします。別のスレッドコンテキストを持つということは、
複数のスレッドが互いに影響を及ぼさずに
(``getcontext.prec=10`` のような) 変更を適用できるということです。

同様に、\ :func:`setcontext` 関数は自動的に引数のコンテキスト\
を現在のスレッドのコンテキストに設定します。

:func:`getcontext` を呼び出す前に :func:`setcontext` が\
呼び出されていなければ、現在のスレッドで使うための新たなコンテキスト\
を生成するために :func:`getcontext` が自動的に呼び出されます。

新たなコンテキストは、\ *DefaultContext* と呼ばれる雛形から\
コピーされます。アプリケーションを通じて全てのスレッドに同じ\
値を使うようにデフォルトを設定したければ、\ *DefaultContext*
オブジェクトを直接変更します。
:func:`getcontext` を呼び出す\
スレッド間で競合条件が生じないようにするため、\ *DefaultContext*
への変更はいかなるスレッドを開始するよりも *前に* 行わねば\
なりません。以下に例を示します::

   # スレッドを立ち上げる前にアプリケーションにわたるデフォルトを設定
   DefaultContext.prec = 12
   DefaultContext.rounding = ROUND_DOWN
   DefaultContext.traps = ExtendedContext.traps.copy()
   DefaultContext.traps[InvalidOperation] = 1
   setcontext(DefaultContext)

   # その後でスレッドを開始
   t1.start()
   t2.start()
   t3.start()
    . . .

.. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


.. _decimal-recipes:

レシピ
------

:class:`Decimal` クラスの利用を実演している例をいくつか示します。
これらはユーティリティ関数としても利用できます::

   def moneyfmt(value, places=2, curr='', sep=',', dp='.',
                pos='', neg='-', trailneg=''):
       """Decimal を通貨表現の文字列に変換します。

       places:  小数点以下の値を表すのに必要な桁数
       curr:    符号の前に置く通貨記号 (オプションで、空でもかまいません)
       sep:     桁のグループ化に使う記号、オプションです (コンマ、ピリオド、
                スペース、または空)
       dp:      小数点 (コンマまたはピリオド)
                小数部がゼロの場合には空にできます。
       pos:     正数の符号オプション: '+', 空白または空文字列
       neg:     負数の符号オプション: '-', '(', 空白または空文字列
       trailneg:後置マイナス符号オプション:  '-', ')', 空白または空文字列

       >>> d = Decimal('-1234567.8901')
       >>> moneyfmt(d, curr='$')
       '-$1,234,567.89'
       >>> moneyfmt(d, places=0, sep='.', dp='', neg='', trailneg='-')
       '1.234.568-'
       >>> moneyfmt(d, curr='$', neg='(', trailneg=')')
       '($1,234,567.89)'
       >>> moneyfmt(Decimal(123456789), sep=' ')
       '123 456 789.00'
       >>> moneyfmt(Decimal('-0.02'), neg='<', trailneg='>')
       '<0.02>'

       """
       q = Decimal(10) ** -places      # 2 places --> '0.01'
       sign, digits, exp = value.quantize(q).as_tuple()
       result = []
       digits = map(str, digits)
       build, next = result.append, digits.pop
       if sign:
           build(trailneg)
       for i in range(places):
           build(next() if digits else '0')
       build(dp)
       if not digits:
           build('0')
       i = 0
       while digits:
           build(next())
           i += 1
           if i == 3 and digits:
               i = 0
               build(sep)
       build(curr)
       build(neg if sign else pos)
       return ''.join(reversed(result))

   def pi():
       """現在の精度まで円周率を計算します。

       >>> print pi()
       3.141592653589793238462643383

       """
       getcontext().prec += 2  # 中間ステップのための余分の数字
       three = Decimal(3)      # 普通の float に対する "three=3.0" の代わり
       lasts, t, s, n, na, d, da = 0, three, 3, 1, 0, 0, 24
       while s != lasts:
           lasts = s
           n, na = n+na, na+8
           d, da = d+da, da+32
           t = (t * n) / d
           s += t
       getcontext().prec -= 2
       return +s               # 単項のプラスで新しい精度に変換します

   def exp(x):
       """e の x 乗を返します。結果の型は入力の型と同じです。

       >>> print exp(Decimal(1))
       2.718281828459045235360287471
       >>> print exp(Decimal(2))
       7.389056098930650227230427461
       >>> print exp(2.0)
       7.38905609893
       >>> print exp(2+0j)
       (7.38905609893+0j)

       """
       getcontext().prec += 2
       i, lasts, s, fact, num = 0, 0, 1, 1, 1
       while s != lasts:
           lasts = s
           i += 1
           fact *= i
           num *= x
           s += num / fact
       getcontext().prec -= 2
       return +s

   def cos(x):
       """x ラジアンの余弦を返します。

       >>> print cos(Decimal('0.5'))
       0.8775825618903727161162815826
       >>> print cos(0.5)
       0.87758256189
       >>> print cos(0.5+0j)
       (0.87758256189+0j)

       """
       getcontext().prec += 2
       i, lasts, s, fact, num, sign = 0, 0, 1, 1, 1, 1
       while s != lasts:
           lasts = s
           i += 2
           fact *= i * (i-1)
           num *= x * x
           sign *= -1
           s += num / fact * sign
       getcontext().prec -= 2
       return +s

   def sin(x):
       """x ラジアンの正弦を返します。

       >>> print sin(Decimal('0.5'))
       0.4794255386042030002732879352
       >>> print sin(0.5)
       0.479425538604
       >>> print sin(0.5+0j)
       (0.479425538604+0j)

       """
       getcontext().prec += 2
       i, lasts, s, fact, num, sign = 1, 0, x, 1, x, 1
       while s != lasts:
           lasts = s
           i += 2
           fact *= i * (i-1)
           num *= x * x
           sign *= -1
           s += num / fact * sign
       getcontext().prec -= 2
       return +s


.. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


.. _decimal-faq:

Decimal FAQ
-----------

Q. ``decimal.Decimal('1234.5')`` などと打ち込むのは煩わしいのですが、
対話式インタプリタを使う際にタイプ量を少なくする方法はありませんか?

A. コンストラクタを1文字に縮める人もいるようです。 :

   >>> D = decimal.Decimal
   >>> D('1.23') + D('3.45')
   Decimal('4.68')


Q. 小数点以下2桁の固定小数点数のアプリケーションの中で、いくつかの\
入力が余計な桁を保持しているのでこれを丸めなければなりません。その他の\
ものに余計な桁はなくそのまま使えます。どのメソッドを使うのがいいでしょうか?

A. :meth:`quantize` メソッドで固定した桁に丸められます。
:const:`Inexact` トラップを設定しておけば、確認にも有用です。
:

   >>> TWOPLACES = Decimal(10) ** -2       # Decimal('0.01') と同じ

   >>> # 小数点以下2桁に丸める
   >>> Decimal('3.214').quantize(TWOPLACES)
   Decimal('3.21')

   >>> # 小数点以下2桁を越える桁を保持していないことの確認
   >>> Decimal('3.21').quantize(TWOPLACES, context=Context(traps=[Inexact]))
   Decimal('3.21')

   >>> Decimal('3.214').quantize(TWOPLACES, context=Context(traps=[Inexact]))
   Traceback (most recent call last):
      ...
   Inexact: None

Q. 正当な2桁の入力が得られたとして、その正当性をアプリケーション実行中も\
変わらず保ち続けるにはどうすればいいでしょうか?

A. 加減算あるいは整数との乗算のような演算は自動的に固定小数点を守ります。
その他の除算や整数以外の乗算などは小数点以下の桁を変えてしまいますので実行後は
:meth:`quantize` ステップが必要です。:

    >>> a = Decimal('102.72')           # Initial fixed-point values
    >>> b = Decimal('3.17')
    >>> a + b                           # Addition preserves fixed-point
    Decimal('105.89')
    >>> a - b
    Decimal('99.55')
    >>> a * 42                          # So does integer multiplication
    Decimal('4314.24')
    >>> (a * b).quantize(TWOPLACES)     # Must quantize non-integer multiplication
    Decimal('325.62')
    >>> (b / a).quantize(TWOPLACES)     # And quantize division
    Decimal('0.03')

固定小数点のアプリケーションを開発する際は、 :meth:`quantize`
の段階を扱う関数を定義しておくと便利です :

    >>> def mul(x, y, fp=TWOPLACES):
    ...     return (x * y).quantize(fp)
    >>> def div(x, y, fp=TWOPLACES):
    ...     return (x / y).quantize(fp)

    >>> mul(a, b)                       # 自動的に固定点を保つ
    Decimal('325.62')
    >>> div(b, a)
    Decimal('0.03')

Q. 一つの値に対して多くの表現方法があります。
:const:`200` と :const:`200.000` と :const:`2E2` と
:const:`.02E+4` は全て同じ値で違った精度の数です。これらをただ一つの\
正規化された値に変換することはできますか?

A. :meth:`normalize` メソッドは全ての等しい値をただ一つの表現に直します。 :

   >>> values = map(Decimal, '200 200.000 2E2 .02E+4'.split())
   >>> [v.normalize() for v in values]
   [Decimal('2E+2'), Decimal('2E+2'), Decimal('2E+2'), Decimal('2E+2')]

Q. ある種の10進数値はいつも指数表記で表示されます。
指数表記以外の表示にする方法はありますか?

A. 値によっては、指数表記だけが有効桁数を表せる表記法なのです。
たとえば、 :const:`5.0E+3` を :const:`5000` と表してしまうと、
値は変わりませんが元々の2桁という有効数字が反映されません。

もしアプリケーションが有効数字の追跡を等閑視するならば、
指数部や末尾のゼロを取り除き、有効数字を忘れ、しかし値を変えずにおくことは容易です::

    def remove_exponent(d):
        '''Remove exponent and trailing zeros.

        >>> remove_exponent(Decimal('5E+3'))
        Decimal('5000')

        '''
        return d.quantize(Decimal(1)) if d == d.to_integral() else d.normalize()

Q. 普通の float を :class:`Decimal` に変換できますか?

A. はい。どんな 2 進浮動小数点数も Decimal として正確に表現できます。
ただし、正確な変換は直感的に考えたよりも多い桁になることがあります。

.. doctest::

    >>> Decimal(math.pi)
    Decimal('3.141592653589793115997963468544185161590576171875')

Q. 複雑な計算の中で、精度不足や丸めの異常で間違った結果になっていない\
ことをどうやって保証すれば良いでしょうか?

A. decimal モジュールでは検算は容易です。一番良い方法は、大きめの精度や\
様々な丸めモードで再計算してみることです。大きく異なった結果が出てきたら、
精度不足や丸めの問題や悪条件の入力、または数値計算的に不安定なアルゴリズム\
を示唆しています。

Q. コンテキストの精度は計算結果には適用されていますが入力には適用されて\
いないようです。様々に異なる精度の入力値を混ぜて計算する時に注意すべき\
ことはありますか?

A. はい。原則として入力値は正確であると見做しておりそれらの値を使った\
計算も同様です。結果だけが丸められます。入力の強みは "what you type
is what you get" (打ち込んだ値が得られる値)という点にあります。
入力が丸められないということを忘れていると結果が奇妙に見えるというのは\
弱点です。 :

.. doctest:: newcontext

   >>> getcontext().prec = 3
   >>> Decimal('3.104') + Decimal('2.104')
   Decimal('5.21')
   >>> Decimal('3.104') + Decimal('0.000') + Decimal('2.104')
   Decimal('5.20')

解決策は精度を上げるかまたは単項のプラス演算子を使って入力の丸めを強制する\
ことです。 :

.. doctest:: newcontext

   >>> getcontext().prec = 3
   >>> +Decimal('1.23456789')      # 単項のプラスで丸めを引き起こします
   Decimal('1.23')

もしくは、入力を :meth:`Context.create_decimal` を使って生成時に丸め\
てしまうこともできます。 :

   >>> Context(prec=5, rounding=ROUND_DOWN).create_decimal('1.2345678')
   Decimal('1.2345')

