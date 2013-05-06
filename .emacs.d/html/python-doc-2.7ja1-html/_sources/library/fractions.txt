
:mod:`fractions` --- 有理数
=====================================

.. module:: fractions
   :synopsis: 有理数
.. moduleauthor:: Jeffrey Yasskin <jyasskin at gmail.com>
.. sectionauthor:: Jeffrey Yasskin <jyasskin at gmail.com>
.. versionadded:: 2.6


:mod:`fractions` モジュールは有理数計算のサポートを提供します。


Fraction インスタンスは一対の整数、他の有理数または文字列から組み立てられます。

.. class:: Fraction(numerator=0, denominator=1)
           Fraction(other_fraction)
           Fraction(float)
           Fraction(decimal)
           Fraction(string)

   最初のバージョンは *numerator* と *denominator* が :class:`numbers.Rational`
   のインスタンスであることを要求し、 ``numerator/denominator`` の値を持つ\
   新しい :class:`Fraction` インスタンスを返します。
   *denominator* が :const:`0` ならば、 :exc:`ZeroDivisionError`
   を送出します。
   二番目のバージョンは *other_fraction* が :class:`numbers.Rational`
   のインスタンスであることを要求し、同じ値を持つ新しい :class:`Fraction`
   インスタンスを返します。
   その次の二つのバージョンは、 :class:`float` と :class:`decimal.Decimal`
   インスタンスを受け付け、それとちょうど同じ値を持つ :class:`Fraction`
   インスタンスを返します。なお、二進浮動小数点数にお決まりの問題
   (:ref:`tut-fp-issues` 参照) のため、 ``Fraction(1.1)`` の引数は
   11/10 と正確に等しいとは言えないので、 ``Fraction(1.1)`` は予期した通りの
   ``Fraction(11, 10)`` を返 *しません* 。最後のバージョンは、
   文字列またはユニコードのインスタンスを渡されると思っています。
   一つめの形式の通常の形式は::

      [sign] numerator ['/' denominator]

   で、ここにオプションの ``sign`` は '+' か '-' のどちらかであり、\
   ``numerator`` および ``denominator`` (もしあるならば) は十進数の\
   数字の並びです。
   さらに、 :class:`float` コンストラクタで受け付けられる、有限の値を表す
   文字列は、必ず :class:`Fraction` コンストラクタでも受け付けられます。
   どちらの形式でも入力される文字列は前後に空白があって構いません。
   例を見ましょう::

      >>> from fractions import Fraction
      >>> Fraction(16, -10)
      Fraction(-8, 5)
      >>> Fraction(123)
      Fraction(123, 1)
      >>> Fraction()
      Fraction(0, 1)
      >>> Fraction('3/7')
      Fraction(3, 7)
      [40794 refs]
      >>> Fraction(' -3/7 ')
      Fraction(-3, 7)
      >>> Fraction('1.414213 \t\n')
      Fraction(1414213, 1000000)
      >>> Fraction('-.125')
      Fraction(-1, 8)
      >>> Fraction('7e-6')
      Fraction(7, 1000000)
      >>> Fraction(2.25)
      Fraction(9, 4)
      >>> Fraction(1.1)
      Fraction(2476979795053773, 2251799813685248)
      >>> from decimal import Decimal
      >>> Fraction(Decimal('1.1'))
      Fraction(11, 10)


   :class:`Fraction` クラスは抽象基底クラス :class:`numbers.Rational`
   を継承し、その全てのメソッドと演算を実装します。 :class:`Fraction`
   インスタンスはハッシュ可能で、したがって不変(immutable)であるものとして\
   扱います。加えて、 :class:`Fraction` には以下のメソッドがあります:

   .. versionchanged:: 2.7
      :class:`Fraction` コンストラクタは、今では :class:`float` や
      :class:`decimal.Decimal` インスタンスを受けつけます。


   .. method:: from_float(flt)

      このクラスメソッドは :class:`float` である *flt* の正確な値を表す
      :class:`Fraction` を構築します。
      気を付けてください ``Fraction.from_float(0.3)`` と ``Fraction(3, 10)``
      の値は同じではありません。

      .. note:: From Python 2.7 以降では、 :class:`float` から直接 
         :class:`Fraction` インスタンスを構成することも出来ます。


   .. method:: from_decimal(dec)

      このクラスメソッドは :class:`decimal.Decimal` である *dec* の正確な値を表す
      :class:`Fraction` を構築します。

      .. note:: From Python 2.7 以降では、 :class:`decimal.Decimal` から直接 
         :class:`Fraction` インスタンスを構成することも出来ます。


   .. method:: limit_denominator(max_denominator=1000000)

      高々 max_denominator を分母に持つ ``self`` に最も近い :class:`Fraction`
      を見付けて返します。
      このメソッドは与えられた浮動小数点数の有理数近似を見つけるのに役立ちます:

         >>> from fractions import Fraction
         >>> Fraction('3.1415926535897932').limit_denominator(1000)
         Fraction(355, 113)

      あるいは float で表された有理数を元に戻すのにも使えます:

         >>> from math import pi, cos
         >>> Fraction(cos(pi/3))
         Fraction(4503599627370497, 9007199254740992)
         >>> Fraction(cos(pi/3)).limit_denominator()
         Fraction(1, 2)
         >>> Fraction(1.1).limit_denominator()
         Fraction(11, 10)


.. function:: gcd(a, b)

   整数 *a* と *b* の最大公約数を返します。 *a* も *b* もゼロでないとすると、
   ``gcd(a, b)`` の絶対値は *a* と *b* の両方を割り切る最も大きな整数です。
   ``gcd(a, b)`` は *b* がゼロでなければ *b* と同じ符号になります。
   そうでなければ *a* の符号を取ります。
   ``gcd(0, 0)`` は `0` を返します。


.. seealso::

   :mod:`numbers` モジュール
      数値の塔を作り上げる抽象基底クラス。
