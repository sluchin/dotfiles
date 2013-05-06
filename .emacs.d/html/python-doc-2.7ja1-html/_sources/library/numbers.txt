:mod:`numbers` --- 数の抽象基底クラス
================================================

.. module:: numbers
   :synopsis: 数の抽象基底クラス (Complex, Real, Integral など)

.. versionadded:: 2.6

:mod:`numbers` モジュール (:pep:`3141`) は数の抽象基底クラスの、
順により多くの演算を定義していく階層を定義します。このモジュールで
定義される型はどれもインスタンス化できません。


.. class:: Number

   数の階層の根。引数 *x* が、種類は何であれ、数であるということだけ
   チェックしたい場合、 ``isinstance(x, Number)`` が使えます。


数値塔
------

.. class:: Complex

   この型のサブクラスは複素数を表し、組み込みの :class:`complex` 型を受け付ける
   演算を含みます。それらは: :class:`complex` および :class:`bool` への変換、
   :attr:`.real`, :attr:`.imag`, ``+``, ``-``, ``*``, ``/``, :func:`abs`,
   :meth:`conjugate`, ``==``, ``!=`` です。 ``-`` と ``!=`` 以外の
   全てのものは抽象メソッドや抽象プロパティです。

   .. attribute:: real

      抽象プロパティ。この複素数の実数部分を取り出します。

   .. attribute:: imag

      抽象プロパティ。この複素数の虚数部分を取り出します。

   .. method:: conjugate()

      抽象プロパティ。複素共役を返します。たとえば、 ``(1+3j).conjugate()
      == (1-3j)`` です。

.. class:: Real

   :class:`Complex` の上に、 :class:`Real` は実数で意味を成す演算を加えます。

   簡潔に言うとそれらは: :class:`float` への変換, :func:`math.trunc`,
   :func:`round`, :func:`math.floor`, :func:`math.ceil`, :func:`divmod`, ``//``,
   ``%``, ``<``, ``<=``, ``>`` および ``>=`` です。

   Real はまた :func:`complex`, :attr:`~Complex.real`,
   :attr:`~Complex.imag` および :meth:`~Complex.conjugate`
   のデフォルトを提供します。


.. class:: Rational

   :class:`Real` をサブタイプ化し :attr:`~Rational.numerator` と
   :attr:`~Rational.denominator` のプロパティを加えたものです。
   これら分子分母は最小の値でなければなりません。この他に :func:`float`
   のデフォルトも提供します。

   .. attribute:: numerator

      抽象プロパティ。

   .. attribute:: denominator

      抽象プロパティ。


.. class:: Integral

   :class:`Rational` をサブタイプ化し :class:`int` への変換が加わります。
   :func:`float`, :attr:`~Rational.numerator` および
   :attr:`~Rational.denominator` のデフォルトと、ビット列演算: ``<<``,
   ``>>``, ``&``, ``^``, ``|``, ``~`` を提供します。


型実装者のための注意事項
---------------------------

実装する人は等しい数が等しく扱われるように同じハッシュを与えるように気を付けねばなりません。
これは二つの異なった実数の拡張があるような場合にはややこしいことになるかもしれません。
たとえば、 :class:`fractions.Fraction` は :func:`hash` を以下のように実装しています::

    def __hash__(self):
        if self.denominator == 1:
            # Get integers right.
            return hash(self.numerator)
        # Expensive check, but definitely correct.
        if self == float(self):
            return hash(float(self))
        else:
            # Use tuple's hash to avoid a high collision rate on
            # simple fractions.
            return hash((self.numerator, self.denominator))


さらに数のABCを追加する
~~~~~~~~~~~~~~~~~~~~~~~~

もちろん、他にも数に対する ABC が有り得ますし、そういったものを付け加える可能性を
閉ざしてしまうとすれば貧相な階層でしかありません。たとえば ``MyFoo`` を
:class:`Complex` と :class:`Real` の間に付け加えるには::

    class MyFoo(Complex): ...
    MyFoo.register(Real)


算術演算の実装
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

私たちは、混在型(mixed-mode)演算について作者が両方の引数の型について知っている
ような実装を呼び出すか、両方を最も近い組み込み型に変換してそこで演算するか、
どちらかを行うように算術演算を実装したいのです。 :class:`Integral` のサブタイプに
対して、このことは :meth:`__add__` と :meth:`__radd__` が次のように定義される
べきであることを意味します::

    class MyIntegral(Integral):

        def __add__(self, other):
            if isinstance(other, MyIntegral):
                return do_my_adding_stuff(self, other)
            elif isinstance(other, OtherTypeIKnowAbout):
                return do_my_other_adding_stuff(self, other)
            else:
                return NotImplemented

        def __radd__(self, other):
            if isinstance(other, MyIntegral):
                return do_my_adding_stuff(other, self)
            elif isinstance(other, OtherTypeIKnowAbout):
                return do_my_other_adding_stuff(other, self)
            elif isinstance(other, Integral):
                return int(other) + int(self)
            elif isinstance(other, Real):
                return float(other) + float(self)
            elif isinstance(other, Complex):
                return complex(other) + complex(self)
            else:
                return NotImplemented


ここには5つの異なる :class:`Complex` のサブクラス間の混在型の演算があります。
上のコードの中で ``MyIntegral`` と ``OtherTypeIKnowAbout`` に触れない部分を
"ボイラープレート" と呼ぶことにしましょう。 ``a`` を :class:`Complex` の
サブタイプである ``A`` のインスタンス (``a : A <: Complex``)、同様に
``b : B <: Complex`` として、 ``a + b`` を考えます:

    1. ``A`` が ``b`` を受け付ける :meth:`__add__` を定義している場合、
       何も問題はありません。
    2. ``A`` でボイラープレート部分に落ち込み、その結果 :meth:`__add__`
       が値を返すならば、 ``B`` に良く考えられた :meth:`__radd__` が定義
       されている可能性を見逃してしまいますので、ボイラープレートは :meth:`__add__`
       から :const:`NotImplemented` を返すのが良いでしょう。(若しくは、 ``A``
       はまったく :meth:`__add__` を実装すべきではなかったかもしれません。)
    3. そうすると、 ``B`` の :meth:`__radd__` にチャンスが巡ってきます。
       ここで ``a`` が受け付けられるならば、結果は上々です。
    4. ここでボイラープレートに落ち込むならば、もう他に試すべきメソッドは
       ありませんので、デフォルト実装の出番です。
    5. もし ``B <: A`` ならば、Python は ``A.__add__`` の前に ``B.__radd__``
       を試します。これで良い理由は、 ``A`` についての知識を持って実装しており、
       :class:`Complex` に委ねる前にこれらのインスタンスを扱えるはずだからです。

もし ``A <: Complex`` かつ ``B <: Real`` で他に共有された知識が無いならば、
適切な共通の演算は組み込みの :class:`complex` を使ったものになり、
どちらの :meth:`__radd__` ともそこに着地するでしょうから、
``a+b == b+a`` です。

ほとんどの演算はどのような型についても非常に良く似ていますので、
与えられた演算子について順結合(forward)および逆結合(reverse)のメソッドを生成
する支援関数を定義することは役に立ちます。たとえば、 :class:`fractions.Fraction`
では次のようなものを利用しています::

    def _operator_fallbacks(monomorphic_operator, fallback_operator):
        def forward(a, b):
            if isinstance(b, (int, long, Fraction)):
                return monomorphic_operator(a, b)
            elif isinstance(b, float):
                return fallback_operator(float(a), b)
            elif isinstance(b, complex):
                return fallback_operator(complex(a), b)
            else:
                return NotImplemented
        forward.__name__ = '__' + fallback_operator.__name__ + '__'
        forward.__doc__ = monomorphic_operator.__doc__

        def reverse(b, a):
            if isinstance(a, Rational):
                # Includes ints.
                return monomorphic_operator(a, b)
            elif isinstance(a, numbers.Real):
                return fallback_operator(float(a), float(b))
            elif isinstance(a, numbers.Complex):
                return fallback_operator(complex(a), complex(b))
            else:
                return NotImplemented
        reverse.__name__ = '__r' + fallback_operator.__name__ + '__'
        reverse.__doc__ = monomorphic_operator.__doc__

        return forward, reverse

    def _add(a, b):
        """a + b"""
        return Fraction(a.numerator * b.denominator +
                        b.numerator * a.denominator,
                        a.denominator * b.denominator)

    __add__, __radd__ = _operator_fallbacks(_add, operator.add)

    # ...
