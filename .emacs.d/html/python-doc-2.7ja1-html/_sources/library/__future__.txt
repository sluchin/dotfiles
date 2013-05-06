:mod:`__future__` --- future 文の定義
=================================================

.. module:: __future__
   :synopsis: future 文の定義


.. :mod:`__future__` is a real module, and serves three purposes:

:mod:`__future__` は実際にモジュールであり、次の3つの役割があります。


.. * To avoid confusing existing tools that analyze import statements and expect to
..   find the modules they're importing.

.. * To ensure that :ref:`future statements <future>` run under releases prior to
..   2.1 at least yield runtime exceptions (the import of :mod:`__future__` will
..   fail, because there was no module of that name prior to 2.1).

.. * To document when incompatible changes were introduced, and when they will be
..   --- or were --- made mandatory.  This is a form of executable documentation, and
..   can be inspected programmatically via importing :mod:`__future__` and examining
..   its contents.


* import 文を解析する既存のツールを混乱させることを避け、
  インポートしようとしているモジュールを見つけられるようにするため。

* 2.1 以前のリリースで :ref:`future 文 <future>` が実行された場合に、最低でもランタイム例外を投げるようにするため。
  (:mod:`__future__` のインポートは失敗します。なぜなら、 2.1 以前にはそういう名前のモジュールはなかったからです。)

* 互換性のない変化がいつ言語に導入され、いつ言語の一部になる --- あるい
  は、なった --- のかを文書化するため。これは実行できる形式で書かれたド
  キュメントなので、 :mod:`__future__` をインポートしてその中身を調べる
  ことでプログラムから調査することができます。


.. Each statement in :file:`__future__.py` is of the form:

:file:`__future__.py` のそれぞれの文は次のような形式をしています::


   FeatureName = _Feature(OptionalRelease, MandatoryRelease,
                          CompilerFlag)


.. where, normally, *OptionalRelease* is less than *MandatoryRelease*, and both are
.. 5-tuples of the same form as ``sys.version_info``:

ここで、普通は、 *OptionalRelease* は *MandatoryRelease* より小さく、2つとも ``sys.version_info``
と同じフォーマットの5つのタプルからなります。 ::


   (PY_MAJOR_VERSION, # the 2 in 2.1.0a3; an int
    PY_MINOR_VERSION, # the 1; an int
    PY_MICRO_VERSION, # the 0; an int
    PY_RELEASE_LEVEL, # "alpha", "beta", "candidate" or "final"; string
    PY_RELEASE_SERIAL # the 3; an int
   )


.. *OptionalRelease* records the first release in which the feature was accepted.

*OptionalRelease* はその機能が導入された最初のリリースを記録します。


.. In the case of a *MandatoryRelease* that has not yet occurred,
.. *MandatoryRelease* predicts the release in which the feature will become part of
.. the language.

まだ時期が来ていない *MandatoryRelease* の場合、 *MandatoryRelease* はその機能が言語の一部となるリリースを記します。


.. Else *MandatoryRelease* records when the feature became part of the language; in
.. releases at or after that, modules no longer need a future statement to use the
.. feature in question, but may continue to use such imports.

その他の場合、 *MandatoryRelease* はその機能がいつ言語の一部になったのかを記録します。
そのリリースから、あるいはそれ以降のリリースでは、この機能を使う際に
future 文は必要ではありませんが、 future 文を使い続けても構いません。


.. *MandatoryRelease* may also be ``None``, meaning that a planned feature got
.. dropped.

*MandatoryRelease* は ``None`` になるかもしれません。つまり、予定された機能が破棄されたということです。


.. Instances of class :class:`_Feature` have two corresponding methods,
.. :meth:`getOptionalRelease` and :meth:`getMandatoryRelease`.

:class:`_Feature` クラスのインスタンスには対応する2つのメソッド、 :meth:`getOptionalRelease` と
:meth:`getMandatoryRelease` があります。


.. *CompilerFlag* is the (bitfield) flag that should be passed in the fourth
.. argument to the built-in function :func:`compile` to enable the feature in
.. dynamically compiled code.  This flag is stored in the :attr:`compiler_flag`
.. attribute on :class:`_Feature` instances.

*CompilerFlag* は、動的にコンパイルされるコードでその機能を有効にするために、組み込み関数 :func:`compile`
の第4引数に渡す(ビットフィールド)フラグです。このフラグは :class:`_Feature` インスタンスの
:attr:`compilier_flag` 属性に保存されています。


.. No feature description will ever be deleted from :mod:`__future__`. Since its
.. introduction in Python 2.1 the following features have found their way into the
.. language using this mechanism:

機能の記述が :mod:`__future__` から削除されたことはまだありません。
Python 2.1 で future 文が導入されて以来、この仕組みを使って以下の機能が
言語に導入されてきました。


+------------------+-------------+--------------+---------------------------------------------+
| feature          | optional in | mandatory in | effect                                      |
+==================+=============+==============+=============================================+
| nested_scopes    | 2.1.0b1     | 2.2          | :pep:`227`:                                 |
|                  |             |              | *Statically Nested Scopes*                  |
+------------------+-------------+--------------+---------------------------------------------+
| generators       | 2.2.0a1     | 2.3          | :pep:`255`:                                 |
|                  |             |              | *Simple Generators*                         |
+------------------+-------------+--------------+---------------------------------------------+
| division         | 2.2.0a2     | 3.0          | :pep:`238`:                                 |
|                  |             |              | *Changing the Division Operator*            |
+------------------+-------------+--------------+---------------------------------------------+
| absolute_import  | 2.5.0a1     | 2.7          | :pep:`328`:                                 |
|                  |             |              | *Imports: Multi-Line and Absolute/Relative* |
+------------------+-------------+--------------+---------------------------------------------+
| with_statement   | 2.5.0a1     | 2.6          | :pep:`343`:                                 |
|                  |             |              | *The "with" Statement*                      |
+------------------+-------------+--------------+---------------------------------------------+
| print_function   | 2.6.0a2     | 3.0          | :pep:`3105`:                                |
|                  |             |              | *Make print a function*                     |
+------------------+-------------+--------------+---------------------------------------------+
| unicode_literals | 2.6.0a2     | 3.0          | :pep:`3112`:                                |
|                  |             |              | *Bytes literals in Python 3000*             |
+------------------+-------------+--------------+---------------------------------------------+

.. seealso::

   :ref:`future`

      .. How the compiler treats future imports.

      コンパイラがどのように future インポートを扱うか
