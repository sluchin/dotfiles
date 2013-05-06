
:mod:`fpectl` --- 浮動小数点例外の制御
======================================

.. module:: fpectl
   :platform: Unix
   :synopsis: 浮動小数点例外処理の制御。
.. moduleauthor:: Lee Busby <busby1@llnl.gov>
.. sectionauthor:: Lee Busby <busby1@llnl.gov>


.. note::

   .. The :mod:`fpectl` module is not built by default, and its usage is discouraged
      and may be dangerous except in the hands of experts.  See also the section
      :ref:`fpectl-limitations` on limitations for more details.

   :mod:`fpectl` モジュールはデフォルトではビルドされません。
   このモジュールの利用は推奨されておらず、熟練者以外がこのモジュールを使うのは危険です。
   このモジュールの制限についての詳細は、 :ref:`fpectl-limitations` 節を参照してください。

.. index:: single: IEEE-754

ほとんどのコンピュータはいわゆるIEEE-754標準に準拠した浮動小数点演算を実行します。実際のどんなコンピュータでも、浮動小数点演算が普通の浮動小数点数では表せない結果になることがあります。例えば、次を試してください。
::

   >>> import math
   >>> math.exp(1000)
   inf
   >>> math.exp(1000) / math.exp(1000)
   nan

(上の例は多くのプラットホームで動作します。DEC Alphaは例外かもしれません。)
"Inf"は"infinity(無限)"を意味するIEEE-754における特殊な非数値の値で、"nan"は"not a
number(数ではない)"を意味します。ここで留意すべき点は、その計算を行うようにPythonに求めたときに非数値の結果以外に特別なことは何も起きないというです。事実、それはIEEE-754標準に規定されたデフォルトのふるまいで、それで良ければここで読むのを止めてください。

いくつかの環境では、誤った演算がなされたところで例外を発生し、処理を止めることがより良いでしょう。 :mod:`fpectl` モジュールはそんな状況で使うためのものです。いくつかのハードウェア製造メーカーの浮動小数点ユニットを制御できるようにします。つまり、IEEE-754例外Division
by Zero、OverflowあるいはInvalid
Operationが起きたときはいつでも :const:`SIGFPE` が生成させるように、ユーザが切り替えられるようにします。あなたのpythonシステムを構成しているCコードの中へ挿入される一組のラッパーマクロと協力して、 :const:`SIGFPE` は捕捉され、Python
:exc:`FloatingPointError` 例外へ変換されます。

:mod:`fpectl` モジュールは次の関数を定義しています。また、所定の例外を発生します:


.. function:: turnon_sigfpe()

   :const:`SIGFPE` を生成するように切り替え、適切なシグナルハンドラを設定します。


.. function:: turnoff_sigfpe()

   浮動小数点例外のデフォルトの処理に再設定します。


.. exception:: FloatingPointError

   :func:`turnon_sigfpe` が実行された後に、IEEE-754例外であるDivision by Zero、OverflowまたはInvalid
   operationの一つを発生する浮動小数点演算は、次にこの標準Python例外を発生します。


.. _fpectl-example:

例
--

以下の例は :mod:`fpectl` モジュールの使用を開始する方法とモジュールのテスト演算について示しています。 ::

   >>> import fpectl
   >>> import fpetest
   >>> fpectl.turnon_sigfpe()
   >>> fpetest.test()
   overflow        PASS
   FloatingPointError: Overflow

   div by 0        PASS
   FloatingPointError: Division by zero
     [ more output from test elided ]
   >>> import math
   >>> math.exp(1000)
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
   FloatingPointError: in math_1


.. _fpectl-limitations:

制限と他に考慮すべきこと
------------------------

特定のプロセッサをIEEE-754浮動小数点エラーを捕らえるように設定することは、現在アーキテクチャごとの基準に基づきカスタムコードを必要とします。あなたの特殊なハードウェアを制御するために :mod:`fpectl` を修正することもできます。

IEEE-754例外のPython例外への変換には、ラッパーマクロ ``PyFPE_START_PROTECT`` と ``PyFPE_END_PROTECT`` があなたのコードに適切な方法で挿入されていることが必要です。Python自身は :mod:`fpectl` モジュールをサポートするために修正されていますが、数値解析にとって興味ある多くの他のコードはそうではありません。

:mod:`fpectl` モジュールはスレッドセーフではありません。


.. seealso::

   このモジュールがどのように動作するのかについてより学習するときに、ソースディストリビューションの中のいくつかのファイルは興味を引くものでしょう。インクルードファイル :file:`Include/pyfpe.h` では、このモジュールの実装について同じ長さで議論されています。 :file:`Modules/fpetestmodule.c` には、いくつかの使い方の例があります。多くの追加の例が :file:`Objects/floatobject.c` にあります。

