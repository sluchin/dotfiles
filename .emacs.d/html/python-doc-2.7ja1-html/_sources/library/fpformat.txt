
:mod:`fpformat` --- 浮動小数点数の変換
======================================

.. module:: fpformat
   :synopsis: 浮動小数点をフォーマットする汎用関数。
   :deprecated:

.. deprecated:: 2.6
    :mod:`fpformat` は Python 3.0 で削除されました。

.. sectionauthor:: Moshe Zadka <moshez@zadka.site.co.il>


:mod:`fpformat` モジュールは浮動小数点数の表示を 100% 純粋に Python
だけで行うための関数を定義しています。

.. note::

   このモジュールは必要ありません: このモジュールのすべてのことは、
   :ref:`string-formatting` 節で説明されている
   ``%`` を使った文字列の補間演算により実現可能です。

:mod:`fpformat` モジュールは次にあげる関数と例外を定義しています。


.. function:: fix(x, digs)

   *x* を ``[-]ddd.ddd`` の形にフォーマットします。
   小数点の後ろに *digs* 桁と、小数点の前に少なくとも1桁です。
   ``digs <= 0`` の場合、小数点以下は切り捨てられます。

   *x* は数字か数字を表した文字列です。
   *digs* は整数です。

   返り値は文字列です。


.. function:: sci(x, digs)

   *x* を ``[-]d.dddE[+-]ddd`` の形にフォーマットします。
   小数点の後ろに *digs* 桁と、小数点の前に1桁だけです。
   ``digs <= 0`` の場合、1桁だけ残され、小数点以下は切り捨てられます。

   *x* は実数か実数を表した文字列です。
   *digs* は整数です。

   返り値は文字列です。


.. exception:: NotANumber

   :func:`fix` や :func:`sci` にパラメータとして渡された文字列
   *x* が数字として認識できなかった場合、例外が発生します。
   標準の例外が文字列の頃から、この例外は :exc:`ValueError` のサブクラスです。
   例外値は、例外を発生させた不適切にフォーマットされた文字列です。

例::

   >>> import fpformat
   >>> fpformat.fix(1.23, 1)
   '1.2'

