
組み込み定数
============

.. A small number of constants live in the built-in namespace.  They are:

組み込み空間には少数の定数があります。以下にそれらの定数を示します。


.. data:: False

   .. The false value of the :class:`bool` type.

   :class:`bool` 型における、偽を表す値です。

   .. versionadded:: 2.3


.. data:: True

   .. The true value of the :class:`bool` type.

   :class:`bool` 型における、真を表す値です。

   .. versionadded:: 2.3


.. data:: None

   .. The sole value of :attr:`types.NoneType`.  ``None`` is frequently used to
   .. represent the absence of a value, as when default arguments are not passed to a
   .. function.

   :attr:`types.NoneType` の唯一の値です。 ``None`` は、例えば関数に
   デフォルト値が渡されないときのように、値がないことを表すために
   しばしば用いられます。


   .. .. versionchanged:: 2.4
   ..    Assignments to ``None`` are illegal and raise a :exc:`SyntaxError`.

   .. versionchanged:: 2.4
      ``None`` に対する代入は不正であり、 :exc:`SyntaxError` を送出します。


.. data:: NotImplemented

   .. Special value which can be returned by the "rich comparison" special methods
   .. (:meth:`__eq__`, :meth:`__lt__`, and friends), to indicate that the comparison
   .. is not implemented with respect to the other type.

   "拡張比較 (rich comparison)" を行う特殊メソッド (:meth:`__eq__`,
   :meth:`__lt__`, およびその仲間) によって返される特別な値で、
   もう片方の型に対しては比較が実装されていないことを示します。


.. data:: Ellipsis

   .. Special value used in conjunction with extended slicing syntax.

   拡張スライス文と同時に用いられる特殊な値です。


.. data:: __debug__

   .. This constant is true if Python was not started with an :option:`-O` option.
   .. It cannot be reassigned.  See also the :keyword:`assert` statement.

   この定数は Python が :option:`-O` オプションを有効にして開始されていないときに真となります。
   :keyword:`assert` 文も参照して下さい。


.. note::

   :data:`None` と :data:`__debug__` という名前は再代入できないので
   (これらに対する代入は、たとえ属性名としてであっても :exc:`SyntaxError` が送出されます)、
   これらは「真の」定数であると考えることができます。

   .. versionchanged:: 2.7
      属性名としての ``__debug__`` への代入が禁止されました。

.. Constants added by the :mod:`site` module

:mod:`site` モジュールで追加される定数
-----------------------------------------

.. The :mod:`site` module (which is imported automatically during startup, except
.. if the :option:`-S` command-line option is given) adds several constants to the
.. built-in namespace.  They are useful for the interactive interpreter shell and
.. should not be used in programs.

:mod:`site` モジュール (コマンドラインオプションとして :option:`-S` が
指定されない限り、開始時に自動的にインポートされます) はいくつかの定数
を組み込みの名前空間に追加します。それらは対話的インタープリタシェルに
とって有用であり、プログラムから使うべきではありません。


.. data:: quit([code=None])
          exit([code=None])

   .. Objects that when printed, print a message like "Use quit() or Ctrl-D
   .. (i.e. EOF) to exit", and when called, raise :exc:`SystemExit` with the
   .. specified exit code.

   表示されたときに "Use quit() or Ctrl-D (i.e. EOF) to exit" のような
   メッセージを出力し、呼び出されたときには指定された終了コードを伴って
   :exc:`SystemExit` を送出するオブジェクトです。


.. data:: copyright
          license
          credits

   .. Objects that when printed, print a message like "Type license() to see the
   .. full license text", and when called, display the corresponding text in a
   .. pager-like fashion (one screen at a time).

   表示されたときに "Type license() to see the full license text" のような
   メッセージを出力し、呼び出されたときにはそれぞれのテキストをページャのような
   形式で (1画面分づつ) 表示するオブジェクトです。
