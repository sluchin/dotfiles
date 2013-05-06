:mod:`future_builtins` --- Python 3 のビルトイン
================================================

.. module:: future_builtins
.. sectionauthor:: Georg Brandl
.. versionadded:: 2.6

.. This module provides functions that exist in 2.x, but have different behavior in
   Python 3, so they cannot be put into the 2.x builtin namespace.

このモジュールは、Python 2.x に存在するけれども、
Python 3では異なった動作をするために、Python 2.xのビルトイン名前空間に追加できない
関数を提供します。

.. Instead, if you want to write code compatible with Python 3 builtins, import
   them from this module, like this::

代わりに、Python 3のビルトイン関数と互換性のあるコードを書きたい場合は、
次のように、このモジュールからその関数をimportしてください。 ::

   from future_builtins import map, filter

   ... code using Python 3-style map and filter ...

.. The :term:`2to3` tool that ports Python 2 code to Python 3 will recognize
   this usage and leave the new builtins alone.

Python 2のコードをPython 3用に変換する :term:`2to3` ツールは、
この利用方法を検出し、新しいビルトイン関数をそのまま利用します。

.. note::

   .. The Python 3 :func:`print` function is already in the builtins, but cannot be
      accessed from Python 2 code unless you use the appropriate future statement::

   Python 3の :func:`print` 関数は、Python 2でもビルトイン関数です。
   しかし、future文で指定しない限り、利用することができません。

      from __future__ import print_function


.. Available builtins are:

利用できるビルトイン関数は、以下の通りです。

.. function:: ascii(object)

   .. Returns the same as :func:`repr`.  In Python 3, :func:`repr` will return
      printable Unicode characters unescaped, while :func:`ascii` will always
      backslash-escape them.  Using :func:`future_builtins.ascii` instead of
      :func:`repr` in 2.6 code makes it clear that you need a pure ASCII return
      value.

   :func:`repr` と同じ値を返します。
   Python 3 では、 :func:`repr` は表示可能なUnicode文字をエスケープせずに返し、
   :func:`ascii` はその文字列をバックスラッシュでエスケープします。
   :func:`future_builtins.ascii` を :func:`repr` の代わりに利用することで、
   ASCII文字列を必要としていることを明示できます。

.. function:: filter(function, iterable)

   .. Works like :func:`itertools.ifilter`.

   :func:`itertools.ifilter` と同じように動作します。

.. function:: hex(object)

   .. Works like the builtin :func:`hex`, but instead of :meth:`__hex__` it will
      use the :meth:`__index__` method on its argument to get an integer that is
      then converted to hexadecimal.

   ビルトイン :func:`hex` と同じように動作しますが、 :meth:`__hex__` の代わりに、
   :meth:`__index__` メソッドを利用して整数を取得し、それを16進数文字列に変換します。

.. function:: map(function, iterable, ...)

   .. Works like :func:`itertools.imap`.

   :func:`itertools.imap` と同じように動作します。

.. function:: oct(object)

   .. Works like the builtin :func:`oct`, but instead of :meth:`__oct__` it will
      use the :meth:`__index__` method on its argument to get an integer that is
      then converted to hexadecimal.

   ビルトイン :func:`oct` と同じように動作しますが、 :meth:`__oct__` の代わりに、
   :meth:`__index__` メソッドを利用して整数を取得し、それを16進数文字列に変換します。

.. function:: zip(*iterables)

   .. Works like :func:`itertools.izip`.

   :func:`itertools.izip` と同じように動作します。
