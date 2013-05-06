
:mod:`__builtin__` --- 組み込みオブジェクト
===========================================

.. module:: __builtin__
   :synopsis: 組み込み名前空間を提供するモジュール


.. This module provides direct access to all 'built-in' identifiers of Python; for
.. example, ``__builtin__.open`` is the full name for the built-in function
.. :func:`open`.

このモジュールは Python の全ての「組み込み」識別子に直接アクセスするためのものです。
例えば ``__builtin__.open`` は組み込み関数 :func:`open` の完全な名前です。


.. This module is not normally accessed explicitly by most applications, but can be
.. useful in modules that provide objects with the same name as a built-in value,
.. but in which the built-in of that name is also needed.  For example, in a module
.. that wants to implement an :func:`open` function that wraps the built-in
.. :func:`open`, this module can be used directly:

通常このモジュールはほとんどのアプリケーションで明示的にアクセスされることはありませんが、
組み込みの値と同じ名前のオブジェクトを提供するモジュールが同時にその名前の組み込み
オブジェクトも必要とするような場合には有用です。
たとえば、組み込みの :func:`open` をラップした :func:`open` という関数を実装したい
モジュールがあったとすると、 ``__builtin__`` モジュールは次のように直接的に使われます。


::

   import __builtin__

   def open(path):
       f = __builtin__.open(path, 'r')
       return UpperCaser(f)

   class UpperCaser:
       '''Wrapper around a file that converts output to upper-case.'''

       def __init__(self, f):
           self._f = f

       def read(self, count=-1):
           return self._f.read(count).upper()

       # ...


.. impl-detail::

   .. Most modules have the name ``__builtins__`` (note the ``'s'``) made available
   .. as part of their globals.  The value of ``__builtins__`` is normally either
   .. this module or the value of this modules's :attr:`__dict__` attribute.  Since
   .. this is an implementation detail, it may not be used by alternate
   .. implementations of Python.

   ほとんどのモジュールではグローバル変数の一部として ``__builtins__`` (``'s'`` に注意) が
   利用できるようになっています。 ``__builtins__`` の内容は通常 ``__builtin__`` モジュールそのものか、
   あるいは ``__builtin__`` モジュールの :attr:`__dict__` 属性です。
   これは実装の詳細部分なので、異なる Python の実装では ``__builtins__`` は使われていないこともあります。
