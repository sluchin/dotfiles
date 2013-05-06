:mod:`importlib` -- :func:`__import__` の便利なラッパー
===============================================================

.. module:: importlib
   :synopsis: Convenience wrappers for __import__

.. moduleauthor:: Brett Cannon <brett@python.org>
.. sectionauthor:: Brett Cannon <brett@python.org>

.. versionadded:: 2.7

このモジュールは、 Python 3.1 にある :keyword:`import` の完全な実装を提供している
同じ名前のパッケージの小さなサブセットです。
このモジュールが提供しているものは、 2.7 から 3.1 への移行をしやすくするための
ものです。


.. function:: import_module(name, package=None)

    モジュールをインポートします。 *name* 引数は、インポートするモジュールを
    指定する絶対形式もしくは相対形式の名前です。 (例: ``pkg.mod`` か ``..mod``)
    name が相対形式で与えられた場合、 *package* 引数にパッケージ名を解決する
    基準点となるパッケージを指定しなければなりません。
    (例: ``import_module('..mod', 'pkg.subpkg')`` は ``pkg.mod`` をインポートします)
    指定されたモジュールは :data:`sys.modules` に追加され、返されます。
