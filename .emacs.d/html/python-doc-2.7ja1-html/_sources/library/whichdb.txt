
:mod:`whichdb` --- どのDBMモジュールがデータベースを作ったかを推測する
======================================================================

.. module:: whichdb
   :synopsis: どのDBM形式のモジュールが与えられたデータベースを作ったかを推測する

.. note::
   .. The :mod:`whichdb` module's only function has been put into the :mod:`dbm`
      module in Python 3.0.  The :term:`2to3` tool will automatically adapt imports
      when converting your sources to 3.0.

   :mod:`whichdb` モジュールが持っている1つの関数は、Python 3.0では :mod:`dbm`
   モジュールに移動されました。
   :term:`2to3` ツールは自動的に import を修正します。


このモジュールに含まれる唯一の関数はあることを推測します。
つまり、与えられたファイルを開くためには、利用可能なデータベースモジュール(:mod:`dbm`, :mod:`gdbm`, :mod:`dbhash`)のどれを用いるべきかということです。


.. function:: whichdb(filename)

   ファイルが読めないか存在しないために開くことが出来ない場合は ``None``
   、ファイルの形式を推測できない場合は空の文字列(``''``)、
   推測できる場合は必要なモジュール名(``'dbm'``, ``'gdbm'`` など)を含む文字列を返します。

