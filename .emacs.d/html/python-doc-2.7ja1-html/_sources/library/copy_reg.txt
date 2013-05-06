
:mod:`copy_reg` --- :mod:`pickle` サポート関数を登録する
========================================================

.. module:: copy_reg
   :synopsis: pickleサポート関数を登録する。

.. note::
   .. The :mod:`copy_reg` module has been renamed to :mod:`copyreg` in Python 3.0.
      The :term:`2to3` tool will automatically adapt imports when converting your
      sources to 3.0.

   :mod:`copy_reg` モジュールはPython 3.0で :mod:`copyreg` に変更されました。
   :term:`2to3` ツールが自動的にソースコードの import を変換します。

.. index::
   module: pickle
   module: cPickle
   module: copy

:mod:`copy_reg` モジュールは :mod:`pickle` と :mod:`cPickle` モジュールに対するサポートを提供します。その上、 :mod:`copy` モジュールは将来これをつかう可能性が高いです。クラスでないオブジェクトコンストラクタについての設定情報を提供します。このようなコンストラクタはファクトリ関数か、またはクラスインスタンスでしょう。


.. function:: constructor(object)

   *object* を有効なコンストラクタであると宣言します。 *object* が呼び出し可能でなければ(そして、それゆえコンストラクタとして有効でないならば)、 :exc:`TypeError` を発生します。


.. function:: pickle(type, function[, constructor])

   *function* が型 *type* のオブジェクトに対する"リダクション"関数として使うことを宣言します。 *type* は"標準的な"クラスオブジェクトであってはいけません。(標準的なクラスは異なった扱われ方をします。詳細は、 :mod:`pickle` モジュールのドキュメンテーションを参照してください。)
   *function* は文字列または二ないし三つの要素を含むタプルです。

   オプションの *constructor* パラメータが与えられた場合は、pickle化時に *function* が返した引数のタプルとともによびだされたときにオブジェクトを再構築するために使われ得る呼び出し可能オブジェクトです。 *object* がクラスであるか、または *constructor* が呼び出し可能でない場合に、 :exc:`TypeError` を発生します。

   *function* と *constructor* の求められるインターフェイスについての詳細は、 :mod:`pickle` モジュールを参照してください。

