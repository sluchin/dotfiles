
:mod:`repr` --- もう一つの :func:`repr` の実装
==============================================

.. module:: repr
   :synopsis: 大きさに制限のある別のrepr()の実装。
.. sectionauthor:: Fred L. Drake, Jr. <fdrake@acm.org>

.. note::
   :mod:`repr` モジュールは Python 3.0 では :mod:`reprlib` にリネームされました。
   :term:`2to3` ツールはソースコード内の import 文を自動で 3.0 に適応させます。

:mod:`repr` モジュールは結果の文字列の大きさを制限したオブジェクト表現を作り出すための方法を提供します。これはPythonデバッガで使われていますが、他の状況でも同じように役に立つかもしれません。

.. seealso::

   最新バージョンの `repr module Python ソースコード
   <http://svn.python.org/view/python/branches/release27-maint/Lib/repr.py?view=markup>`_

このモジュールはクラスとインスタンス、それに関数を提供します:


.. class:: Repr()

   組み込みクラス :func:`repr` によく似た関数を実装するために役に立つ書式化サービスを提供します。過度に長い表現を作り出さないように、異なるオブジェクト型に対する大きさの制限が追加されます。


.. data:: aRepr

   これは下で説明される :func:`.repr` 関数を提供するために使われる :class:`Repr`
   のインスタンスです。このオブジェクトの属性を変更すると、 :func:`.repr` と
   Python デバッガが使うサイズ制限に影響します。


.. function:: repr(obj)

   これは ``aRepr`` の :meth:`~Repr.repr` メソッドです。同じ名前の組み込み
   関数が返す文字列と似ていますが、最大サイズに制限のある文字列を返します。


.. _repr-objects:

Reprオブジェクト
----------------

:class:`Repr` インスタンスは様々なオブジェクト型の表現にサイズ制限を与えるために使えるいくつかのメンバーと、特定のオブジェクト型を書式化するメソッドを提供します。


.. attribute:: Repr.maxlevel

   再帰的な表現を作る場合の深さ制限。デフォルトは ``6`` です。


.. attribute:: Repr.maxdict
               Repr.maxlist
               Repr.maxtuple
               Repr.maxset
               Repr.maxfrozenset
               Repr.maxdeque
               Repr.maxarray

   指定されたオブジェクト型に対するエントリ表現の数についての制限。
   :attr:`maxdict` に対するデフォルトは ``4`` で、 :attr:`maxarray` は ``5`` 、その他に対しては ``6`` です。

   .. versionadded:: 2.4
      :attr:`maxset`, :attr:`maxfrozenset`, :attr:`set`.


.. attribute:: Repr.maxlong

   長整数の表現のおける文字数の最大値。中央の数字が抜け落ちます。デフォルトは ``40`` です。


.. attribute:: Repr.maxstring

   文字列の表現における文字数の制限。文字列の"通常の"表現は文字の材料だということに注意してください:
   表現にエスケープシーケンスが必要とされる場合は、表現が短縮されたときにこれらはマングルされます。デフォルトは ``30`` です。


.. attribute:: Repr.maxother

   この制限は :class:`Repr` オブジェクトに利用できる特定の書式化メソッドがないオブジェクト型のサイズをコントロールするために使われます。 :attr:`maxstring` と同じようなやり方で適用されます。デフォルトは ``20`` です。


.. method:: Repr.repr(obj)

   インスタンスが強制する書式化を使う組み込み :func:`repr` と等価なもの。


.. method:: Repr.repr1(obj, level)

   :meth:`.repr` が使う再帰的な実装。これはどの書式化メソッドを呼び出すかを決定するために *obj* の型を使い、それを *obj* と *level* に渡します。再帰呼び出しにおいて *level* の値に対して
   ``level - 1`` を与える再帰的な書式化を実行するために、型に固有のメソッドは :meth:`repr1` を呼び出します。


.. method:: Repr.repr_TYPE(obj, level)
   :noindex:

   型名に基づく名前をもつメソッドとして、特定の型に対する書式化メソッドは実装されます。
   メソッド名では、 **TYPE** は ``string.join(string.split(type(obj).__name__, '_'))``
   に置き換えられます。
   これらのメソッドへのディスパッチは :meth:`repr1` によって処理されます。
   再帰的に値の書式を整える必要がある型固有のメソッドは、 ``self.repr1(subobj, level - 1)``
   を呼び出します。


.. _subclassing-reprs:

Reprオブジェクトをサブクラス化する
----------------------------------

更なる組み込みオブジェクト型へのサポートを追加するためや、すでにサポートされている型の扱いを変更するために、
:meth:`Repr.repr1` による動的なディスパッチを使って :class:`Repr` をサブクラス化することができます。
この例はファイルオブジェクトのための特別なサポートを追加する方法を示しています::

   import repr as reprlib
   import sys

   class MyRepr(reprlib.Repr):
       def repr_file(self, obj, level):
           if obj.name in ['<stdin>', '<stdout>', '<stderr>']:
               return obj.name
           else:
               return repr(obj)

   aRepr = MyRepr()
   print aRepr.repr(sys.stdin)          # prints '<stdin>'

