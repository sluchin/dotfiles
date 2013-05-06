
:mod:`bisect` --- 配列二分法アルゴリズム
========================================

.. module:: bisect
   :synopsis: バイナリサーチ用の配列二分法アルゴリズム。
.. sectionauthor:: Fred L. Drake, Jr. <fdrake@acm.org>
.. sectionauthor:: Raymond Hettinger <python at rcn.com>
.. example based on the PyModules FAQ entry by Aaron Watters
.. <arw@pythonpros.com>.

このモジュールは、挿入の度にリストをソートすることなく、リストをソートされた順序に保つことをサポートします。
大量の比較操作を伴うような、アイテムがたくさんあるリストでは、より一般的なアプローチに比べて、パフォーマンスが向上します。
動作に基本的な二分法アルゴリズムを使っているので、 :mod:`bisect` と呼ばれています。
ソースコードはこのアルゴリズムの実例として一番役に立つかもしれません (境界条件はすでに正しいです!)。

.. versionadded:: 2.1

.. seealso::

   Latest version of the `bisect module Python source code
   <http://svn.python.org/view/python/branches/release27-maint/Lib/bisect.py?view=markup>`_

次の関数が用意されています。


.. function:: bisect_left(a, x, lo=0, hi=len(a))

   ソートされた順序を保ったまま *x* を *a* に挿入できる点を探し当てます。
   リストの中から検索する部分集合を指定するには、パラメーターの *lo* と *hi* を使います。デフォルトでは、リスト全体が使われます。 *x*
   がすでに *a* に含まれている場合、挿入点は既存のどのエントリーよりも前(左)になります。戻り値は、 ``list.insert()``
   の第一引数として使うのに適しています。 *a* はすでにソートされているものとします。

   返された挿入点 *i* は、配列 *a* を二つに分け、
   ``all(val < x for val in a[lo:i])`` が左側に、
   ``all(val >= x for val in a[i:hi])`` が右側になるようにします。

.. function:: bisect_right(a, x, lo=0, hi=len(a))
              bisect(a, x, lo=0, hi=len(a))

   :func:`bisect_left` と似ていますが、 *a* に含まれる *x*
   のうち、どのエントリーよりも後ろ(右)にくるような挿入点を返します。

.. function:: insort_left(a, x, lo=0, hi=len(a))

   *x* を *a* にソートされた順序で挿入します。これは、
   ``a.insert(bisect.bisect_left(a, x, lo, hi), x)`` と等価です。 *a*
   はすでにソートされているものとします。なお、O(log n) の探索は
   遅い O(n) の挿入の段階に支配されます。

.. function:: insort_right(a, x, lo=0, hi=len(a))
              insort(a, x, lo=0, hi=len(a))

   :func:`insort_left` と似ていますが、 *a* に含まれる *x* のうち、
   どのエントリーよりも後ろに *x* を挿入します。

.. seealso::

   bisect を利用して、直接の探索ができ、キー関数をサポートする、
   完全な機能を持つコレクションクラスを組み立てる `SortedCollection recipe
   <http://code.activestate.com/recipes/577197-sortedcollection/>`_\ 。
   キーは、探索中に不必要な呼び出しをさせないために、予め計算しておきます。


ソート済みリストの探索
----------------------

上記の :func:`bisect` 関数群は挿入点を探索するのには便利ですが、普通の
探索タスクに使うのはトリッキーだったり不器用だったりします。以下の 5 関数は、
これらをどのように標準の探索やソート済みリストに変換するかを説明します::

    def index(a, x):
        'Locate the leftmost value exactly equal to x'
        i = bisect_left(a, x)
        if i != len(a) and a[i] == x:
            return i
        raise ValueError

    def find_lt(a, x):
        'Find rightmost value less than x'
        i = bisect_left(a, x)
        if i:
            return a[i-1]
        raise ValueError

    def find_le(a, x):
        'Find rightmost value less than or equal to x'
        i = bisect_right(a, x)
        if i:
            return a[i-1]
        raise ValueError

    def find_gt(a, x):
        'Find leftmost value greater than x'
        i = bisect_right(a, x)
        if i != len(a):
            return a[i]
        raise ValueError

    def find_ge(a, x):
        'Find leftmost item greater than or equal to x'
        i = bisect_left(a, x)
        if i != len(a):
            return a[i]
        raise ValueError


その他の使用例
--------------

.. _bisect-example:

:func:`bisect` 関数は数値テーブルの探索に役に立ちます。この例では、 :func:`bisect`
を使って、(たとえば)順序のついた数値の区切り点の集合に基づいて、試験の成績の
等級を表す文字を調べます。区切り点は 90 以上は 'A'、 80 から 89 は
'B'、などです。

   >>> def grade(score, breakpoints=[60, 70, 80, 90], grades='FDCBA'):
   ...     i = bisect(breakpoints, score)
   ...     return grades[i]
   ...
   >>> [grade(score) for score in [33, 99, 77, 70, 89, 90, 100]]
   ['F', 'A', 'C', 'C', 'B', 'A', 'A']

:func:`sorted` 関数と違い、 :func:`bisect` 関数に *key* や *reversed* 引数を
用意するのは、設計が非効率になるので、非合理的です。 (連続する bisect 関数の
呼び出しは前回の key 参照の結果を "記憶" しません)

代わりに、事前に計算しておいたキーのリストから検索して、レコードのインデックスを
見つけます。

::

    >>> data = [('red', 5), ('blue', 1), ('yellow', 8), ('black', 0)]
    >>> data.sort(key=lambda r: r[1])
    >>> keys = [r[1] for r in data]         # precomputed list of keys
    >>> data[bisect_left(keys, 0)]
    ('black', 0)
    >>> data[bisect_left(keys, 1)]
    ('blue', 1)
    >>> data[bisect_left(keys, 5)]
    ('red', 5)
    >>> data[bisect_left(keys, 8)]
    ('yellow', 8)
