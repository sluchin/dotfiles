:mod:`collections` --- 高性能なコンテナ・データ型
=================================================

.. module:: collections
   :synopsis: High-performance container datatypes
.. moduleauthor:: Raymond Hettinger <python@rcn.com>
.. sectionauthor:: Raymond Hettinger <python@rcn.com>

.. versionadded:: 2.4

.. testsetup:: *

   from collections import *
   import itertools
   __name__ = '<doctest>'

このモジュールは、汎用の Python 組み込みコンテナ :class:`dict`, :class:`list`,
:class:`set`, および :class:`tuple` に代わる、
特殊なコンテナデータ型を実装しています。

=====================   ================================================================== ===========================
:func:`namedtuple`      名前付きフィールドを持つタプルのサブクラスを作成するファクトリ関数 .. versionadded:: 2.6      
:class:`deque`          両端における append や pop を高速に行えるリスト風のコンテナ        .. versionadded:: 2.4      
:class:`Counter`        ハッシュ可能なオブジェクトを数え上げる辞書のサブクラス             .. versionadded:: 2.7      
:class:`OrderedDict`    項目が追加された順序を記憶する辞書のサブクラス                     .. versionadded:: 2.7      
:class:`defaultdict`    ファクトリ関数を呼び出して存在しない値を供給する辞書のサブクラス   .. versionadded:: 2.5      
=====================   ================================================================== ===========================

コンテナ型の作成に加えて、 collections モジュールは幾つかの ABC (abstract base
classes = 抽象基底クラス) を提供しています。
ABC はクラスが特定のインタフェース持っているかどうか、たとえばハッシュ可能で
あるかやマッピングであるかを判定するのに利用します。

.. seealso::

   Latest version of the `collections module Python source code
   <http://svn.python.org/view/python/branches/release27-maint/Lib/collections.py?view=markup>`_

:class:`Counter` オブジェクト
-----------------------------

便利で迅速な検数をサポートするカウンタツールが提供されています。
例えば::

    >>> # リストに現れる単語の検数
    >>> cnt = Counter()
    >>> for word in ['red', 'blue', 'red', 'green', 'blue', 'blue']:
    ...     cnt[word] += 1
    >>> cnt
    Counter({'blue': 3, 'red': 2, 'green': 1})

    >>> # Hamlet で最も多い 10 単語の発見
    >>> import re
    >>> words = re.findall('\w+', open('hamlet.txt').read().lower())
    >>> Counter(words).most_common(10)
    [('the', 1143), ('and', 966), ('to', 762), ('of', 669), ('i', 631),
     ('you', 554),  ('a', 546), ('my', 514), ('hamlet', 471), ('in', 451)]

.. class:: Counter([iterable-or-mapping])

   :class:`Counter` はハッシュ可能なオブジェクトをカウントする :class:`dict` の
   サブクラスです。これは、要素を辞書のキーとして保存し、そのカウントを
   辞書の値として保存する、順序付けされていないコレクションです。
   カウントは、0 や負のカウントを含む整数値をとれます。
   :class:`Counter` クラスは、他の言語のバッグや多重集合のようなものです。

   要素は、 *iterable* から数え上げられたり、他の *mapping* (やカウンタ)
   から初期化されます:

        >>> c = Counter()                           # 新しい空のカウンタ
        >>> c = Counter('gallahad')                 # イテラブルからの新しいカウンタ
        >>> c = Counter({'red': 4, 'blue': 2})      # マッピングからの新しいカウンタ
        >>> c = Counter(cats=4, dogs=8)             # キーワード引数からの新しいカウンタ

   カウンタオブジェクトは辞書のインタフェースを持ちますが、存在しない要素に
   対して :exc:`KeyError` を送出する代わりに 0 を返すという違いがあります:

        >>> c = Counter(['eggs', 'ham'])
        >>> c['bacon']                              # 存在しない要素のカウントは 0
        0

   カウントを 0 に設定しても、要素はカウンタから取り除かれません。
   完全に取り除くには、 ``del`` を使ってください:

        >>> c['sausage'] = 0                        # カウントが 0 のカウンタの項目
        >>> del c['sausage']                        # del は項目を実際に取り除く

   .. versionadded:: 2.7


   カウンタオブジェクトは、すべての辞書に利用できるものに加え、
   3 つのメソッドをサポートしています:

   .. method:: elements()

      それぞれの要素を、そのカウント分の回数だけ繰り返すイテレータを返します。
      要素は任意の順序で返されます。ある要素のカウントが 1 未満なら、
      :meth:`elements` はそれを無視します。

            >>> c = Counter(a=4, b=2, c=0, d=-2)
            >>> list(c.elements())
            ['a', 'a', 'a', 'a', 'b', 'b']

   .. method:: most_common([n])

      最も多い *n* 要素を、カウントが多いものから少ないものまで順に並べた
      リストを返します。 *n* が指定されなければ、 :func:`most_common` は
      カウンタの *すべての* 要素を返します。等しいカウントの要素は任意に
      並べられます:

            >>> Counter('abracadabra').most_common(3)
            [('a', 5), ('r', 2), ('b', 2)]

   .. method:: subtract([iterable-or-mapping])

      要素から *iterable* の要素または *mapping* の要素が引かれます。
      :meth:`dict.update` に似ていますが、カウントを
      置き換えるのではなく引きます。入力も出力も、 0 や負になりえます。

            >>> c = Counter(a=4, b=2, c=0, d=-2)
            >>> d = Counter(a=1, b=2, c=3, d=4)
            >>> c.subtract(d)
            Counter({'a': 3, 'b': 0, 'c': -3, 'd': -6})

   普通の辞書のメソッドは、以下の 2 つのメソッドがカウンタに対して異なる
   振る舞いをするのを除き、 :class:`Counter` オブジェクトにも利用できます。

   .. method:: fromkeys(iterable)

      このクラスメソッドは :class:`Counter` オブジェクトには実装されていません。

   .. method:: update([iterable-or-mapping])

      要素が *iterable* からカウントされるか、別の *mapping* (やカウンタ)
      が追加されます。 :meth:`dict.update` に似ていますが、カウントを
      置き換えるのではなく追加します。また、 *iterable* には ``(key, value)``
      対のシーケンスではなく、要素のシーケンスが求められます。

:class:`Counter` オブジェクトを使ったよくあるパターン::

    sum(c.values())                 # すべてのカウントの合計
    c.clear()                       # すべてのカウントをリセット
    list(c)                         # 単一の要素として列挙
    set(c)                          # 集合に変換
    dict(c)                         # 普通の辞書に変換
    c.items()                       # (elem, cnt) 対のリストに変換
    Counter(dict(list_of_pairs))    # (elem, cnt) 対のリストから変換
    c.most_common()[:-n:-1]         # 最も少ない n 要素
    c += Counter()                  # 0 と負の要素を取り除く

:class:`Counter` オブジェクトを組み合わせて多重集合 (1 以上のカウントをもつ
カウンタ) を作るために、いくつかの数学演算が提供されています。
足し算と引き算は、対応する要素を足したり引いたりすることによってカウンタを
組み合わせます。共通部分と合併集合は、対応するカウントの最大値と最小値を
返します。それぞれの演算はカウントに符号がついた入力を受け付けますが、
カウントが 0 以下である結果は出力から除かれます。


    >>> c = Counter(a=3, b=1)
    >>> d = Counter(a=1, b=2)
    >>> c + d                       # 2 つのカウンタを足し合わせる:  c[x] + d[x]
    Counter({'a': 4, 'b': 3})
    >>> c - d                       # 引く (正のカウンタだけを残す)
    Counter({'a': 2})
    >>> c & d                       # 共通部分:  min(c[x], d[x])
    Counter({'a': 1, 'b': 1})
    >>> c | d                       # 合併集合:  max(c[x], d[x])
    Counter({'a': 3, 'b': 2})

.. note::

   カウンタはもともと、推移するカウントを正の整数で表すために設計されました。
   しかし、他の型や負の値を必要とするユースケースを不必要に排除することが
   ないように配慮されています。このようなユースケースの助けになるように、
   この節で最低限の範囲と型の制限について記述します。

   * :class:`Counter` クラス自体は辞書のサブクラスで、キーと値に制限は
     ありません。値はカウントを表す数であることを意図していますが、
     値フィールドに任意のものを保存 *できます*\ 。

   * :meth:`most_common` メソッドが要求するのは、値が順序付け可能なことだけです。

   * ``c[key] += 1`` のようなインプレース演算では、値の型に必要なのは
     足し算と引き算ができることだけです。よって分数、浮動小数点数、
     小数も使え、負の値がサポートされています。これと同じことが、
     負や 0 の値を入力と出力に許す :meth:`update` と :meth:`subtract` メソッド
     にも言えます。

   * 多重集合メソッドは正の値を扱うユースケースに対してのみ設計されています。
     入力は負や 0 に出来ますが、正の値の出力のみが生成されます。
     型の制限はありませんが、値の型は足し算、引き算、比較をサポートしている
     必要があります。

   * :meth:`elements` メソッドは整数のカウントを要求します。
     これは 0 と負のカウントを無視します。

.. seealso::

    * Python 2.5 に適応する `Counter class <http://code.activestate.com/recipes/576611/>`_
      と Python 2.4 のための早期の `Bag recipe
      <http://code.activestate.com/recipes/259174/>`_\ 。

    * Smalltalk の `Bag class <http://www.gnu.org/software/smalltalk/manual-base/html_node/Bag.html>`_

    * Wikipedia の `Multisets <http://en.wikipedia.org/wiki/Multiset>`_ の項目。

    * `C++ multisets <http://www.demo2s.com/Tutorial/Cpp/0380__set-multiset/Catalog0380__set-multiset.htm>`_
      の例を交えたチュートリアル。

    * 数学的な多重集合の演算とそのユースケースは、
      *Knuth, Donald. The Art of Computer Programming Volume II,
      Section 4.6.3, Exercise 19* を参照してください。

    * 与えられた要素の集まりから与えられた大きさの別個の多重集合をすべて
      数え上げるには、 :func:`itertools.combinations_with_replacement`
      を参照してください。

          map(Counter, combinations_with_replacement('ABC', 2)) --> AA AB AC BB BC CC


:class:`deque` オブジェクト
---------------------------

.. class:: deque([iterable[, maxlen]])

   *iterable* で与えられるデータから、新しい deque オブジェクトを (:meth:`append` をつかって)
   左から右に初期化して返します。
   *iterable* が指定されない場合、新しい deque オブジェクトは空になります。

   Deque とは、スタックとキューを一般化したものです (この名前は「デック」と発音され、これは「double-ended
   queue」の省略形です)。Deque はどちらの側からも append と pop が可能で、スレッドセーフでメモリ効率がよく、
   どちらの方向からもおよそ ``O(1)`` のパフォーマンスで実行できます。

   :class:`list` オブジェクトでも同様の操作を実現できますが、これは高速な固定長の
   操作に特化されており、内部のデータ表現形式のサイズと位置を両方変えるような
   ``pop(0)`` や ``insert(0, v)`` などの操作ではメモリ移動のために ``O(n)``
   のコストを必要とします。

   .. versionadded:: 2.4

   *maxlen* が指定され無かったり *None* だった場合、 deque は不定のサイズまで
   大きくなります。それ以外の場合、 deque は指定された最大長に制限されます。
   長さが制限された deque がいっぱいになると、新しい要素を追加するときに追加した
   要素数分だけ追加した逆側から要素が捨てられます。長さが制限された deque は Unix に
   おける ``tail`` フィルタと似た機能を提供します。トランザクションの tracking や
   最近使った要素だけを残したいデータプール (pool of data) などにも便利です。

   .. versionchanged:: 2.6
      *maxlen* パラメータを追加しました。

   Deque オブジェクトは以下のようなメソッドをサポートしています:


   .. method:: append(x)

      *x* を deque の右側につけ加えます。


   .. method:: appendleft(x)

      *x* を deque の左側につけ加えます。


   .. method:: clear()

      deque からすべての要素を削除し、長さを 0 にします。


   .. method:: count(x)

      *x* に等しい deque の要素を数え上げます。

      .. versionadded:: 2.7

   .. method:: extend(iterable)

      イテレータ化可能な引数 iterable から得られる要素を deque の右側に追加し拡張します。


   .. method:: extendleft(iterable)

      イテレータ化可能な引数 iterable から得られる要素を deque の左側に追加し拡張します。注意: 左から追加した結果は、イテレータ引数の
      順序とは逆になります。


   .. method:: pop()

      deque の右側から要素をひとつ削除し、その要素を返します。要素がひとつも存在しない場合は :exc:`IndexError` を発生させます。


   .. method:: popleft()

      deque の左側から要素をひとつ削除し、その要素を返します。要素がひとつも存在しない場合は :exc:`IndexError` を発生させます。


   .. method:: remove(value)

      最初に現れる value を削除します。要素がみつからないない場合は :exc:`ValueError` を発生させます。

      .. versionadded:: 2.5

   .. method:: reverse()

      deque の要素をインプレースに逆転し、 ``None`` を返します。

      .. versionadded:: 2.7

   .. method:: rotate(n)

      deque の要素を全体で *n* ステップだけ右にローテートします。
      *n* が負の値の場合は、左にローテートします。Deque を
      ひとつ右にローテートすることは ``d.appendleft(d.pop())`` と同じです。


   deque オブジェクトは読み取り専用属性も 1 つ提供しています。

   .. attribute:: maxlen

      deque の最大長で、制限されていなければ *None* です。

      .. versionadded:: 2.7


上記の操作のほかにも、deque は次のような操作をサポートしています: イテレータ化、pickle、 ``len(d)``, ``reversed(d)``,
``copy.copy(d)``, ``copy.deepcopy(d)``, :keyword:`in` 演算子による包含検査、そして ``d[-1]``
などの添え字による参照。
両端についてインデックスアクセスは O(1) ですが、中央部分については O(n) の遅さです。
高速なランダムアクセスが必要ならリストを使ってください。

例:

.. doctest::

   >>> from collections import deque
   >>> d = deque('ghi')                 # 3つの要素からなる新しい deque をつくる。
   >>> for elem in d:                   # deque の要素をひとつずつたどる。
   ...     print elem.upper()
   G
   H
   I

   >>> d.append('j')                    # 新しい要素を右側につけたす。
   >>> d.appendleft('f')                # 新しい要素を左側につけたす。
   >>> d                                # deque の表現形式。
   deque(['f', 'g', 'h', 'i', 'j'])

   >>> d.pop()                          # いちばん右側の要素を削除し返す。
   'j'
   >>> d.popleft()                      # いちばん左側の要素を削除し返す。
   'f'
   >>> list(d)                          # deque の内容をリストにする。
   ['g', 'h', 'i']
   >>> d[0]                             # いちばん左側の要素をのぞく。
   'g'
   >>> d[-1]                            # いちばん右側の要素をのぞく。
   'i'

   >>> list(reversed(d))                # deque の内容を逆順でリストにする。
   ['i', 'h', 'g']
   >>> 'h' in d                         # deque を検索。
   True
   >>> d.extend('jkl')                  # 複数の要素を一度に追加する。
   >>> d
   deque(['g', 'h', 'i', 'j', 'k', 'l'])
   >>> d.rotate(1)                      # 右ローテート
   >>> d
   deque(['l', 'g', 'h', 'i', 'j', 'k'])
   >>> d.rotate(-1)                     # 左ローテート
   >>> d
   deque(['g', 'h', 'i', 'j', 'k', 'l'])

   >>> deque(reversed(d))               # 新しい deque を逆順でつくる。
   deque(['l', 'k', 'j', 'i', 'h', 'g'])
   >>> d.clear()                        # deque を空にする。
   >>> d.pop()                          # 空の deque からは pop できない。
   Traceback (most recent call last):
     File "<pyshell#6>", line 1, in -toplevel-
       d.pop()
   IndexError: pop from an empty deque

   >>> d.extendleft('abc')              # extendleft() は入力を逆順にする。
   >>> d
   deque(['c', 'b', 'a'])


:class:`deque` のレシピ
------------------------

この節では deque をつかったさまざまなアプローチを紹介します。

長さが制限された deque は Unix における ``tail`` フィルタに相当する機能を
提供します::

   def tail(filename, n=10):
       'ファイルの最後の n 行を返す.'
       return deque(open(filename), n)

別のアプローチとして deque を右に append して左に pop して使うことで追加した要素を維持するのに使えます::

    def moving_average(iterable, n=3):
        # moving_average([40, 30, 50, 46, 39, 44]) --> 40.0 42.0 45.0 43.0
        # http://en.wikipedia.org/wiki/Moving_average
        it = iter(iterable)
        d = deque(itertools.islice(it, n-1))
        d.appendleft(0)
        s = sum(d)
        for elem in it:
            s += elem - d.popleft()
            d.append(elem)
            yield s / float(n)

:meth:`rotate` メソッドのおかげで、 :class:`deque` の一部を切り出したり削除したりできることになります。たとえば ``del
d[n]`` の純粋な Python 実装では pop したい要素まで :meth:`rotate` します ::

   def delete_nth(d, n):
       d.rotate(-n)
       d.popleft()
       d.rotate(n)

:class:`deque` の切り出しを実装するのにも、同様のアプローチを使います。まず対象となる要素を :meth:`rotate` によって deque
の左端までもってきてから、 :meth:`popleft` をつかって古い要素を消します。そして、 :meth:`extend`
で新しい要素を追加したのち、逆のローテートでもとに戻せばよいのです。
このアプローチをやや変えたものとして、Forth スタイルのスタック操作、つまり ``dup``, ``drop``, ``swap``, ``over``,
``pick``, ``rot``, および ``roll`` を実装するのも簡単です。


:class:`defaultdict` オブジェクト
---------------------------------

.. class:: defaultdict([default_factory[, ...]])

   新しいディクショナリ状のオブジェクトを返します。 :class:`defaultdict` は組込みの
   :class:`dict` のサブクラスです。メソッドをオーバーライドし、書き込み可能なインスタンス変数を1つ追加している以外は
   :class:`dict` クラスと同じです。同じ部分については以下では省略されています。

   1つめの引数は :attr:`default_factory` 属性の初期値です。デフォルトは
   ``None`` です。残りの引数はキーワード引数もふくめ、 :class:`dict` のコンストラクタにあたえられた場合と同様に扱われます。

   .. versionadded:: 2.5

   :class:`defaultdict` オブジェクトは標準の :class:`dict` に加えて、以下のメソッドを実装しています:


   .. method:: defaultdict.__missing__(key)

      もし :attr:`default_factory` 属性が ``None`` であれば、このメソッドは
      :exc:`KeyError` 例外を、 *key* を引数として発生させます。

      もし :attr:`default_factory` 属性が ``None`` でなければ、このメソッドは
      :attr:`default_factory` を引数なしで呼び出し、あたえられた *key* に対応するデフォルト値を作ります。そしてこの値を *key*
      に対応する値を辞書に登録して返ります。

      もし :attr:`default_factory` の呼出が例外を発生させた場合には、変更せずそのまま例外を投げます。

      このメソッドは :class:`dict` クラスの :meth:`__getitem__` メソッドで、キー
      が存在しなかった場合によびだされます。値を返すか例外を発生させるのどち
      らにしても、 :meth:`__getitem__` からもそのまま値が返るか例外が発生します。

   :class:`defaultdict` オブジェクトは以下のインスタンス変数をサポートしています:


   .. attribute:: defaultdict.default_factory

      この属性は :meth:`__missing__` メソッドによって使われます。
      これは存在すればコンストラクタの第1引数によって初期化され、そうでなければ
      ``None`` になります。


:class:`defaultdict` の使用例
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:class:`list` を :attr:`default_factory` とすることで、キー=値ペアのシーケンスをリストの辞書へ簡単にグループ化できます。:

   >>> s = [('yellow', 1), ('blue', 2), ('yellow', 3), ('blue', 4), ('red', 1)]
   >>> d = defaultdict(list)
   >>> for k, v in s:
   ...     d[k].append(v)
   ...
   >>> d.items()
   [('blue', [2, 4]), ('red', [1]), ('yellow', [1, 3])]

それぞれのキーが最初に登場したとき、マッピングにはまだ存在しません。
そのためエントリは :attr:`default_factory` 関数が返す空の :class:`list` を使って自動的に作成されます。
:meth:`list.append` 操作は新しいリストに紐付けられます。キーが再度出現下場合には、通常の参照動作が行われます(そのキーに対応す
るリストが返ります)。そして :meth:`list.append` 操作で別の値をリスト
に追加します。このテクニックは :meth:`dict.setdefault` を使った等価なものよりシンプルで速いです:

   >>> d = {}
   >>> for k, v in s:
   ...     d.setdefault(k, []).append(v)
   ...
   >>> d.items()
   [('blue', [2, 4]), ('red', [1]), ('yellow', [1, 3])]

:attr:`default_factory` を :class:`int` にすると、 :class:`defaultdict` を(他の言語の bag や
multisetのように)要素の数え上げに便利に使うことができます:

   >>> s = 'mississippi'
   >>> d = defaultdict(int)
   >>> for k in s:
   ...     d[k] += 1
   ...
   >>> d.items()
   [('i', 4), ('p', 2), ('s', 4), ('m', 1)]

最初に文字が出現したときは、マッピングが存在しないので :attr:`default_factory` 関数が :func:`int` を呼んでデフォルトのカ
ウント0を生成します。インクリメント操作が各文字を数え上げます。

常に0を返す :func:`int` は特殊な関数でした。定数を生成するより速くて柔軟な方法は、
0に限らず何でも定数を生成する :func:`itertools.repeat` を使うことです。

   >>> def constant_factory(value):
   ...     return itertools.repeat(value).next
   >>> d = defaultdict(constant_factory('<missing>'))
   >>> d.update(name='John', action='ran')
   >>> '%(name)s %(action)s to %(object)s' % d
   'John ran to <missing>'

:attr:`default_factory` を :class:`set` に設定することで、
:class:`defaultdict` をセットの辞書を作るために利用することができます:

   >>> s = [('red', 1), ('blue', 2), ('red', 3), ('blue', 4), ('red', 1), ('blue', 4)]
   >>> d = defaultdict(set)
   >>> for k, v in s:
   ...     d[k].add(v)
   ...
   >>> d.items()
   [('blue', set([2, 4])), ('red', set([1, 3]))]

:func:`namedtuple` 名前付きフィールドを持つタプルのファクトリ関数
------------------------------------------------------------------

名前付きタプルはタプルの中の場所に意味を割り当てて、より読みやすく自己解説的な
コードを書けるようにします。通常のタプルが利用されていた場所で利用でき、
場所に対するインデックスの代わりに名前を使ってフィールドにアクセスできます。

.. function:: namedtuple(typename, field_names, [verbose=False], [rename=False])

   *typename* という名前の tuple の新しいサブクラスを返します。新しいサブクラスは、
   tuple に似ているけれどもインデックスやイテレータだけでなく属性名によるアクセスも
   できるオブジェクトを作るのに使います。このサブクラスのインスタンスは、わかりやすい
   docstring (型名と属性名が入っています) や、 tuple の内容を ``name=value`` という
   形のリストで返す使いやすい :meth:`__repr__` も持っています。

   *field_names* は ``['x', 'y']`` のような文字列のシーケンスです。
   *field_names* には、代わりに各属性名を空白文字 (whitespace) および/または
   カンマ (,) で区切った文字列を渡すこともできます。例えば、 ``'x y'`` や ``'x, y'`` です。

   アンダースコア (_) で始まる名前を除いて、 Python の正しい識別子 (identifier)
   ならなんでも属性名として使うことができます。正しい識別子とはアルファベット(letters),
   数字(digits), アンダースコア(_) を含みますが、数字やアンダースコアで始まる名前や、
   *class*, *for*, *return*, *global*, *pass*, *print*, *raise* などといった
   :mod:`keyword` は使えません。

   *rename* が真なら、不適切なフィールド名は自動的に位置引数に置き換えられます。
   例えば ``['abc', 'def', 'ghi', 'abc']`` は、予約語 ``def`` と
   重複しているフィールド名 ``abc`` が除去され、 ``['abc', '_1', 'ghi', '_3']``
   に変換されます。

   *verbose* が真なら、クラスを作る直前にクラス定義が表示されます。

   名前付きタプルのインスタンスはインスタンスごとの辞書を持たないので、
   軽量で、普通のタプル以上のメモリを使用しません。

   .. versionadded:: 2.6

   .. versionchanged:: 2.7
      *rename* のサポートを追加しました。

例:

.. doctest::
   :options: +NORMALIZE_WHITESPACE

   >>> Point = namedtuple('Point', ['x', 'y'], verbose=True)
   class Point(tuple):
           'Point(x, y)'
   <BLANKLINE>
           __slots__ = ()
   <BLANKLINE>
           _fields = ('x', 'y')
   <BLANKLINE>
           def __new__(_cls, x, y):
               'Create a new instance of Point(x, y)'
               return _tuple.__new__(cls, (x, y))
   <BLANKLINE>
           @classmethod
           def _make(cls, iterable, new=tuple.__new__, len=len):
               'Make a new Point object from a sequence or iterable'
               result = new(cls, iterable)
               if len(result) != 2:
                   raise TypeError('Expected 2 arguments, got %d' % len(result))
               return result
   <BLANKLINE>
           def __repr__(self):
               'Return a nicely formatted representation string'
               return 'Point(x=%r, y=%r)' % self
   <BLANKLINE>
           def _asdict(self):
               'Return a new OrderedDict which maps field names to their values'
               return OrderedDict(zip(self._fields, self))
   <BLANKLINE>
           def _replace(_self, **kwds):
               'Return a new Point object replacing specified fields with new values'
               result = _self._make(map(kwds.pop, ('x', 'y'), _self))
               if kwds:
                   raise ValueError('Got unexpected field names: %r' % kwds.keys())
               return result
   <BLANKLINE>
           def __getnewargs__(self):
               'Return self as a plain tuple.   Used by copy and pickle.'
               return tuple(self)
   <BLANKLINE>
           x = _property(_itemgetter(0), doc='Alias for field number 0')
           y = _property(_itemgetter(1), doc='Alias for field number 1')

   >>> Point = namedtuple('Point', 'x y')
   >>> p = Point(11, y=22)     # 順序による引数やキーワード引数を使ってインスタンス化
   >>> p[0] + p[1]             # 通常の tuple (11, 22) と同じようにインデックスアクセス
   33
   >>> x, y = p                # 通常の tuple と同じようにアンパック
   >>> x, y
   (11, 22)
   >>> p.x + p.y               # 名前でフィールドにアクセス
   33
   >>> p                       # name=value スタイルの読みやすい __repr__
   Point(x=11, y=22)

名前付きタプルは :mod:`csv` や :mod:`sqlite3` モジュールが返すタプルのフィールドに名前を
付けるときにとても便利です::

   EmployeeRecord = namedtuple('EmployeeRecord', 'name, age, title, department, paygrade')

   import csv
   for emp in map(EmployeeRecord._make, csv.reader(open("employees.csv", "rb"))):
       print emp.name, emp.title

   import sqlite3
   conn = sqlite3.connect('/companydata')
   cursor = conn.cursor()
   cursor.execute('SELECT name, age, title, department, paygrade FROM employees')
   for emp in map(EmployeeRecord._make, cursor.fetchall()):
       print emp.name, emp.title

タプルから継承したメソッドに加えて、名前付きタプルは3つの追加メソッドと
一つの属性をサポートしています。フィールド名との衝突を避けるために
メソッド名と属性名はアンダースコアで始まります。

.. classmethod:: somenamedtuple._make(iterable)

   既存の sequence や Iterable から新しいインスタンスを作るクラスメソッド.

   .. doctest::

      >>> t = [11, 22]
      >>> Point._make(t)
      Point(x=11, y=22)

.. method:: somenamedtuple._asdict()

   フィールド名を対応する値にマッピングする新しい順序付き辞書
   (:class:`OrderedDict`) を返します::

      >>> p._asdict()
      OrderedDict([('x', 11), ('y', 22)])

   .. versionchanged:: 2.7
      通常の :class:`dict` の代わりに :class:`OrderedDict` を返すようになりました。

.. method:: somenamedtuple._replace(kwargs)

   指定されたフィールドを新しい値で置き換えた、新しい名前付きタプルを作って返します::

      >>> p = Point(x=11, y=22)
      >>> p._replace(x=33)
      Point(x=33, y=22)

      >>> for partnum, record in inventory.items():
              inventory[partnum] = record._replace(price=newprices[partnum], timestamp=time.now())

.. attribute:: somenamedtuple._fields

   フィールド名をリストにしたタプル. 内省 (introspection) したり、既存の名前付きタプルを
   もとに新しい名前つきタプルを作成する時に便利です。

   .. doctest::

      >>> p._fields            # view the field names
      ('x', 'y')

      >>> Color = namedtuple('Color', 'red green blue')
      >>> Pixel = namedtuple('Pixel', Point._fields + Color._fields)
      >>> Pixel(11, 22, 128, 255, 0)
      Pixel(x=11, y=22, red=128, green=255, blue=0)

文字列に格納された名前を使って名前つきタプルから値を取得するには :func:`getattr`
関数を使います:

   >>> getattr(p, 'x')
   11

辞書を名前付きタプルに変換するには、 ``**`` 演算子 (double-star-operator,
:ref:`tut-unpacking-arguments` で説明しています) を使います。:

   >>> d = {'x': 11, 'y': 22}
   >>> Point(**d)
   Point(x=11, y=22)

名前付きタプルは通常の Python クラスなので、継承して機能を追加したり変更するのは
容易です。次の例では計算済みフィールドと固定幅の print format を追加しています:

    >>> class Point(namedtuple('Point', 'x y')):
            __slots__ = ()
            @property
            def hypot(self):
                return (self.x ** 2 + self.y ** 2) ** 0.5
            def __str__(self):
                return 'Point: x=%6.3f  y=%6.3f  hypot=%6.3f' % (self.x, self.y, self.hypot)

    >>> for p in Point(3, 4), Point(14, 5/7.):
            print p
    Point: x= 3.000  y= 4.000  hypot= 5.000
    Point: x=14.000  y= 0.714  hypot=14.018

このサブクラスは ``__slots__`` に空のタプルをセットしています。
これにより、インスタンス辞書の作成を抑制してメモリ使用量を低く保つのに役立ちます。

サブクラス化は新しいフィールドを追加するのには適していません。
代わりに、新しい名前付きタプルを :attr:`_fields` 属性を元に作成してください:

    >>> Point3D = namedtuple('Point3D', Point._fields + ('z',))

:meth:`_replace` でプロトタイプのインスタンスをカスタマイズする方法で、デフォルト値を
実現できます。

   >>> Account = namedtuple('Account', 'owner balance transaction_count')
   >>> default_account = Account('<owner name>', 0.0, 0)
   >>> johns_account = default_account._replace(owner='John')

列挙型定数は名前付きタプルでも実装できますが、クラス定義を利用した方がシンプルで
効率的です:

    >>> Status = namedtuple('Status', 'open pending closed')._make(range(3))
    >>> Status.open, Status.pending, Status.closed
    (0, 1, 2)
    >>> class Status:
            open, pending, closed = range(3)

.. seealso::

   `Named tuple recipe <http://code.activestate.com/recipes/500261/>`_
   は Python 2.4 で使えます。


:class:`OrderedDict` オブジェクト
---------------------------------

順序付き辞書 (ordered dictionary) は、ちょうど普通の辞書と同じようなものですが、
項目が挿入された順序を記憶します。順序付き辞書に渡ってイテレートするとき、
項目はそのキーが最初に追加された順序で返されます。

.. class:: OrderedDict([items])

   通常の :class:`dict` メソッドをサポートする、辞書のサブクラスの
   インスタンスを返します。 *OrderedDict* は、キーが最初に追加された順序を
   記憶します。新しい項目が既存の項目を上書きしても、元の挿入位置は
   変わらないままです。項目を削除して再挿入するとそれが最後に移動します。

   .. versionadded:: 2.7

.. method:: OrderedDict.popitem(last=True)

   順序付き辞書の :meth:`popitem` メソッドは、(key, value) 対を返して
   消去します。この対は *last* が真なら後入先出で、偽なら先入先出で
   返されます。

通常のマッピングのメソッドに加え、順序付き辞書は :func:`reversed` による
逆順の反復もサポートしています。

:class:`OrderedDict` 間の等価判定は順序に影響され、
``list(od1.items())==list(od2.items())`` として実装されます。
:class:`OrderedDict` オブジェクトと他のマッピング (:class:`Mapping`)
オブジェクトの等価判定は、順序に影響されず、通常の辞書と同様です。
これによって、 :class:`OrderedDict` オブジェクトは通常の辞書が使われるところ
ならどこでも代用できます。

:class:`OrderedDict` コンストラクタと :meth:`update` メソッドは、どちらも
キーワード引数を受け付けますが、その順序は失われます。これは、Python の
関数呼び出しの意味づけにおいて、キーワード引数は順序付けされていない辞書を
用いて渡されるからです。

.. seealso::

   `Equivalent OrderedDict recipe <http://code.activestate.com/recipes/576693/>`_
   that runs on Python 2.4 or later.

:class:`OrderedDict` の例とレシピ
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

順序付き辞書は挿入順序を記憶するので、ソートと組み合わせて使うことで、
ソートされた辞書を作れます::

    >>> # regular unsorted dictionary
    >>> d = {'banana': 3, 'apple':4, 'pear': 1, 'orange': 2}

    >>> # dictionary sorted by key
    >>> OrderedDict(sorted(d.items(), key=lambda t: t[0]))
    OrderedDict([('apple', 4), ('banana', 3), ('orange', 2), ('pear', 1)])

    >>> # dictionary sorted by value
    >>> OrderedDict(sorted(d.items(), key=lambda t: t[1]))
    OrderedDict([('pear', 1), ('orange', 2), ('banana', 3), ('apple', 4)])

    >>> # dictionary sorted by length of the key string
    >>> OrderedDict(sorted(d.items(), key=lambda t: len(t[0])))
    OrderedDict([('pear', 1), ('apple', 4), ('orange', 2), ('banana', 3)])

この新しい順序付き辞書は、項目が削除されてもソートされた順序を保持します。
しかし、キーが追加されるとき、そのキーは最後に追加され、ソートは
保持されません。

キーが *最後に* 挿入された順序を記憶するような、順序付き辞書の変種を
作るのも簡単です。新しい項目が既存の項目を上書きしたら、元の挿入位置は
最後に移動します::

    class LastUpdatedOrderedDict(OrderedDict):

        'Store items in the order the keys were last added'
        def __setitem__(self, key, value):
            if key in self:
                del self[key]
            OrderedDict.__setitem__(self, key, value)

順序付き辞書は、 :class:`Counter` クラスと組み合わせて
カウンタに要素が最初に現れた順序を記憶させられます::

   class OrderedCounter(Counter, OrderedDict):
        'Counter that remembers the order elements are first encountered'

        def __repr__(self):
            return '%s(%r)' % (self.__class__.__name__, OrderedDict(self))

        def __reduce__(self):
            return self.__class__, (OrderedDict(self),)


.. _abstract-base-classes:

ABCs - abstract base classes
----------------------------

collections モジュールは以下の :term:`ABC (抽象基底クラス)
<abstract base class>` を提供します:

=========================  =====================  ======================  ====================================================
ABC                        継承しているクラス     抽象メソッド            mixin メソッド
=========================  =====================  ======================  ====================================================
:class:`Container`                                ``__contains__``
:class:`Hashable`                                 ``__hash__``
:class:`Iterable`                                 ``__iter__``
:class:`Iterator`          :class:`Iterable`      ``next``                ``__iter__``
:class:`Sized`                                    ``__len__``
:class:`Callable`                                 ``__call__``

:class:`Sequence`          :class:`Sized`,        ``__getitem__``         ``__contains__``. ``__iter__``, ``__reversed__``.
                           :class:`Iterable`,                             ``index``, ``count``
                           :class:`Container`

:class:`MutableSequence`   :class:`Sequence`      ``__setitem__``         Sequence から継承したメソッドと、
                                                  ``__delitem__``,        ``append``, ``reverse``, ``extend``, ``pop``,
                                                  ``insert``,             ``remove``, ``__iadd__``

:class:`Set`               :class:`Sized`,                                ``__le__``, ``__lt__``, ``__eq__``, ``__ne__``,
                           :class:`Iterable`,                             ``__gt__``, ``__ge__``, ``__and__``, ``__or__``
                           :class:`Container`                             ``__sub__``, ``__xor__``, and ``isdisjoint``

:class:`MutableSet`        :class:`Set`           ``add``,                Set から継承したメソッドと、
                                                  ``discard``             ``clear``, ``pop``, ``remove``, ``__ior__``,
                                                                          ``__iand__``, ``__ixor__``, ``__isub__``

:class:`Mapping`           :class:`Sized`,        ``__getitem__``         ``__contains__``, ``keys``, ``items``, ``values``,
                           :class:`Iterable`,                             ``get``, ``__eq__``, ``__ne__``
                           :class:`Container`

:class:`MutableMapping`    :class:`Mapping`       ``__setitem__``         Mapping から継承したメソッドと、
                                                  ``__detitem__``,        ``pop``, ``popitem``, ``clear``, ``update``,
                                                                          ``setdefault``

:class:`MappingView`       :class:`Sized`                                 ``__len__``
:class:`KeysView`          :class:`MappingView`,                          ``__contains__``,
                           :class:`Set`                                   ``__iter__``
:class:`ItemsView`         :class:`MappingView`,                          ``__contains__``,
                           :class:`Set`                                   ``__iter__``
:class:`ValuesView`        :class:`MappingView`                           ``__contains__``, ``__iter__``
=========================  =====================  ======================  ====================================================


.. class:: Container
           Hashable
           Sized
           Callable

   それぞれメソッド :meth:`__contains__`, :meth:`__hash__`,
   :meth:`__len__`, and :meth:`__call__` を提供するクラスの ABC です。

.. class:: Iterable

   :meth:`__iter__` メソッドを提供するクラスの ABC です。
   :term:`iterable` の定義も参照してください。

.. class:: Iterator

   :meth:`__iter__` および :meth:`next` メソッドを提供するクラスの ABC です。
   :term:`iterator` の定義も参照してください。

.. class:: Sequence
           MutableSequence

   読み込み専用と、ミュータブルな :term:`シーケンス <sequence>` の ABC です。

.. class:: Set
           MutableSet

   読み込み専用と、ミュータブルな集合の ABC です。

.. class:: Mapping
           MutableMapping

   読み込み専用と、ミュータブルな :term:`マッピング <mapping>` の ABC です。

.. class:: MappingView
           ItemsView
           KeysView
           ValuesView

   マッピング、要素、キー、値の :term:`view <view>` の ABC です。

これらの ABC はクラスやインスタンスが特定の機能を提供しているかどうかを
調べるのに使えます。例えば::

    size = None
    if isinstance(myvar, collections.Sized):
       size = len(myvar)

幾つかの ABC はコンテナ型 API を提供するクラスを開発するのを助ける mixin 型としても
使えます。例えば、 :class:`Set` API を提供するクラスを作る場合、3つの基本になる
抽象メソッド :meth:`__contains__`, :meth:`__iter__`, :meth:`__len__` だけが
必要です。 ABC が残りの :meth:`__and__` や :meth:`isdisjoint` といったメソッドを
提供します::

    class ListBasedSet(collections.Set):
         ''' 速度よりもメモリ使用量を重視して、 hashable も提供しない
             set の別の実装 '''
         def __init__(self, iterable):
             self.elements = lst = []
             for value in iterable:
                 if value not in lst:
                     lst.append(value)
         def __iter__(self):
             return iter(self.elements)
         def __contains__(self, value):
             return value in self.elements
         def __len__(self):
             return len(self.elements)

    s1 = ListBasedSet('abcdef')
    s2 = ListBasedSet('defghi')
    overlap = s1 & s2            # __and__() は ABC により自動的に提供される

:class:`Set` と :class:`MutableSet` を mixin型として利用するときの注意点:

(1)
   幾つかの set の操作は新しい set を作るので、デフォルトの mixin メソッドは
   iterable から新しいインスタンスを作成する方法を必要とします。クラスの
   コンストラクタは ``ClassName(iterable)`` の形のシグネチャを持つと仮定されます。
   内部の :meth:`_from_iterable` というクラスメソッドが ``cls(iterable)``
   を呼び出して新しい set を作る部分でこの仮定が使われています。
   コンストラクタのシグネチャが異なるクラスで :class:`Set` を使う場合は、
   iterable 引数から新しいインスタンスを生成するように :meth:`_from_iterable`
   をオーバーライドする必要があります。

(2)
   (たぶん意味はそのままに速度を向上する目的で)比較をオーバーライドする場合、
   :meth:`__le__` だけを再定義すれば、その他の演算は自動的に追随します。

(3)
   :class:`Set` mixin型は set のハッシュ値を計算する :meth:`_hash` メソッドを
   提供しますが、すべての set が hashable や immutable とは限らないので、
   :meth:`__hash__` は提供しません。 mixin を使って hashable な set を作る場合は、
   :class:`Set` と :class:`Hashable` の両方を継承して、 ``__hash__ = Set._hash``
   と定義してください。

.. seealso::

   * Latest version of the `Python source code for the collections abstract base classes
     <http://svn.python.org/view/python/branches/release27-maint/Lib/_abcoll.py?view=markup>`_

   * :class:`MutableSet` を使った例として
     `OrderedSet recipe <http://code.activestate.com/recipes/576694/>`_

   * ABCs についての詳細は、 :mod:`abc` モジュールと :pep:`3119` を参照してください。
