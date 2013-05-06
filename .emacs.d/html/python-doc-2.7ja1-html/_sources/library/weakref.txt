:mod:`weakref` --- 弱参照
=========================

.. module:: weakref
   :synopsis: 弱参照と弱辞書のサポート。
.. moduleauthor:: Fred L. Drake, Jr. <fdrake@acm.org>
.. moduleauthor:: Neil Schemenauer <nas@arctrix.com>
.. moduleauthor:: Martin von Lowis <martin@loewis.home.cs.tu-berlin.de>
.. sectionauthor:: Fred L. Drake, Jr. <fdrake@acm.org>


.. versionadded:: 2.1

:mod:`weakref` モジュールは、Pythonプログラマがオブジェクトへの弱参照
(:dfn:`weak refarence`)を作成できるようにします。

以下では、用語 リファレント(:dfn:`referent`) は弱参照が参照するオブジェクトを
意味します。

オブジェクトに対する弱参照は、そのオブジェクトを生かしておく十分条件にはなりません:
あるリファレントに対する参照が弱参照しか残っていない場合、ガベージコレクション
(:term:`garbage collection`)機構は自由にリファレントを破壊し、そのメモリを別の用途に再利用できます。
弱参照の主な用途は、巨大なオブジェクトを保持するキャッシュやマップ型の実装において、
キャッシュやマップ型にあるという理由だけオブジェクトを存続させたくない場合です。

例えば、巨大なバイナリ画像のオブジェクトがたくさんあり、それぞれに名前を
関連付けたいとします。 Python の辞書型を使って名前を画像に対応付けたり画像を
名前に対応付けたりすると、画像オブジェクトは辞書内のキーや値に使われているため
存続しつづけることになります。 :mod:`weakref` モジュールが提供している
:class:`WeakKeyDictionary` や :class:`WeakValueDictionary` クラスはその代用で、
対応付けを構築するのに弱参照を使い、キャッシュやマップ型に存在するという
理由だけでオブジェクトを存続させないようにします。
例えば、もしある画像オブジェクトが :class:`WeakValueDictionary` の
値になっていた場合、最後に残った画像オブジェクトへの参照を弱参照マップ型が
保持していれば、ガーベジコレクションはこのオブジェクトを再利用でき、
画像オブジェクトに対する弱参照内の対応付けは削除されます。

:class:`WeakKeyDictionary` や :class:`WeakValueDictionary` は弱参照を使って
実装されていて、キーや値がガベージコレクションによって回収されたことを
弱参照辞書に知らせるような弱参照オブジェクトのコールバック関数を設定しています。
ほとんどのプログラムが、いずれかの弱参照辞書型を使うだけで必要を満たせるはずです ---
自作の弱参照辞書を直接作成する必要は普通はありません。
とはいえ、高度な使い方をするために、弱参照辞書の実装に使われている低水準の機構は
:mod:`weakref` モジュールで公開されています。

.. .. note:
  
      Weak references to an object are cleared before the object's :meth:`__del__`
      is called, to ensure that the weak reference callback (if any) finds the
      object still alive.

.. note::

   オブジェクトへの弱参照は、そのオブジェクトの :meth:`__del__` メソッドが呼び出される\
   前にクリアされます。弱参照のコールバック呼ばれるときに、そのオブジェクトがまだ\
   生存しているためです。

すべてのオブジェクトを弱参照できるわけではありません。弱参照できるオブジェクトは、
クラスインスタンス、(Cではなく) Pythonで書かれた関数、(束縛および非束縛の両方の)
メソッド、 :class:`set` と :class:`frozenset` 型、ファイルオブジェクト、
ジェネレータ(:term:`generator`)、 type オブジェクト、 :mod:`bsddb` モジュールの
:class:`DBcursor` 型、ソケット型、 :class:`array` 型、 :class:`deque` 型、
正規表現パターンオブジェクト, code オブジェクトです。

.. versionchanged:: 2.4
   ファイル、ソケット、 :class:`array` 、および正規表現パターンのサポートを追加しました.

.. versionchanged:: 2.7
   thread.lock, threading.Lock, code オブジェクトのサポートを追加しました。

:class:`list` や :class:`dict` など、いくつかの組み込み型は弱参照を
直接サポートしませんが、以下のようにサブクラス化を行えばサポートを追加できます::

   class Dict(dict):
       pass

   obj = Dict(red=1, green=2, blue=3)   # このオブジェクトは弱参照可能

.. impl-detail::

   :class:`tuple` や :class:`long` など、他の組み込み型はサブクラス化しても
   弱参照をサポートしません。

拡張型は、簡単に弱参照をサポートできます。詳細については、 :ref:`weakref-support`
を参照してください。


.. class:: ref(object[, callback])

   *object* への弱参照を返します。リファレントがまだ生きているならば、元の
   オブジェクトは参照オブジェクトの呼び出しで取り出せす。
   リファレントがもはや生きていないならば、参照オブジェクトを呼び出したときに
   :const:`None` を返します。 *callback* に :const:`None` 以外の値を与えた場合、
   オブジェクトをまさに後始末処理しようとするときに呼び出します。
   このとき弱参照オブジェクトは *callback* の唯一のパラメタとして渡されます。
   リファレントはもはや利用できません。

   同じオブジェクトに対してたくさんの弱参照を作れます。それぞれの弱参照に対して
   登録されたコールバックは、もっとも新しく登録されたコールバックからもっとも
   古いものへと呼び出されます。

   コールバックが発生させた例外は標準エラー出力に書き込まれますが、伝播されません。
   それらはオブジェクトの :meth:`__del__` メソッドが発生させる例外と完全に同じ方法で
   処理されます。

   *object* がハッシュ可能(:term:`hashable`)ならば、弱参照はハッシュ可能です。
   それらは *object* が削除された後でもそれらのハッシュ値を保持します。
   *object* が削除されてから初めて :func:`hash` が呼び出された場合に、その呼び出しは
   :exc:`TypeError` を発生させます。

   弱参照は等価性のテストをサポートしていますが、順序をサポートしていません。
   参照がまだ生きているならば、 *callback* に関係なく二つの参照はそれらの
   リファレントと同じ等価関係を持ちます。リファレントのどちらか一方が削除された場合、
   参照オブジェクトが同一である場合に限り、その参照は等価です。

   .. versionchanged:: 2.4
      以前はファクトリでしたが、サブクラス化可能な型になりました。
      :class:`object` 型から派生しています.


.. function:: proxy(object[, callback])

   弱参照を使う *object* へのプロキシを返します。
   弱参照オブジェクトを明示的な参照外しをしながら利用する代わりに、多くのケースで
   プロキシを利用することができます。
   返されるオブジェクトは、 *object* が呼び出し可能かどうかによって、 ``ProxyType``
   または ``CallableProxyType`` のどちらかの型を持ちます。
   プロキシオブジェクトはリファレントに関係なくハッシュ可能(:term:`hashable`)ではありません。
   これによって、それらの基本的な変更可能という性質による多くの問題を避けています。
   そして、辞書のキーとしての利用を妨げます。
   *callback* は :func:`ref` 関数の同じ名前のパラメータと同じものです。

   .. note::

      訳注: リファレントが変更不能型であっても、プロキシはリファレントが消えるという
      状態の変更があるために、変更可能型です。

.. function:: getweakrefcount(object)

   *object* を参照する弱参照とプロキシの数を返します。


.. function:: getweakrefs(object)

   *object* を参照するすべての弱参照とプロキシオブジェクトのリストを返します。


.. class:: WeakKeyDictionary([dict])

   キーを弱参照するマッピングクラス。キーへの強参照がなくなったときに、
   辞書のエントリは捨てられます。アプリケーションの他の部分が所有するオブジェクトへ
   属性を追加することもなく、それらのオブジェクトに追加データを関連づけるために
   使うことができます。これは属性へのアクセスをオーバーライドするオブジェクトに
   特に便利です。

   .. note::

      警告: :class:`WeakKeyDictionary` は Python 辞書型の上に作られているので、
      反復処理を行うときにはサイズ変更してはなりません。 :class:`WeakKeyDictionary`
      の場合、反復処理の最中にプログラムが行った操作が、(ガベージコレクションの副作用として)
      「魔法のように」辞書内の要素を消し去ってしまうため、確実なサイズ変更は困難なのです。

:class:`WeakKeyDictionary` オブジェクトは、以下の追加のメソッドを持ちます。
これらのメソッドは、内部の参照を直接公開します。
その参照は、利用される時に生存しているとは限りません。
なので、参照を利用する前に、その参照をチェックする必要があります。
これにより、必要なくなったキーの参照が残っているために、ガベージコレクタがそのキー\
を削除できなくなる事態を避ける事ができます。

.. method:: WeakKeyDictionary.iterkeyrefs()

   .. Return an :term:`iterator` that yields the weak references to the keys.

   キーへの弱参照を生成する :term:`iterator` を返します。

   .. versionadded:: 2.5

.. method:: WeakKeyDictionary.keyrefs()

   .. Return a list of weak references to the keys.

   キーへの弱参照のリストを返します。

   .. versionadded:: 2.5


.. class:: WeakValueDictionary([dict])

   値を弱参照するマッピングクラス。値への強参照が存在しなくなったときに、
   辞書のエントリは捨てられます。

   .. note::

      警告: :class:`WeakValueDictionary` は Python 辞書型の上に作られているので、
      反復処理を行うときにはサイズ変更してはなりません。 :class:`WeakKeyDictionary`
      の場合、反復処理の最中にプログラムが行った操作が、(ガベージコレクションの副作用として)
      「魔法のように」辞書内の要素を消し去ってしまうため、確実なサイズ変更は困難なのです。

.. :class:`WeakValueDictionary` objects have the following additional methods.
   These method have the same issues as the :meth:`iterkeyrefs` and :meth:`keyrefs`
   methods of :class:`WeakKeyDictionary` objects.

:class:`WeakValueDictionary` オブジェクトは、以下の追加のメソッドを持ちます。
これらのメソッドは、 :class:`WeakKeyDictionary` クラスの :meth:`iterkeyrefs`
と :meth:`keyrefs` メソッドと同じ目的を持っています。

.. method:: WeakValueDictionary.itervaluerefs()

   .. Return an :term:`iterator` that yields the weak references to the values.

   値への弱参照を生成するイテレータ(:term:`iterator`)を返します。

   .. versionadded:: 2.5


.. method:: WeakValueDictionary.valuerefs()

   .. Return a list of weak references to the values.

   値への弱参照のリストを返します。

   .. versionadded:: 2.5


.. class:: WeakSet([elements])

   要素への弱参照を持つ集合型。
   要素への強参照が無くなったときに、その要素は削除されます。

   .. versionadded:: 2.7


.. data:: ReferenceType

   弱参照オブジェクトのための型オブジェクト。


.. data:: ProxyType

   呼び出し可能でないオブジェクトのプロキシのための型オブジェクト。


.. data:: CallableProxyType

   呼び出し可能なオブジェクトのプロキシのための型オブジェクト。


.. data:: ProxyTypes

   プロキシのためのすべての型オブジェクトを含むシーケンス。
   これは両方のプロキシ型の名前付けに依存しないで、オブジェクトがプロキシかどうかのテストをより簡単にできます。


.. exception:: ReferenceError

   プロキシオブジェクトが使われても、元のオブジェクトがガベージコレクション
   されてしまっているときに発生する例外。これは標準の :exc:`ReferenceError` 例外と同じです。


.. seealso::

   :pep:`0205` - Weak References
      この機能の提案と理論的根拠。初期の実装と他の言語における類似の機能についての情報へのリンクを含んでいます。


.. _weakref-objects:

弱参照オブジェクト
------------------

弱参照オブジェクトは属性あるいはメソッドを持ちません。しかし、リファレントが
まだ存在するならば、呼び出すことでそのリファレントを取得できるようにします::

   >>> import weakref
   >>> class Object:
   ...     pass
   ...
   >>> o = Object()
   >>> r = weakref.ref(o)
   >>> o2 = r()
   >>> o is o2
   True

リファレントがもはや存在しないならば、参照オブジェクトの呼び出しは :const:`None` を返します::

   >>> del o, o2
   >>> print r()
   None

弱参照オブジェクトがまだ生きているかどうかのテストは、式 ``ref() is not None``
を用いて行われます。通常、参照オブジェクトを使う必要があるアプリケーションコードはこのパターンに従います::

   # rは弱参照オブジェクト
   o = r()
   if o is None:
       # リファレントがガーベジコレクトされた
       print "Object has been allocated; can't frobnicate."
   else:
       print "Object is still live!"
       o.do_something_useful()

"生存性(liveness)"のテストを分割すると、スレッド化されたアプリケーションにおいて競合状態を作り出します。
(訳注:``if r() is not None: r().do_something()`` では、2度目のr()がNoneを返す可能性があります)
弱参照が呼び出される前に、他のスレッドは弱参照が無効になる原因となり得ます。
上で示したイディオムは、シングルスレッドのアプリケーションと同じくマルチスレッド化されたアプリケーションにおいても安全です。

サブクラス化を行えば、 :class:`ref` オブジェクトの特殊なバージョンを作成できます。これは :class:`WeakValueDictionary`
の実装で使われており、マップ内の各エントリによるメモリのオーバヘッドを減らしています。こうした実装は、ある参照に追加情報を関連付けたい場合に便利ですし、
リファレントを取り出すための呼び出し時に何らかの追加処理を行いたい場合にも使えます。

以下の例では、 :class:`ref` のサブクラスを使って、あるオブジェクトに追加情報を保存し、リファレントがアクセスされたときにその値に作用
をできるようにするための方法を示しています::

   import weakref

   class ExtendedRef(weakref.ref):
       def __init__(self, ob, callback=None, **annotations):
           super(ExtendedRef, self).__init__(ob, callback)
           self.__counter = 0
           for k, v in annotations.iteritems():
               setattr(self, k, v)

       def __call__(self):
           """Return a pair containing the referent and the number of
           times the reference has been called.
           """
           ob = super(ExtendedRef, self).__call__()
           if ob is not None:
               self.__counter += 1
               ob = (ob, self.__counter)
           return ob

.. _weakref-example:

例
--

この簡単な例では、アプリケーションが以前に参照したオブジェクトを
取り出すためにオブジェクトIDを利用する方法を示します。
オブジェクトに生きたままであることを強制することなく、オブジェクトの
IDを他のデータ構造の中で使うことができ、必要に応じてIDからオブジェクトを
取り出せます。

.. Example contributed by Tim Peters

::

   import weakref

   _id2obj_dict = weakref.WeakValueDictionary()

   def remember(obj):
       oid = id(obj)
       _id2obj_dict[oid] = obj
       return oid

   def id2obj(oid):
       return _id2obj_dict[oid]

