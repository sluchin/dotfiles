
:mod:`random` --- 擬似乱数を生成する
====================================

.. module:: random
   :synopsis: よく知られている様々な分布をもつ擬似乱数を生成する。


このモジュールでは様々な分布をもつ擬似乱数生成器を実装しています。

.. seealso::

   最新バージョンの `random モジュールの Python ソースコード
   <http://svn.python.org/view/python/branches/release27-maint/Lib/random.py?view=markup>`_


整数用では、ある値域内の数の選択を一様にします。
シーケンス用には、シーケンスからのランダムな要素の一様な選択、\
リストの要素の順列をランダムに置き換える関数、\
順列を入れ替えずにランダムに取り出す関数があります。

実数用としては、一様分布、正規分布 (ガウス分布)、\
対数正規分布、負の指数分布、ガンマおよびベータ分布を計算する\
関数があります。
角度分布の生成用には、von Mises 分布が利用可能です。

ほとんど全てのモジュール関数は基礎となる関数 :func:`random` に依存\
します。この関数は半開区間 [0.0, 1.0) の値域を持つ一様な浮動小数点数を生\
成します。Python は中心となる乱数生成器として Mersenne Twister を使いま\
す。これは 53 ビットの浮動小数点を生成し、周期が  2\*\*19937-1、本体は C \
で実装されていて、高速でスレッドセーフです。Mersenne Twister は、現存す\
る中で、最も大規模にテストされた乱数生成器のひとつです。しかし、完全に決\
定論的であるため、この乱数生成器は全ての目的に合致しているわけではなく、\
暗号化の目的には全く向いていません。

このモジュールで提供されている関数は、実際には :class:`random.Random`
クラスの隠蔽されたインスタンスのメソッドにバインドされています。
内部状態を共有しない生成器を取得するため、自分で :class:`Random`
のインスタンスを生成することができます。異なる :class:`Random`
のインスタンスを各スレッド毎に生成し、 :meth:`jumpahead`
メソッドを使うことで各々のスレッドにおいて生成された乱数列が\
できるだけ重複しないようにすれば、マルチスレッドプログラムを作成する上で\
特に便利になります。

自分で考案した基本乱数生成器を使いたいなら、クラス :class:`Random` を\
サブクラス化することもできます: この場合、メソッド
:meth:`random` 、 :meth:`seed` 、 :meth:`getstate` 、 :meth:`setstate` 、および
:meth:`jumpahead` をオーバライドしてください。
オプションとして、新しいジェネレータは :meth:`getrandbits`
メソッドを提供できます --- これにより :meth:`randrange` メソッドが\
任意に大きな範囲から選択を行えるようになります。

.. versionadded:: 2.4
   :meth:`getrandbits` メソッド.

サブクラス化の例として、 :mod:`random` モジュールは :class:`WichmannHill`
クラスを提供します。このクラスは Python だけで書かれた代替生成器を実装し\
ています。このクラスは、乱数生成器に Wichmann-Hill 法を使っていた古いバ\
ージョンの Python から得られた結果を再現するための、後方互換の手段になり\
ます。ただし、この Wichmann-Hill 生成器はもはや推奨することができない\
ということに注意してください。現在の水準では生成される周期が短すぎ、また\
厳密な乱数性試験に合格しないことが知られています。こうした欠点を修正した\
最近の改良についてはページの最後に挙げた参考文献を参照してください。

.. versionchanged:: 2.3
   MersenneTwister が Wichmann-Hill の代わりにデフォルト生成器になりました。

:mod:`random` モジュールは :class:`SystemRandom` クラスも提供していますが、
このクラスは OS が提供している乱数発生源を利用して乱数を生成するシステム関数
:func:`os.urandom` を使うものです。

保守関数:


.. function:: seed([x])

   基本乱数生成器を初期化します。
   オプション引数 *x* はハッシュ可能(:term:`hashable`)な任意のオブジェクトを\
   とり得ます。 *x* が省略されるか ``None`` の場合、現在のシステム\
   時間が使われます; 現在のシステム時間はモジュールが最初にインポート\
   された時に乱数生成器を初期化するためにも使われます。

   乱数の発生源をオペレーティングシステムが提供している場合、システム時刻の\
   代わりにその発生源が使われます（詳細については :func:`os.urandom`
   関数を参照）。

   .. versionchanged:: 2.4
      以前は、オペレーティングシステムのリソースは使われませんでした。

.. function:: getstate()

   乱数生成器の現在の内部状態を記憶したオブジェクトを返します。
   このオブジェクトを :func:`setstate` に渡して内部状態を\
   復帰することができます。

   .. versionadded:: 2.1
   .. versionchanged:: 2.6
      Python 2.6 が作り出す状態オブジェクトは以前のバージョンには読み込めません。


.. function:: setstate(state)

   *state* は予め :func:`getstate` を呼び出して得ておかなくては\
   なりません。 :func:`setstate` は :func:`setstate` が呼び出\
   された時の乱数生成器の内部状態を復帰します。

   .. versionadded:: 2.1


.. function:: jumpahead(n)

   内部状態を、現在の状態から、非常に離れているであろう状態に変更します。
   *n* は非負の整数です。
   これはマルチスレッドのプログラムが複数の :class:`Random` クラス\
   のインスタンスと結合されている場合に非常に便利です:
   :meth:`setstate` や :meth:`seed`
   は全てのインスタンスを同じ内部状態にするのに\
   使うことができ、その後 :meth:`jumpahead` を使って各インスタンスの\
   内部状態を引き離すことができます。

   .. versionadded:: 2.1

   .. versionchanged:: 2.3
      *n* ステップ先の特定の状態になるのではなく、
      ``jumpahead(n)`` は何ステップも離れているであろう別の状態にする。


.. function:: getrandbits(k)

   *k* ビット分の乱数ビットを納めた Python の :class:`long` 整数を返します。
   このメソッドは MersenneTwister 生成器で提供されており、その他の\
   乱数生成器でもオプションのAPIとして提供されているかもしれません。
   このメソッドが使えるとき、 :meth:`randrange` メソッドは大きな\
   範囲を扱えるようになります。

   .. versionadded:: 2.4

整数用の関数:


.. function:: randrange([start,] stop[, step])

   ``range(start, stop, step)`` の要素からランダムに選ばれた要素を返します。
   この関数は  ``choice(range(start, stop, step))``
   と等価ですが、実際には range オブジェクトを生成しません。

   .. versionadded:: 1.5.2


.. function:: randint(a, b)

   ``a <= N <= b`` であるようなランダムな整数 *N* を返します。

シーケンス用の関数:


.. function:: choice(seq)

   空でないシーケンス *seq* からランダムに要素を返します。
   *seq* が空のときは、 :exc:`IndexError` が送出されます。


.. function:: shuffle(x[, random])

   シーケンス *x* を直接変更によって混ぜます。
   オプションの引数 *random* は、値域が [0.0, 1.0) のランダムな\
   浮動小数点数を返すような引数を持たない関数です; 標準では、
   この関数は :func:`random` です。

   かなり小さい ``len(x)`` であっても、 *x* の順列は\
   ほとんどの乱数生成器の周期よりも大きくなるので注意してください;
   このことは長いシーケンスに対してはほとんどの順列は生成されないことを\
   意味します。


.. function:: sample(population, k)

   母集団のシーケンスから選ばれた長さ *k* の一意な要素からなるリスト\
   を返します。値の置換を行わないランダムサンプリングに用いられます。

   .. versionadded:: 2.3

   母集団自体を変更せずに、母集団内の要素を含む新たなリストを返します。返さ\
   れたリストは選択された順に並んでいるので、このリストの部分スライスもラン\
   ダムなサンプルになります。これにより、くじの当選者を1等賞と2等賞（の部分\
   スライス）に分けるといったことも可能です。母集団の要素はハッシュ可能
   (:term:`hashable`) でな\
   くても、ユニークでなくても、かまいません。母集団が繰り返しを含む場合、返\
   されたリストの各要素はサンプルから選択可能な要素になります。整数の並びか\
   らサンプルを選ぶには、引数に :func:`xrange` オブジェクトを使いましょう。
   特に、巨大な母集団からサンプルを取るとき、速度と空間効率が上がります。
   ``sample(xrange(10000000), 60)``

以下の関数は特殊な実数値分布を生成します。関数パラメタは\
対応する分布の公式において、数学的な慣行に従って使われている\
変数から取られた名前がつけられています; これらの公式のほとんどは\
多くの統計学のテキストに載っています。


.. function:: random()

   値域 [0.0, 1.0) の次のランダムな浮動小数点数を返します。


.. function:: uniform(a, b)

   ``a <= b`` であれば ``a <= N <= b`` であるようなランダムな浮動小数点数
   *N* を返し、 ``b < a`` であれば ``b <= N <= a`` になります。

   端点 b が値の範囲に含まれるかどうかは、等式
   a + (b-a) * random() における浮動小数点の丸めに依存します。

.. function:: triangular(low, high, mode)

   ``low <= N < high`` でありこれら境界値の間に指定された最頻値 *mode*
   を持つようなランダムな浮動小数点数 *N* を返します。境界 *low* と *high*
   のデフォルトは 0 と 1 です。最頻値 *mode* のデフォルトは両境界値の\
   中点になり、対称な分布を与えます。

   .. versionadded:: 2.6


.. function:: betavariate(alpha, beta)

   ベータ分布です。引数の満たすべき条件は ``alpha > 0`` および
   ``beta > 0`` です。 0 から 1 の値を返します。


.. function:: expovariate(lambd)

   指数分布です。 *lambd* は平均にしたい値で 1.0 を割ったものです。
   (このパラメタは "lambda" と呼ぶべきなのですが、Python の予約語
   なので使えません。) 返される値の範囲は 0 から正の無限大です。


.. function:: gammavariate(alpha, beta)

   ガンマ分布です。 (ガンマ関数 *ではありません* ！)  引数の満たすべき条件は
   ``alpha > 0`` および ``beta > 0`` です。


.. function:: gauss(mu, sigma)

   ガウス分布です。 *mu* は平均であり、 *sigma* は標準偏差です。
   この関数は後で定義する関数 :func:`normalvariate` より少しだけ高速です。


.. function:: lognormvariate(mu, sigma)

   対数正規分布です。この分布を自然対数を用いた分布にした場合、
   平均 *mu* で標準偏差 *sigma* の正規分布になるでしょう。 *mu*
   は任意の値を取ることができ、 *sigma* はゼロより\
   大きくなければなりません。


.. function:: normalvariate(mu, sigma)

   正規分布です、 *mu* は平均で、 *sigma* は標準偏差です。


.. function:: vonmisesvariate(mu, kappa)

   *mu* は平均の角度で、0 から 2\*\ *pi* までのラジアンで\
   表されます。 *kappa* は濃度パラメタで、ゼロまたはそれ以上\
   でなければなりません。 *kappa* がゼロに等しい場合、\
   この分布は範囲 0 から 2\*\ *pi* の一様でランダムな角度の\
   分布に退化します。


.. function:: paretovariate(alpha)

   パレート分布です。 *alpha* は形状パラメタです。


.. function:: weibullvariate(alpha, beta)

   ワイブル分布です。 *alpha* はスケールパラメタで、 *beta* は形状パラメタです。

代替の乱数生成器:


.. class:: WichmannHill([seed])

   乱数生成器として Wichmann-Hill アルゴリズムを実装するクラスです。
   :class:`Random` クラスと同じメソッド全てと、下で説明する :meth:`whseed`
   メソッドを持ちます。このクラスは、Python だけで実装されているので、スレ\
   ッドセーフではなく、呼び出しと呼び出しの間にロックが必要です。また、周期\
   が 6,953,607,871,644 と短く、独立した2つの乱数列が重複しないように注意が\
   必要です。


.. function:: whseed([x])

   これは obsolete で、バージョン 2.1 以前の Python と、ビット・レベルの互\
   換性のために提供されてます。詳細は :func:`seed` を参照してください。
   :func:`whseed` は、引数に与えた整数が異なっても、内部状態が異なること\
   を保障しません。取り得る内部状態の個数が 2\*\*24 以下になる場合もあります。


.. class:: SystemRandom([seed])

   オペレーティングシステムの提供する発生源によって乱数を生成する
   :func:`os.urandom` 関数を使うクラスです。
   すべてのシステムで使えるメソッドではありません。
   ソフトウェアの状態に依存してはいけませんし、一連の操作は再現\
   不能です。それに応じて、 :meth:`seed` と :meth:`jumpahead`
   メソッドは何の影響も及ぼさず、無視されます。 :meth:`getstate` と :meth:`setstate`
   メソッドが呼び出されると、例外 :exc:`NotImplementedError` が送出されます。

   .. versionadded:: 2.4

基本使用例::

   >>> random.random()        # Random float x, 0.0 <= x < 1.0
   0.37444887175646646
   >>> random.uniform(1, 10)  # Random float x, 1.0 <= x < 10.0
   1.1800146073117523
   >>> random.randint(1, 10)  # Integer from 1 to 10, endpoints included
   7
   >>> random.randrange(0, 101, 2)  # Even integer from 0 to 100
   26
   >>> random.choice('abcdefghij')  # Choose a random element
   'c'

   >>> items = [1, 2, 3, 4, 5, 6, 7]
   >>> random.shuffle(items)
   >>> items
   [7, 3, 2, 5, 6, 4, 1]

   >>> random.sample([1, 2, 3, 4, 5],  3)  # Choose 3 elements
   [4, 1, 5]



.. seealso::

   M. Matsumoto and T. Nishimura, "Mersenne Twister: A 623-dimensionally
   equidistributed uniform pseudorandom number generator", ACM Transactions on
   Modeling and Computer Simulation Vol. 8, No. 1, January pp.3-30 1998.

   Wichmann, B. A. & Hill, I. D., "Algorithm AS 183: An efficient and portable
   pseudo-random number generator", Applied Statistics 31 (1982) 188-190.

