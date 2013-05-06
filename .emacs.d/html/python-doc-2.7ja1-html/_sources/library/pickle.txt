
:mod:`pickle` --- Python オブジェクトの整列化
=============================================

.. index::
   single: persistence
   pair: persistent; objects
   pair: serializing; objects
   pair: marshalling; objects
   pair: flattening; objects
   pair: pickling; objects

.. module:: pickle
   :synopsis: Python オブジェクトからバイトストリームへの変換、およびその逆。

.. sectionauthor:: Jim Kerr <jbkerr@sr.hp.com>.
.. sectionauthor:: Barry Warsaw <barry@zope.com>

:mod:`pickle` モジュールでは、Python オブジェクトデータ構造を直列化
(serialize) したり非直列化 (de-serialize)するための基礎的ですが強力な
アルゴリズムを実装しています。 "Pickle 化 (Pickling)" は Python のオブ
ジェクト階層をバイトストリームに変換する過程を指します。"非 Pickle 化
(unpickling)" はその逆の操作で、バイトストリームをオブジェクト階層に戻
すように変換します。Pickle 化 (及び非 Pickle 化) は、別名 "直列化
(serialization)" や "整列化 (marshalling)" [#]_ 、 "平坦化
(flattening)" として知られていますが、ここでは混乱を避けるため、用語
として "Pickle 化" および  "非Pickle 化" を使います。

このドキュメントでは :mod:`pickle` モジュールおよび :mod:`cPickle` モ
ジュールの両方について記述します。

.. warning::

   :mod:`pickle` モジュールはエラーや不正に生成されたデータに対するセキュリティを
   考慮していません。信頼できない、あるいは認証されていないソースから受信したデータを
   unpickle してはいけません。


他の Python モジュールとの関係
------------------------------

:mod:`pickle` モジュールには :mod:`cPickle` と呼ばれる最適化のなされ
た親類モジュールがあります。名前が示すように、 :mod:`cPickle` は C で書
かれており、このため :mod:`pickle` より 1000 倍くらいまで高速になる可
能性があります。しかしながら :mod:`cPickle` では :func:`Pickler` およ
び  :func:`Unpickler` クラスのサブクラス化をサポートしていません。
これは :mod:`cPickle` では、これらは関数であってクラスではないからで
す。ほとんどのアプリケーションではこの機能は不要であり、 :mod:`cPickle`
の持つ高いパフォーマンスの恩恵を受けることができます。その他の点では、
二つのモジュールにおけるインタフェースはほとんど同じです; このマニュア
ルでは共通のインタフェースを記述しており、必要に応じてモジュール間の
相違について指摘します。以下の議論では、 :mod:`pickle`  と
:mod:`cPickle` の総称として "pickle" という用語を使うことにします。

これら二つのモジュールが生成するデータストリームは相互交換できることが
保証されています。

Python には :mod:`marshal` と呼ばれるより原始的な直列化モジュールがあ
りますが、一般的に Python オブジェクトを直列化する方法としては
:mod:`pickle` を選ぶべきです。 :mod:`marshal` は基本的に :file:`.pyc`
ファイルをサポートするために存在しています。

:mod:`pickle` モジュールはいくつかの点で :mod:`marshal` と明確に異なります:

* :mod:`pickle` モジュールでは、同じオブジェクトが再度直列化されること
  のないよう、すでに直列化されたオブジェクトについて追跡情報を保持しま
  す。 :mod:`marshal` はこれを行いません。

  この機能は再帰的オブジェクトと共有オブジェクトの両方に重要な関わり
  をもっています。再帰的オブジェクトとは自分自身に対する参照を持ってい
  るオブジェクトです。再帰的オブジェクトは marshal で扱うことができず、
  実際、再帰的オブジェクトを marshal 化しようとすると Python インタプ
  リタをクラッシュさせてしまいます。共有オブジェクトは、直列化しよう
  とするオブジェクト階層の異なる複数の場所で同じオブジェクトに対する参
  照が存在する場合に生じます。共有オブジェクトを共有のままにしておく
  ことは、変更可能なオブジェクトの場合には非常に重要です。

* :mod:`marshal` はユーザ定義クラスやそのインスタンスを直列化するため
  に使うことができません。 :mod:`pickle` はクラスインスタンスを透過的に
  保存したり復元したりすることができますが、クラス定義をインポートす
  ることが可能で、かつオブジェクトが保存された際と同じモジュールで定義
  されていなければなりません。

* :mod:`marshal` の直列化フォーマットは Python の異なるバージョンで可
  搬性があることを保証していません。 :mod:`marshal` の本来の仕事は
  :file:`.pyc` ファイルのサポートなので、Python  を実装する人々には、
  必要に応じて直列化フォーマットを以前のバージョンと互換性のないものに
  変更する権限が残されています。 :mod:`pickle` 直列化フォーマットには、
  全ての Python リリース間で以前のバージョンとの互換性が保証されていま
  す。

直列化は永続化 (persisitence) よりも原始的な概念です; :mod:`pickle` は
ファイルオブジェクトを読み書きしますが、永続化されたオブジェクトの名前
付け問題や、(より複雑な) オブジェクトに対する競合アクセスの問題を扱い
ません。 :mod:`pickle` モジュールは複雑なオブジェクトをバイトストリーム
に変換することができ、バイトストリームを変換前と同じ内部構造をオブジェ
クトに変換することができます。このバイトストリームの最も明白な用途は
ファイルへの書き込みですが、その他にもネットワークを介して送信したり、
データベースに記録したりすることができます。モジュール :mod:`shelve`
はオブジェクトを DBM 形式のデータベースファイル上で pickle 化したり
unpickle 化したりするための単純なインタフェースを提供しています。


データストリームの形式
----------------------

.. index::
   single: XDR
   single: External Data Representation

:mod:`pickle` が使うデータ形式は Python 特有です。そうすることで、XDR
のような外部の標準が持つ制限 (例えば  XDR ではポインタの共有を表現で
きません) を課せられることがないという利点があります; しかしこれは
Python で書かれていないプログラムが pickle 化された Python オブジェク
トを再構築できない可能性があることを意味します。

標準では、 :mod:`pickle` データ形式では印字可能な ASCII 表現を使います。
これはバイナリ表現よりも少しかさばるデータになります。印字可能な ASCII
の利用 (とその他の :mod:`pickle` 表現形式が持つ特徴) の大きな利点は、
デバッグやリカバリを目的とした場合に、 pickle 化されたファイルを標準的
なテキストエディタで読めるということです。

現在、pickle化に使われるプロトコルは、以下の 3 種類です。

* バージョン 0 のプロトコルは、最初の ASCII プロトコルで、以前のバージョ
  ンのPython と後方互換です。

* バージョン 1 のプロトコルは、古いバイナリ形式で、以前のバージョンの
  Python と後方互換です。

* バージョン 2 のプロトコルは、Python 2.3 で導入されました。
  :term:`new-style class` を、より効率よく piclke 化します。

詳細は :pep:`307` を参照してください。

*protocol* を指定しない場合、プロトコル 0 が使われます。 *protocol* に
負値か :const:`HIGHEST_PROTOCOL` を指定すると、有効なプロトコルの内、
もっとも高いバージョンのものが使われます。

.. versionchanged:: 2.3
   *protocol* パラメータが導入されました。

*protocol* version >= 1 を指定することで、少しだけ効率の高いバイナリ
形式を選ぶことができます。


使用法
------

オブジェクト階層を直列化するには、まず pickler を生成し、続いてpickler
の :meth:`dump` メソッドを呼び出します。データストリームから非直列化
するには、まず unpickler を生成し、続いて unpicklerの :meth:`load` メ
ソッドを呼び出します。 :mod:`pickle` モジュールでは以下の定数を提供して
います:


.. data:: HIGHEST_PROTOCOL

   有効なプロトコルのうち、最も大きいバージョン。この値は、 *protocol*
   として渡せます。

   .. versionadded:: 2.3

.. note::

   protocols >= 1 で作られた pickle ファイルは、常にバイナリモードで
   オープンするようにしてください。古い ASCII ベースの pickle プロトコ
   ル 0 では、矛盾しない限りにおいてテキストモードとバイナリモードの
   いずれも利用することができます。

   プロトコル 0 で書かれたバイナリの pickle ファイルは、行ターミネータ
   として単独の改行(LF)を含んでいて、ですのでこの形式をサポートしない、
   Notepad や他のエディタで見たときに「おかしく」見えるかもしれません。

この pickle 化の手続きを便利にするために、 :mod:`pickle` モジュールでは
以下の関数を提供しています:


.. function:: dump(obj, file[, protocol])

   すでに開かれているファイルオブジェクト *file* に、 *obj* を pickle
   化したものを表現する文字列を書き込みます。
   ``Pickler(file, protocol).dump(obj)`` と同じです。

   *protocol* を指定しない場合、プロトコル 0 が使われます。 *protocol*
    に負値か :const:`HIGHEST_PROTOCOL` を指定すると、有効なプロトコル
    の内、もっとも高いバージョンのものが使われます。

   .. versionchanged:: 2.3
      *protocol* パラメータが導入されました。

   *file* は、単一の文字列引数を受理する :meth:`write` メソッドを持た
    なければなりません。従って、 *file* としては、書き込みのために開か
    れたファイルオブジェクト、 :mod:`StringIO` オブジェクト、その他前
    述のインタフェースに適合する他のカスタムオブジェクトをとることがで
    きます。


.. function:: load(file)

   すでに開かれているファイルオブジェクト *file* から文字列を読み出し、
   読み出された文字列を pickle 化されたデータ列として解釈して、もとの
   オブジェクト階層を再構築して返します。 ``Unpickler(file).load()`` と
   同じです。

   *file* は、整数引数をとる :meth:`read` メソッドと、引数の必要ない
   :meth:`readline` メソッドを持たなければなりません。これらのメソッ
   ドは両方とも文字列を返さなければなりません。従って、 *file* とし
   ては、読み出しのために開かれたファイルオブジェクト、
   :mod:`StringIO` オブジェクト、その他前述のインタフェースに適合す
   る他のカスタムオブジェクトをとることができます。

   この関数はデータ列の書き込まれているモードがバイナリかそうでないか
   を自動的に判断します。


.. function:: dumps(obj[, protocol])

   *obj* の pickle 化された表現を、ファイルに書き込む代わりに文字列で
   返します。

   *protocol* を指定しない場合、プロトコル 0 が使われます。 *protocol*
   に負値か :const:`HIGHEST_PROTOCOL` を指定すると、有効なプロトコル
   の内、もっとも高いバージョンのものが使われます。

   .. versionchanged:: 2.3
      *protocol* パラメータが追加されました。


.. function:: loads(string)

   pickle 化されたオブジェクト階層を文字列から読み出します。文字列中
   で pickle 化されたオブジェクト表現よりも後に続く文字列は無視されま
   す。

:mod:`pickle` モジュールでは、以下の 3 つの例外も定義しています:


.. exception:: PickleError

   下で定義されている他の例外で共通の基底クラスです。 :exc:`Exception`
   を継承しています。


.. exception:: PicklingError

   この例外は unpickle 不可能なオブジェクトが :meth:`dump` メソッドに
   渡された場合に送出されます。


.. exception:: UnpicklingError

   この例外は、オブジェクトを unpickle 化する際に問題が発生した場合に
   送出されます。 unpickle 化中には :exc:`AttributeError` 、
   :exc:`EOFError` 、 :exc:`ImportError` 、および :exc:`IndexError` と
   いった他の例外 (これだけとは限りません) も発生する可能性があるので
   注意してください。

:mod:`pickle` モジュールでは、2 つの呼び出し可能オブジェクト  [#]_ と
して、 :class:`Pickler` および :class:`Unpickler` を提供しています:


.. class:: Pickler(file[, protocol])

   pickle 化されたオブジェクトのデータ列を書き込むためのファイル類似の
   オブジェクトを引数にとります。

   *protocol* を指定しない場合、プロトコル 0 が使われます。 *protocol*
   に負値か :const:`HIGHEST_PROTOCOL` を指定すると、有効なプロトコル
   の内、もっとも高いバージョンのものが使われます。

   .. versionchanged:: 2.3
      *protocol* パラメータが導入されました。

   *file* は単一の文字列引数を受理する :meth:`write` メソッドを持たな
   ければなりません。従って、 *file* としては、書き込みのために開かれ
   たファイルオブジェクト、 :mod:`StringIO` オブジェクト、その他前述
   のインタフェースに適合する他のカスタムオブジェクトをとることができ
   ます。

   :class:`Pickler` オブジェクトでは、一つ (または二つ) の public なメソッドを定義しています:


   .. method:: dump(obj)

   コンストラクタで与えられた、すでに開かれているファイルオブジェクト
   に *obj* の pickle 化された表現を書き込みます。コンストラクタに渡さ
   れた *protocol* 引数の値に応じて、バイナリおよびASCII 形式が使われ
   ます。


   .. method:: clear_memo()

   picller の "メモ" を消去します。メモとは、共有オブジェクトまたは再
   帰的なオブジェクトが値ではなく参照で記憶されるようにするために、
   pickler がこれまでどのオブジェクトに遭遇してきたかを記憶するデータ
   構造です。このメソッドは pickler を再利用する際に便利です。

   .. note::

      Python 2.3 以前では、 :meth:`clear_memo` は :mod:`cPickle` で生
      成された pickler でのみ利用可能でした。 :mod:`pickle` モジュール
      では、pickler は :attr:`memo` と呼ばれる Python 辞書型のインスタ
      ンス変数を持ちます。従って、 :mod:`pickler` モジュールにおける
      pickler のメモを消去は、以下のようにしてできます::

         mypickler.memo.clear()

      以前のバージョンの Python での動作をサポートする必要のないコード
      では、単に :meth:`clear_memo` を使ってください。

同じ :class:`Pickler` のインスタンスに対し、 :meth:`dump` メソッドを
複数回呼び出すことは可能です。この呼び出しは、対応する
:class:`Unpickler` インスタンスで同じ回数だけ :meth:`load` を呼び出す
操作に対応します。同じオブジェクトが :meth:`dump` を複数回呼び出して
pickle 化された場合、 :meth:`load` は全て同じオブジェクトに対して参照
を行います  [#]_ 。


:class:`Unpickler` オブジェクトは以下のように定義されています:


.. class:: Unpickler(file)

   pickle データ列を読み出すためのファイル類似のオブジェクトを引数に
   取ります。このクラスはデータ列がバイナリモードかどうかを自動的に判
   別します。従って、 :class:`Pickler` のファクトリメソッドのような
   フラグを必要としません。

   *file* は、整数引数を取る :meth:`read` メソッド、および引数を持た
   ない :meth:`readline` メソッドの、 2 つのメソッドを持ちます。両方
   のメソッドとも文字列を返します。従って、 *file* としては、読み出
   しのために開かれたファイルオブジェクト、 :mod:`StringIO`  オブジェ
   クト、その他前述のインタフェースに適合する他のカスタムオブジェク
   トをとることができます。

   :class:`Unpickler` オブジェクトは 1 つ (または 2 つ) の public なメソッ
   ドを持っています:


   .. method:: load()

      コンストラクタで渡されたファイルオブジェクトからオブジェクトの
      pickle 化表現を読み出し、中に収められている再構築されたオブジェ
      クト階層を返します。

      このメソッドは自動的にデータストリームがバイナリモードで書き出さ
      れているかどうかを判別します。


   .. method:: noload()

      :meth:`load` に似ていますが、実際には何もオブジェクトを生成しな
      いという点が違います。この関数は第一に pickle 化データ列中で参照
      されている、"永続化 id" と呼ばれている値を検索する上で便利です。
      詳細は以下の  :ref:`pickle-protocol` を参照してください。

      **注意:** :meth:`noload` メソッドは現在 :mod:`cPickle` モジュー
      ルで生成された :class:`Unpickler` オブジェクトのみで利用可能
      です。 :mod:`pickle` モジュールの :class:`Unpickler`  には、
      :meth:`noload` メソッドがありません。


何を pickle 化したり unpickle 化できるのか?
-------------------------------------------

以下の型は pickle 化できます:

* ``None`` 、 ``True`` 、および ``False``

* 整数、長整数、浮動小数点数、複素数

* 通常文字列および Unicode 文字列

* pickle 化可能なオブジェクトからなるタプル、リスト、集合および辞書

* モジュールのトップレベルで定義されている関数

* モジュールのトップレベルで定義されている組込み関数

* モジュールのトップレベルで定義されているクラス

* :attr:`__dict__` または :meth:`__setstate__` を pickle 化できる上記
  クラスのインスタンス (詳細は :ref:`pickle-protocol` 節を参照してく
  ださい)

pickle 化できないオブジェクトを pickle 化しようとすると、
:exc:`PicklingError` 例外が送出されます; この例外が起きた場合、背後の
ファイルには未知の長さのバイト列が書き込まれてしまいます。極端に再帰
的なデータ構造を pickle 化しようとした場合には再帰の深さ制限を越えてし
まうかもしれず、この場合には :exc:`RuntimeError` が送出されます。この
制限は、 :func:`sys.setrecursionlimit` で慎重に上げていくことは可能で
す。

(組み込みおよびユーザ定義の) 関数は、値ではなく "完全記述された" 参照
名として pickle 化されるので注意してください。これは、関数の定義されて
いるモジュールの名前と一緒と併せ、関数名だけが pickle 化されることを
意味します。関数のコードや関数の属性は何も pickle化されません。従っ
て、定義しているモジュールは unpickle 化環境で import 可能でなければな
らず、そのモジュールには指定されたオブジェクトが含まれていなければな
りません。そうでない場合、例外が送出されます  [#]_ 。

クラスも同様に名前参照で pickle 化されるので、unpickle 化環境には同じ
制限が課せられます。クラス中のコードやデータは何も pickle 化されないの
で、以下の例ではクラス属性 ``attr`` が unpickle 化環境で復元されない
ことに注意してください ::

   class Foo:
       attr = 'a class attr'

   picklestring = pickle.dumps(Foo)

pickle 化可能な関数やクラスがモジュールのトップレベルで定義されていな
ければならないのはこれらの制限のためです。

同様に、クラスのインスタンスが pickle 化された際、そのクラスのコード
およびデータはオブジェクトと一緒に pickle 化されることはありません。イ
ンスタンスのデータのみが pickle 化されます。この仕様は、クラス内のバ
グを修正したりメソッドを追加した後でも、そのクラスの以前のバージョンで
作られたオブジェクトを読み出せるように意図的に行われています。あるク
ラスの多くのバージョンで使われるような長命なオブジェクトを作ろうと計画
しているなら、そのクラスの :meth:`__setstate__` メソッドによって適切
な変換が行われるようにオブジェクトのバージョン番号を入れておくとよいか
もしれません。


.. _pickle-protocol:

pickle 化プロトコル
-------------------

.. currentmodule:: None

この節では pickler/unpickler と直列化対象のオブジェクトとの間のインタ
フェースを定義する "pickle 化プロトコル"について記述します。このプロ
トコルは自分のオブジェクトがどのように直列化されたり非直列化されたり
するかを定義し、カスタマイズし、制御するための標準的な方法を提供します。
この節での記述は、unpickle 化環境を不信な pickle 化データに対して安全
にするために使う特殊なカスタマイズ化についてはカバーしていません; 詳細
は :ref:`pickle-sub` を参照してください。


.. _pickle-inst:

通常のクラスインスタンスの pickle 化および unpickle 化
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. method:: object.__getinitargs__()

   pickle 化されたクラスインスタンスが unpickle 化されたとき、
   :meth:`__init__` メソッドは通常呼び出され *ません* 。 unpickle 化の
   際に :meth:`__init__` が呼び出される方が望ましい場合、旧スタイルク
   ラスではメソッド :meth:`__getinitargs__` を定義することができます。
   このメソッドはクラスコンストラクタ (例えば :meth:`__init__`) に渡さ
   れるべき *タプルを* 返さなければなりません。
   :meth:`__getinitargs__` メソッドは pickle 時に呼び出されます;
   この関数が返すタプルはインスタンスの pickle 化データに組み込まれます。

.. method:: object.__getnewargs__()

   新スタイルクラスでは、プロトコル 2 で呼び出される
   :meth:`__getnewargs__` を定義する事ができます。インスタンス生成時に
   内部的な不変条件が成立する必要があったり、（タプルや文字列のように）
   型の :meth:`__new__` メソッドに指定する引数によってメモリの割り当てを
   変更する必要がある場合には :meth:`__getnewargs__` を定義してください。
   新スタイルクラス :class:`C` のインスタンスは、次のように生成されます。::

      obj = C.__new__(C, \*args)

   ここで *args* は元のオブジェクトの :meth:`__getnewargs__` メソッドを呼
   び出した時の戻り値となります。 :meth:`__getnewargs__` を定義していな
   い場合、 *args* は空のタプルとなります。

.. method:: object.__getstate__()

   クラスは、インスタンスの pickle 化方法にさらに影響を与えることがで
   きます; クラスが :meth:`__getstate__` メソッドを定義している場合、
   このメソッドが呼び出され、返された状態値はインスタンスの内容として、
   インスタンスの辞書の代わりに pickle 化されます。
   :meth:`__getstate__` メソッドが定義されていない場合、インスタンス
   の :attr:`__dict__` の内容が pickle 化されます。

.. method:: object.__setstate__(state)

   unpickle 化では、クラスが :meth:`__setstate__` も定義していた場合、
   unpickle 化された状態値とともに呼び出されます。 [#]_
   :meth:`__setstate__` メソッドが定義されていない場合、pickle 化され
   た状態は辞書型でなければならず、その要素は新たなインスタンスの辞書
   に代入されます。クラスが :meth:`__getstate__` と
   :meth:`__setstate__` の両方を定義している場合、状態値オブジェクトは
   辞書である必要はなく、これらのメソッドは期待通りの動作を行います。 [#]_

   .. note::

      新しいスタイルのクラスにおいて :meth:`__getstate__` が負値を返す
      場合、 :meth:`__setstate__` メソッドは呼ばれません。

.. note::

   unpickleするとき、 :meth:`__getattr__`, :meth:`__getattribute__`,
   :meth:`__setattr__` といったメソッドがインスタンスに対して呼ばれます。
   これらのメソッドが何か内部の不変条件に依存しているのであれば、
   その型は :meth:`__getinitargs__` か :meth:`__getnewargs__` のどちらかを
   実装してその不変条件を満たせるようにするべきです。
   それ以外の場合、 :meth:`__new__` も :meth:`__init__` も呼ばれません。

拡張型の pickle 化および unpickle 化
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. method:: object.__reduce__()

   :class:`Pickler` が全く未知の型の --- 拡張型のような --- オブジェク
   トに遭遇した場合、pickle 化方法のヒントとして 2 個所を探します。
   第一は :meth:`__reduce__` メソッドを実装しているかどうかです。もし
   実装されていれば、pickle 化時に :meth:`__reduce__` メソッドが引数
   なしで呼び出されます。メソッドはこの呼び出しに対して文字列またはタ
   プルのどちらかを返さねばなりません。

   文字列を返す場合、その文字列は通常通りに pickle 化されるグローバル
   変数の名前を指しています。 :meth:`__reduce__` の返す文字列は、モジュー
   ルにからみてオブジェクトのローカルな名前でなければなりません;
   pickle モジュールはモジュールの名前空間を検索して、オブジェクトの属
   するモジュールを決定します。

   タプルを返す場合、タプルの要素数は 2 から 5 でなければなりません。
   オプションの要素は省略したり ``None`` を指定したりできます。各要素
   の意味づけは以下の通りです:

   * オブジェクトの初期バージョンを生成するために呼び出される呼び出し
     可能オブジェクトです。この呼び出し可能オブジェクトへの引数
     はタプルの次の要素で与えられます。それ以降の要素では pickle 化さ
     れたデータを完全に再構築するために使われる付加的な状態情報が与え
     られます。

     逆 pickle 化の環境下では、このオブジェクトはクラスか、 "安全なコ
     ンストラクタ (safe constructor, 下記参照)" として登録
     されていたり属性 :attr:`__safe_for_unpickling__` の値が真であるよ
     うな呼び出し可能オブジェクトでなければなりません。
     そうでない場合、逆 pickle 化を行う環境で :exc:`UnpicklingError` が
     送出されます。通常通り、 callable は名前だけで
     pickle 化されるので注意してください。


   * 呼び出し可能なオブジェクトのための引数からなるタプル

     .. versionchanged:: 2.5
        以前は、この引数には ``None`` もあり得ました。

   * オプションとして、 :ref:`pickle-inst` 節で記述されているようにオ
     ブジェクトの :meth:`__setstate__` メソッドに渡される、オブジェクト
     の状態。オブジェクトが :meth:`__setstate__` メソッドを持たない場
     合、上記のように、この値は辞書でなくてはならず、オブジェクトの
     :attr:`__dict__` に追加されます。

   * オプションとして、リスト中の連続する要素を返すイテレータ (シーケ
     ンスではありません)。このリストの要素は pickle 化され、
     ``obj.append(item)`` または ``obj.extend(list_of_items)`` のいず
     れかを使って追加されます。主にリストのサブクラスで用いられていま
     すが、他のクラスでも、適切なシグネチャの :meth:`append` や
     :meth:`extend` を備えている限り利用できます。 (:meth:`append` と
     :meth:`extend` のいずれを使うかは、どのバージョンの pickle プロト
     コルを使っているか、そして追加する要素の数で決まります。従って両
     方のメソッドをサポートしていなければなりません。)

   * オプションとして、辞書中の連続する要素を返すイテレータ (シーケン
     スではありません)。このリストの要素は ``(key, value)`` という形式
     でなければなりません。要素は pickle 化され、 ``obj[key] = value``
     を使ってオブジェクトに格納されます。主に辞書のサブクラスで用いら
     れていますが、他のクラスでも、 :meth:`__setitem__` を備えている限
     り利用できます。

.. method:: object.__reduce_ex__(protocol)

   :meth:`__reduce__` を実装する場合、プロトコルのバージョンを知って
   おくと便利なことがあります。これは :meth:`__reduce__` の代わり
   に :meth:`__reduce_ex__` を使って実現できます。
   :meth:`__reduce_ex__` が定義されている場合、 :meth:`__reduce__` よ
   りも優先して呼び出されます (以前のバージョンとの互換性のために
   :meth:`__reduce__` を残しておいてもかまいません)。
   :meth:`__reduce_ex__` はプロトコルのバージョンを表す整数の引数を一
   つ伴って呼び出されます。

   :class:`object` クラスでは :meth:`__reduce__` と
   :meth:`__reduce_ex__` の両方を定義しています。とはいえ、サブクラス
   で :meth:`__reduce__` をオーバライドしており、
   :meth:`__reduce_ex__` をオーバライドしていない場合には、
   :meth:`__reduce_ex__` の実装がそれを検出して :meth:`__reduce__` を
   呼び出すようになっています。

pickle 化するオブジェクト上で :meth:`__reduce__` メソッドを実装する代
わりに、 :mod:`copy_reg` モジュールを使って呼び出し可能オブジェクトを登
録する方法もあります。このモジュールはプログラムに "縮小化関数
(reduction function)" とユーザ定義型のためのコンストラクタを登録する方
法を提供します。縮小化関数は、単一の引数として pickle 化するオブジェ
クトをとることを除き、上で述べた :meth:`__reduce__` メソッドと同じ意味
とインタフェースを持ちます。

登録されたコンストラクタは上で述べたような unpickle 化については "安全
なコンストラクタ" であると考えられます。


外部オブジェクトの pickle 化および unpickle 化
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index::
   single: persistent_id (pickle protocol)
   single: persistent_load (pickle protocol)


オブジェクトの永続化を便利にするために、 :mod:`pickle` は pickle 化され
たデータ列上にないオブジェクトに対して参照を行うという概念をサポートし
ています。これらのオブジェクトは "永続化 id (persistent id)" で参照さ
れており、この id は単に印字可能なASCII 文字からなる任意の文字列です。
これらの名前の解決方法は :mod:`pickle` モジュールでは定義されていませ
ん;

オブジェクトはこの名前解決を pickler および unpickler 上のユーザ定義関
数にゆだねます  [#]_ 。外部永続化 id の解決を定義するには、pickler オ
ブジェクトの :attr:`persistent_id` 属性と、 unpickler オブジェクトの
:attr:`persistent_load` 属性を設定する必要があります。

外部永続化 id を持つオブジェクトを pickle 化するには、pickler は自作の
:func:`persistent_id` メソッドを持たなければなりません。このメソッドは
一つの引数をとり、 ``None`` とオブジェクトの永続化 id のうちどちらかを
返さなければなりません。 ``None`` が返された場合、 pickler は単にオブジェ
クトを通常のように pickle 化するだけです。永続化 id 文字列が返された
場合、 piclkler はその文字列に対して、、unpickler がこの文字列を永続
化 id として認識できるように、マーカと共にpickle 化します。

外部オブジェクトを unpickle 化するには、unpickler は自作の
:func:`persistent_load` 関数を持たなければなりません。この関数は永続化
id 文字列を引数にとり、参照されているオブジェクトを返します。

*多分* より理解できるようになるようなちょっとした例を以下に示します::

   import pickle
   from cStringIO import StringIO

   src = StringIO()
   p = pickle.Pickler(src)

   def persistent_id(obj):
       if hasattr(obj, 'x'):
           return 'the value %d' % obj.x
       else:
           return None

   p.persistent_id = persistent_id

   class Integer:
       def __init__(self, x):
           self.x = x
       def __str__(self):
           return 'My name is integer %d' % self.x

   i = Integer(7)
   print i
   p.dump(i)

   datastream = src.getvalue()
   print repr(datastream)
   dst = StringIO(datastream)

   up = pickle.Unpickler(dst)

   class FancyInteger(Integer):
       def __str__(self):
           return 'I am the integer %d' % self.x

   def persistent_load(persid):
       if persid.startswith('the value '):
           value = int(persid.split()[2])
           return FancyInteger(value)
       else:
           raise pickle.UnpicklingError, 'Invalid persistent id'

   up.persistent_load = persistent_load

   j = up.load()
   print j

:mod:`cPickle` モジュール内では、 unpickler の :attr:`persistent_load`
属性は Pythonリスト型として設定することができます。この場合、
unpickler が永続化 id に遭遇しても、永続化 id 文字列は単にリストに追加
されるだけです。この仕様は、pickle データ中の全てのオブジェクトを実際
にインスタンス化しなくても、 pickle データ列中でオブジェクトに対する参
照を "嗅ぎ回る" ことができるようにするために存在しています  [#]_ 。リ
ストに :attr:`persistent_load` を設定するやり方は、よく Unpickler クラ
スの :meth:`noload` メソッドと共に使われます。

.. BAW: pickle 化と cPickle 化は、共に inst_persistent_id() をサポート
   します。
   それは永続化 id を2度目に生成するとき、未知の型を返します。
   Jim Fulton は、なぜ、それが追加され、なんのためのものか思い出せなかっ
   たため、ドキュメント化はません。

.. _pickle-sub:

Unpickler をサブクラス化する
----------------------------

.. index::
   single: load_global() (pickle protocol)
   single: find_global() (pickle protocol)

デフォルトでは、逆 pickle 化は pickle 化されたデータ中に見つかったク
ラスを import することになります。自前の unpickler をカスタマイズす
ることで、何が unpickle 化されて、どのメソッドが呼び出されるかを厳密
に制御することはできます。しかし不運なことに、厳密になにを行うべきか
は :mod:`pickle`  と :mod:`cPickle` のどちらを使うかで異なります  [#]_ 。


:mod:`pickle` モジュールでは、 :class:`Unpickler` からサブクラスを派生
し、 :meth:`load_global` メソッドを上書きする必要があります。
:meth:`load_global` は pickle データ列から最初の 2 行を読まなければな
らず、ここで最初の行はそのクラスを含むモジュールの名前、2 行目はその
インスタンスのクラス名になるはずです。次にこのメソッドは、例えばモジュー
ルをインポートして属性を掘り起こすなどしてクラスを探し、発見されたも
のを unpickler のスタックに置きます。その後、このクラスは空のクラスの
:attr:`__class__` 属性に代入する方法で、クラスの :meth:`__init__` を
使わずにインスタンスを魔法のように生成します。あなたの作業は (もしそ
の作業を受け入れるなら)、unpickler のスタックの上に push された
:meth:`load_global` を、unpickle しても安全だと考えられる何らかのク
ラスの既知の安全なバージョンにすることです。あるいは全てのインスタンス
に対して unpickling を許可したくないならエラーを送出してください。こ
のからくりがハックのように思えるなら、あなたは間違っていません。このか
らくりを動かすには、ソースコードを参照してください。

:mod:`cPickle` では事情は多少すっきりしていますが、十分というわけでは
ありません。何を unpickle 化するかを制御するには、 unpickler の
:attr:`find_global` 属性を関数か ``None`` に設定します。属性が
``None`` の場合、インスタンスを unpickle  しようとする試みは全て
:exc:`UnpicklingError` を送出します。属性が関数の場合、この関数はモジュー
ル名またはクラス名を受理し、対応するクラスオブジェクトを返さなくては
なりません。このクラスが行わなくてはならないのは、クラスの探索、必要な
import のやり直しです。そしてそのクラスのインスタンスが unpickle 化さ
れるのを防ぐためにエラーを送出することもできます。

以上の話から言えることは、アプリケーションが unpickle 化する文字列の
発信元については非常に高い注意をはらわなくてはならないということです。


.. _pickle-example:

例
--

いちばん単純には、 :func:`dump` と :func:`load` を使用してください。自
己参照リストが正しく pickle 化およびリストアされることに注目してくださ
い。 ::

   import pickle

   data1 = {'a': [1, 2.0, 3, 4+6j],
            'b': ('string', u'Unicode string'),
            'c': None}

   selfref_list = [1, 2, 3]
   selfref_list.append(selfref_list)

   output = open('data.pkl', 'wb')

   # Pickle dictionary using protocol 0.
   pickle.dump(data1, output)

   # Pickle the list using the highest protocol available.
   pickle.dump(selfref_list, output, -1)

   output.close()

以下の例は pickle 化された結果のデータを読み込みます。 pickle を含むデー
タを読み込む場合、ファイルはバイナリモードでオープンしなければいけませ
ん。これは ASCII 形式とバイナリ形式のどちらが使われているかは分からな
いからです。 ::

   import pprint, pickle

   pkl_file = open('data.pkl', 'rb')

   data1 = pickle.load(pkl_file)
   pprint.pprint(data1)

   data2 = pickle.load(pkl_file)
   pprint.pprint(data2)

   pkl_file.close()

より大きな例で、クラスを pickle 化する挙動を変更するやり方を示します。
:class:`TextReader` クラスはテキストファイルを開き、 :meth:`readline`
メソッドが呼ばれるたびに行番号と行の内容を返します。 :class:`TextReader`
インスタンスが pickle 化された場合、ファイルオブジェクト *以外の* 全
ての属性が保存されます。インスタンスが unpickle 化された際、ファイル
は再度開かれ、以前のファイル位置から読み出しを再開します。上記の動作を
実装するために、 :meth:`__setstat__` および :meth:`__getstate__`  メソッ
ドが使われています。 ::

   #!/usr/local/bin/python

   class TextReader:
       """Print and number lines in a text file."""
       def __init__(self, file):
           self.file = file
           self.fh = open(file)
           self.lineno = 0

       def readline(self):
           self.lineno = self.lineno + 1
           line = self.fh.readline()
           if not line:
               return None
           if line.endswith("\n"):
               line = line[:-1]
           return "%d: %s" % (self.lineno, line)

       def __getstate__(self):
           odict = self.__dict__.copy() # copy the dict since we change it
           del odict['fh']              # remove filehandle entry
           return odict

       def __setstate__(self, dict):
           fh = open(dict['file'])      # reopen file
           count = dict['lineno']       # read from file...
           while count:                 # until line count is restored
               fh.readline()
               count = count - 1
           self.__dict__.update(dict)   # update attributes
           self.fh = fh                 # save the file object

使用例は以下のようになるでしょう::

   >>> import TextReader
   >>> obj = TextReader.TextReader("TextReader.py")
   >>> obj.readline()
   '1: #!/usr/local/bin/python'
   >>> obj.readline()
   '2: '
   >>> obj.readline()
   '3: class TextReader:'
   >>> import pickle
   >>> pickle.dump(obj, open('save.p', 'wb'))

:mod:`pickle` が Python プロセス間でうまく働くことを見たいなら、先に
進む前に他の Python セッションを開始してください。以下の振る舞いは同じ
プロセスでも新たなプロセスでも起こります。 ::

   >>> import pickle
   >>> reader = pickle.load(open('save.p', 'rb'))
   >>> reader.readline()
   '4:     """Print and number lines in a text file."""'



.. seealso::

   Module :mod:`copy_reg`
      拡張型を登録するための Pickle インタフェース構成機構。

   Module :mod:`shelve`
      オブジェクトのインデクス付きデータベース; :mod:`pickle` を使います。

   Module :mod:`copy`
      オブジェクトの浅いコピーおよび深いコピー。

   Module :mod:`marshal`
      高いパフォーマンスを持つ組み込み型整列化機構。


:mod:`cPickle` --- より高速な :mod:`pickle`
===========================================

.. module:: cPickle
   :synopsis: pickle の高速バージョンですが、サブクラスはできません。
.. moduleauthor:: Jim Fulton <jfulton@zope.com>
.. sectionauthor:: Fred L. Drake, Jr. <fdrake@acm.org>


.. index:: module: pickle

:mod:`cPickle` モジュールは Python オブジェクトの直列化および非直列化
をサポートし、 :mod:`pickle` モジュールとほとんど同じインタフェースと機
能を提供します。いくつか相違点がありますが、最も重要な違いはパフォー
マンスとサブクラス化が可能かどうかです。

第一に、 :mod:`cPickle` は C で実装されているため、 :mod:`pickle`  より
も最大で 1000 倍高速です。第二に、 :mod:`cPickle` モジュール内では、呼
び出し可能オブジェクト :func:`Pickler` および :func:`Unpickler` は関数
で、クラスではありません。つまり、pickle 化や unpickle 化を行うカスタ
ムのサブクラスを派生することができないということです。多くのアプリケー
ションではこの機能は不要なので、 :mod:`cPickle` モジュールによる大きな
パフォーマンス向上の恩恵を受けられるはずです。 :mod:`pickle` と
:mod:`cPickle` で作られた pickle データ列は同じなので、既存の pickle
データに対して :mod:`pickle` と :mod:`cPickle` を互換に使用することが
できます。 [#]_

:mod:`cPickle` と :mod:`pickle` の API 間には他にも些細な相違がありま
すが、ほとんどのアプリケーションで互換性があります。より詳細なドキュメ
ンテーションは :mod:`pickle` のドキュメントにあり、そこでドキュメント
化されている相違点について挙げています。

.. rubric:: 注記

.. [#] :mod:`marshal` モジュールと間違えないように注意してください。

.. [#] :mod:`pickle` では、これらの呼び出し可能オブジェクトはクラスで
   あり、サブクラス化してその動作をカスタマイズすることができます。し
   かし、 :mod:`cPickle` モジュールでは、これらの呼び出し可能オブジェ
   クトはファクトリ関数であり、サブクラス化することができません。サブ
   クラスを作成する共通の理由の一つは、どのオブジェクトを実際に
   unpickle するかを制御することです。詳細については :ref:`pickle-sub`
   を参照してください。

.. [#] *警告*: これは、複数のオブジェクトを pickle 化する際に、オブジェ
   クトやそれらの一部に対する変更を妨げないようにするための仕様です。
   あるオブジェクトに変更を加えて、その後同じ :class:`Pickler` を使っ
   て再度 pickle 化しようとしても、そのオブジェクトは pickle 化しなお
   されません --- そのオブジェクトに対する参照が pickle 化さ
   れ、 :class:`Unpickler` は変更された値ではなく、元の値を返します。こ
   れには 2 つの問題点 : (1) 変更の検出、そして (2) 最小限の変更を整列
   化すること、があります。ガーベジコレクションもまた問題になります。

.. [#] 送出される例外は :exc:`ImportError` や :exc:`AttributeError` に
   なるはずですが、他の例外も起こりえます。

.. [#] これらのメソッドはクラスインスタンスのコピーを実装する際にも用
   いられます。

.. [#] このプロトコルはまた、 :mod:`copy` で定義されている浅いコピーや
   深いコピー操作でも用いられます。

.. [#] ユーザ定義関数に関連付けを行うための実際のメカニズムは、
   :mod:`pickle` および :mod:`cPickle` では少し異なります。
   :mod:`pickle` のユーザは、サブクラス化を行い、
   :meth:`persistend_id` および :meth:`persistent_load` メソッドを上書
   きすることで同じ効果を得ることができます。

.. [#] Guide と Jim が居間に座り込んでピクルス (pickles) を嗅いでいる
   光景を想像してください。

.. [#] 注意してください: ここで記述されている機構は内部の属性とメソッ
   ドを使っており、これらはPython の将来のバージョンで変更される対象
   になっています。われわれは将来、この挙動を制御するための、
   :mod:`pickle` および :mod:`cPickle` の両方で動作する、共通のインタ
   フェースを提供するつもりです。

.. [#] pickle データ形式は実際には小規模なスタック指向のプログラム言
   語であり、またあるオブジェクトをエンコードする際に多少の自由度があ
   るため、二つのモジュールが同じ入力オブジェクトに対して異なるデータ
   列を生成することもあります。しかし、常に互いに他のデータ列を読み出
   せることが保証されています。


