
:mod:`xdrlib` --- XDR データのエンコードおよびデコード
======================================================

.. module:: xdrlib
   :synopsis: 外部データ表現 (XDR, External Data Representation)  データのエンコードおよびデコード。


.. index::
   single: XDR
   single: External Data Representation

:mod:`xdrlib` モジュールは外部データ表現標準 (External Data  Representation Standard)
のサポートを実現します。この標準は 1987 年に Sun Microsystems, Inc. によって書かれ、 :rfc:`1014`
で定義されています。このモジュールでは RFC で記述されているほとんどのデータ型をサポートしています。

:mod:`xdrlib` モジュールでは 2 つのクラスが定義されています。一つは変数を XDR 表現にパックするためのクラスで、もう一方は XDR
表現からアンパックするためのものです。2 つの例外クラスが同様にして定義されています。


.. class:: Packer()

   :class:`Packer` はデータを XDR 表現にパックするためのクラスです。 :class:`Packer`
   クラスのインスタンス生成は引数なしで行われます。


.. class:: Unpacker(data)

   ``Unpacker`` は Packer と対をなしていて、文字列バッファから XDR をアンパックするためのクラスです。入力バッファ *data*
   を引数に与えてインスタンスを生成します。


.. seealso::

   :rfc:`1014` - XDR: External Data Representation Standard
      この RFC が、かつてこのモジュールが最初に書かれた当時に XDR 標準であったデータのエンコード方法を定義していました。現在は :rfc:`1832`
      に更新されているようです。

   :rfc:`1832` - XDR: External Data Representation Standard
      こちらが新しい方のRFCで、XDR の改訂版が定義されています。


.. _xdr-packer-objects:

Packer オブジェクト
-------------------

:class:`Packer` インスタンスには以下のメソッドがあります:


.. method:: Packer.get_buffer()

   現在のパック処理用バッファを文字列で返します。


.. method:: Packer.reset()

   パック処理用バッファをリセットして、空文字にします。

一般的には、適切な ``pack_type()`` メソッドを使えば、一般に用いられているほとんどの XDR データをパックすることができます。
各々のメソッドは一つの引数をとり、パックしたい値を与えます。単純なデータ型をパックするメソッドとして、以下のメソッド: :meth:`pack_uint`
、 :meth:`pack_int` 、 :meth:`pack_enum` 、 :meth:`pack_bool` 、 :meth:`pack_uhyper`
そして :meth:`pack_hyper` がサポートされています。


.. method:: Packer.pack_float(value)

   単精度 (single-precision) の浮動小数点数 *value* をパックします。


.. method:: Packer.pack_double(value)

   倍精度 (double-precision) の浮動小数点数 *value* をパックします。

以下のメソッドは文字列、バイト列、不透明データ (opaque data) のパック処理をサポートします:


.. method:: Packer.pack_fstring(n, s)

   固定長の文字列、 *s* をパックします。 *n* は文字列の長さですが、この値自体はデータバッファにはパック *されません* 。 4
   バイトのアラインメントを保証するために、文字列は必要に応じて null  バイト列でパディングされます。


.. method:: Packer.pack_fopaque(n, data)

   :meth:`pack_fstring` と同じく、固定長の不透明データストリームをパックします。


.. method:: Packer.pack_string(s)

   可変長の文字列 *s* をパックします。文字列の長さが最初に符号なし整数でパックされ、続いて :meth:`pack_fstring` を使って文字列データが
   パックされます。


.. method:: Packer.pack_opaque(data)

   :meth:`pack_string` と同じく、可変長の不透明データ文字列をパックします。


.. method:: Packer.pack_bytes(bytes)

   :meth:`pack_string` と同じく、可変長のバイトストリームをパックします。

以下のメソッドはアレイやリストのパック処理をサポートします:


.. method:: Packer.pack_list(list, pack_item)

   一様な項目からなる *list* をパックします。このメソッドはサイズ不定、すなわち、全てのリスト内容を網羅するまでサイズが
   分からないリストに対して有用です。リストのすべての項目に対し、最初に符号無し整数 ``1`` がパックされ、続いてリスト中の
   データがパックされます。 *pack_item* は個々の項目をパックするために呼び出される関数です。リストの末端に到達すると、符号無し整数 ``0``
   がパックされます。

   例えば、整数のリストをパックするには、コードは以下のようになるはずです::

      import xdrlib
      p = xdrlib.Packer()
      p.pack_list([1, 2, 3], p.pack_int)


.. method:: Packer.pack_farray(n, array, pack_item)

   一様な項目からなる固定長のリスト (*array*) をパックします。 *n* はリストの長さです。この値はデータバッファにパック *されません*
   が、 ``len(array)`` が *n* と等しくない場合、例外 :exc:`ValueError` が送出されます。上と同様に、 *pack_item*
   は個々の要素をパック処理するための関数です。


.. method:: Packer.pack_array(list, pack_item)

   一様の項目からなる可変長の *list* をパックします。まず、リストの長さが符号無し整数でパックされ、つづいて各要素が上の
   :meth:`pack_farray` と同じやり方でパックされます。


.. _xdr-unpacker-objects:

Unpacker オブジェクト
---------------------

:class:`Unpacker` クラスは以下のメソッドを提供します:


.. method:: Unpacker.reset(data)

   文字列バッファを *data* でリセットします。


.. method:: Unpacker.get_position()

   データバッファ中の現在のアンパック処理位置を返します。


.. method:: Unpacker.set_position(position)

   データバッファ中のアンパック処理位置を *position* に設定します。 :meth:`get_position` および
   :meth:`set_position` は注意して使わなければなりません。


.. method:: Unpacker.get_buffer()

   現在のアンパック処理用データバッファを文字列で返します。


.. method:: Unpacker.done()

   アンパック処理を終了させます。全てのデータがまだアンパックされていなければ、例外 :exc:`Error` が送出されます。

上のメソッドに加えて、 :class:`Packer` でパック処理できるデータ型はいずれも :class:`Unpacker`
でアンパック処理できます。アンパック処理メソッドは ``unpack_type()`` の形式をとり、引数をとりません。
これらのメソッドはアンパックされたデータオブジェクトを返します。


.. method:: Unpacker.unpack_float()

   単精度の浮動小数点数をアンパックします。


.. method:: Unpacker.unpack_double()

   :meth:`unpack_float` と同様に、倍精度の浮動小数点数をアンパックします。

上のメソッドに加えて、文字列、バイト列、不透明データをアンパックする以下のメソッドが提供されています:


.. method:: Unpacker.unpack_fstring(n)

   固定長の文字列をアンパックして返します。 *n* は予想される文字列の長さです。4 バイトのアラインメントを保証するために null バイトによる
   パディングが行われているものと仮定して処理を行います。


.. method:: Unpacker.unpack_fopaque(n)

   :meth:`unpack_fstring` と同様に、固定長の不透明データストリームをアンパックして返します。


.. method:: Unpacker.unpack_string()

   可変長の文字列をアンパックして返します。最初に文字列の長さが符号無し整数としてアンパックされ、次に :meth:`unpack_fstring` を使って
   文字列データがアンパックされます。


.. method:: Unpacker.unpack_opaque()

   :meth:`unpack_string` と同様に、可変長の不透明データ文字列をアンパックして返します。


.. method:: Unpacker.unpack_bytes()

   :meth:`unpack_string` と同様に、可変長のバイトストリームをアンパックして返します。

以下メソッドはアレイおよびリストのアンパック処理をサポートします。


.. method:: Unpacker.unpack_list(unpack_item)

   一様な項目からなるリストをアンパック処理してかえします。リストは一度に 1 要素づつアンパック処理されます、まず符号無し整数によるフラグ
   がアンパックされます。もしフラグが ``1`` なら、要素はアンパックされ、返り値のリストに追加されます。フラグが ``0`` であれば、リストの終端
   を示します。 *unpack_item* は個々の項目をアンパック処理するために呼び出される関数です。


.. method:: Unpacker.unpack_farray(n, unpack_item)

   一様な項目からなる固定長のアレイをアンパックして（リストとして）返します。 *n* はバッファ内に存在すると期待されるリストの要素数です。上と同様に、
   *unpack_item* は各要素をアンパックするために使われる関数です。


.. method:: Unpacker.unpack_array(unpack_item)

   一様な項目からなる可変長の *list* をアンパックして返します。まず、リストの長さが符号無し整数としてアンパックされ、続いて各要素が上の
   :meth:`unpack_farray` のようにしてアンパック処理されます。


.. _xdr-exceptions:

例外
----

このモジュールでの例外はクラスインスタンスとしてコードされています:


.. exception:: Error

   ベースとなる例外クラスです。 :exc:`Error` public なデータメンバとして :attr:`msg` を持ち、エラーの詳細が収められています。


.. exception:: ConversionError

   :exc:`Error` から派生したクラスです。インスタンス変数は塚されていません。

これらの例外を補足する方法を以下の例に示します::

   import xdrlib
   p = xdrlib.Packer()
   try:
       p.pack_double(8.01)
   except xdrlib.ConversionError, instance:
       print 'packing the double failed:', instance.msg

