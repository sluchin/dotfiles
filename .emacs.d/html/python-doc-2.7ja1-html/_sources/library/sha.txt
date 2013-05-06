
:mod:`sha` --- SHA-1 メッセージダイジェストアルゴリズム
=======================================================

.. module:: sha
   :synopsis: NISTのセキュアハッシュアルゴリズム、SHA。
   :deprecated:
.. sectionauthor:: Fred L. Drake, Jr. <fdrake@acm.org>


.. deprecated:: 2.5
   かわりにモジュール :mod:`hashlib` を使ってください。

.. index::
   single: Secure Hash Algorithm
   single: NIST
   single: checksum; SHA

このモジュールは、SHA-1 として知られている、 NIST のセキュアハッシュアルゴリズムへのインターフェースを実装しています。
SHA-1 はオリジナルの SHA ハッシュアルゴリズムを改善したバージョンです。
:mod:`md5` モジュールと同じように使用します。
sha オブジェクトを生成するために :func:`new` を使い、
:meth:`update` メソッドを使って、このオブジェクトに任意の文字列を入力し、
それまでに入力した文字列全体の :dfn:`digest` をいつでも調べることができます。
SHA-1 のダイジェストは MD5 の 128 bit とは異なり、160 bit です。


.. function:: new([string])

   新たな sha オブジェクトを返します。もし *string* が存在するなら、
   ``update(string)`` を呼び出します。

以下の値はモジュールの中で定数として与えられており、
:func:`new` で返される sha オブジェクトの属性としても与えられます:


.. data:: blocksize

   ハッシュ関数に入力されるブロックのサイズ。このサイズは常に ``1`` です。
   このサイズは、任意の文字列をハッシュできるようにするために使われます。


.. data:: digest_size

   返されるダイジェスト値をバイト数で表した長さ。常に 20 です。

sha オブジェクトには md5 オブジェクトと同じメソッドがあります。


.. method:: sha.update(arg)

   文字列 *arg* を入力として sha オブジェクトを更新します。このメソッドを繰り返し呼び出す(操作は、それぞれの呼び出し時の引数を結合した
   データを引数として一回の呼び出す操作と同等になります。つまり、 ``m.update(a); m.update(b)`` は ``m.update(a+b)``
   と同等です。


.. method:: sha.digest()

   これまで update() メソッドで与えてきた文字列のダイジェストを返します。戻り値は 20 バイトの文字列で、nullバイトを含む非 ASCII
   文字が入っているかもしれません。


.. method:: sha.hexdigest()

   :meth:`digits` と似ていますが、ダイジェストは長さ40の文字列になり、16進表記数字しか含みません。
   電子メールやその他のバイナリを受け付けない環境で安全に値をやりとりするために使うことができます。


.. method:: sha.copy()

   sha オブジェクトのコピー("クローン")を返します。冒頭の部分文字列が共通な複数の文字列のダイジェストを効率よく計算する際に使うことができます。


.. seealso::

   `セキュアハッシュスタンダード <http://csrc.nist.gov/publications/fips/fips180-2/fips180-2withchangenotice.pdf>`_
      セキュアハッシュアルゴリズムは NIST のドキュメント FIPS PUB 180-2 で定義されています。 `セキュアハッシュスタンダード
      <http://csrc.nist.gov/publications/fips/fips180-2/fips180-2withchangenotice.pdf>`_,
      2002年8月出版。

   `暗号ツールキット (セキュアハッシュ) <http://csrc.nist.gov/CryptoToolkit/tkhash.html>`_
      NISTからはられているセキュアハッシュに関するさまざまな情報へのリンク

