
:mod:`md5` --- MD5 メッセージダイジェストアルゴリズム
=====================================================

.. module:: md5
   :synopsis: RSA の MD5 メッセージダイジェストアルゴリズム
   :deprecated:


.. deprecated:: 2.5
   代わりにモジュール :mod:`hashlib` を使ってください。

.. index::
   single: message digest, MD5
   single: checksum; MD5

このモジュールは RSA 社の MD5 メッセージダイジェスト  アルゴリズムへのインタフェースを実装しています。 (Internet :rfc:`1321`
も参照してください)。利用方法は極めて単純です。まず md5 オブジェクトを :func:`new` を使って生成します。後は :meth:`update`
メソッドを使って、生成されたオブジェクトに任意の文字列データを入力します。オブジェクトに入力された文字列データ全体の :dfn:`digest`
("fingerprint" として知られる強力な  128-bit チェックサム) は :meth:`digest` を使っていつでも調べることができます。

例えば、文字列 ``'Nobody inspects the spammish repetition'``  のダイジェストを得るためには以下のようにします:

   >>> import md5
   >>> m = md5.new()
   >>> m.update("Nobody inspects")
   >>> m.update(" the spammish repetition")
   >>> m.digest()
   '\xbbd\x9c\x83\xdd\x1e\xa5\xc9\xd9\xde\xc9\xa1\x8d\xf0\xff\xe9'

もっと詰めて書くと以下のようになります:

   >>> md5.new("Nobody inspects the spammish repetition").digest()
   '\xbbd\x9c\x83\xdd\x1e\xa5\xc9\xd9\xde\xc9\xa1\x8d\xf0\xff\xe9'

以下の値はモジュールの中で定数として与えられており、 :func:`new` で返される md5 オブジェクトの属性としても与えられます:


.. data:: digest_size

   返されるダイジェスト値のバイト数で表した長さ。常に ``16`` です。

md5 クラスオブジェクトは以下のメソッドをサポートします:


.. function:: new([arg])

   新たな md5 オブジェクトを返します。もし *arg* が存在するなら、 ``update(arg)`` を呼び出します。


.. function:: md5([arg])

   下位互換性のために、 :func:`new` の別名として提供されています。

md5 オブジェクトは以下のメソッドをサポートします:


.. method:: md5.update(arg)

   文字列 *arg* を入力として md5 オブジェクトを更新します。このメソッドを繰り返して呼び出す操作は、それぞれの呼び出し時の引数 *arg*
   を結合したデータを引数として一回の呼び出す操作と同等になります: つまり、 ``m.update(a); m.update(b)`` は
   ``m.update(a+b)`` と同等です。


.. method:: md5.digest()

   これまで :meth:`update` で与えてきた文字列入力のダイジェストを返します。返り値は 16 バイトの文字列で、null バイトを含む非 ASCII
   文字が入っているかもしれません。


.. method:: md5.hexdigest()

   :meth:`digest` に似ていますが、ダイジェストは長さ 32 の文字列になり、16 進表記文字しか含みません。この文字列は電子メールやその
   他のバイナリを受け付けない環境でダイジェストを安全にやりとりするために使うことができます。


.. method:: md5.copy()

   md5 オブジェクトのコピー ("クローン") を返します。冒頭の部分文字列が共通な複数の文字列のダイジェストを効率よく計算する際に使うことができます。


.. seealso::

   Module :mod:`sha`
      Secure Hash Algorithm (SHA) を実装した類似のモジュール。 SHA アルゴリズムはより安全なハッシュアルゴリズムだと考えられています。

