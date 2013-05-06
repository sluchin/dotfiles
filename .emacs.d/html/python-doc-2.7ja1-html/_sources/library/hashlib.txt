
:mod:`hashlib` --- セキュアハッシュおよびメッセージダイジェスト
===============================================================

.. module:: hashlib
   :synopsis: セキュアハッシュおよびメッセージダイジェストのアルゴリズム
.. moduleauthor:: Gregory P. Smith <greg@krypto.org>
.. sectionauthor:: Gregory P. Smith <greg@krypto.org>


.. versionadded:: 2.5

.. index::
   single: message digest, MD5
   single: secure hash algorithm, SHA1, SHA224, SHA256, SHA384, SHA512

このモジュールは、セキュアハッシュやメッセージダイジェスト用のさまざまなアルゴリズムを実装したものです。FIPSのセキュアなハッシュアルゴリズムである
SHA1、SHA224、SHA256、SHA384およびSHA512 (FIPS 180-2 で定義されているもの) だけでなくRSAのMD5アルゴリズム
(Internet :rfc:`1321` で定義されています)も実装しています。「セキュアなハッシュ」と「メッセージダイジェスト」はどちらも同じ意味です。
古くからあるアルゴリズムは「メッセージダイジェスト」と呼ばれていますが、最近は「セキュアハッシュ」という用語が用いられています。

.. note::
   adler32 や crc32 ハッシュ関数が使いたければ、
   :mod:`zlib` モジュールにあります。

.. warning::

   中には、ハッシュの衝突の脆弱性をかかえているアルゴリズムもあります。最後のFAQをごらんください。

:dfn:`hash` のそれぞれの型の名前をとったコンストラクタメソッドがひとつずつあります。
返されるハッシュオブジェクトは、どれも同じシンプルなインターフェイスを持っています。
たとえば :func:`sha1` を使用するとSHA1ハッシュオブジェクトが作成されます。
このオブジェクトの :meth:`update` メソッドに、任意の文字列を渡すことができます。
それまでに渡した文字列の :dfn:`digest` を知りたければ、
:meth:`digest` メソッドあるいは :meth:`hexdigest` メソッドを使用します。

.. index:: single: OpenSSL; (use in module hashlib)

このモジュールで常に使用できるハッシュアルゴリズムのコンストラクタは :func:`md5` 、 :func:`sha1` 、 :func:`sha224` 、
:func:`sha256` 、 :func:`sha384` および :func:`sha512` です。
それ以外のアルゴリズムが使用できるかどうかは、Python が使用している OpenSSL
ライブラリに依存します。

たとえば、 ``'Nobody inspects the spammish repetition'`` という文字列のダイジェストを取得するには次のようにします。
:

   >>> import hashlib
   >>> m = hashlib.md5()
   >>> m.update("Nobody inspects")
   >>> m.update(" the spammish repetition")
   >>> m.digest()
   '\xbbd\x9c\x83\xdd\x1e\xa5\xc9\xd9\xde\xc9\xa1\x8d\xf0\xff\xe9'
   >>> m.digest_size
   16
   >>> m.block_size
   64

もっと簡潔に書くと、このようになります。 :

   >>> hashlib.sha224("Nobody inspects the spammish repetition").hexdigest()
   'a4337bc45a8fc544c03f52dc550cd6e1e87021bc896588bd79e901e2'

汎用的なコンストラクタ :func:`new` も用意されています。このコンストラクタの最初のパラメータとして、使いたいアルゴリズムの名前を指定します。
アルゴリズム名として指定できるのは、先ほど説明したアルゴリズムかOpenSSLライブラリが提供するアルゴリズムとなります。
しかし、アルゴリズム名のコンストラクタのほうが :func:`new` よりずっと高速なので、そちらを使うことをお勧めします。

:func:`new` にOpenSSLのアルゴリズムを指定する例です。 :

   >>> h = hashlib.new('ripemd160')
   >>> h.update("Nobody inspects the spammish repetition")
   >>> h.hexdigest()
   'cc4a5ce1b3df48aec5d22d1f16b894a0b894eccc'

このモジュールは以下の定数属性を提供しています。

.. data:: hashlib.algorithms

   このモジュールによってサポートされていることが保証されるハッシュアルゴリズムの
   名前が入ったたぷる。

   .. versionadded:: 2.7

コンストラクタが返すハッシュオブジェクトには、次のような定数属性が用意されています。


.. data:: hash.digest_size

   生成されたハッシュのバイト数。

.. data:: hash.block_size

   内部で使われるハッシュアルゴリズムのブロックのバイト数。

ハッシュオブジェクトには次のようなメソッドがあります。


.. method:: hash.update(arg)

   ハッシュオブジェクトを文字列 *arg* で更新します。繰り返してコールするのは、すべての引数を連結して1回だけコールするのと同じ意味になります。つ
   まり、 ``m.update(a); m.update(b)`` と ``m.update(a+b)`` は同じ意味だということです。

   .. versionchanged:: 2.7

      2048 バイト以上の大きいデータに対して OpenSSL が提供しているハッシュ
      アルゴリズムを使うときは、ハッシュ値の更新中に他のスレッドが動作できる
      ように Python の GIL を開放します。


.. method:: hash.digest()

   これまでに :meth:`update` メソッドに渡した文字列のダイジェストを返しま
   す。これは :attr:`digest_size` バイトの文字列であり、非ASCII文字やnull バイトを含むこともあります。


.. method:: hash.hexdigest()

   :meth:`digest` と似ていますが、返される文字列は倍の長さとなり、16進形式となります。これは、電子メールなどの非バイナリ環境で値を交換する場合に
   便利です。


.. method:: hash.copy()

   ハッシュオブジェクトのコピー ("クローン") を返します。これは、共通部分を持つ複数の文字列のダイジェストを効率的に計算するために使用します。


.. seealso::

   Module :mod:`hmac`
      ハッシュを用いてメッセージ認証コードを生成するモジュールです。

   Module :mod:`base64`
      バイナリハッシュを非バイナリ環境用にエンコードするもうひとつの方法です。

   http://csrc.nist.gov/publications/fips/fips180-2/fips180-2.pdf
      FIPS 180-2 のセキュアハッシュアルゴリズムについての説明。

   http://www.cryptography.com/cnews/hash.html
      Hash Collision FAQ。既知の問題を持つアルゴリズムとその使用上の注意点に関する情報があります。

   http://en.wikipedia.org/wiki/Cryptographic_hash_function#Cryptographic_hash_algorithms
      どのアルゴリズムにどんな既知の問題があって、それが実際に利用する際にどう
      影響するかについての Wikipedia の記事
