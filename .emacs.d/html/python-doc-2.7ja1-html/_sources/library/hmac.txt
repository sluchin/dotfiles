
:mod:`hmac` --- メッセージ認証のための鍵付きハッシュ化
======================================================

.. module:: hmac
   :synopsis: メッセージ認証のための鍵付きハッシュ化 (HMAC: Keyed-Hashing for Message
              Authentication) アルゴリズムの Python 用実装
.. moduleauthor:: Gerhard Häring <ghaering@users.sourceforge.net>
.. sectionauthor:: Gerhard Häring <ghaering@users.sourceforge.net>


.. versionadded:: 2.2

このモジュールでは :rfc:`2104` で記述されている HMAC アルゴリズムを実装して
います。


.. function:: new(key[, msg[, digestmod]])

   新たな hmac オブジェクトを返します。
   *msg* が存在すれば、メソッド呼び出し ``update(msg)`` を行います。
   *digestmod* は HMAC オブジェクトが使うダイジェストコンストラクタあるいは
   モジュールです。
   標準では :func:`hashlib.md5` コンストラクタになっています。


HMAC オブジェクトは以下のメソッドを持っています:


.. method:: hmac.update(msg)

   hmac オブジェクトを文字列 *msg* で更新します。
   このメソッドの呼出の繰り返しは、それらの引数を全て結合した引数で単一の呼び出しを
   した際と同じになります。
   すなわち ``m.update(a); m.update(b)``  は ``m.update(a + b)`` と等価です。


.. method:: hmac.digest()

   これまで :meth:`update` メソッドに渡された文字列のダイジェスト値を返します。
   これは :attr:`digest_size` バイトの文字列で、 NUL バイトを含む非 ASCII
   文字が含まれることがあります。


.. method:: hmac.hexdigest()

   :meth:`digest` と似ていますが、返される文字列は倍の長さとなり、16進形式となります。
   これは、電子メールなどの非バイナリ環境で値を交換する場合に便利です。


.. method:: hmac.copy()

   hmac オブジェクトのコピー ("クローン") を返します。このコピーは最初の部分文字列が
   共通になっている文字列のダイジェスト値を効率よく計算するために使うことができます。


.. seealso::

   Module :mod:`hashlib`
      セキュアハッシュ関数を提供する Python モジュールです。
