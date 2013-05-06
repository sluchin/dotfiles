
:mod:`mimetools` --- MIME メッセージを解析するためのツール
==========================================================

.. module:: mimetools
   :synopsis: MIME-スタイルのメッセージ本体を解析するためのツール。
   :deprecated:


.. deprecated:: 2.3
   :mod:`email` パッケージを :mod:`mimetools` モジュールより\
   優先して使うべきです。このモジュールは、下位互換性維持のためにのみ\
   存在しています。Python 3.x では削除されています。

.. index:: module: rfc822

このモジュールは、 :mod:`rfc822` モジュールの :class:`Message`
クラスのサブクラスと、マルチパート MIME や符合化メッセージの操作に\
役に立つ多くのユーティリティ関数を定義しています。

これには以下の項目が定義されています：


.. class:: Message(fp[, seekable])

   :class:`Message` クラスの新しいインスタンスを返します。
   これは、 :class:`rfc822.Message` クラスのサブクラスで、\
   いくつかの追加のメソッドがあります(以下を参照のこと)。 *seekable* 引数は、
   :class:`rfc822.Message` のものと同じ意味を持ちます。


.. function:: choose_boundary()

   パートの境界として使うことができる見込みが高いユニークな文字列を返します。
   その文字列は、 ``'hostipaddr.uid.pid.timestamp.random'`` の形をしています。


.. function:: decode(input, output, encoding)

   オープンしたファイルオブジェクト *input* から、許される MIME *encoding*
   を使って符号化されたデータを読んで、オープンされたファイルオブジェクト
   *output* に復号化されたデータを書きます。 *encoding*
   に許される値は、 ``'base64'``, ``'quoted-printable'``, ``'uuencode'``, ``'x-uuencode'``,
   ``'uue'``, ``'x-uue'``, ``'7bit'``, および ``'8bit'`` です。
   ``'7bit'`` あるいは ``'8bit'``
   で符号化されたメッセージを復号化しても何も効果がありません。
   入力が出力に単純にコピーされるだけです。


.. function:: encode(input, output, encoding)

   オープンしたファイルオブジェクト  *input* からデータを読んで、
   それを許される MIME *encoding* を使って符号化して、オープンした\
   ファイルオブジェクト *output* に書きます。
   *encoding* に許される値は、 :meth:`decode` のものと同じです。


.. function:: copyliteral(input, output)

   オープンしたファイル *input* から行を EOF まで読んで、\
   それらをオープンしたファイル *output* に書きます。


.. function:: copybinary(input, output)

   オープンしたファイル *input* からブロックを EOF まで読んで、\
   それらをオープンしたファイル *output* に書きます。
   ブロックの大きさは現在 8192 に固定されています。


.. seealso::

   Module :mod:`email`
      圧縮電子メール操作パッケージ； :mod:`mimetools` モジュールに委譲。

   Module :mod:`rfc822`
      :class:`mimetools.Message` のベースクラスを提供する。

   Module :mod:`multifile`
      MIME データのような、別個のパーツを含むファイルの読み込みをサポート。

   http://faqs.cs.uu.nl/na-dir/mail/mime-faq/.html
      MIME でよく訊ねられる質問。MIMEの概要に関しては、\
      この文書の Part 1 の質問 1.1 への答えを見ること。


.. _mimetools-message-objects:

Message オブジェクトの追加メソッド
----------------------------------

:class:`Message` クラスは、 :class:`rfc822.Message` メソッドに加えて、\
以下のメソッドを定義しています：


.. method:: Message.getplist()

   :mailheader:`Content-Type` ヘッダのパラメータリストを返します。
   これは文字列のリストです。
   ``key=value`` の形のパラメータに対しては、\ *key* は小文字に変換されますが、\
   *value* は変換されません。
   たとえば、もしメッセージに、ヘッダ
   ``Content-type: text/html; spam=1; Spam=2; Spam`` が含まれていれば、
   :meth:`getplist` は、Python リスト  ``['spam=1', 'spam=2', 'Spam']``
   を返すでしょう。


.. method:: Message.getparam(name)

   与えられた *name* の( ``name=value`` の形に対して :meth:`getplist` が返す)
   第1パラメータの *value* を返します。
   もし *value* が、'``<``...\ ``>``' あるいは '``"``...\ ``"``'
   のように引用符で囲まれていれば、これらは除去されます。


.. method:: Message.getencoding()

   :mailheader:`Content-Transfer-Encoding` メッセージヘッダで指定された\
   符号化方式を返します。もしそのようなヘッダが存在しなければ、\
   ``'7bit'`` を返します。符号化方式文字列は小文字に変換されます。


.. method:: Message.gettype()

   :mailheader:`Content-Type` ヘッダで指定された (``type/subtype`` の形での)
   メッセージ型を返します。
   もしそのようなヘッダが存在しなければ、 ``'text/plain'`` を返します。
   型文字列は小文字に変換されます。


.. method:: Message.getmaintype()

   :mailheader:`Content-Type` ヘッダで指定された主要型を返します。
   もしそのようなヘッダが存在しなければ、
   ``'text'`` を返します。
   主要型文字列は小文字に変換されます。


.. method:: Message.getsubtype()

   :mailheader:`Content-Type` ヘッダで指定された下位型を返します。
   もしそのようなヘッダが存在しなければ、 ``'plain'`` を返します。
   下位型文字列は小文字に変換されます。

