:mod:`MimeWriter` --- 汎用 MIME ファイルライター
================================================

.. module:: MimeWriter
   :synopsis: MIME 形式ファイルを書く
   :deprecated:


.. sectionauthor:: Christopher G. Petrilli <petrilli@amber.org>


.. deprecated:: 2.3
   :mod:`email` パッケージを、 :mod:`MimeWriter` モジュールよりも優先して使用すべきです。このモジュールは、
   下位互換性維持のためだけに存在します。

このモジュールは、クラス :class:`MimeWriter` を定義します。
この :class:`MimeWriter` クラスは、MIME
マルチパートファイルを作成するための基本的なフォーマッタを実装します。
これは出力ファイル内をあちこち移動することも、
大量のバッファスペースを使うこともありません。
あなたは、最終のファイルに現れるであろう順番に、パートを書かなければなりません。
:class:`MimeWriter` は、あなたが追加するヘッダをバッファして、
それらの順番を並び替えることができるようにします。


.. class:: MimeWriter(fp)

   :class:`MimeWriter` クラスの新しいインスタンスを返します。
   渡される唯一の引数 *fp* は、書くために使用するファイルオブジェクトです。
   :class:`StringIO` オブジェクトを使うこともできることに注意して下さい。


.. _mimewriter-objects:

MimeWriter オブジェクト
-----------------------

:class:`MimeWriter` インスタンスには以下のメソッドがあります：


.. method:: MimeWriter.addheader(key, value[, prefix])

   MIMEメッセージに新しいヘッダ行を追加します。
   *key* は、そのヘッダの名前であり、そして *value* で、そのヘッダの値を明示的に\
   与えます。省略可能な引数 *prefix* は、ヘッダが挿入される場所を決定します;
   ``0`` は最後に追加することを意味し、 ``1`` は先頭への挿入です。
   デフォールトは最後に追加することです。


.. method:: MimeWriter.flushheaders()

   今まで集められたヘッダすべてが書かれ(そして忘れられ)るようにします。これは、もし全く本体が必要でない場合に役に立ちます。例えば、
   ヘッダのような情報を保管するために(誤って)使用された、型 :mimetype:`message/rfc822` のサブパート用。


.. method:: MimeWriter.startbody(ctype[, plist[, prefix]])

   メッセージの本体に書くのに使用できるファイルのようなオブジェクトを返します。コンテント-型は、与えられた *ctype* に設定され、省略可能なパラメータ
   *plist* は、コンテント-型定義のための追加のパラメータを与えます。 *prefix* は、そのデフォールトが先頭への挿入以外は
   :meth:`addheader` でのように働きます。


.. method:: MimeWriter.startmultipartbody(subtype[, boundary[, plist[, prefix]]])

   メッセージ本体を書くのに使うことができるファイルのようなオブジェクトを返します。更に、このメソッドはマルチパートのコードを初期化します。ここで、
   *subtype* が、そのマルチパートのサブタイプを、 *boundary* がユーザ定義の境界仕様を、そして *plist*
   が、そのサブタイプ用の省略可能なパラメータを定義します。 *prefix* は、 :meth:`startbody` でのように働きます。サブパートは、
   :meth:`nextpart` を使って作成するべきです。


.. method:: MimeWriter.nextpart()

   マルチパートメッセージの個々のパートを表す、 :class:`MimeWriter` の新しいインスタンスを返します。これは、そのパートを書くのにも、
   また複雑なマルチパートを再帰的に作成するのにも使うことができます。メッセージは、 :meth:`nextpart` を使う前に, 最初
   :meth:`startmultipartbody` で初期化しなければなりません。


.. method:: MimeWriter.lastpart()

   これは、マルチパートメッセージの最後のパートを指定するのに使うことができ、マルチパートメッセージを書くときは *いつでも* 使うべきです。

