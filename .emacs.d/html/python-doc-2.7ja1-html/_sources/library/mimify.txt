
:mod:`mimify` --- 電子メールメッセージの MIME 処理
==================================================

.. module:: mimify
   :synopsis: 電子メールメッセージの MIME 化および非 MIME 化。
   :deprecated:

.. deprecated:: 2.3
   :mod:`mimify` モジュールを使うよりも :mod:`email` パッケージを使うべきです。
   このモジュールは以前のバージョンとの互換性のために保守されているにすぎません。

:mod:`mimify` モジュールでは電子メールメッセージから MIME へ、および
MIME から電子メールメッセージへの変換を行うための二つの関数を定義しています。
電子メールメッセージは単なるメッセージでも、 MIME 形式でもかまいません。
各パートは個別に扱われます。
メッセージ (の一部) の MIME 化 (mimify) の際、7 ビット ASCII
文字を使って表現できない何らかの文字が含まれていた場合、メッセージの
quoted-printable への符号化が伴います。
メッセージが送信される前に編集しなければならない場合、
MIME 化および非 MIME 化は特に便利です。
典型的な使用法は以下のようになります::

   unmimify message
   edit message
   mimify message
   send message

モジュールでは以下のユーザから呼び出し可能な関数と、ユーザが設定可能な変数を定義しています:


.. function:: mimify(infile, outfile)

   *infile* を *outfile* にコピーします。その際、パートを quoted-printable に変換し、必要なら MIME
   メールヘッダを追加します。 *infile* および *outfile* はファイルオブジェクト (実際には、 :meth:`readline` メソッドを持つ
   (*infile*) か、 :meth:`write` (*outfile*) メソッドを持つあらゆるオブジェクト)
   か、ファイル名を指す文字列を指定することができます。 *infile* および *outfile* が両方とも文字列の場合、同じ値にすることができます。


.. function:: unmimify(infile, outfile[, decode_base64])

   *infile* を *outfile* にコピーします。その際、全ての quoted-printable 化されたパートを復号化します。 *infile*
   および *outfile* はファイルオブジェクト (実際には、 :meth:`readline` メソッドを持つ (*infile*) か、
   :meth:`write` (*outfile*) メソッドを持つあらゆるオブジェクト)  か、ファイル名を指す文字列を指定することができます。
   *decode_base64* 引数が与えられており、その値が真である場合、 base64 符号で符号化されているパートも同様に復号化されます。


.. function:: mime_decode_header(line)

   *line* 内の符号化されたヘッダ行が復号化されたものを返します。ISO 8859-1 文字セット (Latin-1) だけをサポートします。


.. function:: mime_encode_header(line)

   *line* 内のヘッダ行が MIME 符号化されたものを返します。


.. data:: MAXLEN

   標準では、非 ASCII 文字 (8 ビット目がセットされている文字) を含むか、 :const:`MAXLEN` 文字 (標準の値は 200 です)
   よりも長い部分は quoted-printable 形式で符号化されます。


.. data:: CHARSET

   文字セットがメールヘッダで指定されていない場合指定しなければなりません。使われている文字セットを表す文字列は :const:`CHARSET`
   に記憶されます。標準の値は ISO-8859-1 (Latin1 (latin-one) としても知られています)。

このモジュールはコマンドラインから利用することもできます。以下のような使用法::

   mimify.py -e [-l length] [infile [outfile]]
   mimify.py -d [-b] [infile [outfile]]

で、それぞれ符号化 (mimify) および復号化 (unmimify) を行います。
標準の設定では *infile* は標準入力で、 *putfile* は標準出力です。
入出力に同じファイルを指定することもできます。

符号化の際に **-l** オプションを与えた場合、
*length* で指定した長さより長い行があれば、その長さに含まれる部分が符号化されます。

復号化の際に **-b** オプションが与えられていれば、base64 パートも同様に復号化されます。


.. seealso::

   Module :mod:`quopri`
      MIME quoted-printable 形式ファイルのエンコードおよびデコード。

