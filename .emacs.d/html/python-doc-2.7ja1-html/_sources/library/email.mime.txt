:mod:`email`: 電子メールおよび MIME オブジェクトをゼロから作成する
------------------------------------------------------------------

.. module:: email.mime
   :synopsis: MIME メッセージを作成する


ふつう、メッセージオブジェクト構造はファイルまたは何がしかの\
テキストをパーザに通すことで得られます。パーザは与えられた\
テキストを解析し、基底となる root のメッセージオブジェクトを返します。\
しかし、完全なメッセージオブジェクト構造を何もないところから作成することも\
また可能です。個別の :class:`~email.message.Message` を手で作成することさえできます。\
実際には、すでに存在するメッセージオブジェクト構造をとってきて、\
そこに新たな :class:`~email.message.Message` オブジェクトを追加したり、あるものを\
別のところへ移動させたりできます。これは MIME メッセージを\
切ったりおろしたりするために非常に便利なインターフェイスを提供します。

新しいメッセージオブジェクト構造は :class:`~email.message.Message` インスタンスを\
作成することにより作れます。ここに添付ファイルやその他適切なものを\
すべて手で加えてやればよいのです。MIME メッセージの場合、
:mod:`email` パッケージはこれらを簡単におこなえるようにするために\
いくつかの便利なサブクラスを提供しています。

以下がそのサブクラスです:

.. currentmodule:: email.mime.base

.. class:: MIMEBase(_maintype, _subtype, **_params)

   Module: :mod:`email.mime.base`

   これはすべての :class:`~email.message.Message` の MIME 用サブクラスの基底となるクラスです。\
   とくに :class:`MIMEBase` のインスタンスを直接作成することは
   (可能ではありますが) ふつうはしないでしょう。
   :class:`MIMEBase` は単により特化された MIME
   用サブクラスのための便宜的な基底クラスとして提供されています。

   *_maintype* は :mailheader:`Content-Type` の主形式 (maintype)
   であり (:mimetype:`text` や :mimetype:`image` など)、
   *_subtype* は :mailheader:`Content-Type` の副形式 (subtype) です
   (:mimetype:`plain` や :mimetype:`gif` など)。
   *_params* は各パラメータのキーと値を格納した辞書であり、
   これは直接 :meth:`Message.add_header` に渡されます。

   :class:`MIMEBase` クラスはつねに
   (*_maintype* 、 *_subtype* 、および *_params* にもとづいた)
   :mailheader:`Content-Type` ヘッダと、 :mailheader:`MIME-Version` ヘッダ
   (必ず ``1.0`` に設定される) を追加します。


.. currentmodule:: email.mime.nonmultipart

.. class:: MIMENonMultipart()

   Module: :mod:`email.mime.nonmultipart`

   :class:`~email.mime.base.MIMEBase` のサブクラスで、これは :mimetype:`multipart` 形式でない MIME
   メッセージのための中間的な基底クラスです。このクラスのおもな目的は、
   通常 :mimetype:`multipart` 形式のメッセージに対してのみ意味をなす
   :meth:`attach` メソッドの使用をふせぐことです。もし :meth:`attach` メソッドが\
   呼ばれた場合、これは :exc:`~email.errors.MultipartConversionError` 例外を発生します。

   .. versionadded:: 2.2.2


.. currentmodule:: email.mime.multipart

.. class:: MIMEMultipart([_subtype[, boundary[, _subparts[, _params]]]])

   Module: :mod:`email.mime.multipart`

   :class:`~email.mime.base.MIMEBase` のサブクラスで、これは :mimetype:`multipart` 形式の MIME
   メッセージのための中間的な基底クラスです。オプション引数 *_subtype* は\
   デフォルトでは :mimetype:`mixed` になっていますが、そのメッセージの副形式
   (subtype) を指定するのに使うことができます。メッセージオブジェクトには
   :mimetype:`multipart/_subtype` という値をもつ :mailheader:`Content-Type` ヘッダとともに、
   :mailheader:`MIME-Version` ヘッダが追加されるでしょう。

   オプション引数 *boundary* は multipart の境界文字列です。
   これが ``None`` の場合 (デフォルト)、境界は必要に応じて計算されます
   （例えばメッセージがシリアライズされるときなど）。

   *_subparts* はそのペイロードの subpart の初期値からなるシーケンスです。
   このシーケンスはリストに変換できるようになっている必要があります。
   新しい subpart はつねに :meth:`Message.attach` メソッドを使って\
   そのメッセージに追加できるようになっています。

   :mailheader:`Content-Type` ヘッダに対する追加のパラメータは\
   キーワード引数 *_params* を介して取得あるいは設定されます。
   これはキーワード辞書になっています。

   .. versionadded:: 2.2.2


.. currentmodule:: email.mime.application

.. class:: MIMEApplication(_data[, _subtype[, _encoder[, **_params]]])

   Module: :mod:`email.mime.application`

   :class:`~email.mime.nonmultipart.MIMENonMultipart` のサブクラスである :class:`MIMEApplication`
   クラスは MIME メッセージオブジェクトのメジャータイプ :mimetype:`application`
   を表します。
   *_data* は生のバイト列が入った文字列です。オプション引数 *_subtype* は
   MIME のサブタイプを設定します。サブタイプのデフォルトは :mimetype:`octet-stream`
   です。

   オプション引数の *_encoder* は呼び出し可能なオブジェクト(関数など)で、
   データの転送に使う実際のエンコード処理を行います。
   この呼び出し可能なオブジェクトは引数を1つ取り、それは :class:`MIMEApplication`
   のインスタンスです。
   ペイロードをエンコードされた形式に変更するために :meth:`get_payload` と
   :meth:`set_payload` を使い、必要に応じて
   :mailheader:`Content-Transfer-Encoding` やその他のヘッダを\
   メッセージオブジェクトに追加するべきです。デフォルトのエンコードは base64
   です。組み込みのエンコーダの一覧は :mod:`email.encoders`
   モジュールを見てください。

   *_params* は基底クラスのコンストラクタにそのまま渡されます。

   .. versionadded:: 2.5


.. currentmodule:: email.mime.audio

.. class:: MIMEAudio(_audiodata[, _subtype[, _encoder[, **_params]]])

   Module: :mod:`email.mime.audio`

   :class:`MIMEAudio` クラスは :class:`~email.mime.nonmultipart.MIMENonMultipart` のサブクラスで、
   主形式 (maintype) が :mimetype:`audio` の MIME オブジェクトを作成\
   するのに使われます。 *_audiodata* は実際の音声データを格納した文字列です。
   もしこのデータが標準の Python モジュール :mod:`sndhdr` によって\
   認識できるものであれば、
   :mailheader:`Content-Type` ヘッダの副形式 (subtype) は自動的に決定されます。
   そうでない場合はその画像の形式 (subtype) を *_subtype* で
   明示的に指定する必要があります。副形式が自動的に決定できず、
   *_subtype* の指定もない場合は、 :exc:`TypeError` が発生します。

   オプション引数 *_encoder* は呼び出し可能なオブジェクト (関数など) で、
   トランスポートのさいに画像の実際のエンコードをおこないます。このオブジェクトは
   :class:`MIMEAudio` インスタンスの引数をひとつだけ取ることができます。
   この関数は、与えられたペイロードをエンコードされた形式に変換するのに
   :meth:`get_payload` および :meth:`set_payload` を使う必要があります。
   また、これは必要に応じて :mailheader:`Content-Transfer-Encoding` あるいは
   そのメッセージに適した何らかのヘッダを追加する必要があります。
   デフォルトのエンコーディングは base64 です。組み込みのエンコーダの詳細については
   :mod:`email.encoders` を参照してください。

   *_params* は :class:`MIMEBase` コンストラクタに直接渡されます。


.. currentmodule:: email.mime.image

.. class:: MIMEImage(_imagedata[, _subtype[, _encoder[, **_params]]])

   Module: :mod:`email.mime.image`

   :class:`MIMEImage` クラスは :class:`~email.mime.nonmultipart.MIMENonMultipart.MIMENonMultipart` のサブクラスで、
   主形式 (maintype) が :mimetype:`image` の MIME オブジェクトを作成\
   するのに使われます。 *_imagedata* は実際の画像データを格納した文字列です。
   もしこのデータが標準の Python モジュール :mod:`imghdr` によって\
   認識できるものであれば、
   :mailheader:`Content-Type` ヘッダの副形式 (subtype) は自動的に決定されます。
   そうでない場合はその画像の形式 (subtype) を *_subtype* で\
   明示的に指定する必要があります。副形式が自動的に決定できず、
   *_subtype* の指定もない場合は、 :exc:`TypeError` が発生します。

   オプション引数 *_encoder* は呼び出し可能なオブジェクト (関数など) で、
   トランスポートのさいに画像の実際のエンコードをおこないます。
   このオブジェクトは :class:`MIMEImage` インスタンスの引数をひとつだけ\
   取ることができます。
   この関数は、与えられたペイロードをエンコードされた形式に変換するのに
   :meth:`get_payload` および :meth:`set_payload` を使う必要があります。
   また、これは必要に応じて :mailheader:`Content-Transfer-Encoding` あるいは\
   そのメッセージに適した何らかのヘッダを追加する必要があります。
   デフォルトのエンコーディングは base64 です。組み込みのエンコーダの詳細については
   :mod:`email.encoders` を参照してください。

   *_params* は :class:`~email.mime.base.MIMEBase` コンストラクタに直接渡されます。


.. currentmodule:: email.mime.message

.. class:: MIMEMessage(_msg[, _subtype])

   Module: :mod:`email.mime.message`

   :class:`MIMEMessage` クラスは :class:`~email.mime.nonmultipart.MIMENonMultipart` のサブクラスで、
   主形式 (maintype) が :mimetype:`message` の MIME オブジェクトを作成\
   するのに使われます。ペイロードとして使われるメッセージは *_msg*
   になります。これは :class:`~email.message.Message` クラス (あるいはそのサブクラス) の\
   インスタンスでなければいけません。そうでない場合、この関数は
   :exc:`TypeError` を発生します。

   オプション引数 *_subtype* はそのメッセージの副形式 (subtype) を設定します。
   デフォルトではこれは :mimetype:`rfc822` になっています。


.. currentmodule:: email.mime.text

.. class:: MIMEText(_text[, _subtype[, _charset]])

   Module: :mod:`email.mime.text`

   :class:`MIMEText` クラスは :class:`~email.mime.nonmultipart.MIMENonMultipart` のサブクラスで、
   主形式 (maintype) が :mimetype:`text` の MIME オブジェクトを作成\
   するのに使われます。ペイロードの文字列は *_text* になります。
   *_subtype* には副形式 (subtype) を指定し、デフォルトは :mimetype:`plain` です。
   *_charset* はテキストの文字セットで、
   :class:`~email.mime.nonmultipart.MIMENonMultipart` コンストラクタに引数として渡されます。
   デフォルトではこの値は ``us-ascii`` になっています。
   *_text* が unicode の場合には *_charset* の *output_charset* でエンコードされ、
   それ以外の場合にはそのまま使われます。
   
   .. versionchanged:: 2.4
      以前、推奨されない引数であった *_encoding* は撤去されました。
      *_charset* 引数を基にして Content Transfer Encodnig が暗黙に決定されます。

