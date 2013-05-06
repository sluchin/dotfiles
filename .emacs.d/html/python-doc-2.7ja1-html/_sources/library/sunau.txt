
:mod:`sunau` --- Sun AUファイルの読み書き
=========================================

.. module:: sunau
   :synopsis: Sun AUサウンドフォーマットへのインターフェース
.. sectionauthor:: Moshe Zadka <moshez@zadka.site.co.il>


:mod:`sunau` モジュールは、Sun AUサウンドフォーマットへの便利なインター
フェースを提供します。このモジュールは、 :mod:`aifc` モジュールや :mod:`wave` モジュールと互換性のあるインターフェースを備えています。

オーディオファイルはヘッダとそれに続くデータから構成されます。ヘッダのフィールドは以下の通りです：

+---------------+------------------------------------------------------------------+
| フィールド    | 内容                                                             |
+===============+==================================================================+
| magic word    | 4バイト文字列 ``.snd``                                           |
+---------------+------------------------------------------------------------------+
| header size   | infoを含むヘッダのサイズをバイト数で示したもの。                 |
+---------------+------------------------------------------------------------------+
| data size     | データの物理サイズをバイト数で示したもの。                       |
+---------------+------------------------------------------------------------------+
| encoding      | オーディオサンプルのエンコード形式。                             |
+---------------+------------------------------------------------------------------+
| sample rate   | サンプリングレート。                                             |
+---------------+------------------------------------------------------------------+
| # of channels | サンプルのチャンネル数。                                         |
+---------------+------------------------------------------------------------------+
| info          | オーディオファイルについての説明をASCII文字列で示したもの（null  |
|               | バイトで埋められます）。                                         |
+---------------+------------------------------------------------------------------+

infoフィールド以外の全てのヘッダフィールドは4バイトの大きさです。ヘッダフィールドはbig-endianでエンコードされた、計32ビットの符合なし整数です。

:mod:`sunau` モジュールは以下の関数を定義しています：


.. function:: open(file, mode)

   *file* が文字列ならその名前のファイルを開き、そうでないならファイルのようにシーク可能なオブジェクトとして扱います。 *mode* は以下のうち
   のいずれかです。

   ``'r'``
      読み込みのみのモード。

   ``'w'``
      書き込みのみのモード。

   読み込み／書き込み両方のモードで開くことはできないことに注意して下さい。

   ``'r'`` の *mode* は :class:`AU_read` オブジェクトを
   返し、 ``'w'`` と ``'wb'`` の *mode* は :class:`AU_write` オブジェクトを返します。


.. function:: openfp(file, mode)

   :func:`.open` と同義。後方互換性のために残されています。


:mod:`sunau` モジュールは以下の例外を定義しています：


.. exception:: Error

   Sun AUの仕様や実装に対する不適切な操作により何か実行不可能となった時に発生するエラー。


:mod:`sunau` モジュールは以下のデータアイテムを定義しています：


.. data:: AUDIO_FILE_MAGIC

   big-endianで保存された正規のSun AUファイルは全てこの整数で始まります。これは文字列 ``.snd`` を整数に変換したものです。


.. data:: AUDIO_FILE_ENCODING_MULAW_8
          AUDIO_FILE_ENCODING_LINEAR_8
          AUDIO_FILE_ENCODING_LINEAR_16
          AUDIO_FILE_ENCODING_LINEAR_24
          AUDIO_FILE_ENCODING_LINEAR_32
          AUDIO_FILE_ENCODING_ALAW_8

   AUヘッダのencodingフィールドの値で、このモジュールでサポートしているものです。


.. data:: AUDIO_FILE_ENCODING_FLOAT
          AUDIO_FILE_ENCODING_DOUBLE
          AUDIO_FILE_ENCODING_ADPCM_G721
          AUDIO_FILE_ENCODING_ADPCM_G722
          AUDIO_FILE_ENCODING_ADPCM_G723_3
          AUDIO_FILE_ENCODING_ADPCM_G723_5

   AUヘッダのencodingフィールドの値のうち既知のものとして追加されているものですが、このモジュールではサポートされていません。


.. _au-read-objects:

AU_read オブジェクト
--------------------

上述の :func:`.open` によって返されるAU_readオブジェクトには、以下のメソッドがあります：


.. method:: AU_read.close()

   ストリームを閉じ、このオブジェクトのインスタンスを使用できなくします。
   （これはオブジェクトのガベージコレクション時に自動的に呼び出されます。）


.. method:: AU_read.getnchannels()

   オーディオチャンネル数（モノラルなら ``1`` 、ステレオなら ``2`` ）を返します。


.. method:: AU_read.getsampwidth()

   サンプルサイズをバイト数で返します。


.. method:: AU_read.getframerate()

   サンプリングレートを返します。


.. method:: AU_read.getnframes()

   オーディオフレーム数を返します。


.. method:: AU_read.getcomptype()

   圧縮形式を返します。 ``'ULAW'``, ``'ALAW'``, ``'NONE'`` がサポートされている形式です。


.. method:: AU_read.getcompname()

   :meth:`getcomptype` を人に判読可能な形にしたものです。上述の形式に対して、それぞれ ``'CCITT G.711 u-law'``,
   ``'CCITT G.711 A-law'``, ``'not compressed'`` がサポートされています。


.. method:: AU_read.getparams()

   :meth:`get\*` メソッドが返すのと同じ ``(nchannels,  sampwidth, framerate, nframes, comptype,
   compname)`` のタプルを返します。


.. method:: AU_read.readframes(n)

   *n* 個のオーディオフレームの値を読み込んで、バイトごとに文字に変換した文字列を返します。
   データはlinear形式で返されます。もし元のデータがu-LAW形式なら、変換されます。


.. method:: AU_read.rewind()

   ファイルのポインタをオーディオストリームの先頭に戻します。

以下の2つのメソッドは共通の"位置"を定義しています。"位置"は他の関数とは独立して実装されています。


.. method:: AU_read.setpos(pos)

   ファイルのポインタを指定した位置に設定します。 :meth:`tell` で返される値を *pos* として使用しなければなりません。


.. method:: AU_read.tell()

   ファイルの現在のポインタ位置を返します。返される値はファイルの実際の位置に対して何も操作はしません。

以下の2つのメソッドは :mod:`aifc` モジュールとの互換性のために定義されていますが、何も面白いことはしません。


.. method:: AU_read.getmarkers()

   ``None`` を返します。


.. method:: AU_read.getmark(id)

   エラーを発生します。


.. _au-write-objects:

AU_write オブジェクト
---------------------

上述の :func:`.open` によって返されるWave_writeオブジェクトには、以下のメソッドがあります：


.. method:: AU_write.setnchannels(n)

   チャンネル数を設定します。


.. method:: AU_write.setsampwidth(n)

   サンプルサイズを（バイト数で）設定します。


.. method:: AU_write.setframerate(n)

   フレームレートを設定します。


.. method:: AU_write.setnframes(n)

   フレーム数を設定します。あとからフレームが書き込まれるとフレーム数は変更されます。


.. method:: AU_write.setcomptype(type, name)

   圧縮形式とその記述を設定します。 ``'NONE'`` と ``'ULAW'`` だけが、出力時にサポートされている形式です。


.. method:: AU_write.setparams(tuple)

   *tuple* は ``(nchannels, sampwidth, framerate, nframes, comptype, compname)``
   で、それぞれ :meth:`set\*` のメソッドの値にふさわしいものでなければなりません。全ての変数を設定します。


.. method:: AU_write.tell()

   ファイルの中の現在位置を返します。 :meth:`AU_read.tell` と
   :meth:`AU_read.setpos` メソッドでお断りしたことがこのメソッドにも当てはまります。


.. method:: AU_write.writeframesraw(data)

   *nframes* の修正なしにオーディオフレームを書き込みます。


.. method:: AU_write.writeframes(data)

   オーディオフレームを書き込んで *nframes* を修正します。


.. method:: AU_write.close()

   *nframes* が正しいか確認して、ファイルを閉じます。このメソッドはオブジェクトの削除時に呼び出されます。

:meth:`writeframes` や :meth:`writeframesraw` メソッドを呼び出したあ
とで、どんなパラメータを設定しようとしても不正となることに注意して下さい。

