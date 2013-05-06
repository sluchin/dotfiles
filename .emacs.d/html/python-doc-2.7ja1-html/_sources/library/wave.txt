:mod:`wave` --- WAVファイルの読み書き
=====================================

.. module:: wave
   :synopsis: WAVサウンドフォーマットへのインターフェイス
.. sectionauthor:: Moshe Zadka <moshez@zadka.site.co.il>
.. Documentations stolen from comments in file.


:mod:`wave` モジュールは、WAVサウンドフォーマットへの便利なインターフェイスを提供するモジュールです。

このモジュールは圧縮／展開をサポートしていませんが、モノラル／ステレオには対応しています。

:mod:`wave` モジュールは、以下の関数と例外を定義しています。


.. function:: open(file[, mode])

   *file* が文字列ならその名前のファイルを開き、そうでないなら
   シーク可能な file like オブジェクトとして扱います。
   *mode* は以下のうちのいずれかです。

   ``'r'``, ``'rb'``
      読み込みのみのモード。

   ``'w'``, ``'wb'``
      書き込みのみのモード。

   WAVファイルに対して読み込み／書き込み両方のモードで開くことはできないことに注意して下さい。
   ``'r'`` と ``'rb'`` の *mode* は :class:`Wave_read` オブジェクトを
   返し、 ``'w'`` と ``'wb'`` の *mode* は :class:`Wave_write` オブジェクトを返します。
   *mode* が省略されていて、ファイルのようなオブジェクトが *file* として渡されると、 ``file.mode`` が *mode* のデフォルト値として使わ
   れます（必要であれば、さらにフラグ ``'b'`` が付け加えられます）。

   file like オブジェクトを渡した場合、 wave オブジェクトの :meth:`close`
   メソッドを呼び出してもその file like オブジェクトを close しません。
   file like オブジェクトの close は呼び出し側の責任になります。

.. function:: openfp(file, mode)

   :func:`.open` と同義。後方互換性のために残されています。


.. exception:: Error

   WAVの仕様を犯したり、実装の欠陥に遭遇して何か実行不可能となった時に発生するエラー。


.. _wave-read-objects:

Wave_read オブジェクト
----------------------

:func:`.open` によって返される Wave_read オブジェクトには、以下のメソッドが
あります：

.. method:: Wave_read.close()

   :mod:`wave` によって開かれていた場合はストリームを閉じ、このオブジェクトの
   インスタンスを使用できなくします。
   これはオブジェクトのガベージコレクション時に自動的に呼び出されます。


.. method:: Wave_read.getnchannels()

   オーディオチャンネル数（モノラルなら ``1`` 、ステレオなら ``2`` ）を返します。


.. method:: Wave_read.getsampwidth()

   サンプルサイズをバイト数で返します。


.. method:: Wave_read.getframerate()

   サンプリングレートを返します。


.. method:: Wave_read.getnframes()

   オーディオフレーム数を返します。


.. method:: Wave_read.getcomptype()

   圧縮形式を返します（ ``'NONE'`` だけがサポートされている形式です）。


.. method:: Wave_read.getcompname()

   :meth:`getcomptype` を人に判読可能な形にしたものです。通常、 ``'NONE'`` に対して
   ``'not compressed'`` が返されます。


.. method:: Wave_read.getparams()

   :meth:`get\*` メソッドが返すのと同じ
   ``(nchannels,  sampwidth, framerate, nframes, comptype, compname)``
   のタプルを返します。


.. method:: Wave_read.readframes(n)

   現在のポインタから *n* 個のオーディオフレームの値を読み込んで、バイトごとに文字に変換して文字列を返します。


.. method:: Wave_read.rewind()

   ファイルのポインタをオーディオストリームの先頭に戻します。

以下の2つのメソッドは :mod:`aifc` モジュールとの互換性のために定義されていますが、何も面白いことはしません。


.. method:: Wave_read.getmarkers()

   ``None`` を返します。


.. method:: Wave_read.getmark(id)

   エラーを発生します。

以下の2つのメソッドは共通の"位置"を定義しています。"位置"は他の関数とは独立して実装されています。


.. method:: Wave_read.setpos(pos)

   ファイルのポインタを指定した位置に設定します。


.. method:: Wave_read.tell()

   ファイルの現在のポインタ位置を返します。


.. _wave-write-objects:

Wave_write オブジェクト
-----------------------

:func:`.open` によって返される Wave_write オブジェクトには、以下のメソッドが
あります：

.. method:: Wave_write.close()

   *nframes* が正しいか確認して、ファイルが :mod:`wave` によって開かれていた場合は
   閉じます。このメソッドはオブジェクトがガベージコレクションされるときに呼び出されます。


.. method:: Wave_write.setnchannels(n)

   チャンネル数を設定します。


.. method:: Wave_write.setsampwidth(n)

   サンプルサイズを *n* バイトに設定します。


.. method:: Wave_write.setframerate(n)

   サンプリングレートを *n* に設定します。


.. method:: Wave_write.setnframes(n)

   フレーム数を *n* に設定します。あとからフレームが書き込まれるとフレーム数は変更されます。


.. method:: Wave_write.setcomptype(type, name)

   圧縮形式とその記述を設定します。
   現在のところ、非圧縮を示す圧縮形式 ``NONE`` だけがサポートされています。


.. method:: Wave_write.setparams(tuple)

   *tuple* は ``(nchannels, sampwidth, framerate, nframes, comptype, compname)``
   で、それぞれ :meth:`set\*` のメソッドの値にふさわしいものでなければなりません。全ての変数を設定します。


.. method:: Wave_write.tell()

   ファイルの中の現在位置を返します。 :meth:`Wave_read.tell` と
   :meth:`Wave_read.setpos` メソッドでお断りしたことがこのメソッドにも当てはまります。


.. method:: Wave_write.writeframesraw(data)

   *nframes* の修正なしにオーディオフレームを書き込みます。


.. method:: Wave_write.writeframes(data)

   オーディオフレームを書き込んで *nframes* を修正します。

:meth:`writeframes` や :meth:`writeframesraw` メソッドを呼び出したあ
とで、どんなパラメータを設定しようとしても不正となることに注意して下さい。そうすると :exc:`wave.Error` を発生します。

