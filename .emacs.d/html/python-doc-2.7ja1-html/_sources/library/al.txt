
:mod:`al` --- SGIのオーディオ機能
=================================

.. module:: al
   :platform: IRIX
   :synopsis: SGIのオーディオ機能。
   :deprecated:

.. deprecated:: 2.6
    :mod:`al` モジュールは Python 3.0 での削除に向け非推奨になりました。

このモジュールを使うと、SGI Indy と Indigo ワークステーションのオーディオ装置にアクセスできます。
詳しくは IRIX の man ページのセクション 3A を参照してください。
ここに書かれた関数が何をするかを理解するには、man ページを読む必要があります！
IRIX のリリース 4.0.5 より前のものでは使えない関数もあります。
お使いのプラットフォームで特定の関数が使えるかどうか、マニュアルで確認してください。

このモジュールで定義された関数とメソッドは全て、名前に ``AL`` の接頭辞を付けた
C の関数と同義です。

.. index:: module: AL

C のヘッダーファイル ``<audio.h>`` のシンボル定数は標準モジュール :mod:`AL`
に定義されています。
下記を参照してください。

.. warning::

   オーディオライブラリの現在のバージョンは、
   不正な引数が渡されるとエラーステイタスが返るのではなく、coreを吐き出すことがあります。
   残念ながら、この現象が確実に起こる環境は述べられていないし、確認することは難しいので、
   Python インターフェースでこの種の問題に対して防御することはできません。
   （一つの例は過大なキューサイズを特定することです --- 上限については記載されていません。）

このモジュールには、以下の関数が定義されています：


.. function:: openport(name, direction[, config])

   引数 *name* と *direction* は文字列です。
   省略可能な引数 *config* は、 :func:`newconfig`
   で返されるコンフィギュレーションオブジェクトです。
   返り値は :dfn:`audio port object` です；
   オーディオポートオブジェクトのメソッドは下に書かれています。


.. function:: newconfig()

   返り値は新しい :dfn:`audio configuration object` です；
   オーディオコンフィギュレーションオブジェクトのメソッドは下に書かれています。


.. function:: queryparams(device)

   引数 *device* は整数です。
   返り値は :c:func:`ALqueryparams` で返されるデータを含む整数のリストです。


.. function:: getparams(device, list)

   引数 *device* は整数です。
   引数 *list* は :func:`queryparams` で返されるようなリストです；
   :func:`queryparams` を適切に（！）修正して使うことができます。


.. function:: setparams(device, list)

   引数 *device* は整数です。
   引数 *list* は :func:`queryparams` で返されるようなリストです。


.. _al-config-objects:

コンフィギュレーションオブジェクト
----------------------------------

:func:`newconfig` で返されるコンフィギュレーションオブジェクトには以下のメソッドがあります：


.. method:: audio configuration.getqueuesize()

   キューサイズを返します。


.. method:: audio configuration.setqueuesize(size)

   キューサイズを設定します。


.. method:: audio configuration.getwidth()

   サンプルサイズを返します。


.. method:: audio configuration.setwidth(width)

   サンプルサイズを設定します。


.. method:: audio configuration.getchannels()

   チャンネル数を返します。


.. method:: audio configuration.setchannels(nchannels)

   チャンネル数を設定します。


.. method:: audio configuration.getsampfmt()

   サンプルのフォーマットを返します。


.. method:: audio configuration.setsampfmt(sampfmt)

   サンプルのフォーマットを設定します。


.. method:: audio configuration.getfloatmax()

   浮動小数点数でサンプルデータの最大値を返します。


.. method:: audio configuration.setfloatmax(floatmax)

   浮動小数点数でサンプルデータの最大値を設定します。


.. _al-port-objects:

ポートオブジェクト
------------------

:func:`openport` で返されるポートオブジェクトには以下のメソッドがあります：


.. method:: audio port.closeport()

   ポートを閉じます。


.. method:: audio port.getfd()

   ファイルディスクリプタを整数で返します。


.. method:: audio port.getfilled()

   バッファに存在するサンプルの数を返します。


.. method:: audio port.getfillable()

   バッファの空きに入れることのできるサンプルの数を返します。


.. method:: audio port.readsamps(nsamples)

   必要ならブロックして、キューから指定のサンプル数を読み込みます。
   生データを文字列として（例えば、サンプルサイズが 2 バイトならサンプル当たり
   2 バイトが big-endian (high byte、low byte) で）返します。


.. method:: audio port.writesamps(samples)

   必要ならブロックして、キューにサンプルを書き込みます。サンプルは
   :meth:`readsamps` で返される値のようにエンコードされていなければなりません。


.. method:: audio port.getfillpoint()

   'fill point' を返します。


.. method:: audio port.setfillpoint(fillpoint)

   'fill point' を設定します。


.. method:: audio port.getconfig()

   現在のポートのコンフィギュレーションを含んだコンフィギュレーションオブジェクトを返します。


.. method:: audio port.setconfig(config)

   コンフィギュレーションを引数に取り、そのコンフィギュレーションに設定します。


.. method:: audio port.getstatus(list)

   最後のエラーについてのステイタスの情報を返します。


:mod:`AL` --- :mod:`al` モジュールで使われる定数
================================================

.. module:: AL
   :platform: IRIX
   :synopsis: alモジュールで使われる定数。
   :deprecated:

.. deprecated:: 2.6
   :mod:`AL` モジュールは Python 3.0 での削除に向けて非推奨になりました。


このモジュールには、組み込みモジュール :mod:`al` (上記参照)
を使用するのに必要とされるシンボリック定数が定義されています。
定数の名前は C の include ファイル ``<audioio.h>`` で接頭辞 ``AL_``
を除いたものと同じです。

定義されている名前の完全なリストについてはモジュールのソースを参照してください。
お勧めの使い方は以下の通りです：

::

   import al
   from AL import *

