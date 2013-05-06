
:mod:`uu` --- uuencode形式のエンコードとデコード
================================================

.. module:: uu
   :synopsis: uuencode形式のエンコードとデコードを行う。
.. moduleauthor:: Lance Ellinghouse


このモジュールではファイルをuuencode形式(任意のバイナリデータをASCII文字列\
に変換したもの)にエンコード、デコードする機能を提供します。
引数としてファイルが仮定されている所では、ファイルのようなオブジェクトが\
利用できます。後方互換性のために、パス名を含む文字列も利用できるようにし\
ていて、対応するファイルを開いて読み書きします。しかし、このインタフェー\
スは利用しないでください。呼び出し側でファイルを開いて(Windowsでは
``'rb'`` か ``'wb'`` のモードで)利用する方法が推奨されます。

.. index::
   single: Jansen, Jack
   single: Ellinghouse, Lance

このコードはLance Ellinghouseによって提供され、Jack Jansenによって更新さ\
れました。

.. seealso::

   最新バージョンの `uu module Python ソースコード
   <http://svn.python.org/view/python/branches/release27-maint/Lib/uu.py?view=markup>`_

:mod:`uu` モジュールでは以下の関数を定義しています。


.. function:: encode(in_file, out_file[, name[, mode]])

   *in_file* を *out_file* にエンコードします。
   エンコードされたファイルには、デフォルトでデコード時に利用される\
   *name* と *mode* を含んだヘッダがつきます。
   省略された場合には、 *in_file* から取得された名前か ``'-'`` という文字と、
   ``0666`` がそれぞれデフォルト値として与えられます。


.. function:: decode(in_file[, out_file[, mode]])

   uuencode形式でエンコードされた *in_file* をデコードして varout_file
   に書き出します。
   もし *out_file* がパス名でかつファイルを作る必要があるときには、
   *mode* がパーミッションの設定に使われます。
   *out_file* と *mode* のデフォルト値は *in_file* のヘッダから取得\
   されます。
   しかし、ヘッダで指定されたファイルが既に存在していた場合は、
   :exc:`uu.Error` が送出されます。

   誤った実装のuuencoderによる入力で、エラーから復旧できた場合、
   :func:`decode` は標準エラー出力に警告を表示するかもしれません。
   *quiet* を真にすることでこの警告を抑制することができます。


.. exception:: Error()

   :exc:`Exception` のサブクラスで、 :func:`uu.decode` によって、さ\
   まざまな状況で送出される可能性があります。上で紹介された場合以外にも、\
   ヘッダのフォーマットが間違っている場合や、入力ファイルが途中で区切れた\
   場合にも送出されます。


.. seealso::

   :mod:`binascii` モジュール
      ASCII からバイナリへ、バイナリからASCIIへの\
      変換をサポートするモジュール。

