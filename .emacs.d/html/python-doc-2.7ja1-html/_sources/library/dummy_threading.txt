
:mod:`dummy_threading` --- :mod:`threading` の代替モジュール
============================================================

.. module:: dummy_threading
   :synopsis: threading  の代替モジュール。


このモジュールは :mod:`threading` モジュールのインターフェースをそっくりまねるものです。
:mod:`threading` モジュールがサポートされていないプラットフォームで import
することを意図して作られたものです。

使用例::

   try:
       import threading as _threading
   except ImportError:
       import dummy_threading as _threading

生成するスレッドが他のブロックしたスレッドを待ち、デッドロック発生の可能性がある場合には、
このモジュールを使わないようにしてください。ブロッキング I/O を使っている場合によく起きます。

