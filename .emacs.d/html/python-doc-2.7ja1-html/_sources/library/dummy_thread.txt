:mod:`dummy_thread` --- :mod:`thread` の代替モジュール
=======================================================

.. module:: dummy_thread
   :synopsis: thread の代替モジュール。

.. note::
   .. The :mod:`dummy_thread` module has been renamed to :mod:`_dummy_thread` in
      Python 3.0.  The :term:`2to3` tool will automatically adapt imports when
      converting your sources to 3.0; however, you should consider using the
      high-lever :mod:`dummy_threading` module instead.

   :mod:`dummy_thread` モジュールは、Python 3.0では :mod:`_dummy_thread` に変更されました。
   :term:`2to3` ツールは自動的にソースコードの import を修正します。
   しかし、代わりに高レベルの :mod:`dummy_threading` モジュールの利用を検討するべきです。

このモジュールは :mod:`thread` モジュールのインターフェースをそっくりまねるものです。
:mod:`thread` モジュールがサポートされていないプラットフォームで import することを意図して作られたものです。

使用例::

   try:
       import thread as _thread
   except ImportError:
       import dummy_thread as _thread

生成するスレッドが、他のブロックしたスレッドを待ち、デッドロック発生の可能性がある場合には、このモジュールを使わないようにしてください。
ブロッキング I/O を使っている場合によく起きます。

