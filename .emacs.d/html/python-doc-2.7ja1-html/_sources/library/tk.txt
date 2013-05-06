
.. _tkinter:

********************************************
Tkを用いたグラフィカルユーザインターフェイス
********************************************

.. index::
   single: GUI
   single: Graphical User Interface
   single: Tkinter
   single: Tk

Tk/Tcl は長きにわたり Python の不可欠な一部でありつづけています。
Tk/Tcl は頑健でプラットホームに依存しないウィンドウ構築ツールキットであり、
Python プログラマは :mod:`Tkinter` モジュールやその拡張の :mod:`Tix`,
:mod:`ttk` モジュールを使って利用できます。

:mod:`Tkinter` モジュールは、 Tcl/Tk 上に作られた軽量な\
オブジェクト指向のレイヤです。 :mod:`Tkinter` を使うために
Tcl コードを書く必要はありませんが、Tk のドキュメントや、場合によって\
は Tcl のドキュメントを調べる必要があるでしょう。
:mod:`Tkinter` は Tk のウィジェットを Python のクラスとして\
実装しているラッパをまとめたものです。加えて、内部モジュール
:mod:`_tkinter` では、 Python と Tcl がやり取りできるような\
スレッド安全なメカニズムを提供しています。

:mod:`Tkinter` の一番素晴らしい点は速く、そして普通に Python\
に付属してくることです。標準ドキュメントが頼りないものだとしても、
代わりになるものが入手可能です: リファレンス、チュートリアル、
書籍その他です。 :mod:`Tkinter` は古臭いルックアンドフィール
でも有名ですが、その点は Tk 8.5 で幅広く改善されました。
とはいえ、興味を引きそうな GUI ライブラリは他にも多数あります。
そういったものについてはもっと知りたい人は :ref:`other-gui-packages`
節を参照してください。

.. toctree::

   tkinter.rst
   ttk.rst
   tix.rst
   scrolledtext.rst
   turtle.rst
   idle.rst
   othergui.rst

.. Other sections I have in mind are
   Tkinter internals
   Freezing Tkinter applications
   念頭にある他のセクションは
   Tkinter の内側
   Tkinter アプリケーションのパッケージ方法(freezing)

