:mod:`ScrolledText` --- スクロールするテキストウィジェット
==========================================================

.. module:: ScrolledText
   :platform: Tk
   :synopsis: 垂直スクロールバーを持つテキストウィジェット。
.. sectionauthor:: Fred L. Drake, Jr. <fdrake@acm.org>


:mod:`ScrolledText` モジュールは"正しい動作"をするように設定された垂直\
スクロールバーをもつ基本的なテキストウィジェットを実装する同じ名前のク\
ラスを提供します。 :class:`ScrolledText` クラスを使うことは、テキストウィ\
ジェットとスクロールバーを直接設定するより簡単です。コンストラクタは
:class:`Tkinter.Text` クラスのものを同じです。

.. note::

   :mod:`ScrolledText` は Python 3.0 で :mod:`tkinter.scrolledtext`
   に改名されました。 :term:`2to3` ツールはソースの 3.0 への変換時に自動的に
   import を対応させます。

テキストウィジェットとスクロールバーは :class:`Frame` の中に一緒にpack\
され、 :class:`Grid` と :class:`Pack` ジオメトリマネジャのメソッドは\
:class:`Frame` オブジェクトから得られます。これによって、もっとも標準的\
なジオメトリマネジャの振る舞いにするために、直接 :class:`ScrolledText`
ウィジェットを使えるようになります。

特定の制御が必要ならば、以下の属性が利用できます:


.. attribute:: ScrolledText.frame

   テキストとスクロールバーウィジェットを取り囲むフレーム。


.. attribute:: ScrolledText.vbar

   スクロールバーウィジェット。
