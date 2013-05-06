
:mod:`fm` --- *Font Manager* インターフェース
=============================================

.. module:: fm
   :platform: IRIX
   :synopsis: SGIワークステーションの Font Manager インターフェース。
   :deprecated:


.. deprecated:: 2.6
    :mod:`fm` モジュールは Python 3.0 での削除に向けて非推奨になりました。


.. index::
   single: Font Manager, IRIS
   single: IRIS Font Manager

このモジュールはIRIS *Font Manager* ライブラリへのアクセスを提供します。
Silicon Graphics マシン上だけで利用可能です。
次も参照してください： *4Sight User's Guide*, section 1, chapter 5:  "Using the IRIS Font Manager"。
このモジュールは、まだ IRIS Font Manager への完全なインタフェースではありません。
サポートされていない機能は次のものです： matrix operations; cache operations; character
operations（代わりに string operations を使ってください）; font info のうちのいくつか; individual
glyph metrics; printer  matching。

以下の操作をサポートしています：


.. function:: init()

   関数を初期化します。 :c:func:`fminit` を呼び出します。
   この関数は :mod:`fm` モジュールを最初にインポートすると自動的に呼び出さ\
   れるので、普通、呼び出す必要はありません。


.. function:: findfont(fontname)

   フォントハンドルオブジェクトを返します。 ``fmfindfont(fontname)`` を呼び出します。


.. function:: enumerate()

   利用可能なフォント名のリストを返します。この関数は :c:func:`fmenumerate` へのインタフェースです。


.. function:: prstr(string)

   現在のフォントを使って文字列をレンダリングします（下のフォントハンドルメソッド
   :func:`setfont` を参照）。 ``fmprstr(string)`` を呼び出します。


.. function:: setpath(string)

   フォントの検索パスを設定します。 ``fmsetpath(string)`` を呼び出します。（XXX 機能しない！？！）


.. function:: fontpath()

   現在のフォント検索パスを返します。


フォントハンドルオブジェクトは以下の操作をサポートします：

.. method:: font handle.scalefont(factor)

   このフォントを拡大／縮小したハンドルを返します。 ``fmscalefont(fh, factor)`` を呼び出します。


.. method:: font handle.setfont()

   このフォントを現在のフォントに設定します。
   注意：フォントハンドルオブジェクトが削除されると、設定は告知なしに元に戻ります。
   ``fmsetfont(fh)`` を呼び出します。


.. method:: font handle.getfontname()

   このフォントの名前を返します。 ``fmgetfontname(fh)`` を呼び出します。


.. method:: font handle.getcomment()

   このフォントに関連付けられたコメント文字列を返します。コメント文字列が何もなければ例外を返します。 ``fmgetcomment(fh)`` を呼び出します。


.. method:: font handle.getfontinfo()

   このフォントに関連したデータを含むタプルを返します。これは ``fmgetfontinfo()`` へのインタフェースです。以下の数値を含むタプルを返します：
   ``(printermatched, fixed_width, xorig, yorig, xsize, ysize, height, nglyphs)`` 。


.. method:: font handle.getstrwidth(string)

   このフォントで *string* を描いたときの幅をピクセル数で返します。
   ``fmgetstrwidth(fh, string)`` を呼び出します。

