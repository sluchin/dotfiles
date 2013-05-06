
:mod:`ColorPicker` --- 色選択ダイアログ
=======================================

.. module:: ColorPicker
   :platform: Mac
   :synopsis: 標準色選択ダイアログへのインターフェース
   :deprecated:
.. moduleauthor:: Just van Rossum <just@letterror.com>
.. sectionauthor:: Fred L. Drake, Jr. <fdrake@acm.org>


:mod:`ColorPicker` モジュールは標準色選択ダイアログへのアクセスを提\
供します。

.. note::

   このモジュールは Python 3.x で削除されました。

.. function:: GetColor(prompt, rgb)

   標準色選択ダイアログを表示し、ユーザが色を選択することを可能にします。
   *prompt* の文字列によりユーザに指示を与えられ、デフォルトの選択色を
   *rgb* で設定する事ができます。 *rgb* は赤、緑、青の色要素のタプル\
   で与えてください。 :func:`GetColor` はユーザが選択した\
   色のタプルと色が選択されたか、取り消されたかを示すフラグを返します。
