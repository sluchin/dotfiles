
:mod:`fileinput` --- 複数の入力ストリームをまたいだ行の繰り返し処理をサポートする。
===================================================================================

.. module:: fileinput
   :synopsis: 標準入力もしくはファイルのリストをまたいでループする
.. moduleauthor:: Guido van Rossum <guido@python.org>
.. sectionauthor:: Fred L. Drake, Jr. <fdrake@acm.org>


.. % Iterate over lines from multiple input streams}



このモジュールは標準入力やファイルの並びにまたがるループを素早く書くためのヘルパークラスと関数を提供しています。

典型的な使い方は以下の通りです。 ::

   import fileinput
   for line in fileinput.input():
       process(line)

このプログラムは ``sys.argv[1:]`` に含まれる全てのファイルをまたいで繰り返します。
もし該当するものがなければ、 ``sys.stdin`` がデフォルトとして扱われます。
ファイル名として ``'-'`` が与えられた場合も、 ``sys.stdin`` に置き換えられます。別のファイル名リストを使いたい時には、
:func:`.input` の最初の引数にリストを与えます。単一ファイル名の文字列も受け付けます。

全てのファイルはデフォルトでテキストモードでオープンされます。しかし、 :func:`.input` や :class:`FileInput()`
をコールする際に *mode* パラメータを指定すれば、これをオーバーライドすることができます。オープン中あるいは読み込み中にI/Oエラーが発生した場合には、
:exc:`IOError` が発生します。

``sys.stdin`` が2回以上使われた場合は、2回目以降は行を返しません。ただしインタラクティブに利用している時や明示的にリセット
(``sys.stdin.seek(0))`` を使う)を行った場合はその限りではありません。

空のファイルは開いた後すぐ閉じられます。空のファイルはファイル名リストの最後にある場合にしか外部に影響を与えません。

ファイルの各行は、各種改行文字まで含めて返されます。ファイルの最後が改行文字で終っていない場合には、改行文字で終わらない行が返されます。

ファイルのオープン方法を制御するためのオープン時フックは、 :func:`fileinput.input` あるいは :class:`FileInput()` の
*openhook* パラメータで設定します。このフックは、ふたつの引数 *filename* と *mode*
をとる関数でなければなりません。そしてその関数の返り値はオープンしたファイルオブジェクトとなります。このモジュールには、便利なフックが既に用意されています。

.. seealso::

   最新バージョンの `fileinput Python ソースコード
   <http://svn.python.org/view/python/branches/release27-maint/Lib/fileinput.py?view=markup>`_

以下の関数がこのモジュールの基本的なインタフェースです。


.. function:: input([files[, inplace[, backup[, mode[, openhook]]]]])

   :class:`FileInput` クラスのインスタンスを作ります。生成されたインスタンス
   は、このモジュールの関数群が利用するグローバルな状態として利用されます。
   この関数への引数は :class:`FileInput` クラスのコンストラクタへ渡されます。

   .. versionchanged:: 2.5
      パラメータ *mode* および *openhook* が追加されました.

以下の関数は :func:`fileinput.input` 関数によって作られたグローバルな状態を利用します。
アクティブな状態が無い場合には、 :exc:`RuntimeError` が発生します。


.. function:: filename()

   現在読み込み中のファイル名を返します。一行目が読み込まれる前は ``None`` を返します。


.. function:: fileno()

   現在のファイルの "ファイルデスクリプタ" を整数値で返します。ファイルがオープンされていない場合 (最初の行の前、ファイルとファイルの間) は ``-1``
   を返します。

   .. versionadded:: 2.5


.. function:: lineno()

   最後に読み込まれた行の、累積した行番号を返します。1行目が読み込まれる前は ``0`` を返します。最後のファイルの最終行が読み込まれた後には、その
   行の行番号を返します。


.. function:: filelineno()

   現在のファイル中での行番号を返します。1行目が読み込まれる前は ``0`` を返します。最後のファイルの最終行が読み込まれた後には、その
   行のファイル中での行番号を返します。


.. function:: isfirstline()

   最後に読み込まれた行がファイルの1行目ならTrue、そうでなければFalseを返します。


.. function:: isstdin()

   最後に読み込まれた行が ``sys.stdin`` から読まれていればTrue、そうでなければFalseを返します。


.. function:: nextfile()

   現在のファイルを閉じます。次の繰り返しでは(存在すれば)次のファイルの最初の行が読み込まれます。閉じたファイルの読み込まれなかった行は、累積の行
   数にカウントされません。ファイル名は次のファイルの最初の行が読み込まれるまで変更されません。最初の行の読み込みが行われるまでは、この関数は呼
   び出されても何もしませんので、最初のファイルをスキップするために利用することはできません。最後のファイルの最終行が読み込まれた後にも、この関
   数は呼び出されても何もしません。


.. function:: close()

   シーケンスを閉じます。

このモジュールのシーケンスの振舞いを実装しているクラスのサブクラスを作ることもできます。


.. class:: FileInput([files[, inplace[, backup[, mode[, openhook]]]]])

   :class:`FileInput` クラスはモジュールの関数に対応するメソッド
   :meth:`filename` 、 :meth:`fileno` 、 :meth:`lineno` 、
   :meth:`filelineno` 、 :meth:`isfirstline` 、 :meth:`isstdin` 、 :meth:`nextfile` および
   :meth:`close` を実装しています。それに加えて、次の入力行を返す :meth:`readline` メソッドと、シーケンスの振舞
   いの実装をしている :meth:`__getitem__` メソッドがあります。シーケンスはシーケンシャルに読み込むことしかできません。
   つまりランダムアクセスと :meth:`readline` を混在させることはできません。

   *mode* を使用すると、 :func:`open` に渡すファイルモードを指定することができます。これは ``'r'`` 、 ``'rU'`` 、 ``'U'``
   および ``'rb'`` のうちのいずれかとなります。

   *openhook* を指定する場合は、ふたつの引数 *filename* と *mode* をとる関数でなければなりません。この関数の返り値は、オー
   プンしたファイルオブジェクトとなります。 *inplace* と *openhook* を同時に使うことはできません。

   .. versionchanged:: 2.5
      パラメータ *mode* および *openhook* が追加されました.

**インプレース(in-place)フィルタオプション:** キーワード引数 ``inplace=1`` が :func:`input` か
:class:`FileInput` クラスのコンストラクタに渡された場合には、
入力ファイルはバックアップファイルに移動され、標準出力が入力ファイルに設定されます(バックアップファイルと同じ名前のファイルが既に存在していた
場合には、警告無しに置き替えられます)。これによって入力ファイルをその場で書き替えるフィルタを書くことができます。
キーワード引数 *backup* (通常は ``backup='.<拡張子>'`` という形で利用します)が与えられていた場合、
バックアップファイルの拡張子として利用され、バックアップファイルは削除されずに残ります。
デフォルトでは、拡張子は ``'.bak'`` になっていて、出力先のファイルが閉じられればバックアップファイルも消されます。
インプレースフィルタ機能は、標準入力を読み込んでいる間は無効にされます。

.. note::
    現在の実装はMS-DOSの8+3ファイルシステムでは動作しません。

このモジュールには、次のふたつのオープン時フックが用意されています。


.. function:: hook_compressed(filename, mode)

   gzipやbzip2で圧縮された(拡張子が ``'.gz'`` や ``'.bz2'`` の)
   ファイルを、 :mod:`gzip` モジュールや :mod:`bz2` モジュールを使って透過的にオープンします。ファイルの拡張子が ``'.gz'`` や
   ``'.bz2'`` でない場合は、通常通りファイルをオープンします (つまり、 :func:`open` をコールする際に伸長を行いません)。

   使用例: ``fi = fileinput.FileInput(openhook=fileinput.hook_compressed)``

   .. versionadded:: 2.5


.. function:: hook_encoded(encoding)

   各ファイルを :func:`codecs.open` でオープンするフックを返します。指定した *encoding* でファイルを読み込みます。

   使用例: ``fi = fileinput.FileInput(openhook=fileinput.hook_encoded("iso-8859-1"))``

   .. note::

      このフックでは、指定した *encoding* によっては :class:`FileInput` がUnicode文字列を返す可能性があります。

   .. versionadded:: 2.5

