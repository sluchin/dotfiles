
:mod:`StringIO` --- ファイルのように文字列を読み書きする
========================================================

.. module:: StringIO
   :synopsis: ファイルのように文字列を読み書きする。


このモジュールは、(*メモリファイル* としても知られている)
文字列のバッファに対して読み書きを行うファイルのようなクラス、
:class:`StringIO` を実装しています。
(通常の文字列については、 :class:`str` と :class:`unicode` を参照してください)

操作方法についてはファイルオブジェクトの説明を参照してください(セクション :ref:`bltin-file-objects`)。


.. class:: StringIO([buffer])

   :class:`StringIO` オブジェクトを作る際に、コンストラクターに文字列を渡すことで初期化することができます。文字列を渡さない場合、最初は
   :class:`StringIO` はカラです。どちらの場合でも最初のファイル位置は 0 から始まります。

   :class:`StringIO` オブジェクトはユニコードも 8-bit の文字列も受け付けますが、この2つを混ぜることには少し注意が必要です。
   この2つが一緒に使われると、 :meth:`getvalue` が呼ばれたときに、 (8th ビットを使っている)7-bit ASCII に解釈できない
   8-bit の文字列は、 :exc:`UnicodeError` を引き起こします。

次にあげる :class:`StringIO` オブジェクトのメソッドには特別な説明が必要です:


.. method:: StringIO.getvalue()

   :class:`StringIO` オブジェクトの :meth:`close` メソッドが呼ばれる前ならいつでも、 "file" の中身全体を返します。
   ユニコードと 8-bit の文字列を混ぜることの説明は、上の注意を参照してください。この2つの文字コードを混ぜると、このメソッドは
   :exc:`UnicodeError` を引き起こすかもしれません。


.. method:: StringIO.close()

   メモリバッファを解放します。
   close された後の :class:`StringIO` オブジェクトを操作しようとすると
   :exc:`ValueError` が送出されます。

使用例::

   import StringIO

   output = StringIO.StringIO()
   output.write('First line.\n')
   print >>output, 'Second line.'

   # ファイルの内容を取り出す -- ここでは
   # 'First line.\nSecond line.\n'
   contents = output.getvalue()

   # オブジェクトを閉じてメモリバッファを解放する --
   # .getvalue() は例外を送出するようになる。
   output.close()


:mod:`cStringIO` --- 高速化された :mod:`StringIO`
=================================================

.. module:: cStringIO
   :synopsis: StringIOを高速にしたものだが、サブクラス化はできない。
.. moduleauthor:: Jim Fulton <jim@zope.com>
.. sectionauthor:: Fred L. Drake, Jr. <fdrake@acm.org>


:mod:`cStringIO` モジュールは :mod:`StringIO` モジュールと同様のインターフェースを提供しています。
:class:`StringIO.StringIO` オブジェクトを酷使する場合、このモジュールにある  :func:`StringIO`
関数をかわりに使うと効果的です。


.. function:: StringIO([s])

   読み書きのための StringIO 類似ストリームを返します。

   組み込み型オブジェクトを返す factory 関数なので、
   サブクラス化して独自の関数を組むことはできません。
   属性の追加もできません。
   これらをするにはオリジナルの :mod:`StringIO` モジュールを使ってください。

   :mod:`StringIO` モジュールで実装されているメモリファイルとは異なり、このモジュールで提供されているものは、プレイン ASCII
   文字列にエンコードできないユニコードを受け付けることができません。

   Unicode文字列を使って :func:`StringIO` を呼び出すと、文字列をエンコードする\
   のではなく Unicode 文字列の buffer 表現が利用されます。

   また、引数に文字列を指定して :func:`StringIO` 呼び出すと読み出し専用のオブジェクトが生成されますが、この場合
   :class:`cStringIO.StringIO()` では write()メソッドを持たないオブジェクトを生成します。
   これらのオブジェクトは普段は見えません。トレースバックに :class:`StringI` と :class:`StringO` として表示されます。



次にあげるデータオブジェクトも提供されています:


.. data:: InputType

   文字列をパラメーターに渡して :func:`StringIO` を呼んだときに作られるオブジェクトのオブジェクト型。


.. data:: OutputType

   パラメーターを渡さすに :func:`StringIO` を呼んだときに返されるオブジェクトのオブジェクト型。

このモジュールには C API もあります。詳しくはこのモジュールのソースを参照してください。

使用例::

   import cStringIO

   output = cStringIO.StringIO()
   output.write('First line.\n')
   print >>output, 'Second line.'

   # ファイルの内容を取り出す -- ここでは
   # 'First line.\nSecond line.\n'
   contents = output.getvalue()

   # オブジェクトを閉じてメモリバッファを解放する --
   # 以降 .getvalue() は例外を送出するようになる。
   output.close()

