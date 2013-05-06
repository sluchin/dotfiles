
:mod:`multifile` --- 個別の部分を含んだファイル群のサポート
===========================================================

.. module:: multifile
   :synopsis: MIME データのような、個別の部分を含んだファイル群に対する\
      読み出しのサポート。
   :deprecated:
.. sectionauthor:: Eric S. Raymond <esr@snark.thyrsus.com>


.. deprecated:: 2.5
   :mod:`multifile` モジュールよりも  :mod:`email` パッケージを使うべきです。
   このモジュールは後方互換性のためだけに存在しています。

:class:`MultiFile` オブジェクトはテキストファイルを区分したものを\
ファイル類似の入力オブジェクトとして扱えるようにし、指定した区切り文字
(delimiter) パタンに遭遇した際に ``''`` が返されるようにします。
このクラスの標準設定は MIME マルチパートメッセージを解釈する上で
便利となるように設計されていますが、サブクラス化を行って幾つかの\
メソッドを上書きすることで、簡単に汎用目的に対応させることができます。


.. class:: MultiFile(fp[, seekable])

   マルチファイル (multi-file) を生成します。このクラスは :func:`open`
   が返すファイルオブジェクトのような、 :class:`MultiFile`
   インスタンスが行データを取得するための入力となるオブジェクトを\
   引数としてインスタンス化を行わなければなりません。

   :class:`MultiFile` は入力オブジェクトの :meth:`readline` 、
   :meth:`seek` 、および :meth:`tell` メソッドしか参照せず、
   後者の二つのメソッドは個々の MIME パートにランダムアクセスしたい場合にのみ必要です。
   :class:`MultiFile` を seek できないストリームオブジェクトで使うには、
   オプションの *seekable* 引数の値を偽にしてください; これにより、入力オブジェクトの
   :meth:`seek` および :meth:`tail` メソッドを使わないようになります。

:class:`MultiFile` の視点から見ると、テキストは三種類の行データ:
データ、セクション分割子、終了マーカ、からなることを知っていると役に立つでしょう。
MultiFile は、多重入れ子構造になっている可能性のある、
それぞれが独自のセクション分割子および終了マーカのパターンを持つメッセージパートを\
サポートするように設計されています。


.. seealso::

   Module :mod:`email`
      網羅的な電子メール操作パッケージ;
      :mod:`multifile` モジュールに取って代わります。


.. _multifile-objects:

MultiFile オブジェクト
----------------------

:class:`MultiFile` インスタンスには以下のメソッドがあります:


.. method:: MultiFile.readline(str)

   一行データを読みます。
   その行が (セクション分割子や終了マーカや本物の EOF でない) データの場合、
   行データを返します。
   その行がもっとも最近スタックにプッシュされた境界パターンにマッチした場合、
   ``''`` を返し、マッチした内容が終了マーカかそうでないかによって
   ``self.last`` を 1 か 0 に設定します。
   行がその他のスタックされている境界パターンにマッチした場合、エラーが送出されます。
   背後のストリームオブジェクトにおけるファイルの終端に到達した場合、
   全ての境界がスタックから除去されていない限りこのメソッドは :exc:`Error` を送出します。


.. method:: MultiFile.readlines(str)

   このパートの残りの全ての行を文字列のリストとして返します。


.. method:: MultiFile.read()

   次のセクションまでの全ての行を読みます。
   読んだ内容を単一の (複数行にわたる) 文字列として返します。
   このメソッドには size 引数をとらないので注意してください！


.. method:: MultiFile.seek(pos[, whence])

   ファイルを seek します。
   seek する際のインデクスは現在のセクションの開始位置からの相対位置になります。
   *pis* および *whence* 引数はファイルの seek における引数と同じように解釈されます。


.. method:: MultiFile.tell()

   現在のセクションの先頭に対して相対的なファイル位置を返します。


.. method:: MultiFile.next()

   次のセクションまで行を読み飛ばします
   (すなわち、セクション分割子または終了マーカが消費されるまで行データを読みます)。
   次のセクションがあった場合には真を、終了マーカが発見された場合には偽を返します。
   最も最近スタックにプッシュされた境界パターンを最有効化します。


.. method:: MultiFile.is_data(str)

   *str* がデータの場合に真を返し、セクション分割子の可能性がある場合には偽を返します。
   このメソッドは行の先頭が (全ての MIME 境界が持っている)
   ``'-``\ ``-'`` 以外になっているかを調べるように実装されていますが、
   派生クラスで上書きできるように宣言されています。

   このテストは実際の境界テストにおいて高速性を保つために使われているので注意してください;
   このテストが常に false を返す場合、
   テストが失敗するのではなく、単に処理が遅くなるだけです。


.. method:: MultiFile.push(str)

   境界文字列をスタックにプッシュします。
   この境界文字列の修飾されたバージョンが入力行に見つかった場合、セクション分割子\
   または終了マーカであると解釈されます(どちらであるかは修飾に依存します。
   :rfc:`2045` を参照してください)。
   それ以降の全てのデータ読み出しは、 :meth:`pop` を呼んで境界文字列を除去するか、
   :meth:`.next` を呼んで境界文字列を再有効化しないかぎり、
   ファイル終端を示す空文字列を返します。

   一つ以上の境界をプッシュすることは可能です。
   もっとも最近プッシュされた境界に遭遇すると EOF が返ります;
   その他の境界に遭遇するとエラーが送出されます。


.. method:: MultiFile.pop()

   セクション境界をポップします。この境界はもはや EOF として解釈されません。


.. method:: MultiFile.section_divider(str)

   境界をセクション分割子にします。
   標準では、このメソッドは (全ての MIME 境界が持っている) ``'--'``
   を境界文字列の先頭に追加しますが、これは派生クラスで上書きできるように宣言されています。
   末尾の空白は無視されることから考えて、このメソッドでは
   LF や CR-LF を追加する必要はありません。


.. method:: MultiFile.end_marker(str)

   境界文字列を終了マーカ行にします。
   標準では、このメソッドは (MIME マルチパートデータのメッセージ終了マーカのように)
   ``'--'`` を境界文字列の先頭に追加し、かつ ``'--'`` を境界文字列の末尾に追加しますが、
   これは派生クラスで上書きできるように宣言されています。
   末尾の空白は無視されることから考えて、このメソッドでは
   LF や CR-LF を追加する必要はありません。

最後に、
:class:`MultiFile` インスタンスは二つの公開されたインスタンス変数を持っています:


.. attribute:: MultiFile.level

   現在のパートにおける入れ子の深さです。


.. attribute:: MultiFile.last

   最後に見つかったファイル終了イベントがメッセージ終了マーカであった場合に真となります。


.. _multifile-example:

:class:`MultiFile` の例
-----------------------

.. sectionauthor:: Skip Montanaro <skip@pobox.com>


::

   import mimetools
   import multifile
   import StringIO

   def extract_mime_part_matching(stream, mimetype):
       """Return the first element in a multipart MIME message on stream
       matching mimetype."""

       msg = mimetools.Message(stream)
       msgtype = msg.gettype()
       params = msg.getplist()

       data = StringIO.StringIO()
       if msgtype[:10] == "multipart/":

           file = multifile.MultiFile(stream)
           file.push(msg.getparam("boundary"))
           while file.next():
               submsg = mimetools.Message(file)
               try:
                   data = StringIO.StringIO()
                   mimetools.decode(file, data, submsg.getencoding())
               except ValueError:
                   continue
               if submsg.gettype() == mimetype:
                   break
           file.pop()
       return data.getvalue()

