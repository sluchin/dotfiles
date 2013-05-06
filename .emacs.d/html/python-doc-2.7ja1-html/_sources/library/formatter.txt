
:mod:`formatter` --- 汎用の出力書式化機構
=========================================

.. module:: formatter
   :synopsis: 汎用の出力書式化機構およびデバイスインタフェース。


.. index:: single: HTMLParser (class in htmllib)

このモジュールでは、二つのインタフェース定義を提供しており、それらの各インタフェースについて複数の実装を提供しています。 *formatter*
インタフェースは :mod:`htmllib` モジュールの :class:`HTMLParser` クラスで使われており、 *writer*  インタフェースは
formatter インタフェースを使う上で必要です。

formatter オブジェクトはある抽象化された書式イベントの流れを writer オブジェクト上の特定の出力イベントに変換します。 formatter
はいくつかのスタック構造を管理することで、writer  オブジェクトの様々な属性を変更したり復元したりできるようにしています; このため、writer
は相対的な変更や "元に戻す" 操作を処理できなくてもかまいません。writer の特定のプロパティのうち、 formatter
オブジェクトを介して制御できるのは、水平方向の字揃え、フォント、そして左マージンの字下げです。任意の、非排他的なスタイル設定を writer
に提供するためのメカニズムも提供されています。さらに、段落分割のように、可逆でない書式化イベントの機能を提供するインタフェースもあります。

writer オブジェクトはデバイスインタフェースをカプセル化します。ファイル形式のような抽象デバイスも物理デバイス同様にサポートされて
います。ここで提供されている実装内容はすべて抽象デバイス上で動作します。デバイスインタフェースは formatter オブジェクトが
管理しているプロパティを設定し、データを出力端に書き込めるようにします。


.. _formatter-interface:

formatter インタフェース
------------------------

formatter を作成するためのインタフェースは、インスタンス化しようとする個々の formatter クラスに依存します。以下で解説するのは、
インスタンス化された全ての formatter がサポートしなければならないインタフェースです。

モジュールレベルではデータ要素を一つ定義しています:


.. data:: AS_IS

   後に述べる ``push_font()`` メソッドでフォント指定をする時に使える値です。また、その他の ``push_property()``
   メソッドの新しい値として使うことができます。

   ``AS_IS`` の値をスタックに置くと、どのプロパティが変更されたかの追跡を行わずに、対応する ``pop_property()`` メソッドが呼び
   出されるようになります。

formatter インスタンスオブジェクトには以下の属性が定義されています:


.. attribute:: formatter.writer

   formatter とやり取りを行う writer インスタンスです。


.. method:: formatter.end_paragraph(blanklines)

   開かれている段落があれば閉じ、次の段落との間に少なくとも *blanklines* が挿入されるようにします。


.. method:: formatter.add_line_break()

   強制改行挿入します。既に強制改行がある場合は挿入しません。論理的な段落は中断しません。


.. method:: formatter.add_hor_rule(*args, **kw)

   出力に水平罫線を挿入します。現在の段落に何らかのデータがある場合、強制改行が挿入されますが、論理的な段落は中断しません。引数とキーワードは writer の
   :meth:`send_line_break` メソッドに渡されます。


.. method:: formatter.add_flowing_data(data)

   空白を折りたたんで書式化しなければならないデータを提供します。空白の折りたたみでは、直前や直後の :meth:`add_flowing_data` 呼び出しに
   入っている空白も考慮されます。このメソッドに渡されたデータは出力デバイスで行末の折り返し (word-wrap) されるものと想定されて
   います。出力デバイスでの要求やフォント情報に応じて、writer オブジェクトでも何らかの行末折り返しが行われなければならないので注意してください。


.. method:: formatter.add_literal_data(data)

   変更を加えずに writer に渡さなければならないデータを提供します。改行およびタブを含む空白を *data* の値にしても問題ありません。


.. method:: formatter.add_label_data(format, counter)

   現在の左マージン位置の左側に配置されるラベルを挿入します。このラベルは箇条書き、数字つき箇条書きの書式を構築する際に使われます。 *format*
   の値が文字列の場合、整数の値 *counter* の書式指定として解釈されます。

   *format* の値が文字列の場合、整数の値をとる *counter* の書式化指定として解釈されます。書式化された文字列はラベルの値になります;
   *format* が文字列でない場合、ラベルの値として直接使われます。ラベルの値は writer の :meth:`send_label_data`
   メソッドの唯一の引数として渡されます。非文字列のラベル値をどう解釈するかは関連付けられた writer に依存します。

   書式化指定は文字列からなり、 counter の値と合わせてラベルの値を算出するために使われます。書式文字列の各文字はラベル値にコピーされます。
   このときいくつかの文字は counter 値を変換を指すものとして認識されます。特に、文字 ``'1'`` はアラビア数字の counter 値を表し、
   ``'A'`` と ``'a'`` はそれぞれ大文字および小文字のアルファベットによる counter 値を表し、 ``'I'`` と ``'i'``
   はそれぞれ大文字および小文字のローマ数字による counter 値を表します。アルファベットおよびローマ字数字への変換の際には、counter の
   値はゼロ以上である必要があるので注意してください。


.. method:: formatter.flush_softspace()

   以前の :meth:`add_flowing_data` 呼び出しでバッファされている出力待ちの空白を、関連付けられている writer オブジェクトに送信
   します。このメソッドは writer オブジェクトに対するあらゆる直接操作の前に呼び出さなければなりません。


.. method:: formatter.push_alignment(align)

   新たな字揃え (alignment) 設定を字揃えスタックの上にプッシュします。変更を行いたくない場合には :const:`AS_IS`
   にすることができます。字揃え設定値が以前の設定から変更された場合、writer の  :meth:`new_alignment` メソッドが *align*
   の値と共に呼び出されます。


.. method:: formatter.pop_alignment()

   以前の字揃え設定を復元します。


.. method:: formatter.push_font((size, italic, bold, teletype))

   writer オブジェクトのフォントプロパティのうち、一部または全てを変更します。 :const:`AS_IS`
   に設定されていないプロパティは引数で渡された値に設定され、その他の値は現在の設定を維持します。writer の :meth:`new_font`
   メソッドは完全に設定解決されたフォント指定で呼び出されます。


.. method:: formatter.pop_font()

   以前のフォント設定を復元します。


.. method:: formatter.push_margin(margin)

   左マージンのインデント数を一つ増やし、論理タグ *margin* を新たなインデントに関連付けます。マージンレベルの初期値は ``0``
   です。変更された論理タグの値は真値とならなければなりません;  :const:`AS_IS` 以外の偽の値はマージンの変更としては不適切です。


.. method:: formatter.pop_margin()

   以前のマージン設定を復元します。


.. method:: formatter.push_style(*styles)

   任意のスタイル指定をスタックにプッシュします。全てのスタイルはスタイルスタックに順番にプッシュされます。 :const:`AS_IS` 値を含み、
   スタック全体を表すタプルは writer の :meth:`new_styles` メソッドに渡されます。


.. method:: formatter.pop_style([n=1])

   :meth:`push_style` に渡された最新 *n* 個のスタイル指定をポップします。 :const:`AS_IS` 値を含み、変更されたスタックを表す
   タプルは writer の :meth:`new_styles` メソッドに渡されます。


.. method:: formatter.set_spacing(spacing)

   writer の割り付けスタイル (spacing style) を設定します。


.. method:: formatter.assert_line_data([flag=1])

   現在の段落にデータが予期せず追加されたことを formatter に知らせます。このメソッドは writer を直接操作した際に使わなければなりません。
   writer 操作の結果、出力の末尾が強制改行となった場合、オプションの *flag* 引数を偽に設定することができます。


.. _formatter-impls:

formatter 実装
--------------

このモジュールでは、formatter オブジェクトに関して二つの実装を提供しています。ほとんどのアプリケーションではこれらのクラスを
変更したりサブクラス化することなく使うことができます。


.. class:: NullFormatter([writer])

   何も行わない formatter です。 *writer* を省略すると、 :class:`NullWriter` インスタンスが生成されます。
   :class:`NullFormatter` インスタンスは、writer のメソッドを全く呼び出しません。writer へのインタフェースを実装する場合には
   このクラスのインタフェースを継承する必要がありますが、実装を継承する必要は全くありません。


.. class:: AbstractFormatter(writer)

   標準の formatter です。この formatter 実装は広範な writer で適用できることが実証されており、ほとんどの状況で直接使うことが
   できます。高機能の WWW ブラウザを実装するために使われたこともあります。


.. _writer-interface:

writer インタフェース
---------------------

writer を作成するためのインタフェースは、インスタンス化しようとする個々の writer クラスに依存します。以下で解説するのは、
インスタンス化された全ての writer がサポートしなければならないインタフェースです。ほとんどのアプリケーションでは
:class:`AbstractFormatter` クラスを formatter として使うことができますが、通常 writer はアプリケーション
側で与えなければならないので注意してください。


.. method:: writer.flush()

   バッファに蓄積されている出力データやデバイス制御イベントをフラッシュします。


.. method:: writer.new_alignment(align)

   字揃えのスタイルを設定します。 *align* の値は任意のオブジェクトを取りえますが、慣習的な値は文字列または ``None`` で、 ``None`` は
   writer の "好む" 字揃えを使うことを表します。慣習的な *align* の値は ``'left'`` 、 ``'center'`` 、
   ``'right'`` 、および ``'justify'`` です。


.. method:: writer.new_font(font)

   フォントスタイルを設定します。 *font* は、デバイスの標準のフォントが使われることを示す ``None`` か、 ``(size, italic, bold, teletype)`` の形式をとるタプルになります。size はフォントサイズを示す文字列になります;
   特定の文字列やその解釈はアプリケーション側で定義します。 *italic* 、 *bold* 、および *teletype* といった値は
   ブール値で、それらの属性を使うかどうかを指定します。


.. method:: writer.new_margin(margin, level)

   マージンレベルを整数値 *level* に設定し、論理タグ (logical tag) を *margin* に設定します。論理タグの解釈は writer
   の判断に任されます; 論理タグの値に対する唯一の制限は *level* が非ゼロの値の際に偽であってはならないということです。


.. method:: writer.new_spacing(spacing)

   割り付けスタイル (spacing style) を *spacing* に設定します。

   .. Set the spacing style to *spacing*.


.. method:: writer.new_styles(styles)

   追加のスタイルを設定します。 *styles* の値は任意の値からなるタプルです; :const:`AS_IS` 値は無視されます。 *styles*
   タプルはアプリケーションや writer の実装上の都合により、集合としても、スタックとしても解釈され得ます。


.. method:: writer.send_line_break()

   現在の行を改行します。


.. method:: writer.send_paragraph(blankline)

   少なくとも *blankline* 空行分の間隔か、空行そのもので段落を分割します。 *blankline* の値は整数になります。 writer
   の実装では、改行を行う必要がある場合、このメソッドの呼び出しに先立って :meth:`send_line_break` の呼び出しを受ける必要あります;
   このメソッドには段落の最後の行を閉じる機能は含まれておらず、段落間に垂直スペースを空ける役割しかありません。


.. method:: writer.send_hor_rule(*args, **kw)

   水平罫線を出力デバイスに表示します。このメソッドへの引数は全てアプリケーションおよび writer 特有のものなので、注意して
   解釈する必要があります。このメソッドの実装では、すでに改行が :meth:`send_line_break` によってなされているものと仮定しています。


.. method:: writer.send_flowing_data(data)

   行端が折り返され、必要に応じて再割り付け解析を行った (re-flowed)  文字データを出力します。このメソッドを連続して呼び出す上では、 writer
   は複数の空白文字は単一のスペース文字に縮約されていると仮定することがあります。


.. method:: writer.send_literal_data(data)

   すでに表示用に書式化された文字データを出力します。これは通常、改行文字で表された改行を保存し、新たに改行を持ち込まないことを意味します。
   :meth:`send_formatted_data` インタフェースと違って、データには改行やタブ文字が埋め込まれていてもかまいません。


.. method:: writer.send_label_data(data)

   可能ならば、 *data* を現在の左マージンの左側に設定します。 *data* の値には制限がありません; 文字列でない値の扱い方はアプリケーションや
   writer に完全に依存します。このメソッドは行の先頭でのみ呼び出されます。


.. _writer-impls:

writer 実装
-----------

このモジュールでは、3 種類の writer オブジェクトインタフェース実装を提供しています。ほとんどのアプリケーションでは、
:class:`NullWriter` から新しい writer クラスを派生する必要があるでしょう。


.. class:: NullWriter()

   インタフェース定義だけを提供する writer クラスです; どのメソッドも何ら処理を行いません。このクラスは、メソッド実装をまったく継承する必要のない
   writer 全ての基底クラスになります。


.. class:: AbstractWriter()

   この writer は formatter をデバッグするのに利用できますが、それ以外に利用できるほどのものではありません。各メソッドを呼び出すと、
   メソッド名と引数を標準出力に印字して呼び出されたことを示します。


.. class:: DumbWriter([file[, maxcol=72]])

   単純な writer クラスで *file* に渡されたファイルオブジェクトか *file* が省略された場合には標準出力に出力を書き込みます。出力は
   *maxcol* で指定されたカラム数で単純な行端折り返しが行われます。このクラスは連続した段落を再割り付けするのに適しています。

