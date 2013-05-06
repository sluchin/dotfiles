.. _tut-brieftourtwo:

**********************************
標準ライブラリミニツアー -- その 2
**********************************

2回目のツアーでは、プロフェッショナルプログラミングを支えるもっと高度な
モジュールをカバーします。ここで挙げるモジュールは、小さなスクリプトの
開発ではほとんど使いません。


.. _tut-output-formatting:

出力のフォーマット
==================

:mod:`repr` モジュールは、大きなコンテナや、深くネストしたコンテナを
省略して表示するバージョンの :func:`repr` を提供しています。

::

   >>> import repr
   >>> repr.repr(set('supercalifragilisticexpialidocious'))
   "set(['a', 'c', 'd', 'e', 'f', 'g', ...])"

:mod:`pprint` モジュールを使うと、組み込み型やユーザ定義型がより洗練された
形式で出力されるよう制御できます。出力が複数行にわたる場合には、
"pretty printer" が改行を追加して、入れ子構造を理解しやすいようにインデントを
挿入します。

::

   >>> import pprint
   >>> t = [[[['black', 'cyan'], 'white', ['green', 'red']], [['magenta',
   ...     'yellow'], 'blue']]]
   ...
   >>> pprint.pprint(t, width=30)
   [[[['black', 'cyan'],
      'white',
      ['green', 'red']],
     [['magenta', 'yellow'],
      'blue']]]

:mod:`textwrap` モジュールは、一段落の文を指定したスクリーン幅にぴったり
収まるように調整します。

::

   >>> import textwrap
   >>> doc = """The wrap() method is just like fill() except that it returns
   ... a list of strings instead of one big string with newlines to separate
   ... the wrapped lines."""
   ...
   >>> print textwrap.fill(doc, width=40)
   The wrap() method is just like fill()
   except that it returns a list of strings
   instead of one big string with newlines
   to separate the wrapped lines.

:mod:`locale` モジュールは、文化ごとに特化したデータ表現形式のデータベースに
アクセスします。 :mod:`locale` の :func:`format` 関数の grouping
属性を使えば、数値の各桁を適切な区切り文字でグループ化してフォーマットできます。

::

   >>> import locale
   >>> locale.setlocale(locale.LC_ALL, 'English_United States.1252')
   'English_United States.1252'
   >>> conv = locale.localeconv()          # get a mapping of conventions
   >>> x = 1234567.8
   >>> locale.format("%d", x, grouping=True)
   '1,234,567'
   >>> locale.format_string("%s%.*f", (conv['currency_symbol'],
   ...                      conv['frac_digits'], x), grouping=True)
   '$1,234,567.80'


.. _tut-templating:

文字列テンプレート
==================

:mod:`string` モジュールには、柔軟で、エンドユーザが簡単に編集できる簡単な
構文を備えた :class:`Template` クラスが入っています。このクラスを使うと、
ユーザがアプリケーションを修正することなしにアプリケーションの出力を
カスタマイズできるようになります。

テンプレートでは、 ``$`` と有効な Python 識別子名 (英数字とアンダースコア)
からなるプレースホルダ名を使います。プレースホルダの周りを丸括弧で囲えば、
間にスペースをはさまなくても後ろに英数文字を続けられます。
``$$`` のようにすると、 ``$`` 自体をエスケープできます。

::

   >>> from string import Template
   >>> t = Template('${village}folk send $$10 to $cause.')
   >>> t.substitute(village='Nottingham', cause='the ditch fund')
   'Nottinghamfolk send $10 to the ditch fund.'

:meth:`substitute` メソッドは、プレースホルダに相当する値が辞書やキーワード
引数にない場合に :exc:`KeyError` を送出します。
メールマージ型アプリケーションの場合、ユーザが入力するデータは不完全な
ことがあるので、欠落したデータがあるとプレースホルダをそのままにして出力する
:meth:`safe_substitute` メソッドを使う方が適切でしょう。

::

   >>> t = Template('Return the $item to $owner.')
   >>> d = dict(item='unladen swallow')
   >>> t.substitute(d)
   Traceback (most recent call last):
     . . .
   KeyError: 'owner'
   >>> t.safe_substitute(d)
   'Return the unladen swallow to $owner.'

:class:`Template` をサブクラス化すると、区切り文字を自作できます。例えば、
画像ブラウザ用にバッチで名前を変更するユーティリティを作っていたとして、
現在の日付や画像のシーケンス番号、ファイル形式といったプレースホルダに
パーセント記号を選んだとします。

::

   >>> import time, os.path
   >>> photofiles = ['img_1074.jpg', 'img_1076.jpg', 'img_1077.jpg']
   >>> class BatchRename(Template):
   ...     delimiter = '%'
   >>> fmt = raw_input('Enter rename style (%d-date %n-seqnum %f-format):  ')
   Enter rename style (%d-date %n-seqnum %f-format):  Ashley_%n%f

   >>> t = BatchRename(fmt)
   >>> date = time.strftime('%d%b%y')
   >>> for i, filename in enumerate(photofiles):
   ...     base, ext = os.path.splitext(filename)
   ...     newname = t.substitute(d=date, n=i, f=ext)
   ...     print '{0} --> {1}'.format(filename, newname)

   img_1074.jpg --> Ashley_0.jpg
   img_1076.jpg --> Ashley_1.jpg
   img_1077.jpg --> Ashley_2.jpg

テンプレートのもう一つの用途は、複数ある出力様式からのプログラムロジックの
分離です。テンプレートを使えば、カスタムのテンプレートを XML ファイル用や
平文テキストのレポート、 HTML で書かれた web レポート用などに置き換えられます。


.. _tut-binary-formats:

バイナリデータレコードの操作
============================

:mod:`struct` モジュールでは、可変長のバイナリレコード形式を操作する
:func:`pack` や :func:`unpack` といった関数を提供しています。
以下の例では、 :mod:`zipfile` モジュールを使わずに、ZIPファイルのヘッダ情報を
巡回する方法を示しています
``"H"``  と ``"I"`` というパック符号は、それぞれ2バイトと4バイトの符号無し
整数を表しています。
``"<"`` は、そのパック符号が通常のサイズであり、バイトオーダーが
リトルエンディアンであることを示しています。

::

   import struct

   data = open('myfile.zip', 'rb').read()
   start = 0
   for i in range(3):                      # 最初の3ファイルのヘッダを表示する
       start += 14
       fields = struct.unpack('<IIIHH', data[start:start+16])
       crc32, comp_size, uncomp_size, filenamesize, extra_size = fields

       start += 16
       filename = data[start:start+filenamesize]
       start += filenamesize
       extra = data[start:start+extra_size]
       print filename, hex(crc32), comp_size, uncomp_size

       start += extra_size + comp_size     # 次のヘッダまでスキップする。


.. _tut-multi-threading:

マルチスレッド処理
==================

スレッド処理 (threading) とは、順序的な依存関係にない複数のタスクを分割する
テクニックです。スレッドは、ユーザの入力を受け付けつつ、背後で別のタスクを
動かすようなアプリケーションの応答性を高めます。主なユースケースには、 I/O
を別のスレッドの計算処理と並列して動作させるというものがあります。

以下のコードでは、高水準のモジュール :mod:`threading` でメインのプログラムを
動かしながら背後で別のタスクを動作させられるようにする方法を示しています。

::

   import threading, zipfile

   class AsyncZip(threading.Thread):
       def __init__(self, infile, outfile):
           threading.Thread.__init__(self)
           self.infile = infile
           self.outfile = outfile
       def run(self):
           f = zipfile.ZipFile(self.outfile, 'w', zipfile.ZIP_DEFLATED)
           f.write(self.infile)
           f.close()
           print 'Finished background zip of: ', self.infile

   background = AsyncZip('mydata.txt', 'myarchive.zip')
   background.start()
   print 'The main program continues to run in foreground.'

   background.join()    # Wait for the background task to finish
   print 'Main program waited until background was done.'

マルチスレッドアプリケーションを作る上で最も難しい問題は、データやリソースを
共有するスレッド間の調整 (coordination)です。
この問題を解決するため、 :mod:`threading` モジュールではロックやイベント、
状態変数、セマフォといった数々の同期プリミティブを提供しています。

こうしたツールは強力な一方、ちょっとした設計上の欠陥で再現困難な問題を
引き起こすことがあります。
したがって、タスク間調整では :mod:`Queue` モジュールを使って他のスレッドから
一つのスレッドにリクエストを送り込み、一つのリソースへのアクセスをできるだけ
一つのスレッドに集中させるアプローチを勧めます。
スレッド間の通信や調整に :class:`Queue.Queue` オブジェクトを使うと、
設計が容易になり、可読性が高まり、信頼性が増します。


.. _tut-logging:

ログ記録
========

:mod:`logging` モジュールでは、数多くの機能をそなえた柔軟性のあるログ記録
システムを提供しています。最も簡単な使い方では、ログメッセージをファイルや
``sys.stderr`` に送信します。

::

   import logging
   logging.debug('Debugging information')
   logging.info('Informational message')
   logging.warning('Warning:config file %s not found', 'server.conf')
   logging.error('Error occurred')
   logging.critical('Critical error -- shutting down')

上記のコードは以下のような出力になります::

   WARNING:root:Warning:config file server.conf not found
   ERROR:root:Error occurred
   CRITICAL:root:Critical error -- shutting down

デフォルトでは、単なる情報やデバッグメッセージの出力は抑制され、出力は
標準エラーに送信されます。選択可能な送信先には、email、データグラム、ソケット、
HTTP サーバへの送信などがあります。新たにフィルタを作成すると、 :const:`DEBUG`,
:const:`INFO`, :const:`WARNING`, :const:`ERROR`, :const:`CRITICAL` といった
メッセージのプライオリティに従って配送先を変更できます。

ログ記録システムは Python から直接設定できますし、アプリケーションを変更
しなくてもカスタマイズできるよう、ユーザが編集できる設定ファイルでも
設定できます。


.. _tut-weak-references:

弱参照
======

Python は自動的にメモリを管理します (ほとんどのオブジェクトは参照カウント方式で
管理し、ガベージコレクション(:term:`garbage collection`)で循環参照を除去します)。
オブジェクトに対する最後の参照がなくなってしばらくするとメモリは解放されます。

このようなアプローチはほとんどのアプリケーションでうまく動作しますが、
中にはオブジェクトをどこか別の場所で利用している間だけ追跡しておきたい場合も
あります。
残念ながら、オブジェクトを追跡するだけでオブジェクトに対する恒久的な参照を作る
ことになってしまいます。
:mod:`weakref` モジュールでは、オブジェクトへの参照を作らずに追跡するための
ツールを提供しています。
弱参照オブジェクトが不要になると、弱参照 (weakref) テーブルから自動的に除去され、
コールバック関数がトリガされます。弱参照を使う典型的な応用例には、作成コストの
大きいオブジェクトのキャッシュがあります。 ::

   >>> import weakref, gc
   >>> class A:
   ...     def __init__(self, value):
   ...         self.value = value
   ...     def __repr__(self):
   ...         return str(self.value)
   ...
   >>> a = A(10)                   # 参照を作成する.
   >>> d = weakref.WeakValueDictionary()
   >>> d['primary'] = a            # 参照を作成しない.
   >>> d['primary']                # オブジェクトが生きていれば取得する.
   10
   >>> del a                       # 参照を1つ削除する.
   >>> gc.collect()                # ガベージコレクションを実行する.
   0
   >>> d['primary']                # エントリが自動的に削除されている.
   Traceback (most recent call last):
     File "<stdin>", line 1, in <module>
       d['primary']                # entry was automatically removed
     File "C:/python26/lib/weakref.py", line 46, in __getitem__
       o = self.data[key]()
   KeyError: 'primary'


.. _tut-list-tools:

リスト操作のためのツール
========================

多くのデータ構造は、組み込みリスト型を使った実装で事足ります。とはいえ、
時には組み込みリストとは違うパフォーマンス上のトレードオフを持つような
実装が必要になこともあります。

:mod:`array` モジュールでは、同じ形式のデータだけをコンパクトに保存できる、
リスト型に似た :class:`array()` オブジェクトを提供しています。
以下の例では、通常 1 要素あたり 16 バイトを必要とする Python 整数型のリストの
代りに、2 バイトの符号無しの 2 進数 (タイプコード ``"H"``)を使っている
数値配列を示します。

::

   >>> from array import array
   >>> a = array('H', [4000, 10, 700, 22222])
   >>> sum(a)
   26932
   >>> a[1:3]
   array('H', [10, 700])

:mod:`collections` モジュールでは、リスト型に似た :class:`deque()`
オブジェクトを提供しています。 :class:`deque()` オブジェクトでは、
データの追加と左端からの取り出しが高速な半面、中間にある値の検索が低速に
なります。こうしたオブジェクトはキューの実装や幅優先のツリー探索に
向いています。

::

   >>> from collections import deque
   >>> d = deque(["task1", "task2", "task3"])
   >>> d.append("task4")
   >>> print "Handling", d.popleft()
   Handling task1

   unsearched = deque([starting_node])
   def breadth_first_search(unsearched):
       node = unsearched.popleft()
       for m in gen_moves(node):
           if is_goal(m):
               return m
           unsearched.append(m)

リストの代わりの実装以外にも、標準ライブラリにはソート済みのリストを
操作するための関数を備えた :mod:`bisect` のようなツールも提供しています。

::

   >>> import bisect
   >>> scores = [(100, 'perl'), (200, 'tcl'), (400, 'lua'), (500, 'python')]
   >>> bisect.insort(scores, (300, 'ruby'))
   >>> scores
   [(100, 'perl'), (200, 'tcl'), (300, 'ruby'), (400, 'lua'), (500, 'python')]

:mod:`heapq` モジュールでは、通常のリストでヒープを実装するための関数を
提供しています。ヒープでは、最も低い値をもつエントリがつねにゼロの位置に
配置されます。ヒープは、毎回リストをソートすることなく、最小の値をもつ要素に
繰り返しアクセスするようなアプリケーションで便利です。

::

   >>> from heapq import heapify, heappop, heappush
   >>> data = [1, 3, 5, 7, 9, 2, 4, 6, 8, 0]
   >>> heapify(data)                      # rearrange the list into heap order
   >>> heappush(data, -5)                 # add a new entry
   >>> [heappop(data) for i in range(3)]  # fetch the three smallest entries
   [-5, 0, 1]


.. _tut-decimal-fp:

10 進浮動小数演算
=================

:mod:`decimal` では、 10 進浮動小数の算術演算をサポートする :class:`Decimal`
データ型を提供しています。組み込みの 2 進浮動小数の実装である :class:`float`
に比べて、このクラスがとりわけ便利なのは、

* 財務アプリケーションやその他の正確な10進表記が必要なアプリケーション、
* 精度の制御、
* 法的または規制上の理由に基づく値丸めの制御、
* 有効桁数の追跡が必要になる場合、
* ユーザが手計算の結果と同じ演算結果を期待するようなアプリケーション

の場合です。

例えば、 70 セントの電話代にかかる 5% の税金を計算しようとすると、
10 進の浮動小数点値と 2 進の浮動小数点値では違う結果になってしまいます。
計算結果を四捨五入してセント単位にしようとすると違いがはっきり現れます。

::

   >>> from decimal import *
   >>> x = Decimal('0.70') * Decimal('1.05')
   >>> x
   Decimal('0.7350')
   >>> x.quantize(Decimal('0.01'))  # 一番近い 1/100 単位にまとめる.
   Decimal('0.74')
   >>> round(.70 * 1.05, 2)         # 同じ計算を float ですると.
   0.73

:class:`Decimal` を使った計算では、末尾桁のゼロが保存されており、有効数字2桁の
被乗数から自動的に有効数字を 4 桁と判断しています。 :class:`Decimal` は手計算と
同じ方法で計算を行い、 2 進浮動小数点が 10 進小数成分を正確に表現できないことに
よって起きる問題を回避しています。

:class:`Decimal` クラスは厳密な値を表現できるため、2 進浮動小数点数では
期待通りに計算できないようなモジュロの計算や等値テストも実現できます。

::

   >>> Decimal('1.00') % Decimal('.10')
   Decimal('0.00')
   >>> 1.00 % 0.10
   0.09999999999999995

   >>> sum([Decimal('0.1')]*10) == Decimal('1.0')
   True
   >>> sum([0.1]*10) == 1.0
   False

:mod:`decimal` モジュールを使うと、必要なだけの精度で算術演算を行えます。

::

   >>> getcontext().prec = 36
   >>> Decimal(1) / Decimal(7)
   Decimal('0.142857142857142857142857142857142857')


