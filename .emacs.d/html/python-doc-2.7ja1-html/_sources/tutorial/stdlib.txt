.. _tut-brieftour:

************************
標準ライブラリミニツアー
************************


.. _tut-os-interface:

OSへのインタフェース
======================

:mod:`os` モジュールは、オペレーティングシステムと対話するための
何ダースもの関数を提供しています。

::

   >>> import os
   >>> os.getcwd()      # 現在の作業ディレクトリを返す
   'C:\\Python26'
   >>> os.chdir('/server/accesslogs')   # 作業ディレクトリを変更する.
   >>> os.system('mkdir today')   # システムのシェルで mkdir コマンドを実行する
   0

``from os import *`` ではなく、 ``import os`` 形式を使うようにしてください。
そうすることで、動作が大きく異なる組み込み関数 :func:`open` が :func:`os.open`
で隠蔽されるのを避けられます。

.. index:: builtin: help

組み込み関数 :func:`dir` および :func:`help` は、 :mod:`os` のような大規模な
モジュールで作業をするときに、対話的な操作上の助けになります。

::

   >>> import os
   >>> dir(os)
   <モジュール内の関数全てを含むリストを返す>
   >>> help(os)
   <モジュールの docstring から作られた広範囲に渡るマニュアルページを返す>

ファイルやディレクトリの日常的な管理作業のために、より簡単に使える高レベル
インタフェースが :mod:`shutil` モジュールで提供されています。

::

   >>> import shutil
   >>> shutil.copyfile('data.db', 'archive.db')
   >>> shutil.move('/build/executables', 'installdir')


.. _tut-file-wildcards:

ファイルのワイルドカード表記
============================

:mod:`glob` モジュールでは、
ディレクトリのワイルドカード検索からファイルのリストを生成するための
関数を提供しています。

::

   >>> import glob
   >>> glob.glob('*.py')
   ['primes.py', 'random.py', 'quote.py']


.. _tut-command-line-arguments:

コマンドライン引数
==================

一般的なユーティリティスクリプトでは、よくコマンドライン引数を扱う必要が
あります。コマンドライン引数は :mod:`sys` モジュールの *argv*  属性に
リストとして保存されています。
例えば、以下の出力は、 ``python demo.py one two three`` とコマンドライン上で
起動した時に得られるものです。

::

   >>> import sys
   >>> print sys.argv
   ['demo.py', 'one', 'two', 'three']

:mod:`getopt` モジュールは、 *sys.argv* を Unix の :func:`getopt` 関数の慣習に
従って処理します。より強力で柔軟性のあるコマンドライン処理機能は、
:mod:`argparse` モジュールで提供されています。


.. _tut-stderr:

エラー出力のリダイレクトとプログラムの終了
==========================================

:mod:`sys` モジュールには、 *stdin*, *stdout*, *stderr* を表す属性も存在します。
*stderr* は、警告やエラーメッセージを出力して、 *stdout* がリダイレクトされた
場合でも読めるようにするために便利です。

::

   >>> sys.stderr.write('Warning, log file not found starting a new one\n')
   Warning, log file not found starting a new one

``sys.exit()`` は、スクリプトを終了させるもっとも直接的な方法です。


.. _tut-string-pattern-matching:

文字列のパターンマッチング
==========================

:mod:`re` モジュールでは、より高度な文字列処理のための正規表現を提供しています。
正規表現は複雑な一致検索や操作に対して簡潔で最適化された解決策を提供します。

::

   >>> import re
   >>> re.findall(r'\bf[a-z]*', 'which foot or hand fell fastest')
   ['foot', 'fell', 'fastest']
   >>> re.sub(r'(\b[a-z]+) \1', r'\1', 'cat in the the hat')
   'cat in the hat'

最小限の機能だけが必要なら、読みやすくデバッグしやすい文字列メソッドの方が
お勧めです。

::

   >>> 'tea for too'.replace('too', 'two')
   'tea for two'


.. _tut-mathematics:

数学
====

:mod:`math` モジュールは、浮動小数点演算のための C 言語ライブラリ関数に
アクセスする手段を提供しています。

::

   >>> import math
   >>> math.cos(math.pi / 4.0)
   0.70710678118654757
   >>> math.log(1024, 2)
   10.0

:mod:`random` モジュールは、乱数に基づいた要素選択のためのツールを
提供しています。

::

   >>> import random
   >>> random.choice(['apple', 'pear', 'banana'])
   'apple'
   >>> random.sample(xrange(100), 10)   # 要素を戻さないサンプリング
   [30, 83, 16, 4, 8, 81, 41, 50, 18, 33]
   >>> random.random()    # ランダムな浮動小数点数
   0.17970987693706186
   >>> random.randrange(6)    # range(6) からランダムに選ばれた整数
   4


.. _tut-internet-access:

インターネットへのアクセス
==========================

インターネットにアクセスしたり、インターネットプロトコルを処理したりするための
数多くのモジュールがあります。その中でも特にシンプルなモジュールとして、URL
を指定してデータを取得するための :mod:`urllib2` と、メールを送信するための
:mod:`smtplib` があります。

::

   >>> import urllib2
   >>> for line in urllib2.urlopen('http://tycho.usno.navy.mil/cgi-bin/timer.pl'):
   ...     if 'EST' in line or 'EDT' in line:      # EST(東部標準時)を見る
   ...         print line

   <BR>Nov. 25, 09:43:32 PM EST

   >>> import smtplib
   >>> server = smtplib.SMTP('localhost')
   >>> server.sendmail('soothsayer@example.org', 'jcaesar@example.org',
   ... """To: jcaesar@example.org
   ... From: soothsayer@example.org
   ...
   ... Beware the Ides of March.
   ... """)
   >>> server.quit()

(2つ目の例は localhost でメールサーバーが動いている必要があることに注意して
ください。)


.. _tut-dates-and-times:

日付と時刻
==========

:mod:`datetime` モジュールは、日付や時刻を操作するためのクラスを、単純な方法と
複雑な方法の両方で提供しています。日付や時刻に対する算術がサポートされている一方、
実装では出力の書式化や操作のための効率的なデータメンバ抽出に重点を置いています。
このモジュールでは、タイムゾーンに対応したオブジェクトもサポートしています。

::

   >>> # 日付は簡単に生成して書式化することができます。
   >>> from datetime import date
   >>> now = date.today()
   >>> now
   datetime.date(2003, 12, 2)
   >>> now.strftime("%m-%d-%y. %d %b %Y is a %A on the %d day of %B")
   '12-02-03. 02 Dec 2003 is a Tuesday on the 02 day of December'

   >>> # date 型はカレンダー計算をサポートしています。
   >>> birthday = date(1964, 7, 31)
   >>> age = now - birthday
   >>> age.days
   14368


.. _tut-data-compression:

データ圧縮
==========

データの書庫化や圧縮で広く使われている形式については、 :mod:`zlib`, :mod:`gzip`,
:mod:`bz2`, :mod:`zipfile`, :mod:`tarfile` といったモジュールで直接サポートして
います。

::

   >>> import zlib
   >>> s = 'witch which has which witches wrist watch'
   >>> len(s)
   41
   >>> t = zlib.compress(s)
   >>> len(t)
   37
   >>> zlib.decompress(t)
   'witch which has which witches wrist watch'
   >>> zlib.crc32(s)
   226805979


.. _tut-performance-measurement:

パフォーマンスの計測
====================

Python ユーザの中には、同じ問題を異なったアプローチで解いた際の相対的な
パフォーマンスについて知りたいという深い興味を持っている人がいます。
Python は、そういった疑問に即座に答える計測ツールを提供しています。

例えば、引数の入れ替え操作に対して、伝統的なアプローチの代わりにタプルの
パックやアンパックを使ってみたいと思うかもしれません。
:mod:`timeit` モジュールを使えば、パフォーマンスがほんの少し良いことが
すぐに分かります。

::

   >>> from timeit import Timer
   >>> Timer('t=a; a=b; b=t', 'a=1; b=2').timeit()
   0.57535828626024577
   >>> Timer('a,b = b,a', 'a=1; b=2').timeit()
   0.54962537085770791

:mod:`timeit` では小さい粒度を提供しているのに対し、 :mod:`profile` や
:mod:`pstats`  モジュールではより大きなコードブロックにおいて律速となる部分を
判定するためのツールを提供しています。


.. _tut-quality-control:

品質管理
========

高い品質のソフトウェアを開発するための一つのアプローチは、各関数に対して開発と
同時にテストを書き、開発の過程で頻繁にテストを走らせるというものです。

:mod:`doctest` モジュールでは、モジュールを検索してプログラムの docstring に
埋め込まれたテストの評価を行うためのツールを提供しています。
テストの作り方は単純で、典型的な呼び出し例とその結果を docstring に
カット&ペーストするだけです。この作業は、ユーザに使用例を与えるという意味で
ドキュメントの情報を増やすと同時に、ドキュメントに書かれているコードが正しい
事を確認できるようになります。

::

   def average(values):
       """Computes the arithmetic mean of a list of numbers.

       >>> print average([20, 30, 70])
       40.0
       """
       return sum(values, 0.0) / len(values)

   import doctest
   doctest.testmod()   # 組み込まれたテストを自動的に検証する。

:mod:`unittest` モジュールは :mod:`doctest` モジュールほど気楽に使えるものでは
ありませんが、より網羅的なテストセットを別のファイルで管理することができます。

::

   import unittest

   class TestStatisticalFunctions(unittest.TestCase):

       def test_average(self):
           self.assertEqual(average([20, 30, 70]), 40.0)
           self.assertEqual(round(average([1, 5, 7]), 1), 4.3)
           self.assertRaises(ZeroDivisionError, average, [])
           self.assertRaises(TypeError, average, 20, 30, 70)

   unittest.main() # コマンドラインから呼び出すと全てのテストを実行する。


.. _tut-batteries-included:

バッテリー同梱
==============

Python には "バッテリー同梱 (batteries included)" 哲学があります。この哲学は、
洗練され、安定した機能を持つ Python の膨大なパッケージ群に如実に表れています。
例えば、


* The :mod:`xmlrpclib`  および :mod:`SimpleXMLRPCServer` モジュールは、
  遠隔手続き呼び出し (remote procedure call) を全く大したことのない作業に
  変えてしまいます。モジュール名とは違い、XML を扱うための直接的な知識は
  必要ありません。

* The :mod:`email`   パッケージは、MIME やその他の RFC 2822 に基づく
  メッセージ文書を含む電子メールメッセージを管理するためのライブラリです。
  実際にメッセージを送信したり受信したりする :mod:`smtplib` や :mod:`poplib`
  と違って、email パッケージには (添付文書を含む) 複雑なメッセージ構造の構築や
  デコードを行ったり、インターネット標準のエンコードやヘッダプロトコルの実装を
  行ったりするための完全なツールセットを備えています。

* :mod:`xml.dom` および :mod:`xml.sax` パッケージでは、一般的なデータ交換形式
  である XML を解析するための頑健なサポートを提供しています。同様に、
  :mod:`csv` モジュールでは、広く用いられているデータベース形式のデータを
  直接読み書きする機能をサポートしています。これらのモジュールやパッケージを
  利用することで、Python アプリケーションと他のツール群との間でのデータ交換が
  劇的に簡単になります。

* 国際化に関する機能は、 :mod:`gettext`, :mod:`locale`, :mod:`codecs`
  パッケージといったモジュール群でサポートされています。


