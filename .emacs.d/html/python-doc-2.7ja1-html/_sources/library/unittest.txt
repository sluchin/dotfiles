
:mod:`unittest` --- ユニットテストフレームワーク
================================================

.. module:: unittest
   :synopsis: ユニットテストフレームワーク
.. moduleauthor:: Steve Purcell <stephen_purcell@yahoo.com>
.. sectionauthor:: Steve Purcell <stephen_purcell@yahoo.com>
.. sectionauthor:: Fred L. Drake, Jr. <fdrake@acm.org>
.. sectionauthor:: Raymond Hettinger <python@rcn.com>

.. versionadded:: 2.1

(読者の方がすでにテストの基本概念についてなじみがあるようでしたら、
この部分をとばして :ref:`the list of assert methods <assert-methods>` に進むと良いでしょう。)

この Python ユニットテストフレームワークは時に "PyUnit" とも呼ばれ、
Kent Beck と Erich Gamma による JUnit の Python 版です。
JUnit はまた Kent の Smalltalk 用テストフレームワークの Java 版で、
どちらもそれぞれの言語で業界標準のユニットテストフレームワークとなっています。

:mod:`unittest` では、テストの自動化・初期設定と終了処理の共有・テスト
の分類・テスト実行と結果レポートの分離などの機能を提供しており、
:mod:`unittest` のクラスを使って簡単にたくさんのテストを開発できるよ
うになっています。

このようなことを実現するために :mod:`unittest` では、テストを以下のよ
うな構成で開発します。

test fixture (テストフィクスチャー)
   :dfn:`test fixture` とは、テスト実行のために必要な準備や終了処理を
   指します。例: テスト用データベースの作成・ディレクトリ・サーバプロ
   セスの起動など。

test case (テストケース)
   :dfn:`test case` はテストの最小単位で、各入力に対する結果をチェック
   します。テストケースを作成する場合は、 :mod:`unittest` が提供する
   :class:`TestCase` クラスを基底クラスとして利用することができます。

test suite (テストスイート)
   :dfn:`test suite` はテストケースとテストスイートの集まりで、同時に
   実行しなければならないテストをまとめる場合に使用します。

test runner (テストランナー)
   :dfn:`test runner` はテストの実行と結果表示を管理するコンポーネント
   です。ランナーはグラフィカルインターフェースでもテキストインターフェー
   スでも良いですし、何も表示せずにテスト結果を示す値を返すだけの場合
   もあります。

:mod:`unittest` では、テストケースとテストフィクスチャーを、
:class:`TestCase` クラスと :class:`FunctionTestCase` クラスで提供して
います。 :class:`TestCase` クラスは新規にテストを作成する場合に使用し、
:class:`FunctionTestCase` は既存のテストを :mod:`unittest` に組み込む
場合に使用します。テストフィクスチャーの設定処理と終了処理は、
:class:`TestCase` では :meth:`~TestCase.setUp` メソッドと :meth:`~TestCase.tearDown`
をオーバーライドして記述し、 :class:`FunctionTestCase` では初期設定・終了処
理を行う既存の関数をコンストラクタで指定します。テスト実行時、まずテス
トフィクスチャーの初期設定が最初に実行されます。初期設定が正常終了した
場合、テスト実行後にはテスト結果に関わらず終了処理が実行されます。
:class:`TestCase` の各インスタンスが実行するテストは一つだけで、テスト
フィクスチャーは各テストごとに新しく作成されます。

テストスイートは :class:`TestSuite` クラスで実装されており、複数のテス
トとテストスイートをまとめる事ができます。テストスイートを実行すると、
スイートと子スイートに追加されている全てのテストが実行されます。

テストランナーは :meth:`~TestRunner.run` メソッドを持つオブジェクトです。
このメソッドは引数として :class:`TestCase` か :class:`TestSuite` オブ
ジェクトを受け取り、テスト結果を :class:`TestResult` オブジェクトで戻
します。 :mod:`unittest` ではデフォルトでテスト結果を標準エラーに出力
する :class:`TextTestRunner` をサンプルとして実装しています。これ以外
のランナー (グラフィックインターフェース用など) を実装する場合でも、
特別なクラスから派生させて実装する必要はありません。


.. seealso::

   Module :mod:`doctest`
      もうひとつのテストをサポートするモジュールで、本モジュールと趣きが異なっています。

   `unittest2: ユニットテストの新機能の Python 2.4-2.6 向けバックポート <http://pypi.python.org/pypi/unittest2>`_
      Python 2.7 になり多くの機能が unittest に追加されました。特に、
      テストディスカバリが追加されました。 unittest2 を導入する事で
      以前のバージョンの Python でもこれらの機能を使えます。

   `Simple Smalltalk Testing: With Patterns <http://www.XProgramming.com/testfram.htm>`_
      Kent Beck のテスティングフレームワークに関する原論文で、ここに記載されたパターンを
      :mod:`unittest` が使用しています。

   `Nose <http://code.google.com/p/python-nose/>`_ と `py.test <http://pytest.org>`_
      サードパーティのユニットテストフレームワークで軽量な文法でテストを書くことができます。
      例えば、 ``assert func(10) == 42``  のように書きます。

   `The Python Testing Tools Taxonomy <http://pycheesecake.org/wiki/PythonTestingToolsTaxonomy>`_
      多くの Python のテストツールが一覧で紹介されています。
      ファンクショナルテストのフレームワークやモックライブラリも掲載されています。

   `Testing in Python Mailing List <http://lists.idyll.org/listinfo/testing-in-python>`_
      Python でテストやテストツールについての議論に特化したグループです。


.. _unittest-minimal-example:

基礎的な例
----------

:mod:`unittest` モジュールには、テストの開発や実行の為の優れたツールが
用意されており、この節では、その一部を紹介します。ほとんどのユーザとっ
ては、ここで紹介するツールだけで十分でしょう。

以下は、 :mod:`random` モジュールの三つの関数をテストするスクリプトです。::

   import random
   import unittest

   class TestSequenceFunctions(unittest.TestCase):

       def setUp(self):
           self.seq = range(10)

       def test_shuffle(self):
           # make sure the shuffled sequence does not lose any elements
           random.shuffle(self.seq)
           self.seq.sort()
           self.assertEqual(self.seq, range(10))

           # should raise an exception for an immutable sequence
           self.assertRaises(TypeError, random.shuffle, (1,2,3))

       def test_choice(self):
           element = random.choice(self.seq)
           self.assertTrue(element in self.seq)

       def test_sample(self):
           with self.assertRaises(ValueError):
               random.sample(self.seq, 20)
           for element in random.sample(self.seq, 5):
               self.assertTrue(element in self.seq)

   if __name__ == '__main__':
       unittest.main()

テストケースは、 :class:`unittest.TestCase` のサブクラスとして作成しま
す。メソッド名が ``test`` で始まる三つのメソッドがテストです。テストラ
ンナーはこの命名規約によってテストを行うメソッドを検索します。

これらのテスト内では、予定の結果が得られていることを確かめるために
:meth:`~TestCase.assertEqual` を、条件のチェックに :meth:`~TestCase.assertTrue` を、
例外が発生する事を確認するために :meth:`~TestCase.assertRaises` を
それぞれ呼び出しています。 :keyword:`assert` 文の代わりにこれらのメソッドを使用すると、
テストランナーでテスト結果を集計してレポートを作成する事ができます。

:meth:`~TestCase.setUp` メソッドが定義されている場合、テストランナーは各テストを
実行する前に :meth:`~TestCase.setUp` メソッドを呼び出します。同様に、
:meth:`~TestCase.tearDown` メソッドが定義されている場合は各テストの実行後に呼び
出します。上のサンプルでは、それぞれのテスト用に新しいシーケンスを作成
するために :meth:`~TestCase.setUp` を使用しています。

サンプルの末尾が、簡単なテストの実行方法です。 :func:`unittest.main`
は、テストスクリプトのコマンドライン用インターフェースです。コマンドラ
インから起動された場合、上記のスクリプトから以下のような結果が出力され
ます::

   ...
   ----------------------------------------------------------------------
   Ran 3 tests in 0.000s

   OK

簡略化した結果を出力したり、コマンドライン以外からも起動する等のより細かい
制御が必要であれば、 :func:`unittest.main` を使用せずに別の方法でテス
トを実行します。例えば、上記サンプルの最後の2行は以下のように書くこと
ができます::

   suite = unittest.TestLoader().loadTestsFromTestCase(TestSequenceFunctions)
   unittest.TextTestRunner(verbosity=2).run(suite)

変更後のスクリプトをインタープリタや別のスクリプトから実行すると、以下
の出力が得られます::

   test_choice (__main__.TestSequenceFunctions) ... ok
   test_sample (__main__.TestSequenceFunctions) ... ok
   test_shuffle (__main__.TestSequenceFunctions) ... ok

   ----------------------------------------------------------------------
   Ran 3 tests in 0.110s

   OK

以上が :mod:`unittest` モジュールでよく使われる機能で、ほとんどのテス
トではこれだけでも十分です。基礎となる概念や全ての機能については以降の
章を参照してください。


.. _unittest-command-line-interface:

コマンドラインインタフェース
----------------------------

ユニットテストモジュールはコマンドラインから使うこともできます。
モジュール、クラス、もしくは、特定のテストメソッドで定義されたテストを実行します。::

   python -m unittest test_module1 test_module2
   python -m unittest test_module.TestClass
   python -m unittest test_module.TestClass.test_method

引数として渡す事ができるのは、テストが定義されたモジュール名、
もしくはクラス、メソッドのフルパス名です。

テスト実行時に（冗長な）詳細を表示するには -f フラグを渡します。::

   python -m unittest -v test_module

コマンドラインプションの一覧を表示するには以下のコマンドを実行します。::

   python -m unittest -h

..  versionchanged:: 2.7
   以前のバージョンでは、特定のメソッドでしか実行できず、
   モジュールやクラスは指定できませんでした。


コマンドラインオプション
~~~~~~~~~~~~~~~~~~~~~~~~

:program:`unittest` には以下のコマンドラインオプションがあります:

.. program:: unittest

.. cmdoption:: -b, --buffer

   標準出力と標準エラーのストリームをテスト実行の間バッファリングします。
   テストが成功している間は結果の出力は破棄されます。
   テストが失敗、もしくはエラーが発生した場合には、
   結果にエラーメッセージが追加されたうえで通常通り出力されます。

.. cmdoption:: -c, --catch

   control-C を実行中のテストが終了するまで遅延させ、そこまでの結果を出力します。
   二回目の control-C は、通常通り :exc:`KeyboardInterrupt`
   の例外を発生させます。

   この機能の仕組みについては、 `Signal Handling`_ を参照してください。

.. cmdoption:: -f, --failfast

   初回のエラーもしくは失敗の時にテストを停止します。

.. versionadded:: 2.7
   コマンドラインオプションの ``-b``, ``-c`` および ``-f`` が追加されました。

このコマンドラインは、プロジェクト内の全テストを実行したり、
サブセットのみを実行したりといった、テストディスカバリを使用することもできます。


.. _unittest-test-discovery:

テストディスカバリ
------------------

.. versionadded:: 2.7

unittest はシンプルなテストディスカバリをサポートします。
このテストディスカバリに対応するために、テストが定義された全ファイルは
:ref:`modules <tut-modules>` もしくは :ref:`packages <tut-packages>` として
プロジェクトの最上位のディスカバリでインポート可能である必要があります。
（つまり、これらのファイルは :ref:`identifiers <identifiers>` として有効で
ある必要があるということです。）

テストディスカバリは :meth:`TestLoader.discover` で実装されています。
しかし、コマンドラインからも使う事ができます。コマンドラインからは以下のように使用します。::

   cd project_directory
   python -m unittest discover

``discover`` サブコマンドには以下のオプションがあります。

.. program:: unittest discover

.. cmdoption:: -v, --verbose

   詳細な出よr区

.. cmdoption:: -s directory

   ディスカバリを開始するディレクトリ （デフォルトは '.'）

.. cmdoption:: -p pattern

   テストファイル名を識別するパターン （デフォルトは 'test*.py'）

.. cmdoption:: -t directory

   プロジェクトの最上位のディスカバリのディレクトリ （デフォルトは開始のディレクトリ）

The :option:`-s`, :option:`-p`, and :option:`-t` options can be passed in
as positional arguments in that order. The following two command lines
are equivalent::

:option:`-s` 、 :option:`-p` 、および :option:`-t` の各オプションは、
この順番で指定すれば位置固定の引数として指定する事ができます。
以下の二つのコマンドは同じ結果になります。::

   python -m unittest discover -s project_directory -p '*_test.py'
   python -m unittest discover project_directory '*_test.py'

パスを渡すのはもちろんのこと、例えば ``myproject.subpackage.test`` のように、
パッケージ名をスタートディレクトリとして渡すことができます。
指定したパッケージがインポートされ、そのパッケージのファイルシステム上のパスが
スタートディレクトリになります。

.. caution::

    テストディスカバリはテストをインポートすることで読み込みます。
    テストディスカバリは一度、指定した開始ディレクトリから全テストファイルを探索し、
    そのファイルのパスをパッケージ名に変換してインポートします。
    例えば、 `foo/bar/baz.py` は ``foo.bar.baz`` としてインポートされます。

    もしパッケージをグローバルにインストールしていて、
    インストールしたのとは異なるパッケージのコピーをディスカバリしようとすると、
    間違った場所からインポートして *しまうかもしれません* 。
    このような状態になるとテストディスカバリは警告を出し、停止します。

    スタートディレクトリとしてディレクトリのパスではなく
    パッケージ名を指定した場合は、いずれかの場所からインポートされます。
    この場合は警告が表示されません。

テストモジュールとテストパッケージは、テストのロードとディスカバリを
カスタマイズすることができます。そのために `load_tests protocol`_ を使用します。


.. _organizing-tests:

テストの構成
------------

ユニットテストの基礎となる構築要素は、 :dfn:`test case` --- セットアップと
正しさのチェックを行う、独立したシナリオ --- です。 :mod:`unittest` で
は、テストケースは :mod:`unittest` モジュールの :class:`TestCase` クラ
スのインスタンスで示します。テストケースを作成するには
:class:`TestCase` のサブクラスを記述するか、または
:class:`FunctionTestCase` を使用します。

:class:`TestCase` から派生したクラスのインスタンスは、このオブジェクト
だけで一件のテストと初期設定・終了処理を行います。

:class:`TestCase` インスタンスは外部から完全に独立し、単独で実行する事
も、他の任意のテストと一緒に実行する事もできなければなりません。

以下のように、 :class:`TestCase` のサブクラスは :meth:`~TestCase.runTest` をオー
バライドし、必要なテスト処理を記述するだけで簡単に書くことができます::

   import unittest

   class DefaultWidgetSizeTestCase(unittest.TestCase):
       def runTest(self):
           widget = Widget('The widget')
           self.assertEqual(widget.size(), (50,50), 'incorrect default size')

何らかのテストを行う場合、ベースクラス :class:`TestCase` の
:meth:`assert\*` メソッドを使用してください。テスト
が失敗すると例外が送出され、 :mod:`unittest` はテスト結果を
:dfn:`failure` とします。その他の例外は :dfn:`error` となります。
これによりどこに問題があるかが判ります。 :dfn:`failure` は間違った結果
(6 になるはずが 5 だった) で発生します。 :dfn:`error` は間違ったコード
(たとえば間違った関数呼び出しによる :exc:`TypeError`) で発生します。

テストの実行方法については後述とし、まずはテストケースインスタンスの作
成方法を示します。テストケースインスタンスは、以下のように引数なしでコ
ンストラクタを呼び出して作成します。::

   testCase = DefaultWidgetSizeTestCase()

似たようなテストを数多く行う場合、同じ環境設定処理を何度も必要となりま
す。例えば上記のような Widget のテストが 100 種類も必要な場合、それぞ
れのサブクラスで :class:`Widget` オブジェクトを生成する処理を記述する
のは好ましくありません。

このような場合、初期化処理は :meth:`~TestCase.setUp` メソッドに切り出し、テスト
実行時にテストフレームワークが自動的に実行するようにすることができます::

   import unittest

   class SimpleWidgetTestCase(unittest.TestCase):
       def setUp(self):
           self.widget = Widget('The widget')

   class DefaultWidgetSizeTestCase(SimpleWidgetTestCase):
       def runTest(self):
           self.assertEqual(self.widget.size(), (50,50),
                           'incorrect default size')

   class WidgetResizeTestCase(SimpleWidgetTestCase):
       def runTest(self):
           self.widget.resize(100,150)
           self.assertEqual(self.widget.size(), (100,150),
                           'wrong size after resize')

テスト中に :meth:`~TestCase.setUp` メソッドで例外が発生した場合、テストフレーム
ワークはテストを実行することができないとみなし、 :meth:`~TestCase.runTest` を実
行しません。

同様に、終了処理を :meth:`~TestCase.tearDown` メソッドに記述すると、
:meth:`~TestCase.runTest` メソッド終了後に実行されます::

   import unittest

   class SimpleWidgetTestCase(unittest.TestCase):
       def setUp(self):
           self.widget = Widget('The widget')

       def tearDown(self):
           self.widget.dispose()
           self.widget = None

:meth:`~TestCase.setUp` が正常終了した場合、 :meth:`~TestCase.runTest` が成功したかどうか
に従って :meth:`~TestCase.tearDown` が実行されます。

このような、テストを実行する環境を :dfn:`fixture` と呼びます。

JUnit では、多数の小さなテストケースを同じテスト環境で実行する場合、全
てのテストについて :class:`DefaultWidgetSizeTestCase` のような
:class:`SimpleWidgetTestCase` のサブクラスを作成する必要があります。こ
れは時間のかかる、うんざりする作業ですので、 :mod:`unittest` ではより
簡単なメカニズムを用意しています::

   import unittest

   class WidgetTestCase(unittest.TestCase):
       def setUp(self):
           self.widget = Widget('The widget')

       def tearDown(self):
           self.widget.dispose()
           self.widget = None

       def test_default_size(self):
           self.assertEqual(self.widget.size(), (50,50),
                            'incorrect default size')

       def test_resize(self):
           self.widget.resize(100,150)
           self.assertEqual(self.widget.size(), (100,150),
                            'wrong size after resize')

この例では :meth:`~TestCase.runTest` がありませんが、二つのテストメソッドを定義
しています。このクラスのインスタンスは :meth:`test_\*` メソッドのどちら
か一方の実行と、 ``self.widget`` の生成・解放を行います。この場合、テ
ストケースインスタンス生成時に、コンストラクタの引数として実行するメソッ
ド名を指定します::

   defaultSizeTestCase = WidgetTestCase('test_default_size')
   resizeTestCase = WidgetTestCase('test_resize')

:mod:`unittest` では :class:`test suite` によってテストケースインスタ
ンスをテスト対象の機能によってグループ化することができます。
:dfn:`test suite` は、 :mod:`unittest` の :class:`TestSuite` クラスで
作成します。::

   widgetTestSuite = unittest.TestSuite()
   widgetTestSuite.addTest(WidgetTestCase('test_default_size'))
   widgetTestSuite.addTest(WidgetTestCase('test_resize'))

各テストモジュールで、テストケースを組み込んだテストスイートオブジェク
トを作成する呼び出し可能オブジェクトを用意しておくと、テストの実行や参
照が容易になります::

   def suite():
       suite = unittest.TestSuite()
       suite.addTest(WidgetTestCase('test_default_size'))
       suite.addTest(WidgetTestCase('test_resize'))
       return suite

または::

   def suite():
       tests = ['test_default_size', 'test_resize']

       return unittest.TestSuite(map(WidgetTestCase, tests))

一般的には、 :class:`TestCase` のサブクラスには良く似た名前のテスト関
数が複数定義されますので、 :mod:`unittest` ではテストスイートを作成し
て個々のテストで満たすプロセスを自動化するのに使う :class:`TestLoader`
を用意しています。たとえば、::

   suite = unittest.TestLoader().loadTestsFromTestCase(WidgetTestCase)

は ``WidgetTestCase.test_default_size()`` と
``WidgetTestCase.test_resize`` を走らせるテストスイートを作成します。
:class:`TestLoader` は自動的にテストメソッドを識別するのに ``'test'``
というメソッド名の接頭辞を使います。

いろいろなテストケースが実行される順序は、テスト関数名を組み込みの
文字列の順番に従って決まります。

システム全体のテストを行う場合など、テストスイートをさらにグループ化し
たい場合がありますが、このような場合、 :class:`TestSuite` インスタンス
には :class:`TestSuite` と同じように :class:`TestSuite` を追加する事が
できます。::

   suite1 = module1.TheTestSuite()
   suite2 = module2.TheTestSuite()
   alltests = unittest.TestSuite([suite1, suite2])

テストケースやテストスイートは (:file:`widget.py` のような) テスト対象
のモジュール内にも記述できますが、テストは (:file:`test_widget.py` の
ような) 独立したモジュールに置いた方が以下のような点で有利です:

* テストモジュールだけをコマンドラインから実行することができる。

* テストコードと出荷するコードを分離する事ができる。

* テストコードを、テスト対象のコードに合わせて修正する誘惑に駆られにくい。

* テストコードは、テスト対象コードほど頻繁に更新されない。

* テストコードをより簡単にリファクタリングすることができる。

* Cで書いたモジュールのテストは、どっちにしろ独立したモジュールとなる。

* テスト戦略を変更した場合でも、ソースコードを変更する必要がない。


.. _legacy-unit-tests:

既存テストコードの再利用
------------------------

既存のテストコードが有るとき、このテストを :mod:`unittest` で実行しよ
うとするために古いテスト関数をいちいち :class:`TestCase` クラスのサブ
クラスに変換するのは大変です。

このような場合は、 :mod:`unittest` では :class:`TestCase` のサブクラス
である :class:`FunctionTestCase` クラスを使い、既存のテスト関数をラッ
プします。初期設定と終了処理も行なえます。

以下のテストコードがあった場合::

   def testSomething():
       something = makeSomething()
       assert something.name is not None
       # ...

テストケースインスタンスは次のように作成します::

   testcase = unittest.FunctionTestCase(testSomething)

初期設定、終了処理が必要な場合は、次のように指定します::

   testcase = unittest.FunctionTestCase(testSomething,
                                        setUp=makeSomethingDB,
                                        tearDown=deleteSomethingDB)

既存のテストスイートからの移行を容易にするため、 :mod:`unittest` は
:exc:`AssertionError` の送出でテストの失敗を示すような書き方もサポート
しています。
しかしながら、 :meth:`TestCase.fail\*` および
:meth:`TestCase.assert\*` メソッドを使って明確に書くことが推奨されてい
ます。 :mod:`unittest` の将来のバージョンでは、 :exc:`AssertionError`
は別の目的に使用される可能性が有ります。

.. note::

   :class:`FunctionTestCase` を使って既存のテストを :mod:`unittest` ベー
   スのテスト体系に変換することができますが、この方法は推奨されません。
   時間を掛けて :class:`TestCase` のサブクラスに書き直した方が将来的な
   テストのリファクタリングが限りなく易しくなります。

既存のテストが :mod:`doctest` を使って書かれている場合もあるでしょう。
その場合、 :mod:`doctest` は :class:`DocTestSuite` クラスを提供します。
このクラスは、既存の :mod:`doctest`\ ベースのテストから、
自動的に :class:`unittest.TestSuite` のインスタンスを作成します。


.. _unittest-skipping:

テストのスキップと意図的な失敗
--------------------------------

.. versionadded:: 2.7

unittest は特定のテストメソッドやテストクラス全体をスキップする仕組みを備えています。
さらに、この機能はテスト結果を「意図的な失敗」とすることができ、
テストが失敗しても :class:`TestResult` の失敗数にはカウントされなくなります。

テストをスキップするには、 単に :func:`skip` デコレータ(:term:`decorator`) を使用するか、
条件を表現するための :func:`skip` に類するデコレータを使用します。

スキップは以下のようになります。 ::

   class MyTestCase(unittest.TestCase):

       @unittest.skip("demonstrating skipping")
       def test_nothing(self):
           self.fail("shouldn't happen")

       @unittest.skipIf(mylib.__version__ < (1, 3),
                        "not supported in this library version")
       def test_format(self):
           # Tests that work for only a certain version of the library.
           pass

       @unittest.skipUnless(sys.platform.startswith("win"), "requires Windows")
       def test_windows_support(self):
           # windows specific testing code
           pass

このサンプルを詳細モードで実行すると以下のように出力されます。 ::

   test_format (__main__.MyTestCase) ... skipped 'not supported in this library version'
   test_nothing (__main__.MyTestCase) ... skipped 'demonstrating skipping'
   test_windows_support (__main__.MyTestCase) ... skipped 'requires Windows'

   ----------------------------------------------------------------------
   Ran 3 tests in 0.005s

   OK (skipped=3)

テストクラスは以下のようにメソッドをスキップすることができます。 ::

   @skip("showing class skipping")
   class MySkippedTestCase(unittest.TestCase):
       def test_not_run(self):
           pass

:meth:`TestCase.setUp` もスキップすることができます。
この機能はセットアップの対象のリソースが使用不可能な状態の時に便利です。

意図的な失敗の機能を使用するには、 :func:`expectedFailure` デコレータを使います。 ::

   class ExpectedFailureTestCase(unittest.TestCase):
       @unittest.expectedFailure
       def test_fail(self):
           self.assertEqual(1, 0, "broken")

独自のスキップ用のデコレータも簡単に作成することができます。
そのためには、独自のデコレータのスキップしたい時点で :func:`skip` を呼び出します。
以下のデコレータはオブジェクトに指定した属性が無い場合にテストをスキップします。 ::

   def skipUnlessHasattr(obj, attr):
       if hasattr(obj, attr):
           return lambda func: func
       return unittest.skip("{0!r} doesn't have {1!r}".format(obj, attr))

以下のデコレータはテストのスキップと意図的な失敗を実装しています。

.. function:: skip(reason)

   デコレートしたテストを無条件でスキップします。
   *reason* にはテストをスキップした理由を記載します。

.. function:: skipIf(condition, reason)

   *condition* が真の場合に、デコレートしたテストをスキップします。

.. function:: skipUnless(condition, reason)

   *condition* が偽の場合に、デコレートしたテストをスキップします。

.. function:: expectedFailure

   テストの失敗が意図的であることを表します。
   該当のテストが失敗しても、そのテストは失敗にカウントされません。

スキップしたテストの前後では、 :meth:`setUp` および :meth:`tearDown` は実行されません。
同様に、スキップしたテストクラスの前後では、 :meth:`setUpClass` および
 :meth:`tearDownClass` は実行されません。


.. _unittest-contents:

クラスと関数
------------

この節では、 :mod:`unittest` モジュールのAPIの詳細について説明します。


.. _testcase-objects:

テストクラス
~~~~~~~~~~~~

.. class:: TestCase([methodName])

   :class:`TestCase` クラスのインスタンスは、 :mod:`unittest` の世界に
   おけるテストの最小実行単位を示します。このクラスをベースクラスとし
   て使用し、必要なテストを具象サブクラスに実装します。
   :class:`TestCase` クラスでは、テストランナーがテストを実行するため
   のインターフェースと、各種のチェックやテスト失敗をレポートするため
   のメソッドを実装しています。

   それぞれの :class:`TestCase` クラスのインスタンスはただ一つのテスト
   メソッド、 *methodName* という名のメソッドを実行します。既に次のよ
   うな例を扱ったことを憶えているでしょうか。::

      def suite():
          suite = unittest.TestSuite()
          suite.addTest(WidgetTestCase('test_default_size'))
          suite.addTest(WidgetTestCase('test_resize'))
          return suite

   ここでは、それぞれが一つずつのテストを実行するような
   :class:`WidgetTestCase` の二つのインスタンスを作成しています。

   *methodName* のデフォルトは :meth:`runTest` です。

   :class:`TestCase` のインスタンスのメソッドは3種類のグループに分けられます。
   1つ目のグループのメソッドはテストの実行で使用します。2つ目のグループのメソッドは
   条件の確認および失敗のレポートといったテストの実装で使用されます。3つ目のグループである
   問い合わせ用のメソッドはテスト自身の情報を収集するために使用します。

   はじめのグループ（テスト実行）に含まれるメソッドは以下の通りです。


   .. method:: setUp()

      テストフィクスチャの準備のために呼び出されるメソッドです。テストメソッドの直前に
      呼び出されます。このメソッドを実行中に例外が発生した場合、テストの失敗ではなくエラーと
      されます。デフォルトの実装では何も行いません。


   .. method:: tearDown()

      テストメソッドが実行され、結果が記録された直後に呼び出されるメソッドです。
      このメソッドはテストメソッドで例外が投げられても呼び出されます。
      そのため、サブクラスでこのメソッドを実装する場合は、内部状態を確認することが
      必要になるでしょう。メソッドを実行中に例外が発生した場合、テストの失敗ではなく
      エラーとみなされます。このメソッドは、テストの結果に関わらず
      :meth:`setUp` が成功した場合にのみ呼ばれます。
      デフォルトの実装では何も行いません。


   .. method:: setUpClass()

      クラス内に定義されたテストが実行される前に呼び出されるクラスメソッドです。
      ``setUpClass`` はクラスを唯一の引数として取り、 :func:`classmethod` で
      デコレートされている必要があります。 ::

        @classmethod
        def setUpClass(cls):
            ...

      詳しくは `Class and Module Fixtures`_ を参照してください。

      .. versionadded:: 2.7


   .. method:: tearDownClass()

      クラス内に定義されたテストが実行された後に呼び出されるクラスメソッドです。
      ``tearDownClass`` はクラスを唯一の引数として取り、 :func:`classmethod` で
      デコレートされている必要があります。 ::

        @classmethod
        def tearDownClass(cls):
            ...

      詳しくは `Class and Module Fixtures`_ を参照してください。

      .. versionadded:: 2.7


   .. method:: run(result=None)

      テストを実行し、テスト結果を *result* に指定されたテスト結果オブジェ
      クトに渡します。 *result* 省略されるか :const:`None` か渡された場合、
      一時的な結果オブジェクトを（ :meth:`defaultTestCase` メソッドを呼んで）
      生成して使用しますが :meth:`run` の呼び出し元には渡されません。

      このメソッドは、単に :class:`TestCase` インスタンスの呼び出した場合と
      同様に振る舞います。


   .. method:: skipTest(reason)

      現在のテストでテストクラスもしくは :meth:`setUp` をスキップする場合に呼ばれます。
      詳細については、 :ref:`unittest-skipping` を参照してください。

      .. versionadded:: 2.7


   .. method:: debug()

      テスト結果を収集せずにテストを実行します。例外が呼び出し元に通知さ
      れます。また、テストをデバッガで実行することができます。

   .. _assert-methods:

   :class:`TestCase` クラスには、条件の確認と失敗のレポートのために
   以下のメソッドが定義されています。

   +-----------------------------------------+-----------------------------+---------------+
   | メソッド                                | 確認事項                    | 初出          |
   +=========================================+=============================+===============+
   | :meth:`assertEqual(a, b)                | ``a == b``                  |               |
   | <TestCase.assertEqual>`                 |                             |               |
   +-----------------------------------------+-----------------------------+---------------+
   | :meth:`assertNotEqual(a, b)             | ``a != b``                  |               |
   | <TestCase.assertNotEqual>`              |                             |               |
   +-----------------------------------------+-----------------------------+---------------+
   | :meth:`assertTrue(x)                    | ``bool(x) is True``         |               |
   | <TestCase.assertTrue>`                  |                             |               |
   +-----------------------------------------+-----------------------------+---------------+
   | :meth:`assertFalse(x)                   | ``bool(x) is False``        |               |
   | <TestCase.assertFalse>`                 |                             |               |
   +-----------------------------------------+-----------------------------+---------------+
   | :meth:`assertIs(a, b)                   | ``a is b``                  | 2.7           |
   | <TestCase.assertIs>`                    |                             |               |
   +-----------------------------------------+-----------------------------+---------------+
   | :meth:`assertIsNot(a, b)                | ``a is not b``              | 2.7           |
   | <TestCase.assertIsNot>`                 |                             |               |
   +-----------------------------------------+-----------------------------+---------------+
   | :meth:`assertIsNone(x)                  | ``x is None``               | 2.7           |
   | <TestCase.assertIsNone>`                |                             |               |
   +-----------------------------------------+-----------------------------+---------------+
   | :meth:`assertIsNotNone(x)               | ``x is not None``           | 2.7           |
   | <TestCase.assertIsNotNone>`             |                             |               |
   +-----------------------------------------+-----------------------------+---------------+
   | :meth:`assertIn(a, b)                   | ``a in b``                  | 2.7           |
   | <TestCase.assertIn>`                    |                             |               |
   +-----------------------------------------+-----------------------------+---------------+
   | :meth:`assertNotIn(a, b)                | ``a not in b``              | 2.7           |
   | <TestCase.assertNotIn>`                 |                             |               |
   +-----------------------------------------+-----------------------------+---------------+
   | :meth:`assertIsInstance(a, b)           | ``isinstance(a, b)``        | 2.7           |
   | <TestCase.assertIsInstance>`            |                             |               |
   +-----------------------------------------+-----------------------------+---------------+
   | :meth:`assertNotIsInstance(a, b)        | ``not isinstance(a, b)``    | 2.7           |
   | <TestCase.assertNotIsInstance>`         |                             |               |
   +-----------------------------------------+-----------------------------+---------------+

   （ :meth:`assertRaises` と :meth:`assertRaisesRegexp` を除く）すべての
   アサートメソッドには *msg* 引数を指定することができ、テストの失敗時の
   エラーメッセージで使用されます。
   （ :data:`longMessage` も参照してください。）

   .. method:: assertEqual(first, second, msg=None)

      *first* と *second* が等しいことをテストします。
      両者が比較出来ない場合は、テストが失敗します。

      さらに、 *first* と *second* が厳密に同じ型であり、
      その型が、list, tuple, dict, set, frozenset もしくは unicode のいずれか、
      または :meth:`addTypeEqualityFunc` で比較関数が登録されている型の場合には、
      デフォルトのエラーメッセージを生成するために、その型特有の比較関数が呼ばれます。
      （ :ref:`list of type-specific methods <type-specific-methods>` も参照してください。）

      .. versionchanged:: 2.7
         型特有の比較関数の自動呼び出しを追加。


   .. method:: assertNotEqual(first, second, msg=None)

      *first* と *second* が等しくないことをテストします。
      両者が比較出来ない場合は、テストが失敗します。

   .. method:: assertTrue(expr, msg=None)
               assertFalse(expr, msg=None)

      *expr* が真（偽）であることをテストします。

      このメソッドは、 ``bool(expr) is True`` と等価であり、 ``expr is True`` と
      等価ではないことに注意が必要です（後者のためには、 ``assertIs(expr, True)``
      が用意されています）。また、専用のメソッドが使用できる場合には、
      そちらを使用してください（例えば ``assertTrue(a == b)`` の代わりに
      ``assertEqual(a, b)`` を使用してください）。そうすることにより、
      テスト失敗時のエラーメッセージを詳細に表示することができます。


   .. method:: assertIs(first, second, msg=None)
               assertIsNot(first, second, msg=None)

      *first* と *second* が同じオブジェクトであること（そうでないこと）をテストします。

      .. versionadded:: 2.7


   .. method:: assertIsNone(expr, msg=None)
               assertIsNotNone(expr, msg=None)

      *expr* が None であること（そうでないこと）をテストします。

      .. versionadded:: 2.7


   .. method:: assertIn(first, second, msg=None)
               assertNotIn(first, second, msg=None)

      *first* が *second* に含まれること（そうでないこと）をテストします。

      .. versionadded:: 2.7


   .. method:: assertIsInstance(obj, cls, msg=None)
               assertNotIsInstance(obj, cls, msg=None)

      *obj* が *cls* のインスタンスであること（そうでないこと）をテストします。
      （この *cls* は、 :func:`isinstance` が扱うことのできる、クラスもしくは
      クラスのタプルである必要があります。）

      .. versionadded:: 2.7


   例外と例外発生時の警告を確認するために以下のメソッドを使用することができます。

   +---------------------------------------------------------+--------------------------------------+------------+
   | メソッド                                                | 確認事項                             | 初出       |
   +=========================================================+======================================+============+
   | :meth:`assertRaises(exc, fun, *args, **kwds)            | ``fun(*args, **kwds)`` raises `exc`  |            |
   | <TestCase.assertRaises>`                                |                                      |            |
   +---------------------------------------------------------+--------------------------------------+------------+
   | :meth:`assertRaisesRegexp(exc, re, fun, *args, **kwds)  | ``fun(*args, **kwds)`` raises `exc`  | 2.7        |
   | <TestCase.assertRaisesRegexp>`                          | and the message matches `re`         |            |
   +---------------------------------------------------------+--------------------------------------+------------+

   .. method:: assertRaises(exception, callable, *args, **kwds)
               assertRaises(exception)

      *callable* を呼び出した時に例外が発生することをテストします。
      :meth:`assertRaises` で指定した位置パラメータとキーワードパラメータを
      該当メソッドに渡します。 *exception* が投げられた場合にテストが成功します。
      また、他の例外が投げられた場合はエラー、例外が投げられなかった場合は失敗になります。
      複数の例外をキャッチする場合には、例外クラスのタプルを *exception* に
      指定してください。

      *exception* 引数のみが渡された場合には、コンテキストマネージャが返されます。
      これにより関数名を渡す形式ではなく、インラインでテスト対象のコードを書くことができます。 ::

         with self.assertRaises(SomeException):
             do_something()

      このコンテキストマネージャは :attr:`exception` で
      指定されたオブジェクトを格納します。
      これにより、例外発生時の詳細な確認をおこなうことができます。::

        with self.assertRaises(SomeException) as cm:
            do_something()

        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, 3)

      .. versionchanged:: 2.7
         コンテキストマネージャとして :meth:`assertRaises` を使用する機能を追加。


   .. method:: assertRaisesRegexp(exception, regexp, callable, *args, **kwds)
               assertRaisesRegexp(exception, regexp)

      :meth:`assertRaises` と同等ですが、例外の文字列表現が正規表現オブジェクトにマッチ
      することもテストします。 *regexp* は正規表現オブジェクトか、 :func:`re.search` が
      扱える正規表現が書かれた文字列である必要があります。例えば以下のようになります。 ::

         self.assertRaisesRegexp(ValueError, 'invalid literal for.*XYZ$',
                                 int, 'XYZ')

      もしくは ::

         with self.assertRaisesRegexp(ValueError, 'literal'):
            int('XYZ')

      .. versionadded:: 2.7



   さらに特有の確認を行うために以下のメソッドが用意されています。

   +---------------------------------------+--------------------------------+--------------+
   | メソッド                              | 確認項目                       | 初出         |
   +=======================================+================================+==============+
   | :meth:`assertAlmostEqual(a, b)        | ``round(a-b, 7) == 0``         |              |
   | <TestCase.assertAlmostEqual>`         |                                |              |
   +---------------------------------------+--------------------------------+--------------+
   | :meth:`assertNotAlmostEqual(a, b)     | ``round(a-b, 7) != 0``         |              |
   | <TestCase.assertNotAlmostEqual>`      |                                |              |
   +---------------------------------------+--------------------------------+--------------+
   | :meth:`assertGreater(a, b)            | ``a > b``                      | 2.7          |
   | <TestCase.assertGreater>`             |                                |              |
   +---------------------------------------+--------------------------------+--------------+
   | :meth:`assertGreaterEqual(a, b)       | ``a >= b``                     | 2.7          |
   | <TestCase.assertGreaterEqual>`        |                                |              |
   +---------------------------------------+--------------------------------+--------------+
   | :meth:`assertLess(a, b)               | ``a < b``                      | 2.7          |
   | <TestCase.assertLess>`                |                                |              |
   +---------------------------------------+--------------------------------+--------------+
   | :meth:`assertLessEqual(a, b)          | ``a <= b``                     | 2.7          |
   | <TestCase.assertLessEqual>`           |                                |              |
   +---------------------------------------+--------------------------------+--------------+
   | :meth:`assertRegexpMatches(s, re)     | ``regex.search(s)``            | 2.7          |
   | <TestCase.assertRegexpMatches>`       |                                |              |
   +---------------------------------------+--------------------------------+--------------+
   | :meth:`assertNotRegexpMatches(s, re)  | ``not regex.search(s)``        | 2.7          |
   | <TestCase.assertNotRegexpMatches>`    |                                |              |
   +---------------------------------------+--------------------------------+--------------+
   | :meth:`assertItemsEqual(a, b)         | sorted(a) == sorted(b) and     | 2.7          |
   | <TestCase.assertItemsEqual>`          | works with unhashable objs     |              |
   +---------------------------------------+--------------------------------+--------------+
   | :meth:`assertDictContainsSubset(a, b) | all the key/value pairs        | 2.7          |
   | <TestCase.assertDictContainsSubset>`  | in `a` exist in `b`            |              |
   +---------------------------------------+--------------------------------+--------------+


   .. method:: assertAlmostEqual(first, second, places=7, msg=None, delta=None)
               assertNotAlmostEqual(first, second, places=7, msg=None, delta=None)

      *first* と *second* が近似的に等しい（等しくない）ことをテストします。
      この比較は、*places* （デフォルト7）で指定した小数位で丸めた差分を
      ゼロと比べることでおこないます。これらのメソッドは、（ :func:`round` と同様に）
       *小数位* を指定するのであって、*有効桁数* を指定するのではないことに注意してください。

      *places* の代わりに *delta* が渡された場合には、
      *first* と *second* の差分が *delta* より大きい（小さい）ことをテストします。

      *delta* と *places* の両方が指定された場合は ``TypeError`` が投げられます。

      .. versionchanged:: 2.7
         :meth:`assertAlmostEqual` は、オブジェクトが等しい場合には自動で
	 近似的に等しいとみなすようになりました。
         :meth:`assertNotAlmostEqual` は、オブジェクトが等しい場合には自動的に
	 失敗するようになりました。
         *delta* 引数が追加されました。


   .. method:: assertGreater(first, second, msg=None)
               assertGreaterEqual(first, second, msg=None)
               assertLess(first, second, msg=None)
               assertLessEqual(first, second, msg=None)

      *first* が *second* と比べて、メソッド名に対応して >, >=, < もしくは <=
      であることをテストします。そうでない場合はテストが失敗します。 ::

         >>> self.assertGreaterEqual(3, 4)
         AssertionError: "3" unexpectedly not greater than or equal to "4"

      .. versionadded:: 2.7


   .. method:: assertRegexpMatches(text, regexp, msg=None)

      *regexp* の検索が *text* とマッチすることをテストします。テスト失敗時には、
      エラーメッセージにパターンと *text* が表示されます（もしくは、
      パターンと意図しないかたちでマッチした *text* の一部が表示されます）。
      *regexp* は正規表現オブジェクトか、 :func:`re.search` が
      扱える正規表現が書かれた文字列である必要があります。

      .. versionadded:: 2.7


   .. method:: assertNotRegexpMatches(text, regexp, msg=None)

      *regexp* の検索が *text* とマッチしないことをテストします。テスト失敗時には、
      エラーメッセージにマッチしたパターンと *text* が表示されます。
      *regexp* は正規表現オブジェクトか、 :func:`re.search` が
      扱える正規表現が書かれた文字列である必要があります。

      .. versionadded:: 2.7


   .. method:: assertItemsEqual(actual, expected, msg=None)

      シーケンス *expected* が *actual* と同じ要素を含んでいることをテストします。
      要素の順序はテスト結果に影響しません。要素が含まれていない場合には、
      シーケンスの差分がエラーメッセージとして表示されます。

      *actual* と *expected* の比較では、重複した要素は無視 *されません* 。
      両者に同じ数の要素が含まれていることを検証します。このメソッドは
      ``assertEqual(sorted(expected), sorted(actual))`` と同等に振る舞うことに加えて、
      ハッシュ化できないオブジェクトのシーケンスでも動作します。

      .. versionadded:: 2.7


   .. method:: assertDictContainsSubset(expected, actual, msg=None)

      辞書 *actual* のキー/バリューペアが *expected* のスーパーセットになっているかどうかを
      テストします。そうなっていない場合には、足りないキーとバリューの一覧が
      エラーメッセージに表示されます。

      .. versionadded:: 2.7
      .. deprecated:: 3.2



   .. _type-specific-methods:

   :meth:`assertEqual` メソッドは、同じ型のオブジェクトの等価性確認のために、
   型ごとに特有のメソッドにディスパッチします。これらのメソッドは、ほとんどの組み込み型用の
   メソッドは既に実装されています。さらに、 :meth:`addTypeEqualityFunc` を使う事で
   新たなメソッドを登録することができます。

   .. method:: addTypeEqualityFunc(typeobj, function)

      :meth:`assertEqual` で呼び出される型特有のメソッドを登録します。
      登録するメソッドは、 比較する2つのオブジェクトの型がが厳密に *typeobj* と同じ
      （サブクラスでもいけません）の場合に等価性を確認します。 *function*  は
      :meth:`assertEqual` と同様に、2つの位置固定引数と、3番目に msg=None のキーワード引数を
      取れる必要があります。このメソッドは、始めの2つに指定したパラメータ間の差分を
      検出した時に :data:`self.failureException(msg) <failureException>` の例外を投げる
      必要があります。この例外を投げる際は、出来る限り、エラーの内容が分かる有用な情報と
      差分の詳細をエラーメッセージに含めてください。

      .. versionadded:: 2.7

   :meth:`~TestCase.assertEqual` が自動的に呼び出す型特有のメソッドの概要を
   以下の表示に記載しています。これらのメソッドは通常は直接呼び出す必要がない
   ことに注意が必要です。

   +-----------------------------------------+-----------------------------+--------------+
   | メソッド                                | 比較の対象                  | 初出         |
   +=========================================+=============================+==============+
   | :meth:`assertMultiLineEqual(a, b)       | strings                     | 2.7          |
   | <TestCase.assertMultiLineEqual>`        |                             |              |
   +-----------------------------------------+-----------------------------+--------------+
   | :meth:`assertSequenceEqual(a, b)        | sequences                   | 2.7          |
   | <TestCase.assertSequenceEqual>`         |                             |              |
   +-----------------------------------------+-----------------------------+--------------+
   | :meth:`assertListEqual(a, b)            | lists                       | 2.7          |
   | <TestCase.assertListEqual>`             |                             |              |
   +-----------------------------------------+-----------------------------+--------------+
   | :meth:`assertTupleEqual(a, b)           | tuples                      | 2.7          |
   | <TestCase.assertTupleEqual>`            |                             |              |
   +-----------------------------------------+-----------------------------+--------------+
   | :meth:`assertSetEqual(a, b)             | sets or frozensets          | 2.7          |
   | <TestCase.assertSetEqual>`              |                             |              |
   +-----------------------------------------+-----------------------------+--------------+
   | :meth:`assertDictEqual(a, b)            | dicts                       | 2.7          |
   | <TestCase.assertDictEqual>`             |                             |              |
   +-----------------------------------------+-----------------------------+--------------+



   .. method:: assertMultiLineEqual(first, second, msg=None)

      複数行の文字列 *first* が文字列 *second* と等しいことをテストします。
      等しくない場合には、両者の差分がハイライトされてエラーメッセージに表示されます。
      このメソッドは、デフォルトで、 :meth:`assertEqual` が string を比較するときに
      自動的に使用します。

      .. versionadded:: 2.7


   .. method:: assertSequenceEqual(seq1, seq2, msg=None, seq_type=None)

      2つのシーケンスが等しいことをテストします。 *seq_type* が指定された場合、
      *seq1* と *seq2* が *seq_type* のインスタンスで無い場合にはテストが失敗します。
      シーケンスどうしが異なる場合には、両者の差分がエラーメッセージに表示されます。

      このメソッドは直接 :meth:`assertEqual` からは呼ばれませんが、
      :meth:`assertListEqual` と :meth:`assertTupleEqual` の実装で使われています。

      .. versionadded:: 2.7


   .. method:: assertListEqual(list1, list2, msg=None)
               assertTupleEqual(tuple1, tuple2, msg=None)

      2つのリストまたはタプルが等しいかどうかをテストします。等しくない場合には、
      両者の差分を表示します。2つのパラメータの型が異なる場合には
      テストがエラーになります。このメソッドは、デフォルトで、 :meth:`assertEqual` が
      list または tuple を比較するときに自動的に使用します。

      .. versionadded:: 2.7


   .. method:: assertSetEqual(set1, set2, msg=None)

      2つのセットが等しいかどうかをテストします。等しくない場合には、
      両者の差分を表示します。このメソッドは、デフォルトで、 :meth:`assertEqual` が
      set もしくは frozenset を比較するときに自動的に使用します。

      *set1* or *set2* のいずれかに :meth:`set.difference` が無い場合には
      テストは失敗します。

      .. versionadded:: 2.7


   .. method:: assertDictEqual(expected, actual, msg=None)

      2つの辞書が等しいかどうかをテストします。等しくない場合には、
      両者の差分を表示します。このメソッドは、デフォルトで、 :meth:`assertEqual` が
      dict を比較するときに自動的に使用します。

      .. versionadded:: 2.7



   .. _other-methods-and-attrs:

   最後に、 :class:`TestCase` の残りのメソッドと属性を紹介します。


   .. method:: fail(msg=None)

      無条件にテストを失敗させます。
      エラーメッセージの表示に、 *msg* または ``None`` が使われます。


   .. attribute:: failureException

      :meth:`test` メソッドが送出する例外を指定するクラス属性です。
      例えばテストフレームワークで追加情報を付した特殊な例外が必要になる場合、
      この例外のサブクラスとして作成します。この属性の初期値は :exc:`AssertionError`
      です。


   .. attribute:: longMessage

      この属性に ``True`` が設定された場合、 :ref:`assert methods <assert-methods>`
      で指定したすべての明示的な失敗メッセージが、通常の失敗メッセージに追加されます。
      通常の失敗メッセージには、オブジェクトに関する有用な情報が含まれています。
      例えば、 assertEqual は異なるオブジェクトの repr を表示します。
      この属性を ``True`` にすることで、カスタマイズしたエラーメッセージを通常の
      メッセージに追加することができます。

      この属性はデフォルトで ``False`` になっていて、カスタムメッセージが渡されても
      表示しないようになっています。

      アサートメソッドを呼び出す前に、
      インスタンス属性として ``True`` または ``False`` を指定することで、
      この設定をオーバーライドすることができます。

      .. versionadded:: 2.7


   .. attribute:: maxDiff

      この属性は、アサーションメソッドが失敗をレポートする時に表示する
      差分の長さをコントロールします。デフォルトは 80*8 文字です。
      この属性が影響するメソッドは、
      :meth:`assertSequenceEqual` （およびこのメソッドに委譲するシーケンス比較メソッド）、
      :meth:`assertDictEqual` と :meth:`assertMultiLineEqual` です。

      ``maxDiff`` に ``None`` を指定すると差分表示の上限がなくなります。

      .. versionadded:: 2.7


   テストフレームワークは、テスト情報を収集するために以下のメソッドを使用
   します。


   .. method:: countTestCases()

      テストオブジェクトに含まれるテストの数を返します。
      :class:`TestCase` インスタンスは常に ``1`` を返します。


   .. method:: defaultTestResult()

      このテストケースクラスで使われるテスト結果クラスのインスタンスを (
      もし :meth:`run` メソッドに他の結果インスタンスが提供されないならば
      ) 返します。

      :class:`TestCase` インスタンスに対しては、いつも
      :class:`TestResult` のインスタンスですので、 :class:`TestCase` のサ
      ブクラスでは必要に応じてこのメソッドをオーバライドしてください。


   .. method:: id()

      テストケースを特定する文字列を返します。通常、 *id* はモジュール名・
      クラス名を含む、テストメソッドのフルネームを指定します。


   .. method:: shortDescription()

      テストの説明を一行分、または説明がない場合には :const:`None` を返し
      ます。デフォルトでは、テストメソッドの docstring の先頭の一行、また
      は :const:`None` を返します。



   .. method:: addCleanup(function, *args, **kwargs)

      :meth:`tearDown` の後に呼び出される関数を追加します。
      この関数はリソースのクリーンアップのために使用します。
      追加された関数は、追加された順と逆の順番で呼び出されます（LIFO）。
      :meth:`addCleanup` に渡された引数とキーワード引数が
      追加された関数にも渡されます。

      :meth:`setUp` が失敗した場合、つまり :meth:`tearDown` が呼ばれなかった場合でも、
      追加されたクリーンアップ関数は呼び出されます。

      .. versionadded:: 2.7


   .. method:: doCleanups()

      このメソッドは、 :meth:`tearDown` の後、もしくは、
      :meth:`setUp` が例外を投げた場合は :meth:`setUp` の後に、
      無条件で呼ばれます。

      このメソッドは、 :meth:`addCleanup` で追加された関数を呼び出す責務を担います。
      もし、クリーンアップ関数を :meth:`tearDown` より前に呼び出す必要がある場合には、
      :meth:`doCleanups` を明示的に呼び出してください。

      :meth:`doCleanups` は、どこで呼び出されても、
      クリーンアップ関数をスタックから削除して実行します。

      .. versionadded:: 2.7


.. class:: FunctionTestCase(testFunc, setUp=None, tearDown=None, description=None)

   このクラスでは :class:`TestCase` インターフェースの内、テストランナー
   がテストを実行するためのインターフェースだけを実装しており、テスト
   結果のチェックやレポートに関するメソッドは実装していません。既存の
   テストコードを :mod:`unittest` によるテストフレームワークに組み込む
   ために使用します。


廃止予定のエイリアス
####################

歴史的な経緯で、 :class:`TestCase` のいくつかのエイリアスは廃止予定となりました。
以下の表に、廃止予定のエイリアスをまとめます。

   ==============================  ===============================
    メソッド名                      廃止予定のエリアス
   ==============================  ===============================
    :meth:`.assertEqual`            failUnlessEqual, assertEquals
    :meth:`.assertNotEqual`         failIfEqual
    :meth:`.assertTrue`             failUnless, assert\_
    :meth:`.assertFalse`            failIf
    :meth:`.assertRaises`           failUnlessRaises
    :meth:`.assertAlmostEqual`      failUnlessAlmostEqual
    :meth:`.assertNotAlmostEqual`   failIfAlmostEqual
   ==============================  ===============================

   .. deprecated:: 2.7
         表の第2列のエイリアスを廃止予定



.. _testsuite-objects:

テストのグルーピング
~~~~~~~~~~~~~~~~~~~~

.. class:: TestSuite(tests=())


   このクラスは、個々のテストケースやテストスイートの集約を示します。
   通常のテストケースと同じようにテストランナーで実行するためのインタ
   フェースを備えています。 :class:`TestSuite` インスタンスを実行する
   ことはスイートの繰り返しを使って個々のテストを実行することと同じで
   す。

   引数 *tests* が指定された場合、それはテストケースに亘る繰り返し可
   能オブジェクトまたは内部でスイートを組み立てるための他のテストスイー
   トでなければなりません。
   後からテストケースやスイートをコレクションに付け加えるためのメソッ
   ドも提供されています。

   :class:`TestSuite` は :class:`TestCase` オブジェクトのように振る舞います。
   違いは、スイートにはテストを実装しない点にあります。代わりに、テストをまとめて
   グループ化して、同時に実行します。 :class:`TestSuite` のインスタンスに
   テスト追加するためのメソッドが用意されています。

   .. method:: TestSuite.addTest(test)

      :class:`TestCase` 又は :class:`TestSuite` のインスタンスをスイート
      に追加します。


   .. method:: TestSuite.addTests(tests)

      イテラブル *tests* に含まれる全ての :class:`TestCase` 又は
      :class:`TestSuite` のインスタンスをスイートに追加します。

      このメソッドは *test* 上のイテレーションをしながらそれぞれの要素に
      :meth:`addTest` を呼び出すのと等価です。

   :class:`TestSuite` クラスは :class:`TestCase` と以下のメソッドを共有し
   ます。


   .. method:: run(result)

      スイート内のテストを実行し、結果を *result* で指定した結果オブジェ
      クトに収集します。 :meth:`TestCase.run` と異なり、
      :meth:`TestSuite.run` では必ず結果オブジェクトを指定する必要があり
      ます。


  .. method:: debug()

      このスイートに関連づけられたテストを結果を収集せずに実行します。こ
      れによりテストで送出された例外は呼び出し元に伝わるようになり、デバッ
      ガの下でのテスト実行をサポートできるようになります。


   .. method:: TestSuite.countTestCases()

      このテストオブジェクトによって表現されるテストの数を返します。これ
      には個別のテストと下位のスイートも含まれます。

   .. method:: __iter__()

      :class:`TestSuite` でグループ化されたテストはイテレータでアクセスできます。
      サブクラスは :meth:`__iter__` をオーバーライドすることで、テストへのアクセスを
      定義します。1つのメソッド内でこのメソッドは何度も呼ばれる可能性があることに注意
      してください（例えば、テスト数のカウントと等価性の比較）。
      そのため、イテレーションを繰り返しても同じテストを返すように実装してください。

      .. versionchanged:: 2.7
         以前のバージョンでは :class:`TestSuite` はイテレータではなく、直接テストに
	 アクセスしていました。そのため、 :meth:`__iter__` をオーバーラードしても
	 テストにアクセスできませんでした。

   通常、 :class:`TestSuite` の :meth:`run` メソッドは
   :class:`TestRunner` が起動するため、ユーザが直接実行する必要はありません。


テストのロードと起動
~~~~~~~~~~~~~~~~~~~~~~~~~

 .. class:: TestLoader()


   :class:`TestLoader` クラスはクラスとモジュールからテストスイートを生成します。
   通常、このクラスのインスタンスを明示的に生成する必要はありません。
   :mod:`unittest` モジュールの ``unittest.defaultTestLoader`` を
   共用インスタンスとして使用することができます。
   しかし、このクラスのサブクラスやインスタンスで、属性をカスタマイズすることができます。

   :class:`TestLoader` のオブジェクトには以下のメソッドがあります。


   .. method:: loadTestsFromTestCase(testCaseClass)

      :class:`TestCase` の派生クラス :class:`testCaseClass` に含まれる全
      テストケースのスイートを返します。


   .. method:: loadTestsFromModule(module)

      指定したモジュールに含まれる全テストケースのスイートを返します。このメ
      ソッドは *module* 内の :class:`TestCase` 派生クラスを検索し、見つかっ
      たクラスのテストメソッドごとにクラスのインスタンスを作成します。

      .. note::

         :class:`TestCase` クラスを基底クラスとしてクラス階層を構築すると
         fixture や補助的な関数をうまく共用することができますが、基底クラ
         スに直接インスタンス化できないテストメソッドがあると、この
         :meth:`loadTestsFromModule` を使うことができません。この場合でも、
         fixture が全て別々で定義がサブクラスにある場合は使用することがで
         きます。

      モジュールが ``load_tests`` 関数を用意している場合、この関数が
      テストのロードに使われます。これによりテストのロードをカスタマイズできます。
      これが `load_tests protocol`_ です。

      .. versionchanged:: 2.7
         ``load_tests`` をサポートしました。

   .. method:: loadTestsFromName(name, module=None)

      文字列で指定される全テストケースを含むスイートを返します。

      *name* には "ドット修飾名" でモジュールかテストケースクラス、テスト
      ケースクラス内のメソッド、 :class:`TestSuite` インスタンスまたは
      :class:`TestCase` か :class:`TestSuite` のインスタンスを返す呼び出
      し可能オブジェクトを指定します。このチェックはここで挙げた順番に行
      なわれます。すなわち、候補テストケースクラス内のメソッドは「呼び出
      し可能オブジェクト」としてではなく「テストケースクラス内のメソッド」
      として拾い出されます。

      例えば :mod:`SampleTests` モジュールに :class:`TestCase` から派生し
      た :class:`SampleTestCase` クラスがあり、 :class:`SampleTestCase`
      にはテストメソッド :meth:`test_one` ・ :meth:`test_two` ・
      :meth:`test_three` があるとします。この場合、 *name* に
      ``'SampleTests.SampleTestCase'`` と指定すると、
      :class:`SampleTestCase` の三つのテストメソッドを実行するテストスイートが
      作成されます。 ``'SampleTests.SampleTestCase.test_two'`` と指定すれ
      ば、 :meth:`test_two` だけを実行するテストスイートが作成されます。
      インポートされていないモジュールやパッケージ名を含んだ名前を指定し
      た場合は自動的にインポートされます。

      また、 *module* を指定した場合、 *module* 内の *name* を取得します。


   .. method:: loadTestsFromNames(names, module=None)

      :meth:`loadTestsFromName` と同じですが、名前を一つだけ指定するので
      はなく、複数の名前のシーケンスを指定する事ができます。戻り値は
      *names* 中の名前で指定されるテスト全てを含むテストスイートです。


   .. method:: getTestCaseNames(testCaseClass)

      *testCaseClass* 中の全てのメソッド名を含むソート済みシーケンスを返
      します。 *testCaseClass* は :class:`TestCase` のサブクラスでなけれ
      ばなりません。



   .. method:: discover(start_dir, pattern='test*.py', top_level_dir=None)

      すべてのテストモジュールを指定された開始ディレクトリから検索して返します。
      再帰的にサブディレクトリも検索します。 *pattern* にマッチしたテストファイルだけが
      ロードの対象になります。（シェルスタイルのパターンマッチングが使われます。）
      その中で、インポート可能なもジュール（つまりPythonの識別子として
      有効であるということです）がロードされます。

      すべてのテストモジュールはプロジェクトのトップレベルからインポート可能である必要が
      あります。開始ディレクトリがトップレベルディレクトリでない場合は、
      トップレベルディレクトリが分離できなくてはいけません。      

      例えば、シンタックスエラーなどで、モジュールのインポートに失敗した場合、
      エラーが記録され、ディスカバリ自体は続けられます。

      テストパッケージ名（ :file:`__init__.py` の置かれたディレクトリ名） が
      パターンにマッチした場合、 ``load_tests`` 関数がチェックされます。
      この関数が存在している場合、この関数に *loader*, *tests*, *pattern* が渡され
      呼び出されます。

      load_tests が存在して、ディスカバリがパッケージ内を再帰的な検索を
      続けている途中で *ない* 場合、 ``load_tests`` はそのパッケージ内の全ての
      テストをロードする責務を担います。

      意図的にパターンはローダの属性として保持されないようになっています。
      それにより、パッケージが自分自身のディスカバリを続ける事ができます。
      *top_level_dir* は保持されるため、 ``loader.discover()`` に引数として
      渡す必要はありません。

      .. versionadded:: 2.7

   以下の属性は、サブクラス化またはインスタンスの属性値を変更して
   :class:`TestLoader` をカスタマイズする場合に使用します。


   .. attribute:: testMethodPrefix

      テストメソッドの名前と判断されるメソッド名の接頭語を示す文字列。デ
      フォルト値は ``'test'`` です。

      この値は :meth:`getTestCaseNames` と全ての :meth:`loadTestsFrom\*`
      メソッドに影響を与えます。


   .. attribute:: sortTestMethodsUsing

      :meth:`getTestCaseNames` および全ての :meth:`loadTestsFrom\*` メソッ
      ドでメソッド名をソートする際に使用する比較関数。デフォルト値は組み
      込み関数 :func:`cmp` です。ソートを行なわないようにこの属性に
      :const:`None` を指定することもできます。


   .. attribute:: suiteClass

      テストのリストからテストスイートを構築する呼び出し可能オブジェクト。
      メソッドを持つ必要はありません。デフォルト値は :class:`TestSuite`
      です。

      この値は全ての :meth:`loadTestsFrom\*` メソッドに影響を与えます。


.. class:: TestResult

   このクラスはどのテストが成功しどのテストが失敗したかという
   情報を収集するのに使います。

   :class:`TestResult` は、複数のテスト結果を記録します。
   :class:`TestCase` クラスと :class:`TestSuite` クラスのテスト結果を正し
   く記録しますので、テスト開発者が独自にテスト結果を管理する処理を開発す
   る必要はありません。

   :mod:`unittest` を利用したテストフレームワークでは、
   :meth:`TestRunner.run` が返す :class:`TestResult` インスタンスを参照し、
   テスト結果をレポートします。

   以下の属性は、テストの実行結果を検査する際に使用することができます。


   .. attribute:: errors

      :class:`TestCase` と例外のトレースバック情報をフォーマットした文字
      列の 2 要素タプルからなるリスト。それぞれのタプルは予想外の例外を送
      出したテストに対応します。

      .. versionchanged:: 2.2
         :func:`sys.exc_info` の結果ではなく、フォーマットしたトレースバッ
         クを保存します。


   .. attribute:: failures

      :class:`TestCase` と例外のトレースバック情報をフォーマットした文字列の
      2 要素タプルからなるリスト。それぞれのタプルは
      :meth:`TestCase.fail\*` や :meth:`TestCase.assert\*` メソッドを使っ
      て見つけ出した失敗に対応します。

      .. versionchanged:: 2.2
         :func:`sys.exc_info` の結果ではなく、フォーマットしたトレースバック
         を保存します。

   .. attribute:: skipped

      :class:`TestCase` インスタンスと理由の文字列の2要素タプルからなるリストを
      保持します。

      .. versionadded:: 2.7

   .. attribute:: expectedFailures

      :class:`TestCase` と例外のトレースバック情報をフォーマットした文字列の
      2 要素タプルからなるリスト。それぞれのタプルは意図した失敗に対応します。

   .. attribute:: unexpectedSuccesses

      意図した失敗のマークが付いていながら成功してしまった :class:`TestCase` の
      インスタンスのリスト。

   .. attribute:: shouldStop

      ``True`` が設定されると :meth:`stop` によりテストの実行が停止します。


   .. attribute:: testsRun

      これまでに実行したテストの総数です。


   .. attribute:: buffer

      ``True`` が設定されると、 ``sys.stdout`` と ``sys.stderr`` は、
      :meth:`startTest` から :meth:`stopTest` が呼ばれるまでの間バッファリングされます。
      実際に、結果が ``sys.stdout`` と ``sys.stderr`` に出力されるのは、
      テストが失敗するかエラーが発生した時になります。表示の際には、
      全ての失敗 / エラーメッセージが表示されます。

      .. versionadded:: 2.7


   .. attribute:: failfast

      ``True`` が設定されると、 :meth:`stop` が始めの失敗もしくはエラーの時に呼び出され、
      テストの実行が終了します。

      .. versionadded:: 2.7


   .. method:: wasSuccessful()

      これまでに実行したテストが全て成功していれば :const:`True` を、それ
      以外なら :const:`False` を返します。


   .. method:: stop()

      このメソッドを呼び出して :class:`TestResult` の ``shouldStop`` 属性
      に :const:`True` をセットすることで、実行中のテストは中断しなければ
      ならないというシグナルを送ることができます。 :class:`TestRunner` オ
      ブジェクトはこのフラグを尊重してそれ以上のテストを実行することなく
      復帰しなければなりません。

      たとえばこの機能は、ユーザのキーボード割り込みを受け取って
      :class:`TextTestRunner` クラスがテストフレームワークを停止させるの
      に使えます。 :class:`TestRunner` の実装を提供する対話的なツールでも
      同じように使用することができます。

   以下のメソッドは内部データ管理用のメソッドですが、対話的にテスト結果を
   レポートするテストツールを開発する場合などにはサブクラスで拡張すること
   ができます。


   .. method:: startTest(test)
   
      *test* を実行する直前に呼び出されます。
   
   .. method:: stopTest(test)
   
      *test* の実行直後に、テスト結果に関わらず呼び出されます。
   
   .. method:: startTestRun(test)

      全てのテストが実行される前に一度だけ実行されます。

      .. versionadded:: 2.7


   .. method:: stopTestRun(test)

      全てのテストが実行された後に一度だけ実行されます。

      .. versionadded:: 2.7
   
   .. method:: addError(test, err)
   
      テスト *test* 実行中に、想定外の例外が発生した場合に呼び出されます。
      *err* は :func:`sys.exc_info` が返すタプル ``(type, value,
      traceback)`` です。
   
      デフォルトの実装では、タプル、 ``(test, formatted_err)`` をインスタ
      ンスの ``errors`` 属性に追加します。ここで、 *formatted_err* は、
      *err* から導出される、整形されたトレースバックです。
   
   
   .. method:: addFailure(test, err)
   
      テストが失敗した場合に呼び出されます。 *err* は
      :func:`sys.exc_info` が返すタプル ``(type, value, traceback)`` です。
   
      デフォルトの実装では、タプル、 ``(test, formatted_err)`` をインスタ
      ンスの ``errors`` 属性に追加します。ここで、 *formatted_err* は、
      *err* から導出される、整形されたトレースバックです。
   
   
   .. method:: addSuccess(test)
   
      テストケース *test* が成功した場合に呼び出されます。
   
      デフォルトの実装では何もしません。


   .. method:: addSkip(test, reason)

      *test* がスキップされた時に呼び出されます。
      *reason* はスキップの際に渡された理由の文字列です。

      デフォルトの実装では、 ``(test, reason)`` のタプルを
      インスタンスの :attr:`skipped` 属性に追加します。


   .. method:: addExpectedFailure(test, err)

      :func:`expectedFailure` のデコレータでマークされた *test* が
      失敗した時に呼び出されます。

      デフォルトの実装では ``(test, formatted_err)`` のタプルを
      インスタンスの :attr:`expectedFailures` に追加します。
      ここで *formatted_err* は *err* から派生した整形されたトレースバックです。


   .. method:: addUnexpectedSuccess(test)

      :func:`expectedFailure` のデコレータでマークされた *test* が
      成功した時に呼び出されます。

      デフォルトの実装ではテストをインスタンスの :attr:`unexpectedSuccesses` 属性に
      追加します。

.. data:: defaultTestLoader

   :class:`TestLoader` のインスタンスで、共用することが目的です。
   :class:`TestLoader` をカスタマイズする必要がなければ、新しい
   :class:`TestLoader` オブジェクトを作らずにこのインスタンスを使用し
   ます。


.. class:: TextTestRunner(stream=sys.stderr, descriptions=True, verbosity=1)

   実行結果を標準エラーに出力する、単純なテストランナー。いくつかの設
   定項目がありますが、非常に単純です。グラフィカルなテスト実行アプリ
   ケーションでは、独自のテストランナーを作成してください。

   .. method:: _makeResult()

      このメソッドは :meth:`run` で使われる ``TestResult`` のインスタンスを返します。
      このメソッドは明示的に呼び出す必要はありませんが、
      サブクラスで ``TestResult`` をカスタマイズすることができます。

      ``_makeResult()`` は、 ``TextTestRunner`` のコンストラクタで
      ``resultclass`` 引数として渡されたクラスもしくはコーラブルオブジェクトを
      インスタンス化します。 ``resultclass`` が指定されていない場合には、
      デフォルトで :class:`TextTestResult` が使用されます。結果のクラスは
      以下の引数が渡されインスタンス化されます。 ::

            stream, descriptions, verbosity


.. function:: main([module[, defaultTest[, argv[, testRunner[, testLoader[, exit[, verbosity[, failfast[, catchbreak[,buffer]]]]]]]]]])

.. function:: main([module[, defaultTest[, argv[, testRunner[, testLoader]]]]])

   テストを実行するためのコマンドラインプログラム。この関数を使えば、
   簡単に実行可能なテストモジュールを作成する事ができます。
   一番簡単なこの関数の使い方は、以下の行をテストスクリプトの最後に置
   くことです。 ::

      if __name__ == '__main__':
          unittest.main()

   You can run tests with more detailed information by passing in the verbosity
   argument::

      if __name__ == '__main__':
          unittest.main(verbosity=2)

   引数、 *testRunner* は、test runner class、あるいは、そのインスタン
   スのどちらでも構いません。でフォルトでは ``main`` はテストが成功したか失敗したかに
   対応した終了コードと共に :func:`sys.exit` を呼び出します。

   ``main`` は、 ``exit=False`` を指定する事で対話的なインタプリタから
   使用することもできます。この引数を指定すると、 :func:`sys.exit` を呼ばずに、
   結果のみを出力します。 ::

      >>> from unittest import main
      >>> main(module='test_module', exit=False)

   ``failfast``, ``catchbreak`` と ``buffer`` は、 `command-line options`_ にある
   同名のオプションと同じ効果のあるパラメータです。

   ``main`` を呼び出すと、 ``TestProgram`` のインスタンスが返されます。
   このインスタンスは、 ``result`` 属性にテスト結果を保持します。

   .. versionchanged:: 2.7
      ``exit``, ``verbosity``, ``failfast``, ``catchbreak`` と ``buffer``
      パラメータが追加されました。


load_tests プロトコル
#####################

.. versionadded:: 2.7

モジュールやパッケージには、 ``load_tests`` と呼ばれる関数を実装できます。
これにより、通常のテスト実行時やテストディスカバリ時のテストのロードされ方を
カスタマイズできます。

テストモジュールが ``load_tests`` を定義していると、
それが :meth:`TestLoader.loadTestsFromModule` から呼ばれます。引数は以下です::

    load_tests(loader, standard_tests, None)

これは :class:`TestSuite` を返すべきです。

*loader* はローディングを行う :class:`TestLoader` のインスタンスです。
*standard_tests* は、そのモジュールからデフォルトでロードされるテストです。
これは、テストの標準セットのテストの追加や削除のみを行いたい
テストモジュールに一般に使われます。第三引数は、パッケージをテストディスカバリの
一部としてロードするときに使われます。

特定の :class:`TestCase` クラスのセットからテストをロードする
典型的な ``load_tests`` 関数は、このようになります::

    test_cases = (TestCase1, TestCase2, TestCase3)

    def load_tests(loader, tests, pattern):
        suite = TestSuite()
        for test_class in test_cases:
            tests = loader.loadTestsFromTestCase(test_class)
            suite.addTests(tests)
        return suite

ディスカバリが開始されると、パッケージ名にマッチするパターンを、
コマンドラインまたは:meth:`TestLoader.discover` に与えることで、
:file:`__init__.py` に ``load_tests`` があるか調べられます。

.. note::

   デフォルトのパターンは 'test*.py' です。これは、 'test' で始まる
   全ての Python ファイルにマッチしますが、テストディレクトリには絶対に
   マッチ *しません* 。

   'test*' のようなパターンは、モジュールだけでなくテストパッケージにも
   マッチします。

パッケージ :file:`__init__.py` が ``load_tests`` を定義していると、
それが呼び出され、ディスカバリはそれ以上パッケージ内で続けられません。
``load_tests`` が以下の引数で呼び出されます::

    load_tests(loader, standard_tests, pattern)

これはパッケージ内のすべてのテストを表す :class:`TestSuite` を返すべきです。
(``standard_tests`` には、 :file:`__init__.py` から収集されたテストのみが
含まれます。)

パターンは ``load_tests`` に渡されるので、パッケージは自由に
テストディスカバリを継続 (必要なら変更) できます。テストパッケージに
'何もしない' ``load_tests`` 関数は次のようになります。::

    def load_tests(loader, standard_tests, pattern):
        # top level directory cached on loader instance
        this_dir = os.path.dirname(__file__)
        package_tests = loader.discover(start_dir=this_dir, pattern=pattern)
        standard_tests.addTests(package_tests)
        return standard_tests



クラスとモジュールの修正
------------------------

クラスレベルとモジュールレベルの修正が :class:`TestSuite` に実装されました。
テストスイートが新しいクラスのテストに出会うと、以前のクラス (があれば) から
:meth:`tearDownClass` が呼び出され、その後に新しいクラスから
:meth:`setUpClass` が呼び出されます。

同様に、テストが以前のテストとは異なるモジュールからのテストであるとき、
まず以前のモジュールから ``tearDownModule`` が実行され、その後に新しいモジュール
から ``setUpModule`` が実行されます。

すべてのテストが実行された後、最後の ``tearDownClass`` と ``tearDownModule``
が実行されます。

なお、共通の修正は、テストの並列化などの [潜在的な] 機能と同時には
うまくいかず、テストの分離を中断します。気をつけて使うべきです。

unittest テストローダによるテスト作成のデフォルトの順序では、
同じモジュールやクラスからのテストはすべて同じグループにまとめられます。
これにより、 ``setUpClass`` / ``setUpModule`` (など) は、一つのクラスや
モジュールにつき一度だけ呼ばれます。この順序をバラバラにし、
異なるモジュールやクラスのテストが並ぶようにすると、共通の修正関数は、
一度のテストで複数回呼ばれるようにもなります。

共通の修正は、普通でない順序に合わせることを意図していません。
共通の修正を望まないフレームワークのために、 ``BaseTestSuite`` が
まだ存在しています。

共通の修正関数のいずれかの中で送出された例外があれば、そのテストはエラーとして
報告されます。対応するテストインスタンスが無いので、(:class:`TestCase` と
同じインタフェースの) ``_ErrorHolder`` オブジェクトが生成され、エラーを
表します。貴方が標準 unittest テストランナーであればこの詳細は問題に
なりませんが、貴方はそれが関係するフレームワーク作者かもしれません。


setUpClass と tearDownClass
~~~~~~~~~~~~~~~~~~~~~~~~~~~

これらは、クラスメソッドとして実装されなければなりません::

    import unittest

    class Test(unittest.TestCase):
        @classmethod
        def setUpClass(cls):
            cls._connection = createExpensiveConnectionObject()

        @classmethod
        def tearDownClass(cls):
            cls._connection.destroy()

基底クラスの ``setUpClass`` および ``tearDownClass`` を使いたいなら、
それらを自分で呼び出さなければなりません。 :class:`TestCase` の実装は
空です。

``setUpClass`` の中で例外が送出されたら、クラス内のテストは実行されず、
``tearDownClass`` も実行されません。スキップされたクラスは ``setUpClass`` も
``tearDownClass`` も実行されません。例外が ``SkipTest`` 例外であると、
そのクラスはエラーとしてではなくスキップされたものとして報告されます。


setUpModule と tearDownModule
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

これらは、関数として実装されなければなりません::

    def setUpModule():
        createConnection()

    def tearDownModule():
        closeConnection()

``setUpModule`` の中で例外が送出されたら、モジュール内のテストは実行されず、
``tearDownModule`` も実行されません。例外が ``SkipTest`` 例外であると、
そのモジュールはエラーとしてではなくスキップされたものとして報告されます。


シグナルハンドリング
--------------------

unittest の :option:`-c/--catch <unittest -c>` コマンドラインオプションや、
:func:`unittest.main()` の ``catchbreak`` パラメタは、テスト実行中の
control-C の処理をよりフレンドリーにします。中断捕捉動作を有効である場合、
control-C が押されると、現在実行されているテストまで完了され、
そのテストランが終わると今までの結果が報告されます。control-C がもう一度
押されると、通常通り :exc:`KeyboardInterrupt` が送出されます。

シグナルハンドラを処理する control-c は、独自の :const:`signal.SIGINT`
ハンドラをインストールするコードやテストの互換性を保とうとします。
``unittest`` ハンドラが呼ばれ、それがインストールされた
:const:`signal.SIGINT` ハンドラで *なければ* 、すなわちテスト中のシステムに
置き換えられて移譲されたなら、それはデフォルトのハンドラを呼び出します。
インストールされたハンドラを置き換えて委譲するようなコードは、
通常その動作を期待するからです。 ``unittest`` の control-c 処理を
無効にしたいような個別のテストには、 :func:`removeHandler` デコレータが
使えます。


フレームワークの作者がテストフレームワーク内で control-c 処理を有効にする
ための、いくつかのユーティリティ関数があります。

.. function:: installHandler()

   control-c ハンドラをインストールします。(主にユーザが control-c を
   押したことにより) :const:`signal.SIGINT` が受け取られると、
   登録した結果すべてに :meth:`~TestResult.stop` が呼び出されます。

   .. versionadded:: 2.7

.. function:: registerResult(result)

   control-c 処理のために :class:`TestResult` を登録します。結果を登録すると
   それに対する弱参照が格納されるので、結果がガベージコレクトされるのを
   妨げません。

   control-c 処理が有効でなければ、 :class:`TestResult` オブジェクトの
   登録には副作用がありません。ですからテストフレームワークは、
   処理が有効か無効かにかかわらず、作成する全ての結果を無条件に登録できます。

   .. versionadded:: 2.7

.. function:: removeResult(result)

   登録された結果を削除します。一旦結果が削除されると、control-c が
   押された際にその結果オブジェクトに対して :meth:`~TestResult.stop` が
   呼び出されなくなります。

   .. versionadded:: 2.7

.. function:: removeHandler(function=None)

   引数なしで呼び出されたとき、control-c ハンドラがインストールされていると、
   この関数はそれを取り除きます。この関数は、テストが実行されている間だけ
   一時的にハンドラを取り除くテストデコレータとしても使えます。

      @unittest.removeHandler
      def test_signal_handling(self):
          ...

   .. versionadded:: 2.7

