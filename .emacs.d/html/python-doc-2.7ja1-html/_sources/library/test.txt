
:mod:`test` --- Python用回帰テストパッケージ
============================================

.. module:: test


.. sectionauthor:: Brett Cannon <brett@python.org>

.. note::
   :mod:`test` パッケージは、 Python の内部で使用されるためのものです。
   ドキュメントが書かれたのは Python のコア開発者の利便性を考えてのことです。
   Python の標準ライブラリ以外でこのパッケージを使用することは、
   あまりお勧めできません。
   ここで触れられているコードは、Python のリリースの間に予告なく変更されたり
   削除されたりすることがあります。

:mod:`test` パッケージには、Python 用の全ての回帰テストの他に、
:mod:`test.test_support` モジュールと :mod:`test.regrtest` モジュール
が入っています。 :mod:`test.test_support` はテストを充実させるために使い、
:mod:`test.regrtest` はテストスイートを実行するのに使います。

:mod:`test` パッケージ内のモジュールのうち、名前が ``test_`` で始まるものは、
特定のモジュールや機能に対するテストスイートです。
新しいテストはすべて :mod:`unittest` か :mod:`doctest` モジュールを使って
書くようにしてください。古いテストのいくつかは、
``sys.stdout`` への出力を比較する「従来の」テスト形式になっていますが、
この形式のテストは廃止予定です。


.. seealso::

   Module :mod:`unittest`
      PyUnit 回帰テストを書く。

   Module :mod:`doctest`
      ドキュメンテーション文字列に埋め込まれたテスト。


.. _writing-tests:

:mod:`test` パッケージのためのユニットテストを書く
--------------------------------------------------

:mod:`unittest` モジュールを使ってテストを書く場合、幾つかのガイドラインに
従うことが推奨されます。
1つは、テストモジュールの名前を、 ``test_`` で始め、テスト対象となる
モジュール名で終えることです。
テストモジュール中のテストメソッドは名前を ``test_`` で始めて、
そのメソッドが何をテストしているかという説明で終えます。
これはテスト実行プログラムが、そのメソッドをテストメソッドとして
認識するために必要です。
また、テストメソッドにはドキュメンテーション文字列を入れるべきではありません。
コメント（例えば ``# True あるいは False だけを返すテスト関数`` ）を使用して、 
テストメソッドのドキュメントを記述してください。
これは、ドキュメンテーション文字列が存在する場合はその内容が出力されてしまうため、
どのテストを実行しているのかをいちいち表示したくないからです。

以下のような決まり文句を使います::

   import unittest
   from test import test_support

   class MyTestCase1(unittest.TestCase):

       # Only use setUp() and tearDown() if necessary

       def setUp(self):
           ... code to execute in preparation for tests ...

       def tearDown(self):
           ... code to execute to clean up after tests ...

       def test_feature_one(self):
           # Test feature one.
           ... testing code ...

       def test_feature_two(self):
           # Test feature two.
           ... testing code ...

       ... more test methods ...

   class MyTestCase2(unittest.TestCase):
       ... same structure as MyTestCase1 ...

   ... more test classes ...

   def test_main():
       test_support.run_unittest(MyTestCase1,
                                 MyTestCase2,
                                 ... list other tests ...
                                 )

   if __name__ == '__main__':
       test_main()

この定型的なコードによって、テストスイートを :mod:`regrtest.py` から
起動できると同時に、スクリプト自体からも実行できるようになります。

回帰テストの目的はコードを解き明かすことです。
そのためには以下のいくつかのガイドラインに従ってください:

* テストスイートから、すべてのクラス、関数および定数を実行するべきです。
  これには外部に公開される外部APIだけでなく「プライベートな」コードも含みます。

* ホワイトボックス・テスト（対象のコードの詳細を元にテストを書くこと）を
  推奨します。ブラックボックス・テスト
  （公開されるインタフェース仕様だけをテストすること）は、
  すべての境界条件を確実にテストするには完全ではありません。

* すべての取りうる値を、無効値も含めてテストするようにしてください。
  そのようなテストを書くことで、全ての有効値が通るだけでなく、
  不適切な値が正しく処理されることも確認できます。

* コード内のできる限り多くのパスを網羅してください。
  分岐するように入力を調整したテストを書くことで、
  コードの多くのパスをたどることができます。

* テスト対象のコードにバグが発見された場合は、
  明示的にテスト追加するようにしてください。
  そのようなテストを追加することで、将来コードを変更した
  際にエラーが再発することを防止できます。

* テストの後始末（例えば一時ファイルをすべて閉じたり削除したりすること）を
  必ず行ってください。

* テストがオペレーティングシステムの特定の状況に依存する場合、
  テスト開始時に条件を満たしているかを検証してください。

* import するモジュールをできるかぎり少なくし、可能な限り早期に
  import を行ってください。そうすることで、てテストの外部依存性を
  最小限にし、モジュールの import による副作用から生じる変則的な動作を
  最小限にできます。

* できる限りテストコードを再利用するようにしましょう。
  時として、入力の違いだけを記述すれば良くなるくらい、
  テストコードを小さくすることができます。
  例えば以下のように、サブクラスで入力を指定することで、
  コードの重複を最小化することができます::

     class TestFuncAcceptsSequences(unittest.TestCase):

         func = mySuperWhammyFunction

         def test_func(self):
             self.func(self.arg)

     class AcceptLists(TestFuncAcceptsSequences):
         arg = [1, 2, 3]

     class AcceptStrings(TestFuncAcceptsSequences):
         arg = 'abc'

     class AcceptTuples(TestFuncAcceptsSequences):
         arg = (1, 2, 3)


.. seealso::

   Test Driven Development
      コードより前にテストを書く方法論に関する Kent Beck の著書


.. _regrtest:

コマンドラインインタフェースを利用してテストを実行する
---------------------------------------------------------

:mod:`test.regrtest` はスクリプトとして Python の回帰テストスイートを実行できます。
:option:`-m` オプションを利用して、 :program:`python -m test.regrtest` として実行します。
スクリプトを実行すると、自動的に :mod:`test` パッケージ内の
すべての回帰テストを実行し始めます。パッケージ内の名前が ``test_`` で始まる
全モジュールを見つけ、それをインポートし、もしあるなら関数 :func:`test_main` を
実行してテストを行います。
実行するテストの名前もスクリプトに渡される可能性があります。
単一の回帰テストを指定  (:program:`python -m test.regrtest test_spam`)
すると、出力を最小限にします。テストが成功したかあるいは
失敗したかだけを出力するので、出力は最小限になります。

直接 :mod:`test.regrtest` を実行すると、テストに利用するリソースを設定できます。
これを行うには、 :option:`-u` コマンドラインオプションを使います。
すべてのリソースを使うには、 :program:`python -m test.regrtest -uall`
を実行します。 :option:`-u` のオプションに ``all`` を指定すると、
すべてのリソースを有効にします。(よくある場合ですが)
何か一つを除く全てが必要な場合、カンマで区切った不要なリソースのリストを
``all`` の後に並べます。
コマンド :program:`python -m test.regrtest -uall,-audio,-largefile`
とすると、 ``audio`` と ``largefile`` リソースを除く
全てのリソースを使って :mod:`test.regrtest` を実行します。
すべてのリソースのリストと追加のコマンドラインオプションを出力するには、
:program:`python -m test.regrtest -h` を実行してください。

テストを実行しようとするプラットフォームによっては、回帰テストを実行する
別の方法があります。 Unix では、Python をビルドしたトップレベルディレクトリで
:program:`make test` を実行できます。
Windows上では、 :file:`PCBuild` ディレクトリから :program:`rt.bat` を実行すると、
すべての回帰テストを実行します。


:mod:`test.test_support` --- テストのためのユーティリティ関数
-------------------------------------------------------------

.. module:: test.test_support
   :synopsis: Python 回帰テストのサポート

.. note::
   :mod:`test.test_support` モジュールは、Python 3では :mod:`test.support` に
   リネームされました。

:mod:`test.test_support` モジュールでは、 Python の回帰テストに対するサポートを
提供しています。

このモジュールは次の例外を定義しています:


.. exception:: TestFailed

   テストが失敗したとき送出される例外です。
   これは、 :mod:`unittest` ベースのテストでは廃止予定で、
   :class:`unittest.TestCase`
   の assertXXX メソッドが推奨されます。


.. exception:: ResourceDenied

   :exc:`unittest.TestSkipped` のサブクラスです。（ネットワーク接続のような）リソースが
   利用できないとき送出されます。
   :func:`requires` 関数によって送出されます。

:mod:`test.test_support` モジュールでは、以下の定数を定義しています:


.. data:: verbose

   冗長な出力が有効な場合は :const:`True` です。
   実行中のテストについてのより詳細な情報が欲しいときにチェックします。
   *verbose* は :mod:`test.regrtest` によって設定されます。


.. data:: have_unicode

   ユニコードサポートが利用可能ならば :const:`True` になります。


.. data:: is_jython

   実行中のインタプリタが Jython ならば :const:`True` になります。


.. data:: TESTFN

   テンポラリファイルの名前として安全に利用できる名前に設定されます。
   作成した一時ファイルは全て閉じ、unlink (削除) せねばなりません。

:mod:`test.test_support` モジュールでは、以下の関数を定義しています:


.. function:: forget(module_name)

   モジュール名 *module_name* を :mod:`sys.modules` から取り除き、
   モジュールのバイトコンパイル済みファイルを全て削除します。


.. function:: is_resource_enabled(resource)

   *resource* が有効で利用可能ならば :const:`True` を返します。
   利用可能なリソースのリストは、 :mod:`test.regrtest` がテストを実行している
   間のみ設定されます。


.. function:: requires(resource[, msg])

   *resource* が利用できなければ、 :exc:`ResourceDenied` を送出します。
   その場合、 *msg* は :exc:`ResourceDenied` の引数になります。
   ``__name__`` が ``"__main__"`` である関数にから呼び出された場合には
   常に :const:`True` を返します。
   テストを :mod:`test.regrtest` から実行するときに使われます。


.. function:: findfile(filename)

   *filename* という名前のファイルへのパスを返します。
   一致するものが見つからなければ、 *filename* 自体を返します。
   *filename* 自体もファイルへのパスでありえるので、
   *filename* が返っても失敗ではありません。


.. function:: run_unittest(*classes)

   渡された :class:`unittest.TestCase` サブクラスを実行します。
   この関数は名前が ``test_`` で始まるメソッドを探して、
   テストを個別に実行します。

   引数に文字列を渡すことも許可されています。その場合、文字列は ``sys.module``
   のキーでなければなりません。
   指定された各モジュールは、 ``unittest.TestLoader.loadTestsFromModule()``
   でスキャンされます。
   この関数は、よく次のような :func:`test_main` 関数の形で利用されます。 ::

      def test_main():
          test_support.run_unittest(__name__)

   この関数は、名前で指定されたモジュールの中の全ての定義されたテストを
   実行します。


.. function:: check_warnings(*filters, quiet=True)

   warning が正しく発行されているかどうかチェックする、
   :func:`warnings.catch_warnings()` を使いやすくするラッパーです。
   これは、 :meth:`warnings.simplefilter` を ``always`` に設定して、
   記録された結果を自動的に検証するオプションと共に
   ``warnings.catch_warnings(record=True)`` を呼ぶのとほぼ同じです。

   ``check_warnings`` は ``("message regexp", WarningCategory)`` の形をした
   2要素タプルをポジション引数として受け取ります。1つ以上の *filters* が
   与えられた場合や、オプションのキーワード引数 *quiet* が :const:`False`
   の場合、警告が期待通りであるかどうかをチェックします。
   指定された各 filter は最低でも1回は囲われたコード内で発生した警告と
   マッチしなければテストが失敗しますし、指定されたどの filter ともマッチしない
   警告が発生してもテストが失敗します。前者のチェックを無効にするには、
   *quiet* を :const:`True` にします。

   引数が1つもない場合、デフォルトでは次のようになります::

      check_warnings(("", Warning), quiet=True)

   この場合、全ての警告は補足され、エラーは発生しません。

   コンテキストマネージャーに入る時、 :class:`WarningRecorder` インスタンスが
   返されます。このレコーダーオブジェクトの :attr:`warnings` 属性から、
   :func:`~warnings.catch_warnings` から得られる警告のリストを取得することができます。
   便利さのために、レコーダーオブジェクトから直接、一番最近に発生した
   警告を表すオブジェクトの属性にアクセスできます(以下にある例を参照してください)。
   警告が1つも発生しなかった場合、それらの全ての属性は :const:`None` を返します。

   レコーダーオブジェクトの :meth:`reset` メソッドは警告リストをクリアします。

   コンテキストマネージャーは次のようにして使います::

      with check_warnings(("assertion is always true", SyntaxWarning),
                          ("", UserWarning)):
          exec('assert(False, "Hey!")')
          warnings.warn(UserWarning("Hide me!"))

   この場合、どちらの警告も発生しなかった場合や、それ以外の警告が発生した場合は、
   :func:`check_warnings` はエラーを発生させます。

   警告が発生したかどうかだけでなく、もっと詳しいチェックが必要な場合は、
   次のようなコードになります::

      with check_warnings(quiet=True) as w:
          warnings.warn("foo")
          assert str(w.args[0]) == "foo"
          warnings.warn("bar")
          assert str(w.args[0]) == "bar"
          assert str(w.warnings[0].args[0]) == "foo"
          assert str(w.warnings[1].args[0]) == "bar"
          w.reset()
          assert len(w.warnings) == 0

   全ての警告をキャプチャし、テストコードがその警告を直接テストします。

   .. versionadded:: 2.6
   .. versionchanged:: 2.7
      新しいオプション引数 *filters* と *quiet*


.. function:: check_py3k_warnings(*filters, quiet=False)

   :func:`check_warnings` と似ていますが、 Python 3 互換性警告のためのものです。
   ``sys.py3kwarning == 1`` の時、警告が実際に発生していることをチェックします。
   ``sys.py3kwarning == 0`` の時、警告が発生していないことをチェックします。
   ポジション引数として ``("message regexp", WarningCategory)`` の形をした
   2要素タプルを受け取ります。
   オプション引数 *quiet* が :const:`True` のとき、filter になにもマッチ
   しなくても失敗しません。引数がない場合は次と同じになります::

      check_py3k_warnings(("", DeprecationWarning), quiet=False)

   .. versionadded:: 2.7

.. function:: captured_stdout()

   これは、 :keyword:`with` 文の body で ``sys.stdout`` として
   :class:`StringIO.StringIO` オブジェクトを利用するコンテキストマネージャーです。
   このオブジェクトは、 :keyword:`with` 文の ``as`` 節で受け取ることができます。

   使用例::

      with captured_stdout() as s:
          print "hello"
      assert s.getvalue() == "hello"

   .. versionadded:: 2.6


.. function:: import_module(name, deprecated=False)

   この関数は *name* で指定されたモジュールを import して返します。
   通常の import と異なり、この関数はモジュールを import できなかった
   場合に :exc:`unittest.SkipTest` 例外を発生させます。

   *deprecated* が :const:`True` の場合、 import 中はモジュールとパッケージの
   廃止メッセージが抑制されます。

   .. versionadded:: 2.7


.. function:: import_fresh_module(name, fresh=(), blocked=(), deprecated=False)

   この関数は、 *name* で指定された Python モジュールを、 import 前に
   ``sys.modules`` から削除することで新規に import してそのコピーを返します。
   :func:`reload` 関数と違い、もとのモジュールはこの操作によって影響をうけません。

   *fresh* は、同じように import 前に ``sys.modules`` から削除される
   モジュール名の iterable です。

   *blocked* もモジュール名の iterable で、 import 中にモジュールキャッシュ内で
   その名前を :const:`0` に置き換えることで、そのモジュールを import しようとすると
   :exc:`ImportError` を発生させます。

   指定されたモジュールと *fresh* や *blocked* 引数内のモジュール名は
   import 前に保存され、 fresh import が完了したら ``sys.modules``
   に戻されます。

   *deprecated* が :const:`True` の場合、 import 中はモジュールとパッケージの
   廃止メッセージが抑制されます。

   この関数はモジュールを import できなかった場合に
   :exc:`unittest.SkipTest` 例外を発生させます。

   使用例::

      # Get copies of the warnings module for testing without
      # affecting the version being used by the rest of the test suite
      # One copy uses the C implementation, the other is forced to use
      # the pure Python fallback implementation
      py_warnings = import_fresh_module('warnings', blocked=['_warnings'])
      c_warnings = import_fresh_module('warnings', fresh=['_warnings'])

   .. versionadded:: 2.7


:mod:`test.test_support` モジュールは以下のクラスを定義しています。

.. class:: TransientResource(exc[, **kwargs])

   このクラスのインスタンスはコンテキストマネージャーで、
   指定された型の例外が発生した場合に :exc:`ResourceDenied` 例外を発生させます。
   キーワード引数は全て、 :keyword:`with` 文の中で発生した全ての例外の
   属性名/属性値と比較されます。
   全てのキーワード引数が例外の属性に一致した場合に、
   :exc:`ResourceDenied` 例外が発生します。

   .. versionadded:: 2.6

.. class:: EnvironmentVarGuard()

   一時的に環境変数をセット・アンセットするためのクラスです。
   このクラスのインスタンスはコンテキストマネージャーとして利用されます。
   また、 ``os.environ`` に対する参照・更新を行う完全な辞書のインタフェースを
   持ちます。コンテキストマネージャーが終了した時、このインスタンス経由で
   環境変数へ行った全ての変更はロールバックされます。

   .. versionadded:: 2.6
   .. versionchanged:: 2.7
      辞書のインタフェースを追加しました。


.. method:: EnvironmentVarGuard.set(envvar, value)

   一時的に、 ``envvar`` を ``value`` にセットします。


.. method:: EnvironmentVarGuard.unset(envvar)

   一時的に ``envvar`` をアンセットします。

.. class:: WarningsRecorder()

   ユニットテスト時にwarningを記録するためのクラスです。
   上の、 :func:`check_warnings` のドキュメントを参照してください。

   .. versionadded:: 2.6

