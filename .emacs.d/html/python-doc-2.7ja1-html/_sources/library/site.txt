
:mod:`site` --- サイト固有の設定フック
======================================

.. module:: site
   :synopsis: サイト固有のモジュールを参照する標準の方法。


**このモジュールは初期化中に自動的にインポートされます。**
自動インポートはインタプリタの :option:`-S` オプションで禁止できます。

.. index:: triple: module; search; path

このモジュールをインポートすることで、サイト固有のパスをモジュール検索パスへ付け加えます。

.. index::
   pair: site-python; directory
   pair: site-packages; directory

サイト固有のパスは、前部と後部から構築される最大で四つまでのディレクトリから始めます。
前部には、 ``sys.prefix`` と ``sys.exec_prefix`` を使用します。空の前部は省略されます。
後部には、まず空文字列を使い、次に :file:`lib/site-packages` (Windows) または
:file:`lib/python|version|/site-packages`, そして :file:`lib/site-python` (Unix と
Macintosh)を使います。
前部-後部の組み合わせのそれぞれに対して、それが存在するディレクトリを
参照しているかどうかを調べ、もしそうならば ``sys.path`` へ追加します。そして、
パス設定ファイルを新しく追加されたパスからも検索します。

パス設定ファイルは :file:`package.pth` という形式の名前をもつファイルで、
上の4つのディレクトリのひとつにあります。
その内容は ``sys.path`` に追加される追加項目(一行に一つ)です。
存在しない項目は ``sys.path`` へは決して追加されませんが、項目が(ファイルではなく)
ディレクトリを参照しているかどうかはチェックされません。
項目が ``sys.path`` へ二回以上追加されることはありません。
空行と ``#`` で始まる行は読み飛ばされます。
``import`` で始まる(そしてその後ろにスペースかタブが続く)行は実行されます。

.. versionchanged:: 2.6
   import キーワードの後ろにスペースかタブが必要になりました。

   .. A space or tab is now required after the import keyword.

.. index::
   single: package
   triple: path; configuration; file

例えば、 ``sys.prefix`` と ``sys.exec_prefix`` が :file:`/usr/local` に設定されていると仮定します。そのときPython X.Y
ライブラリは :file:`/usr/local/lib/python{X.Y}` にインストールされています(ここで、 ``sys.version`` の最初の三文字だけがインストールパス名を作るために使われます)。ここにはサブディレクトリ :file:`/usr/local/lib/python{X.Y}|/site-packages`
があり、その中に三つのサブディレクトリ :file:`foo`, :file:`bar` および :file:`spam` と二つのパス設定ファイル :file:`foo.pth` と :file:`bar.pth` をもつと仮定します。 :file:`foo.pth` には以下のものが記載されていると想定してください::

   # foo package configuration

   foo
   bar
   bletch

また、 :file:`bar.pth` には::

   # bar package configuration

   bar

が記載されているとします。そのとき、次のバージョンごとのディレクトリが ``sys.path`` へこの順番んで追加されます::

   /usr/local/lib/pythonX.Y/site-packages/bar
   /usr/local/lib/pythonX.Y/site-packages/foo

:file:`bletch` は存在しないため省略されるということに注意してください。 :file:`bar` ディレクトリは :file:`foo` ディレクトリの前に来ます。なぜなら、 :file:`bar.pth` がアルファベット順で :file:`foo.pth` の前に来るからです。また、 :file:`spam` はどちらのパス設定ファイルにも記載されていないため、省略されます。

.. index:: module: sitecustomize

これらのパス操作の後に、 :mod:`sitecustomize` という名前のモジュールをインポートしようします。そのモジュールは任意のサイト固有のカスタマイゼーションを行うことができます。 :exc:`ImportError` 例外が発生してこのインポートに失敗した場合は、何も表示せずに無視されます。

.. index:: module: sitecustomize

いくつかの非Unixシステムでは、 ``sys.prefix`` と ``sys.exec_prefix`` は空で、パス操作は省略されます。しかし、 :mod:`sitecustomize` のインポートはそのときでも試みられます。


.. data:: PREFIXES

   .. A list of prefixes for site package directories

   siteパッケージディレクトリのprefixのリスト

   .. versionadded:: 2.6


.. data:: ENABLE_USER_SITE

   .. Flag showing the status of the user site directory. True means the
      user site directory is enabled and added to sys.path. When the flag
      is None the user site directory is disabled for security reasons.

   ユーザーサイトディレクトリのステータスを示すフラグ。
   Trueの場合、ユーザーサイトディレクトリが有効で sys.path に追加されている
   ことを意味しています。
   None の場合、セキュリティ上の理由でユーザーサイトディレクトリが無効に\
   なっていることを示しています。

   .. versionadded:: 2.6


.. data:: USER_SITE

   .. Path to the user site directory for the current Python version or None

   現在のPythonバージョン用のユーザーサイトディレクトリのパス。もしくは None.

   .. versionadded:: 2.6


.. data:: USER_BASE

   .. Path to the base directory for user site directories

   ユーザーサイトディレクトリのベースディレクトリ

   .. versionadded:: 2.6


.. envvar:: PYTHONNOUSERSITE

   .. versionadded:: 2.6


.. envvar:: PYTHONUSERBASE

   .. versionadded:: 2.6


.. function:: addsitedir(sitedir, known_paths=None)

   .. Adds a directory to sys.path and processes its pth files.

   ディレクトリを sys.path に追加して、その中の pth ファイルも処理する。

.. function:: getsitepackages()

   全てのグローバルの site-packages (site-python が含まれる場合もある)
   ディレクトリを含むリストを返します。

   .. versionadded:: 2.7

.. function:: getuserbase()

   .. Returns the "user base" directory path.

   "ユーザーベース" ディレクトリのパスを返します。

   .. The "user base" directory can be used to store data. If the global
      variable ``USER_BASE`` is not initialized yet, this function will also set
      it.

   "ユーザーベース" ディレクトリをデータを保存するために使うことができます。
   グローバル変数 ``USER_BASE`` がまだ初期化されていない場合、その初期化を行います。

   .. versionadded:: 2.7

.. function:: getusersitepackages()

   .. Returns the user-specific site-packages directory path.

   ユーザー専用の site-packages ディレクトリパスを返します。

   .. If the global variable ``USER_SITE`` is not initialized yet, this
      function will also set it.

   グローバル変数 ``USER_SITE`` が初期化されていない場合、初期化します。

   .. versionadded:: 2.7

.. XXX Update documentation
.. XXX document python -m site --user-base --user-site
