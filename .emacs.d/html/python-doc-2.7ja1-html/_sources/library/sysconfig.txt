:mod:`sysconfig` --- Python の構成情報にアクセスする
======================================================

.. module:: sysconfig
   :synopsis: Python's configuration information
.. moduleauthor:: Tarek Ziade <tarek@ziade.org>
.. sectionauthor:: Tarek Ziade <tarek@ziade.org>
.. versionadded:: 2.7
.. index::
   single: configuration information

:mod:`sysconfig` モジュールは、 インストールパスのリストや、現在のプラットフォーム
に関連した構成などの、 Python の構成情報 (configuration information) へのアクセスを
提供します。

.. Configuration variables

構成変数
---------

Python の配布物は、 Python 自体のバイナリや、 :mod:`distutils` によって
コンパイルされる外部のC拡張をビルドするのに必要な、 :file:`Makefile` と
:file:`pyconfig.h` ヘッダーファイルを含んでいます。

:mod:`sysconfig` はこれらのファイルに含まれる全ての変数を辞書に格納し、
:func:`get_config_vars` や :func:`get_config_var` でアクセスできるようにします。

Windows では構成変数はだいぶ少なくなります。

.. function:: get_config_vars(\*args)

   引数がない場合、現在のプラットフォームに関する全ての構成変数の辞書を
   返します。

   引数がある場合、各引数を構成変数辞書から検索した結果の変数のリストを返します。

   各引数において、変数が見つからなかった場合は ``None`` が返されます。


.. function:: get_config_var(name)

   1つの変数 *name* を返します。 ``get_config_vars().get(name)``
   と同じです。

   *name* が見つからない場合、 ``None`` を返します。

使用例::

   >>> import sysconfig
   >>> sysconfig.get_config_var('Py_ENABLE_SHARED')
   0
   >>> sysconfig.get_config_var('LIBDIR')
   '/usr/local/lib'
   >>> sysconfig.get_config_vars('AR', 'CXX')
   ['ar', 'g++']


.. Installation paths

インストールパス
------------------

Python はプラットフォームとインストールオプションに依存して、異なるインストールスキームを
利用します。このスキームは、 :const:`os.name` の値に基づいてユニークな識別子で
:mod:`sysconfig` に格納されます。

:mod:`Distutils` やそれに基づいたシステムによって新しいコンポーネントをインストールするときは、
同じスキームに従ってファイルを正しい場所にコピーします。

Python は現在7つのスキームをサポートしています:

- *posix_prefix*: Linux や Mac OS X などの Posix プラットフォーム用のスキームです。
  これは Python やコンポーネントをインストールするときに使われるデフォルトの
  スキームです。
- *posix_home*: インストール時に *home* オプションが利用された場合における、
  Posix プラットフォーム用のスキームです。このスキームはコンポーネントが
  Distutils に特定の home prefix を指定してインストールされたときに利用されます。
- *posix_user*: Distutils に *user* オプションを指定してコンポーネントを
  インストールするときに使われる、Posix プラットフォーム用のスキームです。
  このスキームはユーザーのホームディレクトリ以下に配置されたパスヲ定義します。
- *nt*: Windows などの NT プラットフォーム用のスキームです。
- *nt_user*: *user* オプションが利用された場合の、 NT プラットフォーム用のスキームです。
- *os2*: OS/2 プラットフォーム用のスキームです。
- *os2_home*: *user* オプションが利用された場合の、 OS/2 プラットフォーム用のスキームです。

各スキームは、ユニークな識別子を持ったいくつかのパスの集合から成っています。
現在 Python は8つのパスを利用します:

- *stdlib*: プラットフォーム非依存の、標準 Python ライブラリファイルを格納するディレクトリ.
- *platstdlib*: プラットフォーム依存の、標準 Python ライブラリファイルを格納するディレクトリ.
- *platlib*: プラットフォーム依存の、 site ごとのファイルを格納するディレクトリ
- *purelib*: プラットフォーム非依存の、 site ごとのファイルを格納するディレクトリ
- *include*: プラットフォーム非依存のヘッダーファイルを格納するディレクトリ
- *platinclude*: プラットフォーム依存の、ヘッダーファイルを格納するディレクトリ
- *scripts*: スクリプトファイルのためのディレクトリ
- *data*: データファイルのためのディレクトリ

:mod:`sysconfig` はこれらのパスを決定するためのいくつかの関数を提供しています。

.. function:: get_scheme_names()

   現在 :mod:`sysconfig` でサポートされている全てのスキームを格納した
   タプルを返します。


.. function:: get_path_names()

   現在 :mod:`sysconfig` でサポートされている全てのパス名を格納した
   タプルを返します。


.. function:: get_path(name, [scheme, [vars, [expand]]])

   *scheme* で指定されたインストールスキームから、 path *name* に従って
   インストールパスを返します。

   *name* は :func:`get_path_names` が返すリストに含まれる値でなければなりません。

   :mod:`sysconfig` はインストールパスを、パス名、プラットフォーム、展開される変数に
   従って格納します。例えば、 *nt* スキームでの *stdlib* パスは ``{base}/Lib``
   になります。

   :func:`get_path` はパスを展開するのに :func:`get_config_vars` が返す変数を利用します。
   全ての変数は各プラットフォームにおいてデフォルト値を持っていて、
   この関数を呼び出したときにデフォルト値を取得する場合があります。

   *scheme* が指定された場合、 :func:`get_scheme_names` が返すリストに含まれる
   値でなければなりません。指定されなかった場合は、現在のプラットフォームでの
   デフォルトスキームが利用されます。

   *bars* が指定された場合、 :func:`get_config_vars` が返す辞書をアップデートする
   変数辞書でなければなりません。

   *expand* が ``False`` に設定された場合、パスは変数を使って展開されません。

   *name* が見つからなかった場合、 ``None`` を返します。


.. function:: get_paths([scheme, [vars, [expand]]])

   インストールスキームに基づいた全てのインストールパスを格納した辞書を返します。
   詳しい情報は :func:`get_path` を参照してください。

   *scheme* が指定された場合、 :func:`get_scheme_names` が返すリストに含まれる
   値でなければなりません。指定されなかった場合は、現在のプラットフォームでの
   デフォルトスキームが利用されます。

   *bars* が指定された場合、 :func:`get_config_vars` が返す辞書をアップデートする
   変数辞書でなければなりません。

   *expand* が ``False`` に設定された場合、パスは変数を使って展開されません。

   *scheme* が実在するスキームでなかった場合、 :func:`get_paths` は :exc:`KeyError`
   を発生させます。


.. Other functions

その他の関数
---------------

.. function:: get_python_version()

   ``MAJOR.MINOR`` の型の Python バージョン番号文字列を返します。
   ``sys.version[:3]`` に似ています。


.. function:: get_platform()

   現在のプラットフォームを識別するための文字列を返します。

   この関数は主に、プラットフォーム依存のビルドディレクトリやビルド済み配布物を
   判別するのに利用します。典型的に、OS名とバージョンと(:func:`os.uname` で提供
   される)アーキテクチャを含みますが、実際の情報はOS依存です。
   例えば、 IRIX ではアーキテクチャは重要ではない(IRIX は SGI のハードでしか
   動きません) のに対して、 Linux ではカーネルバージョンが重要な情報では
   ありません。

   返される値の例:

   - linux-i586
   - linux-alpha (?)
   - solaris-2.6-sun4u
   - irix-5.3
   - irix64-6.2

   Windows では以下のどれかを返します:

   - win-amd64 (64bit Windows on AMD64 (aka x86_64, Intel64, EM64T, etc)
   - win-ia64 (64bit Windows on Itanium)
   - win32 (all others - specifically, sys.platform is returned)

   Mac OS X では以下のどれかを返すかもしれません:

   - macosx-10.6-ppc
   - macosx-10.4-ppc64
   - macosx-10.3-i386
   - macosx-10.4-fat

   その他の非POSIXプラットフォームでは、現在のところ単に :data:`sys.platform`
   を返します。


.. function:: is_python_build()

   現在の Python インストールがソースからビルドされた場合に
   ``True`` を返します。


.. function:: parse_config_h(fp[, vars])

   :file:`config.h` スタイルのファイルを解析します。

   *fp* は :file:`config.h` スタイルのファイルを指すファイルライク
   オブジェクトです。

   name/value ペアを格納した辞書を返します。
   第二引数にオプションの辞書が渡された場合、新しい辞書ではなくその辞書を
   利用し、ファイルから読み込んだ値で更新します。


.. function:: get_config_h_filename()

   :file:`pyconfig.h` のパスを返します。
