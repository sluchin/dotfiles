
:mod:`pkgutil` --- パッケージ拡張ユーティリティ
===============================================

.. module:: pkgutil
   :synopsis: インポートシステムのユーティリティ

このモジュールはインポートシステムの、特にパッケージサポートに関する
ユーティリティです。

.. versionadded:: 2.3


.. function:: extend_path(path, name)

   パッケージを構成するモジュールの検索パスを拡張します。
   パッケージの :file:`__init__.py` で次のように書くことを意図したものです。 ::

      from pkgutil import extend_path
      __path__ = extend_path(__path__, __name__)

   上記はパッケージの ``__path__`` に ``sys.path`` の全ディレクトリの
   サブディレクトリとしてパッケージ名と同じ名前を追加します。
   これは1つの論理的なパッケージの異なる部品を複数のディレクトリに分けて
   配布したいときに役立ちます。

   同時に :file:`\*.pkg` の ``*`` の部分が *name* 引数に指定された文字列に
   一致するファイルの検索もおこないます。
   この機能は ``import`` で始まる特別な行がないことを除き :file:`\*.pth`
   ファイルに似ています (:mod:`site` の項を参照)。
   :file:`\*.pkg` は重複のチェックを除き、信頼できるものとして扱われます。
   :file:`\*.pkg` ファイルの中に見つかったエントリはファイルシステム上に
   実在するか否かを問わず、そのまますべてパスに追加されます。(このような仕様です。)

   入力パスがリストでない場合(フリーズされたパッケージのとき)は何もせずに
   リターンします。入力パスが変更されていなければ、アイテムを末尾に追加した
   だけのコピーを返します。

   :data:`sys.path` はシーケンスであることが前提になっています。
   :data:`sys.path` の要素の内、実在するディレクトリを指す(ユニコードまた\
   は8ビットの)文字列となっていないものは無視されます。
   ファイル名として使ったときにエラーが発生する :data:`sys.path` のユニコード要素がある場合、
   この関数(:func:`os.path.isdir` を実行している行)で例外が発生する可能性があります。

.. class:: ImpImporter(dirname=None)

   Python の "クラシック" インポートアルゴリズムをラップする :pep:`302` Importer.

   *dirname* が文字列の場合、そのディレクトリを検索する :pep:`302` importer を
   作成します。 *dirname* が ``None`` のとき、現在の :data:`sys.path` と
   フリーズされた、あるいはビルトインの全てのモジュールを検索する :pep:`302`
   importer を作成します。

   :class:`ImpImporter` は現在のところ :data:`sys.meta_path` に配置しての利用を
   サポートしていないことに注意してください。


.. class:: ImpLoader(fullname, file, filename, etc)

   Python の "クラシック" インポートアルゴリズムをラップする :pep:`302` Loader.


.. function:: find_loader(fullname)

   *fullname* に対する :pep:`302` "loader" オブジェクトを検索します。

   *fullname* にドットが含まれる場合、パスはそのモジュールを含むパッケージの
   ``__path__`` でなければなりません。モジュールが見つからない場合や import
   できない場合は ``None`` を返します。
   この関数は :func:`iter_importers` を利用しているので、それと同じように、
   Windows レジストリなどのプラットフォーム依存の特別な import 場所に関する制限が
   あります。


.. function:: get_importer(path_item)

   指定された *path_item* に対する :pep:`302` importer を取得します。

   path hook により新しい importer が作成された場合は、それは
   :data:`sys.path_importer_cache` にキャッシュされます。

   importer が存在しない場合、基本 import 機構のラッパーを返します。
   このラッパーは importer cache にはキャッシュされません。(代わりに ``None``
   が insert されます)

   キャッシュ (やその一部) は、 :data:`sys.path_hooks` のリスキャンが必要になった
   場合は手動でクリアすることができます。


.. function:: get_loader(module_or_name)

   *module_or_name* に対する :pep:`302` "loader" を取得します。

   module か package が通常の import 機構によってアクセスできる場合、その機構の
   該当部分に対するラッパーを返します。モジュールが見つからなかったり import
   できない場合は ``None`` を返します。指定された名前のモジュールがまだ import
   されていない場合、そのモジュールを含むパッケージが(あれば)そのパッケージの
   ``__path__`` を確立するために import されます。

   この関数は :func:`iter_importers` を利用しているので、それと同じように、
   Windows レジストリなどのプラットフォーム依存の特別な import 場所に関する制限が
   あります。


.. function:: iter_importers(fullname='')

   指定されたモジュール名に対する :pep:`302` importer を yield します。

   *fullname* が '.' を含む場合、返される importer は fullname モジュールを
   持つパッケージのものになり、それ以外の場合は :data:`sys.meta_path`, :data:`sys.path`,
   そして Python の "クラシック" import 機構の順のどれかになります。
   指定されたモジュールがパッケージ内にある場合、この関数を実行した副作用として
   そのパッケージが import されます。

   標準の import 機構がファイルを別の場所から探す、 :pep:`302` 以外の機構
   (例: Windows レジストリ) はある程度までサポートされていますが、それは
   :data:`sys.path` の *後に* 検索されます。
   通常、それらの場所は、 :data:`sys.path` のエントリに隠されないように
   :data:`sys.path` の *前に* 検索されます。

   これは動作の違いは問題になり得るので、モジュールやパッケージ名は :data:`sys.path`
   と非 :pep:`302` のファイルシステム機構のどれかとの、どちらからもアクセス
   できる必要があります。こうすると、エミュレート時は前者から、ビルトインの
   import 機構は後者からモジュールやパッケージを見つけます。

   この動作の食い違いにより、以下の種類の要素が影響を受けるかもしれません:
   ``imp.C_EXTENSION``, ``imp.PY_SOURCE``, ``imp.PY_COMPILED``,
   ``imp.PKG_DIRECTORY``.


.. function:: iter_modules(path=None, prefix='')

   *path* があればその全てのサブモジュールに対して、 *path* が ``None`` なら
   ``sys.path`` の全てのトップレベルモジュールに対して、
   ``(module_loader, name, ispkg)`` のタプルを yield します。

   *path* は ``None`` か、モジュールを検索する path のリストの
   どちらかでなければなりません。

   *prefix* は出力の全てのモジュール名の頭に出力する文字列です。


.. function:: walk_packages(path=None, prefix='', onerror=None)

   *path* があれば再帰的にその中のモジュール全てに対して、 *path* が ``None``
   ならばアクセスできる全てのモジュールに対して、 ``(module_loader, name, ispkg)``
   のタプルを yield します。

   *path* は ``None`` か、モジュールを検索するパスのリストでなければなりません。

   *prefix* は出力される全てのモジュール名の先頭に出力されます。

   この関数は与えられた *path* 上の全ての *パッケージ* (全てのモジュール *ではない*)
   を、サブモジュールを検索するのに必要な ``__path__`` 属性にアクセスするために
   import します。

   *onerror* は、パッケージを import しようとしたときに何かの例外が発生した場合に、
   1つの引数(import しようとしていたパッケージの名前)で呼び出される関数です。
   *onerror* 関数が提供されない場合、 :exc:`ImportError` は補足され無視されます。
   それ以外の全ての例外は伝播し、検索を停止させます。

   例::

      # list all modules python can access
      walk_packages()

      # list all submodules of ctypes
      walk_packages(ctypes.__path__, ctypes.__name__ + '.')


.. function:: get_data(package, resource)

   パッケージからリソースを取得します。

   この関数は :pep:`302` ローダーの :func:`get_data` API のラッパーです。
   *package* 引数は標準的なモジュール形式 (``foo.bar``) のパッケージ名で
   なければなりません。
   *resource* 引数は ``/`` をパス区切りに使った相対ファイル名の形式です。
   親ディレクトリを ``..`` としたり、ルートからの (``/`` で始まる) 名前を使うことはできません。

   この関数が返すのは指定されたリソースの内容であるバイナリ文字列です。

   ファイルシステム中に位置するパッケージで既にインポートされているものに対しては、
   次と大体同じです::

       d = os.path.dirname(sys.modules[package].__file__)
       data = open(os.path.join(d, resource), 'rb').read()

   パッケージを見出せなかったりロードできなかったり、あるいは :func:`get_data`
   をサポートしない :pep:`302` ローダーを使ったりした場合は、 ``None`` が返されます。
