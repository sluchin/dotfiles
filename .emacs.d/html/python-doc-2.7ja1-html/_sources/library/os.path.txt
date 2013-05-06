:mod:`os.path` --- 共通のパス名操作
===================================

.. module:: os.path
   :synopsis: Operations on pathnames.

.. index:: single: path; operations

このモジュールには、パス名を操作する便利な関数が定義されています。
ファイルの読み書きに関しては、 :func:`open` 、ファイルシステムへのアク
セスに関しては、 :mod:`os` モジュールを参照下さい。


.. note::

   これらの関数の多くは Windows の一律命名規則 (UNCパス名) を正しくサ
   ポートしていません。 :func:`splitunc` と :func:`ismount` は正しく
   UNC パス名を操作できます。

.. note::

   OSによって異なるパスの決まりがあるので、標準ライブラリにはこのモジュールの
   幾つかのバージョンが含まれています。 :mod:`os.path` モジュールは常に現在
   Pythonが動作しているOSに適したパスモジュールで、ローカルのパスを扱うのに
   適しています。
   各々のモジュールをインポートして *常に* 一つのフォーマットを利用することも
   可能です。これらは全て同じインタフェースを持っています。:

   * :mod:`posixpath` for UNIX-style paths
   * :mod:`ntpath` for Windows paths
   * :mod:`macpath` for old-style MacOS paths
   * :mod:`os2emxpath` for OS/2 EMX paths


.. function:: abspath(path)

   *path* の標準化された絶対パスを返します。たいていのプラットフォーム
   では、 ``normpath(join(os.getcwd(), path))`` と同じ結果になります。

   .. versionadded:: 1.5.2


.. function:: basename(path)

   パス名 *path* の末尾のファイル名を返します。これは ``split(path)``
   で返されるペアの2番目の要素です。この関数が返す値は Unix の
   :program:`basename` とは異なります; Unix の :program:`basename` は
   ``'/foo/bar/'`` に対して ``'bar'`` を返しますが、 :func:`basename`
   は空文字列 (``''``) を返します。


.. function:: commonprefix(list)

   パスの *list* の中の共通する最長のプレフィックスを (パス名の1文字1
   文字を判断して) 返します。
   もし *list* が空なら、空文字列 (``''``) を返します。これは一度に1文
   字を扱うため、不正なパスを返すことがあるかもしれませんので注意して
   下さい。


.. function:: dirname(path)

   パス *path* のディレクトリ名を返します。これは ``split(path)`` で返
   されるペアの最初の要素です。


.. function:: exists(path)

   *path* が存在するなら、 ``True`` を返します。壊れたシンボリックリン
   クについては ``False`` を返します。いくつかのプラットフォームでは、
   たとえ *path* が物理的に存在していたとしても、リクエストされたファ
   イルに対する :func:`os.stat` の実行が許可されなければこの関数が
   ``False`` を返すことがあります。


.. function:: lexists(path)

   *path* が存在するパスなら ``True`` を返す。壊れたシンボリックリンク
   については ``True`` を返します。
   :func:`os.lstat` がない環境では :func:`exists` と同じです。

   .. versionadded:: 2.4


.. function:: expanduser(path)

   Unix 、および、 Windows では、与えられた引数の先頭のパス要素 ``~``
   、または ``~user`` を、 *user* のホームディレクトリのパスに置き換え
   て返します。

   .. index:: module: pwd

   Unix では、先頭の ``~`` は、環境変数 :envvar:`HOME` が設定されてい
   るならその値に置き換えられます。
   そうでなければ、現在のユーザのホームディレクトリをビルトインモジュー
   ル :mod:`pwd` を使ってパスワードディレクトリから探して置き換えます。
   先頭の ``~user`` については、直接パスワードディレクトリから探します。

   Windows では ``~`` だけがサポートされ、環境変数 :envvar:`HOME` または
   :envvar:`HOMEDRIVE` と :envvar:`HOMEPATH` の組み合わせで置き換えら
   れます。

   もし置き換えに失敗したり、引数のパスがチルダで始まっていなかったら、
   パスをそのまま返します。


.. function:: expandvars(path)

   引数のパスの環境変数を展開して返します。引数の中の ``$name`` または
   ``${name}`` のような形式の文字列は環境変数、 *name* に置き換えられます。
   不正な変数名や存在しない変数名の場合には変換されず、そのまま返します。

   Windows では、 ``$name`` や ``${name}`` の形式に加えて、 ``%name%``
   の形式もサポートされています。


.. function:: getatime(path)

   *path* に最後にアクセスした時刻を、エポック (:mod:`time` モジュール
   を参照下さい) からの経過時間を示す秒数で返します。
   ファイルが存在しなかったりアクセスできない場合は :exc:`os.error` を
   送出します。

   .. versionchanged:: 2.3
      :func:`os.stat_float_times` が True を返す場合、戻り値は浮動小数
      点値となります。

   .. versionadded:: 1.5.2


.. function:: getmtime(path)

   *path* の最終更新時刻を、エポック (:mod:`time` モジュールを参照下さ
   い) からの経過時間を示す秒数で返します。
   ファイルが存在しなかったりアクセスできない場合は :exc:`os.error` を
   送出します。

   .. versionchanged:: 2.3
      :func:`os.stat_float_times` が True を返す場合、戻り値は浮動小数点値となります。

   .. versionadded:: 1.5.2


.. function:: getctime(path)

   システムによって、ファイルの最終変更時刻 (Unix のようなシステム) や
   作成時刻 (Windows のようなシステム) をシステムの ctime で返します。
   戻り値はエポック (:mod:`time` モジュールを参照下さい) からの経過秒
   数を示す数値です。
   ファイルが存在しなかったりアクセスできない場合は :exc:`os.error` を
   送出します。

   .. versionadded:: 2.3


.. function:: getsize(path)

   ファイル *path* のサイズをバイト数で返します。ファイルが存在しなかっ
   たりアクセスできない場合は :exc:`os.error` を送出します。

   .. versionadded:: 1.5.2


.. function:: isabs(path)

   *path* が絶対パスなら、 ``True`` を返します。すなわち、 Unix ではス
   ラッシュで始まり、 Windows ではドライブレターに続く (バック) スラッ
   シュで始まる場合です。


.. function:: isfile(path)

   *path* が存在する正しいファイルなら、 *True* を返します。シンボリッ
   クリンクの場合にはその実体をチェックするので、同じパスに対して
   :func:`islink` と :func:`isfile` の両方が *True* を返すことがあり
   ます。


.. function:: isdir(path)

   *path* が存在するなら、 ``True`` を返します。シンボリックリンクの場
   合にはその実体をチェックするので、同じパスに対して :func:`islink`
   と :func:`isdir` の両方が *True* を返すことがあります。


.. function:: islink(path)

   *path* がシンボリックリンクなら、 ``True`` を返します。シンボリック
   リンクがサポートされていないプラットフォームでは、常に ``False``
   を返します。


.. function:: ismount(path)

   パス名 *path* がマウントポイント :dfn:`mount point` (ファイルシステ
   ムの中で異なるファイルシステムがマウントされているところ) なら、
   ``True`` を返します:
   この関数は *path* の親ディレクトリである :file:`path/..` が *path*
   と異なるデバイス上にあるか、あるいは :file:`path/..` と *path* が同
   じデバイス上の同じ i-node を指しているかをチェックします --- これに
   よって全ての Unix と POSIX 標準でマウントポイントが検出できます。


.. function:: join(path1[, path2[, ...]])

   1 つあるいはそれ以上のパスの要素をうまく結合します。付け加える要素
   に絶対パスがあれば、それより前の要素は (Windows ではドライブ名があ
   ればそれも含めて) 全て破棄され、以降の要素を結合します。戻り値は
   *path1* と省略可能な *path2* 以降を結合したもので、 *path2* が空文
   字列でないなら、ディレクトリの区切り文字 (``os.sep``) が各要素の間
   に挿入されます。
   Windows では各ドライブに対してカレントディレクトリがあるので、
   ``os.path.join("c:", "foo")`` によって、 :file:`c:\\foo` ではなく、
   ドライブ :file:`C:` 上のカレントディレクトリからの相対パス
   (:file:`c:foo`) が返されます。


.. function:: normcase(path)

   パス名の大文字、小文字をシステムの標準にします。 Unix と Mac OS X
   ではそのまま返します。
   大文字、小文字を区別しないファイルシステムではパス名を小文字に変換します。
   Windows では、スラッシュをバックスラッシュに変換します。


.. function:: normpath(path)

   パス名を標準化します。余分な区切り文字や上位レベル参照を削除し、
   ``A//B`` 、 ``A/B/`` 、 ``A/./B`` 、 ``A/foo/../B`` が全て ``A/B``
   になるようにします。
   大文字、小文字は標準化しません (それには :func:`normcase` を使って
   下さい) 。 Windows では、スラッシュをバックスラッシュに変換します。
   パスがシンボリックリンクを含んでいるかによって意味が変わることに注
   意してください。


.. function:: realpath(path)

   パスの中のシンボリックリンク (もしそれが当該オペレーティングシステ
   ムでサポートされていれば)を取り除いて、標準化したパスを返します。

   .. versionadded:: 2.2


.. function:: relpath(path[, start])

   カレントディレクトリ、または、オプション引数の *start* から、
   *path* への相対ファイルパスを返します。

   *start* のデフォルト値は :attr:`os.curdir` です。

   利用可能:  Windows 、 Unix

   .. versionadded:: 2.6


.. function:: samefile(path1, path2)

   2つの引数であるパス名が同じファイルあるいはディレクトリを指していれ
   ば (同じデバイスナンバーと i-node ナンバーで示されていれば) 、
   ``True`` を返します。どちらかのパス名で :func:`os.stat` の呼び出し
   に失敗した場合には、例外が発生します。

   利用可能: Unix


.. function:: sameopenfile(fp1, fp2)

   ファイルディスクリプタ *fp1* と *fp2* が同じファイルを指していたら、
   ``True`` を返します。利用可能: Unix


.. function:: samestat(stat1, stat2)

   stat タプル *stat1* と *stat2* が同じファイルを指していたら、
   ``True`` を返します。
   これらのタプルは :func:`fstat` 、 :func:`lstat` や :func:`stat` で
   返されたものでかまいません。
   この関数は、 :func:`samefile` と :func:`sameopenfile` で使われるの
   と同様なものを背後に実装しています。

   利用可能: Unix


.. function:: split(path)

   パス名 *path* を ``(head, tail)`` のペアに分割します。 *tail* はパ
   スの構成要素の末尾で、 *head* はそれより前の部分です。
   *tail* はスラッシュを含みません; もし *path* の最後にスラッシュがあ
   れば、 *tail* は空文字列になります。
   もし *path* にスラッシュがなければ、 *head* は空文字列になります。
   *path* が空文字列なら、 *head* と *tail* のどちらも空文字列になりま
   す。 *head* の末尾のスラッシュは、 *head* がルートディレクトリ (1つ
   以上のスラッシュのみ) でない限り、取り除かれます。
   ``join(head, tail)`` の結果は常に *path* と同じ場所を示すパスになりますが、
   文字列としては異なるかもしれません。


.. function:: splitdrive(path)

   パス名 *path* を ``(drive, tail)`` のペアに分割します。 *drive* はド
   ライブ名か、空文字列です。
   ドライブ名を使用しないシステムでは、 *drive* は常に空文字列です。全
   ての場合に ``drive + tail`` は *path* と等しくなります。

   .. versionadded:: 1.3


.. function:: splitext(path)

   パス名 *path* を ``(root, ext)`` のペアにします。 ``root + ext ==
   path`` になります。
   *ext* は空文字列か1つのピリオドで始まり、多くても1つのピリオドを含みます。
   ベースネームを導出するピリオドは無視されます。 ;  ``splitext('.cshrc')``
   は、 ``('.cshrc', '')`` を返します。

   .. versionchanged:: 2.6
      以前のバージョンでは、最初の文字がピリオドであった場合、空の
      root を生成していました。


.. function:: splitunc(path)

   パス名 *path* をペア ``(unc, rest)`` に分割します。
   ここで *unc* は (``r'\\host\mount'`` のような) UNC マウントポイント、
   そして *rest* は (``r'\path\file.ext'`` のような) パスの残りの部分
   です。ドライブ名を含むパスでは常に *unc* が空文字列になります。

   利用可能: Windows


.. function:: walk(path, visit, arg)

   *path* をルートとする各ディレクトリに対して (もし *path* がディレク
   トリなら *path* も含みます) 、 ``(arg, dirname, names)`` を引数とし
   て関数 *visit* を呼び出します。引数 *dirname* は訪れたディレクトリ
   を示し、引数 *names* はそのディレクトリ内のファイルのリスト
   (``os.listdir(dirname)`` で得られる) です。
   関数 *visit* によって *names* を変更して、 *dirname* 以下の対象とな
   るディレクトリのセットを変更することもできます。例えば、あるディレ
   クトリツリーだけ関数を適用しないなど。 (*names* で参照されるオブジェ
   クトは、 :keyword:`del` あるいはスライスを使って正しく変更しなければなりません。)

   .. note::

      ディレクトリへのシンボリックリンクはサブディレクトリとして扱われ
      ないので、 :func:`walk` による操作対象とはされません。
      ディレクトリへのシンボリックリンクを操作対象とするには、
      ``os.path.islink(file)`` と ``os.path.isdir(file)``
      で識別して、 :func:`walk` で必要な操作を実行しなければなりません。

   .. note::

      この関数は廃止予定で、 3.0 では削除されました。 :func:`os.walk`
      が残っています。


.. data:: supports_unicode_filenames

   任意のユニコード文字列を (ファイルシステムの制限内で) ファイルネームに使う
   ことが可能なら、真を返します。

   .. versionadded:: 2.3

