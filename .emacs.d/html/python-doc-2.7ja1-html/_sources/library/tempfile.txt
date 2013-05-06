
:mod:`tempfile` --- 一時的なファイルやディレクトリの生成
========================================================

.. sectionauthor:: Zack Weinberg <zack@codesourcery.com>


.. module:: tempfile
   :synopsis: 一時的なファイルやディレクトリを生成。


.. index::
   pair: temporary; file name
   pair: temporary; file

このモジュールを使うと、一時的なファイルやディレクトリを生成できます。このモジュールはサポートされている全てのプラットフォームで利用可能です。

バージョン 2.3 の Python では、このモジュールに対してセキュリティを高める為の見直しが行われました。現在では新たに 3 つの関数、
:func:`NamedTemporaryFile` 、 :func:`mkstemp` 、および :func:`mkdtemp` が提供されており、安全でない
:func:`mktemp`  を使いつづける必要をなくしました。このモジュールで生成される一時ファイルはもはやプロセス番号を含みません; その代わり、6
桁のランダムな文字からなる文字列が使われます。

また、ユーザから呼び出し可能な関数は全て、一時ファイルの場所や名前を直接操作できるようにするための追加の引数をとるようになりました。もはや変数
*tempdir* および *template* を使う必要はありません。以前のバージョンとの互換性を維持するために、引数の順番は多少変です;
明確さのためにキーワード引数を使うことをお勧めします。

このモジュールではユーザから呼び出し可能な以下の関数を定義しています:


.. function:: TemporaryFile([mode='w+b'[, bufsize=-1[, suffix=''[, prefix='tmp'[, dir=None]]]]])

   一時的な記憶領域として使うことができるファイルライク(file-like)オブジェクトを返します。ファイルは :func:`mkstemp` を使って
   生成されます。このファイルは閉じられると (オブジェクトがガーベジコレクションされた際に、暗黙のうちに閉じられる場合を含みます)
   すぐに消去されます。Unix環境では、ファイルが生成されるとすぐにそのファイルのディレクトリエントリは除去されてしまいます。一方、他の
   プラットフォームではこの機能はサポートされていません; 従って、コードを書くときには、この関数で作成した一時ファイルをファイルシステム上で見る
   ことができる、あるいはできないということをあてにすべきではありません。

   生成されたファイルを一旦閉じなくてもファイルを読み書きできるようにするために、 *mode* パラメタは標準で ``'w+b'`` に設定されています。
   ファイルに記録するデータが何であるかに関わらず全てのプラットフォームで一貫性のある動作をさせるために、バイナリモードが使われています。 *bufsize*
   の値は標準で ``-1`` で、これはオペレーティングシステムにおける標準の値を使うことを意味しています。

   *dir* 、 *prefix* および *suffix* パラメタは :func:`mkstemp` に渡されます。

   .. The returned object is a true file object on POSIX platforms.  On other
      platforms, it is a file-like object whose :attr:`!file` attribute is the
      underlying true file object. This file-like object can be used in a
      :keyword:`with` statement, just like a normal file.

   返されるオブジェクトは、POSIXプラットフォームでは本物のfileオブジェクトです。
   それ以外のプラットフォームではファイルライクオブジェクトが返され、
   :attr:`!file` 属性に本物のfileオブジェクトがあります。
   このファイルライクオブジェクトは、通常のファイルと同じように :keyword:`with`
   文で利用することができます。

.. function:: NamedTemporaryFile([mode='w+b'[, bufsize=-1[, suffix=''[, prefix='tmp'[, dir=None[, delete=True]]]]]])

   この関数はファイルがファイルシステム上で見ることができるよう保証されている点を除き、 :func:`TemporaryFile` と全く同じに働きます。
   (Unixでは、ディレクトリエントリはunlinkされません) ファイル名はファイルオブジェクトの :attr:`name` メンバから
   取得することができます。このファイル名を使って一時ファイルをもう一度開くとことができるかどうかは、プラットフォームによって異なります。
   (Unixでは可能でしたが、Windows NT以降では開く事ができません。)
   *delete* がtrue(デフォルト)の場合、ファイルは閉じられるとすぐに削除されます。

   .. The returned object is always a file-like object whose :attr:`!file`
      attribute is the underlying true file object. This file-like object can
      be used in a :keyword:`with` statement, just like a normal file.

   返されるオブジェクトは、常にファイルライクオブジェクトです。
   このオブジェクトの :attr:`!file` 属性が本物のfileオブジェクトになります。
   このファイルライクオブジェクトは、通常のファイルと同じように :keyword:`with`
   文を利用することができます。

   .. versionadded:: 2.3

   .. versionadded:: 2.6
      *delete* 引数

.. function:: SpooledTemporaryFile([max_size=0, [mode='w+b'[, bufsize=-1[, suffix=''[, prefix='tmp'[, dir=None]]]]]])

   .. This function operates exactly as :func:`TemporaryFile` does, except that
      data is spooled in memory until the file size exceeds *max_size*, or
      until the file's :func:`fileno` method is called, at which point the
      contents are written to disk and operation proceeds as with
      :func:`TemporaryFile`.

   この関数は、ファイルサイズが *max_size* を超えるか、 :func:`fileno`
   メソッドが呼ばれるまでの間メモリ上で処理される以外は、
   :func:`TemporaryFile` と同じです。
   *max_size* を超えるか :func:`fileno` が呼ばれたとき、一時ファイルの内容が\
   ディスクに書き込まれ、その後の処理は :func:`TemporaryFile` で行われます。

   .. The resulting file has one additional method, :func:`rollover`, which
      causes the file to roll over to an on-disk file regardless of its size.

   この関数が返すファイルは、追加で1つのメソッド :func:`rollover` を持っています。
   このメソッドが呼ばれると、(サイズに関係なく)メモリからディスクへのロールオーバーが\
   実行されます。

   .. The returned object is a file-like object whose :attr:`_file` attribute
      is either a :class:`StringIO` object or a true file object, depending on
      whether :func:`rollover` has been called. This file-like object can be
      used in a :keyword:`with` statement, just like a normal file.

   返されるオブジェクトはファイルライクオブジェクトで、その :attr:`_file`
   属性は、 :func:`rollover` が呼ばれたかどうかによって、 :class:`StringIO`
   オブジェクトか、本物のファイルオブジェクトになります。
   このファイルライクオブジェクトは、通常のファイルオブジェクトと同じように、
   :keyword:`with` 文で利用することができます。

   .. versionadded:: 2.6



.. function:: mkstemp([suffix=''[, prefix='tmp'[, dir=None[, text=False]]]])

   可能な限り最も安全な手段で一時ファイルを生成します。使用するプラットフォームで :func:`os.open` の :const:`O_EXCL`
   フラグが正しく実装されている限り、ファイルの生成で競合条件が起こることはありません。このファイルは、ファイルを生成したユーザのユーザ ID
   からのみ読み書き可能です。使用するプラットフォームにおいて、ファイルを実行可能かどうかを示す許可ビットが使われている場合、
   ファイルは誰からも実行不可なように設定されます。このファイルのファイル記述子は子プロセスに継承されません。

   :func:`TemporaryFile` と違って、 :func:`mkstemp` で生成された
   ファイルが用済みになったときにファイルを消去するのはユーザの責任です。

   *suffix* が指定された場合、ファイル名は指定されたsuffixで終わります。そうでない場合にはsuffixは付けられません。 :func:`mkstemp`
   はファイル名とsuffixの間にドットを追加しません; 必要なら、 *suffix* の先頭につけてください。

   *prefix* が指定された場合、ファイル名は指定されたプレフィクス(接頭文字列) で始まります; そうでない場合、標準のプレフィクスが使われます。

   .. If *dir* is specified, the file will be created in that directory;
      otherwise, a default directory is used.  The default directory is chosen
      from a platform-dependent list, but the user of the application can
      control the directory location by setting the *TMPDIR*, *TEMP* or *TMP*
      environment variables.  There is thus no guarantee that the generated
      filename will have any nice properties, such as not requiring quoting
      when passed to external commands via ``os.popen()``.

   *dir* が指定された場合、一時ファイルは指定されたディレクトリ下に作成されます; そうでない場合、標準のディレクトリが使われます。
   デフォルトのディレクトリは、プラットフォームごとに異なるリストから選ばれます。
   しかし、アプリケーションのユーザーは *TMPDIR*, *TEMP*, *TMP* 環境変数を設定することで、その場所を設定することができます。
   そのため、生成されたファイル名について、クォート無しで ``os.popen()``
   を使って外部コマンドに渡せるかどうかなどの保証はありません。

   *text* が指定された場合、ファイルをバイナリモード (標準の設定)  かテキストモードで開くかを示します。使用するプラットフォームによっては
   この値を設定しても変化はありません。

   :func:`mkstemp` は開かれたファイルを扱うための OS レベルの値とファイルの絶対パス名が順番に並んだタプルを返します。

   .. versionadded:: 2.3


.. function:: mkdtemp([suffix[, prefix[, dir]]])

   可能な限り安全な方法で一時ディレクトリを作成します。ディレクトリの生成で競合条件は発生しません。ディレクトリを作成したユーザ ID だけが、このディレクトリ
   に対して内容を読み出したり、書き込んだり、検索したりすることができます。

   :func:`mkdtemp` によって作られたディレクトリとその内容が用済みになった時、にそれを消去するのはユーザの責任です。

   *prefix* 、 *suffix* 、および *dir* 引数は :func:`mkstemp` のものと同じです。

   :func:`mkdtemp` は新たに生成されたディレクトリの絶対パス名を返します。

   .. versionadded:: 2.3


.. function:: mkdtemp([suffix=''[, prefix='tmp'[, dir=None]]])

   ..
       Creates a temporary directory in the most secure manner possible. There
       are no race conditions in the directory's creation.  The directory is
       readable, writable, and searchable only by the creating user ID.

   可能な限り最もセキュアな方法で、一時ディレクトリを作成します。
   ディレクトリの作成時に、競合状態はありません。
   作成されたディレクトリは、作成したユーザーIDのみで、読み込み可能で、書き込み可能で、
   検索可能です。

   ..
      The user of :func:`mkdtemp` is responsible for deleting the temporary
      directory and its contents when done with it.

   :func:`mkdtemp` 関数のユーザーは、作成された一時ディレクトリとその中身を
   削除する責任があります。

   ..
      The *prefix*, *suffix*, and *dir* arguments are the same as for
      :func:`mkstemp`.

   *prefix*, *suffix*, *dir* 引数は :func:`mkstemp` 関数と同じです。

   .. :func:`mkdtemp` returns the absolute pathname of the new directory.

   :func:`mkdtemp` は生成したディレクトリの絶対パスを返します。

   .. versionadded:: 2.3


.. function:: mktemp([suffix=''[, prefix='tmp'[, dir=None]]])

   .. deprecated:: 2.3
      代わりに :func:`mkstemp` を使って下さい.

   一時ファイルの絶対パス名を返します。このパス名は少なくともこの関数が呼び出された時点ではファイルシステム中に存在しなかったパス名です。
   *prefix* 、 *prefix* 、 *suffix* 、および *dir* 引数は :func:`mkstemp` のものと同じです。

   .. warning::

      この関数を使うとプログラムのセキュリティホールになる可能性があります。この関数が返したファイル名を返した後、あなたがそのファイル名
      を使って次に何かをしようとする段階に至る前に、誰か他の人間があなたにパンチをくらわせてしまうかもしれません。
      :func:`mktemp` の利用は、 :func:`NamedTemporaryFile` に ``delete=False``
      引数を渡すことで、簡単に置き換えることができます。 ::

         >>> f = NamedTemporaryFile(delete=False)
         >>> f
         <open file '<fdopen>', mode 'w+b' at 0x384698>
         >>> f.name
         '/var/folders/5q/5qTPn6xq2RaWqk+1Ytw3-U+++TI/-Tmp-/tmpG7V1Y0'
         >>> f.write("Hello World!\n")
         >>> f.close()
         >>> os.unlink(f.name)
         >>> os.path.exists(f.name)
         False

このモジュールでは、一時的なファイル名の作成方法を指定する 2 つのグローバル変数を使います。これらの変数は上記のいずれかの関数を最初
に呼び出した際に初期化されます。関数呼び出しをおこなうユーザはこれらの値を変更することができますが、これはお勧めできません;
その代わりに関数に適切な引数を指定してください。


.. data:: tempdir

   この値が ``None`` 以外に設定された場合、このモジュールで定義されている関数全ての *dir* 引数に対する標準の設定値となります。

   *tempdir* が設定されていないか ``None`` の場合、上記のいずれかの関数を呼び出した際は常に、Python は標準的なディレクトリ候補のリスト
   を検索し、関数を呼び出しているユーザの権限でファイルを作成できる最初のディレクトリ候補を *tempdir* に設定します。リストは以下の
   ようになっています:

   #. 環境変数 :envvar:`TMPDIR` で与えられているディレクトリ名。

   #. 環境変数 :envvar:`TEMP` で与えられているディレクトリ名。

   #. 環境変数 :envvar:`TMP` で与えられているディレクトリ名。

   #. プラットフォーム依存の場所:

      * RiscOS では環境変数 :envvar:`Wimp$ScrapDir` で与えられているディレクトリ名。

      * Windows ではディレクトリ :file:`C:\\TEMP` 、 :file:`C:\\TMP` 、 :file:`\\TEMP` 、および
              :file:`\\TMP` の順。

      * その他の全てのプラットフォームでは、 :file:`/tmp` 、 :file:`/var/tmp` 、および :file:`/usr/tmp` の順。

   #. 最後の手段として、現在の作業ディレクトリ。


.. function:: gettempdir()

   現在選択されている、テンポラリファイルを作成するためのディレクトリを返します。
   :data:`tempdir` が ``None`` でない場合、単にその内容を返します;
   そうでない場合には上で記述されている検索が実行され、その結果が返されます。

   .. versionadded:: 2.3


.. data:: template

   .. deprecated:: 2.0
      代わりに :func:`gettempprefix` を使ってください。

   この値に ``None`` 以外の値を設定した場合、 :func:`mktemp` が返すファイル名のディレクトリ部を含まない先頭部分 (プレフィクス) を
   定義します。ファイル名を一意にするために、 6 つのランダムな文字および数字がこのプレフィクスの後に追加されます。
   デフォルトのプレフィックスは :file:`tmp` です。

   このモジュールの古いバージョンでは、 :func:`os.fork` を呼び出した後に ``template`` を ``None``
   に設定することが必要でした;  この仕様はバージョン 1.5.2 からは必要なくなりました。


.. function:: gettempprefix()

   一時ファイルを生成する際に使われるファイル名の先頭部分を返します。この先頭部分にはディレクトリ部は含まれません。変数 *template*
   を直接読み出すよりもこの関数を使うことを勧めます。

   .. versionadded:: 1.5.2

