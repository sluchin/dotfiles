.. _tarfile-mod:

:mod:`tarfile` --- tar アーカイブファイルを読み書きする
=======================================================

.. module:: tarfile
   :synopsis: tar-形式のアーカイブファイルを読み書きします。


.. versionadded:: 2.3

.. moduleauthor:: Lars Gustäbel <lars@gustaebel.de>
.. sectionauthor:: Lars Gustäbel <lars@gustaebel.de>


.. The :mod:`tarfile` module makes it possible to read and write tar
.. archives, including those using gzip or bz2 compression.
.. (:file:`.zip` files can be read and written using the :mod:`zipfile` module.)

:mod:`tarfile` モジュールは、gzipやbz2圧縮されたものも含めて、tarアーカイブの読み書きができます。
(:file:`.zip` ファイルの読み書きは :mod:`zipfile` モジュールで可能です。)


.. Some facts and figures:

いくつかの事実と外観：


.. * reads and writes :mod:`gzip` and :mod:`bz2` compressed archives.

* :mod:`gzip` と :mod:`bz2` で圧縮されたアーカイブを読み書きします。


.. * read/write support for the POSIX.1-1988 (ustar) format.

* POSIX.1-1988 (ustar) フォーマットの読み書きをサポートしています。


.. * read/write support for the GNU tar format including *longname* and *longlink*
..   extensions, read-only support for the *sparse* extension.

* *longname*, *longlink* 拡張を含めた、GNU tarフォーマットの読み書きをサポートしています。
  *sparse* 拡張は読み込みのみサポートしています。


.. * read/write support for the POSIX.1-2001 (pax) format.

* POSIX.1-2001 (pax) フォーマットの読み書きをサポートしています。

  .. versionadded:: 2.6


.. * handles directories, regular files, hardlinks, symbolic links, fifos,
..   character devices and block devices and is able to acquire and restore file
..   information like timestamp, access permissions and owner.

* ディレクトリ、普通のファイル、ハードリンク、シンボリックリンク、fifo、キャラクタデバイスおよびブロックデバイスを処理します。また、タイムスタンプ、
  アクセス許可およびオーナーのようなファイル情報の取得および保存が可能です。


.. function:: open(name=None, mode='r', fileobj=None, bufsize=10240, \*\*kwargs)

   .. Return a :class:`TarFile` object for the pathname *name*. For detailed
   .. information on :class:`TarFile` objects and the keyword arguments that are
   .. allowed, see :ref:`tarfile-objects`.

   パス名 *name* の :class:`TarFile` オブジェクトを返します。
   :class:`TarFile` オブジェクトと、利用出来るキーワード引数に関する詳細な情報については、
   :ref:`tarfile-objects` 節を参照してください。


   .. *mode* has to be a string of the form ``'filemode[:compression]'``, it defaults
   .. to ``'r'``. Here is a full list of mode combinations:

   *mode* は ``'filemode[:compression]'`` の形式をとる文字列でなければなりません。デフォルトの値は ``'r'``
   です。以下に *mode* のとりうる組み合わせ全てを示します。


   .. +------------------+---------------------------------------------+
   .. | mode             | action                                      |
   .. +==================+=============================================+
   .. | ``'r' or 'r:*'`` | Open for reading with transparent           |
   .. |                  | compression (recommended).                  |
   .. +------------------+---------------------------------------------+
   .. | ``'r:'``         | Open for reading exclusively without        |
   .. |                  | compression.                                |
   .. +------------------+---------------------------------------------+
   .. | ``'r:gz'``       | Open for reading with gzip compression.     |
   .. +------------------+---------------------------------------------+
   .. | ``'r:bz2'``      | Open for reading with bzip2 compression.    |
   .. +------------------+---------------------------------------------+
   .. | ``'a' or 'a:'``  | Open for appending with no compression. The |
   .. |                  | file is created if it does not exist.       |
   .. +------------------+---------------------------------------------+
   .. | ``'w' or 'w:'``  | Open for uncompressed writing.              |
   .. +------------------+---------------------------------------------+
   .. | ``'w:gz'``       | Open for gzip compressed writing.           |
   .. +------------------+---------------------------------------------+
   .. | ``'w:bz2'``      | Open for bzip2 compressed writing.          |
   .. +------------------+---------------------------------------------+

   +----------------------+-----------------------------------------------------------------+
   | mode                 | 動作                                                            |
   +======================+=================================================================+
   | ``'r' または 'r:*'`` | 圧縮方法に関して透過的に、読み込み用にオープンします(推奨)。    |
   +----------------------+-----------------------------------------------------------------+
   | ``'r:'``             | 非圧縮で読み込み用に排他的にオープンします。                    |
   +----------------------+-----------------------------------------------------------------+
   | ``'r:gz'``           | gzip 圧縮で読み込み用にオープンします。                         |
   +----------------------+-----------------------------------------------------------------+
   | ``'r:bz2'``          | bzip2 圧縮で読み込み用にオープンします。                        |
   +----------------------+-----------------------------------------------------------------+
   | ``'a' または 'a:'``  | 非圧縮で追加用にオープンします。ファイルが存在しない            |
   |                      | 場合は新たに作成されます。                                      |
   +----------------------+-----------------------------------------------------------------+
   | ``'w' または 'w:'``  | 非圧縮で書き込み用にオープンします。                            |
   +----------------------+-----------------------------------------------------------------+
   | ``'w:gz'``           | gzip 圧縮で書き込み用にオープンします。                         |
   +----------------------+-----------------------------------------------------------------+
   | ``'w:bz2'``          | bzip2 圧縮で書き込み用にオープンします。                        |
   +----------------------+-----------------------------------------------------------------+


   .. Note that ``'a:gz'`` or ``'a:bz2'`` is not possible. If *mode* is not suitable
   .. to open a certain (compressed) file for reading, :exc:`ReadError` is raised. Use
   .. *mode* ``'r'`` to avoid this.  If a compression method is not supported,
   .. :exc:`CompressionError` is raised.

   ``'a:gz'`` あるいは ``'a:bz2'`` は可能ではないことに注意して下さい。もし
   *mode* が、ある(圧縮した)ファイルを読み込み用にオープンするのに適していないなら、 :exc:`ReadError` が発生します。これを防ぐには
   *mode* ``'r'`` を使って下さい。もし圧縮メソッドがサポートされていなければ、 :exc:`CompressionError` が発生します。


   .. If *fileobj* is specified, it is used as an alternative to a file object opened
   .. for *name*. It is supposed to be at position 0.

   もし *fileobj* が指定されていれば、それは *name* でオープンされたファイルオブジェクトの代替として使うことができます。
   そのファイルオブジェクトの位置が0にあることを前提に動作します。


   .. For special purposes, there is a second format for *mode*:
   .. ``'filemode|[compression]'``.  :func:`tarfile.open` will return a :class:`TarFile`
   .. object that processes its data as a stream of blocks.  No random seeking will
   .. be done on the file. If given, *fileobj* may be any object that has a
   .. :meth:`read` or :meth:`write` method (depending on the *mode*). *bufsize*
   .. specifies the blocksize and defaults to ``20 * 512`` bytes. Use this variant
   .. in combination with e.g. ``sys.stdin``, a socket file object or a tape
   .. device. However, such a :class:`TarFile` object is limited in that it does
   .. not allow to be accessed randomly, see :ref:`tar-examples`.  The currently
   .. possible modes:

   特別な目的のために、 *mode* の2番目の形式: ``'ファイルモード|[圧縮]'`` があります。この形式を使うと、
   :func:`tarfile.open` が返すのはデータをブロックからなるストリームとして扱う :class:`TarFile` オブジェクトになります。この場合、ファイルに対して
   ランダムな seek を行えなくなります。 *fileobj* を指定する場合、 ``read()`` および ``write()``
   メソッドを持つ任意のオブジェクトにできます。 *bufsize* にはブロックサイズを指定します。デフォルトは ``20 * 512``
   バイトです。 ``sys.stdin`` 、ソケットファイルオブジェクト、テープデバイスと組み合わせる場合にはこの形式を
   使ってください。ただし、このような :class:`TarFile` オブジェクトにはランダムアクセスを行えないという制限があります。
   :ref:`tar-examples` 節を参照してください。現在可能なモードは：


   .. +-------------+--------------------------------------------+
   .. | Mode        | Action                                     |
   .. +=============+============================================+
   .. | ``'r|*'``   | Open a *stream* of tar blocks for reading  |
   .. |             | with transparent compression.              |
   .. +-------------+--------------------------------------------+
   .. | ``'r|'``    | Open a *stream* of uncompressed tar blocks |
   .. |             | for reading.                               |
   .. +-------------+--------------------------------------------+
   .. | ``'r|gz'``  | Open a gzip compressed *stream* for        |
   .. |             | reading.                                   |
   .. +-------------+--------------------------------------------+
   .. | ``'r|bz2'`` | Open a bzip2 compressed *stream* for       |
   .. |             | reading.                                   |
   .. +-------------+--------------------------------------------+
   .. | ``'w|'``    | Open an uncompressed *stream* for writing. |
   .. +-------------+--------------------------------------------+
   .. | ``'w|gz'``  | Open an gzip compressed *stream* for       |
   .. |             | writing.                                   |
   .. +-------------+--------------------------------------------+
   .. | ``'w|bz2'`` | Open an bzip2 compressed *stream* for      |
   .. |             | writing.                                   |
   .. +-------------+--------------------------------------------+

   +-------------+-------------------------------------------------------------------+
   | モード      | 動作                                                              |
   +=============+===================================================================+
   | ``'r|*'``   | tar ブロックの *ストリーム* を 圧縮方法に関して透過的に           |
   |             | 読み込み用にオープンします。                                      |
   +-------------+-------------------------------------------------------------------+
   | ``'r|'``    | 非圧縮 tar ブロックの *ストリーム* を読み込み用にオープンします。 |
   +-------------+-------------------------------------------------------------------+
   | ``'r|gz'``  | gzip 圧縮の *ストリーム* を読み込み用にオープンします。           |
   +-------------+-------------------------------------------------------------------+
   | ``'r|bz2'`` | bzip2 圧縮の *ストリーム* を読み込み用にオープンします。          |
   +-------------+-------------------------------------------------------------------+
   | ``'w|'``    | 非圧縮の *ストリーム* を書き込み用にオープンします。              |
   +-------------+-------------------------------------------------------------------+
   | ``'w|gz'``  | gzip 圧縮の *ストリーム* を書き込み用にオープンします。           |
   +-------------+-------------------------------------------------------------------+
   | ``'w|bz2'`` | bzip2 圧縮の *ストリーム* を書き込み用にオープンします。          |
   +-------------+-------------------------------------------------------------------+


.. class:: TarFile

   .. Class for reading and writing tar archives. Do not use this class directly,
   .. better use :func:`tarfile.open` instead. See :ref:`tarfile-objects`.

   tar アーカイブを読んだり、書いたりするためのクラスです。このクラスを直接使わず、代わりに :func:`tarfile.open` を使ってください。
   :ref:`tarfile-objects` を参照してください。


.. function:: is_tarfile(name)

   .. Return :const:`True` if *name* is a tar archive file, that the :mod:`tarfile`
   .. module can read.

   もし *name* が tar アーカイブファイルであり、 :mod:`tarfile` モジュールで読み出せる場合に :const:`True` を返します。


.. class:: TarFileCompat(filename, mode='r', compression=TAR_PLAIN)

   .. Class for limited access to tar archives with a :mod:`zipfile`\ -like interface.
   .. Please consult the documentation of the :mod:`zipfile` module for more details.
   .. *compression* must be one of the following constants:

   :mod:`zipfile` 風なインターフェースを持つ tar アーカイブへの制限されたアクセスのためのクラスです。詳細は
   :mod:`zipfile` のドキュメントを参照してください。 *compression* は、以下の定数のどれかでなければなりません：


   .. data:: TAR_PLAIN

      .. Constant for an uncompressed tar archive.

      非圧縮 tar アーカイブのための定数。


   .. data:: TAR_GZIPPED

      .. Constant for a :mod:`gzip` compressed tar archive.

      :mod:`gzip` 圧縮 tar アーカイブのための定数。


   .. .. deprecated:: 2.6
   ..    The :class:`TarFileCompat` class has been deprecated for removal in Python 3.0.

   .. deprecated:: 2.6
      :class:`TarFileCompat` クラスは、 Python 3.0 で削除されるので、非推奨になりました。


.. exception:: TarError

   .. Base class for all :mod:`tarfile` exceptions.

   すべての :mod:`tarfile` 例外のための基本クラスです。


.. exception:: ReadError

   .. Is raised when a tar archive is opened, that either cannot be handled by the
   .. :mod:`tarfile` module or is somehow invalid.

   tar アーカイブがオープンされた時、 :mod:`tarfile` モジュールで操作できないか、あるいは何か無効であるとき発生します。


.. exception:: CompressionError

   .. Is raised when a compression method is not supported or when the data cannot be
   .. decoded properly.

   圧縮方法がサポートされていないか、あるいはデータを正しくデコードできない時に発生します。


.. exception:: StreamError

   .. Is raised for the limitations that are typical for stream-like :class:`TarFile`
   .. objects.

   ストリーム風の :class:`TarFile` オブジェクトで典型的な制限のために発生します。


.. exception:: ExtractError

   .. Is raised for *non-fatal* errors when using :meth:`TarFile.extract`, but only if
   .. :attr:`TarFile.errorlevel`\ ``== 2``.

   :meth:`TarFile.extract` を使った時に *致命的でない* エラーに対して発生します。
   ただし :attr:`TarFile.errorlevel`\ ``== 2`` の場合に限ります。


.. exception:: HeaderError

   .. Is raised by :meth:`TarInfo.frombuf` if the buffer it gets is invalid.

   :meth:`TarInfo.frombuf` メソッドが、バッファが不正だったときに送出します。

   .. versionadded:: 2.6


.. Each of the following constants defines a tar archive format that the
.. :mod:`tarfile` module is able to create. See section :ref:`tar-formats` for
.. details.

以下の各定数は、 :mod:`tarfile` モジュールが作成できるtarアーカイブフォーマットを定義しています。
詳細は、 :ref:`tar-formats` を参照してください。


.. data:: USTAR_FORMAT

   .. POSIX.1-1988 (ustar) format.

   POSIX.1-1988 (ustar) フォーマット


.. data:: GNU_FORMAT

   .. GNU tar format.

   GNU tar フォーマット


.. data:: PAX_FORMAT

   .. POSIX.1-2001 (pax) format.

   POSIX.1-2001 (pax) フォーマット


.. data:: DEFAULT_FORMAT

   .. The default format for creating archives. This is currently :const:`GNU_FORMAT`.

   アーカイブを作成する際のデフォルトのフォーマット。
   現在は :const:`GNU_FORMAT`


.. The following variables are available on module level:

以下のモジュールレベル変数が利用できます。


.. data:: ENCODING

   .. The default character encoding i.e. the value from either
   .. :func:`sys.getfilesystemencoding` or :func:`sys.getdefaultencoding`.

   デフォルト文字エンコーディング。
   :func:`sys.getfilesystemencoding` か :func:`sys.getdefaultencoding`
   のどちらかの値。


.. seealso::

   .. Module :mod:`zipfile`
   ..    Documentation of the :mod:`zipfile` standard module.

   Module :mod:`zipfile`
      :mod:`zipfile` 標準モジュールのドキュメント。


   .. `GNU tar manual, Basic Tar Format <http://www.gnu.org/software/tar/manual/html_node/Standard.html>`_
   ..    Documentation for tar archive files, including GNU tar extensions.

   `GNU tar マニュアル, 基本 Tar 形式 <http://www.gnu.org/software/tar/manual/html_node/Standard.html>`_
      GNU tar 拡張機能を含む、 tar アーカイブファイルのためのドキュメント。


.. _tarfile-objects:

TarFile オブジェクト
--------------------

.. The :class:`TarFile` object provides an interface to a tar archive. A tar
.. archive is a sequence of blocks. An archive member (a stored file) is made up of
.. a header block followed by data blocks. It is possible to store a file in a tar
.. archive several times. Each archive member is represented by a :class:`TarInfo`
.. object, see :ref:`tarinfo-objects` for details.

:class:`TarFile` オブジェクトは、tar アーカイブへのインターフェースを提供します。 tar
アーカイブは一連のブロックです。アーカイブメンバー(保存されたファイル)は、
ヘッダーブロックとそれに続くデータブロックから構成されています。ある tar
アーカイブにファイルを何回も保存することができます。各アーカイブメンバーは、 :class:`TarInfo`
オブジェクトによって表わされます、詳細については :ref:`tarinfo-objects` を参照してください。

:class:`TarFile` オブジェクトは :keyword:`with` 文によりコンテキストマネージャーとして
利用できます。 with ブロックが終了したときにオブジェクトは close されます。
例外が発生した時、内部で利用されているファイルオブジェクトのみが close され、
開かれたアーカイブは終了されないことに注意してください。
:ref:`tar-examples` セクションにあるユースケースを参照してください。

.. versionadded:: 2.7
   コンテキストマネージャープロトコルがサポートされました。

.. class:: TarFile(name=None, mode='r', fileobj=None, format=DEFAULT_FORMAT, tarinfo=TarInfo, dereference=False, ignore_zeros=False, encoding=ENCODING, errors=None, pax_headers=None, debug=0, errorlevel=0)

   .. All following arguments are optional and can be accessed as instance attributes
   .. as well.

   以下の全ての引数はオプションで、インスタンス属性としてもアクセスすることができます。


   .. *name* is the pathname of the archive. It can be omitted if *fileobj* is given.
   .. In this case, the file object's :attr:`name` attribute is used if it exists.

   *name* はアーカイブのパス名。 *fileobj* が渡された場合は省略可能。
   その場合、ファイルオブジェクトの :attr:`name` 属性があれば、それを利用します。


   .. *mode* is either ``'r'`` to read from an existing archive, ``'a'`` to append
   .. data to an existing file or ``'w'`` to create a new file overwriting an existing
   .. one.

   *mode* は、既存のアーカイブファールから読み込むための ``'r'``,
   既存のアーカイブファイルに追記するための ``'a'``,
   既存のファイルがあれば上書きして新しいファイルを作成する ``'w'``
   のいずれかです。


   .. If *fileobj* is given, it is used for reading or writing data. If it can be
   .. determined, *mode* is overridden by *fileobj*'s mode. *fileobj* will be used
   .. from position 0.

   もし *fileobj* が与えられていれば、それを使ってデータを読み書きします。もしそれが決定できれば、 *mode* は *fileobj*
   のモードで上書きされます。
   *fileobj* は位置0から利用されます。


   .. note::

      .. *fileobj* is not closed, when :class:`TarFile` is closed.

      :class:`TarFile` をクローズする時に、 *fileobj* はクローズされません。


   .. *format* controls the archive format. It must be one of the constants
   .. :const:`USTAR_FORMAT`, :const:`GNU_FORMAT` or :const:`PAX_FORMAT` that are
   .. defined at module level.

   *format* はアーカイブのフォーマットを制御します。
   モジュールレベルで定義されている、 :const:`USTAR_FORMAT`, :const:`GNU_FORMAT`, :const:`PAX_FORMAT`
   のいずれかである必要があります。

   .. versionadded:: 2.6


   .. The *tarinfo* argument can be used to replace the default :class:`TarInfo` class
   .. with a different one.

   *tarinfo* 引数を利用して、デフォルトの :class:`TarInfo` クラスを別のクラスで置き換えられます。


   .. versionadded:: 2.6

   .. If *dereference* is :const:`False`, add symbolic and hard links to the archive. If it
   .. is :const:`True`, add the content of the target files to the archive. This has no
   .. effect on systems that do not support symbolic links.

   *dereference* が :const:`False` だった場合、シンボリックリンクやハードリンクがアーカイブに追加されます。
   :const:`True` だった場合、リンクのターゲットとなるファイルの内容がアーカイブに追加されます。
   シンボリックリンクをサポートしていないシステムでは効果がありません。


   .. todo::
      訳者note: ハードリンクにまで対応している？原文が間違っている可能性があるので要確認。


   .. If *ignore_zeros* is :const:`False`, treat an empty block as the end of the archive.
   .. If it is :const:`True`, skip empty (and invalid) blocks and try to get as many members
   .. as possible. This is only useful for reading concatenated or damaged archives.

   *ignore_zeros* が :const:`False` だった場合、空ブロックをアーカイブの終端だと扱います。
   :const:`True` だった場合、空の(無効な)ブロックをスキップして、可能な限り多くのメンバを取得しようとします。
   このオプションは、連結(concatenate)されたり、壊れたアーカイブファイルを扱うときにのみ、意味があります。


   .. *debug* can be set from ``0`` (no debug messages) up to ``3`` (all debug
   .. messages). The messages are written to ``sys.stderr``.

   *debug* は ``0`` (デバッグメッセージ無し)から ``3`` (全デバッグメッセージ)
   まで設定できます。このメッセージは ``sys.stderr`` に書き込まれます。


   .. If *errorlevel* is ``0``, all errors are ignored when using :meth:`TarFile.extract`.
   .. Nevertheless, they appear as error messages in the debug output, when debugging
   .. is enabled.  If ``1``, all *fatal* errors are raised as :exc:`OSError` or
   .. :exc:`IOError` exceptions. If ``2``, all *non-fatal* errors are raised as
   .. :exc:`TarError` exceptions as well.

   *errorlevel* が ``0`` の場合、 :meth:`TarFile.extract` 使用時に全てのエラーが無視されます。
   エラーが無視された場合でも、 *debug* が有効であれば、エラーメッセージは出力されます。
   ``1`` の場合、全ての *致命的な(fatal)* エラーは :exc:`OSError` か :exc:`IOError` を送出します。
   ``2`` の場合、全ての *致命的でない(non-fatal)* エラーも :exc:`TarError` 例外として送出されます。


   .. The *encoding* and *errors* arguments control the way strings are converted to
   .. unicode objects and vice versa. The default settings will work for most users.
   .. See section :ref:`tar-unicode` for in-depth information.

   *encoding* と *errors* 引数は、文字列と unicode オブジェクトとの間の相互変換方法を指定します。
   デフォルトの設定で、ほとんどのユーザーでうまく動作するでしょう。
   詳しい情報は、 :ref:`tar-unicode` 節を参照してください。

   .. versionadded:: 2.6


   .. The *pax_headers* argument is an optional dictionary of unicode strings which
      will be added as a pax global header if *format* is :const:`PAX_FORMAT`.

   *pax_headers* 引数は、オプションの、 unicode 文字列の辞書で、 *format* が :const:`PAX_FORMAT`
   だった場合に pax グローバルヘッダに追加されます。

   .. versionadded:: 2.6


.. method:: TarFile.open(...)

   .. Alternative constructor. The :func:`tarfile.open` function is actually a
   .. shortcut to this classmethod.

   代替コンストラクタです。モジュールレベルでの :func:`tarfile.open`
   関数は、実際はこのクラスメソッドへのショートカットです。


.. method:: TarFile.getmember(name)

   .. Return a :class:`TarInfo` object for member *name*. If *name* can not be found
   .. in the archive, :exc:`KeyError` is raised.

   メンバー *name* に対する :class:`TarInfo` オブジェクトを返します。もし
   *name* がアーカイブに見つからなければ、 :exc:`KeyError` が発生します。


   .. note::

      .. If a member occurs more than once in the archive, its last occurrence is assumed
      .. to be the most up-to-date version.

      アーカイブ内にメンバーが複数ある場合は、最後に出現するものが最新のバージョンとみなされます。


.. method:: TarFile.getmembers()

   .. Return the members of the archive as a list of :class:`TarInfo` objects. The
   .. list has the same order as the members in the archive.

   :class:`TarInfo` アーカイブのメンバーをオブジェクトのリストとして返します。このリストはアーカイブ内のメンバーと同じ順番です。


.. method:: TarFile.getnames()

   .. Return the members as a list of their names. It has the same order as the list
   .. returned by :meth:`getmembers`.

   メンバーをその名前のリストとして返します。これは :meth:`getmembers` で返されるリストと同じ順番です。


.. method:: TarFile.list(verbose=True)

   .. Print a table of contents to ``sys.stdout``. If *verbose* is :const:`False`,
   .. only the names of the members are printed. If it is :const:`True`, output
   .. similar to that of :program:`ls -l` is produced.

   目次を ``sys.stdout`` に表示します。もし *verbose* が :const:`False`
   であれば、メンバー名のみ表示します。 :const:`True` であれば、 ``"ls -l"`` に似た出力を生成します。


.. method:: TarFile.next()

   .. Return the next member of the archive as a :class:`TarInfo` object, when
   .. :class:`TarFile` is opened for reading. Return :const:`None` if there is no more
   .. available.

   :class:`TarFile` が読み込み用にオープンされている時、アーカイブの次のメンバーを
   :class:`TarInfo` オブジェクトとして返します。もしそれ以上利用可能なものがなければ、 :const:`None` を返します。


.. method:: TarFile.extractall(path=".", members=None)

   .. Extract all members from the archive to the current working directory or
   .. directory *path*. If optional *members* is given, it must be a subset of the
   .. list returned by :meth:`getmembers`. Directory information like owner,
   .. modification time and permissions are set after all members have been extracted.
   .. This is done to work around two problems: A directory's modification time is
   .. reset each time a file is created in it. And, if a directory's permissions do
   .. not allow writing, extracting files to it will fail.

   全てのメンバーをアーカイブから現在の作業ディレクトリーまたは *path* に抽出します。オプションの *members* が与えられるときには、
   :meth:`getmembers` で返されるリストの一部でなければなりません。
   所有者、変更時刻、パーミッションのようなディレクトリー情報は全てのメンバーが抽出された後にセットされます。これは二つの問題を回避するためです。一つはディレクトリー
   の変更時刻はその中にファイルが作成されるたびにリセットされるということ。もう一つは、ディレクトリーに書き込み許可がなければその中のファイル抽出は
   失敗してしまうということです。

   .. warning::

      .. Never extract archives from untrusted sources without prior inspection.
      .. It is possible that files are created outside of *path*, e.g. members
      .. that have absolute filenames starting with ``"/"`` or filenames with two
      .. dots ``".."``.

      内容を信頼できないtarアーカイブを、事前の内部チェック前に展開してはいけません。
      ファイルが *path* の外側に作られる可能性があります。
      例えば、 ``"/"`` で始まる絶対パスのファイル名や、2重ドット ``".."``
      で始まるパスのファイル名です。


   .. versionadded:: 2.5


.. method:: TarFile.extract(member, path="")

   .. Extract a member from the archive to the current working directory, using its
   .. full name. Its file information is extracted as accurately as possible. *member*
   .. may be a filename or a :class:`TarInfo` object. You can specify a different
   .. directory using *path*.

   メンバーをアーカイブから現在の作業ディレクトリに、その完全名を使って抽出します。ファイル情報はできるだけ正確に抽出されます。
   *member* は、ファイル名でも :class:`TarInfo` オブジェクトでも構いません。
   *path* を使って、異なるディレクトリを指定することができます。


   .. note::

      .. The :meth:`extract` method does not take care of several extraction issues.
      .. In most cases you should consider using the :meth:`extractall` method.

      :meth:`extract` メソッドは幾つかの展開に関する問題を扱いません。
      殆どの場合、 :meth:`extractall` メソッドの利用を考慮するべきです。


   .. warning::

      .. See the warning for :meth:`extractall`.

      :meth:`extractall` の警告(warning)を参照


.. method:: TarFile.extractfile(member)

   .. Extract a member from the archive as a file object. *member* may be a filename
   .. or a :class:`TarInfo` object. If *member* is a regular file, a file-like object
   .. is returned. If *member* is a link, a file-like object is constructed from the
   .. link's target. If *member* is none of the above, :const:`None` is returned.

   アーカイブからメンバーをオブジェクトとして抽出します。 *member* は、ファイル名あるいは :class:`TarInfo` オブジェクトです。もし
   *member* が普通のファイルであれば、ファイル風のオブジェクトを返します。もし
   *member* がリンクであれば、ファイル風のオブジェクトをリンクのターゲットから構成します。もし *member* が上のどれでもなければ、
   :const:`None` が返されます。


   .. note::

      .. The file-like object is read-only.  It provides the methods
      .. :meth:`read`, :meth:`readline`, :meth:`readlines`, :meth:`seek`, :meth:`tell`,
      .. and :meth:`close`, and also supports iteration over its lines.

      ファイル風のオブジェクトは読み出し専用です。このオブジェクトは
      :meth:`read`, :meth:`readline`, :meth:`readlines`, :meth:`seek`,
      :meth:`tell`, :meth:`close`. の各メソッドを提供し、
      行に対するイテレーションをサポートします。


.. method:: TarFile.add(name, arcname=None, recursive=True, exclude=None, filter=None)

   ファイル *name* をアーカイブに追加します。
   *name* は、任意のファイルタイプ (ディレクトリ、fifo、シンボリックリンク等)です。
   もし *arcname* が与えられていれば、それはアーカイブ内のファイルの代替名を指定します。
   デフォルトではディレクトリは再帰的に追加されます。
   これは、 *recursive* を :const:`False` に設定することで避けることができます。
   *exclude* を指定する場合、それは1つのファイル名を引数にとってブール値を返す
   関数である必要があります。
   この関数の戻り値が :const:`True` の場合、そのファイルが除外されます。
   :const:`False` の場合、そのファイルは追加されます。
   *filter* を指定する場合、それは :class:`TarInfo` オブジェクトを引数として受け取り、
   操作した :class:`TarInfo` オブジェクトを返す関数でなければなりません。
   代わりに :const:`None` を返した場合、 :class:`TarInfo` オブジェクトは
   アーカイブから除外されます。
   :ref:`tar-examples` にある例を参照してください。

   .. versionchanged:: 2.6
      *exclude* 引数が追加されました。

   .. versionchanged:: 2.7
      *filter* 引数が追加されました。

   .. deprecated:: 2.7
      *exclude* 引数は廃止予定です。代わりに *filter* 引数を利用してください。
      将来 *exclude* 引数が削除されたときに互換性を保つため、
      *filter* は位置引数ではなくてキーワード引数として利用するべきです。


.. method:: TarFile.addfile(tarinfo, fileobj=None)

   .. Add the :class:`TarInfo` object *tarinfo* to the archive. If *fileobj* is given,
   .. ``tarinfo.size`` bytes are read from it and added to the archive.  You can
   .. create :class:`TarInfo` objects using :meth:`gettarinfo`.

   :class:`TarInfo` オブジェクト *tarinfo* をアーカイブに追加します。もし *fileobj*
   が与えられていれば、 ``tarinfo.size``  バイトがそれから読まれ、アーカイブに追加されます。 :meth:`gettarinfo` を使って
   :class:`TarInfo` オブジェクトを作成することができます。


   .. note::

      .. On Windows platforms, *fileobj* should always be opened with mode ``'rb'`` to
      .. avoid irritation about the file size.

      Windows プラットフォームでは、 *fileobj* は、ファイルサイズに関する問題を避けるために、常に、モード ``'rb'``
      でオープンされるべきです。


.. method:: TarFile.gettarinfo(name=None, arcname=None, fileobj=None)

   .. Create a :class:`TarInfo` object for either the file *name* or the file object
   .. *fileobj* (using :func:`os.fstat` on its file descriptor).  You can modify some
   .. of the :class:`TarInfo`'s attributes before you add it using :meth:`addfile`.
   .. If given, *arcname* specifies an alternative name for the file in the archive.

   ファイル *name* あるいはファイルオブジェクト *fileobj* のどちらかに対して
   (そのファイル記述子に ``os.fstat()`` を使って) :class:`TarInfo` オブジェクトを作成します。
   :class:`TarInfo` の属性のいくつかは、 :meth:`addfile` を使って追加する前に修正することができます。
   もし *arcname* が与えられていれば、アーカイブ内のファイルの代替名を指定します。


.. method:: TarFile.close()

   .. Close the :class:`TarFile`. In write mode, two finishing zero blocks are
   .. appended to the archive.

   :class:`TarFile` をクローズします。書き出しモードでは、完了ゼロブロックが 2つ、アーカイブに追加されます。


.. attribute:: TarFile.posix

   .. Setting this to :const:`True` is equivalent to setting the :attr:`format`
   .. attribute to :const:`USTAR_FORMAT`, :const:`False` is equivalent to
   .. :const:`GNU_FORMAT`.

   この値を :const:`True` にすることは、 :attr:`format` を :const:`USTAR_FORMAT` にすることと同じです。
   この値を :const:`False` にすることは、 :attr:`format` を :const:`GNU_FORMAT` にすることと同じです。


   .. .. versionchanged:: 2.4
   ..    *posix* defaults to :const:`False`.

   .. versionchanged:: 2.4
      *posix* のデフォルト値が :const:`False` になりました.


   .. .. deprecated:: 2.6
   ..    Use the :attr:`format` attribute instead.

   .. deprecated:: 2.6
      代わりに :attr:`format` 属性を利用してください。


.. attribute:: TarFile.pax_headers

   .. A dictionary containing key-value pairs of pax global headers.

   pax グローバルヘッダに含まれる key-value ペアの辞書

   .. versionadded:: 2.6


.. _tarinfo-objects:

TarInfo オブジェクト
--------------------

.. A :class:`TarInfo` object represents one member in a :class:`TarFile`. Aside
.. from storing all required attributes of a file (like file type, size, time,
.. permissions, owner etc.), it provides some useful methods to determine its type.
.. It does *not* contain the file's data itself.

:class:`TarInfo` オブジェクトは :class:`TarFile` の一つのメンバーを表します。ファイルに
必要な(ファイルタイプ、ファイルサイズ、時刻、パーミッション、所有者等のような)すべての属性を保存する他に、
そのタイプを決定するのに役に立ついくつかのメソッドを提供します。これにはファイルのデータそのものは *含まれません* 。


.. :class:`TarInfo` objects are returned by :class:`TarFile`'s methods
.. :meth:`getmember`, :meth:`getmembers` and :meth:`gettarinfo`.

:class:`TarInfo` オブジェクトは ``TarFile`` のメソッド ``getmember()`` 、 ``getmembers()`` および
``gettarinfo()`` によって返されます。


.. class:: TarInfo(name="")

   .. Create a :class:`TarInfo` object.

   :class:`TarInfo` オブジェクトを作成します。


.. method:: TarInfo.frombuf(buf)

   .. Create and return a :class:`TarInfo` object from string buffer *buf*.

   :class:`TarInfo` オブジェクトを文字列バッファ *buf* から作成して返します。


   .. .. versionadded:: 2.6
   ..    Raises :exc:`HeaderError` if the buffer is invalid..

   .. versionadded:: 2.6
      バッファが不正な場合は、 :exc:`HeaderError` を送出します。


.. method:: TarInfo.fromtarfile(tarfile)

   .. Read the next member from the :class:`TarFile` object *tarfile* and return it as
      a :class:`TarInfo` object.

   :class:`TarFile` オブジェクトの *tarfile* から、次のメンバを読み込んで、それを
   :class:`TarInfo` オブジェクトとして返します。

   .. versionadded:: 2.6


.. method:: TarInfo.tobuf(format=DEFAULT_FORMAT, encoding=ENCODING, errors='strict')

   .. Create a string buffer from a :class:`TarInfo` object. For information on the
      arguments see the constructor of the :class:`TarFile` class.

   :class:`TarInfo` オブジェクトから文字列バッファを作成します。
   引数についての情報は、 :class:`TarFile` クラスのコンストラクタを参照してください。


   .. .. versionchanged:: 2.6
   ..    The arguments were added.

   .. versionchanged:: 2.6
      引数が追加されました。


.. A ``TarInfo`` object has the following public data attributes:

``TarInfo`` オブジェクトには以下の public なデータ属性があります：


.. attribute:: TarInfo.name

   .. Name of the archive member.

   アーカイブメンバーの名前。


.. attribute:: TarInfo.size

   .. Size in bytes.

   バイト単位でのサイズ。


.. attribute:: TarInfo.mtime

   .. Time of last modification.

   最終更新時刻。


.. attribute:: TarInfo.mode

   .. Permission bits.

   許可ビット。


.. attribute:: TarInfo.type

   .. File type.  *type* is usually one of these constants: :const:`REGTYPE`,
   .. :const:`AREGTYPE`, :const:`LNKTYPE`, :const:`SYMTYPE`, :const:`DIRTYPE`,
   .. :const:`FIFOTYPE`, :const:`CONTTYPE`, :const:`CHRTYPE`, :const:`BLKTYPE`,
   .. :const:`GNUTYPE_SPARSE`.  To determine the type of a :class:`TarInfo` object
   .. more conveniently, use the ``is_*()`` methods below.

   ファイルタイプです。 *type* は普通、以下の定数: :const:`REGTYPE`, :const:`AREGTYPE`,
   :const:`LNKTYPE`, :const:`SYMTYPE`, :const:`DIRTYPE`, :const:`FIFOTYPE`,
   :const:`CONTTYPE`, :const:`CHRTYPE`, :const:`BLKTYPE`, :const:`GNUTYPE_SPARSE`
   のいずれかです。 :class:`TarInfo` オブジェクトのタイプをもっと簡単に決定するには、下記の ``is_*()`` メソッドを使って下さい。


.. attribute:: TarInfo.linkname

   .. Name of the target file name, which is only present in :class:`TarInfo` objects
   .. of type :const:`LNKTYPE` and :const:`SYMTYPE`.

   ターゲットファイル名の名前で、これはタイプ :const:`LNKTYPE` と  :const:`SYMTYPE`
   の :class:`TarInfo` オブジェクトにだけ存在します。


.. attribute:: TarInfo.uid

   .. User ID of the user who originally stored this member.

   ファイルメンバを保存した元のユーザのユーザ ID です。


.. attribute:: TarInfo.gid

   .. Group ID of the user who originally stored this member.

   ファイルメンバを保存した元のユーザのグループ ID です。


.. attribute:: TarInfo.uname

   .. User name.

   ファイルメンバを保存した元のユーザのユーザ名です。


.. attribute:: TarInfo.gname

   .. Group name.

   ファイルメンバを保存した元のユーザのグループ名です。


.. attribute:: TarInfo.pax_headers

   .. A dictionary containing key-value pairs of an associated pax extended header.

   pax 拡張ヘッダに関連付けられた、 key-value ペアの辞書。

   .. versionadded:: 2.6


.. A :class:`TarInfo` object also provides some convenient query methods:

:class:`TarInfo` オブジェクトは便利な照会用のメソッドもいくつか提供しています:


.. method:: TarInfo.isfile()

   .. Return :const:`True` if the :class:`Tarinfo` object is a regular file.

   :class:`Tarinfo` オブジェクトが普通のファイルの場合に、 :const:`True` を返します。


.. method:: TarInfo.isreg()

   .. Same as :meth:`isfile`.

   :meth:`isfile` と同じです。


.. method:: TarInfo.isdir()

   .. Return :const:`True` if it is a directory.

   ディレクトリの場合に :const:`True` を返します。


.. method:: TarInfo.issym()

   .. Return :const:`True` if it is a symbolic link.

   シンボリックリンクの場合に :const:`True` を返します。


.. method:: TarInfo.islnk()

   .. Return :const:`True` if it is a hard link.

   ハードリンクの場合に :const:`True` を返します。


.. method:: TarInfo.ischr()

   .. Return :const:`True` if it is a character device.

   キャラクタデバイスの場合に :const:`True` を返します。


.. method:: TarInfo.isblk()

   .. Return :const:`True` if it is a block device.

   ブロックデバイスの場合に :const:`True` を返します。


.. method:: TarInfo.isfifo()

   .. Return :const:`True` if it is a FIFO.

   FIFO の場合に :const:`True` を返します。


.. method:: TarInfo.isdev()

   .. Return :const:`True` if it is one of character device, block device or FIFO.

   キャラクタデバイス、ブロックデバイスあるいは FIFOのいずれかの場合に :const:`True` を返します。


.. _tar-examples:

例
--


.. How to extract an entire tar archive to the current working directory:

tar アーカイブから現在のディレクトリーに全て抽出する方法


::

   import tarfile
   tar = tarfile.open("sample.tar.gz")
   tar.extractall()
   tar.close()


.. How to extract a subset of a tar archive with :meth:`TarFile.extractall` using
.. a generator function instead of a list:

tarアーカイブの一部を、リストの代わりにジェネレータ関数を利用して、
:meth:`TarFile.extractall` で展開する方法


::

   import os
   import tarfile

   def py_files(members):
       for tarinfo in members:
           if os.path.splitext(tarinfo.name)[1] == ".py":
               yield tarinfo

   tar = tarfile.open("sample.tar.gz")
   tar.extractall(members=py_files(tar))
   tar.close()


.. How to create an uncompressed tar archive from a list of filenames:

非圧縮 tar アーカイブをファイル名のリストから作成する方法


::

   import tarfile
   tar = tarfile.open("sample.tar", "w")
   for name in ["foo", "bar", "quux"]:
       tar.add(name)
   tar.close()

:keyword:`with` 文を利用した同じ例::

    import tarfile
    with tarfile.open("sample.tar", "w") as tar:
        for name in ["foo", "bar", "quux"]:
            tar.add(name)

.. How to read a gzip compressed tar archive and display some member information:

gzip 圧縮 tar アーカイブを作成してメンバー情報のいくつかを表示する方法


::

   import tarfile
   tar = tarfile.open("sample.tar.gz", "r:gz")
   for tarinfo in tar:
       print tarinfo.name, " は大きさが ", tarinfo.size, "バイトで ",
       if tarinfo.isreg():
           print "普通のファイルです。"
       elif tarinfo.isdir():
           print "ディレクトリです。"
       else:
           print "ファイル・ディレクトリ以外のものです。"
   tar.close()


:meth:`TarFile.add` 関数の *filter* 引数を利用してユーザー情報をリセット
しながらアーカイブを作成する例::

    import tarfile
    def reset(tarinfo):
        tarinfo.uid = tarinfo.gid = 0
        tarinfo.uname = tarinfo.gname = "root"
        return tarinfo
    tar = tarfile.open("sample.tar.gz", "w:gz")
    tar.add("foo", filter=reset)
    tar.close()


.. _tar-formats:

サポートされる tar のフォーマット
----------------------------------

.. There are three tar formats that can be created with the :mod:`tarfile` module:

:mod:`tarfile` モジュールは、3つの tar フォーマットを作成することができます。


.. * The POSIX.1-1988 ustar format (:const:`USTAR_FORMAT`). It supports filenames
..   up to a length of at best 256 characters and linknames up to 100 characters. The
..   maximum file size is 8 gigabytes. This is an old and limited but widely
..   supported format.

* POSIX.1-1988 ustar format (:const:`USTAR_FORMAT`). ファイル名の長さは256文字までで、
  リンク名の長さは100文字までです。最大のファイルサイズは8GBです。
  このフォーマットは古くて制限が多いですが、広くサポートされています。


.. * The GNU tar format (:const:`GNU_FORMAT`). It supports long filenames and
..   linknames, files bigger than 8 gigabytes and sparse files. It is the de facto
..   standard on GNU/Linux systems. :mod:`tarfile` fully supports the GNU tar
..   extensions for long names, sparse file support is read-only.

* GNU tar format (:const:`GNU_FORMAT`). 長いファイル名とリンク名、8GBを超えるファイルや\
  スパース(sparse)ファイルをサポートしています。
  これは GNU/Linux システムにおいて、デ・ファクト・スタンダードになっています。
  :mod:`tarfile` モジュールは長いファイル名を完全にサポートしています。
  スパースファイルは読み込みのみサポートしています。


.. * The POSIX.1-2001 pax format (:const:`PAX_FORMAT`). It is the most flexible
..   format with virtually no limits. It supports long filenames and linknames, large
..   files and stores pathnames in a portable way. However, not all tar
..   implementations today are able to handle pax archives properly.

* The POSIX.1-2001 pax format (:const:`PAX_FORMAT`).
  一番柔軟性があり、ほぼ制限が無いフォーマットです。
  長いファイル名やリンク名、大きいファイルをサポートし、パス名をポータブルな方法で保存します。
  しかし、現在のところ、全ての tar の実装が pax フォーマットを正しく扱えるわけではありません。


  .. The *pax* format is an extension to the existing *ustar* format. It uses extra
  .. headers for information that cannot be stored otherwise. There are two flavours
  .. of pax headers: Extended headers only affect the subsequent file header, global
  .. headers are valid for the complete archive and affect all following files. All
  .. the data in a pax header is encoded in *UTF-8* for portability reasons.

  *pax* フォーマットは既存の *ustar* フォーマットの拡張です。
  *ustar* では保存できない情報を追加のヘッダを利用して保存します。
  *pax* には2種類のヘッダがあります。
  1つ目は拡張ヘッダで、その次のファイルヘッダに影響します。
  2つ目はグローバルヘッダで、アーカイブ全体に対して有効で、それ以降の全てのファイルに影響します。
  全ての pax ヘッダの内容は、ポータブル性のために *UTF-8* で保存されます。


.. There are some more variants of the tar format which can be read, but not
.. created:

他にも、読み込みのみサポートしている tar フォーマットが幾つかあります。


.. * The ancient V7 format. This is the first tar format from Unix Seventh Edition,
..   storing only regular files and directories. Names must not be longer than 100
..   characters, there is no user/group name information. Some archives have
..   miscalculated header checksums in case of fields with non-ASCII characters.

* ancient V7 format.
  これは Unix 7th Edition から存在する、最初の tar フォーマットです。
  通常のファイルとディレクトリのみ保存します。
  名前は100文字を超えてはならず、ユーザー/グループ名に関する情報は保存されません。
  幾つかのアーカイブは、フィールドがASCIIでない文字を含む場合に、
  ヘッダのチェックサムの計算を誤っています。


.. * The SunOS tar extended format. This format is a variant of the POSIX.1-2001
..   pax format, but is not compatible.

* The SunOS tar extended format.
  POSIX.1-2001 pax フォーマットの亜流ですが、互換性がありません。


.. _tar-unicode:

Unicode に関する問題
--------------------

.. The tar format was originally conceived to make backups on tape drives with the
.. main focus on preserving file system information. Nowadays tar archives are
.. commonly used for file distribution and exchanging archives over networks. One
.. problem of the original format (that all other formats are merely variants of)
.. is that there is no concept of supporting different character encodings. For
.. example, an ordinary tar archive created on a *UTF-8* system cannot be read
.. correctly on a *Latin-1* system if it contains non-ASCII characters. Names (i.e.
.. filenames, linknames, user/group names) containing these characters will appear
.. damaged.  Unfortunately, there is no way to autodetect the encoding of an
.. archive.

tarフォーマットはもともと、テープドライブにファイルシステムのバックアップを取る目的で設計されました。
現在、tarアーカイブはファイルを配布する場合に一般的に用いられ、ネットワークごしに送受信されます。
オリジナルのフォーマットの抱える1つの問題(ほか多くのフォーマットも同じですが)は、
文字エンコーディングが異なる環境を考慮していないことです。
例えば、通常の *UTF-8* の環境で作成されたアーカイブは、非ASCII文字を含んでいた場合
*Latin-1* のシステムでは正しく読み込むことができません。
非ASCII文字を含む名前(ファイル名、リンク名、ユーザー/グループ名)が破壊されます。
不幸なことに、アーカイブのエンコーディングを自動検出する方法はありません。


.. The pax format was designed to solve this problem. It stores non-ASCII names
.. using the universal character encoding *UTF-8*. When a pax archive is read,
.. these *UTF-8* names are converted to the encoding of the local file system.

pax フォーマットはこの問題を解決するように設計されました。
このフォーマットは、非ASCII文字の名前を *UTF-8* で保存します。
pax アーカイブを読み込むときに、この *UTF-8* の名前がローカルのファイルシステムの\
エンコーディングに変換されます。


.. The details of unicode conversion are controlled by the *encoding* and *errors*
.. keyword arguments of the :class:`TarFile` class.

unicode 変換の動作は、 :class:`TarFile` クラスの *encoding* と *errors*
キーワード引数によって制御されます。


.. The default value for *encoding* is the local character encoding. It is deduced
.. from :func:`sys.getfilesystemencoding` and :func:`sys.getdefaultencoding`. In
.. read mode, *encoding* is used exclusively to convert unicode names from a pax
.. archive to strings in the local character encoding. In write mode, the use of
.. *encoding* depends on the chosen archive format. In case of :const:`PAX_FORMAT`,
.. input names that contain non-ASCII characters need to be decoded before being
.. stored as *UTF-8* strings. The other formats do not make use of *encoding*
.. unless unicode objects are used as input names. These are converted to 8-bit
.. character strings before they are added to the archive.

*encoding* のデフォルト値はローカルの文字エンコーディングです。
これは :func:`sys.getfilesystemencoding` と :func:`sys.getdefaultencoding`
から取得されます。
読み込みモードでは、 *encoding* は pax フォーマット内の unicode
の名前をローカルの文字エンコーディングに変換するために利用されます。
書き込みモードでは、 *encoding* の扱いは選択されたアーカイブフォーマットに依存します。
:const:`PAX_FORMAT` の場合、入力された非ASCII文字を含む文字は *UTF-8*
文字列として保存する前に一旦デコードする必要があるので、そこで *encoding* が利用されます。
それ以外のフォーマットでは、 *encoding* は、入力された名前に unicode が含まれない限りは\
利用されません。unicodeが含まれている場合、アーカイブに保存する前に *encoding*
でエンコードされます。


.. The *errors* argument defines how characters are treated that cannot be
.. converted to or from *encoding*. Possible values are listed in section
.. :ref:`codec-base-classes`. In read mode, there is an additional scheme
.. ``'utf-8'`` which means that bad characters are replaced by their *UTF-8*
.. representation. This is the default scheme. In write mode the default value for
.. *errors* is ``'strict'`` to ensure that name information is not altered
.. unnoticed.

*errors* 引数は、 *encoding* を利用して変換できない文字の扱いを指定します。
利用可能な値は、 :ref:`codec-base-classes` 節でリストアップされています。
読み込みモードでは、追加の値として ``'utf-8'`` を選択することができ、\
エラーが発生したときは *UTF-8* を利用することができます。(これがデフォルトです)
書き込みモードでは、 *errors* のデフォルト値は ``'strict'`` になっていて、\
名前が気づかないうちに変化することが無いようにしています。
