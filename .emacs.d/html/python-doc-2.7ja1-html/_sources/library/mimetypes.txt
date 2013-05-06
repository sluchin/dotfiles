
:mod:`mimetypes` --- ファイル名を MIME 型へマップする
=====================================================

.. module:: mimetypes
   :synopsis: ファイル名拡張子の MIME 型へのマッピング。
.. sectionauthor:: Fred L. Drake, Jr. <fdrake@acm.org>


.. index:: pair: MIME; content type

:mod:`mimetypes` モジュールは、ファイル名あるいは URL と、ファイル名拡張子に関連付けられた MIME 型とを変換します。ファイル名から
MIME 型へと、 MIME 型からファイル名拡張子への変換が提供されます；後者の変換では符号化方式はサポートされていません。

このモジュールは、一つのクラスと多くの便利な関数を提供します。これらの関数がこのモジュールへの標準のインターフェースですが、
アプリケーションによっては、そのクラスにも関係するかもしれません。

以下で説明されている関数は、このモジュールへの主要なインターフェースを提供します。たとえモジュールが初期化されていなくても、もしこれらの関数が、
:func:`init` がセットアップする情報に依存していれば、これらの関数は、 :func:`init` を呼びます。


.. function:: guess_type(filename[, strict])

   .. index:: pair: MIME; headers

   *filename* で与えられるファイル名あるいは URL に基づいて、ファイルの型を推定します。戻り値は、タプル ``(type, encoding)``
   です、ここで  *type* は、もし型が(拡張子がないあるいは未定義のため)推定できない場合は、 ``None`` を、あるいは、 MIME
   :mailheader:`content-type` ヘッダ  に利用できる、 ``'type/subtype'`` の形の文字列です。

   *encoding* は、符合化方式がない場合は ``None`` を、あるいは、符号化に使われるプログラムの名前
   (たとえば、 :program:`compress` あるいは :program:`gzip`)です。符号化方式は
   :mailheader:`Content-Encoding` ヘッダとして使うのに適しており、
   :mailheader:`Content-Transfer-Encoding` ヘッダには適して *いません* 。
   マッピングはテーブルドリブンです。符号化方式のサフィックスは大/小文字を区別します; データ型サフィックスは、最初大/小文字を区別して試し、
   それから大/小文字を区別せずに試します。

   省略可能な *strict* は、既知の MIME 型のリストとして認識されるものが、
   `IANAに登録された <http://www.iana.org/assignments/media-types>`_
   正式な型のみに限定されるかどうかを指定するフラグです。 *strict* が
   true (デフォルト)の時は、IANA 型のみがサポートされます;
   *strict* が false のときは、いくつかの追加の、非標準ではあるが、一般的に\
   使用される MIME 型も認識されます。


.. function:: guess_all_extensions(type[, strict])

   *type* で与えられる MIME 型に基づいてファイルの拡張子を推定します。戻り値は、先頭のドット (``'.'``)を含む、可能なファイル拡張子すべてを
   与える文字列のリストです。拡張子と特別なデータストリームとの関連付けは保証されませんが、 :func:`guess_type` によって MIME型
   *type* とマップされます。

   省略可能な *strict* は :func:`guess_type` 関数のものと同じ意味を持ちます。


.. function:: guess_extension(type[, strict])

   *type* で与えられる MIME 型に基づいてファイルの拡張子を推定します。戻り値は、先頭のドット (``'.'``)を含む、ファイル拡張子を
   与える文字列のリストです。拡張子と特別なデータストリームとの関連付けは保証されませんが、 :func:`guess_type` によって MIME型
   *type* とマップされます。もし *type* に対して拡張子が推定できない場合は、 ``None`` が返されます。

   省略可能な *strict* は :func:`guess_type` 関数のものと同じ意味を持ちます。

モジュールの動作を制御するために、いくつかの追加の関数とデータ項目が利用できます。


.. function:: init([files])

   内部のデータ構造を初期化します。もし  *files* が与えられていれば、
   これはデフォールトの type map を増やすために使われる、一連のファイル名でなければなりません。
   もし省略されていれば、使われるファイル名は :const:`knownfiles` から
   取られます。 Windows であれば、現在のレジストリの設定が読み込まれます。
   *file* あるいは :const:`knownfiles` 内の各ファイル名は、それ以前に現れる
   名前より優先されます。繰り返し :func:`init` を呼び出すことは許されています。

   .. versionchanged:: 2.7
      前のバージョンでは、 Windows のレジストリの設定は無視されていました。

.. function:: read_mime_types(filename)

   ファイル *filename* で与えられた型のマップが、もしあればロードします。
   型のマップは、先頭の dot (``'.'``) を含むファイル名拡張子を、
   ``'type/subtype'`` の形の文字列にマッピングする辞書として返されます。
   もしファイル *filename* が存在しないか、読み込めなければ、
   ``None`` が返されます。


.. function:: add_type(type, ext[, strict])

   mime型 *type* からのマッピングを拡張子 *ext* に追加します。
   拡張子がすでに既知であれば、新しい型が古いものに置き替わります。
   その型がすでに既知であれば、その拡張子が、既知の拡張子のリストに追加されます。

   *strict* が True の時(デフォルト)は、そのマッピングは正式なMIME型に、
   そうでなければ、非標準のMIME型に追加されます。


.. data:: inited

   グローバルなデータ構造が初期化されているかどうかを示すフラグ。これは :func:`init` により true に設定されます。


.. data:: knownfiles

   .. index:: single: file; mime.types

   共通にインストールされた型マップファイル名のリスト。これらのファイルは、普通 :file:`mime.types` という名前であり、パッケージごとに
   異なる場所にインストールされます。


.. data:: suffix_map

   サフィックスをサフィックスにマップする辞書。これは、符号化方式と型が同一拡張子で示される符号化ファイルが認識できるように
   使用されます。例えば、 :file:`.tgz` 拡張子は、符号化と型が別個に認識できるように :file:`.tar.gz` にマップされます。


.. data:: encodings_map

   ファイル名拡張子を符号化方式型にマッピングする辞書


.. data:: types_map

   ファイル名拡張子をMIME型にマップする辞書


.. data:: common_types

   ファイル名拡張子を非標準ではあるが、一般に使われているMIME型にマップする辞書

:class:`MimeTypes` クラスは、1つ以上のMIME-型データベースを必要とするアプリケーションに役に立つでしょう。


.. class:: MimeTypes([filenames])

   このクラスは、MIME-型データベースを表現します。デフォールトでは、このモジュールの他のものと同じデータベースへのアクセスを提供します。
   初期データベースは、このモジュールによって提供されるもののコピーで、追加の :file:`mime.types` \
   -形式のファイルを、 :meth:`read` あるいは :meth:`readfp` メソッドを使って、データベースにロードすることで拡張されます。
   マッピング辞書も、もしデフォールトのデータが望むものでなければ、追加のデータをロードする前にクリアされます。

   省略可能な *filenames* パラメータは、追加のファイルを、デフォールトデータベースの"トップに"ロードさせるのに使うことができます。

   .. versionadded:: 2.2

モジュールの使用例::

   >>> import mimetypes
   >>> mimetypes.init()
   >>> mimetypes.knownfiles
   ['/etc/mime.types', '/etc/httpd/mime.types', ... ]
   >>> mimetypes.suffix_map['.tgz']
   '.tar.gz'
   >>> mimetypes.encodings_map['.gz']
   'gzip'
   >>> mimetypes.types_map['.tgz']
   'application/x-tar-gz'


.. _mimetypes-objects:

Mime型オブジェクト
-------------------

:class:`MimeTypes` インスタンスは、 :mod:`mimetypes` モジュールのそれと非常によく似たインターフェースを提供します。


.. attribute:: MimeTypes.suffix_map

   サフィックスをサフィックスにマップする辞書。これは、符号化方式と型が同一拡張子で示されるような符号化ファイルが認識できるように
   使用されます。例えば、 :file:`.tgz` 拡張子は、符号化方式と型が別個に認識できるように :file:`.tar.gz` に対応づけられます。
   これは、最初はモジュールで定義されたグローバルな ``suffix_map`` のコピーです。


.. attribute:: MimeTypes.encodings_map

   ファイル名拡張子を符号化型にマッピングする辞書。これは、最初はモジュールで定義されたグローバルな ``encodings_map`` のコピーです。


.. attribute:: MimeTypes.types_map

   ファイル名拡張子をMIME型にマッピングするる辞書。これは、最初はモジュールで定義されたグローバルな ``types_map`` のコピーです。


.. attribute:: MimeTypes.common_types

   ファイル名拡張子を非標準ではあるが、一般に使われているMIME型にマップする辞書。これは、最初はモジュールで定義されたグローバルな
   ``common_types`` のコピーです。


.. method:: MimeTypes.guess_extension(type[, strict])

   :func:`guess_extension` 関数と同様ですが、オブジェクトに保存されたテーブルを使用します。


.. method:: MimeTypes.guess_all_extensions(type[, strict])

   :func:`guess_all_extensions` と同様ですが、オブジェクトに保存されたテーブルを参照します。


.. method:: MimeTypes.guess_type(url[, strict])

   :func:`guess_type` 関数と同様ですが、オブジェクトに保存されたテーブルを使用します。


.. method:: MimeTypes.read(path)

   MIME情報を、 *path* という名のファイルからロードします。
   これはファイルを解析するのに :meth:`readfp` を使用します。


.. method:: MimeTypes.readfp(file)

   MIME型情報を、オープンしたファイルからロードします。ファイルは、標準の :file:`mime.types` ファイルの形式でなければなりません。


.. method:: MimeTypes.read_windows_registry()

   MIME type 情報を Windows のレジストリから読み込みます。 Windows でのみ利用できます。

   .. versionadded:: 2.7
