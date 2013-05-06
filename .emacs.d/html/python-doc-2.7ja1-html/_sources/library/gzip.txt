
:mod:`gzip` --- :program:`gzip` ファイルのサポート
==================================================

.. module:: gzip
   :synopsis: ファイルオブジェクトを用いた gzip  圧縮および解凍のためのインタフェース

.. This module provides a simple interface to compress and decompress files just
.. like the GNU programs :program:`gzip` and :program:`gunzip` would.

このモジュールは、ファイルをGNUの :program:`gzip`, :program:`gunzip`
のように圧縮、伸長するシンプルなインタフェースを提供しています。

.. The data compression is provided by the :mod:`zlib` module.

データ圧縮は :mod:`zlib` モジュールで提供されています。

.. The :mod:`gzip` module provides the :class:`GzipFile` class which is modeled
.. after Python's File Object. The :class:`GzipFile` class reads and writes
.. :program:`gzip`\ -format files, automatically compressing or decompressing the
.. data so that it looks like an ordinary file object.

:mod:`gzip` モジュールは、Pythonのファイルオブジェクトに似た
:class:`GzipFile` クラスを提供しています。
:class:`GzipFile` クラスは :program:`gzip` フォーマットのファイルを読み書きします。
自動的にデータを圧縮・伸張するので、外からは通常のファイルオブジェクトのように見えます。

.. Note that additional file formats which can be decompressed by the
.. :program:`gzip` and :program:`gunzip` programs, such  as those produced by
.. :program:`compress` and :program:`pack`, are not supported by this module.

:program:`compress` や :program:`pack` 等によって作られる、 :program:`gzip` や
:program:`gunzip` が伸長できる他のファイルフォーマットについては、
このモジュールは対応していないので注意してください。

.. For other archive formats, see the :mod:`bz2`, :mod:`zipfile`, and
.. :mod:`tarfile` modules.

他のアーカイブフォーマットについては、 :mod:`bz2`, :mod:`zipfile`, :mod:`tarfile`
モジュールを参照してください。


このモジュールでは以下の項目を定義しています:

.. class:: GzipFile([filename[, mode[, compresslevel[, fileobj[, mtime]]]]])

   :class:`GzipFile` クラスのコンストラクタです。
   :class:`GzipFile` オブジェクトは :meth:`readinto` と
   :meth:`truncate` メソッドを除くほとんどのファイルオブジェクトの
   メソッドをシミュレートします。
   少なくとも *fileobj* および *filename* は有効な値でなければなりません。

   クラスの新しいインスタンスは、 *fileobj* に基づいて作成されます。
   *fileobj* は通常のファイル、 :class:`StringIO` オブジェクト、
   そしてその他ファイルをシミュレートできるオブジェクトでかまいません。
   値はデフォルトでは None で、ファイルオブジェクトを生成するために
   *filename* を開きます。

   :program:`gzip` ファイルヘッダ中には、
   ファイルが解凍されたときの元のファイル名を収めることができますが、 *fileobj* が
   ``None``  でない場合、引数 *filename* がファイル名として認識できる文字列であれば、
   *filename* はファイルヘッダに収めるためだけに使われます。
   そうでない場合（この値はデフォルトでは空文字列です）、
   元のファイル名はヘッダに収められません。

   *mode* 引数は、ファイルを読み出すのか、書き込むのかによって、
   ``'r'``, ``'rb'``, ``'a'``, ``'ab'``, ``'w'``,  そして ``'wb'``
   のいずれかになります。
   *fileobj* のファイルモードが認識可能な場合、 *mode* はデフォルトで
   *fileobj* のモードと同じになります。
   そうでない場合、デフォルトのモードは ``'rb'`` です。
   'b' フラグがついていなくても、ファイルがバイナリモードで開かれることを保証するために
   'b' フラグが追加されます。これはプラットフォーム間での移植性のためです。

   *compresslevel* 引数は ``1`` から ``9`` までの整数で、圧縮のレベルを制御します。
   ``1`` は最も高速で最小限の圧縮しか行いません。
   ``9`` は最も低速ですが、最大限の圧縮を行います。デフォルトの値は ``9`` です。

   *mtime* 引数はオプションで、圧縮時にストリームに書かれる数値型のタイムスタンプです。
   :program:`gzip` で圧縮された全てのストリームはタイムスタンプを必要とします。
   省略された場合や ``None`` が渡された場合は、現在の時刻が利用されます。
   このモジュールは伸長時にはタイムスタンプを無視しますが、 :program:`gunzip`
   などのいくつかのプログラムはタイムスタンプを利用します。
   タイムスタンプのフォーマットは ``time.time()`` の戻り値や、 ``os.stat()``
   の戻り値となるオブジェクトの ``st_mtime`` メンバと同じです。

   圧縮したデータの後ろにさらに何か追記したい場合もあるので、
   :class:`GzipFile` オブジェクトの :meth:`close` メソッド呼び出しは
   *fileobj* をクローズしません。
   この機能によって、書き込みのためにオープンした :class:`StringIO` オブジェクトを
   *fileobj* として渡し、(:class:`GzipFile` を :meth:`close` した後に)
   :class:`StringIO` オブジェクトの :meth:`getvalue` メソッドを使って
   書き込んだデータの入っているメモリバッファを取得することができます。

   :class:`GzipFile` はイテレーションと :keyword:`with` 文をサポートします。

   .. versionchanged:: 2.7
      :keyword:`with` 構文のサポートが追加されました。

   .. versionchanged:: 2.7
      zero-pad されたファイルのサポートが追加されました。


.. function:: open(filename[, mode[, compresslevel]])

   ``GzipFile(filename,`` ``mode,`` ``compresslevel)`` の短縮形です。
   引数 *filename* は必須です。
   デフォルトで *mode* は ``'rb'`` に、 *compresslevel* は ``9`` に設定されています。


.. _gzip-usage-examples:

.. Examples of usage

使い方の例
-----------------

..
   Example of how to read a compressed file::

圧縮されたファイルを読み込む例::

   import gzip
   f = gzip.open('/home/joe/file.txt.gz', 'rb')
   file_content = f.read()
   f.close()

..
   Example of how to create a compressed GZIP file::

GZIP圧縮されたファイルを作成する例::

   import gzip
   content = "Lots of content here"
   f = gzip.open('/home/joe/file.txt.gz', 'wb')
   f.write(content)
   f.close()

..
   Example of how to GZIP compress an existing file::

既存のファイルをGZIP圧縮する例::

   import gzip
   f_in = open('/home/joe/file.txt', 'rb')
   f_out = gzip.open('/home/joe/file.txt.gz', 'wb')
   f_out.writelines(f_in)
   f_out.close()
   f_in.close()

.. seealso::

   Module :mod:`zlib`
      :program:`gzip` ファイル形式のサポートを行うために必要な基本ライブラリモジュール。

