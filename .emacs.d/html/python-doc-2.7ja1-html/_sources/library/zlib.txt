
:mod:`zlib` --- :program:`gzip` 互換の圧縮
==========================================

.. module:: zlib
   :synopsis: gzip 互換の圧縮／解凍ルーチンへの低レベルインタフェース


.. For applications that require data compression, the functions in this module
.. allow compression and decompression, using the zlib library. The zlib library
.. has its own home page at http://www.zlib.net.   There are known
.. incompatibilities between the Python module and versions of the zlib library
.. earlier than 1.1.3; 1.1.3 has a security vulnerability, so we recommend using
.. 1.1.4 or later.

このモジュールでは、データ圧縮を必要とするアプリケーションが zlib ライブラリを使って圧縮および解凍を行えるようにします。 zlib ライブラリ自体の
Webページは http://www.zlib.net です。 Pythonモジュールと zlib
ライブラリの1.1.3より前のバージョンには互換性のない部分があることが知られています。1.1.3にはセキュリティホールが存
在しますので、1.1.4以降のバージョンを利用することをお勧めします。


.. zlib's functions have many options and often need to be used in a particular
.. order.  This documentation doesn't attempt to cover all of the permutations;
.. consult the zlib manual at http://www.zlib.net/manual.html for authoritative
.. information.

zlib の関数にはたくさんのオプションがあり、しばしば特定の順番で使う必要があります。
このドキュメントでは順番のことについて全てを説明し尽くそうとはしていません。信頼できる情報が必要ならば
http://www.zlib.net/manual.html にある zlib のマニュアルを参照するようにしてください。


.. For reading and writing ``.gz`` files see the :mod:`gzip` module. For
.. other archive formats, see the :mod:`bz2`, :mod:`zipfile`, and
.. :mod:`tarfile` modules.

``.gz`` ファイルの読み書きのためには、 :mod:`gzip` モジュールを参照してください。
その他のアーカイブフォーマットについては、 :mod:`bz2`, :mod:`zipfile`, :mod:`tarfile`
モジュールを参照してください。


.. The available exception and functions in this module are:

このモジュールで利用可能な例外と関数を以下に示します:


.. exception:: error

   .. Exception raised on compression and decompression errors.

   圧縮および解凍時のエラーによって送出される例外。


.. function:: adler32(data[, value])

   .. Computes a Adler-32 checksum of *data*.  (An Adler-32 checksum is almost as
   .. reliable as a CRC32 but can be computed much more quickly.)  If *value* is
   .. present, it is used as the starting value of the checksum; otherwise, a fixed
   .. default value is used.  This allows computing a running checksum over the
   .. concatenation of several inputs.  The algorithm is not cryptographically
   .. strong, and should not be used for authentication or digital signatures.  Since
   .. the algorithm is designed for use as a checksum algorithm, it is not suitable
   .. for use as a general hash algorithm.

   *data* のAdler-32 チェックサムを計算します。（Adler-32 チェックサムは、おおむね CRC32 と同等の信頼性を持ちながら
   はるかに高速に計算することができます。） *value* が与えられていれば、 *value* はチェックサム計算の
   初期値として使われます。それ以外の場合には固定のデフォルト値が使われます。この機能によって、複数の入力を結合したデータ全体
   にわたり、通しのチェックサムを計算することができます。このアルゴリズムは暗号論的には強力とはいえないので、認証やデジタル
   署名などに用いるべきではありません。このアルゴリズムはチェックサムアルゴリズムとして用いるために設計されたものなので、汎用的な
   ハッシュアルゴリズムには向きません。


   .. This function always returns an integer object.

   この関数は常に整数オブジェクトを返します。


.. note::

   .. To generate the same numeric value across all Python versions and
   .. platforms use adler32(data) & 0xffffffff.  If you are only using
   .. the checksum in packed binary format this is not necessary as the
   .. return value is the correct 32bit binary representation
   .. regardless of sign.

   全てのPythonのバージョンとプラットフォームで共通な数値を生成するには、
   ``adler32(data) & 0xffffffff`` を利用してください。
   もしチェックサムをパックされたバイナリフォーマットのためにしか利用しないのであれば、
   符号が関係なくなり、32bitのバイナリ値としては戻り値は正しいので、この処理は必要ありません。


.. .. versionchanged:: 2.6
..    The return value is in the range [-2**31, 2**31-1]
..    regardless of platform.  In older versions the value is
..    signed on some platforms and unsigned on others.

.. versionchanged:: 2.6
   戻り値の範囲は、プラットフォームに関係なく [-2**31, 2**31-1] になりました。
   古いバージョンでは、この値は幾つかのプラットフォームでは符号付き、
   別のプラットフォームでは符号なしになっていました。


.. .. versionchanged:: 3.0
..    The return value is unsigned and in the range [0, 2**32-1]
..    regardless of platform.

.. versionchanged:: 3.0
   戻り値の範囲は、プラットフォームに関係なく [0, 2**32-1] です。


.. function:: compress(string[, level])

   .. Compresses the data in *string*, returning a string contained compressed data.
   .. *level* is an integer from ``1`` to ``9`` controlling the level of compression;
   .. ``1`` is fastest and produces the least compression, ``9`` is slowest and
   .. produces the most.  The default value is ``6``.  Raises the :exc:`error`
   .. exception if any error occurs.

   *string* で与えられた文字列を圧縮し、圧縮されたデータを含む文字列を返します。 *level* は ``1`` から ``9`` までの
   整数をとる値で、圧縮のレベルを制御します。 ``1`` は最も高速で最小限の圧縮を行います。 ``9`` はもっとも低速になりますが
   最大限の圧縮を行います。デフォルトの値は ``6`` です。圧縮時に何らかのエラーが発生した場合、 :exc:`error` 例外を送出します。


.. function:: compressobj([level])

   .. Returns a compression object, to be used for compressing data streams that won't
   .. fit into memory at once.  *level* is an integer from ``1`` to ``9`` controlling
   .. the level of compression; ``1`` is fastest and produces the least compression,
   .. ``9`` is slowest and produces the most.  The default value is ``6``.

   一度にメモリ上に置くことができないようなデータストリームを圧縮するための圧縮オブジェクトを返します。 *level* は ``1`` から ``9``
   までの整数で、圧縮レベルを制御します。 ``1`` はもっとも高速で最小限の圧縮を、 ``9`` はもっとも低速になりますが
   最大限の圧縮を行います。デフォルトの値は ``6`` です。


.. function:: crc32(data[, value])

   .. index::
      single: Cyclic Redundancy Check
      single: checksum; Cyclic Redundancy Check


   .. Computes a CRC (Cyclic Redundancy Check)  checksum of *data*. If *value* is
   .. present, it is used as the starting value of the checksum; otherwise, a fixed
   .. default value is used.  This allows computing a running checksum over the
   .. concatenation of several inputs.  The algorithm is not cryptographically
   .. strong, and should not be used for authentication or digital signatures.  Since
   .. the algorithm is designed for use as a checksum algorithm, it is not suitable
   .. for use as a general hash algorithm.

   *data* の CRC (Cyclic Redundancy Check, 巡回符号方式)   チェックサムを計算します。 *value*
   が与えられていれば、チェックサム計算の初期値として使われます。与えられていなければデフォルトの初期値が使われます。 *value*
   を与えることで、複数の入力を結合したデータ全体にわたり、通しのチェックサムを計算することができます。
   このアルゴリズムは暗号論的には強力ではなく、認証やデジタル署名に用いるべきではありません。アルゴリズムはチェックサムアルゴリズムと
   して設計されているので、汎用のハッシュアルゴリズムには向きません。


   .. This function always returns an integer object.

   この関数は常に整数オブジェクトを返します。


.. note::

   .. To generate the same numeric value across all Python versions and
   .. platforms use crc32(data) & 0xffffffff.  If you are only using
   .. the checksum in packed binary format this is not necessary as the
   .. return value is the correct 32bit binary representation
   .. regardless of sign.

   全てのPythonのバージョンとプラットフォームで共通な数値を生成するには、
   ``crc32(data) & 0xffffffff`` を利用してください。
   もしチェックサムをパックされたバイナリフォーマットのためにしか利用しないのであれば、
   符号が関係なくなり、32bitのバイナリ値としては戻り値は正しいので、この処理は必要ありません。


.. .. versionchanged:: 2.6
..    The return value is in the range [-2**31, 2**31-1]
..    regardless of platform.  In older versions the value would be
..    signed on some platforms and unsigned on others.

.. versionchanged:: 2.6
   戻り値の範囲は、プラットフォームに関係なく [-2**31, 2**31-1] になりました。
   古いバージョンでは、この値は幾つかのプラットフォームでは符号付き、
   別のプラットフォームでは符号なしになっていました。


.. .. versionchanged:: 3.0
..    The return value is unsigned and in the range [0, 2**32-1]
..    regardless of platform.

.. versionchanged:: 3.0
   戻り値の範囲は、プラットフォームに関係なく [0, 2**32-1] です。


.. function:: decompress(string[, wbits[, bufsize]])

   .. Decompresses the data in *string*, returning a string containing the
   .. uncompressed data.  The *wbits* parameter controls the size of the window
   .. buffer, and is discussed further below.
   .. If *bufsize* is given, it is used as the initial size of the output
   .. buffer.  Raises the :exc:`error` exception if any error occurs.

   *string* 内のデータを解凍して、解凍されたデータを含む文字列を返します。 *wbits* パラメータはウィンドウバッファの大きさを制御します。より詳しい説明は後で行います。
   *bufsize* が与えられていれば、出力バッファの初期サイズとして使われます。解凍処理に何らかのエラーが生じた場合、 :exc:`error`
   例外を送出します。


   .. The absolute value of *wbits* is the base two logarithm of the size of the
   .. history buffer (the "window size") used when compressing data.  Its absolute
   .. value should be between 8 and 15 for the most recent versions of the zlib
   .. library, larger values resulting in better compression at the expense of greater
   .. memory usage.  When decompressing a stream, *wbits* must not be smaller
   .. than the size originally used to compress the stream; using a too-small
   .. value will result in an exception. The default value is therefore the
   .. highest value, 15.  When *wbits* is negative, the standard
   .. :program:`gzip` header is suppressed.

   *wbits* の絶対値は、データを圧縮する際に用いられるヒストリバッファのサイズ (ウィンドウサイズ) に対し、 2 を底とする対数を
   とったものです。最近のほとんどのバージョンの zlib ライブラリを使っているなら、 *wbits* の絶対値は 8 から 15 とするべきです。
   より大きな値はより良好な圧縮につながりますが、より多くのメモリを必要とします。
   ストリームを解凍するとき、 *wbits* は元のストリームを圧縮するために使用した
   サイズより小さくしてはいけません。小さすぎる値を使用すると例外が発生します。
   そのため、デフォルトの値は 15 です。 *wbits* の値が負の場合、標準的な
   :program:`gzip` ヘッダを出力しません。


   .. *bufsize* is the initial size of the buffer used to hold decompressed data.  If
   .. more space is required, the buffer size will be increased as needed, so you
   .. don't have to get this value exactly right; tuning it will only save a few calls
   .. to :cfunc:`malloc`.  The default size is 16384.

   *bufsize* は解凍されたデータを保持するためのバッファサイズの初期値です。バッファの空きは必要に応じて必要なだけ増加するので、
   必ずしも正確な値を指定する必要はありません。この値のチューニングでできることは、 :c:func:`malloc` が呼ばれる回数を
   数回減らすことぐらいです。デフォルトのサイズは 16384 です。


.. function:: decompressobj([wbits])

   .. Returns a decompression object, to be used for decompressing data streams that
   .. won't fit into memory at once.  The *wbits* parameter controls the size of the
   .. window buffer.

   メモリ上に一度に展開できないようなデータストリームを解凍するために用いられる解凍オブジェクトを返します。 *wbits* パラメータは
   ウィンドウバッファのサイズを制御します。


.. Compression objects support the following methods:

圧縮オブジェクトは以下のメソッドをサポートします:


.. method:: Compress.compress(string)

   .. Compress *string*, returning a string containing compressed data for at least
   .. part of the data in *string*.  This data should be concatenated to the output
   .. produced by any preceding calls to the :meth:`compress` method.  Some input may
   .. be kept in internal buffers for later processing.

   *string* を圧縮し、圧縮されたデータを含む文字列を返します。この文字列は少なくとも *string* の一部分のデータに対する圧縮データを含みます。このデータは以前に呼んだ
   :meth:`compress` が返した出力と結合することができます。入力の一部は以後の処理のために内部バッファに保存されることもあります。


.. method:: Compress.flush([mode])

   .. All pending input is processed, and a string containing the remaining compressed
   .. output is returned.  *mode* can be selected from the constants
   .. :const:`Z_SYNC_FLUSH`,  :const:`Z_FULL_FLUSH`,  or  :const:`Z_FINISH`,
   .. defaulting to :const:`Z_FINISH`.  :const:`Z_SYNC_FLUSH` and
   .. :const:`Z_FULL_FLUSH` allow compressing further strings of data, while
   .. :const:`Z_FINISH` finishes the compressed stream and  prevents compressing any
   .. more data.  After calling :meth:`flush` with *mode* set to :const:`Z_FINISH`,
   .. the :meth:`compress` method cannot be called again; the only realistic action is
   .. to delete the object.

   未処理の入力データが処理され、この未処理部分を圧縮したデータを含む文字列が返されます。 *mode* は定数 :const:`Z_SYNC_FLUSH` 、
   :const:`Z_FULL_FLUSH` 、または :const:`Z_FINISH` のいずれかをとり、デフォルト値は :const:`Z_FINISH`
   です。 :const:`Z_SYNC_FLUSH` および :const:`Z_FULL_FLUSH` ではこれ以後にもデータ文字列を圧縮できる
   モードです。一方、 :const:`Z_FINISH` は圧縮ストリームを閉じ、これ以後のデータの圧縮を禁止します。 *mode* に
   :const:`Z_FINISH` を設定して :meth:`flush` メソッドを呼び出した後は、 :meth:`compress`
   メソッドを再び呼ぶべきではありません。唯一の現実的な操作はこのオブジェクトを削除することだけです。


.. method:: Compress.copy()

   .. Returns a copy of the compression object.  This can be used to efficiently
   .. compress a set of data that share a common initial prefix.

   圧縮オブジェクトのコピーを返します。これを使うと先頭部分が共通している複数のデータを効率的に圧縮することができます。

   .. versionadded:: 2.5


.. Decompression objects support the following methods, and two attributes:

解凍オブジェクトは以下のメソッドと 2 つの属性をサポートします:


.. attribute:: Decompress.unused_data

   .. A string which contains any bytes past the end of the compressed data. That is,
   .. this remains ``""`` until the last byte that contains compression data is
   .. available.  If the whole string turned out to contain compressed data, this is
   .. ``""``, the empty string.

   圧縮データの末尾より後のバイト列が入った文字列です。すなわち、この値は圧縮データの入っているバイト列の最後の文字が利用可能になるまでは ``""``
   のままとなります。入力文字列全てが圧縮データを含んでいた場合、この属性は ``""`` 、すなわち空文字列になります。


   .. The only way to determine where a string of compressed data ends is by actually
   .. decompressing it.  This means that when compressed data is contained part of a
   .. larger file, you can only find the end of it by reading data and feeding it
   .. followed by some non-empty string into a decompression object's
   .. :meth:`decompress` method until the :attr:`unused_data` attribute is no longer
   .. the empty string.

   圧縮データ文字列がどこで終了しているかを決定する唯一の方法は、実際にそれを解凍することです。つまり、大きなファイル
   の一部分に圧縮データが含まれているときに、その末端を調べるためには、データをファイルから読み出し、空でない文字列を後ろに続けて、
   :attr:`unused_data` が空文字列でなくなるまで、解凍オブジェクトの  :meth:`decompress`
   メソッドに入力しつづけるしかありません。


.. attribute:: Decompress.unconsumed_tail

   .. A string that contains any data that was not consumed by the last
   .. :meth:`decompress` call because it exceeded the limit for the uncompressed data
   .. buffer.  This data has not yet been seen by the zlib machinery, so you must feed
   .. it (possibly with further data concatenated to it) back to a subsequent
   .. :meth:`decompress` method call in order to get correct output.

   解凍されたデータを収めるバッファの長さ制限を超えたために、最も最近の :meth:`decompress` 呼び出しで処理しきれなかったデータを含む文字列です。
   このデータはまだ zlib 側からは見えていないので、正しい解凍出力を得るには以降の :meth:`decompress` メソッド呼び出しに
   (場合によっては後続のデータが追加された) データを差し戻さなければなりません。


.. method:: Decompress.decompress(string[, max_length])

   .. Decompress *string*, returning a string containing the uncompressed data
   .. corresponding to at least part of the data in *string*.  This data should be
   .. concatenated to the output produced by any preceding calls to the
   .. :meth:`decompress` method.  Some of the input data may be preserved in internal
   .. buffers for later processing.

   *string* を解凍し、少なくとも *string* の一部分に対応する解凍されたデータを含む文字列を返します。このデータは以前に
   :meth:`decompress` メソッドを呼んだ時に返された出力と結合することができます。入力データの一部分が以後の処理のために内部バッファに
   保存されることもあります。


   .. If the optional parameter *max_length* is supplied then the return value will be
   .. no longer than *max_length*. This may mean that not all of the compressed input
   .. can be processed; and unconsumed data will be stored in the attribute
   .. :attr:`unconsumed_tail`. This string must be passed to a subsequent call to
   .. :meth:`decompress` if decompression is to continue.  If *max_length* is not
   .. supplied then the whole input is decompressed, and :attr:`unconsumed_tail` is an
   .. empty string.

   オプションパラメータ *max_length* が与えられると、返される解凍データの長さが *max_length* 以下に制限されます。このことは入力した圧縮
   データの全てが処理されるとは限らないことを意味し、処理されなかったデータは :attr:`unconsumed_tail` 属性に保存されます。
   解凍処理を継続したいならば、この保存されたデータを以降の :meth:`decompress` 呼び出しに渡さなくてはなりません。 *max_length*
   が与えられなかった場合、全ての入力が解凍され、 :attr:`unconsumed_tail` 属性は空文字列になります。


.. method:: Decompress.flush([length])

   .. All pending input is processed, and a string containing the remaining
   .. uncompressed output is returned.  After calling :meth:`flush`, the
   .. :meth:`decompress` method cannot be called again; the only realistic action is
   .. to delete the object.

   未処理の入力データを全て処理し、最終的に圧縮されなかった残りの出力文字列を返します。 :meth:`flush` を呼んだ後、
   :meth:`decompress`  を再度呼ぶべきではありません。このときできる唯一の現実的な操作はオブジェクトの削除だけです。


   .. The optional parameter *length* sets the initial size of the output buffer.

   オプション引数 *length* は出力バッファの初期サイズを決めます。


.. method:: Decompress.copy()

   .. Returns a copy of the decompression object.  This can be used to save the state
   .. of the decompressor midway through the data stream in order to speed up random
   .. seeks into the stream at a future point.

   解凍オブジェクトのコピーを返します。これを使うとデータストリームの途中にある解凍オブジェクトの状態を保存でき、未来のある時点で行なわれるストリームの
   ランダムなシークをスピードアップするのに利用できます。

   .. versionadded:: 2.5


.. seealso::

   Module :mod:`gzip`
      Reading and writing :program:`gzip` \ -format files.

   http://www.zlib.net
      .. The zlib library home page.

      zlib ライブラリホームページ

   http://www.zlib.net/manual.html
      .. The zlib manual explains  the semantics and usage of the library's many
      .. functions.

      zlib ライブラリの多くの関数の意味と使い方を解説したマニュアル

