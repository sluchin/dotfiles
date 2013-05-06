
:mod:`marshal` --- 内部使用向けの Python オブジェクト整列化
===========================================================

.. module:: marshal
   :synopsis: Python オブジェクトをバイト列に変換したり、その逆を (異なる制約条件下で) 行います。

.. This module contains functions that can read and write Python values in a binary
.. format.  The format is specific to Python, but independent of machine
.. architecture issues (e.g., you can write a Python value to a file on a PC,
.. transport the file to a Sun, and read it back there).  Details of the format are
.. undocumented on purpose; it may change between Python versions (although it
.. rarely does). [#]_

このモジュールには Python 値をバイナリ形式で読み書きできるような関数が含まれています。
このバイナリ形式は Python 特有のものですが、マシンアーキテクチャ非依存のものです
(つまり、Python の値を PC 上でファイルに書き込み、Sun に転送し、そこで読み戻すことができます)。
バイナリ形式の詳細は意図的にドキュメント化されていません;
この形式は (稀にしかないことですが) Python のバージョン間で変更される可能性があるからです。 [#]_


.. index::
   module: pickle
   module: shelve
   object: code


.. This is not a general "persistence" module.  For general persistence and
.. transfer of Python objects through RPC calls, see the modules :mod:`pickle` and
.. :mod:`shelve`.  The :mod:`marshal` module exists mainly to support reading and
.. writing the "pseudo-compiled" code for Python modules of :file:`.pyc` files.
.. Therefore, the Python maintainers reserve the right to modify the marshal format
.. in backward incompatible ways should the need arise.  If you're serializing and
.. de-serializing Python objects, use the :mod:`pickle` module instead -- the
.. performance is comparable, version independence is guaranteed, and pickle
.. supports a substantially wider range of objects than marshal.

このモジュールは汎用の "永続化 (persistence)" モジュールではありません。
汎用的な永続化や、RPC 呼び出しを通じた Python オブジェクトの転送については、
モジュール :mod:`pickle` および :mod:`shelve` を参照してください。
:mod:`marshal` モジュールは主に、 "擬似コンパイルされた (pseudo-compiled)" コードの
:file:`.pyc` ファイルへの読み書きをサポートするために存在します。
したがって、 Python のメンテナは、必要が生じれば marshal 形式を後方互換性のないものに変更する権利を有しています。
Python オブジェクトを直列化 (シリアライズ) および非直列化 (デシリアライズ) したい場合には、 :mod:`pickle` モジュールを使ってください。
:mod:`pickle` は速度は同等で、バージョン間の互換性が保証されていて、 marshal より広範囲のオブジェクトをサポートしています。


.. warning::

   .. The :mod:`marshal` module is not intended to be secure against erroneous or
   .. maliciously constructed data.  Never unmarshal data received from an
   .. untrusted or unauthenticated source.

   :mod:`marshal` モジュールは、誤ったデータや悪意を持って作成されたデータに対する安全性を考慮していません。
   信頼できない、もしくは認証されていない出所からのデータを非整列化してはなりません。


.. Not all Python object types are supported; in general, only objects whose value
.. is independent from a particular invocation of Python can be written and read by
.. this module.  The following types are supported: ``None``, integers, long
.. integers, floating point numbers, strings, Unicode objects, tuples, lists, sets,
.. dictionaries, and code objects, where it should be understood that tuples, lists
.. and dictionaries are only supported as long as the values contained therein are
.. themselves supported; and recursive lists and dictionaries should not be written
.. (they will cause infinite loops).

すべての Python オブジェクト型がサポートされているわけではありません;
一般的には、 Python の特定の実行に依存しないオブジェクトだけがこのモジュールで読み書きできます。
以下の型: 真偽値、整数、長整数、浮動小数点数、複素数、文字列、 Unicode オブジェクト、
タプル、リスト、 set 、 frozenset 、辞書、コードオブジェクト、がサポートされています。
ただし、タプル、リスト、 set 、 frozenset 、辞書は、それらに含まれた値がサポートされている型である限りサポートされると解釈しなければなりません;
また、再帰的なリストおよび辞書は書き込んではなりません (無限ループを引き起こします)。
シングルトンである :const:`None`\ 、 :const:`Ellipsis`\ 、 :exc:`StopIteration` も整列化可能です。


.. warning::

   .. On machines where C's ``long int`` type has more than 32 bits (such as the
   .. DEC Alpha), it is possible to create plain Python integers that are longer
   .. than 32 bits. If such an integer is marshaled and read back in on a machine
   .. where C's ``long int`` type has only 32 bits, a Python long integer object
   .. is returned instead.  While of a different type, the numeric value is the
   .. same.  (This behavior is new in Python 2.2.  In earlier versions, all but the
   .. least-significant 32 bits of the value were lost, and a warning message was
   .. printed.)

   (DEC Alpha のように) C言語の ``long int`` が 32 ビットよりも長いビット長を持つ場合、
   32 ビットよりも長い Python 整数を作成することが可能です。
   そのような整数が整列化された後、 C 言語の ``long int`` のビット長が 32 ビットしかないマシン上で読み戻された場合、
   通常整数の代わりに Python 長整数が返されます。型は異なりますが、数値は同じです。
   (この動作は Python 2.2 で新たに追加されたものです。それ以前のバージョンでは、
   値のうち最小桁から 32 ビット以外の情報は失われ、警告メッセージが出力されていました。)


.. There are functions that read/write files as well as functions operating on
.. strings.

文字列を操作する関数と同様に、ファイルの読み書きを行う関数が提供されています。


.. The module defines these functions:

このモジュールでは以下の関数を定義しています:


.. function:: dump(value, file[, version])

   .. Write the value on the open file.  The value must be a supported type.  The
   .. file must be an open file object such as ``sys.stdout`` or returned by
   .. :func:`open` or :func:`os.popen`.  It must be opened in binary mode (``'wb'``
   .. or ``'w+b'``).

   開かれたファイルに値を書き込みます。値はサポートされている型でなくてはなりません。
   ファイルは ``sys.stdout`` か、 :func:`open` や :func:`posix.popen` が返すようなファイルオブジェクトでなくてはなりません。
   またファイルはバイナリモード (``'wb'`` または ``'w+b'``) で開かれていなければなりません。


   .. If the value has (or contains an object that has) an unsupported type, a
   .. :exc:`ValueError` exception is raised --- but garbage data will also be written
   .. to the file.  The object will not be properly read back by :func:`load`.

   値 (または値に含まれるオブジェクト) がサポートされていない型の場合、
   :exc:`ValueError` 例外が送出されます --- しかし、同時にごみのデータがファイルに書き込まれます。
   このオブジェクトは :func:`load` で適切に読み出されることはありません。

   .. .. versionadded:: 2.4
   ..    The *version* argument indicates the data format that ``dump`` should use
   ..    (see below).

   .. versionadded:: 2.4
      *version* 引数は ``dump`` が利用するデータフォーマットを表します (下記参照)。


.. function:: load(file)

   .. Read one value from the open file and return it.  If no valid value is read
   .. (e.g. because the data has a different Python version's incompatible marshal
   .. format), raise :exc:`EOFError`, :exc:`ValueError` or :exc:`TypeError`.  The
   .. file must be an open file object opened in binary mode (``'rb'`` or
   .. ``'r+b'``).

   開かれたファイルから値を一つ読んで返します。
   (異なるバージョンの Python の互換性のない marshal フォーマットだったなどの理由で)
   有効な値が読み出せなかった場合、 :exc:`EOFError`\ 、 :exc:`ValueError`\ 、または :exc:`TypeError` を送出します。
   file はバイナリモード (``'rb'`` または ``'r+b'``) で開かれたファイルオブジェクトでなければなりません.

   .. note::

      .. If an object containing an unsupported type was marshalled with :func:`dump`,
      .. :func:`load` will substitute ``None`` for the unmarshallable type.

      サポートされていない型を含むオブジェクトが :func:`dump` で整列化されている場合、 :func:`load` は整列化不能な値を ``None`` で置き換えます。


.. function:: dumps(value[, version])

   .. Return the string that would be written to a file by ``dump(value, file)``.  The
   .. value must be a supported type.  Raise a :exc:`ValueError` exception if value
   .. has (or contains an object that has) an unsupported type.

   ``dump(value, file)`` でファイルに書き込まれるような文字列を返します。値はサポートされている型でなければなりません。
   値がサポートされていない型のオブジェクト (またはサポートされていない型のオブジェクトを含むようなオブジェクト) の場合、
   :exc:`ValueError` 例外が送出されます。

   .. .. versionadded:: 2.4
   ..    The *version* argument indicates the data format that ``dumps`` should use
   ..    (see below).

   .. versionadded:: 2.4
      *version* 引数は ``dumps`` が利用するデータフォーマットを表します (下記参照)。


.. function:: loads(string)

   .. Convert the string to a value.  If no valid value is found, raise
   .. :exc:`EOFError`, :exc:`ValueError` or :exc:`TypeError`.  Extra characters in the
   .. string are ignored.

   データ文字列を値に変換します。
   有効な値が見つからなかった場合、 :exc:`EOFError`\ 、 :exc:`ValueError`\ 、または :exc:`TypeError` が送出されます。
   文字列中の余分な文字は無視されます。


.. In addition, the following constants are defined:

これに加えて、以下の定数が定義されています:


.. data:: version

   .. Indicates the format that the module uses. Version 0 is the historical format,
   .. version 1 (added in Python 2.4) shares interned strings and version 2 (added in
   .. Python 2.5) uses a binary format for floating point numbers. The current version
   .. is 2.

   このモジュールが利用するバージョンを表します。バージョン0 は歴史的なフォーマットです。
   バージョン1 は文字列の再利用をします (Python 2.4で追加されました)。
   バージョン2 は浮動小数点数にバイナリフォーマットを使用します (Python 2.5で追加されました)。
   現在のバージョンは2です。


   .. versionadded:: 2.4


.. rubric:: 注記


.. .. [#] The name of this module stems from a bit of terminology used by the designers of
..    Modula-3 (amongst others), who use the term "marshalling" for shipping of data
..    around in a self-contained form. Strictly speaking, "to marshal" means to
..    convert some data from internal to external form (in an RPC buffer for instance)
..    and "unmarshalling" for the reverse process.

.. [#] このモジュールの名前は (特に) Modula-3 の設計者の間で使われていた用語の一つに由来しています。
   彼らはデータを自己充足的な形式で輸送する操作に "整列化 (marshalling)" という用語を使いました。
   厳密に言えば、"整列させる (to marshal)" とは、あるデータを (例えば RPC バッファのように)
   内部表現形式から外部表現形式に変換することを意味し、"非整列化 (unmarshalling)" とはその逆を意味します。

