:mod:`types` --- 組み込み型の名前
=================================

.. module:: types
   :synopsis: 組み込み型の名前


このモジュールは標準のPythonインタプリタで使われているオブジェクトの型について、
名前を定義しています(拡張モジュールで定義されている型を除く)。
このモジュールは ``listiterator`` 型のようなプロセス中に例外をふくまないので、
``from types import *`` のように使っても安全です。
このモジュールの将来のバージョンで追加される名前は、 ``Type`` で終わる予定です。

関数での典型的な利用方法は、以下のように引数の型によって異なる動作をする場合です::

   from types import *
   def delete(mylist, item):
       if type(item) is IntType:
          del mylist[item]
       else:
          mylist.remove(item)

Python 2.2以降では、 :func:`int` や :func:`str` のような
ファクトリ関数は、型の名前となりましたので、 :mod:`types` を使用する必要はなくなりました。上記のサンプルは、以下のように記述する事が
推奨されています。 ::

   def delete(mylist, item):
       if isinstance(item, int):
          del mylist[item]
       else:
          mylist.remove(item)

このモジュールは以下の名前を定義しています。


.. data:: NoneType

   ``None`` の型です。


.. data:: TypeType

   .. index:: builtin: type

   typeオブジェクトの型です (:func:`type` などによって返されます)。
   組み込みの :class:`type` のエイリアスになります。


.. data:: BooleanType

   :class:`bool` の ``True`` と ``False`` の型です。これは組み込みの :class:`bool` のエイリアスです。

   .. versionadded:: 2.3

.. data:: IntType

   整数の型です(e.g. ``1``)。
   組み込みの :class:`int` のエイリアスになります。


.. data:: LongType

   長整数の型です(e.g. ``1L``)。
   組み込みの :class:`long` のエイリアスになります。


.. data:: FloatType

   浮動小数点数の型です(e.g. ``1.0``)。
   組み込みの :class:`float` のエイリアスになります。


.. data:: ComplexType

   複素数の型です(e.g. ``1.0j``)。 Pythonが複素数のサポートなしでコンパイルされていた場合には定義されません。


.. data:: StringType

   文字列の型です(e.g. ``'Spam'``)。
   組み込みの :class:`str` のエイリアスになります。


.. data:: UnicodeType

   Unicode文字列の型です(e.g. ``u'Spam'``)。 Pythonがユニコードのサポートなしでコンパイルされていた場合には定義されません。
   組み込みの :class:`Unicode` のエイリアスになります。


.. data:: TupleType

   タプルの型です(e.g. ``(1, 2, 3, 'Spam')``)。
   組み込みの :class:`tuple` のエイリアスになります。


.. data:: ListType

   リストの型です(e.g. ``[0, 1, 2, 3]``)。
   組み込みの :class:`list` のエイリアスになります。


.. data:: DictType

   辞書の型です(e.g. ``{'Bacon': 1, 'Ham': 0}``)。
   組み込みの :class:`dict` のエイリアスになります。


.. data:: DictionaryType

   ``DictType`` の別名です。


.. data:: FunctionType
          LambdaType

   ユーザー定義の関数または :keyword:`lambda` 式によって作成された関数の型です。


.. data:: GeneratorType

   ジェネレータ(:term:`generator`)関数の呼び出しによって生成されたイテレータオブジェクトの型です。

   .. versionadded:: 2.2


.. data:: CodeType

   .. index:: builtin: compile

   :func:`compile` 関数などによって返されるコードオブジェクトの型です。


.. data:: ClassType

   ユーザー定義の、古いスタイルのクラスの型です。


.. data:: InstanceType

   ユーザー定義のクラスのインスタンスの型です。


.. data:: MethodType

   ユーザー定義のクラスのインスタンスのメソッドの型です。


.. data:: UnboundMethodType

   ``MethodType`` の別名です。


.. data:: BuiltinFunctionType
          BuiltinMethodType

   :func:`len` や :func:`sys.exit` のような組み込み関数や、組み込み型のメソッドの型です。
   (ここでは、"組み込み"という単語を、"Cで書かれた"という意味で使っています)

.. data:: ModuleType

   モジュールの型です。


.. data:: FileType

   ``sys.stdout`` のようなopenされたファイルオブジェクトの型です。
   組み込みの :class:`file` のエイリアスになります。


.. data:: XRangeType

   .. index:: builtin: xrange

   :func:`xrange` 関数によって返されるrangeオブジェクトの型です。
   組み込みの :class:`xrange` のエイリアスになります。


.. data:: SliceType

   .. index:: builtin: slice

   :func:`slice` 関数によって返されるオブジェクトの型です。
   組み込みの :class:`slice` のエイリアスになります。


.. data:: EllipsisType

   ``Ellipsis`` の型です。


.. data:: TracebackType

   ``sys.exc_traceback`` に含まれるようなトレースバックオブジェクトの型です。


.. data:: FrameType

   フレームオブジェクトの型です。トレースバックオブジェクト ``tb`` の ``tb.tb_frame`` などです。


.. data:: BufferType

   .. index:: builtin: buffer

   :func:`buffer` 関数によって作られるバッファオブジェクトの型です。


.. data:: DictProxyType

   ``TypeType.__dict__`` のような dictへのプロキシ型です。


.. data:: NotImplementedType

   ``NotImplemented`` の型です。


.. data:: GetSetDescriptorType

   ``FrameType.f_locals`` や ``array.array.typecode`` のような、拡張モジュールにおいて ``PyGetSetDef``
   によって定義されたオブジェクトの型です。
   この型はオブジェクト属性のディスクリプタとして利用されます。
   :class:`property` 型と同じ目的を持った型ですが、こちらは拡張モジュールで定義された型の\
   ためのものです。

   .. versionadded:: 2.5


.. data:: MemberDescriptorType

   ``datetime.timedelta.days`` のような、拡張モジュールにおいて ``PyMemberDef`` によって定義されたオブジェクトの型です。
   この型は、標準の変換関数を利用するような、Cのシンプルなデータメンバで利用されます。
   :class:`property` 型と同じ目的を持った型ですが、こちらは拡張モジュールで定義された型の\
   ためのものです。

   .. impl-detail::

      Pythonの他の実装では、この型は ``GetSetDescriptorType`` と同一かもしれません。

   .. versionadded:: 2.5


.. data:: StringTypes

   文字列型のチェックを簡単にするための ``StringType`` と ``UnicodeType`` を含むシーケンスです。
   ``UnicodeType`` は実行中の版のPythonに含まれている場合にだけ含まれるの
   で、2つの文字列型のシーケンスを使うよりこれを使う方が移植性が高くなります。例: ``isinstance(s, types.StringTypes)``.

   .. versionadded:: 2.2

