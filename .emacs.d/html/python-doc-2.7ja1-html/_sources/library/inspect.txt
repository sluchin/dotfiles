
:mod:`inspect` --- 活動中のオブジェクトの情報を取得する
=======================================================

.. module:: inspect
   :synopsis: 活動中のオブジェクトから、情報とソースコードを取得する。
.. moduleauthor:: Ka-Ping Yee <ping@lfw.org>
.. sectionauthor:: Ka-Ping Yee <ping@lfw.org>


.. versionadded:: 2.1


.. The :mod:`inspect` module provides several useful functions to help get
.. information about live objects such as modules, classes, methods, functions,
.. tracebacks, frame objects, and code objects.  For example, it can help you
.. examine the contents of a class, retrieve the source code of a method, extract
.. and format the argument list for a function, or get all the information you need
.. to display a detailed traceback.

:mod:`inspect` は、 活動中のオブジェクト (モジュール、クラス、メソッド、関数、トレースバック、
フレームオブジェクト、コードオブジェクトなど) から情報を取得する関数を定義しており、
クラスの内容を調べたり、メソッドのソースコードを取得したり、関数の引数リストを取り出して整形したり、
詳細なトレースバックを表示するのに必要な情報を取得したりするために利用できます。


.. There are four main kinds of services provided by this module: type checking,
.. getting source code, inspecting classes and functions, and examining the
.. interpreter stack.

このモジュールの機能は4種類に分類することができます。
型チェック、ソースコードの情報取得、クラスや関数からの情報取得、インタープリタのスタック情報の調査です。


.. _inspect-types:

型とメンバー
------------

.. The :func:`getmembers` function retrieves the members of an object such as a
.. class or module. The sixteen functions whose names begin with "is" are mainly
.. provided as convenient choices for the second argument to :func:`getmembers`.
.. They also help you determine when you can expect to find the following special
.. attributes:

:func:`getmembers` は、クラスやモジュールなどのオブジェクトからメンバーを取得します。
名前が"is"で始まる16個の関数は、主に :func:`getmembers` の第2引数として利用するために提供されています。
以下のような特殊属性を参照できるかどうか調べる時にも使えるでしょう。


.. +-----------+-----------------+---------------------------+-------+
.. | Type      | Attribute       | Description               | Notes |
.. +===========+=================+===========================+=======+
.. | module    | __doc__         | documentation string      |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | __file__        | filename (missing for     |       |
.. |           |                 | built-in modules)         |       |
.. +-----------+-----------------+---------------------------+-------+
.. | class     | __doc__         | documentation string      |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | __module__      | name of module in which   |       |
.. |           |                 | this class was defined    |       |
.. +-----------+-----------------+---------------------------+-------+
.. | method    | __doc__         | documentation string      |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | __name__        | name with which this      |       |
.. |           |                 | method was defined        |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | im_class        | class object that asked   | \(1)  |
.. |           |                 | for this method           |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | im_func or      | function object           |       |
.. |           | __func__        | containing implementation |       |
.. |           |                 | of method                 |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | im_self or      | instance to which this    |       |
.. |           | __self__        | method is bound, or       |       |
.. |           |                 | ``None``                  |       |
.. +-----------+-----------------+---------------------------+-------+
.. | function  | __doc__         | documentation string      |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | __name__        | name with which this      |       |
.. |           |                 | function was defined      |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | func_code       | code object containing    |       |
.. |           |                 | compiled function         |       |
.. |           |                 | :term:`bytecode`          |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | func_defaults   | tuple of any default      |       |
.. |           |                 | values for arguments      |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | func_doc        | (same as __doc__)         |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | func_globals    | global namespace in which |       |
.. |           |                 | this function was defined |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | func_name       | (same as __name__)        |       |
.. +-----------+-----------------+---------------------------+-------+
.. | generator | __iter__        | defined to support        |       |
.. |           |                 | iteration over container  |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | close           | raises new GeneratorExit  |       |
.. |           |                 | exception inside the      |       |
.. |           |                 | generator to terminate    |       |
.. |           |                 | the iteration             |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | gi_code         | code object               |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | gi_frame        | frame object or possibly  |       |
.. |           |                 | None once the generator   |       |
.. |           |                 | has been exhausted        |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | gi_running      | set to 1 when generator   |       |
.. |           |                 | is executing, 0 otherwise |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | next            | return the next item from |       |
.. |           |                 | the container             |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | send            | resumes the generator and |       |
.. |           |                 | "sends" a value that      |       |
.. |           |                 | becomes the result of the |       |
.. |           |                 | current yield-expression  |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | throw           | used to raise an          |       |
.. |           |                 | exception inside the      |       |
.. |           |                 | generator                 |       |
.. +-----------+-----------------+---------------------------+-------+
.. | traceback | tb_frame        | frame object at this      |       |
.. |           |                 | level                     |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | tb_lasti        | index of last attempted   |       |
.. |           |                 | instruction in bytecode   |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | tb_lineno       | current line number in    |       |
.. |           |                 | Python source code        |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | tb_next         | next inner traceback      |       |
.. |           |                 | object (called by this    |       |
.. |           |                 | level)                    |       |
.. +-----------+-----------------+---------------------------+-------+
.. | frame     | f_back          | next outer frame object   |       |
.. |           |                 | (this frame's caller)     |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | f_builtins      | builtins namespace seen   |       |
.. |           |                 | by this frame             |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | f_code          | code object being         |       |
.. |           |                 | executed in this frame    |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | f_exc_traceback | traceback if raised in    |       |
.. |           |                 | this frame, or ``None``   |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | f_exc_type      | exception type if raised  |       |
.. |           |                 | in this frame, or         |       |
.. |           |                 | ``None``                  |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | f_exc_value     | exception value if raised |       |
.. |           |                 | in this frame, or         |       |
.. |           |                 | ``None``                  |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | f_globals       | global namespace seen by  |       |
.. |           |                 | this frame                |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | f_lasti         | index of last attempted   |       |
.. |           |                 | instruction in bytecode   |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | f_lineno        | current line number in    |       |
.. |           |                 | Python source code        |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | f_locals        | local namespace seen by   |       |
.. |           |                 | this frame                |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | f_restricted    | 0 or 1 if frame is in     |       |
.. |           |                 | restricted execution mode |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | f_trace         | tracing function for this |       |
.. |           |                 | frame, or ``None``        |       |
.. +-----------+-----------------+---------------------------+-------+
.. | code      | co_argcount     | number of arguments (not  |       |
.. |           |                 | including \* or \*\*      |       |
.. |           |                 | args)                     |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | co_code         | string of raw compiled    |       |
.. |           |                 | bytecode                  |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | co_consts       | tuple of constants used   |       |
.. |           |                 | in the bytecode           |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | co_filename     | name of file in which     |       |
.. |           |                 | this code object was      |       |
.. |           |                 | created                   |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | co_firstlineno  | number of first line in   |       |
.. |           |                 | Python source code        |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | co_flags        | bitmap: 1=optimized ``|`` |       |
.. |           |                 | 2=newlocals ``|`` 4=\*arg |       |
.. |           |                 | ``|`` 8=\*\*arg           |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | co_lnotab       | encoded mapping of line   |       |
.. |           |                 | numbers to bytecode       |       |
.. |           |                 | indices                   |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | co_name         | name with which this code |       |
.. |           |                 | object was defined        |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | co_names        | tuple of names of local   |       |
.. |           |                 | variables                 |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | co_nlocals      | number of local variables |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | co_stacksize    | virtual machine stack     |       |
.. |           |                 | space required            |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | co_varnames     | tuple of names of         |       |
.. |           |                 | arguments and local       |       |
.. |           |                 | variables                 |       |
.. +-----------+-----------------+---------------------------+-------+
.. | builtin   | __doc__         | documentation string      |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | __name__        | original name of this     |       |
.. |           |                 | function or method        |       |
.. +-----------+-----------------+---------------------------+-------+
.. |           | __self__        | instance to which a       |       |
.. |           |                 | method is bound, or       |       |
.. |           |                 | ``None``                  |       |
.. +-----------+-----------------+---------------------------+-------+

+-----------+-----------------+-----------------------------------------------------+-------+
| 型        | 属性            | 説明                                                | 注    |
+===========+=================+=====================================================+=======+
| module    | __doc__         | ドキュメント文字列                                  |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | __file__        | ファイル名 (組み込みモジュールには存在しません)     |       |
+-----------+-----------------+-----------------------------------------------------+-------+
| class     | __doc__         | ドキュメント文字列                                  |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | __module__      | クラスを定義しているモジュールの名前                |       |
+-----------+-----------------+-----------------------------------------------------+-------+
| method    | __doc__         | ドキュメント文字列                                  |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | __name__        | メソッドが定義された時の名前                        |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | im_class        | メソッドを呼び出すために必要なクラスオブジェクト    | \(1)  |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | im_func または  | メソッドを実装している関数オブジェクト              |       |
|           | __func__        |                                                     |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | im_self または  | メソッドに結合しているインスタンス、または ``None`` |       |
|           | __self__        |                                                     |       |
+-----------+-----------------+-----------------------------------------------------+-------+
| function  | __doc__         | ドキュメント文字列                                  |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | __name__        | 関数が定義された時の名前                            |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | func_code       | 関数をコンパイルしたバイトコード (:term:`bytecode`) |       |
|           |                 | を格納するコードオブジェクト                        |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | func_defaults   | 引数のデフォルト値のタプル                          |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | func_doc        | (__doc__と同じ)                                     |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | func_globals    | 関数を定義した時のグローバル名前空間                |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | func_name       | (__name__と同じ)                                    |       |
+-----------+-----------------+-----------------------------------------------------+-------+
| generator | __iter__        | コンテナを通したイテレーションのために定義されます  |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | close           | イテレーションを停止するために、ジェネレータの      |       |
|           |                 | 内部で GeneratorExitexception を発生させます        |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | gi_code         | コードオブジェクト                                  |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | gi_frame        | フレームオブジェクト。ジェネレータが終了したあとは  |       |
|           |                 | None になることもあります                           |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | gi_running      | ジェネレータが実行中の時は 1                        |       |
|           |                 | それ以外の場合は 0                                  |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | next            | コンテナから次の要素を返します                      |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | send            | ジェネレータを再開して、現在の yield 式の結果と     |       |
|           |                 | なる値を送ります                                    |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | throw           | ジェネレータ内部で例外を発生させるために用います    |       |
+-----------+-----------------+-----------------------------------------------------+-------+
| traceback | tb_frame        | このレベルのフレームオブジェクト                    |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | tb_lasti        | 最後に実行しようとしたバイトコード中の              |       |
|           |                 | インストラクションを示すインデックス                |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | tb_lineno       | 現在の Python ソースコードの行番号                  |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | tb_next         | このオブジェクトの内側 (このレベルから呼び出された) |       |
|           |                 | のトレースバックオブジェクト                        |       |
+-----------+-----------------+-----------------------------------------------------+-------+
| frame     | f_back          | 外側 (このフレームを呼び出した) の                  |       |
|           |                 | フレームオブジェクト                                |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | f_builtins      | このフレームで参照している組み込み名前空間          |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | f_code          | このフレームで実行しているコードオブジェクト        |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | f_exc_traceback | このフレームで例外が発生した場合には                |       |
|           |                 | トレースバックオブジェクト、それ以外なら ``None``   |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | f_exc_type      | このフレームで例外が発生した場合には例外型、        |       |
|           |                 | それ以外なら ``None``                               |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | f_exc_value     | このフレームで例外が発生した場合には例外の値、      |       |
|           |                 | それ以外なら ``None``                               |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | f_globals       | このフレームで参照しているグローバル名前空間        |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | f_lasti         | 最後に実行しようとしたバイトコードのインデックス    |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | f_lineno        | 現在の Python ソースコードの行番号                  |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | f_locals        | このフレームで参照しているローカル名前空間          |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | f_restricted    | 制限実行モードなら1、それ以外なら0                  |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | f_trace         | このフレームのトレース関数、または ``None``         |       |
+-----------+-----------------+-----------------------------------------------------+-------+
| code      | co_argcount     | 引数の数 (\* や \*\* 引数は含まない)                |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | co_code         | コンパイルされたバイトコードそのままの文字列        |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | co_consts       | バイトコード中で使用している定数のタプル            |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | co_filename     | コードオブジェクトを生成したファイルのファイル名    |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | co_firstlineno  | Python ソースコードの先頭行                         |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | co_flags        | 以下の値の組み合わせ: 1=optimized                   |       |
|           |                 | ``|`` 2=newlocals  ``|``                            |       |
|           |                 | 4=\*arg ``|`` 8=\*\*arg                             |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | co_lnotab       | 行番号からバイトコードインデックスへの変換表を      |       |
|           |                 | 文字列にエンコードしたもの                          |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | co_name         | コードオブジェクトが定義されたときの名前            |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | co_names        | ローカル変数名のタプル                              |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | co_nlocals      | ローカル変数の数                                    |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | co_stacksize    | 必要とされる仮想マシンのスタックスペース            |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | co_varnames     | 引数名とローカル変数名のタプル                      |       |
+-----------+-----------------+-----------------------------------------------------+-------+
| builtin   | __doc__         | ドキュメント文字列                                  |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | __name__        | 関数、メソッドの元々の名前                          |       |
+-----------+-----------------+-----------------------------------------------------+-------+
|           | __self__        | メソッドが結合しているインスタンス、または ``None`` |       |
+-----------+-----------------+-----------------------------------------------------+-------+


Note:

(1)

   .. .. versionchanged:: 2.2
   ..    :attr:`im_class` used to refer to the class that defined the method.

   .. versionchanged:: 2.2
      :attr:`im_class` は、以前はメソッドを定義しているクラスを指していました。


.. function:: getmembers(object[, predicate])

   .. Return all the members of an object in a list of (name, value) pairs sorted by
   .. name.  If the optional *predicate* argument is supplied, only members for which
   .. the predicate returns a true value are included.

   オブジェクトの全メンバーを、 (名前, 値) の組み合わせのリストで返します。リストはメンバー名でソートされています。
   *predicate* が指定されている場合、 predicate の戻り値が真となる値のみを返します。


   .. note::

      .. :func:`getmembers` does not return metaclass attributes when the argument
      .. is a class (this behavior is inherited from the :func:`dir` function).

      :func:`getmembers` は、引数がクラスの場合にメタクラス属性を返しません。
      (この動作は :func:`dir` 関数に合わせてあります。)


.. function:: getmoduleinfo(path)

   *path* で指定したファイルがモジュールであれば、そのモジュールが Python でどのように解釈されるかを示す
   ``(name, suffix, mode, module_type)`` のタプルを返します。
   モジュールでなければ ``None`` を返します。
   *name* はパッケージ名を含まないモジュール名、 *suffix* はファイル名からモジュール名を除いた残りの部分
   (ドット区切りの拡張子であってはなりません)、
   *mode* は :func:`open` で指定されるファイルモード (``'r'`` または ``'rb'``)、
   *module_type* はモジュールタイプを示す整数で、 :mod:`imp` で定義している
   定数のいずれかが指定されます。
   モジュールタイプについては :mod:`imp` を参照してください。


   .. .. versionchanged:: 2.6
   ..    Returns a :term:`named tuple` ``ModuleInfo(name, suffix, mode,
   ..    module_type)``.

   .. versionchanged:: 2.6
      名前付きタプル (:term:`named tuple`) の ``ModuleInfo(name, suffix, mode, module_type)`` を返します。


.. function:: getmodulename(path)

   .. Return the name of the module named by the file *path*, without including the
   .. names of enclosing packages.  This uses the same algorithm as the interpreter
   .. uses when searching for modules.  If the name cannot be matched according to the
   .. interpreter's rules, ``None`` is returned.

   *path* で指定したファイルの、パッケージ名を含まないモジュール名を返します。
   この処理は、インタープリタがモジュールを検索する時と同じアルゴリズムで行われます。
   ファイルがこのアルゴリズムで見つからない場合には ``None`` が返ります。


.. function:: ismodule(object)

   .. Return true if the object is a module.

   オブジェクトがモジュールの場合は真を返します。


.. function:: isclass(object)

   オブジェクトが、ビルトインもしくは Python で作られたクラスの場合に
   真を返します。


.. function:: ismethod(object)

   .. Return true if the object is a method.

   オブジェクトがメソッドの場合は真を返します。


.. function:: isfunction(object)

   オブジェクトが Python の関数(:term:`lambda` 式で生成されたものを含む)
   である場合に真を返します。


.. function:: isgeneratorfunction(object)

   .. Return true if the object is a Python generator function.

   *object* が Python のジェネレータ関数であるときに真を返します。


   .. versionadded:: 2.6


.. function:: isgenerator(object)

   .. Return true if the object is a generator.

   *object* がジェネレータであるときに真を返します。


   .. versionadded:: 2.6


.. function:: istraceback(object)

   .. Return true if the object is a traceback.

   オブジェクトがトレースバックの場合は真を返します。


.. function:: isframe(object)

   .. Return true if the object is a frame.

   オブジェクトがフレームの場合は真を返します。


.. function:: iscode(object)

   .. Return true if the object is a code.

   オブジェクトがコードの場合は真を返します。


.. function:: isbuiltin(object)

   オブジェクトが組み込み関数か束縛済みのビルトインメソッドの場合に
   真を返します。


.. function:: isroutine(object)

   .. Return true if the object is a user-defined or built-in function or method.

   オブジェクトがユーザ定義か組み込みの関数またはメソッドの場合は真を返します。


.. function:: isabstract(object)

   .. Return true if the object is an abstract base class.

   *object* が抽象規定型 (ABC) であるときに真を返します。

   .. versionadded:: 2.6


.. function:: ismethoddescriptor(object)

   オブジェクトがメソッドデスクリプタであり、
   :func:`ismethod`, :func:`isclass`, :func:`isfunction`, :func:`isbuiltin`
   でない場合に真を返します。

   この機能は Python 2.2 から新たに追加されたもので、例えば ``int.__add__`` は真になります。
   このテストをパスするオブジェクトは :attr:`__get__` 属性を持ちますが :attr:`__set__` 属性を持ちません。
   それ以外の属性を持っているかもしれません。
   通常 :attr:`__name__` を持っていますし、しばしば :attr:`__doc__` も持っています。

   デスクリプタを使って実装されたメソッドで、上記のいずれかのテストもパスしているものは、
   :func:`ismethoddescriptor` では偽を返します。これは単に他のテストの方がもっと確実だからです --
   例えば、 :func:`ismethod` をパスしたオブジェクトは :attr:`im_func` 属性などを持っていると期待できます。


.. function:: isdatadescriptor(object)

   .. Return true if the object is a data descriptor.

   オブジェクトがデータデスクリプタの場合に真を返します。


   .. Data descriptors have both a :attr:`__get__` and a :attr:`__set__` attribute.
   .. Examples are properties (defined in Python), getsets, and members.  The
   .. latter two are defined in C and there are more specific tests available for
   .. those types, which is robust across Python implementations.  Typically, data
   .. descriptors will also have :attr:`__name__` and :attr:`__doc__` attributes
   .. (properties, getsets, and members have both of these attributes), but this is
   .. not guaranteed.

   データデスクリプタは :attr:`__get__` および :attr:`__set__` 属性の両方を持ちます。
   データデスクリプタの例は (Python 上で定義された) プロパティや getset やメンバーです。
   後者のふたつは C で定義されており、個々の型に特有のテストも行います。
   そのため、Python の実装よりもより確実です。
   通常、データデスクリプタは :attr:`__name__` や :attr:`__doc__`  属性を持ちます
   (プロパティ、 getset 、メンバーは両方の属性を持っています) が、保証されているわけではありません。


   .. versionadded:: 2.3


.. function:: isgetsetdescriptor(object)

   .. Return true if the object is a getset descriptor.

   オブジェクトが getset デスクリプタの場合に真を返します。


   .. impl-detail::

      .. getsets are attributes defined in extension modules via
      .. :ctype:`PyGetSetDef` structures.  For Python implementations without such
      .. types, this method will always return ``False``.

      getset とは、拡張モジュールで :c:type:`PyGetSetDef` 構造体を用いて定義された属性のことです。
      そのような型を持たない Python 実装の場合は、このメソッドは常に ``False`` を返します。


   .. versionadded:: 2.5


.. function:: ismemberdescriptor(object)

   .. Return true if the object is a member descriptor.

   オブジェクトがメンバーデスクリプタの場合に真を返します。


   .. impl-detail::

      .. Member descriptors are attributes defined in extension modules via
      .. :ctype:`PyMemberDef` structures.  For Python implementations without such
      .. types, this method will always return ``False``.

      メンバーデスクリプタとは、拡張モジュールで :c:type:`PyMemberDef` 構造体を用いて定義された属性のことです。
      そのような型を持たない Python 実装の場合は、このメソッドは常に ``False`` を返します。


   .. versionadded:: 2.5


.. _inspect-source:

ソースコードの情報取得
----------------------

.. function:: getdoc(object)

   .. Get the documentation string for an object, cleaned up with :func:`cleandoc`.

   :func:`cleandoc` でクリーンアップされた、オブジェクトのドキュメンテーション文字列を取得します。


.. function:: getcomments(object)

   .. Return in a single string any lines of comments immediately preceding the
   .. object's source code (for a class, function, or method), or at the top of the
   .. Python source file (if the object is a module).

   オブジェクトがクラス、関数、メソッドのいずれかの場合は、
   オブジェクトのソースコードの直後にあるコメント行 (複数行) を、単一の文字列として返します。
   オブジェクトがモジュールの場合、ソースファイルの先頭にあるコメントを返します。


.. function:: getfile(object)

   .. Return the name of the (text or binary) file in which an object was defined.
   .. This will fail with a :exc:`TypeError` if the object is a built-in module,
   .. class, or function.

   オブジェクトを定義している (テキストまたはバイナリの) ファイルの名前を返します。
   オブジェクトが組み込みモジュール、クラス、関数の場合は :exc:`TypeError` 例外が発生します。


.. function:: getmodule(object)

   .. Try to guess which module an object was defined in.

   オブジェクトを定義しているモジュールを推測します。


.. function:: getsourcefile(object)

   .. Return the name of the Python source file in which an object was defined.  This
   .. will fail with a :exc:`TypeError` if the object is a built-in module, class, or
   .. function.

   オブジェクトを定義している Python ソースファイルの名前を返します。
   オブジェクトが組み込みのモジュール、クラス、関数の場合には、 :exc:`TypeError` 例外が発生します。


.. function:: getsourcelines(object)

   .. Return a list of source lines and starting line number for an object. The
   .. argument may be a module, class, method, function, traceback, frame, or code
   .. object.  The source code is returned as a list of the lines corresponding to the
   .. object and the line number indicates where in the original source file the first
   .. line of code was found.  An :exc:`IOError` is raised if the source code cannot
   .. be retrieved.

   オブジェクトのソース行のリストと開始行番号を返します。
   引数にはモジュール、クラス、メソッド、関数、トレースバック、フレーム、コードオブジェクトを指定することができます。
   戻り値は指定したオブジェクトに対応するソースコードのソース行リストと元のソースファイル上での開始行となります。
   ソースコードを取得できない場合は :exc:`IOError` が発生します。


.. function:: getsource(object)

   .. Return the text of the source code for an object. The argument may be a module,
   .. class, method, function, traceback, frame, or code object.  The source code is
   .. returned as a single string.  An :exc:`IOError` is raised if the source code
   .. cannot be retrieved.

   オブジェクトのソースコードを返します。
   引数にはモジュール、クラス、メソッド、関数、トレースバック、フレーム、コードオブジェクトを指定することができます。
   ソースコードは単一の文字列で返します。ソースコードを取得できない場合は :exc:`IOError` が発生します。


.. function:: cleandoc(doc)

   .. Clean up indentation from docstrings that are indented to line up with blocks
   .. of code.  Any whitespace that can be uniformly removed from the second line
   .. onwards is removed.  Also, all tabs are expanded to spaces.

   インデントされた docstring から、コードブロックまでのインデントを削除します。
   2行目以降では行頭の空白は一様に削除されます。全てのタブはスペースに展開されます。


   .. versionadded:: 2.6


.. _inspect-classes-functions:

クラスと関数
------------

.. function:: getclasstree(classes[, unique])

   .. Arrange the given list of classes into a hierarchy of nested lists. Where a
   .. nested list appears, it contains classes derived from the class whose entry
   .. immediately precedes the list.  Each entry is a 2-tuple containing a class and a
   .. tuple of its base classes.  If the *unique* argument is true, exactly one entry
   .. appears in the returned structure for each class in the given list.  Otherwise,
   .. classes using multiple inheritance and their descendants will appear multiple
   .. times.

   リストで指定したクラスの継承関係から、ネストしたリストを作成します。
   ネストしたリストには、直前の要素から派生したクラスが格納されます。
   各要素は長さ2のタプルで、クラスと基底クラスのタプルを格納しています。
   *unique* が真の場合、各クラスは戻り値のリスト内に一つだけしか格納されません。
   真でなければ、多重継承を利用したクラスとその派生クラスは複数回格納される場合があります。


.. function:: getargspec(func)

   Python 関数の引数名とデフォルト値を取得します。戻り値は長さ4のタプルで、
   次の値を返します: ``(args, varargs, keywords, defaults)`` 。
   *args* は引数名のリストです (ネストしたリストが格納される場合があります)。
   *varargs* と *keywords* は ``*`` 引数と ``**`` 引数の名前で、引数がなければ ``None`` となります。
   *defaults* は引数のデフォルト値のタプルか、デフォルト値がない場合は ``None`` です。
   このタプルに *n* 個の要素があれば、各要素は *args* の後ろから *n* 個分の引数のデフォルト値となります。

   .. versionchanged:: 2.6
      ``ArgSpec(args, varargs, keywords, defaults)`` 形式の
      名前付きタプル (:term:`named tuple`) を返します。


.. function:: getargvalues(frame)

   指定したフレームに渡された引数の情報を取得します。
   戻り値は長さ4のタプルで、次の値を返します: ``(args, varargs, keywords, locals)``.
   *args* は引数名のリストです (ネストしたリストが格納される場合があります)。
   *varargs* と *keywords* は ``*`` 引数と ``**`` 引数の名前で、引数がなければ ``None`` となります。
   *locals* は指定したフレームのローカル変数の辞書です。

   .. versionchanged:: 2.6
      ``ArgInfo(args, varargs, keywords, locals)`` 形式の
      名前付きタプル (:term:`named tuple`) を返します。


.. function:: formatargspec(args[, varargs, varkw, defaults, formatarg, formatvarargs, formatvarkw, formatvalue, join])

   .. Format a pretty argument spec from the four values returned by
   .. :func:`getargspec`.  The format\* arguments are the corresponding optional
   .. formatting functions that are called to turn names and values into strings.

   :func:`getargspec` で取得した4つの値を読みやすく整形します。
   format\* 引数はオプションで、名前と値を文字列に変換する整形関数を指定することができます。


.. function:: formatargvalues(args[, varargs, varkw, locals, formatarg, formatvarargs, formatvarkw, formatvalue, join])

   .. Format a pretty argument spec from the four values returned by
   .. :func:`getargvalues`.  The format\* arguments are the corresponding optional
   .. formatting functions that are called to turn names and values into strings.

   :func:`getargvalues` で取得した4つの値を読みやすく整形します。
   format\* 引数はオプションで、名前と値を文字列に変換する整形関数を指定することができます。


.. function:: getmro(cls)

   *cls* クラスの基底クラス (*cls* 自身も含む) を、メソッドの優先順位順に並べたタプルを返します。
   結果のリスト内で各クラスは一度だけ格納されます。メソッドの優先順位はクラスの型によって異なります。
   非常に特殊なユーザ定義のメタクラスを使用していない限り、 *cls* が戻り値の先頭要素となります。

.. function:: getcallargs(func[, *args][, **kwds])

   *args* と *kwds* を、 Python の関数もしくはメソッド *func* を呼び出した
   場合と同じように引数名に束縛します。束縛済みメソッド(bound method)の場合、
   最初の引数(慣習的に ``self`` という名前が付けられます)にも、関連づけられた
   インスタンスを束縛します。引数名 (``*`` や ``**`` 引数が存在すればその名前も)
   に *args* と *kwds* からの値をマップした辞書を返します。
   *func* を正しく呼び出せない場合、つまり ``func(*args, **kwds)`` が
   シグネチャの不一致のために例外を投げるような場合には、それと同じ型で
   同じか似ているメッセージの例外を発生させます。例::

    >>> from inspect import getcallargs
    >>> def f(a, b=1, *pos, **named):
    ...     pass
    >>> getcallargs(f, 1, 2, 3)
    {'a': 1, 'named': {}, 'b': 2, 'pos': (3,)}
    >>> getcallargs(f, a=2, x=4)
    {'a': 2, 'named': {'x': 4}, 'b': 1, 'pos': ()}
    >>> getcallargs(f)
    Traceback (most recent call last):
    ...
    TypeError: f() takes at least 1 argument (0 given)

   .. versionadded:: 2.7


.. _inspect-stack:

インタープリタスタック
-----------------------

.. When the following functions return "frame records," each record is a tuple of
.. six items: the frame object, the filename, the line number of the current line,
.. the function name, a list of lines of context from the source code, and the
.. index of the current line within that list.

以下の関数には、戻り値として"フレームレコード"を返す関数があります。
"フレームレコード"は長さ6のタプルで、以下の値を格納しています:
フレームオブジェクト、ファイル名、実行中の行番号、関数名、コンテキストのソース行のリスト、
ソース行のリストにおける実行中の行のインデックス。


.. note::

   .. Keeping references to frame objects, as found in the first element of the frame
   .. records these functions return, can cause your program to create reference
   .. cycles.  Once a reference cycle has been created, the lifespan of all objects
   .. which can be accessed from the objects which form the cycle can become much
   .. longer even if Python's optional cycle detector is enabled.  If such cycles must
   .. be created, it is important to ensure they are explicitly broken to avoid the
   .. delayed destruction of objects and increased memory consumption which occurs.

   フレームレコードの最初の要素などのフレームオブジェクトへの参照を保存すると、循環参照になってしまう場合があります。
   循環参照ができると、 Python の循環参照検出機能を有効にしていたとしても関連するオブジェクトが参照しているすべてのオブジェクトが解放されにくくなり、
   明示的に参照を削除しないとメモリ消費量が増大する恐れがあります。


   .. Though the cycle detector will catch these, destruction of the frames (and local
   .. variables) can be made deterministic by removing the cycle in a
   .. :keyword:`finally` clause.  This is also important if the cycle detector was
   .. disabled when Python was compiled or using :func:`gc.disable`.  For example:

   参照の削除を Python の循環参照検出機能にまかせることもできますが、 :keyword:`finally` 節で循環参照を解除すれば確実にフレーム (とそのローカル変数) は削除されます。
   また、循環参照検出機能は Python のコンパイルオプションや :func:`gc.disable` で無効とされている場合がありますので注意が必要です。例：


   ::

      def handle_stackframe_without_leak():
          frame = inspect.currentframe()
          try:
              # do something with the frame
          finally:
              del frame


.. The optional *context* argument supported by most of these functions specifies
.. the number of lines of context to return, which are centered around the current
.. line.

以下の関数でオプション引数 *context* には、戻り値のソース行リストに何行分のソースを含めるかを指定します。
ソース行リストには、実行中の行を中心として指定された行数分のリストを返します。


.. function:: getframeinfo(frame[, context])

   .. Get information about a frame or traceback object.  A 5-tuple is returned, the
   .. last five elements of the frame's frame record.

   フレームまたはトレースバックオブジェクトの情報を取得します。
   フレームレコードの先頭要素を除いた、長さ5のタプルを返します。


   .. .. versionchanged:: 2.6
   ..    Returns a :term:`named tuple` ``Traceback(filename, lineno, function,
   ..    code_context, index)``.

   .. versionchanged:: 2.6
      ``Traceback(filename, lineno, function, code_context, index)``
      形式の名前付きタプル (:term:`named tuple`) を返します。


.. function:: getouterframes(frame[, context])

   .. Get a list of frame records for a frame and all outer frames.  These frames
   .. represent the calls that lead to the creation of *frame*. The first entry in the
   .. returned list represents *frame*; the last entry represents the outermost call
   .. on *frame*'s stack.

   指定したフレームと、その外側の全フレームのフレームレコードを返します。
   外側のフレームとは *frame* が生成されるまでのすべての関数呼び出しを示します。
   戻り値のリストの先頭は *frame* のフレームレコードで、
   末尾の要素は *frame* のスタックにある最も外側のフレームのフレームレコードとなります。


.. function:: getinnerframes(traceback[, context])

   .. Get a list of frame records for a traceback's frame and all inner frames.  These
   .. frames represent calls made as a consequence of *frame*.  The first entry in the
   .. list represents *traceback*; the last entry represents where the exception was
   .. raised.

   指定したフレームと、その内側の全フレームのフレームレコードを返します。
   内のフレームとは *frame* から続く一連の関数呼び出しを示します。
   戻り値のリストの先頭は *traceback* のフレームレコードで、末尾の要素は例外が発生した位置を示します。


.. function:: currentframe()

   .. Return the frame object for the caller's stack frame.

   呼び出し元のフレームオブジェクトを返します。


   .. impl-detail::

      .. This function relies on Python stack frame support in the interpreter,
      .. which isn't guaranteed to exist in all implementations of Python.  If
      .. running in an implementation without Python stack frame support this
      .. function returns ``None``.

      この関数はインタプリタの Python スタックフレームサポートに依存します。
      これは Python のすべての実装に存在している保証はありません。
      Python スタックフレームサポートのない環境では、この関数は ``None`` を返します。

 
.. function:: stack([context])

   .. Return a list of frame records for the caller's stack.  The first entry in the
   .. returned list represents the caller; the last entry represents the outermost
   .. call on the stack.

   呼び出し元スタックのフレームレコードのリストを返します。
   最初の要素は呼び出し元のフレームレコードで、末尾の要素はスタックにある最も外側のフレームのフレームレコードとなります。


.. function:: trace([context])

   .. Return a list of frame records for the stack between the current frame and the
   .. frame in which an exception currently being handled was raised in.  The first
   .. entry in the list represents the caller; the last entry represents where the
   .. exception was raised.

   実行中のフレームと処理中の例外が発生したフレームの間のフレームレコードのリストを返します。
   最初の要素は呼び出し元のフレームレコードで、末尾の要素は例外が発生した位置を示します。

