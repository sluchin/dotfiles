:mod:`ctypes` --- Pythonのための外部関数ライブラリ
==================================================

.. module:: ctypes
   :synopsis: A foreign function library for Python.
.. moduleauthor:: Thomas Heller <theller@python.net>


.. versionadded:: 2.5

:mod:`ctypes` は Python のための外部関数ライブラリです。このライブラリは
C と互換性のあるデータ型を提供し、動的リンク/共有ライブラリ内の関数呼び
出しを可能にします。動的リンク/共有ライブラリを純粋な Python でラップ
するために使うことができます。


.. _ctypes-ctypes-tutorial:

ctypesチュートリアル
--------------------

注意: このチュートリアルのコードサンプルは動作確認のために :mod:`doctest`
を使います。コードサンプルの中には Linux、 Windows、あるいは Mac OS X
上で異なる動作をするものがあるため、サンプルのコメントに doctest 命令
を入れてあります。

注意: いくつかのコードサンプルで ctypes の :class:`c_int` 型を参照して
います。 32 ビットシステムにおいてこの型は :class:`c_long` 型のエイリ
アスです。そのため、 :class:`c_int` 型を想定しているときに
:class:`c_long` が表示されたとしても、混乱しないようにしてください ---
実際には同じ型なのです。


.. _ctypes-loading-dynamic-link-libraries:

動的リンクライブラリをロードする
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

動的リンクライブラリをロードするために、 :mod:`ctypes` は *cdll* を
エクスポートします。
Windows では *windll* と *oledll* オブジェクトをエクスポートします。

これらのオブジェクトの属性としてライブラリにアクセスすることでライブラ
リをロードします。 *cdll* は標準 ``cdecl`` 呼び出し規約を用いて関数を
エクスポートしているライブラリをロードします。それに対して、 *windll*
ライブラリは ``stdcall`` 呼び出し規約を用いる関数を呼び出します。
*oledll* も ``stdcall`` 呼び出し規約を使いますが、関数が Windows
:c:type:`HRESULT` エラーコードを返すことを想定しています。
このエラーコードは関数呼び出しが失敗したとき、
:class:`WindowsError` 例外を自動的に送出させるために使われます。

Windows用の例ですが、 ``msvcrt`` はほとんどの標準 C 関数が含まれている
MS 標準 C ライブラリであり、 cdecl 呼び出し規約を使うことに注意してく
ださい::

   >>> from ctypes import *
   >>> print windll.kernel32 # doctest: +WINDOWS
   <WinDLL 'kernel32', handle ... at ...>
   >>> print cdll.msvcrt # doctest: +WINDOWS
   <CDLL 'msvcrt', handle ... at ...>
   >>> libc = cdll.msvcrt # doctest: +WINDOWS
   >>>

Windows では通常の ``.dll`` ファイル拡張子を自動的に追加します。

Linux ではライブラリをロードするために拡張子を *含む* ファイル名を指定
する必要があるので、ロードしたライブラリに対する属性アクセスはできませ
ん。
dll ローダーの :meth:`LoadLibrary` メソッドを使うか、コンストラクタを
呼び出して CDLL のインスタンスを作ることでライブラリをロードするかのど
ちらかを行わなければなりません::

   >>> cdll.LoadLibrary("libc.so.6") # doctest: +LINUX
   <CDLL 'libc.so.6', handle ... at ...>
   >>> libc = CDLL("libc.so.6")     # doctest: +LINUX
   >>> libc                         # doctest: +LINUX
   <CDLL 'libc.so.6', handle ... at ...>
   >>>

.. XXX Add section for Mac OS X.


.. _ctypes-accessing-functions-from-loaded-dlls:

ロードしたdllから関数にアクセスする
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

dll オブジェクトの属性として関数にアクセスします::

   >>> from ctypes import *
   >>> libc.printf
   <_FuncPtr object at 0x...>
   >>> print windll.kernel32.GetModuleHandleA # doctest: +WINDOWS
   <_FuncPtr object at 0x...>
   >>> print windll.kernel32.MyOwnFunction # doctest: +WINDOWS
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
     File "ctypes.py", line 239, in __getattr__
       func = _StdcallFuncPtr(name, self)
   AttributeError: function 'MyOwnFunction' not found
   >>>

``kernel32`` や ``user32`` のような win32 システム dll は、多くの場合
関数の UNICODE バージョンに加えて ANSI バージョンもエクスポートするこ
とに注意してください。 UNICODE バージョンは後ろに ``W`` が付いた名前で
エクスポートされ、 ANSI バージョンは ``A`` が付いた名前でエクスポート
されます。
与えられたモジュールの *モジュールハンドル* を返す win32
``GetModuleHandle`` 関数は次のような C プロトタイプを持ちます。
UNICODE バージョンが定義されているかどうかにより ``GetModuleHandle``
としてどちらか一つを公開するためにマクロが使われます::

   /* ANSI version */
   HMODULE GetModuleHandleA(LPCSTR lpModuleName);
   /* UNICODE version */
   HMODULE GetModuleHandleW(LPCWSTR lpModuleName);

*windll* は魔法を使ってどちらか一つを選ぶようなことはしません。
``GetModuleHandleA`` もしくは ``GetModuleHandleW`` を明示的に指定して
必要とするバージョンにアクセスし、文字列かユニコード文字列を使ってそれ
ぞれ呼び出さなければなりません。

時には、 dll が関数を ``"??2@YAPAXI@Z"`` のような Python 識別子として
有効でない名前でエクスポートすることがあります。このような場合に関数を
取り出すには、 :func:`getattr` を使わなければなりません。::

   >>> getattr(cdll.msvcrt, "??2@YAPAXI@Z") # doctest: +WINDOWS
   <_FuncPtr object at 0x...>
   >>>

Windows では、名前ではなく序数によって関数をエクスポートする dll もあ
ります。こうした関数には序数を使って dll オブジェクトにインデックス指
定することでアクセスします::

   >>> cdll.kernel32[1] # doctest: +WINDOWS
   <_FuncPtr object at 0x...>
   >>> cdll.kernel32[0] # doctest: +WINDOWS
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
     File "ctypes.py", line 310, in __getitem__
       func = _StdcallFuncPtr(name, self)
   AttributeError: function ordinal 0 not found
   >>>


.. _ctypes-calling-functions:

関数を呼び出す
^^^^^^^^^^^^^^

これらの関数は他の Python 呼び出し可能オブジェクトと同じように呼び出す
ことができます。
この例では ``time()`` 関数 (Unixエポックからのシステム時間を秒単位で返
す) と、 ``GetModuleHandleA()`` 関数 (win32モジュールハンドルを返す)
を使います。

この例は両方の関数を NULL ポインタとともに呼び出します (``None`` を
NULL ポインタとして使う必要があります)::

   >>> print libc.time(None) # doctest: +SKIP
   1150640792
   >>> print hex(windll.kernel32.GetModuleHandleA(None)) # doctest: +WINDOWS
   0x1d000000
   >>>

:mod:`ctypes` は引数の数を間違えたり、あるいは呼び出し規約を間違えた関数
呼び出しからあなたを守ろうとします。残念ながら、これは Windows でしか
機能しません。関数が返った後にスタックを調べることでこれを行います。し
たがって、エラーは発生しますが、その関数は呼び出された *後です*::

   >>> windll.kernel32.GetModuleHandleA() # doctest: +WINDOWS
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
   ValueError: Procedure probably called with not enough arguments (4 bytes missing)
   >>> windll.kernel32.GetModuleHandleA(0, 0) # doctest: +WINDOWS
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
   ValueError: Procedure probably called with too many arguments (4 bytes in excess)
   >>>

同じ例外が ``cdecl`` 呼び出し規約を使って ``stdcall`` 関数を呼び出した
ときに送出されますし、逆の場合も同様です。::

   >>> cdll.kernel32.GetModuleHandleA(None) # doctest: +WINDOWS
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
   ValueError: Procedure probably called with not enough arguments (4 bytes missing)
   >>>

   >>> windll.msvcrt.printf("spam") # doctest: +WINDOWS
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
   ValueError: Procedure probably called with too many arguments (4 bytes in excess)
   >>>

正しい呼び出し規約を知るためには、呼び出したい関数についての C ヘッダ
ファイルもしくはドキュメントを見なければなりません。

Windows では、関数が無効な引数とともに呼び出された場合の一般保護例外による
クラッシュを防ぐために、 :mod:`ctypes` は win32 構造化例外処理を使います::

   >>> windll.kernel32.GetModuleHandleA(32) # doctest: +WINDOWS
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
   WindowsError: exception: access violation reading 0x00000020
   >>>

しかし、 :mod:`ctypes` を使って Python をクラッシュさせる方法は十分なほど
あるので、よく注意すべきです。

``None`` 、整数、長整数、バイト文字列およびユニコード文字列だけが、
こうした関数呼び出しにおいてパラメータとして直接使えるネイティブの
Python オブジェクトです。 ``None`` は C の ``NULL`` ポインタとして渡さ
れ、バイト文字列とユニコード文字列はそのデータを含むメモリブロックへの
ポインタ (:c:type:`char *` または :c:type:`wchar_t *`) として渡されます。
Python 整数と Python 長整数はプラットホームのデフォルトの C :c:type:`int` 型として
渡され、その値は C :c:type:`int` 型に合うようにマスクされます。

他のパラメータ型をもつ関数呼び出しに移る前に、 :mod:`ctypes` データ型につ
いてさらに学ぶ必要があります。


.. _ctypes-fundamental-data-types:

基本のデータ型
^^^^^^^^^^^^^^

:mod:`ctypes` はたくさんの C と互換性のあるデータ型を定義しています :

+-----------------------+-----------------------------------+----------------------------+
| ctypes の型           | C の型                            | Python の型                |
+=======================+===================================+============================+
| :class:`c_bool`       | :c:type:`_Bool`                   | bool (1)                   |
+-----------------------+-----------------------------------+----------------------------+
| :class:`c_char`       | :c:type:`char`                    | 1文字の文字列              |
+-----------------------+-----------------------------------+----------------------------+
| :class:`c_wchar`      | :c:type:`wchar_t`                 | 1文字のユニコード文字列    |
+-----------------------+-----------------------------------+----------------------------+
| :class:`c_byte`       | :c:type:`char`                    | 整数/長整数                |
+-----------------------+-----------------------------------+----------------------------+
| :class:`c_ubyte`      | :c:type:`unsigned char`           | 整数/長整数                |
+-----------------------+-----------------------------------+----------------------------+
| :class:`c_short`      | :c:type:`short`                   | 整数/長整数                |
+-----------------------+-----------------------------------+----------------------------+
| :class:`c_ushort`     | :c:type:`unsigned short`          | 整数/長整数                |
+-----------------------+-----------------------------------+----------------------------+
| :class:`c_int`        | :c:type:`int`                     | 整数/長整数                |
+-----------------------+-----------------------------------+----------------------------+
| :class:`c_uint`       | :c:type:`unsigned int`            | 整数/長整数                |
+-----------------------+-----------------------------------+----------------------------+
| :class:`c_long`       | :c:type:`long`                    | 整数/長整数                |
+-----------------------+-----------------------------------+----------------------------+
| :class:`c_ulong`      | :c:type:`unsigned long`           | 整数/長整数                |
+-----------------------+-----------------------------------+----------------------------+
| :class:`c_longlong`   | :c:type:`__int64` または          | 整数/長整数                |
|                       | :c:type:`long long`               |                            |
+-----------------------+-----------------------------------+----------------------------+
| :class:`c_ulonglong`  | :c:type:`unsigned __int64` または | 整数/長整数                |
|                       | :c:type:`unsigned long long`      |                            |
+-----------------------+-----------------------------------+----------------------------+
| :class:`c_float`      | :c:type:`float`                   | 浮動小数点数               |
+-----------------------+-----------------------------------+----------------------------+
| :class:`c_double`     | :c:type:`double`                  | 浮動小数点数               |
+-----------------------+-----------------------------------+----------------------------+
| :class:`c_longdouble` | :c:type:`longdouble`              | 浮動小数点数               |
+-----------------------+-----------------------------------+----------------------------+
| :class:`c_char_p`     | :c:type:`char *` (NUL 終端)       | 文字列または ``None``      |
+-----------------------+-----------------------------------+----------------------------+
| :class:`c_wchar_p`    | :c:type:`wchar_t *` (NUL 終端)    | ユニコードまたは ``None``  |
+-----------------------+-----------------------------------+----------------------------+
| :class:`c_void_p`     | :c:type:`void *`                  | 整数/長整数または ``None`` |
+-----------------------+-----------------------------------+----------------------------+

.. x*

(1)
   コンストラクタは任意のオブジェクトをその真偽値として受け取ります。


これら全ての型はその型を呼び出すことによって作成でき、オプションとして
型と値が合っている初期化子を指定することができます::

   >>> c_int()
   c_long(0)
   >>> c_char_p("Hello, World")
   c_char_p('Hello, World')
   >>> c_ushort(-3)
   c_ushort(65533)
   >>>

これらの型は変更可能であり、値を後で変更することもできます::

   >>> i = c_int(42)
   >>> print i
   c_long(42)
   >>> print i.value
   42
   >>> i.value = -99
   >>> print i.value
   -99
   >>>

新しい値をポインタ型 :class:`c_char_p`, :class:`c_wchar_p` および
:class:`c_void_p` のインスタンスへ代入すると、変わるのは指している
*メモリ位置* であって、メモリブロックの *内容ではありません* 
(これは当然で、なぜなら、 Python 文字列は変更不可能だからです)::

   >>> s = "Hello, World"
   >>> c_s = c_char_p(s)
   >>> print c_s
   c_char_p('Hello, World')
   >>> c_s.value = "Hi, there"
   >>> print c_s
   c_char_p('Hi, there')
   >>> print s                 # 最初の文字列は変更されていない
   Hello, World
   >>>

しかし、変更可能なメモリを指すポインタであることを想定している関数へ
それらを渡さないように注意すべきです。もし変更可能なメモリブロックが必要
なら、 ctypes には :func:`create_string_buffer` 関数があり、
いろいろな方法で作成することできます。
現在のメモリブロックの内容は ``raw`` プロパティを使ってアクセス (ある
いは変更) することができます。もし現在のメモリブロックに NUL 終端文字
列としてアクセスしたいなら、 ``value`` プロパティを使ってください::

   >>> from ctypes import *
   >>> p = create_string_buffer(3)      # 3バイトのバッファを作成、NULで初期化される
   >>> print sizeof(p), repr(p.raw)
   3 '\x00\x00\x00'
   >>> p = create_string_buffer("Hello")      # NUL終端文字列を含むバッファを作成
   >>> print sizeof(p), repr(p.raw)
   6 'Hello\x00'
   >>> print repr(p.value)
   'Hello'
   >>> p = create_string_buffer("Hello", 10)  # 10バイトのバッファを作成
   >>> print sizeof(p), repr(p.raw)
   10 'Hello\x00\x00\x00\x00\x00'
   >>> p.value = "Hi"
   >>> print sizeof(p), repr(p.raw)
   10 'Hi\x00lo\x00\x00\x00\x00\x00'
   >>>

:func:`create_string_buffer` 関数は初期の ctypes リリースにあった
:func:`c_string` 関数だけでなく、 (エイリアスとしてはまだ利用できる)
:func:`c_buffer` 関数をも置き換えるものです。
C の型 :c:type:`wchar_t` のユニコード文字を含む変更可能なメモリブロックを
作成するには、 :func:`create_unicode_buffer` 関数を使ってください。


.. _ctypes-calling-functions-continued:

続・関数を呼び出す
^^^^^^^^^^^^^^^^^^

printf は :data:`sys.stdout` では *なく* 、本物の標準出力チャンネルへ
プリントすることに注意してください。したがって、これらの例はコンソールプロ
ンプトでのみ動作し、 *IDLE* や *PythonWin* では動作しません。::

   >>> printf = libc.printf
   >>> printf("Hello, %s\n", "World!")
   Hello, World!
   14
   >>> printf("Hello, %S\n", u"World!")
   Hello, World!
   14
   >>> printf("%d bottles of beer\n", 42)
   42 bottles of beer
   19
   >>> printf("%f bottles of beer\n", 42.5)
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
   ArgumentError: argument 2: exceptions.TypeError: Don't know how to convert parameter 2
   >>>

前に述べたように、必要な C のデータ型へ変換できるようにするためには、
整数、文字列およびユニコード文字列を除くすべての Python 型を対応する
:mod:`ctypes` 型でラップしなければなりません。::

   >>> printf("An int %d, a double %f\n", 1234, c_double(3.14))
   An int 1234, a double 3.140000
   31
   >>>


.. _ctypes-calling-functions-with-own-custom-data-types:

自作のデータ型とともに関数を呼び出す
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

自作のクラスのインスタンスを関数引数として使えるように、 :mod:`ctypes`
引数変換をカスタマイズすることもできます。
:mod:`ctypes` は :attr:`_as_parameter_` 属性を探し出し、関数引数として使います。
もちろん、整数、文字列もしくはユニコードの中の一つでなければなりません。::

   >>> class Bottles(object):
   ...     def __init__(self, number):
   ...         self._as_parameter_ = number
   ...
   >>> bottles = Bottles(42)
   >>> printf("%d bottles of beer\n", bottles)
   42 bottles of beer
   19
   >>>

インスタンスのデータを :attr:`_as_parameter_` インスタンス変数の中に入
れたくない場合には、そのデータを利用できるようにする :func:`property`
を定義することができます。


.. _ctypes-specifying-required-argument-types:

要求される引数の型を指定する (関数プロトタイプ)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:attr:`argtypes` 属性を設定することによって、 DLL からエクスポートされ
ている関数に要求される引数の型を指定することができます。

:attr:`argtypes` は C データ型のシーケンスでなければなりません (この場
合 ``printf`` 関数はおそらく良い例ではありません。なぜなら、引数の数が
可変であり、フォーマット文字列に依存した異なる型のパラメータを取るから
です。一方では、この機能の実験にはとても便利です)。::

   >>> printf.argtypes = [c_char_p, c_char_p, c_int, c_double]
   >>> printf("String '%s', Int %d, Double %f\n", "Hi", 10, 2.2)
   String 'Hi', Int 10, Double 2.200000
   37
   >>>

(C の関数のプロトタイプのように) 書式を指定すると互換性のない引数型に
なるのを防ぎ、引数を有効な型へ変換しようとします。::

   >>> printf("%d %d %d", 1, 2, 3)
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
   ArgumentError: argument 2: exceptions.TypeError: wrong type
   >>> printf("%s %d %f\n", "X", 2, 3)
   X 2 3.000000
   13
   >>>

関数呼び出しへ渡す自作のクラスを定義した場合には、 :attr:`argtypes` シー
ケンスの中で使えるようにするために、そのクラスに :meth:`from_param` ク
ラスメソッドを実装しなければなりません。
:meth:`from_param` クラスメソッドは関数呼び出しへ渡された Python オブ
ジェクトを受け取り、型チェックもしくはこのオブジェクトが受け入れ可能で
あると確かめるために必要なことはすべて行ってから、オブジェクト自身、
:attr:`_as_parameter_` 属性、あるいは、この場合に C 関数引数として渡し
たい何かの値を返さなければなりません。
繰り返しになりますが、その返される結果は整数、文字列、ユニコード、
:mod:`ctypes` インスタンス、あるいは :attr:`_as_parameter_` 属性をもつ
オブジェクトであるべきです。


.. _ctypes-return-types:

戻り値の型
^^^^^^^^^^

デフォルトでは、関数は C :c:type:`int` を返すと仮定されます。他の戻り値の型
を指定するには、関数オブジェクトの :attr:`restype` 属性に設定します。

さらに高度な例として、 ``strchr`` 関数を使います。この関数は文字列ポイ
ンタと char を受け取り、文字列へのポインタを返します。::

   >>> strchr = libc.strchr
   >>> strchr("abcdef", ord("d")) # doctest: +SKIP
   8059983
   >>> strchr.restype = c_char_p # c_char_pは文字列へのポインタ
   >>> strchr("abcdef", ord("d"))
   'def'
   >>> print strchr("abcdef", ord("x"))
   None
   >>>

上の ``ord("x")`` 呼び出しを避けたいなら、 :attr:`argtypes` 属性を設定
することができます。
二番目の引数が一文字の Python 文字列から C の char へ変換されます。::

   >>> strchr.restype = c_char_p
   >>> strchr.argtypes = [c_char_p, c_char]
   >>> strchr("abcdef", "d")
   'def'
   >>> strchr("abcdef", "def")
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
   ArgumentError: argument 2: exceptions.TypeError: one character string expected
   >>> print strchr("abcdef", "x")
   None
   >>> strchr("abcdef", "d")
   'def'
   >>>

外部関数が整数を返す場合は、 :attr:`restype` 属性として呼び出し可能な
Python オブジェクト (例えば、関数またはクラス) を使うこともできます。
呼び出し可能オブジェクトは C 関数が返す *整数* とともに呼び出され、
この呼び出しの結果は関数呼び出しの結果として使われるでしょう。
これはエラーの戻り値をチェックして自動的に例外を送出させるために役に立
ちます。::

   >>> GetModuleHandle = windll.kernel32.GetModuleHandleA # doctest: +WINDOWS
   >>> def ValidHandle(value):
   ...     if value == 0:
   ...         raise WinError()
   ...     return value
   ...
   >>>
   >>> GetModuleHandle.restype = ValidHandle # doctest: +WINDOWS
   >>> GetModuleHandle(None) # doctest: +WINDOWS
   486539264
   >>> GetModuleHandle("something silly") # doctest: +WINDOWS
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
     File "<stdin>", line 3, in ValidHandle
   WindowsError: [Errno 126] The specified module could not be found.
   >>>

``WinError`` はエラーコードの文字列表現を得るために Windows の
``FormatMessage()`` api を呼び出し、例外を *返す* 関数です。
``WinError`` はオプションでエラーコードパラメータを取ります。このパラ
メータが使われない場合は、エラーコードを取り出すために
:func:`GetLastError` を呼び出します。

:attr:`errcheck` 属性によってもっと強力なエラーチェック機構を利用でき
ることに注意してください。詳細はリファレンスマニュアルを参照してくださ
い。


.. _ctypes-passing-pointers:

ポインタを渡す(または、パラメータの参照渡し)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

時には、 C api 関数がパラメータのデータ型として *ポインタ* を想定して
いることがあります。おそらくパラメータと同一の場所に書き込むためか、も
しくはそのデータが大きすぎて値渡しできない場合です。これは *パラメータ
の参照渡し* としても知られています。

:mod:`ctypes` は :func:`byref` 関数をエクスポートしており、パラメータを
参照渡しするために使用します。 :func:`pointer` 関数を使っても同じ効果が得ら
れます。
しかし、 :func:`pointer` は本当のポインタオブジェクトを構築するためより
多くの処理を行うことから、 Python 側でポインタオブジェクト自体を必要とし
ないならば :func:`byref` を使う方がより高速です。::

   >>> i = c_int()
   >>> f = c_float()
   >>> s = create_string_buffer('\000' * 32)
   >>> print i.value, f.value, repr(s.value)
   0 0.0 ''
   >>> libc.sscanf("1 3.14 Hello", "%d %f %s",
   ...             byref(i), byref(f), s)
   3
   >>> print i.value, f.value, repr(s.value)
   1 3.1400001049 'Hello'
   >>>


.. _ctypes-structures-unions:

構造体と共用体
^^^^^^^^^^^^^^

構造体と共用体は :mod:`ctypes` モジュールに定義されている
:class:`Structure` および :class:`Union` ベースクラスからの派生クラスでなけ
ればなりません。それぞれのサブクラスは :attr:`_fields_` 属性を定義する
必要があります。 :attr:`_fields_` は *フィールド名* と *フィールド型*
を持つ *2要素タプル* のリストでなければなりません。

フィールド型は :class:`c_int` か他の :mod:`ctypes` 型 (構造体、共用体、
配列、ポインタ) から派生した :mod:`ctypes` 型である必要があります。

*x* と *y* という名前の二つの整数からなる簡単な POINT 構造体の例で
す。コンストラクタで構造体の初期化する方法の説明にもなっています。::

   >>> from ctypes import *
   >>> class POINT(Structure):
   ...     _fields_ = [("x", c_int),
   ...                 ("y", c_int)]
   ...
   >>> point = POINT(10, 20)
   >>> print point.x, point.y
   10 20
   >>> point = POINT(y=5)
   >>> print point.x, point.y
   0 5
   >>> POINT(1, 2, 3)
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
   ValueError: too many initializers
   >>>

また、さらに複雑な構造体を構成することができます。 Structure はそれ自
体がフィールド型に構造体を使うことで他の構造体を内部に持つことができま
す。

*upperleft* と *lowerright* という名前の二つの POINT を持つ RECT
構造体です。::

   >>> class RECT(Structure):
   ...     _fields_ = [("upperleft", POINT),
   ...                 ("lowerright", POINT)]
   ...
   >>> rc = RECT(point)
   >>> print rc.upperleft.x, rc.upperleft.y
   0 5
   >>> print rc.lowerright.x, rc.lowerright.y
   0 0
   >>>

入れ子になった構造体はいくつかの方法を用いてコンストラクタで初期化す
ることができます。::

   >>> r = RECT(POINT(1, 2), POINT(3, 4))
   >>> r = RECT((1, 2), (3, 4))

フィールド :term:`descriptor` (記述子)は *クラス* から取り出せます。デ
バッグするときに役に立つ情報を得ることができます::

   >>> print POINT.x
   <Field type=c_long, ofs=0, size=4>
   >>> print POINT.y
   <Field type=c_long, ofs=4, size=4>
   >>>


.. _ctypes-structureunion-alignment-byte-order:

構造体/共用体アライメントとバイトオーダー
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

デフォルトでは、 Structure と Union のフィールドは C コンパイラが行う
のと同じ方法でアライメントされています。サブクラスを定義するときに
:attr:`_pack_` クラス属性を指定することでこの動作を変えることは可能です。
このクラス属性には正の整数を設定する必要があり、フィールドの最大アライ
メントを指定します。これは MSVC で ``#pragma pack(n)`` が行っていること
同じです。

:mod:`ctypes` は Structure と Union に対してネイティブのバイトオーダーを
使います。
ネイティブではないバイトオーダーの構造体を作成するには、
:class:`BigEndianStructure`, :class:`LittleEndianStructure`, 
:class:`BigEndianUnion` および :class:`LittleEndianUnion`
ベースクラスの中の一つを使います。これらのクラスに
ポインタフィールドを持たせることはできません。


.. _ctypes-bit-fields-in-structures-unions:

構造体と共用体におけるビットフィールド
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

ビットフィールドを含む構造体と共用体を作ることができます。ビットフィー
ルドは整数フィールドに対してのみ作ることができ、ビット幅は
:attr:`_fields_` タプルの第三要素で指定します。::

   >>> class Int(Structure):
   ...     _fields_ = [("first_16", c_int, 16),
   ...                 ("second_16", c_int, 16)]
   ...
   >>> print Int.first_16
   <Field type=c_long, ofs=0:0, bits=16>
   >>> print Int.second_16
   <Field type=c_long, ofs=0:16, bits=16>
   >>>


.. _ctypes-arrays:

配列
^^^^

Array はシーケンスであり、決まった数の同じ型のインスタンスを持ちます。

推奨されている配列の作成方法はデータ型に正の整数を掛けることです。::

   TenPointsArrayType = POINT * 10

ややわざとらしいデータ型の例になりますが、他のものに混ざって 4 個の
POINT がある構造体です。::

   >>> from ctypes import *
   >>> class POINT(Structure):
   ...    _fields_ = ("x", c_int), ("y", c_int)
   ...
   >>> class MyStruct(Structure):
   ...    _fields_ = [("a", c_int),
   ...                ("b", c_float),
   ...                ("point_array", POINT * 4)]
   >>>
   >>> print len(MyStruct().point_array)
   4
   >>>

インスタンスはクラスを呼び出す通常の方法で作成します。::

   arr = TenPointsArrayType()
   for pt in arr:
       print pt.x, pt.y

上記のコードは ``0 0`` という行が並んだものを表示します。配列の要素が
ゼロで初期化されているためです。

正しい型の初期化子を指定することもできます。::

   >>> from ctypes import *
   >>> TenIntegers = c_int * 10
   >>> ii = TenIntegers(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
   >>> print ii
   <c_long_Array_10 object at 0x...>
   >>> for i in ii: print i,
   ...
   1 2 3 4 5 6 7 8 9 10
   >>>


.. _ctypes-pointers:

ポインタ
^^^^^^^^

ポインタのインスタンスは :mod:`ctypes` 型に対して :func:`pointer` 関数を呼び
出して作成します。::

   >>> from ctypes import *
   >>> i = c_int(42)
   >>> pi = pointer(i)
   >>>

ポインタインスタンスはポインタが指すオブジェクト (上の例では ``i`` )
を返す :attr:`contents` 属性を持ちます。::

   >>> pi.contents
   c_long(42)
   >>>

:mod:`ctypes` は OOR (original object return 、元のオブジェクトを返すこと)
ではないことに注意してください。属性を取り出す度に、新しい同等のオブジェ
クトを作成しているのです。::

   >>> pi.contents is i
   False
   >>> pi.contents is pi.contents
   False
   >>>

別の :class:`c_int` インスタンスがポインタの contents 属性に代入される
と、これが記憶されているメモリ位置を指すポインタに変化します。::

   >>> i = c_int(99)
   >>> pi.contents = i
   >>> pi.contents
   c_long(99)
   >>>

.. XXX Document dereferencing pointers, and that it is preferred over the
   .contents attribute.

ポインタインスタンスは整数でインデックス指定することもできます。::

   >>> pi[0]
   99
   >>>

整数インデックスへ代入するとポインタが指す値が変更されます。::

   >>> print i
   c_long(99)
   >>> pi[0] = 22
   >>> print i
   c_long(22)
   >>>

0 ではないインデックスを使うこともできますが、 C の場合と同じように自
分が何をしているかを理解している必要があります。
任意のメモリ位置にアクセスもしくは変更できるのです。一般的にこの機能を
使うのは、 C 関数からポインタを受け取り、そのポインタが単一の要素では
なく実際に配列を指していると *分かっている* 場合だけです。

舞台裏では、 :func:`pointer` 関数は単にポインタインスタンスを作成するとい
う以上のことを行っています。はじめにポインタ *型* を作成する必要があり
ます。
これは任意の :mod:`ctypes` 型を受け取る :func:`POINTER` 関数を使って行われ、
新しい型を返します。::

   >>> PI = POINTER(c_int)
   >>> PI
   <class 'ctypes.LP_c_long'>
   >>> PI(42)
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
   TypeError: expected c_long instead of int
   >>> PI(c_int(42))
   <ctypes.LP_c_long object at 0x...>
   >>>

ポインタ型を引数なしで呼び出すと ``NULL`` ポインタを作成します。
``NULL`` ポインタは ``False`` ブール値を持っています。::

   >>> null_ptr = POINTER(c_int)()
   >>> print bool(null_ptr)
   False
   >>>

:mod:`ctypes` はポインタの指す値を取り出すときに ``NULL`` かどうかを調べ
ます(しかし、 ``NULL`` でない不正なポインタの指す値の取り出す行為は
Python をクラッシュさせるでしょう)。::

   >>> null_ptr[0]
   Traceback (most recent call last):
       ....
   ValueError: NULL pointer access
   >>>

   >>> null_ptr[0] = 1234
   Traceback (most recent call last):
       ....
   ValueError: NULL pointer access
   >>>


.. _ctypes-type-conversions:

型変換
^^^^^^

たいていの場合、 ctypes は厳密な型チェックを行います。これが意味するの
は、関数の :attr:`argtypes` リスト内に、もしくは、構造体定義におけるメ
ンバーフィールドの型として ``POINTER(c_int)`` がある場合、厳密に同じ型
のインスタンスだけを受け取るということです。このルールには ctypes が他
のオブジェクトを受け取る場合に例外がいくつかあります。例えば、ポインタ
型の代わりに互換性のある配列インスタンスを渡すことができます。このよう
に、 ``POINTER(c_int)`` に対して、 ctypes は c_int の配列を受け取りま
す。::

   >>> class Bar(Structure):
   ...     _fields_ = [("count", c_int), ("values", POINTER(c_int))]
   ...
   >>> bar = Bar()
   >>> bar.values = (c_int * 3)(1, 2, 3)
   >>> bar.count = 3
   >>> for i in range(bar.count):
   ...     print bar.values[i]
   ...
   1
   2
   3
   >>>

POINTER型フィールドを ``NULL`` に設定するために、 ``None`` を代入して
もかまいません。::

   >>> bar.values = None
   >>>

.. XXX list other conversions...

時には、非互換な型のインスタンスであることもあります。 C では、ある型
を他の型へキャストすることができます。 :mod:`ctypes` は同じやり方で使える
:func:`cast` 関数を提供しています。上で定義した ``Bar`` 構造体は
``POINTER(c_int)`` ポインタまたは :class:`c_int` 配列を ``values`` フィー
ルドに対して受け取り、他の型のインスタンスは受け取りません::

   >>> bar.values = (c_byte * 4)()
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
   TypeError: incompatible types, c_byte_Array_4 instance instead of LP_c_long instance
   >>>

このような場合には、 :func:`cast` 関数が便利です。

:func:`cast` 関数は ctypes インスタンスを異なる ctypes データ型を指すポイ
ンタへキャストするために使えます。 :func:`cast` は二つのパラメータ、ある種
のポインタかそのポインタへ変換できる ctypes オブジェクトと、 ctypes ポ
インタ型を取ります。そして、第二引数のインスタンスを返します。
このインスタンスは第一引数と同じメモリブロックを参照しています::

   >>> a = (c_byte * 4)()
   >>> cast(a, POINTER(c_int))
   <ctypes.LP_c_long object at ...>
   >>>

したがって、 :func:`cast` を ``Bar`` 構造体の ``values`` フィールドへ代入
するために使うことができます::

   >>> bar = Bar()
   >>> bar.values = cast((c_byte * 4)(), POINTER(c_int))
   >>> print bar.values[0]
   0
   >>>


.. _ctypes-incomplete-types:

不完全型
^^^^^^^^

*不完全型* はメンバーがまだ指定されていない構造体、共用体もしくは配列
です。 C では、前方宣言により指定され、後で定義されます。::

   struct cell; /* 前方宣言 */

   struct {
       char *name;
       struct cell *next;
   } cell;

ctypes コードへの直接的な変換ではこうなるでしょう。しかし、動作しませ
ん::

   >>> class cell(Structure):
   ...     _fields_ = [("name", c_char_p),
   ...                 ("next", POINTER(cell))]
   ...
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
     File "<stdin>", line 2, in cell
   NameError: name 'cell' is not defined
   >>>

なぜなら、新しい ``class cell`` はクラス文自体の中では利用できないから
です。 :mod:`ctypes` では、 ``cell`` クラスを定義して、 :attr:`_fields_`
属性をクラス文の後で設定することができます。::

   >>> from ctypes import *
   >>> class cell(Structure):
   ...     pass
   ...
   >>> cell._fields_ = [("name", c_char_p),
   ...                  ("next", POINTER(cell))]
   >>>

試してみましょう。 ``cell`` のインスタンスを二つ作り、互いに参照し合う
ようにします。最後に、つながったポインタを何度かたどります。::

   >>> c1 = cell()
   >>> c1.name = "foo"
   >>> c2 = cell()
   >>> c2.name = "bar"
   >>> c1.next = pointer(c2)
   >>> c2.next = pointer(c1)
   >>> p = c1
   >>> for i in range(8):
   ...     print p.name,
   ...     p = p.next[0]
   ...
   foo bar foo bar foo bar foo bar
   >>>


.. _ctypes-callback-functions:

コールバック関数
^^^^^^^^^^^^^^^^

:mod:`ctypes` は C の呼び出し可能な関数ポインタを Python 呼び出し可能オブ
ジェクトから作成できるようにします。これらは *コールバック関数* と呼ば
れることがあります。

最初に、コールバック関数のためのクラスを作る必要があります。そのクラス
には呼び出し規約、戻り値の型およびこの関数が受け取る引数の数と型につい
ての情報があります。

CFUNCTYPE ファクトリ関数は通常の cdecl 呼び出し規約を用いてコールバッ
ク関数のための型を作成します。
Windows では、 WINFUNCTYPE ファクトリ関数が stdcall 呼び出し規約を用い
てコールバック関数の型を作成します。

これらのファクトリ関数はともに最初の引数に戻り値の型、残りの引数として
コールバック関数が想定する引数の型を渡して呼び出されます。

標準 C ライブラリの :func:`qsort` 関数を使う例を示します。これはコール
バック関数の助けをかりて要素をソートするために使われます。
:func:`qsort` は整数の配列をソートするために使われます。::

   >>> IntArray5 = c_int * 5
   >>> ia = IntArray5(5, 1, 7, 33, 99)
   >>> qsort = libc.qsort
   >>> qsort.restype = None
   >>>

:func:`qsort` はソートするデータを指すポインタ、データ配列の要素の数、
要素の一つの大きさ、およびコールバック関数である比較関数へのポインタを
引数に渡して呼び出さなければなりません。そして、コールバック関数は要素
を指す二つのポインタを渡されて呼び出され、一番目が二番目より小さいなら
負の数を、等しいならゼロを、それ以外なら正の数を返さなければなりません。

コールバック関数は整数へのポインタを受け取り、整数を返す必要があります。
まず、コールバック関数のための ``type`` を作成します。::

   >>> CMPFUNC = CFUNCTYPE(c_int, POINTER(c_int), POINTER(c_int))
   >>>

コールバック関数のはじめての実装なので、受け取った引数を単純に表示して、
0 を返します (漸進型開発 (incremental development)です ;-)::

   >>> def py_cmp_func(a, b):
   ...     print "py_cmp_func", a, b
   ...     return 0
   ...
   >>>

C の呼び出し可能なコールバック関数を作成します。::

   >>> cmp_func = CMPFUNC(py_cmp_func)
   >>>

そうすると、準備完了です。::

   >>> qsort(ia, len(ia), sizeof(c_int), cmp_func) # doctest: +WINDOWS
   py_cmp_func <ctypes.LP_c_long object at 0x00...> <ctypes.LP_c_long object at 0x00...>
   py_cmp_func <ctypes.LP_c_long object at 0x00...> <ctypes.LP_c_long object at 0x00...>
   py_cmp_func <ctypes.LP_c_long object at 0x00...> <ctypes.LP_c_long object at 0x00...>
   py_cmp_func <ctypes.LP_c_long object at 0x00...> <ctypes.LP_c_long object at 0x00...>
   py_cmp_func <ctypes.LP_c_long object at 0x00...> <ctypes.LP_c_long object at 0x00...>
   py_cmp_func <ctypes.LP_c_long object at 0x00...> <ctypes.LP_c_long object at 0x00...>
   py_cmp_func <ctypes.LP_c_long object at 0x00...> <ctypes.LP_c_long object at 0x00...>
   py_cmp_func <ctypes.LP_c_long object at 0x00...> <ctypes.LP_c_long object at 0x00...>
   py_cmp_func <ctypes.LP_c_long object at 0x00...> <ctypes.LP_c_long object at 0x00...>
   py_cmp_func <ctypes.LP_c_long object at 0x00...> <ctypes.LP_c_long object at 0x00...>
   >>>

ポインタの中身にアクセスする方法がわかっているので、コールバック関数を
再定義しましょう。::

   >>> def py_cmp_func(a, b):
   ...     print "py_cmp_func", a[0], b[0]
   ...     return 0
   ...
   >>> cmp_func = CMPFUNC(py_cmp_func)
   >>>

Windowsでの実行結果です。::

   >>> qsort(ia, len(ia), sizeof(c_int), cmp_func) # doctest: +WINDOWS
   py_cmp_func 7 1
   py_cmp_func 33 1
   py_cmp_func 99 1
   py_cmp_func 5 1
   py_cmp_func 7 5
   py_cmp_func 33 5
   py_cmp_func 99 5
   py_cmp_func 7 99
   py_cmp_func 33 99
   py_cmp_func 7 33
   >>>

linuxではソート関数がはるかに効率的に動作しており、実施する比較の数が
少ないように見えるのが不思議です。::

   >>> qsort(ia, len(ia), sizeof(c_int), cmp_func) # doctest: +LINUX
   py_cmp_func 5 1
   py_cmp_func 33 99
   py_cmp_func 7 33
   py_cmp_func 5 7
   py_cmp_func 1 7
   >>>

ええ、ほぼ完成です! 最終段階は、実際に二つの要素を比較して実用的な結果
を返すことです。::

   >>> def py_cmp_func(a, b):
   ...     print "py_cmp_func", a[0], b[0]
   ...     return a[0] - b[0]
   ...
   >>>

Windowsでの最終的な実行結果です。::

   >>> qsort(ia, len(ia), sizeof(c_int), CMPFUNC(py_cmp_func)) # doctest: +WINDOWS
   py_cmp_func 33 7
   py_cmp_func 99 33
   py_cmp_func 5 99
   py_cmp_func 1 99
   py_cmp_func 33 7
   py_cmp_func 1 33
   py_cmp_func 5 33
   py_cmp_func 5 7
   py_cmp_func 1 7
   py_cmp_func 5 1
   >>>

Linuxでは::

   >>> qsort(ia, len(ia), sizeof(c_int), CMPFUNC(py_cmp_func)) # doctest: +LINUX
   py_cmp_func 5 1
   py_cmp_func 33 99
   py_cmp_func 7 33
   py_cmp_func 1 7
   py_cmp_func 5 7
   >>>

Windows の :func:`qsort` 関数は linux バージョンより多く比較する必要が
あることがわかり、非常におもしろいですね!

簡単に確認できるように、今では配列はソートされています。::

   >>> for i in ia: print i,
   ...
   1 5 7 33 99
   >>>

**コールバック関数についての重要な注意事項:**

C コードから使われる限り、 CFUNCTYPE オブジェクトへの参照を確実に保持
してください。
:mod:`ctypes` は保持しません。もしあなたがやらなければ、オブジェクトはゴ
ミ集めされてしまい、コールバックしたときにあなたのプログラムをクラッシュ
させるかもしれません。


.. _ctypes-accessing-values-exported-from-dlls:

dllからエクスポートされている値へアクセスする
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

共有ライブラリの一部は関数だけでなく変数もエクスポートしています。
Python ライブラリにある例としては ``Py_OptimizeFlag`` 、起動時の
:option:`-O` または :option:`-OO` フラグに依存して、 0 , 1 または 2 が
設定される整数があります。

:mod:`ctypes` は型の :meth:`in_dll` クラスメソッドを使ってこのように値に
アクセスできます。 *pythonapi* はPython C api へアクセスできるようにす
るための予め定義されたシンボルです。::

   >>> opt_flag = c_int.in_dll(pythonapi, "Py_OptimizeFlag")
   >>> print opt_flag
   c_long(0)
   >>>

インタープリタが :option:`-O` を指定されて動き始めた場合、サンプルは
``c_long(1)`` を表示するでしょうし、 :option:`-OO` が指定されたならば
``c_long(2)`` を表示するでしょう。

ポインタの使い方を説明する拡張例では、 Python がエクスポートする
``PyImport_FrozenModules`` ポインタにアクセスします。

Python ドキュメントから引用すると: *このポインタは
"struct _frozen" のレコードからなり、
終端の要素のメンバが NULL かゼロになっているような配列を指すよう初期化されます。
フリーズされたモジュールを import するとき、このテーブルを検索します。
サードパーティ製のコードからこのポインタに仕掛けを講じて、
動的に生成されたフリーズ化モジュールの集合を提供するようにできます。*

.. 注: c-api/import より引用

これで、このポインタを操作することが役に立つことを証明できるでしょう。
例の大きさを制限するために、このテーブルを :mod:`ctypes` を使って読む方法
だけを示します。::

   >>> from ctypes import *
   >>>
   >>> class struct_frozen(Structure):
   ...     _fields_ = [("name", c_char_p),
   ...                 ("code", POINTER(c_ubyte)),
   ...                 ("size", c_int)]
   ...
   >>>

私たちは ``struct _frozen`` データ型を定義済みなので、このテーブルを指
すポインタを得ることができます。::

   >>> FrozenTable = POINTER(struct_frozen)
   >>> table = FrozenTable.in_dll(pythonapi, "PyImport_FrozenModules")
   >>>

``table`` が ``struct_frozen`` レコードの配列への ``pointer`` なので、
その配列に対して反復処理を行えます。しかし、ループが確実に終了するよう
にする必要があります。なぜなら、ポインタに大きさの情報がないからです。
遅かれ早かれ、アクセス違反か何かでクラッシュすることになるでしょう。
NULL エントリに達したときはループを抜ける方が良いです。::

   >>> for item in table:
   ...    print item.name, item.size
   ...    if item.name is None:
   ...        break
   ...
   __hello__ 104
   __phello__ -104
   __phello__.spam 104
   None 0
   >>>

標準 Python はフローズンモジュールとフローズンパッケージ (負のサイズの
メンバーで表されています) を持っているという事実はあまり知られておらず、
テストにだけ使われています。例えば、 ``import __hello__`` を試してみて
ください。


.. _ctypes-surprises:

予期しないこと
^^^^^^^^^^^^^^

:mod:`ctypes` には別のことを期待しているのに実際に起きることは違う
という場合があります。

次に示す例について考えてみてください。::

   >>> from ctypes import *
   >>> class POINT(Structure):
   ...     _fields_ = ("x", c_int), ("y", c_int)
   ...
   >>> class RECT(Structure):
   ...     _fields_ = ("a", POINT), ("b", POINT)
   ...
   >>> p1 = POINT(1, 2)
   >>> p2 = POINT(3, 4)
   >>> rc = RECT(p1, p2)
   >>> print rc.a.x, rc.a.y, rc.b.x, rc.b.y
   1 2 3 4
   >>> # now swap the two points
   >>> rc.a, rc.b = rc.b, rc.a
   >>> print rc.a.x, rc.a.y, rc.b.x, rc.b.y
   3 4 3 4
   >>>

うーん、最後の文に ``3 4 1 2`` と表示されることを期待していたはずです。
何が起きたのでしょうか? 上の行の ``rc.a, rc.b = rc.b, rc.a`` の各段階
はこのようになります。::

   >>> temp0, temp1 = rc.b, rc.a
   >>> rc.a = temp0
   >>> rc.b = temp1
   >>>

``temp0`` と ``temp1`` は前記の ``rc`` オブジェクトの内部バッファでま
だ使われているオブジェクトです。したがって、 ``rc.a = temp0`` を実行す
ると ``temp0`` のバッファ内容が ``rc`` のバッファへコピーされます。さ
らに、これは ``temp1`` の内容を変更します。そのため、最後の代入 ``rc.b
= temp1`` は、期待する結果にはならないのです。

Structure 、 Union および Array のサブオブジェクトを取り出しても、その
サブオブジェクトが *コピー* されるわけではなく、ルートオブジェクトの内
部バッファにアクセスするラッパーオブジェクトを取り出すことを覚えておい
てください。

期待とは違う振る舞いをする別の例はこれです。::

   >>> s = c_char_p()
   >>> s.value = "abc def ghi"
   >>> s.value
   'abc def ghi'
   >>> s.value is s.value
   False
   >>>

なぜ ``False`` と表示されるのでしょうか? ctypes インスタンスはメモリと、
メモリの内容にアクセスするいくつかの :term:`descriptor` (記述子)を含む
オブジェクトです。
メモリブロックに Python オブジェクトを保存してもオブジェクト自身が保存
される訳ではなく、オブジェクトの ``contents`` が保存されます。
その contents に再アクセスすると新しい Python オブジェクトがその度に作
られます。


.. _ctypes-variable-sized-data-types:

可変サイズのデータ型
^^^^^^^^^^^^^^^^^^^^

:mod:`ctypes` は可変サイズの配列と構造体をサポートしています。

:func:`resize` 関数は既存の ctypes オブジェクトのメモリバッファのサイズを
変更したい場合に使えます。この関数は第一引数にオブジェクト、第二引数に
要求されたサイズをバイト単位で指定します。メモリブロックはオブジェクト
型で指定される通常のメモリブロックより小さくすることはできません。
これをやろうとすると、 :exc:`ValueError` が送出されます。::

   >>> short_array = (c_short * 4)()
   >>> print sizeof(short_array)
   8
   >>> resize(short_array, 4)
   Traceback (most recent call last):
       ...
   ValueError: minimum size is 8
   >>> resize(short_array, 32)
   >>> sizeof(short_array)
   32
   >>> sizeof(type(short_array))
   8
   >>>

これはこれで上手くいっていますが、この配列の追加した要素へどうやってア
クセスするのでしょうか? この型は要素の数が 4 個であるとまだ認識してい
るので、他の要素にアクセスするとエラーになります。::

   >>> short_array[:]
   [0, 0, 0, 0]
   >>> short_array[7]
   Traceback (most recent call last):
       ...
   IndexError: invalid index
   >>>

:mod:`ctypes` で可変サイズのデータ型を使うもう一つの方法は、必要なサイズ
が分かった後に Python の動的性質を使って一つ一つデータ型を(再)定義する
ことです。


.. _ctypes-ctypes-reference:

ctypesリファレンス
------------------


.. _ctypes-finding-shared-libraries:

共有ライブラリを見つける
^^^^^^^^^^^^^^^^^^^^^^^^

コンパイルされる言語でプログラミングしている場合、共有ライブラリはプロ
グラムをコンパイル/リンクしているときと、そのプログラムが動作している
ときにアクセスされます。

ctypes ライブラリローダーはプログラムが動作しているときのように振る舞
い、ランタイムローダーを直接呼び出すのに対し、 :func:`find_library` 関数の
目的はコンパイラが行うのと似た方法でライブラリを探し出すことです。
(複数のバージョンの共有ライブラリがあるプラットホームでは、一番最近に
見つかったものがロードされます)。

:mod:`ctypes.util` モジュールはロードするライブラリを決めるのに役立つ関数
を提供します。


.. data:: find_library(name)
   :module: ctypes.util
   :noindex:

   ライブラリを見つけてパス名を返そうと試みます。 *name* は ``lib`` のよ
   うな接頭辞、 ``.so``, ``.dylib`` のような接尾辞、あるいは、バージョン
   番号が何も付いていないライブラリの名前です (これは posix リンカの
   オプション :option:`-l` に使われている形式です)。
   もしライブラリが見つからなければ、 ``None`` を返します。

厳密な機能はシステムに依存します。

Linux では、 :func:`find_library` はライブラリファイルを見つけるために外部
プログラム (``/sbin/ldconfig``, ``gcc`` および ``objdump``) を
実行しようとします。ライブラリファイルのファイル名を返します。いくつか例があります。::

   >>> from ctypes.util import find_library
   >>> find_library("m")
   'libm.so.6'
   >>> find_library("c")
   'libc.so.6'
   >>> find_library("bz2")
   'libbz2.so.1.0'
   >>>

OS X では、 :func:`find_library` はライブラリの位置を探すために、予め定義さ
れた複数の命名方法とパスを試し、成功すればフルパスを返します。::

   >>> from ctypes.util import find_library
   >>> find_library("c")
   '/usr/lib/libc.dylib'
   >>> find_library("m")
   '/usr/lib/libm.dylib'
   >>> find_library("bz2")
   '/usr/lib/libbz2.dylib'
   >>> find_library("AGL")
   '/System/Library/Frameworks/AGL.framework/AGL'
   >>>

Windows では、 :func:`find_library` はシステムの探索パスに沿って探し、
フルパスを返します。しかし、予め定義された命名方法がないため、
``find_library("c")`` のような呼び出しは失敗し、 ``None`` を返します。

もし :mod:`ctypes` を使って共有ライブラリをラップするなら、実行時にライブ
ラリを探すために :func:`find_library` を使う代わりに、開発時に共有ライブラ
リ名を決めて、ラッパーモジュールにハードコードした方が良い *かもしれません* 。


.. _ctypes-loading-shared-libraries:

共有ライブラリをロードする
^^^^^^^^^^^^^^^^^^^^^^^^^^

共有ライブラリを Python プロセスへロードする方法はいくつかあります。一
つの方法は下記のクラスの一つをインスタンス化することです。:


.. class:: CDLL(name, mode=DEFAULT_MODE, handle=None, use_errno=False, use_last_error=False)

   このクラスのインスタンスはロードされた共有ライブラリをあらわします。
   これらのライブラリの関数は標準 C 呼び出し規約を使用し、 :c:type:`int` を
   返すと仮定されます。


.. class:: OleDLL(name, mode=DEFAULT_MODE, handle=None, use_errno=False, use_last_error=False)

   Windows用: このクラスのインスタンスはロードされた共有ライブラリをあ
   らわします。これらのライブラリの関数は ``stdcall`` 呼び出し規約を使
   用し、 windows 固有の :class:`HRESULT` コードを返すと仮定されます。
   :class:`HRESULT` 値には関数呼び出しが失敗したのか成功したのかを特定
   する情報とともに、補足のエラーコードが含まれます。戻り値が失敗を知
   らせたならば、 :class:`WindowsError` が自動的に送出されます。


.. class:: WinDLL(name, mode=DEFAULT_MODE, handle=None, use_errno=False, use_last_error=False)

   Windows用: このクラスのインスタンスはロードされた共有ライブラリをあ
   らわします。これらのライブラリの関数は ``stdcall`` 呼び出し規約を使
   用し、デフォルトでは :c:type:`int` を返すと仮定されます。

   Windows CE では標準呼び出し規約だけが使われます。便宜上、このプラッ
   トホームでは、 :class:`WinDLL` と :class:`OleDLL` が標準呼び出し規
   約を使用します。

これらのライブラリがエクスポートするどの関数でも呼び出す前に Python
GIL (:term:`global interpreter lock`) は解放され、後でまた獲得されます。


.. class:: PyDLL(name, mode=DEFAULT_MODE, handle=None)

   Python GIL が関数呼び出しの間解放 *されず* 、関数実行の後に Python
   エラーフラグがチェックされるということを除けば、このクラスのインス
   タンスは :class:`CDLL` インスタンスのように振る舞います。エラーフラ
   グがセットされた場合、 Python 例外が送出されます。

   要するに、これは Python C api 関数を直接呼び出すのに便利だというだ
   けです。

これらすべてのクラスは少なくとも一つの引数、すなわちロードする共有ライ
ブラリのパスを渡して呼び出すことでインスタンス化されます。すでにロード
済みの共有ライブラリへのハンドルがあるなら、 ``handle`` 名前付き引数と
して渡すことができます。土台となっているプラットホームの ``dlopen`` ま
たは ``LoadLibrary`` 関数がプロセスへライブラリをロードするために
使われ、そのライブラリに対するハンドルを得ます。

*mode* パラメータはライブラリがどうやってロードされたかを特定するため
に使うことができます。詳細は、 :manpage:`dlopen(3)` マニュアルページを参考に
してください。 Windows では *mode* は無視されます。

*use_errno* 変数が True に設定されたとき、システムの :data:`errno` エラーナ
ンバーに安全にアクセスする ctypes の仕組みが有効化されます。
:mod:`ctypes` はシステムの :data:`errno` 変数のスレッド限定のコピーを管理します。
もし、 ``use_errno=True`` の状態で作られた外部関数を呼び出したなら、
関数呼び出し前の :data:`errno` 変数は ctypes のプライベートコピーと置き換え
られ、同じことが関数呼び出しの直後にも発生します。

:func:`ctypes.get_errno` 関数は ctypes のプライベートコピーの値を返します。
そして、 :func:`ctypes.set_errno` 関数は ctypes のプライベートコピー
を置き換え、以前の値を返します。

*use_last_error* パラメータは、 True に設定されたとき、
:func:`GetLastError` と :func:`SetLastError`  Windows API によって管理
される Windows エラーコードに対するのと同じ仕組みが有効化されます。
:func:`ctypes.get_last_error` と :func:`ctypes.set_last_error` は Windows
エラーコードの ctypes プライベートコピーを変更したり要求したりするのに
使われます。

.. versionadded:: 2.6
   *use_last_error* と *use_errno* オプション変数が追加されました。

.. data:: RTLD_GLOBAL
   :noindex:

   *mode* パラメータとして使うフラグ。このフラグが利用できないプラット
   ホームでは、整数のゼロと定義されています。


.. data:: RTLD_LOCAL
   :noindex:

   *mode* パラメータとして使うフラグ。これが利用できないプラットホーム
    では、 *RTLD_GLOBAL* と同様です。


.. data:: DEFAULT_MODE
   :noindex:

   共有ライブラリをロードするために使われるデフォルトモード。 OSX 10.3
   では *RTLD_GLOBAL* であり、そうでなければ *RTLD_LOCAL* と同じです。

これらのクラスのインスタンスには公開メソッドがありません。けれども、
:meth:`__getattr__` と :meth:`__getitem__` は特別なはたらきをします。
その共有ライブラリがエクスポートする関数に添字を使って属性としてアクセ
スできるのです。 :meth:`__getattr__` と :meth:`__getitem__` のどちらも
が結果をキャッシュし、そのため常に同じオブジェクトを返すことに注意して
ください。

次に述べる公開属性が利用できます。それらの名前はエクスポートされた関数
名に衝突しないように下線で始まります。:


.. attribute:: PyDLL._handle

   ライブラリへのアクセスに用いられるシステムハンドル。


.. attribute:: PyDLL._name

   コンストラクタに渡されたライブラリの名前。

共有ライブラリは ( :class:`LibraryLoader` クラスのインスタンスである )
前もって作られたオブジェクトの一つを使うことによってロードすることもで
きます。
それらの :meth:`LoadLibrary` メソッドを呼び出すか、ローダーインスタン
スの属性としてライブラリを取り出すかのどちらかによりロードします。


.. class:: LibraryLoader(dlltype)

   共有ライブラリをロードするクラス。 *dlltype* は :class:`CDLL` 、
   :class:`PyDLL` 、 :class:`WinDLL` もしくは :class:`OleDLL` 型の一つ
   であるべきです。

   :meth:`__getattr__` は特別なはたらきをします: ライブラリローダーイ
   ンスタンスの属性として共有ライブラリにアクセスするとそれがロードさ
   れるということを可能にします。結果はキャッシュされます。そのため、
   繰り返し属性アクセスを行うといつも同じライブラリが返されます。


   .. method:: LoadLibrary(name)

      共有ライブラリをプロセスへロードし、それを返します。このメソッド
      はライブラリの新しいインスタンスを常に返します。

これらの前もって作られたライブラリローダーを利用することができます。:


.. data:: cdll
   :noindex:

   :class:`CDLL` インスタンスを作ります。


.. data:: windll
   :noindex:

   Windows用: :class:`WinDLL` インスタンスを作ります。


.. data:: oledll
   :noindex:

   Windows用: :class:`OleDLL` インスタンスを作ります。


.. data:: pydll
   :noindex:

   :class:`PyDLL` インスタンスを作ります。

C Python api に直接アクセするために、すぐに使用できる Python 共有ライ
ブラリオブジェクトが用意されています。:


.. data:: pythonapi
   :noindex:

   属性として Python C api 関数を公開する :class:`PyDLL` のインスタン
   ス。これらすべての関数は C :c:type:`int` を返すと仮定されますが、もちろん
   常に正しいとは限りません。そのため、これらの関数を使うためには正し
   い :attr:`restype` 属性を代入しなければなりません。


.. _ctypes-foreign-functions:

外部関数
^^^^^^^^

前節で説明した通り、外部関数はロードされた共有ライブラリの属性としてア
クセスできます。デフォルトではこの方法で作成された関数オブジェクトはど
んな数の引数でも受け取り、引数としてどんな ctypes データのインスタンス
をも受け取り、そして、ライブラリローダーが指定したデフォルトの結果の値
の型を返します。関数オブジェクトはプライベートクラスのインスタンスで
す。:


.. class:: _FuncPtr

   C の呼び出し可能外部関数のためのベースクラス。

   外部関数のインスタンスも C 互換データ型です。それらは C の関数ポイ
   ンタを表しています。

   この振る舞いは外部関数オブジェクトの特別な属性に代入することによっ
   て、カスタマイズすることができます。


   .. attribute:: restype

      外部関数の結果の型を指定するために ctypes 型を代入する。何も返さ
      ない関数を表す :c:type:`void` に対しては ``None`` を使います。

      ctypes 型ではない呼び出し可能な Python オブジェクトを代入するこ
      とは可能です。このような場合、関数が C :c:type:`int` を返すと仮定され、
      呼び出し可能オブジェクトはこの整数を引数に呼び出されます。さらに
      処理を行ったり、エラーチェックをしたりできるようにするためです。
      これの使用は推奨されません。より柔軟な後処理やエラーチェックのた
      めには restype として ctypes 型を使い、 :attr:`errcheck` 属性へ
      呼び出し可能オブジェクトを代入してください。


   .. attribute:: argtypes

      関数が受け取る引数の型を指定するために ctypes 型のタプルを代入し
      ます。 ``stdcall`` 呼び出し規約をつかう関数はこのタプルの長さと同じ
      数の引数で呼び出されます。その上、 C 呼び出し規約をつかう関数は追加
      の不特定の引数も取ります。

      外部関数が呼ばれたとき、それぞれの実引数は :attr:`argtypes` タプ
      ルの要素の :meth:`from_param` クラスメソッドへ渡されます。このメ
      ソッドは実引数を外部関数が受け取るオブジェクトに合わせて変えられ
      るようにします。
      例えば、 :attr:`argtypes` タプルの :class:`c_char_p` 要素は、
      ctypes 変換規則にしたがって引数として渡されたユニコード文字列を
      バイト文字列へ変換するでしょう。

      新: ctypes 型でない要素を argtypes に入れることができますが、個々
      の要素は引数として使える値 ( 整数、文字列、 ctypes インスタンス
      ) を返す :meth:`from_param` メソッドを持っていなければなりません。
      これにより関数パラメータとしてカスタムオブジェクトを適合するよう
      に変更できるアダプタが定義可能となります。


   .. attribute:: errcheck

      Python 関数または他の呼び出し可能オブジェクトをこの属性に代入し
      ます。呼び出し可能オブジェクトは三つ以上の引数とともに呼び出され
      ます。


      .. function:: callable(result, func, arguments)
         :noindex:

         *result* は外部関数が返すもので、 :attr:`restype` 属性で指定さ
         れます。

         *func* は外部関数オブジェクト自身で、これにより複数の関数の処
         理結果をチェックまたは後処理するために、同じ呼び出し可能オブジェ
         クトを再利用できるようになります。

         *arguments* は関数呼び出しに最初に渡されたパラメータが入っ
         たタプルです。これにより使われた引数に基づた特別な振る舞いを
         させることができるようになります。

      この関数が返すオブジェクトは外部関数呼び出しから返された値でしょ
      う。しかし、戻り値をチェックして、外部関数呼び出しが失敗している
      なら例外を送出させることもできます。


.. exception:: ArgumentError()

   この例外は外部関数呼び出しが渡された引数を変換できなかったときに送
   出されます。


.. _ctypes-function-prototypes:

関数プロトタイプ
^^^^^^^^^^^^^^^^

外部関数は関数プロトタイプをインスタンス化することによって作成されます。
関数プロトタイプは C の関数プロトタイプと似ています。実装を定義せずに、
関数 ( 戻り値、引数の型、呼び出し規約 ) を記述します。ファクトリ関数は
関数に要求する戻り値の型と引数の型とともに呼び出されます。


.. function:: CFUNCTYPE(restype, *argtypes, use_errno=False, use_last_error=False)

   返された関数プロトタイプは標準 C 呼び出し規約をつかう関数を作成しま
   す。関数は呼び出されている間 GIL を解放します。
   *use_errno* が True に設定されれば、呼び出しの前後で System 変数
   :data:`errno` の ctypesプライベートコピーは本当の :data:`errno` の値と交換され
   ます。
   *use_last_error* も Windows エラーコードに対するのと同様です。

   .. versionchanged:: 2.6
      オプションの *use_errno* と *use_last_error* 変数が追加されました。


.. function:: WINFUNCTYPE(restype, *argtypes, use_errno=False, use_last_error=False)

   Windows 用: 返された関数プロトタイプは ``stdcall`` 呼び出し規約をつかう関数を作成します。
   ただし、 :func:`WINFUNCTYPE` が :func:`CFUNCTYPE` と同じである Windows CE を除きます。
   関数は呼び出されている間 GIL を解放します。
   *use_errno* と *use_last_error* は前述と同じ意味を持ちます。


.. function:: PYFUNCTYPE(restype, *argtypes)

   返された関数プロトタイプは Python 呼び出し規約をつかう関数を作成し
   ます。関数は呼び出されている間 GIL を解放 *しません* 。

ファクトリ関数によって作られた関数プロトタイプは呼び出しのパラメータの
型と数に依存した別の方法でインスタンス化することができます。 :


   .. function:: prototype(address)
      :noindex:
      :module:

      指定されたアドレス(整数でなくてはなりません)の外部関数を返します。


   .. function:: prototype(callable)
      :noindex:
      :module:

      Python の *callable* から C の呼び出し可能関数(コールバック関数)
      を作成します。


   .. function:: prototype(func_spec[, paramflags])
      :noindex:
      :module:

      共有ライブラリがエクスポートしている外部関数を返します。
      *func_spec* は 2 要素タプル ``(name_or_ordinal, library)`` でなけ
      ればなりません。第一要素はエクスポートされた関数の名前である文字列、
      またはエクスポートされた関数の序数である小さい整数です。第二要素は
      共有ライブラリインスタンスです。


   .. function:: prototype(vtbl_index, name[, paramflags[, iid]])
      :noindex:
      :module:

      COM メソッドを呼び出す外部関数を返します。 *vtbl_index* は仮想
      関数テーブルのインデックスで、非負の小さい整数です。
      *name* は COM メソッドの名前です。 *iid* はオプションのインター
      フェイス識別子へのポインタで、拡張されたエラー情報の提供のために
      使われます。

      COM メソッドは特殊な呼び出し規約を用います。このメソッドは
      :attr:`argtypes` タプルに指定されたパラメータに加えて、第一引数
      として COM インターフェイスへのポインタを必要とします。

   オプションの *paramflags* パラメータは上述した機能より多機能な外部
   関数ラッパーを作成します。

   *paramflags* は :attr:`argtypes` と同じ長さのタプルでなければなりま
   せん。

   このタプルの個々の要素はパラメータについてのより詳細な情報を持ち、
   1 、 2 もしくは 3 要素を含むタプルでなければなりません。

   第一要素はパラメータについてのフラグの組み合わせを含んだ整数です。


      1
         入力パラメータを関数に指定します。

      2
         出力パラメータ。外部関数が値を書き込みます。

      4
         デフォルトで整数ゼロになる入力パラメータ。

   オプションの第二要素はパラメータ名の文字列です。これが指定された場
   合は、外部関数を名前付きパラメータで呼び出すことができます。

   オプションの第三要素はこのパラメータのデフォルト値です。

この例では、デフォルトパラメータと名前付き引数をサポートするために
Windows ``MessageBoxA`` 関数をラップする方法を示します。
windowsヘッダファイルの C の宣言はこれです。::

   WINUSERAPI int WINAPI
   MessageBoxA(
       HWND hWnd ,
       LPCSTR lpText,
       LPCSTR lpCaption,
       UINT uType);

:mod:`ctypes` を使ってラップします。::

   >>> from ctypes import c_int, WINFUNCTYPE, windll
   >>> from ctypes.wintypes import HWND, LPCSTR, UINT
   >>> prototype = WINFUNCTYPE(c_int, HWND, LPCSTR, LPCSTR, UINT)
   >>> paramflags = (1, "hwnd", 0), (1, "text", "Hi"), (1, "caption", None), (1, "flags", 0)
   >>> MessageBox = prototype(("MessageBoxA", windll.user32), paramflags)
   >>>

今は MessageBox 外部関数をこのような方法で呼び出すことができます。::

   >>> MessageBox()
   >>> MessageBox(text="Spam, spam, spam")
   >>> MessageBox(flags=2, text="foo bar")
   >>>

二番目の例は出力パラメータについて説明します。 win32 の
``GetWindowRect`` 関数は、指定されたウィンドウの大きさを呼び出し側が与
える ``RECT`` 構造体へコピーすることで取り出します。 C の宣言はこうで
す。::

   WINUSERAPI BOOL WINAPI
   GetWindowRect(
        HWND hWnd,
        LPRECT lpRect);

:mod:`ctypes` を使ってラップします。::

   >>> from ctypes import POINTER, WINFUNCTYPE, windll, WinError
   >>> from ctypes.wintypes import BOOL, HWND, RECT
   >>> prototype = WINFUNCTYPE(BOOL, HWND, POINTER(RECT))
   >>> paramflags = (1, "hwnd"), (2, "lprect")
   >>> GetWindowRect = prototype(("GetWindowRect", windll.user32), paramflags)
   >>>

もし単一の値もしくは一つより多い場合には出力パラメータ値が入ったタプル
があるならば、出力パラメータを持つ関数は自動的に出力パラメータ値を返す
でしょう。
そのため、今は GetWindowRect 関数は呼び出されたときに RECT インスタン
スを返します。

さらに出力処理やエラーチェックを行うために、出力パラメータを
:attr:`errcheck` プロトコルと組み合わせることができます。 win32
``GetWindowRect`` api 関数は成功したか失敗したかを知らせるために
``BOOL`` を返します。そのため、この関数はエラーチェックを行って、
api 呼び出しが失敗した場合に例外を送出させることができます。::

   >>> def errcheck(result, func, args):
   ...     if not result:
   ...         raise WinError()
   ...     return args
   ...
   >>> GetWindowRect.errcheck = errcheck
   >>>

:attr:`errcheck` 関数が変更なしに受け取った引数タプルを返したならば、
:mod:`ctypes` は出力パラメータに対して通常の処理を続けます。
``RECT`` インスタンスの代わりに window 座標のタプルを返してほしいなら、
関数のフィールドを取り出し、代わりにそれらを返すことができます。
通常処理はもはや行われないでしょう。::

   >>> def errcheck(result, func, args):
   ...     if not result:
   ...         raise WinError()
   ...     rc = args[1]
   ...     return rc.left, rc.top, rc.bottom, rc.right
   ...
   >>> GetWindowRect.errcheck = errcheck
   >>>


.. _ctypes-utility-functions:

ユーティリティ関数
^^^^^^^^^^^^^^^^^^


.. function:: addressof(obj)

   メモリバッファのアドレスを示す整数を返します。 *obj* は ctypes 型
   のインスタンスでなければなりません。


.. function:: alignment(obj_or_type)

   ctypes 型のアライメントの必要条件を返します。 *obj_or_type* は
   ctypes 型またはインスタンスでなければなりません。


.. function:: byref(obj[, offset])

   *obj* (ctypes 型のインスタンスでなければならない) への軽量ポインタを
   返します。 *offset* はデフォルトでは 0 で、内部ポインターへ加算される
   整数です。

   ``byref(obj, offset)`` は、 C コードとしては、以下のようにみなされ
   ます。::

      (((char *)&obj) + offset)

   返されるオブジェクトは外部関数呼び出しのパラメータとしてのみ使用で
   きます。 ``pointer(obj)`` と似たふるまいをしますが、作成が非常に速
   く行えます。

   .. versionadded:: 2.6
      *offset* オプション引数が追加されました。


.. function:: cast(obj, type)

   この関数は C のキャスト演算子に似ています。 *obj* と同じメモリブ
   ロックを指している *type* の新しいインスタンスを返します。
   *type* はポインタ型でなければならず、 *obj* はポインタとして解
   釈できるオブジェクトでなければなりません。


.. function:: create_string_buffer(init_or_size[, size])

   この関数は変更可能な文字バッファを作成します。返されるオブジェクト
   は :class:`c_char` の ctypes 配列です。

   *init_or_size* は配列のサイズを指定する整数もしくは配列要素を初期
   化するために使われる文字列である必要があります。

   第一引数として文字列が指定された場合は、バッファが文字列の長さより
   一要素分大きく作られます。配列の最後の要素が NUL 終端文字であるため
   です。
   文字列の長さを使うべきでない場合は、配列のサイズを指定するために整
   数を第二引数として渡すことができます。

   第一引数がユニコード文字列ならば、 ctypes 変換規則にしたがい 8 ビッ
   ト文字列へ変換されます。


.. function:: create_unicode_buffer(init_or_size[, size])

   この関数は変更可能なユニコード文字バッファを作成します。返されるオ
   ブジェクトは :class:`c_wchar` の ctypes 配列です。

   *init_or_size* は配列のサイズを指定する整数もしくは配列要素を初期
   化するために使われるユニコード文字列です。

   第一引数としてユニコード文字列が指定された場合は、バッファが文字列
   の長さより一要素分大きく作られます。配列の最後の要素が NUL 終端文字
   であるためです。
   文字列の長さを使うべきでない場合は、配列のサイズを指定するために整
   数を第二引数として渡すことができます。

   第一引数が 8 ビット文字列ならば、 ctypes 変換規則にしたがいユニコー
   ド文字列へ変換されます。


.. function:: DllCanUnloadNow()

   Windows用: この関数は ctypes をつかってインプロセス COM サーバーを
   実装できるようにするためのフックです。 _ctypes 拡張 dll がエクスポー
   トしている DllCanUnloadNow 関数から呼び出されます。


.. function:: DllGetClassObject()

   Windows用: この関数は ctypes をつかってインプロセス COM サーバーを
   実装できるようにするためのフックです。 ``_ctypes`` 拡張 dll が
   エクスポートしている DllGetClassObject 関数から呼び出されます。


.. function:: find_library(name)
   :module: ctypes.util

   ライブラリを検索し、パス名を返します。
   *name* は ``lib`` のような接頭辞、
   ``.so`` や ``.dylib`` のような接尾辞、そして、バージョンナンバー
   を除くライブラリ名です (これは posix のリンカーオプション
   :option:`-l` で使われる書式です) 。もしライブラリが見つからなければ、
   ``None`` を返します。

   実際の機能はシステムに依存します。

   .. versionchanged:: 2.6
      Windows限定: ``find_library("m")`` もしくは ``find_library("c")``
      は ``find_msvcrt()`` の呼び出し結果を返します。

.. function:: find_msvcrt()
   :module: ctypes.util

   Windows用: Python と拡張モジュールで使われる VC ランタイプライブラ
   リのファイル名を返します。もしライブラリ名が同定できなければ、
   ``None`` を返します。

   もし、例えば拡張モジュールにより割り付けられたメモリを ``free(void
   *)`` で解放する必要があるなら、メモリ割り付けを行ったのと同じライブ
   ラリの関数を使うことが重要です。

   .. versionadded:: 2.6


.. function:: FormatError([code])

   Windows用: エラーコード *code* の説明文を返します。エラーコードが指定されな
   い場合は、 Windows api 関数 GetLastError を呼び出して、もっとも新し
   いエラーコードが使われます。


.. function:: GetLastError()

   Windows用: 呼び出し側のスレッド内で Windows によって設定された最新
   のエラーコードを返します。
   この関数はWindowsの `GetLastError()` 関数を直接実行します。
   ctypesのプライベートなエラーコードのコピーを返したりはしません。


.. function:: get_errno()

   システムの :data:`errno` 変数の、スレッドローカルなプライベートコピーを返します。

   .. versionadded:: 2.6

.. function:: get_last_error()

   Windowsのみ: システムの :data:`LastError` 変数の、スレッドローカルなプライベートコピーを返します。

   .. versionadded:: 2.6

.. function:: memmove(dst, src, count)

   標準 C の memmove ライブラリ関数と同じものです。: *count* バイトを
   *src* から *dst* へコピーします。 *dst* と *src* はポインタへ変
   換可能な整数または ctypes インスタンスでなければなりません。


.. function:: memset(dst, c, count)

   標準 C の memset ライブラリ関数と同じものです。: アドレス *dst* の
   メモリブロックを値 *c* を *count* バイト分書き込みます。
   *dst* はアドレスを指定する整数または ctypes インスタンスである必要
   があります。


.. function:: POINTER(type)

   このファクトリ関数は新しい ctypes ポインタ型を作成して返します。ポ
   インタ型はキャッシュされ、内部で再利用されます。したがって、この関
   数を繰り返し呼び出してもコストは小さいです。 *type* は ctypes 型でなけれ
   ばなりません。


.. function:: pointer(obj)

   この関数は *obj* を指す新しいポインタインスタンスを作成します。戻
   り値は ``POINTER(type(obj))`` 型のオブジェクトです。

   注意: 外部関数呼び出しへオブジェクトへのポインタを渡したいだけなら、
   はるかに高速な ``byref(obj)`` を使うべきです。


.. function:: resize(obj, size)

   この関数は *obj* の内部メモリバッファのサイズを変更します。 *obj*
   は ctypes 型のインスタンスでなければなりません。
   バッファを ``sizeof(type(obj))`` で与えられるオブジェクト型の本来の
   サイズより小さくすることはできませんが、バッファを拡大することはできます。


.. function:: set_conversion_mode(encoding, errors)

   この関数は 8 ビット文字列とユニコード文字列の間で変換するときに使わ
   れる規則を設定します。 *encoding* は ``'utf-8'`` や ``'mbcs'`` のよう
   なエンコーディングを指定する文字列でなければなりません。 *errors* は
   エンコーディング/デコーディングエラーについてのエラー処理を指定する
   文字列でなければなりません。指定可能な値の例としては、 ``"strict"``,
   ``"replace"``,  ``"ignore"`` があります。

   :func:`set_conversion_mode` は以前の変換規則を含む 2 要素タプルを返します。
   windows では初期の変換規則は ``('mbcs', 'ignore')`` であり、
   他のシステムでは ``('ascii', 'strict')`` です。


.. function:: set_errno(value)

   システム変数 `errno` の、呼び出し元スレッドでの ctypes のプライベー
   トコピーの現在値を `value` に設定し、前の値を返します。

   .. versionadded:: 2.6


.. function:: set_last_error(value)

   Windows用: システム変数 `LastError` の、呼び出し元スレッドでの
   ctypes のプライベートコピーの現在値を `value` に設定し、前の値を返
   します。

   .. versionadded:: 2.6


.. function:: sizeof(obj_or_type)

   ctypes 型もしくはインスタンスのメモリバッファのサイズをバイト単位で
   返します。 C の ``sizeof()`` 関数と同じ動作です。


.. function:: string_at(address[, size])

   この関数はメモリアドレス address から始まる文字列を返します。 size
   が指定された場合はサイズとして使われます。指定されなければ、文字列
   がゼロ終端されていると仮定します。


.. function:: WinError(code=None, descr=None)

   Windows用: この関数は ctypes の中でもおそらく最悪な名前がつけられたも
   のです。
   WindowsError のインスタンスを作成します。 *code* が指定されないなら
   ば、エラーコードを決めるために ``GetLastError`` が呼び出されます。
   *descr* が指定されないならば、 :func:`FormatError` がエラーの説明
   文を得るために呼び出されます。


.. function:: wstring_at(address[, size])

   この関数はユニコード文字列としてメモリアドレス *address* から始ま
   るワイドキャラクタ文字列を返します。 *size* が指定されたならば、
   文字列の文字数として使われます。指定されなければ、文字列がゼロ終端
   されていると仮定します。


.. _ctypes-data-types:

データ型
^^^^^^^^


.. class:: _CData

   この非公開クラスはすべての ctypes データ型の共通のベースクラスです。
   他のことはさておき、すべての ctypes 型インスタンスは C 互換データを
   保持するメモリブロックを内部に持ちます。このメモリブロック
   のアドレスは :func:`addressof` ヘルパー関数が返します。別のインスタ
   ンス変数が :attr:`_objects` として公開されます。これはメモリブロッ
   クがポインタを含む場合に存続し続ける必要のある他の Python オブジェ
   クトを含んでいます。

   ctypes データ型の共通メソッド、すべてのクラスメソッドが存在します
   (正確には、メタクラスのメソッドです):


   .. method:: _CData.from_buffer(source[, offset])

      このメソッドは *source* オブジェクトのバッファを共有する ctypes の
      インスタンスを返します。 *source* オブジェクトは書き込み可能バッ
      ファインターフェースをサポートしている必要があります。オプション
      の *offset* 引数では *source* バッファのオフセットをバイト単位で
      指定します。
      デフォルトではゼロです。もし *source* バッファが十分に大きくなけれ
      ば、 :exc:`ValueError` が送出されます。

      .. versionadded:: 2.6


   .. method:: _CData.from_buffer_copy(source[, offset])

      このメソッドは *source* オブジェクトの読み出し可能バッファをコピー
      することで、ctypes のインスタンスを生成します。オプションの
      *offset* 引数では *source* バッファのオフセットをバイト単位で指
      定します。
      デフォルトではゼロです。もし *source* バッファが十分に大きくなけれ
      ば、 :exc:`ValueError` が送出されます。

      .. versionadded:: 2.6


   .. method:: from_address(address)

      このメソッドは *address* で指定されたメモリを使って ctypes 型の
      インスタンスを返します。 *address* は整数でなければなりません。


   .. method:: from_param(obj)

      このメソッドは *obj* を ctypes 型に適合させます。外部関数の
      :attr:`argtypes` タプルに、その型があるとき、外部関数呼び出しで
      実際に使われるオブジェクトと共に呼び出されます。

      すべての ctypes のデータ型は、それが型のインスタンスであれば、
      *obj* を返すこのクラスメソッドのデフォルトの実装を持ちます。
      いくつかの型は、別のオブジェクトも受け付けます。


   .. method:: in_dll(library, name)

      このメソッドは、共有ライブラリによってエクスポートされた ctypes
      型のインスタンスを返します。
      *name* はエクスポートされたデータの名前で、 *library* はロードさ
      れた共有ライブラリです。


   ctypes データ型共通のインスタンス変数:


   .. attribute:: _b_base_

      ctypes 型データのインスタンスは、それ自身のメモリブロックを持たず、
      基底オブジェクトのメモリブロックの一部を共有することがあります。
      :attr:`_b_base_` 読み出し専用属性は、メモリブロックを保持する
      ctypes の基底オブジェクトです。


   .. attribute:: _b_needsfree_

      この読み出し専用の変数は、 ctypes データインスタンスが、それ自身
      に割り当てられたメモリブロックを持つとき true になります。それ以
      外の場合は false になります。


   .. attribute:: _objects

      このメンバは ``None`` 、または、メモリブロックの内容が正しく保
      つために、生存させておかなくてはならない Python オブジェクトを持
      つディクショナリです。このオブジェクトはデバッグでのみ使われま
      す。決してディクショナリの内容を変更しないで下さい。


.. _ctypes-fundamental-data-types-2:

基本データ型
^^^^^^^^^^^^


.. class:: _SimpleCData

   この非公開クラスはすべての基本 ctypes データ型のベースクラスです。
   ここでこのクラスに触れたのは、基本 ctypes データ型の共通属性を含ん
   でいるからです。
   :class:`_SimpleCData` は :class:`_CData` のサブクラスですので、
   そのメソッドと属性を継承しています。

   .. versionchanged:: 2.6
      ポインタと、ポインタを含まない ctypes データ型が pickle 化できる
      ようになりました。


   インスタンスは一つだけ属性を持ちます:

   .. attribute:: value

      この属性は、インスタンスの実際の値を持ちます。整数型とポインタ型
      に対しては整数型、文字型に対しては一文字の文字列、文字へのポイン
      タに対しては Python の文字列もしくはユニコード文字列となります。

      ``value`` 属性が ctypes インスタンスより参照されたとき、大抵の場
      合はそれぞれに対し新しいオブジェクトを返します。 :mod:`ctypes` はオ
      リジナルのオブジェクトを返す実装にはなって *おらず* 新しいオブジェ
      クトを構築します。同じことが他の ctypes オブジェクトインスタンス
      に対しても言えます。


基本データ型は、外部関数呼び出しの結果として返されたときや、例えば構造
体のフィールドメンバーや配列要素を取り出すときに、ネイティブの Python
型へ透過的に変換されます。言い換えると、外部関数が :class:`c_char_p`
の :attr:`restype` を持つ場合は、 :class:`c_char_p` インスタンスでは
*なく* 常に Python 文字列を受け取ることでしょう。

基本データ型のサブクラスはこの振る舞いを継承 *しません* 。したがって、
外部関数の :attr:`restype` が :class:`c_void_p` のサブクラスならば、関
数呼び出しからこのサブクラスのインスタンスを受け取ります。もちろん、
``value`` 属性にアクセスしてポインタの値を得ることができます。

これらが基本データ型です:

.. class:: c_byte

   C の :c:type:`signed char` データ型を表し、小整数として値を解釈します。
   コンストラクタはオプションの整数初期化子を受け取ります。
   オーバーフローのチェックは行われません。


.. class:: c_char

   C :c:type:`char` データ型を表し、単一の文字として値を解釈します。
   コンストラクタはオプションの文字列初期化子を受け取り、その文字列の長さちょうど
   一文字である必要があります。


.. class:: c_char_p

   C :c:type:`char *` データ型を表し、ゼロ終端文字列へのポインタ
   でなければなりません。バイナリデータを指す可能性のある一般的な
   ポインタに対しては ``POINTER(c_char)`` を使わなければなりません。
   コンストラクタは整数のアドレスもしくは文字列を受け取ります。


.. class:: c_double

   C :c:type:`double` データ型を表します。コンストラクタはオプションの浮動小数点
   数初期化子を受け取ります。


.. class:: c_longdouble

   C :c:type:`long double` データ型を表します。コンストラクタはオプションで浮動
   小数点数初期化子を受け取ります。 ``sizeof(long double) ==
   sizeof(double)`` であるプラットホームでは :class:`c_double` の別名
   です。

   .. versionadded:: 2.6


.. class:: c_float

   C :c:type:`float` データ型を表します。コンストラクタはオプションの浮動小数点
   数初期化子を受け取ります。


.. class:: c_int

   C :c:type:`signed int` データ型を表します。コンストラクタはオプションの整数初
   期化子を受け取ります。オーバーフローのチェックは行われません。
   ``sizeof(int) == sizeof(long)`` であるプラットホームでは、
   :class:`c_long` の別名です。


.. class:: c_int8

   C 8-bit :c:type:`signed int` データ型を表します。たいていは、
   :class:`c_byte` の別名です。


.. class:: c_int16

   C 16-bit :c:type:`signed int` データ型を表します。たいていは、
   :class:`c_short` の別名です。


.. class:: c_int32

   C 32-bit :c:type:`signed int` データ型を表します。たいていは、
   :class:`c_int` の別名です。


.. class:: c_int64

   C 64-bit :c:type:`signed int` データ型を表します。たいていは、
   :class:`c_longlong` の別名です。


.. class:: c_long

   C :c:type:`signed long` データ型を表します。コンストラクタはオプションの
   整数初期化子を受け取ります。オーバーフローのチェックは行われません。


.. class:: c_longlong

   C :c:type:`signed long long` データ型を表します。コンストラクタはオプションの
   整数初期化子を受け取ります。オーバーフローのチェックは行われません。


.. class:: c_short

   C :c:type:`signed short` データ型を表します。コンストラクタはオプションの
   整数初期化子を受け取ります。オーバーフローのチェックは行われません。


.. class:: c_size_t

   C :c:type:`size_t` データ型を表します。


.. class:: c_ssize_t

   C :c:type:`ssize_t` データ型を表します。

   .. versionadded:: 2.7


.. class:: c_ubyte

   C :c:type:`unsigned char` データ型を表します。その値は小整数として解釈さ
   れます。コンストラクタはオプションの整数初期化子を受け取ります。オー
   バーフローのチェックは行われません。


.. class:: c_uint

   C :c:type:`unsigned int` データ型を表します。コンストラクタはオプションの
   整数初期化子を受け取ります。オーバーフローのチェックは行われません。
   ``sizeof(int) == sizeof(long)`` であるプラットホームでは、
   :class:`c_ulong` の別名です。


.. class:: c_uint8

   C 8-bit :c:type:`unsigned int` データ型を表します。たいていは、
   :class:`c_ubyte` の別名です。


.. class:: c_uint16

   C 16-bit :c:type:`unsigned int` データ型を表します。たいていは、
   :class:`c_ushort` の別名です。


.. class:: c_uint32

   C 32-bit :c:type:`unsigned int` データ型を表します。たいていは、
   :class:`c_uint` の別名です。


.. class:: c_uint64

   C 64-bit :c:type:`unsigned int` データ型を表します。たいていは、
   :class:`c_ulonglong` の別名です。


.. class:: c_ulong

   C :c:type:`unsigned long` データ型を表します。コンストラクタはオプション
   の整数初期化子を受け取ります。オーバーフローのチェックは行われませ
   ん。


.. class:: c_ulonglong

   C :c:type:`unsigned long long` データ型を表します。コンストラクタはオプショ
   ンの整数初期化子を受け取ります。オーバーフローのチェックは行われま
   せん。


.. class:: c_ushort

   C :c:type:`unsigned short` データ型を表します。コンストラクタはオプション
   の整数初期化子を受け取ります。オーバーフローのチェックは行われませ
   ん。


.. class:: c_void_p

   C :c:type:`void *` データ型を表します。値は整数として表されます。コンスト
   ラクタはオプションの整数初期化子を受け取ります。


.. class:: c_wchar

   C :c:type:`wchar_t` データ型を表し、値はユニコード文字列の単一の文字とし
   て解釈されます。コンストラクタはオプションの文字列初期化子を受け取
   り、その文字列の長さはちょうど一文字である必要があります。


.. class:: c_wchar_p

   C :c:type:`wchar_t *` データ型を表し、ゼロ終端ワイド文字列へのポインタで
   なければなりません。コンストラクタは整数のアドレスもしくは文字列を
   受け取ります。


.. class:: c_bool

   C :c:type:`bool` データ型 (より正確には、C99 の :c:type:`_Bool`) を表します。そ
   の値は True または False であり、コンストラクタはどんなオブジェクト
   ( 真値を持ちます ) でも受け取ります。

   .. versionadded:: 2.6


.. class:: HRESULT

   Windows用: :c:type:`HRESULT` 値を表し、関数またはメソッド呼び出しに
   対する成功またはエラーの情報を含んでいます。


.. class:: py_object

   C :c:type:`PyObject *` データ型を表します。引数なしでこれを呼び出すと
   ``NULL`` :c:type:`PyObject *` ポインタを作成します。

:mod:`ctypes.wintypes` モジュールは他の Windows 固有のデータ型を提供します。
例えば、 :c:type:`HWND`, :c:type:`WPARAM`, :c:type:`DWORD` です。
:c:type:`MSG` や :c:type:`RECT` のような有用な構造体も定義されています。


.. _ctypes-structured-data-types:

標準データ型
^^^^^^^^^^^^


.. class:: Union(*args, **kw)

   ネイティブのバイトオーダーでの共用体のための抽象ベースクラス。


.. class:: BigEndianStructure(*args, **kw)

   *ビックエンディアン* バイトオーダーでの構造体のための抽象ベースクラス。


.. class:: LittleEndianStructure(*args, **kw)

   *リトルエンディアン* バイトオーダーでの構造体のための抽象ベースクラス。

ネイティブではないバイトオーダーを持つ構造体にポインタ型フィールドある
いはポインタ型フィールドを含む他のどんなデータ型をも入れることはでき
ません。


.. class:: Structure(*args, **kw)

   *ネイティブ* のバイトオーダーでの構造体のための抽象ベースクラス。

具象構造体型と具象共用体型はこれらの型の一つをサブクラス化することで作
らなければなりません。少なくとも、 :attr:`_fields_` クラス変数を定義す
る必要があります。 :mod:`ctypes` は、属性に直接アクセスしてフィールドを読
み書きできるようにする記述子 ( :term:`descriptor` ) を作成するでしょう。
これらは、


   .. attribute:: _fields_

      構造体のフィールドを定義するシーケンス。要素は2要素タプルか3要素
      タプルでなければなりません。第一要素はフィールドの名前です。第
      二要素はフィールドの型を指定します。それはどんな ctypes データ型で
      も構いません。

      :class:`c_int` のような整数型のために、オプションの第三要素を与
      えることができます。フィールドのビット幅を定義する正の小整数であ
      る必要があります。

      一つの構造体と共用体の中で、フィールド名はただ一つである必要があ
      ります。これはチェックされません。名前が繰り返しでてきたときにア
      クセスできるのは一つのフィールドだけです。

      Structure サブクラスを定義するクラス文の *後で* 、
      :attr:`_fields_` クラス変数を定義することができます。
      これにより自身を直接または間接的に参照するデータ型を作成できるよ
      うになります。::

         class List(Structure):
             pass
         List._fields_ = [("pnext", POINTER(List)),
                          ...
                         ]

      しかし、 :attr:`_fields_` クラス変数はその型が最初に使われる (
      インスタンスが作成される、それに対して ``sizeof()`` が呼び出される
      など ) より前に定義されていなければなりません。その後
      :attr:`_fields_` クラス変数へ代入すると AttributeError が送出されます。

      構造体および共用体サブクラスは位置引数と名前付き引数の両方を受け取
      ります。位置引数は :attr:`_fields_` 定義中に現れたのと同じ順番で
      フィールドを初期化するために使われ、名前付き引数は対応する名前を
      使ってフィールドを初期化するために使われます。

      構造体型のサブクラスを定義することができ、もしあるならサブクラス
      内で定義された :attr:`_fields_` に加えて、ベースクラスのフィールドも
      継承します。


   .. attribute:: _pack_

      インスタンスの構造体フィールドのアライメントを上書きできるように
      するオブションの小整数。 :attr:`_pack_` は :attr:`_fields_` が
      代入されたときすでに定義されていなければなりません。そうでなけれ
      ば、何ら影響はありません。


   .. attribute:: _anonymous_

      無名 (匿名) フィールドの名前が並べあげられたオプションのシーケ
      ンス。 :attr:`_fields_` が代入されたとき、 :attr:`_anonymous_` がすでに
      定義されていなければなりません。そうでなければ、何ら影響はありません。

      この変数に並べあげられたフィールドは構造体型もしくは共用体型フィー
      ルドである必要があります。構造体フィールドまたは共用体フィールド
      を作る必要なく、入れ子になったフィールドに直接アクセスできるよう
      にするために、 :mod:`ctypes` は構造体型の中に記述子を作成します。

      型の例です(Windows)::

         class _U(Union):
             _fields_ = [("lptdesc", POINTER(TYPEDESC)),
                         ("lpadesc", POINTER(ARRAYDESC)),
                         ("hreftype", HREFTYPE)]

         class TYPEDESC(Structure):
             _anonymous_ = ("u",)
             _fields_ = [("u", _U),
                         ("vt", VARTYPE)]


      ``TYPEDESC`` 構造体はCOMデータ型を表現しており、 ``vt`` フィール
      ドは共用体フィールドのどれが有効であるかを指定します。 ``u`` フィー
      ルドは匿名フィールドとして定義されているため、 TYPEDESC インスタ
      ンスから取り除かれてそのメンバーへ直接アクセスできます。
      ``td.lptdesc`` と ``td.u.lptdesc`` は同等ですが、前者がより高速
      です。なぜなら一時的な共用体インスタンスを作る必要がないためで
      す。::

         td = TYPEDESC()
         td.vt = VT_PTR
         td.lptdesc = POINTER(some_type)
         td.u.lptdesc = POINTER(some_type)

   構造体のサブ-サブクラスを定義することができ、ベースクラスのフィールド
   を継承します。サブクラス定義に別の :attr:`_fields_` 変数がある場合は、
   この中で指定されたフィールドはベースクラスのフィールドへ追加されます。

   構造体と共用体のコンストラクタは位置引数とキーワード引数の両方を受け取ります。
   位置引数は :attr:`_fields_` の中に現れたのと同じ順番でメンバーフィールドを
   初期化するために使われます。コンストラクタのキーワード引数は属性代入と
   して解釈され、そのため、同じ名前をもつ :attr:`_fields_` を初期化するか、
   :attr:`_fields_` に存在しない名前に対しては新しい属性を作ります。


.. _ctypes-arrays-pointers:

配列とポインタ
^^^^^^^^^^^^^^

未作成 - チュートリアルの節 :ref:`ctypes-pointers` と
:ref:`ctypes-arrays` を参照してください。

