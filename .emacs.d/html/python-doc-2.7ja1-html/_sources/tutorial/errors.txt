.. _tut-errors:

************
エラーと例外
************

.. Until now error messages haven't been more than mentioned, but if you have tried
   out the examples you have probably seen some.  There are (at least) two
   distinguishable kinds of errors: *syntax errors* and *exceptions*.

これまでエラーメッセージについては簡単に触れるだけでしたが、チュートリアル中の
例を自分で試していたら、実際にいくつかのエラーメッセージを見ていることでしょう。
エラーには (少なくとも) 二つのはっきり異なる種類があります。
それは *構文エラー (syntax error)* と *例外 (exception)* です。


.. _tut-syntaxerrors:

構文エラー
==========

.. Syntax errors, also known as parsing errors, are perhaps the most common kind of
   complaint you get while you are still learning Python:

構文エラーは構文解析エラー (parsing error) としても知られており、 Python を勉強している間に
最もよく遭遇する問題の一つでしょう。

::

   >>> while True print 'Hello world'
     File "<stdin>", line 1, in ?
       while True print 'Hello world'
                      ^
   SyntaxError: invalid syntax


.. The parser repeats the offending line and displays a little 'arrow' pointing at
   the earliest point in the line where the error was detected.  The error is
   caused by (or at least detected at) the token *preceding* the arrow: in the
   example, the error is detected at the keyword :keyword:`print`, since a colon
   (``':'``) is missing before it.  File name and line number are printed so you
   know where to look in case the input came from a script.

パーサは違反の起きている行を表示し、小さな「矢印」を表示して、行中でエラーが
検出された最初の位置を示します。
エラーは矢印の *直前の* トークンでひき起こされています (または、少なくともそこで検出されています)。
上記の例では、エラーは :keyword:`print` で検出されています。
コロン (``':'``) がその前に無いからです。
入力がスクリプトから来ている場合は、どこを見ればよいか分かるように
ファイル名と行番号が出力されます。



.. _tut-exceptions:

例外
====

.. Even if a statement or expression is syntactically correct, it may cause an
   error when an attempt is made to execute it. Errors detected during execution
   are called *exceptions* and are not unconditionally fatal: you will soon learn
   how to handle them in Python programs.  Most exceptions are not handled by
   programs, however, and result in error messages as shown here:

たとえ文や式が構文的に正しくても、実行しようとしたときにエラーが発生するかもしれません。
実行中に検出されたエラーは *例外 (exception)* と呼ばれ、常に致命的とは限りません。
これから、 Python プログラムで例外をどのように扱うかを学んでいきます。
ほとんどの例外はプログラムで処理されず、以下に示されるようなメッセージになります。

::

   >>> 10 * (1/0)
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
   ZeroDivisionError: integer division or modulo by zero
   >>> 4 + spam*3
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
   NameError: name 'spam' is not defined
   >>> '2' + 2
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
   TypeError: cannot concatenate 'str' and 'int' objects


.. The last line of the error message indicates what happened. Exceptions come in
   different types, and the type is printed as part of the message: the types in
   the example are :exc:`ZeroDivisionError`, :exc:`NameError` and :exc:`TypeError`.
   The string printed as the exception type is the name of the built-in exception
   that occurred.  This is true for all built-in exceptions, but need not be true
   for user-defined exceptions (although it is a useful convention). Standard
   exception names are built-in identifiers (not reserved keywords).

エラーメッセージの最終行は何が起こったかを示しています。
例外は様々な型 (type) で起こり、その型がエラーメッセージの一部として出力されます。
上の例での型は :exc:`ZeroDivisionError`, :exc:`NameError`, :exc:`TypeError` です。
例外型として出力される文字列は、発生した例外の組み込み名です。
これは全ての組み込み例外について成り立ちますが、ユーザ定義の例外では
(成り立つようにするのは有意義な慣習ですが) 必ずしも成り立ちません。
標準例外の名前は組み込みの識別子です (予約語ではありません)。


.. The rest of the line provides detail based on the type of exception and what
   caused it.

残りの行は例外の詳細で、その例外の型と何が起きたかに依存します。


.. The preceding part of the error message shows the context where the exception
   happened, in the form of a stack traceback. In general it contains a stack
   traceback listing source lines; however, it will not display lines read from
   standard input.

エラーメッセージの先頭部分では、例外が発生した実行コンテキスト (context) を、
スタックのトレースバック (stack traceback) の形式で示しています。
一般には、この部分にはソースコード行をリストしたトレースバックが表示されます。
しかし、標準入力から読み取られたコードは表示されません。

.. :ref:`bltin-exceptions` lists the built-in exceptions and their meanings.

:ref:`bltin-exceptions` には、組み込み例外とその意味がリストされています。


.. _tut-handling:

例外を処理する
==============

.. It is possible to write programs that handle selected exceptions. Look at the
   following example, which asks the user for input until a valid integer has been
   entered, but allows the user to interrupt the program (using :kbd:`Control-C` or
   whatever the operating system supports); note that a user-generated interruption
   is signalled by raising the :exc:`KeyboardInterrupt` exception.

例外を選別して処理するようなプログラムを書くことができます。
以下の例を見てください。この例では、有効な文字列が入力されるまで
ユーザに入力を促しますが、ユーザがプログラムに (:kbd:`Control-C` か、
またはオペレーティングシステムがサポートしている何らかのキーを使って)
割り込みをかけてプログラムを中断させることができるようにしています。
ユーザが生成した割り込みは、 :exc:`KeyboardInterrupt`
例外が送出されることで通知されるということに注意してください。

::

   >>> while True:
   ...     try:
   ...         x = int(raw_input("Please enter a number: "))
   ...         break
   ...     except ValueError:
   ...         print "Oops!  That was no valid number.  Try again..."
   ...


.. The :keyword:`try` statement works as follows.

:keyword:`try` 文は下記のように動作します。


.. * First, the *try clause* (the statement(s) between the :keyword:`try` and
     :keyword:`except` keywords) is executed.

* まず、 *try 節 (try clause)* (キーワード :keyword:`try` と :keyword:`except` の間の文)
  が実行されます。


.. * If no exception occurs, the *except clause* is skipped and execution of the
     :keyword:`try` statement is finished.

* 何も例外が発生しなければ、 *except 節* をスキップして :keyword:`try` 文の実行を終えます。


.. * If an exception occurs during execution of the try clause, the rest of the
     clause is skipped.  Then if its type matches the exception named after the
     :keyword:`except` keyword, the except clause is executed, and then execution
     continues after the :keyword:`try` statement.

* try 節内の実行中に例外が発生すると、その節の残りは飛ばされます。
  次に、例外型が :keyword:`except` キーワードの後に指定されている
  例外に一致する場合、except 節が実行された後、 :keyword:`try` 文の後ろへ実行が継続されます。


.. * If an exception occurs which does not match the exception named in the except
     clause, it is passed on to outer :keyword:`try` statements; if no handler is
     found, it is an *unhandled exception* and execution stops with a message as
     shown above.

* もしも except 節で指定された例外と一致しない例外が発生すると、その例外は
  :keyword:`try` 文の外側に渡されます。例外に対するハンドラ (handler、処理部)
  がどこにもなければ、 *処理されない例外 (unhandled exception)* となり、
  上記に示したようなメッセージを出して実行を停止します。


.. A :keyword:`try` statement may have more than one except clause, to specify
   handlers for different exceptions.  At most one handler will be executed.
   Handlers only handle exceptions that occur in the corresponding try clause, not
   in other handlers of the same :keyword:`try` statement.  An except clause may
   name multiple exceptions as a parenthesized tuple, for example:

一つの :keyword:`try` 文に複数の except 節を設けて、さまざまな例外に対する
ハンドラを指定することができます。
同時に一つ以上のハンドラが実行されることはありません。
ハンドラは対応する try 節内で発生した例外だけを処理し、同じ try 節内の
別の例外ハンドラで起きた例外は処理しません。
except 節には複数の例外を丸括弧で囲ったタプルにして渡すことができます。
例えば以下のようにします。

::

   ... except (RuntimeError, TypeError, NameError):
   ...     pass


.. The last except clause may omit the exception name(s), to serve as a wildcard.
   Use this with extreme caution, since it is easy to mask a real programming error
   in this way!  It can also be used to print an error message and then re-raise
   the exception (allowing a caller to handle the exception as well):

最後の except 節では例外名を省いて、ワイルドカード (wildcard、総称記号)
にすることができます。
ワイルドカードの except 節は非常に注意して使ってください。
というのは、ワイルドカードは通常のプログラムエラーをたやすく隠してしまうからです！
ワイルドカードの except 節はエラーメッセージを出力した後に例外を再送出する
(関数やメソッドの呼び出し側が同様にして例外を処理できるようにする) 用途にも使えます。

::

   import sys

   try:
       f = open('myfile.txt')
       s = f.readline()
       i = int(s.strip())
   except IOError as (errno, strerror):
       print "I/O error({0}): {1}".format(errno, strerror)
   except ValueError:
       print "Could not convert data to an integer."
   except:
       print "Unexpected error:", sys.exc_info()[0]
       raise


.. The :keyword:`try` ... :keyword:`except` statement has an optional *else
   clause*, which, when present, must follow all except clauses.  It is useful for
   code that must be executed if the try clause does not raise an exception.  For
   example:

:keyword:`try` ... :keyword:`except` 文には、オプションで *else 節 (else clause)*
を設けることができます。
:keyword:`else` 節を設ける場合、全ての :keyword:`except` 節よりも後ろに置かねばなりません。
:keyword:`else` 節は try 節で全く例外が送出されなかったときに実行されるコードを書くのに役立ちます。
例えば次のようにします。

::

   for arg in sys.argv[1:]:
       try:
           f = open(arg, 'r')
       except IOError:
           print 'cannot open', arg
       else:
           print arg, 'has', len(f.readlines()), 'lines'
           f.close()


.. The use of the :keyword:`else` clause is better than adding additional code to
   the :keyword:`try` clause because it avoids accidentally catching an exception
   that wasn't raised by the code being protected by the :keyword:`try`   .
   :keyword:`except` statement.

追加のコードを追加するのは :keyword:`try` 節の後ろよりも :keyword:`else`
節の方がよいでしょう。なぜなら、そうすることで :keyword:`try` ...
:keyword:`except` 文で保護したいコードから送出されたもの以外の例外を
偶然に捕捉してしまうという事態を避けられるからです。

.. When an exception occurs, it may have an associated value, also known as the
   exception's *argument*. The presence and type of the argument depend on the
   exception type.

例外が発生するとき、例外は関連付けられた値を持つことができます。
この値は例外の *引数 (argument)* とも呼ばれます。
引数の有無および引数の型は、例外の型に依存します。


.. The except clause may specify a variable after the exception name (or tuple).
   The variable is bound to an exception instance with the arguments stored in
   ``instance.args``.  For convenience, the exception instance defines
   :meth:`__str__` so the arguments can be printed directly without having to
   reference ``.args``.

except 節では、例外名 (または例外名タプル) の後に変数を指定することができます。
この変数は例外インスタンスに結び付けられており、 ``instance.args`` に
例外インスタンス生成時の引数が入っています。
例外インスタンスには :meth:`__str__` が定義されており、 ``.args``
を参照しなくても引数を直接印字できるように利便性が図られています。

.. One may also instantiate an exception first before raising it and add any
   attributes to it as desired.

必要なら、例外を送出する前にインスタンス化して、任意の属性を追加できます。

::

   >>> try:
   ...    raise Exception('spam', 'eggs')
   ... except Exception as inst:
   ...    print type(inst)     # 例外インスタンス
   ...    print inst.args      # .args に記憶されている引数
   ...    print inst           # __str__ により引数を直接出力できる
   ...    x, y = inst          # __getitem__ により引数を直接アンパックできる
   ...    print 'x =', x
   ...    print 'y =', y
   ...
   <type 'exceptions.Exception'>
   ('spam', 'eggs')
   ('spam', 'eggs')
   x = spam
   y = eggs


.. If an exception has an argument, it is printed as the last part ('detail') of
   the message for unhandled exceptions.

例外が引数を持っていれば、それは処理されない例外のメッセージの最後の部分
(「詳細説明」) に出力されます。


.. Exception handlers don't just handle exceptions if they occur immediately in the
   try clause, but also if they occur inside functions that are called (even
   indirectly) in the try clause. For example:

例外ハンドラは、 try 節の直接内側で発生した例外を処理するだけではなく、
その try 節から (たとえ間接的にでも) 呼び出された関数の内部で発生した
例外も処理します。例えば

::

   >>> def this_fails():
   ...     x = 1/0
   ...
   >>> try:
   ...     this_fails()
   ... except ZeroDivisionError, detail:
   ...     print 'Handling run-time error:', detail
   ...
   Handling run-time error: integer division or modulo by zero



.. _tut-raising:

例外を送出する
==============

.. The :keyword:`raise` statement allows the programmer to force a specified
   exception to occur. For example:

:keyword:`raise` 文を使って、特定の例外を発生させることができます。例えば、

::

   >>> raise NameError('HiThere')
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
   NameError: HiThere


:keyword:`raise` は送出したい例外を引数として取ります。
これは例外クラス(:class:`Exception` を継承したクラス)か、
例外クラスのインスタンスです。

.. If you need to determine whether an exception was raised but don't intend to
   handle it, a simpler form of the :keyword:`raise` statement allows you to
   re-raise the exception:

例外が発生したかどうかを判定したいだけで、その例外を処理するつもりがなければ、
単純な形式の :keyword:`raise` 文を使って例外を再送出させることができます。

::

   >>> try:
   ...     raise NameError('HiThere')
   ... except NameError:
   ...     print 'An exception flew by!'
   ...     raise
   ...
   An exception flew by!
   Traceback (most recent call last):
     File "<stdin>", line 2, in ?
   NameError: HiThere


.. _tut-userexceptions:

ユーザ定義の例外
================

.. Programs may name their own exceptions by creating a new exception class (see
   :ref:`tut-classes` for more about Python classes).  Exceptions should typically
   be derived from the :exc:`Exception` class, either directly or indirectly.  For
   example:

プログラム上で新しい例外クラスを作成することで、独自の例外を指定することができます
(Python のクラスについては :ref:`tut-classes` 参照)。
例外は、典型的に :exc:`Exception` クラスから、直接または間接的に派生したものです。
例を示します。

::

   >>> class MyError(Exception):
   ...     def __init__(self, value):
   ...         self.value = value
   ...     def __str__(self):
   ...         return repr(self.value)
   ...
   >>> try:
   ...     raise MyError(2*2)
   ... except MyError as e:
   ...     print 'My exception occurred, value:', e.value
   ...
   My exception occurred, value: 4
   >>> raise MyError('oops!')
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
   __main__.MyError: 'oops!'


.. In this example, the default :meth:`__init__` of :class:`Exception` has been
   overridden.  The new behavior simply creates the *value* attribute.  This
   replaces the default behavior of creating the *args* attribute.

この例では :class:`Exception` のデフォルト :meth:`__init__` が
オーバーライドされています。
新しい振る舞いでは、単に *value* 属性を作ります。
これは、デフォルトの *args* 属性を作成する振る舞いを置き換えています。

.. Exception classes can be defined which do anything any other class can do, but
   are usually kept simple, often only offering a number of attributes that allow
   information about the error to be extracted by handlers for the exception.  When
   creating a module that can raise several distinct errors, a common practice is
   to create a base class for exceptions defined by that module, and subclass that
   to create specific exception classes for different error conditions:

例外クラスでは、普通のクラスができることなら何でも定義することができますが、
通常は単純なものにしておきます。
大抵は、いくつかの属性だけを提供し、例外が発生したときにハンドラが
エラーに関する情報を取り出せるようにする程度にとどめます。
複数の別個の例外を送出するようなモジュールを作成する際には、そのモジュールで
定義されている例外の基底クラスを作成するのが一般的なプラクティスです。

::

   class Error(Exception):
       """Base class for exceptions in this module."""
       pass

   class InputError(Error):
       """Exception raised for errors in the input.

       Attributes:
           expr -- input expression in which the error occurred
           msg  -- explanation of the error
       """

       def __init__(self, expr, msg):
           self.expr = expr
           self.msg = msg

   class TransitionError(Error):
       """Raised when an operation attempts a state transition that's not
       allowed.

       Attributes:
           prev -- state at beginning of transition
           next -- attempted new state
           msg  -- explanation of why the specific transition is not allowed
       """

       def __init__(self, prev, next, msg):
           self.prev = prev
           self.next = next
           self.msg = msg


.. Most exceptions are defined with names that end in "Error," similar to the
   naming of the standard exceptions.

ほとんどの例外は、標準の例外の名前付けと同様に、 "Error" で終わる名前で
定義されています。


.. Many standard modules define their own exceptions to report errors that may
   occur in functions they define.  More information on classes is presented in
   chapter :ref:`tut-classes`.

多くの標準モジュールでは、モジュールで定義されている関数内で発生する
可能性のあるエラーを報告させるために、独自の例外を定義しています。
クラスについての詳細な情報は :ref:`tut-classes` 章で提供されています。


.. _tut-cleanup:

クリーンアップ動作を定義する
============================

.. The :keyword:`try` statement has another optional clause which is intended to
   define clean-up actions that must be executed under all circumstances.  For
   example:

:keyword:`try` 文にはもう一つオプションの節があります。
この節はクリーンアップ動作を定義するためのもので、どんな状況でも必ず実行されます。
例を示します。

::

   >>> try:
   ...     raise KeyboardInterrupt
   ... finally:
   ...     print 'Goodbye, world!'
   ...
   Goodbye, world!
   Traceback (most recent call last):
     File "<stdin>", line 2, in ?
   KeyboardInterrupt


.. A *finally clause* is always executed before leaving the :keyword:`try`
   statement, whether an exception has occurred or not. When an exception has
   occurred in the :keyword:`try` clause and has not been handled by an
   :keyword:`except` clause (or it has occurred in a :keyword:`except` or
   :keyword:`else` clause), it is re-raised after the :keyword:`finally` clause has
   been executed.  The :keyword:`finally` clause is also executed "on the way out"
   when any other clause of the :keyword:`try` statement is left via a
   :keyword:`break`, :keyword:`continue` or :keyword:`return` statement.  A more
   complicated example (having :keyword:`except` and :keyword:`finally` clauses in
   the same :keyword:`try` statement works as of Python 2.5):

*finally 節 (finally clause)* は、例外が発生したかどうかに関わらず、
:keyword:`try` 文を抜ける前に常に実行されます。
:keyword:`try` 節の中で例外が発生して、 :keyword:`except` 節で
処理されていない場合、または :keyword:`except` 節か :keyword:`else`
節で例外が発生した場合は、 :keyword:`finally` 節を実行した後、
その例外を再送出します。
:keyword:`finally` 節はまた、 :keyword:`try` 節から :keyword:`break`
文や  :keyword:`continue` 文、 :keyword:`return` 文経由で抜ける際にも、
"抜ける途中で" 実行されます。
より複雑な例です (:keyword:`except` 節や :keyword:`finally` 節が同じ
:keyword:`try` 文の中にあって、 Python 2.5 以降で動作します)。

::

   >>> def divide(x, y):
   ...     try:
   ...         result = x / y
   ...     except ZeroDivisionError:
   ...         print "division by zero!"
   ...     else:
   ...         print "result is", result
   ...     finally:
   ...         print "executing finally clause"
   ...
   >>> divide(2, 1)
   result is 2
   executing finally clause
   >>> divide(2, 0)
   division by zero!
   executing finally clause
   >>> divide("2", "1")
   executing finally clause
   Traceback (most recent call last):
     File "<stdin>", line 1, in ?
     File "<stdin>", line 3, in divide
   TypeError: unsupported operand type(s) for /: 'str' and 'str'


.. As you can see, the :keyword:`finally` clause is executed in any event.  The
   :exc:`TypeError` raised by dividing two strings is not handled by the
   :keyword:`except` clause and therefore re-raised after the :keyword:`finally`
   clause has been executed.

見てわかるとおり、 :keyword:`finally` 節はどの場合にも実行されています。
文字列を割り算することで発生した :exc:`TypeError` は :keyword:`except` 節で
処理されていないので、 :keyword:`finally` 節実行後に再度送出されています。


.. In real world applications, the :keyword:`finally` clause is useful for
   releasing external resources (such as files or network connections), regardless
   of whether the use of the resource was successful.

実世界のアプリケーションでは、 :keyword:`finally` 節は(ファイルや
ネットワーク接続などの)外部リソースを、利用が成功したかどうかにかかわらず
解放するために便利です。


.. _tut-cleanup-with:

定義済みクリーンアップ処理
==========================

.. Some objects define standard clean-up actions to be undertaken when the object
   is no longer needed, regardless of whether or not the operation using the object
   succeeded or failed. Look at the following example, which tries to open a file
   and print its contents to the screen.

オブジェクトのなかには、その利用の成否にかかわらず、不要になった際に実行される
標準的なクリーンアップ処理が定義されているものがあります。
以下の、ファイルをオープンして内容を画面に表示する例をみてください。

::

   for line in open("myfile.txt"):
       print line

.. The problem with this code is that it leaves the file open for an indeterminate
   amount of time after the code has finished executing. This is not an issue in
   simple scripts, but can be a problem for larger applications. The
   :keyword:`with` statement allows objects like files to be used in a way that
   ensures they are always cleaned up promptly and correctly.

このコードの問題点は、コードが実行された後に不定の時間ファイルを
開いたままでいることです。
これは単純なスクリプトでは問題になりませんが、大きなアプリケーションでは
問題になりえます。
:keyword:`with` 文はファイルのようなオブジェクトが常に、即座に正しく
クリーンアップされることを保証します。

::

   with open("myfile.txt") as f:
       for line in f:
           print line


.. After the statement is executed, the file *f* is always closed, even if a
   problem was encountered while processing the lines. Other objects which provide
   predefined clean-up actions will indicate this in their documentation.

この文が実行されたあとで、たとえ行の処理中に問題があったとしても、
ファイル *f* は常に close されます。
他の定義済みクリーンアップ処理を持つオブジェクトについては、
それぞれのドキュメントで示されます。
