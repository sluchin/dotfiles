:mod:`warnings` --- 警告の制御
==============================

.. index:: single: warnings

.. module:: warnings
   :synopsis: 警告メッセージを送出したり、その処理方法を制御したりします。


.. versionadded:: 2.1


.. Warning messages are typically issued in situations where it is useful to alert
.. the user of some condition in a program, where that condition (normally) doesn't
.. warrant raising an exception and terminating the program.  For example, one
.. might want to issue a warning when a program uses an obsolete module.

警告メッセージは一般に、ユーザに警告しておいた方がよいような状況下にプログラムが置かれているが、その状況は (通常は) 例外を送出したり
そのプログラムを終了させるほどの正当な理由がないといった状況で発されます。例えば、プログラムが古いモジュールを使っている場合
には警告を発したくなるかもしれません。


.. Python programmers issue warnings by calling the :func:`warn` function defined
.. in this module.  (C programmers use :cfunc:`PyErr_WarnEx`; see
.. :ref:`exceptionhandling` for details).

Python プログラマは、このモジュールの :func:`warn` 関数を使って警告を発することができます。(C 言語のプログラマは
:c:func:`PyErr_WarnEx` を使います; 詳細は :ref:`exceptionhandling` を参照してください)。


.. Warning messages are normally written to ``sys.stderr``, but their disposition
.. can be changed flexibly, from ignoring all warnings to turning them into
.. exceptions.  The disposition of warnings can vary based on the warning category
.. (see below), the text of the warning message, and the source location where it
.. is issued.  Repetitions of a particular warning for the same source location are
.. typically suppressed.

警告メッセージは通常 ``sys.stderr`` に出力されますが、その処理方法は柔軟に変更することができます。
すべての警告を無視することも、警告を例外に変更することもできます。
警告の処理方法は警告カテゴリ (以下参照)、警告メッセージテキスト、そして警告を
発したソースコード上の場所に基づいて変更することができます。ソースコード上の同じ場所に対して特定の警告が繰り返された場合、通常は抑制されます。


.. There are two stages in warning control: first, each time a warning is issued, a
.. determination is made whether a message should be issued or not; next, if a
.. message is to be issued, it is formatted and printed using a user-settable hook.

警告制御には 2 つの段階 (stage) があります: 第一に、警告が発されるたびに、メッセージを出力すべきかどうかの決定が行われます; 次に、
メッセージを出力するなら、メッセージはユーザによって設定が可能なフックを使って書式化され印字されます。


.. The determination whether to issue a warning message is controlled by the
.. warning filter, which is a sequence of matching rules and actions. Rules can be
.. added to the filter by calling :func:`filterwarnings` and reset to its default
.. state by calling :func:`resetwarnings`.

警告メッセージを出力するかどうかの決定は、警告フィルタによって制御されます。警告フィルタは一致規則 (matching
rule)と動作からなるシーケンスです。 :func:`filterwarnings` を呼び出して一致規則をフィルタに追加する
ことができ、 :func:`resetwarnings` を呼び出してフィルタを標準設定の状態にリセットすることができます。


.. The printing of warning messages is done by calling :func:`showwarning`, which
.. may be overridden; the default implementation of this function formats the
.. message by calling :func:`formatwarning`, which is also available for use by
.. custom implementations.

警告メッセージの印字は :func:`showwarning` を呼び出して行うことができ、この関数は上書きすることができます; この関数の標準の実装では、
:func:`formatwarning` を呼び出して警告メッセージを書式化しますが、この関数についても自作の実装を使うことができます。


.. _warning-categories:

警告カテゴリ
------------

.. There are a number of built-in exceptions that represent warning categories.
.. This categorization is useful to be able to filter out groups of warnings.  The
.. following warnings category classes are currently defined:

警告カテゴリを表現する組み込み例外は数多くあります。このカテゴリ化は警告をグループごとフィルタする上で便利です。現在以下の警告カテゴリ
クラスが定義されています:


+----------------------------------+---------------------------------------------------------------------------------------+
| クラス                           | 記述                                                                                  |
+==================================+=======================================================================================+
| :exc:`Warning`                   | すべての警告カテゴリクラスの基底クラスです。 :exc:`Exception`                         |
|                                  | のサブクラスです。                                                                    |
+----------------------------------+---------------------------------------------------------------------------------------+
| :exc:`UserWarning`               | :func:`warn` の標準のカテゴリです。                                                   |
+----------------------------------+---------------------------------------------------------------------------------------+
| :exc:`DeprecationWarning`        | その機能が廃止されていることを示す警告カテゴリの基底クラスです。                      |
|                                  | (デフォルトでは無視されます)                                                          |
+----------------------------------+---------------------------------------------------------------------------------------+
| :exc:`SyntaxWarning`             | その文法機能があいまいであることを示す警告カテゴリの基底クラスです。                  |
+----------------------------------+---------------------------------------------------------------------------------------+
| :exc:`RuntimeWarning`            | そのランタイム機能があいまいであることを示す警告カテゴリの基底クラスです。            |
+----------------------------------+---------------------------------------------------------------------------------------+
| :exc:`FutureWarning`             | その構文の意味付けが将来変更される予定であることを示す警告カテゴリの基底クラスです。  |
+----------------------------------+---------------------------------------------------------------------------------------+
| :exc:`PendingDeprecationWarning` | 将来その機能が廃止されることを示す                                                    |
|                                  | 警告カテゴリの基底クラスです(デフォルトでは無視されます)。                            |
+----------------------------------+---------------------------------------------------------------------------------------+
| :exc:`ImportWarning`             | モジュールのインポート処理中に引き起こされる                                          |
|                                  | 警告カテゴリの基底クラスです(デフォルトでは無視されます)。                            |
+----------------------------------+---------------------------------------------------------------------------------------+
| :exc:`UnicodeWarning`            | Unicode に関係した警告カテゴリの基底クラスです。                                      |
+----------------------------------+---------------------------------------------------------------------------------------+


.. While these are technically built-in exceptions, they are documented here,
.. because conceptually they belong to the warnings mechanism.

これらは厳密に言えば組み込み例外ですが、概念的には警告メカニズムに属しているのでここで記述されています。


.. User code can define additional warning categories by subclassing one of the
.. standard warning categories.  A warning category must always be a subclass of
.. the :exc:`Warning` class.

標準の警告カテゴリをユーザの作成したコード上でサブクラス化することで、さらに別の警告カテゴリを定義することができます。警告カテゴリは常に
:exc:`Warning` クラスのサブクラスでなければなりません。

.. versionchanged:: 2.7
   :exc:`DeprecationWarning` はデフォルトでは無視されます。


.. _warning-filter:

警告フィルタ
------------

.. The warnings filter controls whether warnings are ignored, displayed, or turned
.. into errors (raising an exception).

警告フィルタは、ある警告を無視すべきか、表示すべきか、あるいは (例外を送出する) エラーにするべきかを制御します。


.. Conceptually, the warnings filter maintains an ordered list of filter
.. specifications; any specific warning is matched against each filter
.. specification in the list in turn until a match is found; the match determines
.. the disposition of the match.  Each entry is a tuple of the form (*action*,
.. *message*, *category*, *module*, *lineno*), where:

概念的には、警告フィルタは複数のフィルタ仕様からなる順番リストを維持しています;
何らかの特定の警告が生じると、一致するものが見つかるまでリスト中の各フィルタとの照合が行われます;
一致したフィルタ仕様がその警告の処理方法を決定します。フィルタの各エントリは
(*action*, *message*, *category*, *module*, *lineno*) からなるタプルです。ここで、


.. * *action* is one of the following strings:

* *action* は以下の文字列のうちの一つです:


  .. +---------------+----------------------------------------------+
  .. | Value         | Disposition                                  |
  .. +===============+==============================================+
  .. | ``"error"``   | turn matching warnings into exceptions       |
  .. +---------------+----------------------------------------------+
  .. | ``"ignore"``  | never print matching warnings                |
  .. +---------------+----------------------------------------------+
  .. | ``"always"``  | always print matching warnings               |
  .. +---------------+----------------------------------------------+
  .. | ``"default"`` | print the first occurrence of matching       |
  .. |               | warnings for each location where the warning |
  .. |               | is issued                                    |
  .. +---------------+----------------------------------------------+
  .. | ``"module"``  | print the first occurrence of matching       |
  .. |               | warnings for each module where the warning   |
  .. |               | is issued                                    |
  .. +---------------+----------------------------------------------+
  .. | ``"once"``    | print only the first occurrence of matching  |
  .. |               | warnings, regardless of location             |
  .. +---------------+----------------------------------------------+

  +---------------+-------------------------------------------------------------------------------------+
  | 値            | 処理方法                                                                            |
  +===============+=====================================================================================+
  | ``"error"``   | 一致した警告を例外に変えます                                                        |
  +---------------+-------------------------------------------------------------------------------------+
  | ``"ignore"``  | 一致した警告を出力しません                                                          |
  +---------------+-------------------------------------------------------------------------------------+
  | ``"always"``  | 一致した警告を常に出力します                                                        |
  +---------------+-------------------------------------------------------------------------------------+
  | ``"default"`` | 一致した警告のうち、警告の原因になった                                              |
  |               | ソースコード上の場所ごとに、最初の警告のみ出力します。                              |
  +---------------+-------------------------------------------------------------------------------------+
  | ``"module"``  | 一致した警告のうち、警告の原因になったモジュールごとに、最初の警告のみ出力します。  |
  +---------------+-------------------------------------------------------------------------------------+
  | ``"once"``    | 一致した警告のうち、警告の原因になった場所にかかわらず最初の警告のみ出力します。    |
  +---------------+-------------------------------------------------------------------------------------+


.. * *message* is a string containing a regular expression that the warning message
..   must match (the match is compiled to always be case-insensitive).

* *message* は正規表現を含む文字列で、メッセージはこのパターンに一致しなければなりません (常に大小文字の区別を
  しないようにコンパイルされます)。


.. * *category* is a class (a subclass of :exc:`Warning`) of which the warning
..   category must be a subclass in order to match.

* *category* はクラス (:exc:`Warning` のサブクラス) です。警告クラスはこのクラスのサブクラスに一致しなければなりません。


.. * *module* is a string containing a regular expression that the module name must
..   match (the match is compiled to be case-sensitive).

* *module* は正規表現を含む文字列で、モジュール名はこのパターンに一致しなければなりません (常に大小文字の区別を
  しないようにコンパイルされます)。


.. * *lineno* is an integer that the line number where the warning occurred must
..   match, or ``0`` to match all line numbers.

* *lineno* は整数で、警告が発生した場所の行番号に一致しなければなりません。 ``0`` の場合はすべての行と一致します。


.. Since the :exc:`Warning` class is derived from the built-in :exc:`Exception`
.. class, to turn a warning into an error we simply raise ``category(message)``.

:exc:`Warning` クラスは組み込みの :exc:`Exception` クラスから派生しているので、警告をエラーに変換するには単に
``category(message)`` を ``raise`` します。


.. The warnings filter is initialized by :option:`-W` options passed to the Python
.. interpreter command line.  The interpreter saves the arguments for all
.. :option:`-W` options without interpretation in ``sys.warnoptions``; the
.. :mod:`warnings` module parses these when it is first imported (invalid options
.. are ignored, after printing a message to ``sys.stderr``).

警告フィルタは Python インタプリタのコマンドラインに渡される :option:`-W` オプションで初期化されます。インタプリタは
:option:`-W` オプションに渡されるすべての引数を ``sys.warnoptions`` に変換せずに保存します; :mod:`warnings`
モジュールは最初に ``import`` された際にこれらの引数を解釈します (無効なオプションは ``sys.stderr`` にメッセージを出力した上で
無視されます)。


.. Default Warning Filters

デフォルトの警告フィルタ
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

デフォルトで、 Python はいくつかの警告フィルタをインストールします。
これはコマンドラインオプション :option:`-W` か :func:`filterwarnings`
関数でオーバーライドできます。

* :exc:`PendingDeprecationWarning`, :exc:`ImportWarning` は無視されます。

* :option:`-b` オプションが1度か2度指定されない限り、 :exc:`BytesWarning` は無視されます。
  ``-b`` の時は警告が表示され、 ``-bb`` の時は例外になります。


.. _warning-suppress:

一時的に警告を抑制する
--------------------------------

.. If you are using code that you know will raise a warning, such as a deprecated
.. function, but do not want to see the warning, then it is possible to suppress
.. the warning using the :class:`catch_warnings` context manager:

廃止予定の関数など、警告を発生させることが分かっているコードを利用する場合に、
そのような警告を表示したくなければ、 :class:`catch_warnings` コンテキストマネージャーを
使って警告を抑制することができます。


::

    import warnings

    def fxn():
        warnings.warn("deprecated", DeprecationWarning)

    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        fxn()


.. While within the context manager all warnings will simply be ignored. This
.. allows you to use known-deprecated code without having to see the warning while
.. not suppressing the warning for other code that might not be aware of its use
.. of deprecated code.  Note: this can only be guaranteed in a single-threaded
.. application. If two or more threads use the :class:`catch_warnings` context
.. manager at the same time, the behavior is undefined.

このサンプルのコンテキストマネージャーの中では、すべての警告が無視されています。
これで、他の廃止予定のコードを含まない(つもりの)部分まで警告を抑止せずに、
廃止予定だと分かっているコードだけ警告を表示させないようにすることができます。
注意: これが保証できるのはシングルスレッドのアプリケーションだけです。
2つ以上のスレッドが同時に :class:`catch_warnings` コンテキストマネージャーを使用した場合、
動作は未定義です。


.. _warning-testing:

警告のテスト
----------------

.. To test warnings raised by code, use the :class:`catch_warnings` context
.. manager. With it you can temporarily mutate the warnings filter to facilitate
.. your testing. For instance, do the following to capture all raised warnings to
.. check:

コードが警告を発生させることをテストするには、 :class:`catch_warnings`
コンテキストマネージャーを利用します。
このクラスを使うと、一時的に警告フィルタを操作してテストに利用できます。
例えば、次のコードでは、発生したすべての警告を取得してチェックしています。


::

    import warnings

    def fxn():
        warnings.warn("deprecated", DeprecationWarning)

    with warnings.catch_warnings(record=True) as w:
        # Cause all warnings to always be triggered.
        warnings.simplefilter("always")
        # Trigger a warning.
        fxn()
        # Verify some things
        assert len(w) == 1
        assert issubclass(w[-1].category, DeprecationWarning)
        assert "deprecated" in str(w[-1].message)


.. One can also cause all warnings to be exceptions by using ``error`` instead of
.. ``always``. One thing to be aware of is that if a warning has already been
.. raised because of a ``once``/``default`` rule, then no matter what filters are
.. set the warning will not be seen again unless the warnings registry related to
.. the warning has been cleared.

``always`` の代わりに ``error`` を利用することで、すべての警告で例外を発生させることができます。
1つ気をつけないといけないのは、一度 ``once``/``default`` ルールによって発生した警告は、
フィルタに何をセットしているかにかかわらず、警告レジストリをクリアしない限りは
2度と発生しません。


.. Once the context manager exits, the warnings filter is restored to its state
.. when the context was entered. This prevents tests from changing the warnings
.. filter in unexpected ways between tests and leading to indeterminate test
.. results. The :func:`showwarning` function in the module is also restored to
.. its original value.  Note: this can only be guaranteed in a single-threaded
.. application. If two or more threads use the :class:`catch_warnings` context
.. manager at the same time, the behavior is undefined.

コンテキストマネージャーが終了したら、警告フィルタはコンテキストマネージャーに\
入る前のものに戻されます。これは、テスト中に予期しない方法で警告フィルタが変更され、
テスト結果が中途半端になる事を予防します。
このモジュールの :func:`showwarning` 関数も元の値に戻されます。
注意: これが保証できるのはシングルスレッドのアプリケーションだけです。
2つ以上のスレッドが同時に :class:`catch_warnings` コンテキストマネージャを使用した場合、
動作は未定義です。


.. When testing multiple operations that raise the same kind of warning, it
.. is important to test them in a manner that confirms each operation is raising
.. a new warning (e.g. set warnings to be raised as exceptions and check the
.. operations raise exceptions, check that the length of the warning list
.. continues to increase after each operation, or else delete the previous
.. entries from the warnings list before each new operation).

同じ種類の警告を発生させる複数の操作をテストする場合、
各操作が新しい警告を発生させている事を確認するのは大切な事です。
(例えば、警告を例外として発生させて各操作が例外を発生させることを確認したり、
警告リストの長さが各操作で増加していることを確認したり、
警告リストを各操作の前に毎回クリアする事ができます。)


.. Updating Code For New Versions of Python

コードを新しいバージョンの Python のために更新する
---------------------------------------------------

開発者にしか興味の無い警告はデフォルトでは無視されるようになりました。
なので、通常、コードをテストするときには無視されている例外を表示するように
するべきです。これは :option:`-Wd` コマンドライン引数 (:option:`-W default` の省略形)
をインタプリタに指定することで可能です。これはデフォルトでは無視されているものも
含めた全ての警告に対して、デフォルトの動作を有効にします。
この動作を変更するためには、 :option:`-W` への引数を変更します。
例えば、 :option:`-W error` のようにします。
何が可能かについての詳細は、 :option:`-W` フラグを参照してください。


プログラムから :option:`-Wd` と同じ事をするには、次のようにします::

  warnings.simplefilter('default')

このコードは可能な限り早く実行してください。そうすることで、どの警告を
発生させるかの登録が、将来の警告がどう扱われるかについて思わぬ影響を与える
ことを避けることができます。


いくつかの警告がデフォルトで無視されているのは、開発者向けの警告をユーザーに
見せることを避けるためです。ユーザーがどのインタプリタを使ってコードを実行するのかを
必ずしも制御できるわけではないので、自分のコードのリリースサイクルの間に
新しいバージョンの Python がリリースされるかもしれません。
新しいインタプリタは、古いインタプリタが出さなかった新しい警告を発生させるかもしれません。
例えば、利用しているモジュールに対して :exc:`DeprecationWarning` が発生されることがあります。
開発者としては、廃止予定のモジュールを利用していることに対する通知は有益ですが、
一般ユーザーにとってはこの情報はノイズでしか無く、何の役にも立ちません。


.. _warning-functions:

利用可能な関数
--------------


.. function:: warn(message[, category[, stacklevel]])

   .. Issue a warning, or maybe ignore it or raise an exception.  The *category*
   .. argument, if given, must be a warning category class (see above); it defaults to
   .. :exc:`UserWarning`.  Alternatively *message* can be a :exc:`Warning` instance,
   .. in which case *category* will be ignored and ``message.__class__`` will be used.
   .. In this case the message text will be ``str(message)``. This function raises an
   .. exception if the particular warning issued is changed into an error by the
   .. warnings filter see above.  The *stacklevel* argument can be used by wrapper
   .. functions written in Python, like this:

   警告を発するか、無視するか、あるいは例外を送出します。 *category* 引数が与えられた場合、警告カテゴリクラスでなければなりません
   (上記を参照してください); 標準の値は :exc:`UserWarning` です。 *message* を :exc:`Warning` インスタンスで代用する
   こともできますが、この場合 *category* は無視され、 ``message.__class__`` が使われ、メッセージ文は
   ``str(message)`` になります。発された例外が前述した警告フィルタによってエラーに変更された場合、この関数は例外を送出します。引数
   *stacklevel* は Python でラッパー関数を書く際に利用することができます。例えば


   ::

      def deprecation(message):
          warnings.warn(message, DeprecationWarning, stacklevel=2)


   .. This makes the warning refer to :func:`deprecation`'s caller, rather than to the
   .. source of :func:`deprecation` itself (since the latter would defeat the purpose
   .. of the warning message).

   こうすることで、警告が参照するソースコード部分を、 :func:`deprecation` 自身ではなく :func:`deprecation` を
   呼び出した側にできます (というのも、前者の場合は警告メッセージの目的を台無しにしてしまうからです)。


.. function:: warn_explicit(message, category, filename, lineno[, module[, registry[, module_globals]]])

   .. This is a low-level interface to the functionality of :func:`warn`, passing in
   .. explicitly the message, category, filename and line number, and optionally the
   .. module name and the registry (which should be the ``__warningregistry__``
   .. dictionary of the module).  The module name defaults to the filename with
   .. ``.py`` stripped; if no registry is passed, the warning is never suppressed.
   .. *message* must be a string and *category* a subclass of :exc:`Warning` or
   .. *message* may be a :exc:`Warning` instance, in which case *category* will be
   .. ignored.

   :func:`warn` の機能に対する低レベルのインタフェースで、メッセージ、警告カテゴリ、ファイル名および行番号、そしてオプションの
   モジュール名およびレジストリ情報 (モジュールの  ``__warningregistry__`` 辞書) を明示的に渡します。モジュール名は標準で
   ``.py`` が取り去られたファイル名になります; レジストリが渡されなかった場合、警告が抑制されることはありません。 *message*
   が文字列のとき、 *category* は :exc:`Warning` のサブクラスでなければなりません。また *message* は
   :exc:`Warning` のインスタンスであってもよく、この場合 *category* は無視されます。


   .. *module_globals*, if supplied, should be the global namespace in use by the code
   .. for which the warning is issued.  (This argument is used to support displaying
   .. source for modules found in zipfiles or other non-filesystem import
   .. sources).

   *module_globals* は、もし与えられるならば、警告が発せられるコードが使っているグローバル名前空間でなければなりません。(この引数は
   zipfile やその他の非ファイルシステムのインポート元の中にあるモジュールのソースを表示することをサポートするためのものです)


   .. .. versionchanged:: 2.5
   ..    Added the *module_globals* parameter.

   .. versionchanged:: 2.5
      *module_globals* 引数が追加されました


.. function:: warnpy3k(message[, category[, stacklevel]])

   .. Issue a warning related to Python 3.x deprecation. Warnings are only shown
   .. when Python is started with the -3 option. Like :func:`warn` *message* must
   .. be a string and *category* a subclass of :exc:`Warning`. :func:`warnpy3k`
   .. is using :exc:`DeprecationWarning` as default warning class.

   Python 3.x で廃止予定についての警告を発生させます。
   Pythonが -3 オプション付きで実行されているときのみ警告が表示されます。
   :func:`warn` と同じく、 *message* は文字列で、 *category* は :exc:`Warninp`
   のサブクラスである必要があります。
   :func:`warnpy3k` は :exc:`DeprecationWarning` をデフォルトのwarningクラスとして利用しています。


   .. versionadded:: 2.6


.. function:: showwarning(message, category, filename, lineno[, file[, line]])

   警告をファイルに書き込みます。標準の実装では、 ``formatwarning(message, category, filename, lineno, line)``
   を呼び出し、返された文字列を *file* に書き込みます。 *file* は標準では ``sys.stderr`` です。この関数は
   ``warnings.showwarning`` に別の実装を代入して置き換えることができます。
   *line* は警告メッセージに含めるソースコードの1行です。
   *line* が与えられない場合、 :func:`showwarning` は *filename* と *lineno*
   から行を取得することを試みます。

   .. versionchanged:: 2.7
      *line* 引数のサポートが必須になりました。


.. function:: formatwarning(message, category, filename, lineno[, line])

   警告を通常の方法で書式化します。返される文字列内には改行が埋め込まれている可能性があり、かつ文字列は改行で終端されています。
   *line* は警告メッセージに含まれるソースコードの1行です。
   *line* が渡されない場合、 :func:`formatwarning` は *filename* と *fileno*
   から行の取得を試みます。


   .. .. versionchanged:: 2.6
   ..    Added the *line* argument.

   .. versionchanged:: 2.6
      *line* 引数を追加しました。


.. function:: filterwarnings(action[, message[, category[, module[, lineno[, append]]]]])

   .. Insert an entry into the list of :ref:`warnings filter specifications
   .. <warning-filter>`.  The entry is inserted at the front by default; if
   .. *append* is true, it is inserted at the end.  This checks the types of the
   .. arguments, compiles the *message* and *module* regular expressions, and
   .. inserts them as a tuple in the list of warnings filters.  Entries closer to
   .. the front of the list override entries later in the list, if both match a
   .. particular warning.  Omitted arguments default to a value that matches
   .. everything.

   :ref:`警告フィルタ仕様 <warning-filter>` のリストにエントリを一つ挿入します。標準ではエントリは先頭に挿入されます; *append* が真ならば、末尾に挿入されます。
   この関数は引数の型をチェックし、 *message* および *module* の正規表現をコンパイルしてから、これらをタプルにして警告フィルタ
   のリストに挿入します。二つのエントリが特定の警告に合致した場合、リストの先頭に近い方のエントリが後方にあるエントリに優先します。
   引数が省略されると、標準ではすべてにマッチする値に設定されます。


.. function:: simplefilter(action[, category[, lineno[, append]]])

   .. Insert a simple entry into the list of :ref:`warnings filter specifications
   .. <warning-filter>`.  The meaning of the function parameters is as for
   .. :func:`filterwarnings`, but regular expressions are not needed as the filter
   .. inserted always matches any message in any module as long as the category and
   .. line number match.

   単純なエントリを :ref:`警告フィルタ仕様 <warning-filter>` のリストに挿入します。引数の意味は :func:`filterwarnings` と同じですが、この関数により挿入されるフィ
   ルタはカテゴリと行番号が一致していればすべてのモジュールのすべてのメッセージに合致しますので、正規表現は必要ありません。


.. function:: resetwarnings()

   .. Reset the warnings filter.  This discards the effect of all previous calls to
   .. :func:`filterwarnings`, including that of the :option:`-W` command line options
   .. and calls to :func:`simplefilter`.

   警告フィルタをリセットします。これにより、 :option:`-W` コマンドラインオプションによるもの :func:`simplefilter`
   呼び出しによるものを含め、 :func:`filterwarnings` の呼び出しによる影響はすべて無効化されます。


.. Available Context Managers

利用可能なコンテキストマネージャー
------------------------------------

.. class:: catch_warnings([\*, record=False, module=None])

   .. A context manager that copies and, upon exit, restores the warnings filter
   .. and the :func:`showwarning` function.
   .. If the *record* argument is :const:`False` (the default) the context manager
   .. returns :class:`None` on entry. If *record* is :const:`True`, a list is
   .. returned that is progressively populated with objects as seen by a custom
   .. :func:`showwarning` function (which also suppresses output to ``sys.stdout``).
   .. Each object in the list has attributes with the same names as the arguments to
   .. :func:`showwarning`.

   警告フィルタと :func:`showwarning` 関数をコピーし、終了時に復元するコンテキストマネージャーです。
   *record* 引数が :const:`False` (デフォルト値)だった場合、コンテキスト開始時には :const:`None`
   を返します。もし *record* が :const:`True` だった場合、リストを返します。
   このリストにはカスタムの :func:`showwarning` 関数(この関数は同時に ``sys.stdout`` への出力を抑制します)によって
   オブジェクトが継続的に追加されます。
   リストの中の各オブジェクトは、 :func:`showwarning` 関数の引数と同じ名前の属性を持っています。


   .. The *module* argument takes a module that will be used instead of the
   .. module returned when you import :mod:`warnings` whose filter will be
   .. protected. This argument exists primarily for testing the :mod:`warnings`
   .. module itself.

   *module* 引数は、保護したいフィルタを持つモジュールを取ります。
   :mod:`warnings` を import して得られるモジュールの代わりに利用されます。
   この引数は、主に :mod:`warnings` モジュール自体をテストする目的で追加されました。

   .. note::

      .. The :class:`catch_warnings` manager works by replacing and
      .. then later restoring the module's
      .. :func:`showwarning` function and internal list of filter
      .. specifications.  This means the context manager is modifying
      .. global state and therefore is not thread-safe.

      :class:`catch_warnings` マネージャーは、モジュールの :func:`showwarning`
      関数と内部のフィルタ仕様のリストを置き換え、その後復元することによって
      動作しています。これは、コンテキストマネージャーがグローバルな状態を変更
      していることを意味していて、したがってスレッドセーフではありません。


   .. note::

      .. In Python 3.0, the arguments to the constructor for
      .. :class:`catch_warnings` are keyword-only arguments.

      Python 3.0 では、 :class:`catch_warnings` コンストラクタの引数はキーワード引数のみになりました。


   .. versionadded:: 2.6
