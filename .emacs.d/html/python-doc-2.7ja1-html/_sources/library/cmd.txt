
:mod:`cmd` --- 行指向のコマンドインタープリタのサポート
=======================================================

.. module:: cmd
   :synopsis: 行指向のコマンドインタープリタを構築
.. sectionauthor:: Eric S. Raymond <esr@snark.thyrsus.com>


.. The :class:`Cmd` class provides a simple framework for writing line-oriented
.. command interpreters.  These are often useful for test harnesses, administrative
.. tools, and prototypes that will later be wrapped in a more sophisticated
.. interface.

:class:`Cmd` クラスでは、行指向のコマンドインタープリタを書くための簡単なフレームワークを提供します。
テストハーネスや管理ツール、そして、後により洗練されたインターフェイスでラップするプロトタイプとして、
こうしたインタープリタはよく役に立ちます。

.. seealso::

   最新バージョンの `cmd モジュールの Python ソースコード
   <http://svn.python.org/view/python/branches/release27-maint/Lib/cmd.py?view=markup>`_

.. class:: Cmd([completekey[, stdin[, stdout]]])

   .. A :class:`Cmd` instance or subclass instance is a line-oriented interpreter
   .. framework.  There is no good reason to instantiate :class:`Cmd` itself; rather,
   .. it's useful as a superclass of an interpreter class you define yourself in order
   .. to inherit :class:`Cmd`'s methods and encapsulate action methods.

   :class:`Cmd` インスタンス、あるいはサブクラスのインスタンスは、行指向のインタープリタ・フレームワークです。
   :class:`Cmd` 自身をインスタンス化することはありません。むしろ、 :class:`Cmd` のメソッドを継承したり、
   アクションメソッドをカプセル化するために、あなたが自分で定義するインタープリタクラスのスーパークラスとしての便利です。


   .. The optional argument *completekey* is the :mod:`readline` name of a completion
   .. key; it defaults to :kbd:`Tab`. If *completekey* is not :const:`None` and
   .. :mod:`readline` is available, command completion is done automatically.

   オプション引数 *completekey* は、補完キーの :mod:`readline` 名です。
   デフォルトは :kbd:`Tab` です。 *completekey* が :const:`None` でなく、
   :mod:`readline` が利用できるならば、コマンド補完は自動的に行われます。


   .. The optional arguments *stdin* and *stdout* specify the  input and output file
   .. objects that the Cmd instance or subclass  instance will use for input and
   .. output. If not specified, they will default to :data:`sys.stdin` and
   .. :data:`sys.stdout`.

   オプション引数 *stdin* と *stdout* には、Cmd またはそのサブクラスのインスタンスが
   入出力に使用するファイルオブジェクトを指定します。
   省略時には :data:`sys.stdin` と :data:`sys.stdout` が使用されます。


   .. If you want a given *stdin* to be used, make sure to set the instance's
   .. :attr:`use_rawinput` attribute to ``False``, otherwise *stdin* will be
   .. ignored.

   引数に渡した *stdin* を使いたい場合は、インスタンスの :attr:`use_rawinput` 属性を
   ``False`` にセットしてください。そうしないと *stdin* は無視されます。


   .. .. versionchanged:: 2.3
   ..    The *stdin* and *stdout* parameters were added.

   .. versionchanged:: 2.3
      引数 *stdin* と *stdout* を追加.


.. _cmd-objects:

Cmdオブジェクト
---------------

.. A :class:`Cmd` instance has the following methods:

:class:`Cmd` インスタンスは、次のメソッドを持ちます:


.. method:: Cmd.cmdloop([intro])

   .. Repeatedly issue a prompt, accept input, parse an initial prefix off the
   .. received input, and dispatch to action methods, passing them the remainder of
   .. the line as argument.

   プロンプトを繰り返し出力し、入力を受け取り、受け取った入力から取り去った先頭の語を解析し、
   その行の残りを引数としてアクションメソッドへディスパッチします。


   .. The optional argument is a banner or intro string to be issued before the first
   .. prompt (this overrides the :attr:`intro` class member).

   オプションの引数は、最初のプロンプトの前に表示されるバナーあるいはイントロ用の文字列です
   (これはクラスメンバ :attr:`intro` をオーバーライドします)。


   .. If the :mod:`readline` module is loaded, input will automatically inherit
   .. :program:`bash`\ -like history-list editing (e.g. :kbd:`Control-P` scrolls back
   .. to the last command, :kbd:`Control-N` forward to the next one, :kbd:`Control-F`
   .. moves the cursor to the right non-destructively, :kbd:`Control-B` moves the
   .. cursor to the left non-destructively, etc.).

   :mod:`readline` モジュールがロードされているなら、入力は自動的に :program:`bash`
   のような履歴リスト編集機能を受け継ぎます(例えば、 :kbd:`Control-P`
   は直前のコマンドへのスクロールバック、 :kbd:`Control-N` は次のものへ進む、
   :kbd:`Control-F` はカーソルを右へ非破壊的に進める、 :kbd:`Control-B` はカーソルを非破壊的に左へ移動させる等)。


   .. An end-of-file on input is passed back as the string ``'EOF'``.

   入力のファイル終端は、文字列 ``'EOF'`` として渡されます。


   .. An interpreter instance will recognize a command name ``foo`` if and only if it
   .. has a method :meth:`do_foo`.  As a special case, a line beginning with the
   .. character ``'?'`` is dispatched to the method :meth:`do_help`.  As another
   .. special case, a line beginning with the character ``'!'`` is dispatched to the
   .. method :meth:`do_shell` (if such a method is defined).

   メソッド :meth:`do_foo` を持っている場合に限って、インタープリタのインスタンスはコマンド名
   ``foo`` を認識します。特別な場合として、文字 ``'?'`` で始まる行はメソッド :meth:`do_help`
   へディスパッチします。他の特別な場合として、文字 ``'!'`` で始まる行はメソッド :meth:`do_shell`
   へディスパッチします(このようなメソッドが定義されている場合)。


   .. This method will return when the :meth:`postcmd` method returns a true value.
   .. The *stop* argument to :meth:`postcmd` is the return value from the command's
   .. corresponding :meth:`do_\*` method.

   このメソッドは :meth:`postcmd` メソッドが真を返したときに return します。
   :meth:`postcmd` に対する *stop* 引数は、このコマンドが対応する :meth:`do_\*` メソッドからの返り値です。


   .. If completion is enabled, completing commands will be done automatically, and
   .. completing of commands args is done by calling :meth:`complete_foo` with
   .. arguments *text*, *line*, *begidx*, and *endidx*.  *text* is the string prefix
   .. we are attempting to match: all returned matches must begin with it. *line* is
   .. the current input line with leading whitespace removed, *begidx* and *endidx*
   .. are the beginning and ending indexes of the prefix text, which could be used to
   .. provide different completion depending upon which position the argument is in.

   補完が有効になっているなら、コマンドの補完が自動的に行われます。
   また、コマンド引数の補完は、引数 *text*, *line*, *begidx*, および *endidx*
   と共に :meth:`complete_foo` を呼び出すことによって行われます。
   *text* は、マッチしようとしている文字列の先頭の語です。
   返されるマッチは全てそれで始まっていなければなりません。
   *line* は始めの空白を除いた現在の入力行です。
   *begidx* と *endidx* は先頭のテキストの始まりと終わりのインデックスで、
   引数の位置に依存した異なる補完を提供するのに使えます。


   .. All subclasses of :class:`Cmd` inherit a predefined :meth:`do_help`.  This
   .. method, called with an argument ``'bar'``, invokes the corresponding method
   .. :meth:`help_bar`, and if that is not present, prints the docstring of
   .. :meth:`do_bar`, if available.  With no argument, :meth:`do_help` lists all
   .. available help topics (that is, all commands with corresponding
   .. :meth:`help_\*` methods or commands that have docstrings), and also lists any
   .. undocumented commands.

   :class:`Cmd` のすべてのサブクラスは、定義済みの :meth:`do_help` を継承します。
   このメソッドは、(引数 ``'bar'`` と共に呼ばれたとすると)対応するメソッド :meth:`help_bar`
   を呼び出します。そのメソッドが存在しない場合、 :meth:`do_bar` の docstring があればそれを表示します。
   引数がなければ、 :meth:`do_help` は、すべての利用可能なヘルプ見出し(すなわち、対応する :meth:`help_\*`
   メソッドを持つすべてのコマンドまたは docstring を持つコマンド)をリストアップします。
   また、文書化されていないコマンドでも、すべてリストアップします。


.. method:: Cmd.onecmd(str)

   .. Interpret the argument as though it had been typed in response to the prompt.
   .. This may be overridden, but should not normally need to be; see the
   .. :meth:`precmd` and :meth:`postcmd` methods for useful execution hooks.  The
   .. return value is a flag indicating whether interpretation of commands by the
   .. interpreter should stop.  If there is a :meth:`do_\*` method for the command
   .. *str*, the return value of that method is returned, otherwise the return value
   .. from the :meth:`default` method is returned.

   プロンプトに答えてタイプしたかのように引数を解釈実行します。
   これをオーバーライドすることがあるかもしれませんが、通常は必要ないでしょう。
   便利な実行フックについては、 :meth:`precmd` と :meth:`postcmd` メソッドを参照してください。
   戻り値は、インタープリタによるコマンドの解釈実行をやめるかどうかを示すフラグです。
   コマンド *str* に対応する :meth:`do_\*` メソッドがある場合、そのメソッドの返り値が返されます。
   そうでない場合は :meth:`default` メソッドからの返り値が返されます。


.. method:: Cmd.emptyline()

   .. Method called when an empty line is entered in response to the prompt. If this
   .. method is not overridden, it repeats the last nonempty command entered.

   プロンプトに空行が入力されたときに呼び出されるメソッド。
   このメソッドがオーバーライドされていないなら、最後に入力された空行でないコマンドが繰り返されます。


.. method:: Cmd.default(line)

   .. Method called on an input line when the command prefix is not recognized. If
   .. this method is not overridden, it prints an error message and returns.

   コマンドの先頭の語が認識されないときに、入力行に対して呼び出されます。
   このメソッドがオーバーライドされていないなら、エラーメッセージを表示して戻ります。


.. method:: Cmd.completedefault(text, line, begidx, endidx)

   .. Method called to complete an input line when no command-specific
   .. :meth:`complete_\*` method is available.  By default, it returns an empty list.

   利用可能なコマンド固有の :meth:`complete_\*` が存在しないときに、入力行を補完するために呼び出されるメソッド。
   デフォルトでは、空行を返します。


.. method:: Cmd.precmd(line)

   .. Hook method executed just before the command line *line* is interpreted, but
   .. after the input prompt is generated and issued.  This method is a stub in
   .. :class:`Cmd`; it exists to be overridden by subclasses.  The return value is
   .. used as the command which will be executed by the :meth:`onecmd` method; the
   .. :meth:`precmd` implementation may re-write the command or simply return *line*
   .. unchanged.

   コマンド行 *line* が解釈実行される直前、しかし入力プロンプトが作られ表示された後に実行されるフックメソッド。
   このメソッドは :class:`Cmd` 内のスタブであって、サブクラスでオーバーライドされるために存在します。
   戻り値は :meth:`onecmd` メソッドが実行するコマンドとして使われます。 :meth:`precmd`
   の実装では、コマンドを書き換えるかもしれないし、あるいは単に変更していない *line* を返すかもしれません。


.. method:: Cmd.postcmd(stop, line)

   .. Hook method executed just after a command dispatch is finished.  This method is
   .. a stub in :class:`Cmd`; it exists to be overridden by subclasses.  *line* is the
   .. command line which was executed, and *stop* is a flag which indicates whether
   .. execution will be terminated after the call to :meth:`postcmd`; this will be the
   .. return value of the :meth:`onecmd` method.  The return value of this method will
   .. be used as the new value for the internal flag which corresponds to *stop*;
   .. returning false will cause interpretation to continue.

   コマンドディスパッチが終わった直後に実行されるフックメソッド。
   このメソッドは :class:`Cmd` 内のスタブで、サブクラスでオーバーライドされるために存在します。
   *line* は実行されたコマンド行で、 *stop* は :meth:`postcmd`
   の呼び出しの後に実行を停止するかどうかを示すフラグです。
   これは :meth:`onecmd` メソッドの戻り値です。
   このメソッドの戻り値は、 *stop* に対応する内部フラグの新しい値として使われます。偽を返すと、実行を続けます。


.. method:: Cmd.preloop()

   .. Hook method executed once when :meth:`cmdloop` is called.  This method is a stub
   .. in :class:`Cmd`; it exists to be overridden by subclasses.

   :meth:`cmdloop` が呼び出されたときに一度だけ実行されるフックメソッド。
   このメソッドは :class:`Cmd` 内のスタブであって、サブクラスでオーバーライドされるために存在します。


.. method:: Cmd.postloop()

   .. Hook method executed once when :meth:`cmdloop` is about to return. This method
   .. is a stub in :class:`Cmd`; it exists to be overridden by subclasses.

   :meth:`cmdloop` が戻る直前に一度だけ実行されるフックメソッド。
   このメソッドは :class:`Cmd` 内のスタブであって、サブクラスでオーバーライドされるために存在します。


.. Instances of :class:`Cmd` subclasses have some public instance variables:

:class:`Cmd` のサブクラスのインスタンスは、公開されたインスタンス変数をいくつか持っています:


.. attribute:: Cmd.prompt

   .. The prompt issued to solicit input.

   入力を求めるために表示されるプロンプト。


.. attribute:: Cmd.identchars

   .. The string of characters accepted for the command prefix.

   コマンドの先頭の語として受け入れられる文字の文字列。


.. attribute:: Cmd.lastcmd

   .. The last nonempty command prefix seen.

   最後の空でないコマンドプリフィックス。


.. attribute:: Cmd.intro

   .. A string to issue as an intro or banner.  May be overridden by giving the
   .. :meth:`cmdloop` method an argument.

   イントロあるいはバナーとして表示される文字列。
   :meth:`cmdloop` メソッドに引数を与えるために、オーバーライドされるかもしれません。


.. attribute:: Cmd.doc_header

   .. The header to issue if the help output has a section for documented commands.

   ヘルプ出力に文書化されたコマンドのセクションがある場合に表示するヘッダ。


.. attribute:: Cmd.misc_header

   .. The header to issue if the help output has a section for miscellaneous  help
   .. topics (that is, there are :meth:`help_\*` methods without corresponding
   .. :meth:`do_\*` methods).

   ヘルプの出力にその他のヘルプ見出しがある(すなわち、 :meth:`do_\*` メソッドに対応していない :meth:`help_\*`
   メソッドが存在する)場合に表示するヘッダ。


.. attribute:: Cmd.undoc_header

   .. The header to issue if the help output has a section for undocumented  commands
   .. (that is, there are :meth:`do_\*` methods without corresponding :meth:`help_\*`
   .. methods).

   ヘルプ出力に文書化されていないコマンドのセクションがある(すなわち、対応する :meth:`help_\*`
   メソッドを持たない :meth:`do_\*` メソッドが存在する)場合に表示するヘッダ。


.. attribute:: Cmd.ruler

   .. The character used to draw separator lines under the help-message headers.  If
   .. empty, no ruler line is drawn.  It defaults to ``'='``.

   ヘルプメッセージのヘッダの下に、区切り行を表示するために使われる文字。
   空のときは、ルーラ行が表示されません。デフォルトでは、 ``'='`` です。


.. attribute:: Cmd.use_rawinput

   .. A flag, defaulting to true.  If true, :meth:`cmdloop` uses :func:`raw_input` to
   .. display a prompt and read the next command; if false, :meth:`sys.stdout.write`
   .. and :meth:`sys.stdin.readline` are used. (This means that by importing
   .. :mod:`readline`, on systems that support it, the interpreter will automatically
   .. support :program:`Emacs`\ -like line editing  and command-history keystrokes.)

   フラグで、デフォルトでは真です。
   真ならば、 :meth:`cmdloop` はプロンプトを表示して次のコマンド読み込むために :func:`raw_input` を使います。
   偽ならば、 :meth:`sys.stdout.write` と :meth:`sys.stdin.readline` が使われます。
   (これが意味するのは、 :mod:`readline` を import することによって、
   それをサポートするシステム上では、インタープリタが自動的に
   :program:`Emacs` 形式の行編集とコマンド履歴のキーストロークをサポートするということです。)

