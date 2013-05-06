
:mod:`code` --- インタプリタ基底クラス
======================================

.. module:: code
   :synopsis: read-eval-print ループを実装するのを助ける



``code`` モジュールはread-eval-print (読み込み-評価-表示)ループをPythonで実装するための機能を提供します。
対話的なインタプリタプロンプトを提供するアプリケーションを作るために使える二つのクラスと便利な関数が含まれています。


.. class:: InteractiveInterpreter([locals])

   このクラスは構文解析とインタプリタ状態(ユーザの名前空間)を取り扱います。
   入力バッファリングやプロンプト出力、または入力ファイル指定を扱いません(ファイル名は常に明示的に渡されます)。
   オプションの *locals* 引数はその中でコードが実行される辞書を指定します。
   その初期値は、キー ``'__name__'`` が ``'__console__'`` に設定され、キー ``'__doc__'`` が ``None``
   に設定された新しく作られた辞書です。


.. class:: InteractiveConsole([locals[, filename]])

   対話的なPythonインタプリタの振る舞いを厳密にエミュレートします。
   このクラスは :class:`InteractiveInterpreter` を元に作られていて、通常の
   ``sys.ps1`` と ``sys.ps2`` をつかったプロンプト出力と入力バッファリングが追加されています。


.. function:: interact([banner[, readfunc[, local]]])

   read-eval-printループを実行するための便利な関数。
   これは :class:`InteractiveConsole` の新しいインスタンスを作り、
   *readfunc* が与えられた場合は :meth:`raw_input`
   メソッドとして使われるように設定します。
   *local* が与えられた場合は、インタプリタループのデフォルト名前空間として使うために
   :class:`InteractiveConsole` コンストラクタへ渡されます。
   そして、インスタンスの :meth:`interact` メソッドは見出しとして使うために渡される
   *banner* を受け取り実行されます。コンソールオブジェクトは使われた後捨てられます。


.. function:: compile_command(source[, filename[, symbol]])

   この関数はPythonのインタプリタメインループ(別名、read-eval-printループ)を
   エミュレートしようとするプログラムにとって役に立ちます。
   扱いにくい部分は、ユーザが(完全なコマンドや構文エラーではなく)さらにテキストを入力すれば完全になりうる不完全なコマンドを入力したときを決定することです。
   この関数は*ほとんど*の場合に実際のインタプリタメインループと同じ決定を行います。

   *source* はソース文字列です。
   *filename* はオプションのソースが読み出されたファイル名で、デフォルトで ``'<input>'`` です。
   *symbol* はオプションの文法の開始記号で、 ``'single'``
   (デフォルト)または ``'eval'`` のどちらかにすべきです。

   コマンドが完全で有効ならば、コードオブジェクトを返します(``compile(source, filename,
   symbol)`` と同じ)。コマンドが完全でないならば、 ``None`` を返します。
   コマンドが完全で構文エラーを含む場合は、 :exc:`SyntaxError` を発生させます。
   または、コマンドが無効なリテラルを含む場合は、 :exc:`OverflowError` もしくは :exc:`ValueError`
   を発生させます。


.. _interpreter-objects:

対話的なインタプリタオブジェクト
--------------------------------


.. method:: InteractiveInterpreter.runsource(source[, filename[, symbol]])

   インタプリタ内のあるソースをコンパイルし実行します。
   引数は :func:`compile_command` のものと同じです。
   *filename* のデフォルトは ``'<input>'`` で、 *symbol* は ``'single'`` です。
   あるいくつかのことが起きる可能性があります:

   * 入力はが正しくない。 :func:`compile_command` が例外(:exc:`SyntaxError` か :exc:`OverflowError`)を起こした場合。
     :meth:`showsyntaxerror` メソッドの呼び出によって、構文トレースバックが表示されるでしょう。
     :meth:`runsource` は ``False`` を返します。

   * 入力が完全でなく、さらに入力が必要。 :func:`compile_command` が ``None`` を返した場合。
     :meth:`runsource` は ``True`` を返します。

   * 入力が完全。 :func:`compile_command` がコードオブジェクトを返した場合。
     (:exc:`SystemExit` を除く実行時例外も処理する) :meth:`runcode` を呼び出すことによって、
     コードは実行されます。 :meth:`runsource` は ``False`` を返します。

   次の行を要求するために ``sys.ps1`` か ``sys.ps2`` のどちらを使うかを決定するために、戻り値を利用できます。


.. method:: InteractiveInterpreter.runcode(code)

   コードオブジェクトを実行します。例外が生じたときは、トレースバックを表示するために
   :meth:`showtraceback` が呼び出されます。
   伝わることが許されている :exc:`SystemExit` を除くすべての例外が捉えられます。

   :exc:`KeyboardInterrupt` についての注意。
   このコードの他の場所でこの例外が生じる可能性がありますし、常に捕らえることができるとは限りません。
   呼び出し側はそれを処理するために準備しておくべきです。


.. method:: InteractiveInterpreter.showsyntaxerror([filename])

   起きたばかりの構文エラーを表示します。複数の構文エラーに対して一つあるのではないため、
   これはスタックトレースを表示しません。
   *filename* が与えられた場合は、Pythonのパーサが与えるデフォルトのファイル名の代わりに
   例外の中へ入れられます。なぜなら、文字列から読み込んでいるときはパーサは常に ``'<string>'``
   を使うからです。出力は :meth:`write` メソッドによって書き込まれます。


.. method:: InteractiveInterpreter.showtraceback()

   起きたばかりの例外を表示します。スタックの最初の項目を取り除きます。
   なぜなら、それはインタプリタオブジェクトの実装の内部にあるからです。
   出力は :meth:`write` メソッドによて書き込まれます。


.. method:: InteractiveInterpreter.write(data)

   文字列を標準エラーストリーム(``sys.stderr``)へ書き込みます。
   必要に応じて適切な出力処理を提供するために、派生クラスはこれをオーバーライドすべきです。


.. _console-objects:

対話的なコンソールオブジェクト
------------------------------

:class:`InteractiveConsole` クラスは :class:`InteractiveInterpreter` のサブクラスです。
以下の追加メソッドだけでなく、インタプリタオブジェクトのすべてのメソッドも提供します。


.. method:: InteractiveConsole.interact([banner])

   対話的なPythonコンソールをそっくりにエミュレートします。
   オプションのbanner引数は最初のやりとりの前に表示するバナーを指定します。
   デフォルトでは、標準Pythonインタプリタが表示するものと同じようなバナーを表示します。それに続けて、実際のインタプリタと混乱しないように(とても似ているから!)括弧の中にコンソールオブジェクトのクラス名を表示します。


.. method:: InteractiveConsole.push(line)

   ソーステキストの一行をインタプリタへ送ります。
   その行の末尾に改行がついていてはいけません。
   内部に改行を持っているかもしれません。
   その行はバッファへ追加され、ソースとして連結された内容が渡されインタプリタの :meth:`runsource` メソッドが呼び出されます。
   コマンドが実行されたか、有効であることをこれが示している場合は、バッファはリセットされます。
   そうでなければ、コマンドが不完全で、その行が付加された後のままバッファは残されます。
   さらに入力が必要ならば、戻り値は ``True`` です。
   その行がある方法で処理されたならば、 ``False`` です(これは :meth:`runsource` と同じです)。


.. method:: InteractiveConsole.resetbuffer()

   入力バッファから処理されていないソーステキストを取り除きます。


.. method:: InteractiveConsole.raw_input([prompt])

   プロンプトを書き込み、一行を読み込みます。返る行は末尾に改行を含みません。
   ユーザがEOFキーシーケンスを入力したときは、 :exc:`EOFError` を発生させます。
   基本実装では、組み込み関数 :func:`raw_input` を使います。
   サブクラスはこれを異なる実装と置き換えるかもしれません。

