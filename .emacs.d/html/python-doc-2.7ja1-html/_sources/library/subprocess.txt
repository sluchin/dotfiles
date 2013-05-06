
:mod:`subprocess` --- サブプロセス管理
======================================

.. module:: subprocess
   :synopsis: サブプロセス管理
.. moduleauthor:: Peter Åstrand <astrand@lysator.liu.se>
.. sectionauthor:: Peter Åstrand <astrand@lysator.liu.se>


.. versionadded:: 2.4

:mod:`subprocess` モジュールは、新しくプロセスを開始したり、それらの標準入出力/エラー出力に対してパイプで接続したり、
それらの終了ステータスを取得したりします。このモジュールは以下のような古いいくつかのモジュールを置き換えることを目的としています::

   os.system
   os.spawn*
   os.popen*
   popen2.*
   commands.*

これらのモジュールや関数の代わりに、 :mod:`subprocess` モジュールをどのように使うかについては以下の節で説明します。

.. seealso::

   :pep:`324` -- PEP proposing the subprocess module


subprocess モジュールを使う
---------------------------

このモジュールでは :class:`Popen` と呼ばれるクラスを定義しています:


.. class:: Popen(args, bufsize=0, executable=None, stdin=None, stdout=None, stderr=None, preexec_fn=None, close_fds=False, shell=False, cwd=None, env=None, universal_newlines=False, startupinfo=None, creationflags=0)

   各引数の説明は以下のとおりです:

   *args* は文字列か、あるいはプログラムへの引数のシーケンスです。
   実行するプログラムは通常 *args* シーケンスあるいは文字列の最初の要素ですが、
   *executable* 引数を使うことにより明示的に指定することもできます。
   *executable* が与えられると、その引数シーケンスの最初の要素は、
   多くのプログラムではコマンド名として扱われます。
   ただ、コマンド名は実際に実行される名前とは違う場合があります。
   Unix では、コマンド名は :program:`ps` のようなユーティリティプログラムに表示されます。

   Unix で *shell=False* の場合 (デフォルト): この場合、 Popen クラスは子プログラムを実行するのに
   :meth:`os.execvp` を使います。 文字列が引数として与えられた場合、
   実行されるプログラムの名前かパスとして使われます；
   ただし、プログラムは引数無しの場合のみ動作します。

   .. note::

      *args* を正しくトークン化するには、 :meth:`shlex.split` が便利です。
      このメソッドは特に複雑な状況で活躍します。

      ::

        >>> import shlex, subprocess
        >>> command_line = raw_input()
        /bin/vikings -input eggs.txt -output "spam spam.txt" -cmd "echo '$MONEY'"
        >>> args = shlex.split(command_line)
        >>> print args
        ['/bin/vikings', '-input', 'eggs.txt', '-output', 'spam spam.txt', '-cmd', "ech '$MONEY"]
        >>> p = subprocess.Popen(args) # Success!

      シェルの中で (*-input* 、 *eggs.txt* のように)
      スペースで区切られたオプションと引数は
      リストの別の要素として区切られていること、
      シェルの中で (上にあるようなスペースを含むファイル名や
      *echo* コマンドのように) クォーティングか
      バックスラッシュエスケープが必要なものは
      単一のリスト要素にされていることに注目してください。


   Unix で *shell=True* の場合: args が文字列の場合、
   シェルを介して実行されるコマンドライン文字列を指定します。
   文字列は厳密にシェルプロンプトで打つ形式と一致しなければなりません。
   例えば、文字列の中にスペースを含むファイル名がある場合、
   はクォーティングかバックスラッシュエスケープが必要です。
   args が文字列の場合には最初の要素はコマンド名を表わす文字列として
   そして残りの要素は続く引数としてシェルに渡されます。
   これは、以下の *Popen* と等価ということです。

   ::

      Popen(['/bin/sh', '-c', args[0], args[1], ...])

   .. warning::

      信頼されていないソースからの衛生化されていない入力を組み込んだ
      シェルコマンドを実行すると、任意のコマンドを実行されることになる
      セキュリティ上の重大な欠陥 `シェルインジェクション(en)
      <http://en.wikipedia.org/wiki/Shell_injection#Shell_injection>`_
      に対して脆弱になります。この理由から、コマンド文字列が外部入力から
      構成される場合、 *shell=True* は *絶対に使うべきではありません*::

         >>> from subprocess import call
         >>> filename = input("What file would you like to display?\n")
         What file would you like to display?
         non_existent; rm -rf / #
         >>> call("cat " + filename, shell=True) # Uh-oh. This will end badly...

      *shell=False* は、この脆弱性に悩まされません。上述のノートは、
      *shell=False* を使ったコードを動かすのに役立つでしょう。

   Windows の場合: :class:`Popen` クラスは子プログラムを実行するのに文字列の扱える CreateProcess()
   を使います。 *args* がシーケンスの場合、これは
   :ref:`converting-argument-sequence` で解説する方法に従って、
   文字列に変換されます。

   *bufsize* は、もしこれが与えられた場合、ビルトインの open() 関数の該当する引数と同じ意味をもちます: :const:`0`
   はバッファされないことを意味し、 :const:`1` は行ごとにバッファされることを、それ以外の正の値は (ほぼ)
   その大きさのバッファが使われることを意味します。負の *bufsize* はシステムのデフォルト値が使われることを意味し、
   通常これはバッファがすべて有効となります。 *bufsize* のデフォルト値は :const:`0` (バッファされない) です。

   .. note::
      パフォーマンス上の問題がある場合、 *bufsize* を -1 か十分大きな
      正の値 (例えば 4096) に設定し、バッファを有効にすることを勧めます。

   *executable* 引数には実行するプログラムを指定します。これはほとんど必要ありません: ふつう、実行するプログラムは *args*
   引数で指定されるからです。 ``shell=True`` の場合、 *executable* 引数は使用するシェルを指定します。 Unix
   では、デフォルトのシェルは :file:`/bin/sh` です。Windows では、デフォルトのシェルは :envvar:`COMSPEC`
   環境変数で指定されます。
   Windows で ``shell=True`` を有効にする必要があるのは ``dir`` や ``copy`` などの
   シェル組み込みのコマンドを使いたい場合だけです。
   バッチファイルを実行するときも、コンソールベースで起動するときも、
   ``shell=True`` にする必要はありません。

   *stdin*, *stdout* および *stderr* には、実行するプログラムの標準入力、標準出力、および標準エラー出力の
   ファイルハンドルをそれぞれ指定します。とりうる値は :data:`PIPE` 、既存のファイル記述子 (正の整数)、既存のファイルオブジェクト、そして
   ``None`` です。
   :data:`PIPE` を指定すると新しいパイプが子プロセスに向けて作られます。 ``None``
   を指定するとリダイレクトは起こりません。子プロセスのファイルハンドルはすべて親から受け継がれます。
   加えて、 *stderr* を :data:`STDOUT` にすると、アプリケーションの stderr からの出力は stdout と同じファイルハンドルに出力されます。

   *preexec_fn* に callable オブジェクトが指定されている場合、このオブジェクトは子プロセスが起動されてから、プログラムが exec
   される直前に呼ばれます。(Unixのみ)
   もしくは、Windowsで *close_fds* が真の場合、すべてのファイルハンドルは子プロセスに引き継がれません。
   Windowsの場合、 *close_fds* を真にしながら、 *stdin*, *stdout*, *stderr* を利用して標準ハンドルをリダイレクトすることはできません。

   *close_fds* が真の場合、子プロセスが実行される前に :const:`0` 、 :const:`1` および :const:`2`
   をのぞくすべてのファイル記述子が閉じられます。(Unixのみ)

   *shell* が :const:`True` の場合、指定されたコマンドはシェルを介して実行されます。

   *cwd* が ``None`` 以外の場合、子プロセスのカレントディレクトリが実行される前に *cwd* に変更されます。
   このディレクトリは実行ファイルを探す段階では考慮されませんので、プログラムのパスを *cwd* に対する相対パスで指定することはできない、
   ということに注意してください。

   *env* が ``None`` 以外の場合、これは新しいプロセスでの環境変数を定義します。
   デフォルトでは、子プロセスは現在のプロセスの環境変数を引き継ぎます。

   .. note::

      *env* を特定の値として与える場合、プログラムを実行するのに
      必要な変数全てを与えなければなりません。
      Windows で `side-by-side assembly`_ を実行するためには、
      *env* は正しい :envvar:`SystemRoot` を **含まなければいけません** 。

    .. _side-by-side assembly: http://en.wikipedia.org/wiki/Side-by-Side_Assembly

   *universal_newlines* が :const:`True` の場合、 stdout および stderr
   のファイルオブジェクトはテキストファイルとして open されますが、行の終端は Unix形式の行末 ``'\n'`` か、古い Macintosh 形式の行末
   ``'\r'`` か、あるいは Windows 形式の行末 ``'\r\n'`` のいずれも許されます。これらすべての外部表現は Python プログラムには
   ``'\n'`` として認識されます。

   .. note::

      この機能は Python に universal newline がサポートされている場合 (デフォルト) にのみ有効です。また、
      :attr:`stdout`, :attr:`stdin` および :attr:`stderr` のファイルオブジェクトの newlines 属性は
      communicate() メソッドでは更新されません。

   *startupinfo* は、根底の ``CreateProcess`` 関数に渡される
   :class:`STARTUPINFO` オブジェクトになります。
   *creationflags* は、与えられるなら、 :data:`CREATE_NEW_CONSOLE` または
   :data:`CREATE_NEW_PROCESS_GROUP` にできます。(Windows のみ)


.. data:: PIPE

   :class:`Popen` の *stdin*, *stdout*, *stderr* 引数に渡して、標準ストリームに対する
   パイプを開くことを指定するための特別な値.


.. data:: STDOUT

   :class:`Popen` の *stderr* 引数に渡して、標準エラーが標準出力と同じハンドルに出力されるように指定するための特別な値.


便利な関数
^^^^^^^^^^

このモジュールは以下の二つのショートカット関数も定義しています:


.. function:: call(*popenargs, **kwargs)

   コマンドを指定された引数で実行し、そのコマンドが完了するのを待って、 :attr:`returncode` 属性を返します。

   この引数は :class:`Popen` コンストラクタの引数と同じです。使用例::

      >>> retcode = call(["ls", "-l"])

   .. warning::

      :meth:`Popen.wait` のように、これは以下の場合にデッドロックになります。
      ``stdout=PIPE`` および/または ``stderr=PIPE`` を使って、
      子プロセスが十分な出力を生成したのに、出力先が、OS パイプバッファが
      それ以上のデータを受け付けるのを待っているような場合です。


.. function:: check_call(*popenargs, **kwargs)

   コマンドを引数付きで実行します。コマンドが完了するのを待ちます。終了コードがゼロならば終わりますが、そうでなければ
   :exc:`CalledProcessError` 例外を送出します。 :exc:`CalledProcessError` オブジェクトにはリターンコードが
   :attr:`returncode` 属性として収められています。

   引数は :class:`Popen` コンストラクタと一緒です。使用例::

      >>> subprocess.check_call(["ls", "-l"])
      0

   .. versionadded:: 2.5

   .. warning::

      :func:`call` の警告を参照してください。


.. function:: check_output(*popenargs, **kwargs)

   引数でコマンドを実行し、その出力をバイト文字列として返します。

   終了コードが非 0 なら、 :exc:`CalledProcessError` を送出します。
   :exc:`CalledProcessError` オブジェクトは、戻りコードを
   :attr:`returncode` 属性に、出力を :attr:`output` 属性に保持します。

   引数は、 :class:`Popen` コンストラクタのものと同じです。例::

      >>> subprocess.check_output(["ls", "-l", "/dev/null"])
      'crw-rw-rw- 1 root root 1, 3 Oct 18  2007 /dev/null\n'

   stdout 引数は内部で使われるため、許可されません。
   結果の標準エラーを捕捉するには、 ``stderr=subprocess.STDOUT`` を
   使ってください::

      >>> subprocess.check_output(
      ...     ["/bin/sh", "-c", "ls non_existent_file; exit 0"],
      ...     stderr=subprocess.STDOUT)
      'ls: non_existent_file: No such file or directory\n'

   .. versionadded:: 2.7


例外
^^^^

子プロセス内で raise した例外は、新しいプログラムが実行される前であれば、親プロセスでも raise されます。さらに、この例外オブジェクトには
:attr:`child_traceback` という属性が追加されており、これには子プロセスの視点からの traceback 情報が格納されています。

もっとも一般的に起こる例外は :exc:`OSError` です。これは、たとえば存在しないファイルを実行しようとしたときなどに
発生します。アプリケーションは :exc:`OSError` 例外にはあらかじめ準備しておく必要があります。

不適当な引数で :class:`Popen` が呼ばれた場合は、 :exc:`ValueError` が発生します。

:func:`check_call` はもし呼び出されたプロセスがゼロでないリターンコードを返したならば :exc:`CalledProcessError`
を送出します。


セキュリティ
^^^^^^^^^^^^

ほかの popen 関数とは異なり、この実装は決して暗黙のうちに /bin/sh を実行しません。これはシェルのメタ文字をふくむすべての文字が
安全に子プロセスに渡されるということを意味しています。


Popen オブジェクト
------------------

:class:`Popen` クラスのインスタンスには、以下のようなメソッドがあります:


.. method:: Popen.poll()

   子プロセスが終了しているかどうかを検査します。
   :attr:`returncode` 属性を設定し、返します。


.. method:: Popen.wait()

   子プロセスが終了するまで待ちます。
   :attr:`returncode` 属性を設定し、返します。

   .. warning::

      これは、子プロセスが十分な出力を生成したのに、出力先が、
      OS パイプバッファがそれ以上のデータを受け付けるのを待っているような
      場合に、デッドロックになります。
      これを避けるために、 :meth:`communicate` を利用してください。

.. method:: Popen.communicate(input=None)

   プロセスと通信します: end-of-file に到達するまでデータを stdin に送信し、stdout および stderr からデータを受信します。
   プロセスが終了するまで待ちます。オプション引数 *input* には子プロセスに送られる文字列か、あるいはデータを送らない場合は ``None``
   を指定します。

   :meth:`communicate` はタプル ``(stdoutdata, stderrdata)`` を返します。

   子プロセスの標準入力にデータを送りたい場合は、 Popen オブジェクトを ``stdin=PIPE``
   と指定して作成しなければなりません。
   同じく、戻り値のタプルから ``None`` ではない値を取得するためには、
   ``stdout=PIPE`` かつ/または ``stderr=PIPE`` を指定しなければなりません。

   .. note::

      受信したデータはメモリ中にバッファされます。
      そのため、返されるデータが大きいかあるいは制限がないような場合はこのメソッドを使うべきではありません。


.. method:: Popen.send_signal(signal)

   *signal* シグナルを子プロセスに送ります。

   .. note::

      Windows では、 SIGTERM は :meth:`terminate` のエイリアスです。
      CTRL_C_EVENT と CTRL_BREAK_EVENT を、
      `CREATE_NEW_PROCESS_GROUP` を含む *creationflags* で始まった、
      プロセスに送れます。

   .. versionadded:: 2.6


.. method:: Popen.terminate()

   .. Stop the child. On Posix OSs the method sends SIGTERM to the
      child. On Windows the Win32 API function :c:func:`TerminateProcess` is called
      to stop the child.

   子プロセスを止めます。
   Posix OSでは、このメソッドは SIGTERM シグナルを子プロセスに送ります。
   Windows では、 Win32 API の :c:func:`TerminateProcess` 関数を利用して子プロセスを止めます。

   .. versionadded:: 2.6


.. method:: Popen.kill()

   .. Kills the child. On Posix OSs the function sends SIGKILL to the child.
      On Windows :meth:`kill` is an alias for :meth:`terminate`.

   子プロセスを殺します。
   Posix OS では SIGKILL シグナルを子プロセスに送ります。
   Windows では、 :meth:`kill` は :meth:`terminate` のエイリアスです。

   .. versionadded:: 2.6


以下の属性も利用できます:

.. warning::

   :meth:`.stdin.write`, :meth:`.stdout.read`, :meth:`.stderr.read` を利用すると、
   別のパイプのOSパイプバッファがいっぱいになってデッドロックする恐れがあります。
   これを避けるためには :meth:`communicate` を利用してください。


.. attribute:: Popen.stdin

   *stdin* 引数が :data:`PIPE` の場合、この属性には子プロセスの入力に使われるファイルオブジェクトになります。そうでない場合は ``None``
   です。


.. attribute:: Popen.stdout

   *stdout* 引数が :data:`PIPE` の場合、この属性には子プロセスの出力に使われるファイルオブジェクトになります。そうでない場合は ``None``
   です。


.. attribute:: Popen.stderr

   *stderr* 引数が :data:`PIPE` の場合、この属性には子プロセスのエラー出力に使われるファイルオブジェクトになります。そうでない場合は
   ``None`` です。


.. attribute:: Popen.pid

   子プロセスのプロセス ID が入ります。

   *shell* 引数を ``True`` にセットした場合は、生成されたシェルのプロセス ID になります。


.. attribute:: Popen.returncode

   :meth:`poll` か :meth:`wait` (か、間接的に :meth:`communicate` )から設定された、子プロセスの終了ステータスが入ります。
   ``None`` はまだその子プロセスが終了していないことを示します。

   負の値 -N は子プロセスがシグナル N により中止させられたことを示します (Unix のみ)。


Windows Popen ヘルパ
---------------------

:class:`STARTUPINFO` クラスと以下の定数は、Windows でいつでも利用できます。

.. class:: STARTUPINFO()

   :class:`Popen` の生成に使われる Windows
   `STARTUPINFO <http://msdn.microsoft.com/en-us/library/ms686331(v=vs.85).aspx>`__
   構造の部分的なサポートです。

   .. attribute:: dwFlags

      特定の :class:`STARTUPINFO` のメンバが、プロセスがウィンドウを
      生成するときに使われるかを決定するビットフィールドです::

         si = subprocess.STARTUPINFO()
         si.dwFlags = subprocess.STARTF_USESTDHANDLES | subprocess.STARTF_USESHOWWINDOW

   .. attribute:: hStdInput

      :attr:`dwFlags` が :data:`STARTF_USESTDHANDLES` を指定すれば、
      このメンバがプロセスの標準入力処理です。
      :data:`STARTF_USESTDHANDLES` が指定されなければ、標準入力のデフォルトは
      キーボードバッファです。

   .. attribute:: hStdOutput

      :attr:`dwFlags` が :data:`STARTF_USESTDHANDLES` を指定すれば、
      このメンバがプロセスの標準出力処理です。
      そうでなければ、このメンバは無視され、標準出力のデフォルトは
      コンソールウィンドウのバッファです。

   .. attribute:: hStdError

      :attr:`dwFlags` が :data:`STARTF_USESTDHANDLES` を指定すれば、
      このメンバがプロセスの標準エラー処理です。
      そうでなければ、このメンバは無視され、標準エラーのデフォルトは
      コンソールウィンドウのバッファです。

   .. attribute:: wShowWindow

      :attr:`dwFlags` が :data:`STARTF_USESHOWWINDOW` を指定すれば、
      このメンバは
      `ShowWindow <http://msdn.microsoft.com/en-us/library/ms633548(v=vs.85).aspx>`__
      関数の ``nCmdShow`` パラメタで指定された値なら、 ``SW_SHOWDEFAULT``
      以外の任意のものにできます。しかし、このメンバは無視されます。

      この属性には :data:`SW_HIDE` が提供されています。
      これは、 :class:`Popen` が ``shell=True`` として呼び出されたときに
      使われます。


定数
^^^^

:mod:`subprocess` モジュールは、以下の定数を公開します。

.. data:: STD_INPUT_HANDLE

   標準入力デバイスです。この初期値は、コンソール入力バッファ、
   ``CONIN$`` です。

.. data:: STD_OUTPUT_HANDLE

   標準出力デバイスです。この初期値は、アクティブコンソールスクリーン、
   ``CONOUT$`` です。

.. data:: STD_ERROR_HANDLE

   標準エラーデバイスです。この初期値は、アクティブコンソールスクリーン、
   ``CONOUT$`` です。

.. data:: SW_HIDE

   ウィンドウを隠します。別のウィンドウが活性化します。

.. data:: STARTF_USESTDHANDLES

   追加情報を保持する、 :attr:`STARTUPINFO.hStdInput`,
   :attr:`STARTUPINFO.hStdOutput`, および :attr:`STARTUPINFO.hStdError`
   メンバを指定します。

.. data:: STARTF_USESHOWWINDOW

   追加情報を保持する、 :attr:`STARTUPINFO.wShowWindow` 
   メンバを指定します。

.. data:: CREATE_NEW_CONSOLE

   新しいプロセスが、親プロセスのコンソールを継承する (デフォルト) 
   のではなく、新しいコンソールを持ちます。

   :class:`Popen` が ``shell=True`` として生成されたとき、
   このフラグは必ず設定されます。

.. data:: CREATE_NEW_PROCESS_GROUP

   新しいプロセスグループが生成されることを指定する :class:`Popen`
   ``creationflags`` パラメタです。このフラグは、サブプロセスで
   :func:`os.kill` を使うのに必要です。

   :data:`CREATE_NEW_CONSOLE` が指定されていたら、このフラグは
   無視されます。


.. _subprocess-replacements:

古い関数を subprocess モジュールで置き換える
--------------------------------------------

以下、この節では、"a ==> b" と書かれているものは a の代替として b が使えるということを表します。

.. note::

   この節で紹介されている関数はすべて、実行するプログラムが見つからないときは (いくぶん) 静かに終了します。このモジュールは :exc:`OSError`
   例外を発生させます。

以下の例では、 subprocess モジュールは "from subprocess import \*" でインポートされたと仮定しています。


/bin/sh シェルのバッククォートを置き換える
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

   output=`mycmd myarg`
   ==>
   output = Popen(["mycmd", "myarg"], stdout=PIPE).communicate()[0]


シェルのパイプラインを置き換える
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

   output=`dmesg | grep hda`
   ==>
   p1 = Popen(["dmesg"], stdout=PIPE)
   p2 = Popen(["grep", "hda"], stdin=p1.stdout, stdout=PIPE)
   p1.stdout.close()  # Allow p1 to receive a SIGPIPE if p2 exits.
   output = p2.communicate()[0]

p2 を開始した後の p1.stdout.close() の呼び出しは、p1 が p2 の前に
存在した場合に、p1 が SIGPIPE を受け取るために重要です。

:func:`os.system()` を置き換える
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

   sts = os.system("mycmd" + " myarg")
   ==>
   p = Popen("mycmd" + " myarg", shell=True)
   sts = os.waitpid(p.pid, 0)[1]

注意:

* このプログラムは普通シェル経由で呼び出す必要はありません。

* 終了状態を見るよりも :attr:`returncode` 属性を見るほうが簡単です。

より現実的な例ではこうなるでしょう::

   try:
       retcode = call("mycmd" + " myarg", shell=True)
       if retcode < 0:
           print >>sys.stderr, "子プロセスがシグナルによって中止されました", -retcode
       else:
           print >>sys.stderr, "子プロセスが終了コードを返しました", retcode
   except OSError, e:
       print >>sys.stderr, "実行に失敗しました:", e


:func:`os.spawn <os.spawnl>` 関数群を置き換える
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

P_NOWAIT の例::

   pid = os.spawnlp(os.P_NOWAIT, "/bin/mycmd", "mycmd", "myarg")
   ==>
   pid = Popen(["/bin/mycmd", "myarg"]).pid

P_WAIT の例::

   retcode = os.spawnlp(os.P_WAIT, "/bin/mycmd", "mycmd", "myarg")
   ==>
   retcode = call(["/bin/mycmd", "myarg"])

シーケンスを使った例::

   os.spawnvp(os.P_NOWAIT, path, args)
   ==>
   Popen([path] + args[1:])

環境変数を使った例::

   os.spawnlpe(os.P_NOWAIT, "/bin/mycmd", "mycmd", "myarg", env)
   ==>
   Popen(["/bin/mycmd", "myarg"], env={"PATH": "/usr/bin"})


:func:`os.popen`, :func:`os.popen2`, :func:`os.popen3` を置き換える
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

   pipe = os.popen("cmd", 'r', bufsize)
   ==>
   pipe = Popen("cmd", shell=True, bufsize=bufsize, stdout=PIPE).stdout

::

   pipe = os.popen("cmd", 'w', bufsize)
   ==>
   pipe = Popen("cmd", shell=True, bufsize=bufsize, stdin=PIPE).stdin

::

   (child_stdin, child_stdout) = os.popen2("cmd", mode, bufsize)
   ==>
   p = Popen("cmd", shell=True, bufsize=bufsize,
             stdin=PIPE, stdout=PIPE, close_fds=True)
   (child_stdin, child_stdout) = (p.stdin, p.stdout)

::

   (child_stdin,
    child_stdout,
    child_stderr) = os.popen3("cmd", mode, bufsize)
   ==>
   p = Popen("cmd", shell=True, bufsize=bufsize,
             stdin=PIPE, stdout=PIPE, stderr=PIPE, close_fds=True)
   (child_stdin,
    child_stdout,
    child_stderr) = (p.stdin, p.stdout, p.stderr)

::

   (child_stdin, child_stdout_and_stderr) = os.popen4("cmd", mode,
                                                      bufsize)
   ==>
   p = Popen("cmd", shell=True, bufsize=bufsize,
             stdin=PIPE, stdout=PIPE, stderr=STDOUT, close_fds=True)
   (child_stdin, child_stdout_and_stderr) = (p.stdin, p.stdout)

Unix では、 os.popen2、os.popen3、os.popen4 は
実行するコマンドとしてシーケンスも受け入れます。
どちらにせよ、引数はシェルの干渉を受けることなく直接渡されます。
この使い方は以下のように置き換えられます。

::

   (child_stdin, child_stdout) = os.popen2(["/bin/ls", "-l"], mode,
                                           bufsize)
   ==>
   p = Popen(["/bin/ls", "-l"], bufsize=bufsize,
             stdin=PIPE, stdout=PIPE)
   (child_stdin, child_stdout) = (p.stdin, p.stdout)

終了コードハンドリングは以下のように解釈します。

::

   pipe = os.popen("cmd", 'w')
   ...
   rc = pipe.close()
   if rc is not None and rc >> 8:
       print "There were some errors"
   ==>
   process = Popen("cmd", 'w', shell=True, stdin=PIPE)
   ...
   process.stdin.close()
   if process.wait() != 0:
       print "There were some errors"

:mod:`popen2` モジュールの関数群を置き換える
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

   (child_stdout, child_stdin) = popen2.popen2("somestring", bufsize, mode)
   ==>
   p = Popen(["somestring"], shell=True, bufsize=bufsize,
             stdin=PIPE, stdout=PIPE, close_fds=True)
   (child_stdout, child_stdin) = (p.stdout, p.stdin)

Unix では、 popen2 は実行するコマンドとしてシーケンスも受け入れます。
どちらにせよ、引数はシェルの干渉を受けることなく、直接渡されます。
この使い方は、以下のように置き換えられます。

::

   (child_stdout, child_stdin) = popen2.popen2(["mycmd", "myarg"], bufsize,
                                               mode)
   ==>
   p = Popen(["mycmd", "myarg"], bufsize=bufsize,
             stdin=PIPE, stdout=PIPE, close_fds=True)
   (child_stdout, child_stdin) = (p.stdout, p.stdin)

popen2.Popen3 および popen2.Popen4 は基本的には subprocess.Popen と同様です。ただし、違う点は:

* :class:`Popen` は実行できなかった場合に例外を発生させます。

* *capturestderr* 引数は *stderr* 引数に代わりました。

* ``stdin=PIPE`` および ``stdout=PIPE`` を指定する必要があります。

* popen2 はデフォルトですべてのファイル記述子を閉じますが、 :class:`Popen` では明示的に ``close_fds=True``
  を指定する必要があります。

注釈
-----

.. _converting-argument-sequence:

Windows における引数シーケンスから文字列への変換
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Windows では、 *args* シーケンスは以下の (MS C ランタイムで使われる規則に
対応する) 規則を使って解析できる文字列に変換されます:

1. 引数は、スペースかタブのどちらかの空白で分けられます。

2. ダブルクオーテーションマークで囲まれた文字列は、空白が含まれていたとしても
   1 つの引数として解釈されます。クオートされた文字列は引数に埋め込めます。

3. バックスラッシュに続くダブルクオーテーションマークは、
   文字通りのダブルクオーテーションマークと解釈されます。

4. バックスラッシュは、ダブルクオーテーションが続かない限り、
   文字通りに解釈されます。

5. 複数のバックスラッシュにダブルクオーテーションマークが続くなら、
   バックスラッシュ 2 つで 1 つのバックスラッシュ文字と解釈されます。
   バックスラッシュの数が奇数なら、最後のバックスラッシュは
   規則 3 に従って続くダブルクオーテーションマークをエスケープします。

