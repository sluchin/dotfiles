:mod:`os` --- 雑多なオペレーティングシステムインタフェース
==========================================================

.. module:: os
   :synopsis: 雑多なオペレーティングシステムインタフェース。

.. This module provides a portable way of using operating system dependent
   functionality.  If you just want to read or write a file see :func:`open`, if
   you want to manipulate paths, see the :mod:`os.path` module, and if you want to
   read all the lines in all the files on the command line see the :mod:`fileinput`
   module.  For creating temporary files and directories see the :mod:`tempfile`
   module, and for high-level file and directory handling see the :mod:`shutil`
   module.

このモジュールは、OS依存の機能をポータブルな方法で利用する方法を提供します。
単純なファイルの読み書きについては、 :func:`open` を参照してください。
パス操作については、 :mod:`os.path` モジュールを参照してください。
コマンドラインに与えられた全てのファイルから行を読み込んでいくには、
:mod:`fileinput` モジュールを参照してください。
一時ファイルや一時ディレクトリの作成については、
:mod:`tempfile` モジュールを参照してください。
高レベルなファイルとディレクトリの操作については、
:mod:`shutil` モジュールを参照してください。

.. The design of all built-in operating system dependent modules of Python is such
   that as long as the same functionality is available, it uses the same interface;
   for example, the function ``os.stat(path)`` returns stat information about
   *path* in the same format (which happens to have originated with the POSIX
   interface).

利用可能性に関する注意:

* Pythonの、全てのOS依存モジュールの設計方針は、
  可能な限り同一のインタフェースで同一の機能を利用できるようにする、というものです。
  例えば、 ``os.stat(path)`` は *path* に関する stat 情報を、
  (POSIXを元にした)同じフォーマットで返します。

* 特定のオペレーティングシステム固有の拡張も :mod:`os` を介して利用することができますが、
  これらの利用はもちろん、可搬性を脅かします！

* 「利用できる環境: Unix」の意味はこの関数が Unix システムにあることが多い
  ということです。
  このことは特定の OS における存在を主張するものではありません。

* 特に記述がない場合、「利用できる環境: Unix」と書かれている関数は、
  Unix をコアにしている Mac OS X でも利用することができます。

.. Availability notes get their own line and occur at the end of the function
.. documentation.
.. 利用可能性に関する注意は各関数の説明の最後に別に一行を割いて書きます。

.. note::

   .. All functions in this module raise :exc:`OSError` in the case of invalid or
      inaccessible file names and paths, or other arguments that have the correct
      type, but are not accepted by the operating system.

   このモジュール内のすべての関数は、間違った、あるいはアクセス出来ないファイル名や
   ファイルパス、その他型が合っていてもOSが受理しない引数に対して、 :exc:`OSError`
   を送出します。


.. exception:: error

   .. An alias for the built-in :exc:`OSError` exception.

   組み込みの :exc:`OSError` 例外に対するエイリアス

.. data:: name

   import されているオペレーティング・システム依存モジュールの名前です。
   現在次の名前が登録されています: ``'posix'``, ``'nt'``,
   ``'os2'``, ``'ce'``, ``'java'``, ``'riscos'``.


.. _os-procinfo:

プロセスのパラメタ
------------------

これらの関数とデータ要素は、現在のプロセスおよびユーザに対する情報提供および操作のための機能を提供しています。


.. data:: environ

   環境変数の値を表すマップ型オブジェクトです。例えば、 ``environ['HOME']`` は( いくつかのプラットフォーム上での) あなたの
   ホームディレクトリへのパスです。これは C の ``getenv("HOME")`` と等価です。

   このマップ型の内容は、 :mod:`os` モジュールの最初の import の時点、通常は Python の起動時に :file:`site.py`
   が処理される中で取り込まれます。それ以後に変更された環境変数は ``os.environ`` を直接変更しない限り反映されません。

   プラットフォーム上で :func:`putenv` がサポートされている場合、このマップ型オブジェクトは環境変数に対するクエリと同様に変更するために使うこ
   ともできます。 :func:`putenv` はマップ型オブジェクトが修正される時に、自動的に呼ばれることになります。

   .. note::

      :func:`putenv` を直接呼び出しても ``os.environ`` の
      内容は変わらないので、 ``os.environ`` を直接変更する方がベターです。

   .. note::

      FreeBSD と Mac OS X を含むいつくかのプラットフォームでは、 ``environ`` の値を変更するとメモリリークの原因になる場合があります。
      システムの :c:func:`putenv` に関するドキュメントを参照してください。

   :func:`putenv` が提供されていない場合、このマッピングオブジェクト
   に変更を加えたコピーを適切なプロセス生成機能に渡して、子プロセスが修正された環境変数を利用するようにできます。

   プラットフォームが :func:`unsetenv` 関数をサポートしているならば、このマッピングからアイテムを取り除いて(delete)環境変数を消すことができます。
   :func:`unsetenv` は ``os.environ`` からアイテムが取り除かれた時に自動的に呼ばれます。
   :meth:`pop` か :meth:`clear` が呼ばれた時も同様です。

   .. versionchanged:: 2.6

      .. Also unset environment variables when calling :meth:`os.environ.clear`
         and :meth:`os.environ.pop`.

      :meth:`os.environ.clear` か :meth:`os.environ.pop` を呼び出した時も、(deleteした時と同様に)
      環境変数を削除するようになりました。

.. function:: chdir(path)
              fchdir(fd)
              getcwd()
   :noindex:

   これらの関数は、 :ref:`os-file-dir` 節で説明されています。


.. function:: ctermid()

   プロセスの制御端末に対応するファイル名を返します。

   利用できる環境: Unix


.. function:: getegid()

   現在のプロセスの実効(effective)グループ id を返します。
   この id は現在のプロセスで実行されているファイルの "set id" ビットに対応します。

   利用できる環境: Unix


.. function:: geteuid()

   .. index:: single: user; effective id

   現在のプロセスの実効(effective)ユーザ id を返します。

   利用できる環境: Unix


.. function:: getgid()

   .. index:: single: process; group

   現在のプロセスの実際のグループ id を返します。

   利用できる環境: Unix


.. function:: getgroups()

   現在のプロセスに関連づけられた従属グループ id のリストを返します。

   利用できる環境: Unix


.. function:: initgroups(username, gid)

   システムの initgroups() を呼んで、指定された *username* がメンバーであるグループと
   *gid* でしていされたグループでグループアクセスリストを初期化する。

   利用できる環境: Unix

   .. versionadded:: 2.7


.. function:: getlogin()

   現在のプロセスの制御端末にログインしているユーザ名を返します。
   ほとんどの場合、ユーザが誰かを知りたいときには環境変数 :envvar:`LOGNAME` を、
   現在の実効ユーザ id のユーザ名を知りたいときには
   ``pwd.getpwuid(os.getuid())[0]`` を使うほうが便利です。

   利用できる環境: Unix


.. function:: getpgid(pid)

   プロセス id *pid* のプロセスのプロセス・グループ id を返します。
   もし *pid* が 0 ならば、現在のプロセスのプロセス・グループ id を返します。

   利用できる環境: Unix
 
   .. versionadded:: 2.3
 
 
.. function:: getpgrp()

   .. index:: single: process; group

   現在のプロセス・グループの id を返します。

   利用できる環境: Unix


.. function:: getpid()

   .. index:: single: process; id

   現在のプロセス id を返します。

   利用できる環境: Unix、 Windows


.. function:: getppid()

   .. index:: single: process; id of parent

   親プロセスの id を返します。

   利用できる環境: Unix


.. function:: getresuid()

   現在のプロセスの real, effective, saved user id を示す、
   (ruid, euid, suid) のタプルを返します。

   利用できる環境: Unix

   .. versionadded:: 2.7


.. function:: getresgid()

   現在のプロセスの real, effective, saved group id を示す、
   (ruid, euid, suid) のタプルを返します。

   利用できる環境: Unix

   .. versionadded:: 2.7


.. function:: getuid()

   .. index:: single: user; id

   現在のプロセスのユーザ id を返します。

   利用できる環境: Unix


.. function:: getenv(varname[, value])

   環境変数 *varname* が存在する場合にはその値を返し、
   存在しない場合には *value* を返します。
   *value* のデフォルト値は ``None`` です。

   利用できる環境: 主な Unix 互換環境、Windows


.. function:: putenv(varname, value)

   .. index:: single: environment variables; setting

   *varname* と名づけられた環境変数の値を文字列 *value* に設定します。
   このような環境変数への変更は、 :func:`os.system`,
   :func:`popen` , :func:`fork` および :func:`execv`
   により起動された子プロセスに影響します。

   利用できる環境: 主な Unix 互換環境、Windows

   .. note::

      FreeBSD と Mac OS X を含むいつくかのプラットフォームでは、
      ``environ`` の値を変更するとメモリリークの原因になる場合があります。
      システムの putenv に関するドキュメントを参照してください。

   :func:`putenv` がサポートされている場合、
   ``os.environ`` の要素に対する代入を行うと自動的に :func:`putenv`
   を呼び出します; 
   しかし、 :func:`putenv` の呼び出しは ``os.environ`` を更新しないので、
   実際には ``os.environ`` の要素に代入する方が望ましい操作です。


.. function:: setegid(egid)

   現在のプロセスに実効グループ id をセットします。

   利用できる環境: Unix


.. function:: seteuid(euid)

   現在のプロセスに実効ユーザ id をセットします。

   利用できる環境: Unix


.. function:: setgid(gid)

   現在のプロセスにグループ id をセットします。

   利用できる環境: Unix


.. function:: setgroups(groups)

   現在のグループに関連付けられた従属グループ id のリストを *groups* に設定します。
   *groups* はシーケンス型でなくてはならず、
   各要素はグループを特定する整数でなくてはなりません。
   この操作は通常、スーパユーザしか利用できません。

   利用できる環境: Unix

   .. versionadded:: 2.2


.. function:: setpgrp()

   システムコール :c:func:`setpgrp` または :c:func:`setpgrp(0, 0)` 
   のどちらかのバージョンのうち、 (実装されていれば)
   実装されている方を呼び出します。
   機能については Unix マニュアルを参照してください。

   利用できる環境: Unix


.. function:: setpgid(pid, pgrp)

   システムコール :c:func:`setpgid` を呼び出して、
   *pid* の id をもつプロセスのプロセスグループ id を *pgrp* に設定します。

   利用できる環境: Unix


.. function:: setregid(rgid, egid)

   現在のプロセスの real, effective group id を設定します。

   利用できる環境: Unix


.. function:: setresgid(rgid, egid, sgid)

   現在のプロセスの real, effective, saved group id を設定します。

   利用できる環境: Unix

   .. versionadded:: 2.7


.. function:: setresuid(ruid, euid, suid)

   Set the current process's real, effective, and saved user ids.

   利用できる環境: Unix

   .. versionadded:: 2.7


.. function:: setreuid(ruid, euid)

   現在のプロセスに対して実際のユーザ id および実効ユーザ id を設定します。

   利用できる環境: Unix


.. function:: getsid(pid)

   システムコール :c:func:`getsid` を呼び出します。
   機能については Unix マニュアルを参照してください。

   利用できる環境: Unix

   .. versionadded:: 2.4


.. function:: setsid()

   システムコール :c:func:`setsid` を呼び出します。
   機能については Unix マニュアルを参照してください。

   利用できる環境: Unix


.. function:: setuid(uid)

   .. index:: single: user; id, setting

   現在のプロセスのユーザ id を設定します。

   利用できる環境: Unix


.. placed in this section since it relates to errno.... a little weak
.. function:: strerror(code)

   エラーコード *code* に対応するエラーメッセージを返します。
   不明なエラーコードに対して :c:func:`strerror` が ``NULL``
   を返す環境では、その場合に :exc:`ValueError` を送出します。

   利用できる環境: Unix, Windows


.. function:: umask(mask)

   現在の数値 umask を設定し、以前の umask 値を返します。

   利用できる環境: Unix, Windows


.. function:: uname()

   .. index::
      single: gethostname() (in module socket)
      single: gethostbyaddr() (in module socket)

   現在のオペレーティングシステムを特定する情報の入った 5 要素のタプルを返します。
   このタプルには 5 つの文字列: ``(sysname, nodename, release, version, machine)`` 
   が入っています。
   システムによっては、ノード名を 8 文字、または先頭の要素だけに切り詰めます;
   ホスト名を取得する方法としては、 :func:`socket.gethostname`
   を使う方がよいでしょう、あるいは
   ``socket.gethostbyaddr(socket.gethostname())`` でもかまいません。

   利用できる環境: Unix互換環境


.. function:: unsetenv(varname)

   .. index:: single: environment variables; deleting

   *varname* という名前の環境変数を取り消します。
   このような環境の変化は :func:`os.system`, :func:`popen` または
   :func:`fork` と :func:`execv` で開始されるサブプロセスに影響を与えます。

   利用できる環境:  ほとんどの Unix 互換環境、Windows

   :func:`unsetenv` がサポートされている時には
   ``os.environ`` のアイテムの削除が対応する :func:`unsetenv`
   の呼び出しに自動的に翻訳されます。
   しかし、 :func:`unsetenv` の呼び出しは ``os.environ`` を更新しませんので、
   むしろ ``os.environ`` のアイテムを削除する方が好ましい方法です。


.. _os-newstreams:

ファイルオブジェクトの生成
--------------------------

以下の関数は新しいファイルオブジェクトを作成します。(:func:`open` も参照してください)


.. function:: fdopen(fd[, mode[, bufsize]])

   .. index:: single: I/O control; buffering

   ファイル記述子 *fd* に接続している、開かれたファイルオブジェクトを返します。
   引数 *mode* および *bufsize* は、組み込み関数
   :func:`open`  における対応する引数と同じ意味を持ちます。

   利用できる環境: Unix, Windows

   .. versionchanged:: 2.3
      引数 *mode* は、指定されるならば、
      ``'r'``, ``'w'``, ``'a'`` のいずれかの文字で始まらなければなりません。
      そうでなければ :exc:`ValueError` が送出されます.

   .. versionchanged:: 2.5
      Unixでは、引数 *mode* が ``'a'`` で始まる時には
      *O_APPEND* フラグがファイル記述子に設定されます。
      (ほとんどのプラットフォームで :c:func:`fdopen` 実装が既に行なっていることです).


.. function:: popen(command[, mode[, bufsize]])

   *command* への、または *command* からのパイプ入出力を開きます。
   戻り値はパイプに接続されている開かれたファイルオブジェクトで、
   *mode* が ``'r'`` (標準の設定です) または ``'w'`` かによって
   読み出しまたは書き込みを行うことができます。
   引数 *bufsize* は、組み込み関数 :func:`open` における対応する引数と
   同じ意味を持ちます。
   *command* の終了ステータス (:func:`wait` で指定された書式でコード化されています)
   は、 :meth:`close` メソッドの戻り値として取得することができます。
   例外は終了ステータスがゼロ
   (すなわちエラーなしで終了) の場合で、このときには ``None`` を返します。

   利用できる環境: Unix, Windows

   .. deprecated:: 2.6
      この関数は撤廃されました。
      代わりに :mod:`subprocess` モジュールを利用してください。
      特に、 :ref:`subprocess-replacements` 節をチェックしてください。

   .. versionchanged:: 2.0
      この関数は、Pythonの初期のバージョンでは、
      Windows環境下で信頼できない動作をしていました。
      これはWindowsに付属して提供されるライブラリの
      :c:func:`_popen` 関数を利用したことによるものです。
      新しいバージョンの Python では、
      Windows 付属のライブラリにある壊れた実装を利用しません。


.. function:: tmpfile()

   更新モード(``w+b``)で開かれた新しいファイルオブジェクトを返します。
   このファイルはディレクトリエントリ登録に関連付けられておらず、
   このファイルに対するファイル記述子がなくなると自動的に削除されます。

   利用できる環境: Unix, Windows

.. There are a number of different :func:`popen\*` functions that provide slightly
   different ways to create subprocesses.

幾つかの少し異なった方法で子プロセスを作成するために、幾つかの :func:`popen\*` 関数が提供されています。

.. deprecated:: 2.6
   全ての :func:`popen\*` 関数は撤廃されました。
   代わりに :mod:`subprocess` モジュールを利用してください。

:func:`popen\*` の変種はどれも、 *bufsize* が指定されている場合には
I/O パイプのバッファサイズを表します。 *mode* を指定する場合には、
文字列 ``'b'`` または ``'t'`` でなければなりません;
これは、Windows でファイルをバイナリモードで開くか
テキストモードで開くかを決めるために必要です。
*mode* の標準の設定値は ``'t'`` です。

また Unix ではこれらの変種はいずれも *cmd* をシーケンスにできます。
その場合、引数はシェルの介在なしに直接 (:func:`os.spawnv` のように) 渡されます。
*cmd* が文字列の場合、引数は( :func:`os.system` のように) シェルに渡されます。

.. These methods do not make it possible to retrieve the exit status from the child
   processes.  The only way to control the input and output streams and also
   retrieve the return codes is to use the :mod:`subprocess` module; these are only
   available on Unix.

以下のメソッドは子プロセスから終了ステータスを取得できるようにはしていません。
入出力ストリームを制御し、かつ終了コードの取得も行える唯一の方法は、
:mod:`subprocess` モジュールを利用する事です。
以下のメソッドはUnixでのみ利用可能です。

これらの関数の利用に関係して起きうるデッドロック状態についての議論は、
:ref:`popen2-flow-control` 節を参照してください。


.. function:: popen2(cmd[, mode[, bufsize]])

   *cmd* を子プロセスとして実行します。ファイル・オブジェクト ``(child_stdin, child_stdout)`` を返します。

   .. deprecated:: 2.6
      この関数は撤廃されました。 :mod:`subprocess` モジュールを利用してください。
      特に、 :ref:`subprocess-replacements` 節を参照してください。

   利用できる環境: Unix, Windows

   .. versionadded:: 2.0


.. function:: popen3(cmd[, mode[, bufsize]])

   *cmd* を子プロセスとして実行します。ファイルオブジェクト  ``(child_stdin, child_stdout, child_stderr)`` を
   返します。

   .. deprecated:: 2.6
      この関数は撤廃されました。 :mod:`subprocess` モジュールを利用してください。
      特に、 :ref:`subprocess-replacements` 節を参照してください。

   利用できる環境: Unix, Windows

   .. versionadded:: 2.0


.. function:: popen4(cmd[, mode[, bufsize]])

   *cmd* を子プロセスとして実行します。ファイルオブジェクト ``(child_stdin, child_stdout_and_stderr)``
   を返します。

   .. deprecated:: 2.6
      この関数は撤廃されました。 :mod:`subprocess` モジュールを利用してください。
      特に、 :ref:`subprocess-replacements` 節を参照してください。

   利用できる環境: Unix, Windows

   .. versionadded:: 2.0

(``child_stdin, child_stdout, および child_stderr`` は子プロセスの視点で名付けられているので注意してください。
すなわち、 *child_stdin* とは子プロセスの標準入力を意味します。)

この機能は :mod:`popen2` モジュール内の同じ名前の関数を使っても実現できますが、これらの関数の戻り値は異なる順序を持っています。


.. _os-fd-ops:

ファイル記述子の操作
--------------------

これらの関数は、ファイル記述子を使って参照されている I/Oストリームを操作します。

ファイル記述子とは現在のプロセスから開かれたファイルに対応する小さな整数です。
例えば、標準入力のファイル記述子はいつでも 0 で、標準出力は 1、標準エラーは 2 です。
その他にさらにプロセスから開かれたファイルには 3、4、5、などが割り振られます。
「ファイル記述子」という名前は少し誤解を与えるものかもしれませんが、
Unixプラットフォームにおいて、ソケットやパイプもファイル記述子によって参照されます。

ファイルオブジェクトに紐付けられたファイル記述子は :meth:`~file.fileno` 
メソッドによって取得可能です。
ただし、ファイル記述子を直接使うとファイルオブジェクトのメソッドは経由しませんので、
内部でバッファするかどうかといったファイルオブジェクトの都合は無視されます。

.. function:: close(fd)

   ファイルディスクリプタ *fd* を閉じます。

   利用できる環境: Unix、 Windows

   .. note::

      注:この関数は低レベルの I/O のためのもので、
      :func:`os.open` や :func:`pipe` が返すファイル記述子に対して
      適用しなければなりません。
      組み込み関数 :func:`open` や :func:`popen`, :func:`fdopen` の返す
      "ファイルオブジェクト" を閉じるには、
      オブジェクトの :meth:`~file.close` メソッドを使ってください。


.. function:: closerange(fd_low, fd_high)

   .. Close all file descriptors from *fd_low* (inclusive) to *fd_high* (exclusive),
      ignoring errors. Availability: Unix, Windows. Equivalent to

   *fd_low* (を含む) から *fd_high* (含まない) までの全てのディスクリプタを、
   エラーを無視しながら閉じます。
   次のコードと等価です::

      for fd in xrange(fd_low, fd_high):
          try:
              os.close(fd)
          except OSError:
              pass

   利用できる環境: Unix, Windows

   .. versionadded:: 2.6


.. function:: dup(fd)

   ファイル記述子 *fd* の複製を返します。

   利用できる環境: Unix, Windows

.. function:: dup2(fd, fd2)

   ファイル記述子を *fd* から *fd2* に複製し、
   必要なら後者の記述子を前もって閉じておきます。

   利用できる環境: Unix, Windows


.. function:: fchmod(fd, mode)

   .. Change the mode of the file given by *fd* to the numeric *mode*.  See the docs
      for :func:`chmod` for possible values of *mode*.  Availability: Unix.

   *fd* で指定されたファイルのモードを *mode* に変更します。
   *mode* に指定できる値については、 :func:`chmod` のドキュメントを参照してください。

   利用できる環境: Unix

   .. versionadded:: 2.6


.. function:: fchown(fd, uid, gid)

   .. Change the owner and group id of the file given by *fd* to the numeric *uid*
      and *gid*.  To leave one of the ids unchanged, set it to -1.
      Availability: Unix.

   *fd* で指定されたファイルの owner id と group id を、
   *uid* と *gid* に変更します。
   どちらかの id を変更しない場合は、 -1 を渡してください。

   利用できる環境: Unix

   .. versionadded:: 2.6

.. function:: fdatasync(fd)

   ファイル記述子 *fd* を持つファイルのディスクへの書き込みを強制します。
   メタデータの更新は強制しません。

   利用できる環境: Unix


.. function:: fpathconf(fd, name)

   開いているファイルに関連したシステム設定情報 (system configuration information)
   を返します。
   *name* には取得したい設定名を指定します;
   これは定義済みのシステム固有値名の文字列で、多くの標準
   (POSIX.1、 Unix 95、 Unix 98 その他) で定義されています。
   プラットフォームによっては別の名前も定義しています。
   ホストオペレーティングシステムの関知する名前は ``pathconf_names``
   辞書で与えられています。
   このマップオブジェクトに入っていない設定変数については、
   *name* に整数を渡してもかまいません。

   もし *name* が文字列でかつ不明である場合、 :exc:`ValueError` を送出します。
   *name* の指定値がホストシステムでサポートされておらず、
   ``pathconf_names`` にも入っていない場合、 :const:`errno.EINVAL`
   をエラー番号として :exc:`OSError` を送出します。

   利用できる環境: Unix

.. function:: fstat(fd)

   :func:`~os.stat` のようにファイル記述子 *fd* の状態を返します。

   利用できる環境: Unix, Windows


.. function:: fstatvfs(fd)

   :func:`statvfs` のように、ファイル記述子
   *fd* に関連づけられたファイルが入っているファイルシステムに関する情報を返します。

   利用できる環境: Unix


.. function:: fsync(fd)

   ファイル記述子 *fd* を持つファイルのディスクへの書き込みを強制します。
   Unix では、ネイティブの :c:func:`fsync` 関数を、Windows
   では MS :c:func:`_commit` 関数を呼び出します。

   Python のファイルオブジェクト *f* を使う場合、
   *f* の内部バッファを確実にディスクに書き込むために、まず ``f.flush()`` を実行し、
   それから ``os.fsync(f.fileno())`` してください。

   利用できる環境: Unix, Windows (2.2.3 以降)


.. function:: ftruncate(fd, length)

   ファイル記述子 *fd* に対応するファイルを、
   サイズが最大で *length* バイトになるように切り詰めます。

   利用できる環境: Unix


.. function:: isatty(fd)

   ファイル記述子 *fd* が開いていて、tty(のような)装置に接続されている場合、
   ``1`` を返します。そうでない場合は ``0`` を返します。

   利用できる環境: Unix


.. function:: lseek(fd, pos, how)

   ファイル記述子 *fd* の現在の位置を *pos* に設定します。
   *pos* の意味は *how* で修飾されます:
   ファイルの先頭からの相対には :const:`SEEK_SET` か ``0`` を設定します;
   現在の位置からの相対には :const:`SEEK_CUR` か ``1`` を設定します;
   ファイルの末尾からの相対には :const:`SEEK_END` か ``2`` を設定します。

   利用できる環境: Unix, Windows

.. data:: SEEK_SET
          SEEK_CUR
          SEEK_END

   :func:`lseek` 関数に渡すパラメータ。
   値は順に 0, 1, 2 です。

   利用できる環境: Unix, Windows

   .. versionadded:: 2.5

.. function:: open(file, flags[, mode])

   ファイル *file* を開き、 *flag* に従って様々なフラグを設定し、
   可能なら *mode* に従ってファイルモードを設定します。
   *mode* の標準の設定値は ``0777`` (8進表現) で、
   先に現在の umask を使ってマスクを掛けます。
   新たに開かれたファイルのファイル記述子を返します。

   フラグとファイルモードの値についての詳細は C
   ランタイムのドキュメントを参照してください;
   (:const:`O_RDONLY` や :const:`O_WRONLY` のような)
   フラグ定数はこのモジュールでも定義されています (以下を参照してください)。
   特に、Windows ではバイナリファイルを開くときに :const:`O_BINARY` 
   を加える必要があります。

   利用できる環境: Unix, Windows

   .. note::

      この関数は低レベルの I/O のためのものです。
      通常の利用では、 :meth:`~file.read` や :meth:`~file.write` (やその他多くの)
      メソッドを持つ「ファイルオブジェクト」を返す、
      組み込み関数 :func:`open` を使ってください。
      ファイル記述子を「ファイルオブジェクト」でラップするには
      :func:`fdopen` を使ってください。


.. function:: openpty()

   .. index:: module: pty

   新しい擬似端末のペアを開きます。
   ファイル記述子のペア ``(master, slave)`` を返し、
   それぞれ pty および tty を表します。
   (少しだけ) より可搬性のあるアプローチとしては、
   :mod:`pty` モジュールを使ってください。

   利用できる環境: いくつかの Unix 系システム


.. function:: pipe()

   パイプを作成します。
   ファイル記述子のペア ``(r, w)``  を返し、
   それぞれ読み出し、書き込み用に使うことができます。

   利用できる環境: Unix, Windows


.. function:: read(fd, n)

   ファイル記述子 *fd* から最大で *n* バイト読み出します。
   読み出されたバイト列の入った文字列を返します。
   *fd* が参照しているファイルの終端に達した場合、空の文字列が返されます。

   利用できる環境: Unix, Windows

   .. note::

      この関数は低レベルの I/O のためのもので、 :func:`os.open` や :func:`pipe`
      が返すファイル記述子に対して適用しなければなりません。
      組み込み関数 :func:`open` や :func:`popen`, :func:`fdopen` の返す
      "ファイルオブジェクト"、あるいは :data:`sys.stdin` から読み出すには、
      オブジェクトの
      :meth:`~file.read` か :meth:`~file.readline` メソッドを使ってください。


.. function:: tcgetpgrp(fd)

   *fd* (:func:`open` が返す開かれたファイル記述子)
   で与えられる端末に関連付けられたプロセスグループを返します。

   利用できる環境: Unix


.. function:: tcsetpgrp(fd, pg)

   *fd* (:func:`open` が返す開かれたファイル記述子)
   で与えられる端末に関連付けられたプロセスグループを *pg* に設定します。

   利用できる環境: Unix


.. function:: ttyname(fd)

   ファイル記述子 *fd* に関連付けられている端末デバイスを特定する文字列を返します。
   *fd* が端末に関連付けられていない場合、例外が送出されます。

   利用できる環境: Unix


.. function:: write(fd, str)

   ファイル記述子 *fd* に文字列 *str* を書き込みます。
   実際に書き込まれたバイト数を返します。

   利用できる環境: Unix, Windows

   .. note::

      この関数は低レベルの I/O のためのもので、 :func:`os.open` や :func:`pipe`
      が返すファイル記述子に対して適用しなければなりません。
      組み込み関数 :func:`open` や :func:`popen`, :func:`fdopen` の返す
      "ファイルオブジェクト"、あるいは :data:`sys.stdout`, :data:`sys.stderr`
      に書き込むには、オブジェクトの :meth:`~file.write`
      メソッドを使ってください。

``open()`` フラグ定数
~~~~~~~~~~~~~~~~~~~~~

.. The following constants are options for the *flags* parameter to the
   :func:`open` function.  They can be combined using the bitwise OR operator
   ``|``.  Some of them are not available on all platforms.  For descriptions of
   their availability and use, consult the :manpage:`open(2)` manual page on Unix
   or `the MSDN <http://msdn.microsoft.com/en-us/library/z0kc8e3z.aspx>` on Windows.

以下の定数は :func:`~os.open` 関数の *flags* 引数に利用します。
これらの定数は、ビット単位OR ``|`` で組み合わせることができます。
幾つかの定数は、全てのプラットフォームで使えるわけではありません。
利用可能かどうかや使い方については、 Unix では :manpage:`open(2)`, Windows
では `MSDN <http://msdn.microsoft.com/en-us/library/z0kc8e3z.aspx>`_
を参照してください。


.. data:: O_RDONLY
          O_WRONLY
          O_RDWR
          O_APPEND
          O_CREAT
          O_EXCL
          O_TRUNC

   利用できる環境: Unix, Windows


.. data:: O_DSYNC
          O_RSYNC
          O_SYNC
          O_NDELAY
          O_NONBLOCK
          O_NOCTTY
          O_SHLOCK
          O_EXLOCK

   利用できる環境: Unix


.. data:: O_BINARY
          O_NOINHERIT
          O_SHORT_LIVED
          O_TEMPORARY
          O_RANDOM
          O_SEQUENTIAL
          O_TEXT

   利用できる環境: Windows

.. data:: O_ASYNC
          O_DIRECT
          O_DIRECTORY
          O_NOFOLLOW
          O_NOATIME

   .. These constants are GNU extensions and not present if they are not defined by
      the C library.

   これらの定数は GNU 拡張で、Cライブラリで定義されていない場合は利用できません。


.. _os-file-dir:

ファイルとディレクトリ
----------------------

.. function:: access(path, mode)

   実 uid/gid を使って *path* に対するアクセスが可能か調べます。
   ほとんどのオペレーティングシステムは実効 uid/gid を使うため、
   このルーチンは suid/sgid 環境において、プログラムを起動したユーザが
   *path* に対するアクセス権をもっているかを調べるために使われます。
   *path* が存在するかどうかを調べるには *mode* を :const:`F_OK` にします。
   ファイル操作許可 (permission) を調べるために
   :const:`R_OK`, :const:`W_OK`, :const:`X_OK`
   から一つまたはそれ以上のフラグと OR をとることもできます。
   アクセスが許可されている場合 ``True`` を、そうでない場合 ``False`` を返します。
   詳細は :manpage:`access(2)` のマニュアルページを参照してください。

   利用できる環境: Unix, Windows

   .. note::

      :func:`access` を使ってユーザーが例えばファイルを開く権限を持っているか
      :func:`open` を使って実際にそうする前に調べることは
      セキュリティ・ホールを作り出してしまいます。
      というのは、調べる時点と開く時点の時間差を利用して
      そのユーザーがファイルを操作してしまうかもしれないからです。
      :term:`EAFP` テクニックを利用する方が好ましいです。例えば::

         if os.access("myfile", os.R_OK):
             with open("myfile") as fp:
                 return fp.read()
         return "some default data"

      このコードは次のように書いたほうが良いです::

         try:
             fp = open("myfile")
         except IOError as e:
             if e.errno == errno.EACCESS:
                 return "some default data"
             # Not a permission error.
             raise
         else:
             with fp:
                 return fp.read()

   .. note::

      I/O 操作は :func:`access` が成功を思わせるときにも失敗することがありえます。
      特にネットワーク・ファイルシステムにおける操作が通常の POSIX 
      許可ビット・モデルをはみ出す意味論を備える場合にはそのようなことが起こりえます。


.. data:: F_OK

   :func:`access` の *mode* に渡すための値で、 *path* が存在するかどうかを調べます。


.. data:: R_OK

   :func:`access` の *mode* に渡すための値で、 *path* が読み出し可能かどうかを調べます。


.. data:: W_OK

   :func:`access` の *mode* に渡すための値で、 *path* が書き込み可能かどうかを調べます。


.. data:: X_OK

   :func:`access` の *mode* に渡すための値で、 *path* が実行可能かどうかを調べます。


.. function:: chdir(path)

   .. index:: single: directory; changing

   現在の作業ディレクトリ (current working directory) を *path* に設定します。

   利用できる環境: Unix, Windows


.. function:: getcwd()

   現在の作業ディレクトリを表現する文字列を返します。

   利用できる環境: Unix, Windows


.. function:: getcwdu()

   現在の作業ディレクトリを表現するユニコードオブジェクトを返します。

   利用できる環境: Unix、 Windows

   .. versionadded:: 2.3


.. function:: chflags(path, flags)

   .. Set the flags of *path* to the numeric *flags*. *flags* may take a combination
      (bitwise OR) of the following values (as defined in the :mod:`stat` module):

   *path* のフラグを *flags* に変更する。
   *flags* は、以下の値を(bitwise ORで)組み合わせたものです。
   (:mod:`stat` モジュールを参照してください):

   * :data:`stat.UF_NODUMP`
   * :data:`stat.UF_IMMUTABLE`
   * :data:`stat.UF_APPEND`
   * :data:`stat.UF_OPAQUE`
   * :data:`stat.UF_NOUNLINK`
   * :data:`stat.SF_ARCHIVED`
   * :data:`stat.SF_IMMUTABLE`
   * :data:`stat.SF_APPEND`
   * :data:`stat.SF_NOUNLINK`
   * :data:`stat.SF_SNAPSHOT`

   利用できる環境: Unix

   .. versionadded:: 2.6


.. function:: chroot(path)

   現在のプロセスに対してルートディレクトリを *path* に変更します。

   利用できる環境: Unix

   .. versionadded:: 2.2


.. function:: chmod(path, mode)

   *path* のモードを数値 *mode* に変更します。
   *mode* は、(:mod:`stat` モジュールで定義されている)
   以下の値のいずれかまたはビット単位の OR で組み合わせた値を取り得ます:

   * :data:`stat.S_ISUID`
   * :data:`stat.S_ISGID`
   * :data:`stat.S_ENFMT`
   * :data:`stat.S_ISVTX`
   * :data:`stat.S_IREAD`
   * :data:`stat.S_IWRITE`
   * :data:`stat.S_IEXEC`
   * :data:`stat.S_IRWXU`
   * :data:`stat.S_IRUSR`
   * :data:`stat.S_IWUSR`
   * :data:`stat.S_IXUSR`
   * :data:`stat.S_IRWXG`
   * :data:`stat.S_IRGRP`
   * :data:`stat.S_IWGRP`
   * :data:`stat.S_IXGRP`
   * :data:`stat.S_IRWXO`
   * :data:`stat.S_IROTH`
   * :data:`stat.S_IWOTH`
   * :data:`stat.S_IXOTH`

   利用できる環境: Unix、 Windows

   .. note::

      Windows でも :func:`chmod` はサポートされていますが、
      ファイルの読み込み専用フラグを (定数 ``S_IWRITE`` と ``S_IREAD``,
      または対応する整数値を通して) 設定できるだけです。他のビットは全て無視されます。


.. function:: chown(path, uid, gid)

   *path* の所有者 (owner) id とグループ id を、
   数値 *uid* および *gid* に変更します。
   いずれかの id を変更せずにおくには、その値として -1 をセットします。

   利用できる環境: Unix


.. function:: lchflags(path, flags)

   .. Set the flags of *path* to the numeric *flags*, like :func:`chflags`, but do not
      follow symbolic links. Availability: Unix.

   *path* のフラグを数値 *flags* に設定します。
   :func:`chflags` に似ていますが、シンボリックリンクをたどりません。

   利用できる環境: Unix

   .. versionadded:: 2.6


.. function:: lchown(path, uid, gid)

   *path* の所有者 (owner) id とグループ id を、数値 *uid* および *gid* に変更します。
   この関数はシンボリックリンクをたどりません。

   利用できる環境: Unix

   .. versionadded:: 2.3


.. function:: link(source, link_name)

   *source* を指しているハードリンク *link_name* を作成します。

   利用できる環境: Unix


.. function:: listdir(path)

   *path* で指定されたディレクトリ内のエントリ名が入ったリストを返します。
   リスト内の順番は不定です。特殊エントリ ``'.'`` および ``'..'``
   は、それらがディレクトリに入っていてもリストには含められません。

   利用できる環境: Unix、 Windows

   .. versionchanged:: 2.3
      Windows NT/2k/XP と Unixでは、 *path* が Unicode オブジェクトの場合、
      Unicode オブジェクトのリストが返されます。
      デコード不可能なファイル名は依然として string オブジェクトになります。


.. function:: lstat(path)

   与えられた *path* に対して、 :c:func:`lstat` システムコールと同じ
   動作をします。
   :func:`~os.stat` に似ていますが、シンボリックリンクをたどりません。
   シンボリックリンクのない環境では :func:`~os.stat` の別名です。


.. function:: mkfifo(path[, mode])

   数値で指定されたモード *mode* を持つ FIFO (名前付きパイプ) を *path* に作成します。
   *mode* の標準の値は ``0666`` (8進) です。
   現在の umask 値が前もって *mode* からマスクされます。

   利用できる環境: Unix

   FIFO は通常のファイルのようにアクセスできるパイプです。
   FIFO は (例えば :func:`os.unlink` を使って) 削除されるまで存在しつづけます。
   一般的に、FIFO は "クライアント" と "サーバ" 形式のプロセス間で
   ランデブーを行うために使われます:
   このとき、サーバは FIFO を読み出し用に開き、クライアントは書き込み用に開きます。
   :func:`mkfifo` は FIFO を開かない --- 単にランデブーポイントを作成するだけ
   --- なので注意してください。


.. function:: mknod(filename[, mode=0600, device])

   *filename* という名前で、ファイルシステム・ノード (ファイル、
   デバイス特殊ファイル、または、名前つきパイプ) を作ります。
   *mode* は、作ろうとするノードの使用権限とタイプを、
   ``stat.S_IFREG``, ``stat.S_IFCHR``, ``stat.S_IFBLK``, ``stat.S_IFIFO``
   (これらの定数は :mod:`stat` で使用可能)
   のいずれかと（ビット OR で）組み合わせて指定します。
   ``S_IFCHR`` と ``S_IFBLK`` を指定すると、 *device* 
   は新しく作られたデバイス特殊ファイルを (おそらく :func:`os.makedev` を使って)
   定義し、指定しなかった場合には無視します。

   .. versionadded:: 2.3


.. function:: major(device)

   生のデバイス番号から、デバイスのメジャー番号を取り出します。
   (たいてい :c:type:`stat` の :attr:`st_dev` フィールドか
   :attr:`st_rdev` フィールドです)

   .. versionadded:: 2.3


.. function:: minor(device)

   生のデバイス番号から、デバイスのマイナー番号を取り出します。
   (たいてい :c:type:`stat` の :attr:`st_dev` フィールドか
   :attr:`st_rdev` フィールドです)

   .. versionadded:: 2.3


.. function:: makedev(major, minor)

   major と minor から、新しく生のデバイス番号を作ります。

   .. versionadded:: 2.3


.. function:: mkdir(path[, mode])

   数値で指定されたモード *mode* をもつディレクトリ *path*  を作成します。
   *mode* の標準の値は ``0777`` (8進)です。
   システムによっては、 *mode* は無視されます。
   利用の際には、現在の umask 値が前もってマスクされます。
   指定されたディレクトリがすでに存在する場合は :exc:`OSError` 例外を発生
   させます。

   一時ディレクトリを作成することもできます:
   :mod:`tempfile` モジュールの :func:`tempfile.mkdtemp`
   関数を参照してください。

   利用できる環境: Unix, Windows


.. function:: makedirs(path[, mode])

   .. index::
      single: directory; creating
      single: UNC paths; and os.makedirs()

   再帰的なディレクトリ作成関数です。
   :func:`mkdir` に似ていますが、
   末端 (leaf) となるディレクトリを作成するために必要な
   中間の全てのディレクトリを作成します。
   末端ディレクトリがすでに存在する場合や、作成ができなかった場合には :exc:`error`
   例外を送出します。 *mode* の標準の値は ``0777`` (8進)です。
   システムによっては、 *mode* は無視されます。
   利用の際には、現在の umask 値が前もってマスクされます。

   .. note::

      :func:`makedirs` は作り出すパス要素が
      :data:`os.pardir` を含むと混乱することになります。

   .. versionadded:: 1.5.2

   .. versionchanged:: 2.3
      この関数は UNC パスを正しく扱えるようになりました.


.. function:: pathconf(path, name)

   指定されたファイルに関係するシステム設定情報を返します。
   *name* には取得したい設定名を指定します;
   これは定義済みのシステム固有値名の文字列で、
   多くの標準 (POSIX.1、 Unix 95、 Unix 98 その他) で定義されています。
   プラットフォームによっては別の名前も定義しています。
   ホストオペレーティングシステムの関知する名前は ``pathconf_names``
   辞書で与えられています。
   このマップ型オブジェクトに入っていない設定変数については、
   *name* に整数を渡してもかまいません。

   もし *name* が文字列でかつ不明である場合、 :exc:`ValueError` を送出します。
   *name* の指定値がホストシステムでサポートされておらず、
   ``pathconf_names`` にも入っていない場合、 :const:`errno.EINVAL`
   をエラー番号として :exc:`OSError` を送出します。

   利用できる環境: Unix

.. data:: pathconf_names

   :func:`pathconf` および :func:`fpathconf` が受理するシステム設定名を、
   ホストオペレーティングシステムで定義されている整数値に対応付けている辞書です。
   この辞書はシステムでどの設定名が定義されているかを決定するために利用できます。

   利用できる環境: Unix


.. function:: readlink(path)

   シンボリックリンクが指しているパスを表す文字列を返します。
   返される値は絶対パスにも、相対パスにもなり得ます; 相対パスの場合、
   ``os.path.join(os.path.dirname(path), result)`` 
   を使って絶対パスに変換することができます。

   .. versionchanged:: 2.6

      *path* が unicode オブジェクトだった場合、戻り値も unicode オブジェクトになります。

   利用できる環境: Unix


.. function:: remove(path)

   ファイル *path* を削除(消去)します。
   *path* がディレクトリの場合、 :exc:`OSError` が送出されます;
   ディレクトリの削除については :func:`rmdir` を参照してください。
   この関数は下で述べられている :func:`unlink` 関数と同一です。
   Windows では、使用中のファイルを削除しようと試みると例外を送出します;
   Unixでは、ディレクトリエントリは削除されますが、記憶装置上にアロケーションされた
   ファイル領域は元のファイルが使われなくなるまで残されます。

   利用できる環境: Unix, Windows


.. function:: removedirs(path)

   .. index:: single: directory; deleting

   再帰的なディレクトリ削除関数です。
   :func:`rmdir` と同じように動作しますが、末端ディレクトリがうまく削除できるかぎり、
   :func:`removedirs` は *path* に現れる親ディレクトリをエラーが送出されるまで
   (このエラーは通常、指定したディレクトリの親ディレクトリが空でないことを
   意味するだけなので無視されます) 順に削除することを試みます。
   例えば、 ``os.removedirs('foo/bar/baz')`` では最初にディレクトリ
   ``'foo/bar/baz'`` を削除し、次に ``'foo/bar'``
   さらに ``'foo'`` をそれらが空ならば削除します。
   末端のディレクトリが削除できなかった場合には :exc:`OSError` が送出されます。

   .. versionadded:: 1.5.2


.. function:: rename(src, dst)

   ファイルまたはディレクトリ *src* を *dst* に名前変更します。
   *dst* がディレクトリの場合、 :exc:`OSError` が送出されます。
   Unixでは、 *dst* が存在し、かつファイルの場合、
   ユーザの権限があるかぎり暗黙のうちに元のファイルが置き換えられます。
   この操作はいくつかの Unix 系システムにおいて、
   *src* と *dst* が異なるファイルシステム上にあると失敗することがあります。
   ファイル名の変更が成功する場合、この操作は原子的
   (atomic) 操作となります (これは POSIX 要求仕様です)。
   Windows では、 *dst* が既に存在する場合には、たとえファイルの場合でも
   :exc:`OSError` が送出されます; 
   これは *dst* が既に存在するファイル名の場合、
   名前変更の原子的操作を実装する手段がないからです。

   利用できる環境: Unix, Windows


.. function:: renames(old, new)

   再帰的にディレクトリやファイル名を変更する関数です。
   :func:`rename` のように動作しますが、新たなパス名を持つファイルを配置するために
   必要な途中のディレクトリ構造をまず作成しようと試みます。
   名前変更の後、元のファイル名のパス要素は
   :func:`removedirs` を使って右側から順に枝刈りされてゆきます。

   .. versionadded:: 1.5.2

   .. note::

      この関数はコピー元の末端のディレクトリまたはファイルを削除する権限がない場合には失敗します。


.. function:: rmdir(path)

   ディレクトリ *path* を削除します。
   ディレクトリが空の場合にだけ正常に動作します。
   そうでなければ :exc:`OSError` が送出されます。
   ディレクトリ・ツリー全体を削除するのには :func:`shutil.rmtree` が使えます。

   利用できる環境: Unix, Windows


.. function:: stat(path)

   与えられた *path* に対して :c:func:`stat` システムコール相当の処理を実行します。
   (この関数はシンボリックリンクをたどります。シンボリックリンクに対して stat
   したい場合は :func:`lstat` を利用してください)

   戻り値はオブジェクトで、その属性は :c:type:`stat` 構造体に従います:

   * :attr:`st_mode` - 保護 bits,
   * :attr:`st_ino` - inode 番号,
   * :attr:`st_dev` - デバイス,
   * :attr:`st_nlink` - ハードリンク数,
   * :attr:`st_uid` - オーナーの uid,
   * :attr:`st_gid` - オーナーの group id,
   * :attr:`st_size` - ファイルのサイズ(単位: byte),
   * :attr:`st_atime` - 最近にアクセスされた時間,
   * :attr:`st_mtime` - 最近に内容を変更した時間,
   * :attr:`st_ctime` - プラットフォーム依存; Unix では最近のメタデータ変更時間、
     Windows ではファイルが生成された時間

   .. versionchanged:: 2.3
      もし :func:`stat_float_times`
      が ``True`` を返す場合、時間値は浮動小数点で秒を計ります。
      ファイルシステムがサポートしていれば、秒の小数点以下の桁も含めて返されます。
      Mac OS では、時間は常に浮動小数点です。
      詳細な説明は :func:`stat_float_times` を参照してください。

   (Linux のような) いくつかの Unix システムでは、以下の属性が利用できるかもしれません:

   * :attr:`st_blocks`  - ファイル用にアロケーションされているブロック数,
   * :attr:`st_blksize` - ファイルシステムのブロックサイズ,
   * :attr:`st_rdev`    - i ノードデバイスの場合、デバイスの形式,
   * :attr:`st_flags`   - ファイルに対するユーザー定義のフラグ

   他の (FreeBSD のような) Unix システムでは、以下の属性が利用できる場合があります
   (ただし root がそれらを使うことにした場合以外は値が入っていないでしょう):

   * :attr:`st_gen` - ファイル生成番号,
   * :attr:`st_birthtime` - ファイル生成時刻

   Mac OS システムでは、以下の属性も利用可能なときがあります:

   * :attr:`st_rsize`,
   * :attr:`st_creator`,
   * :attr:`st_type`,

   RISCOS システムでは、以下の属性も利用できます:

   * :attr:`st_ftype` (file type),
   * :attr:`st_attrs` (attributes),
   * :attr:`st_obtype` (object type)

   .. note::

      :attr:`st_atime`, :attr:`st_mtime`, および :attr:`st_ctime`
      メンバの厳密な意味や精度はオペレーティングシステムや
      ファイルシステムによって変わります。
      例えば、FAT や FAT32 ファイルシステムを使っている Windows システムでは、
      :attr:`st_atime` の精度は 1 日に過ぎません。
      詳しくはお使いのオペレーティングシステムのドキュメントを参照してください。

.. x

   後方互換性のために、 :func:`stat` の戻り値は
   少なくとも 10 個の整数からなるタプルとしてアクセスすることができます。
   このタプルはもっとも重要な (かつ可搬性のある) :c:type:`stat` 
   構造体のメンバを与えており、以下の順番
   :attr:`st_mode`, :attr:`st_ino`,
   :attr:`st_dev`, :attr:`st_nlink`, :attr:`st_uid`, :attr:`st_gid`,
   :attr:`st_size`, :attr:`st_atime`, :attr:`st_mtime`, :attr:`st_ctime`,
   に並んでいます。

   .. index:: module: stat

   実装によっては、この後ろにさらに値が付け加えられていることもあります。
   Mac OS では、時刻の値は Mac OS の他の時刻表現値と同じように浮動小数点数
   なので注意してください。
   標準モジュール :mod:`stat` では、
   :c:type:`stat` 構造体から情報を引き出す上で便利な関数や定数を定義しています。
   (Windows では、いくつかのデータ要素はダミーの値が埋められています。)

   利用できる環境: Unix, Windows

   例::

      >>> import os
      >>> statinfo = os.stat('somefile.txt')
      >>> statinfo
      (33188, 422511, 769, 1, 1032, 100, 926, 1105022698,1105022732, 1105022732)
      >>> statinfo.st_size
      926

   .. versionchanged:: 2.2
      返されたオブジェクトの属性としてのアクセス機能を追加しました。

   .. versionchanged:: 2.5
      :attr:`st_gen`, :attr:`st_birthtime` を追加しました。


.. function:: stat_float_times([newvalue])

   :class:`stat_result` がタイムスタンプに浮動小数点オブジェクトを
   使うかどうかを決定します。
   *newvalue* が ``True`` の場合、以後の :func:`~os.stat` 呼び出しは
   浮動小数点を返し、 ``False`` の場合には以後整数を返します。
   *newvalue* が省略された場合、現在の設定どおりの戻り値になります。

   古いバージョンの Python と互換性を保つため、 :class:`stat_result`
   にタプルとしてアクセスすると、常に整数が返されます。

   .. versionchanged:: 2.5
      Python はデフォルトで浮動小数点数を返すようになりました。浮動小数点数のタイムスタンプではうまく動かないアプリケーションはこの機能を利用して
      昔ながらの振る舞いを取り戻すことができます。

   タイムスタンプの精度 (すなわち最小の小数部分) はシステム依存です。システムによっては秒単位の精度しかサポートしません。
   そういったシステムでは小数部分は常に 0 です。

   この設定の変更は、プログラムの起動時に、 *__main__* モジュールの中でのみ行うことを推奨します。
   ライブラリは決して、この設定を変更するべきではありません。浮動小数点型のタイムスタンプを処理すると、不正確な動作をするようなライブ
   ラリを使う場合、ライブラリが修正されるまで、浮動小数点型を返す機能を停止させておくべきです。


.. function:: statvfs(path)

   与えられた *path* に対して :c:func:`statvfs` システムコールを実行します。
   戻り値はオブジェクトで、その属性は与えられたパスが収められている
   ファイルシステムについて記述したものです。
   各属性は :c:type:`statvfs` 構造体のメンバ: :attr:`f_bsize`,
   :attr:`f_frsize`, :attr:`f_blocks`, :attr:`f_bfree`, :attr:`f_bavail`,
   :attr:`f_files`, :attr:`f_ffree`, :attr:`f_favail`, :attr:`f_flag`,
   :attr:`f_namemax`,に対応します。

   .. index:: module: statvfs

   後方互換性のために、戻り値は上の順にそれぞれ対応する属性値が並んだタプルとしてアクセスすることもできます。標準モジュール :mod:`statvfs`
   では、シーケンスとしてアクセスする場合に、 :c:type:`statvfs` 構造体から情報を引き出す上便利な関数や定数を定義しています; これは
   属性として各フィールドにアクセスできないバージョンの Python で動作する必要のあるコードを書く際に便利です。

   利用できる環境: Unix

   .. versionchanged:: 2.2
      返されたオブジェクトの属性としてのアクセス機能を追加しました.


.. function:: symlink(source, link_name)

   *source* を指しているシンボリックリンクを *link_name* に作成します。

   利用できる環境: Unix


.. function:: tempnam([dir[, prefix]])

   一時ファイル (temporary file) を生成する上でファイル名として相応しい一意なパス名を返します。この値は一時的なディレクトリエントリ
   を表す絶対パスで、 *dir* ディレクトリの下か、 *dir* が省略されたり ``None`` の場合には一時ファイルを置くための共通の
   ディレクトリの下になります。 *prefix* が与えられており、かつ ``None`` でない場合、ファイル名の先頭につけられる短い
   接頭辞になります。アプリケーションは :func:`tempnam` が返したパス名を使って正しくファイルを生成し、生成したファイルを管理する責任があります;
   一時ファイルの自動消去機能は提供されていません。

   .. warning::

      :func:`tempnam` を使うと、symlink 攻撃に対して脆弱になります; 代りに :func:`tmpfile`
      (:ref:`os-newstreams`) を使うよう検討してください。

   利用できる環境: Unix、 Windows


.. function:: tmpnam()

   一時ファイル (temporary file) を生成する上でファイル名として相応しい一意なパス名を返します。この値は一時ファイルを置くための共通の
   ディレクトリ下の一時的なディレクトリエントリを表す絶対パスです。アプリケーションは :func:`tmpnam`
   が返したパス名を使って正しくファイルを生成し、生成したファイルを管理する責任があります; 一時ファイルの自動消去機能は提供されていません。

   .. warning::

      :func:`tmpnam` を使うと、symlink 攻撃に対して脆弱になります; 代りに :func:`tmpfile`
      (:ref:`os-newstreams`) を使うよう検討してください。

   利用できる環境: Unix, Windowsこの関数はおそらく Windows では使うべきではないでしょう; Micorosoft の
   :func:`tmpnam` 実装では、常に現在のドライブのルートディレクトリ下のファイル名を生成しますが、これは一般的には
   テンポラリファイルを置く場所としてはひどい場所です (アクセス権限によっては、この名前をつかってファイルを開くことすらできないかもしれません)。


.. data:: TMP_MAX

   :func:`tmpnam` がテンポラリ名を再利用し始めるまでに生成できる一意な名前の最大数です。


.. function:: unlink(path)

   ファイル *path* を削除します。
   :func:`remove` と同じです;  :func:`unlink` の名前は伝統的な Unix の関数名です。

   利用できる環境: Unix, Windows


.. function:: utime(path, times)

   *path* で指定されたファイルに最終アクセス時刻および最終修正時刻を設定します。
   *times* が ``None`` の場合、ファイルの最終アクセス時刻および最終更新時刻は現在の時刻になります。
   (この動作は、その *path* に対してUnixの :program:`touch` プログラムを実行するのに似ています)

   そうでない場合、 *times* は2要素のタプルで、 ``(atime, mtime)`` の形式をとらなくてはなりません。
   これらはそれぞれアクセス時刻および修正時刻を設定するために使われます。

   *path* にディレクトリを指定できるかどうかは、オペレーティングシステムがディレクトリをファイルの一種として実装しているかどうかに依存します (例えば、 Windows
   はそうではありません)。
   ここで設定した時刻の値は、オペレーティングシステムがアクセス時刻や
   更新時刻を記録する際の精度によっては、後で :func:`~os.stat`
   呼び出したときの値と同じにならないかも知れないので注意してください。
   :func:`~os.stat` も参照してください。

   .. versionchanged:: 2.0
      *times* として ``None`` をサポートするようにしました.

   利用できる環境: Unix, Windows


.. function:: walk(top[, topdown=True [, onerror=None[, followlinks=False]]])

   .. index::
      single: directory; walking
      single: directory; traversal

   ディレクトリツリー以下のファイル名を、
   ツリーをトップダウンもしくはボトムアップに走査することで生成します。
   ディレクトリ *top* を根に持つディレクトリツリーに含まれる、
   各ディレクトリ(*top* 自身を含む) から、タプル ``(dirpath, dirnames, filenames)``
   を生成します。

   *dirpath* は文字列で、ディレクトリへのパスです。
   *dirnames* は  *dirpath* 内のサブディレクトリ名のリスト (``'.'`` と
   ``'..'``  は除く）です。
   *filenames* は *dirpath* 内の非ディレクトリ・ファイル名のリストです。
   このリスト内の名前には、ファイル名までのパスが含まれないことに、注意してください。
   *dirpath* 内のファイルやディレクトリへの (*top* からたどった) フルパスを得るには、
   ``os.path.join(dirpath, name)`` してください。

   オプション引数 *topdown* が ``True`` であるか、指定されなかった場合、
   各ディレクトリからタプルを生成した後で、サブディレクトリからタプルを生成します。
   (ディレクトリはトップダウンで生成)。 
   *topdown* が ``False`` の場合、ディレクトリに対応するタプルは、
   そのディレクトリ以下の全てのサブディレクトリに対応するタプルの後で
   (ボトムアップで) 生成されます

   *topdown* が ``True`` のとき、呼び出し側は *dirnames* リストを、
   インプレースで (たとえば、 :keyword:`del` やスライスを使った代入で) 変更でき、
   :func:`walk` は *dirnames* に残っているサブディレクトリ内のみを再帰します。
   これにより、検索を省略したり、特定の訪問順序を強制したり、呼び出し側が
   :func:`walk` を再開する前に、呼び出し側が作った、
   または名前を変更したディレクトリを、 :func:`walk` に知らせたりすることができます。
   *topdown* が ``False`` のときに *dirnames* を変更しても効果はありません。
   ボトムアップモードでは *dirpath* 自身が生成される前に *dirnames*
   内のディレクトリの情報が生成されるからです。

   デフォルトでは、 :func:`os.listdir()` 呼び出しから送出されたエラーは無視されます。
   オプションの引数 *onerror* を指定するなら、この値は関数でなければなりません;
   この関数は単一の引数として、 :exc:`OSError` インスタンスを伴って呼び出されます。
   この関数ではエラーを報告して歩行を続けたり、例外を送出して歩行を中断したりできます。
   ファイル名は例外オブジェクトの ``filename`` 
   属性として取得できることに注意してください。

   .. By default, :func:`walk` will not walk down into symbolic links that resolve to
      directories. Set *followlinks* to ``True`` to visit directories pointed to by
      symlinks, on systems that support them.

   デフォルトでは、 :func:`walk` はディレクトリへのシンボリックリンクをたどりません。
   *followlinks* に ``True`` を設定すると、
   ディレクトリへのシンボリックリンクをサポートしているシステムでは、
   シンボリックリンクの指しているディレクトリを走査します。

   .. versionadded:: 2.6
      *followlinks* 引数

   .. note::

      *followlinks* を ``True`` に設定すると、
      シンボリックリンクが親ディレクトリを指していた場合に、
      無限ループになることに気をつけてください。
      :func:`walk` は、すでにたどったディレクトリを管理したりはしません。

   .. note::

      相対パスを渡した場合、 :func:`walk` の回復の間で
      カレント作業ディレクトリを変更しないでください。
      :func:`walk` はカレントディレクトリを変更しませんし、
      呼び出し側もカレントディレクトリを変更しないと仮定しています。

   以下の例では、最初のディレクトリ以下にある各ディレクトリに含まれる、
   非ディレクトリファイルのバイト数を表示します。ただし、CVS
   サブディレクトリより下を見に行きません。 ::

      import os
      from os.path import join, getsize
      for root, dirs, files in os.walk('python/Lib/email'):
          print root, "consumes",
          print sum(getsize(join(root, name)) for name in files),
          print "bytes in", len(files), "non-directory files"
          if 'CVS' in dirs:
              dirs.remove('CVS')  # don't visit CVS directories

   次の例では、ツリーをボトムアップで歩行することが不可欠になります;
   :func:`rmdir` はディレクトリが空になる前に削除させないからです::

      # Delete everything reachable from the directory named in "top",
      # assuming there are no symbolic links.
      # CAUTION:  This is dangerous!  For example, if top == '/', it
      # could delete all your disk files.
      import os
      for root, dirs, files in os.walk(top, topdown=False):
          for name in files:
              os.remove(os.path.join(root, name))
          for name in dirs:
              os.rmdir(os.path.join(root, name))

   .. versionadded:: 2.3


.. _os-process:

プロセス管理
------------

プロセスを生成したり管理するために、以下の関数を利用することができます。

様々な :func:`exec\*` 関数が、プロセス内にロードされた新たなプログラムに与えるための引数からなるリストをとります。どの場合でも、
新たなプログラムに渡されるリストの最初の引数は、ユーザがコマンドラインで入力する引数ではなく、プログラム自身の名前になります。 C
プログラマにとっては、これはプログラムの :c:func:`main` に渡される ``argv[0]`` になります。例えば、
``os.execv('/bin/echo', ['foo', 'bar'])`` は、標準出力に ``bar`` を出力します; ``foo``
は無視されたかのように見えることでしょう。


.. function:: abort()

   :const:`SIGABRT` シグナルを現在のプロセスに対して生成します。 Unixでは、標準設定の動作はコアダンプの生成です; Windows では、
   プロセスは即座に終了コード ``3`` を返します。 :func:`signal.signal` を使って :const:`SIGABRT` に対する
   シグナルハンドラを設定しているプログラムは異なる挙動を示すので注意してください。
   利用できる環境: Unix、 Windows


.. function:: execl(path, arg0, arg1, ...)
              execle(path, arg0, arg1, ..., env)
              execlp(file, arg0, arg1, ...)
              execlpe(file, arg0, arg1, ..., env)
              execv(path, args)
              execve(path, args, env)
              execvp(file, args)
              execvpe(file, args, env)

   これらの関数はすべて、現在のプロセスを置き換える形で新たなプログラムを実行します; 現在のプロセスは戻り値を返しません。
   Unixでは、新たに実行される実行コードは現在のプロセス内にロードされ、呼び出し側と同じプロセス ID を持つことになります。エラーは
   :exc:`OSError` 例外として報告されます。

   .. The current process is replaced immediately. Open file objects and
      descriptors are not flushed, so if there may be data buffered
      on these open files, you should flush them using
      :func:`sys.stdout.flush` or :func:`os.fsync` before calling an
      :func:`exec\*` function.

   現在のプロセスは瞬時に置き換えられます。
   開かれているファイルオブジェクトやディスクリプタはフラッシュされません。
   そのため、バッファ内にデータが残っているかもしれない場合、
   :func:`exec\*` 関数を実行する前に :func:`sys.stdout.flush` か :func:`os.fsync`
   を利用してバッファをフラッシュしておく必要があります。

   "l" および "v" のついた :func:`exec\*` 関数は、コマンドライン引数をどのように渡すかが異なります。 "l"
   型は、コードを書くときにパラメタ数が決まっている場合に、おそらくもっとも簡単に利用できます。個々のパラメタは単に :func:`execl\*`
   関数の追加パラメタとなります。 "v" 型は、パラメタの数が可変の時に便利で、リストかタプルの引数が *args*
   パラメタとして渡されます。
   どちらの場合も、子プロセスに渡す引数は動作させようとしているコマンドの名前から始めるべきですが、これは強制ではありません。

   末尾近くに "p" をもつ型 (:func:`execlp`, :func:`execlpe`, :func:`execvp`,および
   :func:`execvpe`) は、プログラム *file* を探すために環境変数 :envvar:`PATH` を利用します。環境変数が (次の段で述べる
   :func:`exec\*e` 型関数で) 置き換えられる場合、環境変数は :envvar:`PATH` を決定する上の情報源として使われます。
   その他の型、 :func:`execl`, :func:`execle`, :func:`execv`,および :func:`execve` では、実行
   コードを探すために :envvar:`PATH` を使いません。 *path* には適切に設定された絶対パスまたは相対パスが入っていなくてはなりません。

   :func:`execle`, :func:`execlpe`, :func:`execve`,および :func:`execvpe`
   (全て末尾に "e" がついていることに注意してください) では、 *env* パラメタは新たなプロセスで利用
   される環境変数を定義するためのマップ型でなくてはなりません(現在のプロセスの環境変数の代わりに利用されます);
   :func:`execl`, :func:`execlp`, :func:`execv`,および
   :func:`execvp` では、全て新たなプロセスは現在のプロセスの環境を引き継ぎます。

   利用できる環境: Unix, Windows


.. function:: _exit(n)

   終了ステータス *n* でプロセスを終了します。
   このときクリーンアップハンドラの呼び出しや、
   標準入出力バッファのフラッシュなどは行いません。

   利用できる環境: Unix, Windows

   .. note::

      システムを終了する標準的な方法は ``sys.exit(n)`` です。
      :func:`_exit` は通常、 :func:`fork` された後の子プロセスでのみ使われます。

以下の終了コードは必須ではありませんが :func:`_exit` と共に使うことができます。
一般に、メールサーバの外部コマンド配送プログラムのような、
Python で書かれたシステムプログラムに使います。

.. note::

   いくらかの違いがあって、これらの全てが全ての Unix プラットフォームで使えるわけではありません。以下の定数は基礎にあるプラットフォームで
   定義されていれば定義されます。


.. data:: EX_OK

   エラーが起きなかったことを表す終了コード。

   利用できる環境: Unix

   .. versionadded:: 2.3


.. data:: EX_USAGE

   誤った個数の引数が渡されたときなど、コマンドが間違って使われたことを表す終了コード。

   利用できる環境: Unix

   .. versionadded:: 2.3


.. data:: EX_DATAERR

   入力データが間違っていたことを表す終了コード。

   利用できる環境: Unix

   .. versionadded:: 2.3


.. data:: EX_NOINPUT

   入力ファイルが存在しなかった、または、読み込み不可だったことを表す終了コード。

   利用できる環境: Unix

   .. versionadded:: 2.3


.. data:: EX_NOUSER

   指定されたユーザが存在しなかったことを表す終了コード。

   利用できる環境: Unix

   .. versionadded:: 2.3


.. data:: EX_NOHOST

   指定されたホストが存在しなかったことを表す終了コード。

   利用できる環境: Unix

   .. versionadded:: 2.3


.. data:: EX_UNAVAILABLE

   要求されたサービスが利用できないことを表す終了コード。

   利用できる環境: Unix

   .. versionadded:: 2.3


.. data:: EX_SOFTWARE

   内部ソフトウェアエラーが検出されたことを表す終了コード。

   利用できる環境: Unix

   .. versionadded:: 2.3


.. data:: EX_OSERR

   fork できない、pipe の作成ができないなど、
   オペレーティング・システム・エラーが検出されたことを表す終了コード。

   利用できる環境: Unix

   .. versionadded:: 2.3


.. data:: EX_OSFILE

   システムファイルが存在しなかった、開けなかった、
   あるいはその他のエラーが起きたことを表す終了コード。

   利用できる環境: Unix

   .. versionadded:: 2.3


.. data:: EX_CANTCREAT

   ユーザには作成できない出力ファイルを指定したことを表す終了コード。

   利用できる環境: Unix

   .. versionadded:: 2.3


.. data:: EX_IOERR

   ファイルの I/O を行っている途中にエラーが発生したときの終了コード。

   利用できる環境: Unix

   .. versionadded:: 2.3


.. data:: EX_TEMPFAIL

   一時的な失敗が発生したことを表す終了コード。
   これは、再試行可能な操作の途中に、ネットワークに接続できないというような、
   実際にはエラーではないかも知れないことを意味します。

   利用できる環境: Unix

   .. versionadded:: 2.3


.. data:: EX_PROTOCOL

   プロトコル交換が不正、不適切、または理解不能なことを表す終了コード。

   利用できる環境: Unix

   .. versionadded:: 2.3


.. data:: EX_NOPERM

   操作を行うために十分な許可がなかった（ファイルシステムの問題を除く）ことを表す終了コード。

   利用できる環境: Unix

   .. versionadded:: 2.3


.. data:: EX_CONFIG

   設定エラーが起こったことを表す終了コード。

   利用できる環境: Unix

   .. versionadded:: 2.3


.. data:: EX_NOTFOUND

   "an entry was not found" のようなことを表す終了コード。

   利用できる環境: Unix

   .. versionadded:: 2.3


.. function:: fork()

   子プロセスを fork します。
   子プロセスでは ``0`` が返り、親プロセスでは子プロセスの id が返ります。
   エラーが発生した場合は、 :exc:`OSError` 例外を送出します。

   FreeBSD <= 6.3, Cygwin, OS/2 EMX を含む幾つかのプラットフォームにおいて、
   fork() をスレッド内から利用した場合に既知の問題があることに注意してください。

   利用できる環境: Unix


.. function:: forkpty()

   子プロセスを fork します。
   このとき新しい擬似端末 (psheudo-terminal) を子プロセスの制御端末として使います。
   親プロセスでは ``(pid, fd)`` からなるペアが返り、
   *fd* は擬似端末のマスタ側 (master end) のファイル記述子となります。
   可搬性のあるアプローチを取るためには、 :mod:`pty` モジュールを利用してください。
   エラーが発生した場合は、 :exc:`OSError` 例外を送出します。

   利用できる環境: いくつかの Unix系。


.. function:: kill(pid, sig)

   .. index::
      single: process; killing
      single: process; signalling

   プロセス *pid* にシグナル *sig* を送ります。
   ホストプラットフォームで利用可能なシグナルを特定する定数は :mod:`signal`
   モジュールで定義されています。

   Windows: :data:`signal.CTRL_C_EVENT` と :data:`signal.CTRL_BREAK_EVENT`
   は、同じコンソールウィンドウを共有しているコンソールプロセス(例: 子プロセス)
   にだけ送ることができる特別なシグナルです。
   その他の値を *sig* に与えると、そのプロセスが無条件に TerminateProcess API
   によって kill され、終了コードが *sig* に設定されます。
   Windows の :func:`kill` は kill するプロセスのハンドルも受け取ります。

   .. versionadded:: 2.7 Windows サポート


.. function:: killpg(pgid, sig)

   .. index::
      single: process; killing
      single: process; signalling

   プロセスグループ *pgid* にシグナル *sig* を送ります。

   利用できる環境: Unix

   .. versionadded:: 2.3


.. function:: nice(increment)

   プロセスの "nice 値" に *increment* を加えます。新たな nice 値を返します。

   利用できる環境: Unix


.. function:: plock(op)

   プログラムのセグメント (program segment) をメモリ内でロックします。
   *op* (``<sys/lock.h>`` で定義されています)
   にはどのセグメントをロックするかを指定します。

   利用できる環境: Unix


.. function:: popen(...)
              popen2(...)
              popen3(...)
              popen4(...)
   :noindex:

   子プロセスを起動し、子プロセスとの通信のために開かれたパイプを返します。これらの関数は :ref:`os-newstreams` 節で記述されています。


.. function:: spawnl(mode, path, ...)
              spawnle(mode, path, ..., env)
              spawnlp(mode, file, ...)
              spawnlpe(mode, file, ..., env)
              spawnv(mode, path, args)
              spawnve(mode, path, args, env)
              spawnvp(mode, file, args)
              spawnvpe(mode, file, args, env)

   新たなプロセス内でプログラム *path* を実行します。

   .. (Note that the :mod:`subprocess` module provides more powerful facilities for
      spawning new processes and retrieving their results; using that module is
      preferable to using these functions.  Check specially the *Replacing Older
      Functions with the subprocess Module* section in that documentation page.)

   (:mod:`subprocess` モジュールが、新しいプロセスを実行して結果を取得するための、
   より強力な機能を提供しています。
   この関数の代わりに、 :mod:`subprocess` モジュールを利用することが推奨されています。
   :mod:`subprocess` モジュールのドキュメントの、
   :ref:`subprocess-replacements`
   というセクションを読んでください。)

   *mode* が :const:`P_NOWAIT` の場合、この関数は新たなプロセスのプロセス
   ID となります。; *mode* が :const:`P_WAIT` の場合、
   子プロセスが正常に終了するとその終了コードが返ります。
   そうでない場合にはプロセスを kill したシグナル *signal* に対して
   ``-signal`` が返ります。
   Windows では、プロセス ID は実際にはプロセスハンドル値になります。

   "l" および "v" のついた :func:`spawn\*` 関数は、
   コマンドライン引数をどのように渡すかが異なります。
   "l" 型は、コードを書くときにパラメタ数が決まっている場合に、
   おそらくもっとも簡単に利用できます。
   個々のパラメタは単に :func:`spawnl\*` 関数の追加パラメタとなります。
   "v" 型は、パラメタの数が可変の時に便利で、リストかタプルの引数が *args*
   パラメタとして渡されます。
   どちらの場合も、子プロセスに渡す引数は動作させようとしているコマンドの名前から始まらなくてはなりません。

   末尾近くに "p" をもつ型 (:func:`spawnlp`, :func:`spawnlpe`, :func:`spawnvp`,
   :func:`spawnvpe`) は、プログラム *file* を探すために環境変数 :envvar:`PATH` 
   を利用します。
   環境変数が (次の段で述べる :func:`spawn\*e` 型関数で) 置き換えられる場合、
   環境変数は :envvar:`PATH` を決定する上の情報源として使われます。
   その他の型、 :func:`spawnl`, :func:`spawnle`, :func:`spawnv`,および
   :func:`spawnve` では、実行コードを探すために :envvar:`PATH` を使いません。 *path*
   には適切に設定された絶対パスまたは相対パスが入っていなくてはなりません。

   :func:`spawnle`, :func:`spawnlpe`, :func:`spawnve`,および :func:`spawnvpe`
   (全て末尾に "e" がついていることに注意してください) では、 *env* 
   パラメタは新たなプロセスで利用される環境変数を定義するための
   マップ型でなくてはなりません; :func:`spawnl`, :func:`spawnlp`, :func:`spawnv`,
   および :func:`spawnvp` では、全て新たなプロセスは現在のプロセスの環境を引き継ぎます。
   *env* 辞書のキーと値は全て文字列である必要があります。
   不正なキーや値を与えると関数が失敗し、 ``127`` を返します。

   例えば、以下の :func:`spawnlp` および :func:`spawnvpe`  呼び出し::

      import os
      os.spawnlp(os.P_WAIT, 'cp', 'cp', 'index.html', '/dev/null')

      L = ['cp', 'index.html', '/dev/null']
      os.spawnvpe(os.P_WAIT, 'cp', L, os.environ)

   は等価です。

   利用できる環境: Unix, Windows

   :func:`spawnlp`, :func:`spawnlpe`, :func:`spawnvp`  および :func:`spawnvpe` は
   Windows では利用できません。

   .. versionadded:: 1.6


.. data:: P_NOWAIT
          P_NOWAITO

   :func:`spawn\*` 関数ファミリに対する *mode* パラメタとして取れる値です。
   この値のいずれかを *mode* として与えた場合、
   :func:`spawn\*` 関数は新たなプロセスが生成されるとすぐに、
   プロセスの ID を戻り値として返ります。

   利用できる環境: Unix, Windows

   .. versionadded:: 1.6


.. data:: P_WAIT

   :func:`spawn\ *` 関数ファミリに対する *mode* パラメタとして取れる値です。
   この値を *mode* として与えた場合、
   :func:`spawn\*` 関数は新たなプロセスを起動して完了するまで返らず、
   プロセスがうまく終了した場合には終了コードを、シグナルによってプロセスが
   kill された場合には ``-signal`` を返します。

   利用できる環境: Unix, Windows

   .. versionadded:: 1.6


.. data:: P_DETACH
          P_OVERLAY

   :func:`spawn\*` 関数ファミリに対する *mode* パラメタとして取れる値です。
   これらの値は上の値よりもやや可搬性において劣っています。
   :const:`P_DETACH` は :const:`P_NOWAIT` に似ていますが、
   新たなプロセスは呼び出しプロセスのコンソールから切り離され (detach) ます。
   :const:`P_OVERLAY` が使われた場合、現在のプロセスは置き換えられます;
   従って :func:`spawn\*` は返りません。

   利用できる環境: Windows

   .. versionadded:: 1.6


.. function:: startfile(path[, operation])

   ファイルを関連付けられたアプリケーションを使って「スタート」します。

   *operation* が指定されないかまたは ``'open'`` であるとき、この動作は、 Windows の Explorer
   上でのファイルをダブルクリックや、コマンドプロンプト (interactive command shell) 上でのファイル名を
   :program:`start` 命令の引数としての実行と同様です: ファイルは拡張子が関連付けされているアプリケーション (が存在する場合)
   を使って開かれます。

   他の *operation* が与えられる場合、それはファイルに対して何がなされるべきかを表す "command verb" (コマンドを表す動詞)
   でなければなりません。 Microsoft が文書化している動詞は、 ``'print'`` と ``'edit'`` (ファイルに対して) および
   ``'explore'`` と ``'find'`` (ディレクトリに対して) です。

   :func:`startfile` は関連付けされたアプリケーションが起動すると同時に返ります。アプリケーションが閉じるまで待機させるためのオプション
   はなく、アプリケーションの終了状態を取得する方法もありません。 *path* 引数は現在のディレクトリからの相対で表します。
   絶対パスを利用したいなら、最初の文字はスラッシュ  (``'/'``) ではないので注意してください; もし最初の文字がスラッシュなら、システムの背後にある
   Win32 :c:func:`ShellExecute` 関数は動作しません。
   :func:`os.path.normpath` 関数を使って、Win32 用に
   正しくコード化されたパスになるようにしてください。

   利用できる環境: Windows

   .. versionadded:: 2.0

   .. versionadded:: 2.5
      *operation* パラメータ.


.. function:: system(command)

   サブシェル内でコマンド (文字列) を実行します。
   この関数は標準 C 関数 :c:func:`system` を使って実装されており、
   :c:func:`system` と同じ制限があります。
   :data:`sys.stdin` などに対する変更を行っても、
   実行されるコマンドの環境には反映されません。

   Unixでは、戻り値はプロセスの終了ステータスで、 :func:`wait`
   で定義されている書式にコード化されています。 POSIX は
   :c:func:`system` 関数の戻り値の意味について定義していないので、
   Python の :func:`system` における戻り値はシステム依存となることに注意してください。

   Windows では、戻り値は *command* を実行した後にシステムシェルから返される値で、Windows の環境変数
   :envvar:`COMSPEC` となります: :program:`command.com` ベースのシステム (Windows 95, 98 および ME)
   では、この値は常に ``0`` です; :program:`cmd.exe` ベースのシステム (Windows NT, 2000 および XP)
   では、この値は実行したコマンドの終了ステータスです; ネイティブでないシェルを使っているシステムについては、
   使っているシェルのドキュメントを参照してください。

   .. The :mod:`subprocess` module provides more powerful facilities for spawning new
      processes and retrieving their results; using that module is preferable to using
      this function.  Use the :mod:`subprocess` module.  Check especially the
      :ref:`subprocess-replacements` section.

   :mod:`subprocess` モジュールが、新しいプロセスを実行して結果を取得するための、
   より強力な機能を提供しています。
   この関数の代わりに、 :mod:`subprocess` モジュールを利用することが推奨されています。
   :mod:`subprocess` モジュールのドキュメントの、
   :ref:`subprocess-replacements`
   というセクションのレシピを参考にして下さい。

   利用できる環境: Unix, Windows


.. function:: times()

   (プロセッサまたはその他の) 積算時間を秒で表す浮動小数点数からなる、
   5 要素のタプルを返します。
   タプルの要素は、ユーザ時間 (user time)、システム時間 (system time)、
   子プロセスのユーザ時間、子プロセスのシステム時間、
   そして過去のある固定時点からの経過時間で、この順に並んでいます。
   Unix マニュアルページ :manpage:`times(2)` 
   または対応する Windows プラットフォーム API ドキュメントを参照してください。
   Windows では、最初の２つの要素だけが埋められ、残りは0になります。

   利用できる環境: Unix, Windows


.. function:: wait()

   子プロセスの実行完了を待機し、子プロセスの pid と終了コードインジケータ
   --- 16 ビットの数で、下位バイトがプロセスを kill
   したシグナル番号、上位バイトが終了ステータス (シグナル番号がゼロの場合)
   --- の入ったタプルを返します;
   コアダンプファイルが生成された場合、下位バイトの最上桁ビットが立てられます。

   利用できる環境: Unix


.. function:: waitpid(pid, options)

   Unix の場合:
   プロセス id *pid* で与えられた子プロセスの完了を待機し、
   子プロセスのプロセス id と(:func:`wait` と同様にコード化された)
   終了ステータスインジケータからなるタプルを返します。
   この関数の動作は *options* によって影響されます。
   通常の操作では ``0`` にします。

   *pid* が ``0`` よりも大きい場合、 :func:`waitpid` は特定のプロセスのステータス情報を要求します。 *pid* が ``0``
   の場合、現在のプロセスグループ内の任意の子プロセスの状態に対する要求です。 *pid* が ``-1`` の場合、現在のプロセス
   の任意の子プロセスに対する要求です。 *pid* が ``-1`` よりも小さい場合、プロセスグループ ``-pid`` (すなわち *pid* の絶対値)
   内の任意のプロセスに対する要求です。

   .. An :exc:`OSError` is raised with the value of errno when the syscall
      returns -1.

   システムコールが -1 を返したとき、 :exc:`OSError` を errno と共に送出します。

   .. On Windows: Wait for completion of a process given by process handle *pid*, and
      return a tuple containing *pid*, and its exit status shifted left by 8 bits
      (shifting makes cross-platform use of the function easier). A *pid* less than or
      equal to ``0`` has no special meaning on Windows, and raises an exception. The
      value of integer *options* has no effect. *pid* can refer to any process whose
      id is known, not necessarily a child process. The :func:`spawn` functions called
      with :const:`P_NOWAIT` return suitable process handles.

   Windowsでは、プロセスハンドル *pid* を指定してプロセスの終了を待って、
   *pid* と、終了ステータスを8bit左シフトした値のタプルを返します。
   (シフトは、この関数をクロスプラットフォームで利用しやすくするために行われます)
   ``0`` 以下の *pid* はWindowsでは特別な意味を持っておらず、例外を発生させます。
   *options* の値は効果がありません。
   *pid* は、子プロセスで無くても、プロセスIDを知っているどんなプロセスでも参照することが可能です。
   :func:`spawn` 関数を :const:`P_NOWAIT` と共に呼び出した場合、適切なプロセスハンドルが返されます。

.. function:: wait3([options])

   :func:`waitpid` に似ていますが、プロセス id を引数に取らず、子プロセス
   id、終了ステータスインジケータ、リソース使用情報の3要素からなるタプルを返します。リソース使用情報の詳しい情報は :mod:`resource`.\
   :func:`getrusage` を参照してください。 *options* は :func:`waitpid` および :func:`wait4`
   と同様です。

   利用できる環境: Unix

   .. versionadded:: 2.5


.. function:: wait4(pid, options)

   :func:`waitpid` に似ていますが、子プロセス id、終了ステータスインジケータ、リソース使用情報の3要素からなるタプルを返します。
   リソース使用情報の詳しい情報は :mod:`resource`.\ :func:`getrusage` を参照してください。 :func:`wait4`
   の引数は :func:`waitpid` に与えられるものと同じです。

   利用できる環境: Unix

   .. versionadded:: 2.5


.. data:: WNOHANG

   子プロセス状態がすぐに取得できなかった場合に直ちに終了するようにするための :func:`waitpid` のオプションです。
   この場合、関数は ``(0, 0)`` を返します。

   利用できる環境: Unix


.. data:: WCONTINUED

   このオプションによって子プロセスは前回状態が報告された後にジョブ制御による停止状態から実行を継続された場合に報告されるようになります。

   利用できる環境: ある種の Unix システム。

   .. versionadded:: 2.3


.. data:: WUNTRACED

   このオプションによって子プロセスは停止されていながら停止されてから状態が報告されていない場合に報告されるようになります。

   利用できる環境: Unix

   .. versionadded:: 2.3

以下の関数は :func:`system`, :func:`wait`, あるいは :func:`waitpid` が返すプロセス状態コード
を引数にとります。これらの関数はプロセスの配置を決めるために利用することができます。


.. function:: WCOREDUMP(status)

   プロセスに対してコアダンプが生成されていた場合には ``True`` を、それ以外の場合は ``False`` を返します。

   利用できる環境: Unix

   .. versionadded:: 2.3


.. function:: WIFCONTINUED(status)

   プロセスがジョブ制御による停止状態から実行を継続された (continue) 場合に ``True`` を、それ以外の場合は ``False`` を返します。

   利用できる環境: Unix

   .. versionadded:: 2.3


.. function:: WIFSTOPPED(status)

   プロセスが停止された (stop) 場合に ``True`` を、それ以外の場合は ``False`` を返します。

   利用できる環境: Unix


.. function:: WIFSIGNALED(status)

   プロセスがシグナルによって終了した (exit) 場合に ``True`` を、それ以外の場合は ``False`` を返します。

   利用できる環境: Unix


.. function:: WIFEXITED(status)

   プロセスが :manpage:`exit(2)` システムコールで終了した場合に ``True`` を、それ以外の場合は ``False`` を返します。

   利用できる環境: Unix


.. function:: WEXITSTATUS(status)

   ``WIFEXITED(status)`` が真の場合、 :manpage:`exit(2)` システムコールに渡された整数パラメタを返します。そうでない場合、
   返される値には意味がありません。

   利用できる環境: Unix


.. function:: WSTOPSIG(status)

   プロセスを停止させたシグナル番号を返します。

   利用できる環境: Unix


.. function:: WTERMSIG(status)

   プロセスを終了させたシグナル番号を返します。

   利用できる環境: Unix


.. _os-path:

雑多なシステム情報
------------------


.. function:: confstr(name)

   文字列形式によるシステム設定値 (system configuration value)を返します。
   *name* には取得したい設定名を指定します; 
   この値は定義済みのシステム値名を表す文字列にすることができます;
   名前は多くの標準 (POSIX.1、 Unix 95、 Unix 98 その他)
   で定義されています。ホストオペレーティングシステムの関知する名前は
   ``confstr_names`` 辞書のキーとして与えられています。
   このマップ型オブジェクトに入っていない設定変数については、
   *name* に整数を渡してもかまいません。

   *name* に指定された設定値が定義されていない場合、 ``None`` を返します。

   もし *name* が文字列でかつ不明である場合、 :exc:`ValueError`  を送出します。 *name*
   の指定値がホストシステムでサポートされておらず、 ``confstr_names`` にも入っていない場合、 :const:`errno.EINVAL`
   をエラー番号として :exc:`OSError` を送出します。

   利用できる環境: Unix

.. data:: confstr_names

   :func:`confstr` が受理する名前を、
   ホストオペレーティングシステムで定義されている整数値に対応付けている辞書です。
   この辞書はシステムでどの設定名が定義されているかを決定するために利用できます。

   利用できる環境: Unix


.. function:: getloadavg()

   過去 1 分、5 分、15分間で、システムで走っているキューの平均プロセス数を返します。
   平均負荷が得られない場合には :exc:`OSError` を送出します。

   利用できる環境: Unix

   .. versionadded:: 2.3


.. function:: sysconf(name)

   整数値のシステム設定値を返します。
   *name* で指定された設定値が定義されていない場合、 ``-1``  が返されます。
   *name* に関するコメントとしては、 :func:`confstr` で述べた内容が
   同様に当てはまります; 既知の設定名についての情報を与える辞書は
   ``sysconf_names`` で与えられています。

   利用できる環境: Unix


.. data:: sysconf_names

   :func:`sysconf` が受理する名前を、
   ホストオペレーティングシステムで定義されている整数値に対応付けている辞書です。
   この辞書はシステムでどの設定名が定義されているかを決定するために利用できます。

   利用できる環境: Unix

以下のデータ値はパス名編集操作をサポートするために利用されます。
これらの値は全てのプラットフォームで定義されています。

パス名に対する高レベルの操作は :mod:`os.path` モジュールで定義されています。


.. data:: curdir

   現在のディレクトリ参照するためにオペレーティングシステムで使われる文字列定数です。
   POSIX と Windows では ``'.'`` になります。
   :mod:`os.path` からも利用できます。


.. data:: pardir

   親ディレクトリを参照するためにオペレーティングシステムで使われる文字列定数です。
   POSIX と Windows では ``'..'`` になります。
   :mod:`os.path` からも利用できます。


.. data:: sep

   パス名を要素に分割するためにオペレーティングシステムで利用されている文字です。
   例えば POSIX では ``'/'`` で、Windowsでは ``'\\'`` です。
   しかし、このことを知っているだけではパス名を解析したり、
   パス名同士を結合したりするには不十分です ---  こうした操作には
   :func:`os.path.split` や :func:`os.path.join`  を使ってください
   --- が、たまに便利なこともあります。
   :mod:`os.path` からも利用できます。


.. data:: altsep

   文字パス名を要素に分割する際にオペレーティングシステムで利用されるもう一つの文字で、
   分割文字が一つしかない場合には ``None`` になります。
   この値は ``sep`` がバックスラッシュとなっている DOS や Windows  システムでは
   ``'/'`` に設定されています。
   :mod:`os.path` からも利用できます。


.. data:: extsep

   ベースのファイル名と拡張子を分ける文字。
   たとえば、 :file:`os.py` だったら ``'.'`` です。
   :mod:`os.path` からも利用できます。

   .. versionadded:: 2.2


.. data:: pathsep

   (:envvar:`PATH` のような) サーチパス内の要素を分割するために
   オペレーティングシステムが慣習的に用いる文字で、POSIX における
   ``':'`` や DOS および Windows における ``';'`` に相当します。
   :mod:`os.path` からも利用できます。


.. data:: defpath

   :func:`exec\*p\*` や :func:`spawn\*p\*` において、環境変数辞書内に ``'PATH'``
   キーがない場合に使われる標準設定のサーチパスです。
   :mod:`os.path` からも利用できます。


.. data:: linesep

   現在のプラットフォーム上で行を分割 (あるいは終端) するために用いられている文字列です。
   この値は例えば POSIX での ``'\n'`` や Mac OS での ``'\r'`` のように、
   単一の文字にもなりますし、例えば Windows での ``'\r\n'`` 
   のように複数の文字列にもなります。
   テキストモードで開いたファイルに書き込むときには、
   *os.linesep* を利用しないでください。
   全てのプラットフォームで、単一の ``'\n'`` を使ってください。


.. data:: devnull

   ヌルデバイス (null device) のファイルパスです。
   例えばPOSIX では ``'/dev/null'`` で、 Windows では ``'nul'`` です。
   この値は :mod:`os.path` からも利用できます。

   .. versionadded:: 2.4


.. _os-miscfunc:

雑多な関数
----------


.. function:: urandom(n)

   暗号に関する用途に適した *n* バイトからなるランダムな文字列を返します。

   この関数は OS 固有の乱数発生源からランダムなバイト列を生成して返します。
   この関数の返すデータは暗号を用いたアプリケーションで十分利用できる程度に
   予測不能ですが、実際のクオリティは OS の実装によって異なります。
   Unix系のシステムでは :file:`/dev/urandom` への問い合わせを行い、
   Windows では :c:func:`CryptGenRandom` を使います。乱数発生源
   が見つからない場合、 :exc:`NotImplementedError` を送出します。

   .. versionadded:: 2.4

