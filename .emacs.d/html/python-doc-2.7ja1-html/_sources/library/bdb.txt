:mod:`bdb` --- デバッガーフレームワーク
=======================================

.. module:: bdb
   :synopsis: デバッガーフレームワーク


.. The :mod:`bdb` module handles basic debugger functions, like setting breakpoints
   or managing execution via the debugger.

:mod:`bdb` モジュールは、ブレークポイントを設定したり、デバッガー経由で実行を管理するような、
基本的なデバッガー機能を提供します


.. The following exception is defined:

以下の例外が定義されています。


.. exception:: BdbQuit

   .. Exception raised by the :class:`Bdb` class for quitting the debugger.

   :class:`Bdb` クラスが、デバッガーを終了させるために投げる例外。


.. The :mod:`bdb` module also defines two classes:

:mod:`bdb` モジュールは2つのクラスを定義しています。


.. class:: Breakpoint(self, file, line[, temporary=0[, cond=None [, funcname=None]]])

   .. This class implements temporary breakpoints, ignore counts, disabling and
      (re-)enabling, and conditionals.

   このクラスはテンポラリブレークポイント、無視するカウント、無効化と再有効化、条件付き
   ブレークポイントを実装しています。


   .. Breakpoints are indexed by number through a list called :attr:`bpbynumber`
      and by ``(file, line)`` pairs through :attr:`bplist`.  The former points to a
      single instance of class :class:`Breakpoint`.  The latter points to a list of
      such instances since there may be more than one breakpoint per line.

   ブレークポイントは :attr:`bpbynumber` という名前のリストで番号によりインデックスされ、
   :attr:`bplist` により ``(file, line)`` の形でインデックスされます。
   :attr:`bpbynumber` は :class:`Breakpoint` クラスのインスタンスを指しています。
   一方 :attr:`bplist` は、同じ行に複数のブレークポイントが設定される場合があるので、
   インスタンスのリストを指しています。


   .. When creating a breakpoint, its associated filename should be in canonical
      form.  If a *funcname* is defined, a breakpoint hit will be counted when the
      first line of that function is executed.  A conditional breakpoint always
      counts a hit.

   ブレークポイントを作るとき、設定されるファイル名は正規化されていなければなりません。
   *funcname* が設定されたとき、ブレークポイントはその関数の最初の行が実行されたときに
   ヒットカウントにカウントされます。
   条件付ブレークポイントは毎回カウントされます。


   .. :class:`Breakpoint` instances have the following methods:

   :class:`Breakpoint` インスタンスは以下のメソッドを持ちます。


   .. method:: deleteMe()

      .. Delete the breakpoint from the list associated to a file/line.  If it is
         the last breakpoint in that position, it also deletes the entry for the
         file/line.

      このブレークポイントをファイル/行に関連付けられたリストから削除します。
      このブレークポイントがその行に設定された最後のブレークポイントだった場合、
      そのファイル/行に対するエントリ自体を削除します。


   .. method:: enable()

      .. Mark the breakpoint as enabled.

      このブレークポイントを有効にします。


   .. method:: disable()

      .. Mark the breakpoint as disabled.

      このブレークポイントを無効にします。


   .. method:: pprint([out])

      .. Print all the information about the breakpoint:

      このブレークポイントに関するすべての情報を表示します。


      .. * The breakpoint number.
         * If it is temporary or not.
         * Its file,line position.
         * The condition that causes a break.
         * If it must be ignored the next N times.
         * The breakpoint hit count.

      * ブレークポイント番号
      * テンポラリブレークポイントかどうか
      * ファイル/行の位置
      * ブレークする条件
      * 次のN回無視されるか
      * ヒットカウント


.. class:: Bdb(skip=None)

   .. The :class:`Bdb` class acts as a generic Python debugger base class.

   :class:`Bdb` クラスは一般的なPythonデバッガーの基本クラスとして振舞います。


   .. This class takes care of the details of the trace facility; a derived class
      should implement user interaction.  The standard debugger class
      (:class:`pdb.Pdb`) is an example.

   このクラスはトレース機能の詳細を扱います。ユーザーとのインタラクションは、
   派生クラスが実装するべきです。標準ライブラリのデバッガクラス (:class:`pdb.Pdb`)
   がその利用例です。


   .. The *skip* argument, if given, must be an iterable of glob-style
      module name patterns.  The debugger will not step into frames that
      originate in a module that matches one of these patterns. Whether a
      frame is considered to originate in a certain module is determined
      by the ``__name__`` in the frame globals.

   *skip* 引数は、もし与えられたならグロブ形式のモジュール名パターンの iterable でなければなりません。
   デバッガはこれらのパターンのどれかにマッチするモジュールで発生したフレームにステップインしなくなります。
   フレームが特定のモジュールで発生したかどうかは、フレームのグローバル変数の ``__name__`` によって決定されます。


   .. .. versionadded:: 2.7
   ..    The *skip* argument.

   .. versionadded:: 2.7
      *skip* 引数
 

   .. The following methods of :class:`Bdb` normally don't need to be overridden.

   以下の :class:`Bdb` のメソッドは、通常オーバーライドする必要はありません。


   .. method:: canonic(filename)

      .. Auxiliary method for getting a filename in a canonical form, that is, as a
         case-normalized (on case-insensitive filesystems) absolute path, stripped
         of surrounding angle brackets.

      標準化されたファイル名を取得するための補助関数。標準化されたファイル名とは、
      (大文字小文字を区別しないファイルシステムにおいて)大文字小文字を正規化し、
      絶対パスにしたものです。ファイル名が "<" と ">" で囲まれていた場合はそれを
      取り除いたものです。


   .. method:: reset()

      .. Set the :attr:`botframe`, :attr:`stopframe`, :attr:`returnframe` and
         :attr:`quitting` attributes with values ready to start debugging.

      :attr:`botframe`, :attr:`stopframe`, :attr:`returnframe`, :attr:`quitting`
      属性を、デバッグを始められる状態に設定します。


   .. method:: trace_dispatch(frame, event, arg)

      .. This function is installed as the trace function of debugged frames.  Its
         return value is the new trace function (in most cases, that is, itself).

      この関数は、デバッグされているフレームのトレース関数としてインストールされます。
      戻り値は新しいトレース関数(殆どの場合はこの関数自身)です。


      .. The default implementation decides how to dispatch a frame, depending on
         the type of event (passed as a string) that is about to be executed.
         *event* can be one of the following:

      デフォルトの実装は、実行しようとしている *event* (文字列として渡されます) の種類に基づいて
      フレームのディスパッチ方法を決定します。
      *event* は次のうちのどれかです。

      * ``"line"``: 新しい行を実行しようとしています。
      * ``"call"``: 関数が呼び出されているか、別のコードブロックに入ります。
      * ``"return"``: 関数か別のコードブロックからreturnしようとしています。
      * ``"exception"``: 例外が発生しました。
      * ``"c_call"``: C関数を呼び出そうとしています。
      * ``"c_return"``: C関数からreturnしました。
      * ``"c_exception"``: C関数が例外を発生させました。

      .. For the Python events, specialized functions (see below) are called.  For
         the C events, no action is taken.

      Pythonのイベントに対しては、以下の専用の関数群が呼ばれます。Cのイベントに対しては何もしません。


      .. The *arg* parameter depends on the previous event.

      *arg* 引数は以前のイベントに依存します。


      .. See the documentation for :func:`sys.settrace` for more information on the
         trace function.  For more information on code and frame objects, refer to
         :ref:`types`.

      トレース関数についてのより詳しい情報は、 :func:`sys.settrace` の
      ドキュメントを参照してください。
      コードとフレームオブジェクトについてのより詳しい情報は、 :ref:`types`
      を参照してください。


   .. method:: dispatch_line(frame)

      .. If the debugger should stop on the current line, invoke the
         :meth:`user_line` method (which should be overridden in subclasses).
         Raise a :exc:`BdbQuit` exception if the :attr:`Bdb.quitting` flag is set
         (which can be set from :meth:`user_line`).  Return a reference to the
         :meth:`trace_dispatch` method for further tracing in that scope.

      デバッガーが現在の行で止まるべきであれば、 :meth:`user_line` メソッド
      (サブクラスでオーバーライドされる)を呼び出します。
      :attr:`Bdb.quitting` フラグ(:meth:`user_line` から設定できます)が設定されていた場合、
      :exc:`BdbQuit` 例外を発生させます。
      このスコープのこれからのトレースのために、 :meth:`trace_dispatch` メソッドの
      参照を返します。


   .. method:: dispatch_call(frame, arg)

      .. If the debugger should stop on this function call, invoke the
         :meth:`user_call` method (which should be overridden in subclasses).
         Raise a :exc:`BdbQuit` exception if the :attr:`Bdb.quitting` flag is set
         (which can be set from :meth:`user_call`).  Return a reference to the
         :meth:`trace_dispatch` method for further tracing in that scope.

      デバッガーがこの関数呼び出しで止まるべきであれば、 :meth:`user_call` メソッド
      (サブクラスでオーバーライドされる)を呼び出します。
      :attr:`Bdb.quitting` フラグ(:meth:`user_line` から設定できます)が設定されていた場合、
      :exc:`BdbQuit` 例外を発生させます。
      このスコープのこれからのトレースのために、 :meth:`trace_dispatch` メソッドの
      参照を返します。


   .. method:: dispatch_return(frame, arg)

      .. If the debugger should stop on this function return, invoke the
         :meth:`user_return` method (which should be overridden in subclasses).
         Raise a :exc:`BdbQuit` exception if the :attr:`Bdb.quitting` flag is set
         (which can be set from :meth:`user_return`).  Return a reference to the
         :meth:`trace_dispatch` method for further tracing in that scope.

      デバッガーがこの関数からのリターンで止まるべきであれば、 :meth:`user_call` メソッド
      (サブクラスでオーバーライドされる)を呼び出します。
      :attr:`Bdb.quitting` フラグ(:meth:`user_line` から設定できます)が設定されていた場合、
      :exc:`BdbQuit` 例外を発生させます。
      このスコープのこれからのトレースのために、 :meth:`trace_dispatch` メソッドの
      参照を返します。


   .. method:: dispatch_exception(frame, arg)

      .. If the debugger should stop at this exception, invokes the
         :meth:`user_exception` method (which should be overridden in subclasses).
         Raise a :exc:`BdbQuit` exception if the :attr:`Bdb.quitting` flag is set
         (which can be set from :meth:`user_exception`).  Return a reference to the
         :meth:`trace_dispatch` method for further tracing in that scope.

      デバッガーがこの例外発生で止まるべきであれば、 :meth:`user_call` メソッド
      (サブクラスでオーバーライドされる)を呼び出します。
      :attr:`Bdb.quitting` フラグ(:meth:`user_line` から設定できます)が設定されていた場合、
      :exc:`BdbQuit` 例外を発生させます。
      このスコープのこれからのトレースのために、 :meth:`trace_dispatch` メソッドの
      参照を返します。


   .. Normally derived classes don't override the following methods, but they may
      if they want to redefine the definition of stopping and breakpoints.

   通常、継承クラスは以下のメソッド群をオーバーライドしません。
   しかし、停止やブレークポイント機能を再定義したい場合には、オーバーライドする
   こともあります。


   .. method:: stop_here(frame)

      .. This method checks if the *frame* is somewhere below :attr:`botframe` in
         the call stack.  :attr:`botframe` is the frame in which debugging started.

      このメソッドは *frame* がコールスタック中で :attr:`botframe` よりも下にあるかチェックします。
      :attr:`botframe` はデバッグを開始したフレームです。


   .. method:: break_here(frame)

      .. This method checks if there is a breakpoint in the filename and line
         belonging to *frame* or, at least, in the current function.  If the
         breakpoint is a temporary one, this method deletes it.

      このメソッドは、 *frame* に属するファイル名と行に、あるいは、少なくとも現在の関数に
      ブレークポイントがあるかどうかをチェックします。
      ブレークポイントがテンポラリブレークポイントだった場合、このメソッドはその
      ブレークポイントを削除します。


   .. method:: break_anywhere(frame)

      .. This method checks if there is a breakpoint in the filename of the current
         frame.

      このメソッドは、現在のフレームのファイル名の中にブレークポイントが存在するかどうかをチェックします。


   .. Derived classes should override these methods to gain control over debugger
      operation.

   継承クラスはデバッガー操作をするために以下のメソッド群をオーバーライドするべきです。


   .. method:: user_call(frame, argument_list)

      .. This method is called from :meth:`dispatch_call` when there is the
         possibility that a break might be necessary anywhere inside the called
         function.

      このメソッドは、呼ばれた関数の中でブレークする必要がある可能性がある場合に、
      :meth:`dispatch_call` から呼び出されます。


   .. method:: user_line(frame)

      .. This method is called from :meth:`dispatch_line` when either
         :meth:`stop_here` or :meth:`break_here` yields True.

      このメソッドは、 :meth:`stop_here` か :meth:`break_here` が True を返したときに、
      :meth:`dispatch_line` から呼び出されます。


   .. method:: user_return(frame, return_value)

      .. This method is called from :meth:`dispatch_return` when :meth:`stop_here`
         yields True.

      このメソッドは、 :meth:`stop_here` が True を返したときに、 :meth:`dispatch_return`
      から呼び出されます。


   .. method:: user_exception(frame, exc_info)

      .. This method is called from :meth:`dispatch_exception` when
         :meth:`stop_here` yields True.

      このメソッドは、 :meth:`stop_here` が True を返したときに、 :meth:`dispatch_exception`
      から呼び出されます。


   .. method:: do_clear(arg)

      .. Handle how a breakpoint must be removed when it is a temporary one.

      ブレークポイントがテンポラリブレークポイントだったときに、それをどう削除するかを決定します。


      .. This method must be implemented by derived classes.

      継承クラスはこのメソッドを実装しなければなりません。


   .. Derived classes and clients can call the following methods to affect the
      stepping state.

   継承クラスとクライアントは、ステップ状態に影響を及ぼすために以下のメソッドを呼び出すことができます。


   .. method:: set_step()

      .. Stop after one line of code.

      コードの次の行でストップします。


   .. method:: set_next(frame)

      .. Stop on the next line in or below the given frame.

      与えられたフレームかそれより下(のフレーム)にある、次の行でストップします。


   .. method:: set_return(frame)

      .. Stop when returning from the given frame.

      指定されたフレームから抜けるときにストップします。


   .. method:: set_until(frame)

      .. Stop when the line with the line no greater than the current one is
         reached or when returning from current frame

      現在の行番号よりも大きい行番号に到達したとき、あるいは、現在のフレーム
      から戻るときにストップします。


   .. method:: set_trace([frame])

      .. Start debugging from *frame*.  If *frame* is not specified, debugging
         starts from caller's frame.

      *frame* からデバッグを開始します。 *frame* が指定されなかった場合、
      デバッグは呼び出し元のフレームから開始します。


   .. method:: set_continue()

      .. Stop only at breakpoints or when finished.  If there are no breakpoints,
         set the system trace function to None.

      ブレークポイントに到達するか終了したときにストップします。
      もしブレークポイントが1つも無い場合、システムのトレース関数を None
      に設定します。


   .. method:: set_quit()

      .. Set the :attr:`quitting` attribute to True.  This raises :exc:`BdbQuit` in
         the next call to one of the :meth:`dispatch_\*` methods.

      :attr:`quitting` 属性を True に設定します。
      これにより、次回の :meth:`dispatch_\*` メソッドのどれかの呼び出しで、
      :exc:`BdbQuit` 例外を発生させます。


   .. Derived classes and clients can call the following methods to manipulate
      breakpoints.  These methods return a string containing an error message if
      something went wrong, or ``None`` if all is well.

   継承クラスとクライアントは以下のメソッドをブレークポイント操作に利用できます。
   これらのメソッドは、何か悪いことがあればエラーメッセージを含む文字列を返し、
   すべてが順調であれば ``None`` を返します。


   .. method:: set_break(filename, lineno[, temporary=0[, cond[, funcname]]])

      .. Set a new breakpoint.  If the *lineno* line doesn't exist for the
         *filename* passed as argument, return an error message.  The *filename*
         should be in canonical form, as described in the :meth:`canonic` method.

      新しいブレークポイントを設定します。
      引数の *lineno* 行が *filename* に存在しない場合、エラーメッセージを返します。
      *filename* は、 :meth:`canonic` メソッドで説明されているような、標準形である
      必要があります。


   .. method:: clear_break(filename, lineno)

      .. Delete the breakpoints in *filename* and *lineno*.  If none were set, an
         error message is returned.

      *filename* の *lineno* 行にあるブレークポイントを削除します。
      もしブレークポイントが無かった場合、エラーメッセージを返します。


   .. method:: clear_bpbynumber(arg)

      .. Delete the breakpoint which has the index *arg* in the
         :attr:`Breakpoint.bpbynumber`.  If *arg* is not numeric or out of range,
         return an error message.

      :attr:`Breakpoint.bpbynumber` の中で *arg* のインデックスを持つブレークポイントを
      削除します。
      *arg* が数値でないか範囲外の場合、エラーメッセージを返します。


   .. method:: clear_all_file_breaks(filename)

      .. Delete all breakpoints in *filename*.  If none were set, an error message
         is returned.

      *filename* に含まれるすべてのブレークポイントを削除します。
      もしブレークポイントが無い場合、エラーメッセージを返します。


   .. method:: clear_all_breaks()

      .. Delete all existing breakpoints.

      すべてのブレークポイントを削除します。


   .. method:: get_break(filename, lineno)

      .. Check if there is a breakpoint for *lineno* of *filename*.

      *filename* の *lineno* にブレークポイントが存在するかどうかをチェックします。


   .. method:: get_breaks(filename, lineno)

      .. Return all breakpoints for *lineno* in *filename*, or an empty list if
         none are set.

      *filename* の *lineno* にあるすべてのブレークポイントを返します。
      ブレークポイントが存在しない場合は空のリストを返します。


   .. method:: get_file_breaks(filename)

      .. Return all breakpoints in *filename*, or an empty list if none are set.

      *filename* の中のすべてのブレークポイントを返します。
      ブレークポイントが存在しない場合は空のリストを返します。


   .. method:: get_all_breaks()

      .. Return all breakpoints that are set.

      セットされているすべてのブレークポイントを返します。


   .. Derived classes and clients can call the following methods to get a data
      structure representing a stack trace.

   継承クラスとクライアントは以下のメソッドを呼んでスタックトレースを表現する
   データ構造を取得することができます。


   .. method:: get_stack(f, t)

      .. Get a list of records for a frame and all higher (calling) and lower
         frames, and the size of the higher part.

      与えられたフレームおよび上位(呼び出し側)と下位のすべてのフレームに対するレコードのリストと、
      上位フレームのサイズを得ます。


   .. method:: format_stack_entry(frame_lineno, [lprefix=': '])

      .. Return a string with information about a stack entry, identified by a
         ``(frame, lineno)`` tuple:

      ``(frame, lineno)`` で指定されたスタックエントリに関する次のような情報を持つ
      文字列を返します。


      .. * The canonical form of the filename which contains the frame.
         * The function name, or ``"<lambda>"``.
         * The input arguments.
         * The return value.
         * The line of code (if it exists).

      * そのフレームを含むファイル名の標準形
      * 関数名、もしくは ``"<lambda>"``
      * 入力された引数
      * 戻り値
      * (あれば)その行のコード


   .. The following two methods can be called by clients to use a debugger to debug
      a :term:`statement`, given as a string.

   以下の2つのメソッドは、文字列として渡された文(:term:`statement`)をデバッグするもので、
   クライアントから利用されます。


   .. method:: run(cmd, [globals, [locals]])

      .. Debug a statement executed via the :keyword:`exec` statement.  *globals*
         defaults to :attr:`__main__.__dict__`, *locals* defaults to *globals*.

      :keyword:`exec` 文を利用して文を実行しデバッグします。
      *globals* はデフォルトでは :attr:`__main__.__dict__` で、 *locals* はデフォルトでは
      *globals* です。


   .. method:: runeval(expr, [globals, [locals]])

      .. Debug an expression executed via the :func:`eval` function.  *globals* and
         *locals* have the same meaning as in :meth:`run`.

      :func:`eval` 関数を利用して式を実行しデバッグします。
      *globals* と *locals* は :meth:`run` と同じ意味です。


   .. method:: runctx(cmd, globals, locals)

      .. For backwards compatibility.  Calls the :meth:`run` method.

      後方互換性のためのメソッドです。 :meth:`run` を使ってください。


   .. method:: runcall(func, *args, **kwds)

      .. Debug a single function call, and return its result.

      1つの関数呼び出しをデバッグし、その結果を返します。


.. Finally, the module defines the following functions:

最後に、このモジュールは以下の関数を提供しています。


.. function:: checkfuncname(b, frame)

   .. Check whether we should break here, depending on the way the breakpoint *b*
      was set.

   この場所でブレークする必要があるかどうかを、ブレークポイント *b* が設定された
   方法に依存する方法でチェックします。


   .. If it was set via line number, it checks if ``b.line`` is the same as the one
      in the frame also passed as argument.  If the breakpoint was set via function
      name, we have to check we are in the right frame (the right function) and if
      we are in its first executable line.

   ブレークポイントが行番号で設定されていた場合、この関数は ``b.line`` が、同じく引数と
   して与えられた *frame* の中の行に一致するかどうかをチェックします。
   ブレークポイントが関数名で設定されていた場合、この関数は *frame* が指定された関数の
   ものであるかどうかと、その関数の最初の行であるかどうかをチェックします。


.. function:: effective(file, line, frame)

   指定されたソースコード中の行に(有効な)ブレークポイントがあるかどうかを判断します。
   ブレークポイントと、テンポラリブレークポイントを削除して良いかどうかを示すフラグからなる
   タプルを返します。
   マッチするブレークポイントが存在しない場合は ``(None, None)`` を返します。


.. function:: set_trace()

   :class:`Bdb` クラスのインスタンスを使って、呼び出し元のフレームからデバッグを開始します。
