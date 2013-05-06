
.. _profile:

*******************
Python プロファイラ
*******************

.. sectionauthor:: James Roskind

.. module:: profile
   :synopsis: Python source profiler.

.. index:: single: InfoSeek Corporation

Copyright © 1994, by InfoSeek Corporation, all rights reserved.

執筆者 James Roskind  [#]_

Permission to use, copy, modify, and distribute this Python software and its
associated documentation for any purpose (subject to the restriction in the
following sentence) without fee is hereby granted, provided that the above
copyright notice appears in all copies, and that both that copyright notice and
this permission notice appear in supporting documentation, and that the name of
InfoSeek not be used in advertising or publicity pertaining to distribution of
the software without specific, written prior permission.  This permission is
explicitly restricted to the copying and modification of the software to remain
in Python, compiled Python, or other languages (such as C) wherein the modified
or derived code is exclusively imported into a Python module.

INFOSEEK CORPORATION DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT
SHALL INFOSEEK CORPORATION BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING
OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


.. _profiler-introduction:

プロファイラとは
================

.. index::
   single: deterministic profiling
   single: profiling, deterministic


.. A :dfn:`profiler` is a program that describes the run time performance
.. of a program, providing a variety of statistics.  This documentation
.. describes the profiler functionality provided in the modules
.. :mod:`cProfile`, :mod:`profile` and :mod:`pstats`.  This profiler
.. provides :dfn:`deterministic profiling` of Python programs.  It also
.. provides a series of report generation tools to allow users to rapidly
.. examine the results of a profile operation.

:dfn:`プロファイラ` とは、様々な統計値を算出してプログラムの実行効率を調べるためのプログラムです。
この文書では、 :mod:`cProfile`, :mod:`profile`, :mod:`pstats` モジュールが提供するプロファイラ機能について解説します。
このプロファイラは Python プログラムに対する :dfn:`決定論的プロファイリング` を行います。
また、プロファイル結果の検証を素早く行えるように、レポート生成用のツールも提供されています。


.. The Python standard library provides three different profilers:

Python 標準ライブラリは3つの異なるプロファイラを提供しています。


.. #. :mod:`cProfile` is recommended for most users; it's a C extension
..    with reasonable overhead
..    that makes it suitable for profiling long-running programs.
..    Based on :mod:`lsprof`,
..    contributed by Brett Rosen and Ted Czotter.

1. :mod:`cProfile` はほとんどのユーザーに推奨されるモジュールです。
   C言語で書かれた拡張モジュールで、オーバーヘッドが少ないため長時間実行されるプログラムのプロファイルに適しています。
   Brett Rosen と Ted Czotter によって提供された :mod:`lsprof` に基づいています。


   .. versionadded:: 2.5


.. #. :mod:`profile`, a pure Python module whose interface is imitated by
..    :mod:`cProfile`.  Adds significant overhead to profiled programs.
..    If you're trying to extend
..    the profiler in some way, the task might be easier with this module.
..    Copyright © 1994, by InfoSeek Corporation.

2. :mod:`profile` はピュア Python モジュールで、 :mod:`cProfile` モジュールはこのモジュールのインタフェースを真似ています。
   対象プログラムに相当のオーバーヘッドが生じます。
   もしプロファイラに何らかの拡張をしたいのであれば、こちらのモジュールを拡張する方が簡単でしょう。
   Copyright © 1994, by InfoSeek Corporation.


   .. .. versionchanged:: 2.4
   ..    Now also reports the time spent in calls to built-in functions and methods.

   .. versionchanged:: 2.4
      組み込みの関数やメソッドで消費された時間も報告するようになりました。


.. #. :mod:`hotshot` was an experimental C module that focused on minimizing
..    the overhead of profiling, at the expense of longer data
..    post-processing times.  It is no longer maintained and may be
..    dropped in a future version of Python.

3. :mod:`hotshot` は、後処理時間の長さと引き換えにプロファイル中のオーバーヘッドを小さくすることに主眼を置いた実験的な C モジュールでした。
   このモジュールはもう保守されておらず、将来のバージョンのPythonからは外されるかもしれません。


   .. .. versionchanged:: 2.5
   ..    The results should be more meaningful than in the past: the timing core
   ..    contained a critical bug.

   .. versionchanged:: 2.5
      以前より意味のある結果が得られているはずです。かつては時間計測の中核部分に致命的なバグがありました.


.. The :mod:`profile` and :mod:`cProfile` modules export the same interface, so
.. they are mostly interchangeable; :mod:`cProfile` has a much lower overhead but
.. is newer and might not be available on all systems.
.. :mod:`cProfile` is really a compatibility layer on top of the internal
.. :mod:`_lsprof` module.  The :mod:`hotshot` module is reserved for specialized
.. usage.

:mod:`profile` と :mod:`cProfile` の両モジュールは同じインタフェースを提供しているので、ほぼ取り替え可能です。
:mod:`cProfile` はずっと小さなオーバーヘッドで動きますが、まだ新しく、全てのシステムで使えるとは限らないでしょう。
:mod:`cProfile` は実際には :mod:`_lsprof` 内部モジュールに被せられた互換性レイヤです。
:mod:`hotshot` モジュールは特別な使い道のために取っておいてあります。


.. _profile-instant:

かんたんユーザマニュアル
================================

.. This section is provided for users that "don't want to read the manual." It
.. provides a very brief overview, and allows a user to rapidly perform profiling
.. on an existing application.

この節は "マニュアルなんか読みたくない人"のために書かれています。ここではきわめて簡単な概要説明とアプリケーションのプロファイリングを手っ取り早く行う方法だけを解説します。


.. To profile an application with a main entry point of :func:`foo`, you would add
.. the following to your module:

エントリポイント :func:`foo` を持つアプリケーションをプロファイルしたいとき、モジュールに次の内容を追加します。


::

   import cProfile
   cProfile.run('foo()')


.. (Use :mod:`profile` instead of :mod:`cProfile` if the latter is not available on
.. your system.)

(お使いのシステムで :mod:`cProfile` が使えないときは代わりに :mod:`profile` を使って下さい)


.. The above action would cause :func:`foo` to be run, and a series of informative
.. lines (the profile) to be printed.  The above approach is most useful when
.. working with the interpreter.  If you would like to save the results of a
.. profile into a file for later examination, you can supply a file name as the
.. second argument to the :func:`run` function:

このように書くことで :func:`foo` を実行すると同時に一連の情報 (プロファイル) が表示されます。
この方法は、インタプリタ上で作業をしている場合、最も便利なやり方です。
プロファイルの結果をファイルに残し、後で検証したいときは、 :func:`run` の第2引数にファイル名を指定します。


::

   import cProfile
   cProfile.run('foo()', 'fooprof')


.. The file :file:`cProfile.py` can also be invoked as a script to profile another
.. script.  For example:

ファイル :file:`cProfile.py` を使って、別のスクリプトをプロファイルすることも可能です。次のように実行します。


::

   python -m cProfile myscript.py


.. :file:`cProfile.py` accepts two optional arguments on the command line:

:file:`cProfile.py` はコマンドラインから2つのオプション引数を受け取ります。


::

   cProfile.py [-o output_file] [-s sort_order]


.. ``-s`` only applies to standard output (``-o`` is not supplied).
.. Look in the :class:`Stats` documentation for valid sort values.

``-s`` は標準出力にのみ適用されます (つまり、 ``-o`` が与えられなかった場合)。
利用可能なソートの値は、 :class:`Stats` のドキュメントをご覧ください。


.. When you wish to review the profile, you should use the methods in the
.. :mod:`pstats` module.  Typically you would load the statistics data as follows:

プロファイル内容を確認するときは、 :mod:`pstats` モジュールのメソッドを使用します。統計データの読み込みは次のようにします。


::

   import pstats
   p = pstats.Stats('fooprof')


.. The class :class:`Stats` (the above code just created an instance of this class)
.. has a variety of methods for manipulating and printing the data that was just
.. read into ``p``.  When you ran :func:`cProfile.run` above, what was printed was
.. the result of three method calls:

:class:`Stats` クラス (上記コードはこのクラスのインスタンスを生成するだけの内容です) は
``p`` に読み込まれたデータを操作したり、表示したりするための各種メソッドを備えています。
先に :func:`cProfile.run` を実行したとき表示された内容と同じものは、3つのメソッド呼び出しにより実現できます。


::

   p.strip_dirs().sort_stats(-1).print_stats()


.. The first method removed the extraneous path from all the module names. The
.. second method sorted all the entries according to the standard module/line/name
.. string that is printed. The third method printed out all the statistics.  You
.. might try the following sort calls:

最初のメソッドはモジュール名からファイル名の前に付いているパス部分を取り除きます。
2番目のメソッドはエントリをモジュール名/行番号/名前に基づいてソートします。
3番目のメソッドですべての統計情報を出力します。次のようなソートメソッドも使えます。


.. (this is to comply with the semantics of the old profiler).

.. (旧プロファイラとの構文上の互換性機能)


::

   p.sort_stats('name')
   p.print_stats()


.. The first call will actually sort the list by function name, and the second call
.. will print out the statistics.  The following are some interesting calls to
.. experiment with:

最初の行ではリストを関数名でソートしています。2行目で情報を出力しています。さらに次の内容も試してください。


::

   p.sort_stats('cumulative').print_stats(10)


.. This sorts the profile by cumulative time in a function, and then only prints
.. the ten most significant lines.  If you want to understand what algorithms are
.. taking time, the above line is what you would use.

このようにすると、関数が消費した累計時間でソートして、さらにその上位10件だけを表示します。
どのアルゴリズムが時間を多く消費しているのか知りたいときは、この方法が役に立つはずです。


.. If you were looking to see what functions were looping a lot, and taking a lot
.. of time, you would do:

ループで多くの時間を消費している関数はどれか調べたいときは、次のようにします。


::

   p.sort_stats('time').print_stats(10)


.. to sort according to time spent within each function, and then print the
.. statistics for the top ten functions.

上記はそれぞれの関数で消費された時間でソートして、上位10件の関数の情報が表示されます。


.. You might also try:

次の内容も試してください。


::

   p.sort_stats('file').print_stats('__init__')


.. This will sort all the statistics by file name, and then print out statistics
.. for only the class init methods (since they are spelled with ``__init__`` in
.. them).  As one final example, you could try:

このようにするとファイル名でソートされ、そのうちクラスの初期化メソッド (メソッド名 ``__init__``) に関する統計情報だけが表示されます。


::

   p.sort_stats('time', 'cum').print_stats(.5, 'init')


.. This line sorts statistics with a primary key of time, and a secondary key of
.. cumulative time, and then prints out some of the statistics. To be specific, the
.. list is first culled down to 50% (re: ``.5``) of its original size, then only
.. lines containing ``init`` are maintained, and that sub-sub-list is printed.

上記は時間 (time) をプライマリキー、累計時間 (cumulative time) をセカンダリキーにしてソートした後でさらに条件を絞って統計情報を出力します。
``.5`` は上位 50% だけを選択することを意味し、さらにその中から文字列 ``init`` を含むものだけが表示されます。


.. If you wondered what functions called the above functions, you could now (``p``
.. is still sorted according to the last criteria) do:

どの関数がどの関数を呼び出しているのかを知りたければ、次のようにします (``p`` は最後に実行したときの状態でソートされています)。


::

   p.print_callers(.5, 'init')


.. and you would get a list of callers for each of the listed functions.

このようにすると、関数ごとの呼び出し側関数の一覧が得られます。


.. If you want more functionality, you're going to have to read the manual, or
.. guess what the following functions do:

さらに詳しい機能を知りたければマニュアルを読むか、次の関数の実行結果から内容を推察してください。


::

   p.print_callees()
   p.add('fooprof')


.. Invoked as a script, the :mod:`pstats` module is a statistics browser for
.. reading and examining profile dumps.  It has a simple line-oriented interface
.. (implemented using :mod:`cmd`) and interactive help.

スクリプトとして起動した場合、 :mod:`pstats` モジュールはプロファイルのダンプを読み込み、分析するための統計ブラウザとして動きます。
シンプルな行指向のインタフェース (:mod:`cmd` を使って実装) とヘルプ機能を備えています。


.. _deterministic-profiling:

決定論的プロファイリングとは
=============================

.. :dfn:`Deterministic profiling` is meant to reflect the fact that all *function
.. call*, *function return*, and *exception* events are monitored, and precise
.. timings are made for the intervals between these events (during which time the
.. user's code is executing).  In contrast, :dfn:`statistical profiling` (which is
.. not done by this module) randomly samples the effective instruction pointer, and
.. deduces where time is being spent.  The latter technique traditionally involves
.. less overhead (as the code does not need to be instrumented), but provides only
.. relative indications of where time is being spent.

:dfn:`決定論的プロファイリング` とは、すべての *関数呼び出し*, *関数からのリターン*, *例外発生* をモニターし、
正確なタイミングを記録することで、イベント間の時間、つまりどの時間にユーザコードが実行されているのかを計測するやり方です。
もう一方の :dfn:`統計的プロファイリング` (このモジュールでこの方法は採用していません) とは、
有効なインストラクションポインタからランダムにサンプリングを行い、プログラムのどこで時間が使われているかを推定する方法です。
後者の方法は、オーバヘッドが少ないものの、プログラムのどこで多くの時間が使われているか、その相対的な示唆に留まります。


.. In Python, since there is an interpreter active during execution, the presence
.. of instrumented code is not required to do deterministic profiling.  Python
.. automatically provides a :dfn:`hook` (optional callback) for each event.  In
.. addition, the interpreted nature of Python tends to add so much overhead to
.. execution, that deterministic profiling tends to only add small processing
.. overhead in typical applications.  The result is that deterministic profiling is
.. not that expensive, yet provides extensive run time statistics about the
.. execution of a Python program.

Python の場合、実行中は必ずインタプリタが動作しているため、決定論的プロファイリングを行うにあたり、
計測用にコードを追加する必要はありません。
Python は自動的に各イベントに :dfn:`フック` (オプションのコールバック) を提供します。
加えて Python のインタプリタという性質によって、実行時に大きなオーバーヘッドを伴う傾向がありますが、
それに比べると一般的なアプリケーションでは決定論的プロファイリングで追加される処理のオーバーヘッドは少ない傾向にあります。
結果的に、決定論的プロファイリングは少ないコストで Python プログラムの実行時間に関する詳細な統計を得られる方法となっているのです。


.. Call count statistics can be used to identify bugs in code (surprising counts),
.. and to identify possible inline-expansion points (high call counts).  Internal
.. time statistics can be used to identify "hot loops" that should be carefully
.. optimized.  Cumulative time statistics should be used to identify high level
.. errors in the selection of algorithms.  Note that the unusual handling of
.. cumulative times in this profiler allows statistics for recursive
.. implementations of algorithms to be directly compared to iterative
.. implementations.

呼び出し回数はコード中のバグ発見にも使用できます (とんでもない数の呼び出しが行われている部分)。
インライン拡張の対象とすべき部分を見つけるためにも使えます (呼び出し頻度の高い部分)。
内部時間の統計は、注意深く最適化すべき"ホットループ"の発見にも役立ちます。
累積時間の統計は、アルゴリズム選択に関連した高レベルのエラー検知に役立ちます。
なお、このプロファイラは再帰的なアルゴリズム実装の累計時間を計ることが可能で、
通常のループを使った実装と直接比較することもできるようになっています。


リファレンスマニュアル -- :mod:`profile` と :mod:`cProfile`
=============================================================

.. module:: cProfile
   :synopsis: Python profiler


.. The primary entry point for the profiler is the global function
.. :func:`profile.run` (resp. :func:`cProfile.run`). It is typically used to create
.. any profile information.  The reports are formatted and printed using methods of
.. the class :class:`pstats.Stats`.  The following is a description of all of these
.. standard entry points and functions.  For a more in-depth view of some of the
.. code, consider reading the later section on Profiler Extensions, which includes
.. discussion of how to derive "better" profilers from the classes presented, or
.. reading the source code for these modules.

プロファイラの主要なエントリポイントはグローバル関数 :func:`profile.run` (または :func:`cProfile.run`) です。
この関数は、通常プロファイル情報の作成に使われます。
:class:`pstats.Stats` クラスのメソッドを使ってプロファイル情報を整形出力します。
以下はすべての標準エントリポイントと関数の解説です。
さらにいくつかのコードの詳細を知りたければ、「プロファイラの拡張」を読んでください。
派生クラスを使ってプロファイラを"改善"する方法やモジュールのソースコードの読み方が述べられています。


.. function:: run(command[, filename])

   .. This function takes a single argument that can be passed to the
   .. :keyword:`exec` statement, and an optional file name.  In all cases this
   .. routine attempts to :keyword:`exec` its first argument, and gather profiling
   .. statistics from the execution. If no file name is present, then this function
   .. automatically prints a simple profiling report, sorted by the standard name
   .. string (file/line/function-name) that is presented in each line.  The
   .. following is a typical output from such a call:

   この関数はオプション引数として :keyword:`exec` 文に渡すファイル名を指定できます。
   このルーチンは必ず最初の引数の :keyword:`exec` を試み、実行結果からプロファイル情報を収集しようとします。
   ファイル名が指定されていないときは、各行の標準名文字列 (ファイル名/行数/関数名) でソートされた
   簡単なレポートが表示されます。以下はその出力例です。


   ::

            2706 function calls (2004 primitive calls) in 4.504 CPU seconds

      Ordered by: standard name

      ncalls  tottime  percall  cumtime  percall filename:lineno(function)
           2    0.006    0.003    0.953    0.477 pobject.py:75(save_objects)
        43/3    0.533    0.012    0.749    0.250 pobject.py:99(evaluate)
       ...


   .. The first line indicates that 2706 calls were monitored.  Of those calls, 2004
   .. were :dfn:`primitive`.  We define :dfn:`primitive` to mean that the call was not
   .. induced via recursion. The next line: ``Ordered by: standard name``, indicates
   .. that the text string in the far right column was used to sort the output. The
   .. column headings include:

   最初の行は2706回の関数呼び出しがあったことを示しています。このうち2004回は :dfn:`プリミティブ` なものです。
   :dfn:`プリミティブ` な呼び出しとは、再帰によるものではない関数呼び出しを指します。
   次の行 ``Ordered by: standard name`` は、一番右側の欄の文字列を使ってソートされたことを意味します。
   各カラムの見出しの意味は次の通りです。


   .. ncalls
   ..    for the number of calls,

   ncalls
      呼び出し回数


   .. tottime
   ..    for the total time spent in the given function (and excluding time made in calls
   ..    to sub-functions),

   tottime
      この関数が消費した時間の合計 (下位の関数呼び出しの時間は除く)


   .. percall
   ..    is the quotient of ``tottime`` divided by ``ncalls``

   percall
      ``tottime`` を ``ncalls`` で割った値


   .. cumtime
   ..    is the total time spent in this and all subfunctions (from invocation till
   ..    exit). This figure is accurate *even* for recursive functions.

   cumtime
      下位の関数を含むこの関数の (実行開始から終了までの) 消費時間の合計。
      この項目は再帰的な関数においても正確に計測されます。


   .. percall
   ..    is the quotient of ``cumtime`` divided by primitive calls

   percall
      ``cumtime`` をプリミティブな呼び出し回数で割った値


   .. filename:lineno(function)
   ..    provides the respective data of each function

   filename:lineno(function)
      その関数のファイル名、行番号、関数名


   .. When there are two numbers in the first column (for example, ``43/3``), then the
   .. latter is the number of primitive calls, and the former is the actual number of
   .. calls.  Note that when the function does not recurse, these two values are the
   .. same, and only the single figure is printed.

   (``43/3`` など) 最初の欄に2つの数字が表示されている場合、最初の値は呼び出し回数、
   2番目はプリミティブな呼び出しの回数を表しています。
   関数が再帰していない場合はどちらの回数も同じになるため、1つの数値しか表示されません。


.. function:: runctx(command, globals, locals[, filename])

   .. This function is similar to :func:`run`, with added arguments to supply the
   .. globals and locals dictionaries for the *command* string.

   この関数は :func:`run` に似ていますが、 *command* 文字列に対するグローバル辞書とローカル辞書の引数が追加されています。


.. Analysis of the profiler data is done using the :class:`Stats` class.

プロファイラデータの分析は :class:`Stats` クラスを使って行います。


.. note::

   .. The :class:`Stats` class is defined in the :mod:`pstats` module.

   :class:`Stats` クラスは :mod:`pstats` モジュールで定義されています。


.. .. module:: pstats
..    :synopsis: Statistics object for use with the profiler.

.. module:: pstats
   :synopsis: プロファイラで用いる統計情報オブジェクト


.. class:: Stats(filename[, stream=sys.stdout[, ...]])

   .. This class constructor creates an instance of a "statistics object" from a
   .. *filename* (or set of filenames).  :class:`Stats` objects are manipulated by
   .. methods, in order to print useful reports.  You may specify an alternate output
   .. stream by giving the keyword argument, ``stream``.

   このコンストラクタは *filename* で指定した (単一または複数の) ファイルから
   "統計情報オブジェクト"のインスタンスを生成します。
   :class:`Stats` オブジェクトはレポートを出力するメソッドを通じて操作します。
   また、他の出力ストリームをキーワード引数 ``stream`` で指定できます。


   .. The file selected by the above constructor must have been created by the
   .. corresponding version of :mod:`profile` or :mod:`cProfile`.  To be specific,
   .. there is *no* file compatibility guaranteed with future versions of this
   .. profiler, and there is no compatibility with files produced by other profilers.
   .. If several files are provided, all the statistics for identical functions will
   .. be coalesced, so that an overall view of several processes can be considered in
   .. a single report.  If additional files need to be combined with data in an
   .. existing :class:`Stats` object, the :meth:`add` method can be used.

   上記コンストラクタで指定するファイルは、使用する :class:`Stats` に対応したバージョンの
   :mod:`profile` または :mod:`cProfile` で作成されたものでなければなりません。
   将来のバージョンのプロファイラとの互換性は *保証されておらず* 、
   他のプロファイラとの互換性もないことに注意してください。
   複数のファイルを指定した場合、同一の関数の統計情報はすべて合算され、
   複数のプロセスで構成される全体をひとつのレポートで検証することが可能になります。
   既存の :class:`Stats` オブジェクトに別のファイルの情報を追加するときは、
   :meth:`add` メソッドを使用します。


   .. (such as the old system profiler).

   .. (旧バージョンのものなど)


   .. .. versionchanged:: 2.5
   ..    The *stream* parameter was added.

   .. versionchanged:: 2.5
      *stream* 引数が追加されました.


.. _profile-stats:

:class:`Stats` クラス
---------------------

.. :class:`Stats` objects have the following methods:

:class:`Stats` には次のメソッドがあります。


.. method:: Stats.strip_dirs()

   .. This method for the :class:`Stats` class removes all leading path information
   .. from file names.  It is very useful in reducing the size of the printout to fit
   .. within (close to) 80 columns.  This method modifies the object, and the stripped
   .. information is lost.  After performing a strip operation, the object is
   .. considered to have its entries in a "random" order, as it was just after object
   .. initialization and loading.  If :meth:`strip_dirs` causes two function names to
   .. be indistinguishable (they are on the same line of the same filename, and have
   .. the same function name), then the statistics for these two entries are
   .. accumulated into a single entry.

   :class:`Stats` クラスのこのメソッドは、ファイル名の前に付いているすべてのパス情報を取り除くためのものです。
   出力の幅を80文字以内に収めたいときに重宝します。このメソッドはオブジェクトを変更するため、取り除いたパス情報は失われます。
   パス情報除去の操作後、オブジェクトが保持するデータエントリは、オブジェクトの初期化、ロード直後と同じように"ランダムに"並んでいます。
   :meth:`strip_dirs` を実行した結果、2つの関数名が区別できない (両者が同じファイルの同じ行番号で同じ関数名となった) 場合、
   一つのエントリに合算されされます。


.. method:: Stats.add(filename[, ...])

   .. This method of the :class:`Stats` class accumulates additional profiling
   .. information into the current profiling object.  Its arguments should refer to
   .. filenames created by the corresponding version of :func:`profile.run` or
   .. :func:`cProfile.run`. Statistics for identically named (re: file, line, name)
   .. functions are automatically accumulated into single function statistics.

   :class:`Stats` クラスのこのメソッドは、既存のプロファイリングオブジェクトに情報を追加します。
   引数は対応するバージョンの :func:`profile.run` または :func:`cProfile.run` によって生成されたファイルの名前でなくてはなりません。
   関数の名前が区別できない (ファイル名、行番号、関数名が同じ) 場合、一つの関数の統計情報として合算されます。


.. method:: Stats.dump_stats(filename)

   .. Save the data loaded into the :class:`Stats` object to a file named *filename*.
   .. The file is created if it does not exist, and is overwritten if it already
   .. exists.  This is equivalent to the method of the same name on the
   .. :class:`profile.Profile` and :class:`cProfile.Profile` classes.

   :class:`Stats` オブジェクトに読み込まれたデータを、ファイル名 *filename* のファイルに保存します。
   ファイルが存在しない場合は新たに作成され、すでに存在する場合には上書きされます。
   このメソッドは :class:`profile.Profile` クラスおよび :class:`cProfile.Profile` クラスの同名のメソッドと等価です。


   .. versionadded:: 2.3


.. method:: Stats.sort_stats(key[, ...])

   .. This method modifies the :class:`Stats` object by sorting it according to the
   .. supplied criteria.  The argument is typically a string identifying the basis of
   .. a sort (example: ``'time'`` or ``'name'``).

   このメソッドは :class:`Stats` オブジェクトを指定した基準に従ってソートします。
   典型的には引数にソートのキーにしたい項目を示す文字列を指定します (例: ``'time'`` や ``'name'`` など)。


   .. When more than one key is provided, then additional keys are used as secondary
   .. criteria when there is equality in all keys selected before them.  For example,
   .. ``sort_stats('name', 'file')`` will sort all the entries according to their
   .. function name, and resolve all ties (identical function names) by sorting by
   .. file name.

   2つ以上のキーが指定された場合、2つ目以降のキーは、それ以前のキーで等価となったデータエントリの再ソートに使われます。
   たとえば ``sort_stats('name', 'file')`` とした場合、まずすべてのエントリが関数名でソートされた後、
   同じ関数名で複数のエントリがあればファイル名でソートされます。


   .. Abbreviations can be used for any key names, as long as the abbreviation is
   .. unambiguous.  The following are the keys currently defined:

   キー名には他のキーと判別可能である限り綴りを省略して名前を指定できます。
   現在のバージョンで定義されているキー名は以下の通りです。


   .. +------------------+----------------------+
   .. | Valid Arg        | Meaning              |
   .. +==================+======================+
   .. | ``'calls'``      | call count           |
   .. +------------------+----------------------+
   .. | ``'cumulative'`` | cumulative time      |
   .. +------------------+----------------------+
   .. | ``'file'``       | file name            |
   .. +------------------+----------------------+
   .. | ``'module'``     | file name            |
   .. +------------------+----------------------+
   .. | ``'pcalls'``     | primitive call count |
   .. +------------------+----------------------+
   .. | ``'line'``       | line number          |
   .. +------------------+----------------------+
   .. | ``'name'``       | function name        |
   .. +------------------+----------------------+
   .. | ``'nfl'``        | name/file/line       |
   .. +------------------+----------------------+
   .. | ``'stdname'``    | standard name        |
   .. +------------------+----------------------+
   .. | ``'time'``       | internal time        |
   .. +------------------+----------------------+

   +------------------+------------------------------+
   | 正式名           | 内容                         |
   +==================+==============================+
   | ``'calls'``      | 呼び出し回数                 |
   +------------------+------------------------------+
   | ``'cumulative'`` | 累積時間                     |
   +------------------+------------------------------+
   | ``'file'``       | ファイル名                   |
   +------------------+------------------------------+
   | ``'module'``     | モジュール名                 |
   +------------------+------------------------------+
   | ``'pcalls'``     | プリミティブな呼び出し回数   |
   +------------------+------------------------------+
   | ``'line'``       | 行番号                       |
   +------------------+------------------------------+
   | ``'name'``       | 関数名                       |
   +------------------+------------------------------+
   | ``'nfl'``        | 関数名/ファイル名/行番号     |
   +------------------+------------------------------+
   | ``'stdname'``    | 標準名                       |
   +------------------+------------------------------+
   | ``'time'``       | 内部時間                     |
   +------------------+------------------------------+


   .. Note that all sorts on statistics are in descending order (placing most time
   .. consuming items first), where as name, file, and line number searches are in
   .. ascending order (alphabetical). The subtle distinction between ``'nfl'`` and
   .. ``'stdname'`` is that the standard name is a sort of the name as printed, which
   .. means that the embedded line numbers get compared in an odd way.  For example,
   .. lines 3, 20, and 40 would (if the file names were the same) appear in the string
   .. order 20, 3 and 40.  In contrast, ``'nfl'`` does a numeric compare of the line
   .. numbers.  In fact, ``sort_stats('nfl')`` is the same as ``sort_stats('name',
   .. 'file', 'line')``.

   すべての統計情報のソート結果は降順 (最も多く時間を消費したものが一番上に来る) となることに注意してください。
   ただし、関数名、ファイル名、行数に関しては昇順 (アルファベット順) になります。
   ``'nfl'`` と  ``'stdname'`` には微妙な違いがあります。
   標準名 (standard name) とは表示された名前によるソートで、埋め込まれた行番号のソート順が特殊です。
   たとえば、 (ファイル名が同じで) 3、20、40という行番号のエントリがあった場合、20、3、40 の順に表示されます。
   一方 ``'nfl'`` は行番号を数値として比較します。
   要するに、 ``sort_stats('nfl')`` は ``sort_stats('name', 'file', 'line')`` と指定した場合と同じになります。


   .. For backward-compatibility reasons, the numeric arguments ``-1``, ``0``, ``1``,
   .. and ``2`` are permitted.  They are interpreted as ``'stdname'``, ``'calls'``,
   .. ``'time'``, and ``'cumulative'`` respectively.  If this old style format
   .. (numeric) is used, only one sort key (the numeric key) will be used, and
   .. additional arguments will be silently ignored.

   後方互換性のため、数値を引数に使った  ``-1``, ``0``, ``1``, ``2`` の形式もサポートしています。
   それぞれ ``'stdname'``, ``'calls'``, ``'time'``, ``'cumulative'`` として処理されます。
   引数をこの旧スタイルで指定した場合、最初のキー (数値キー) だけが使われ、複数のキーを指定しても2番目以降は無視されます。


   .. For compatibility with the old profiler,

   .. 旧バージョンのプロファイラとの互換性のため、


.. method:: Stats.reverse_order()

   .. This method for the :class:`Stats` class reverses the ordering of the basic list
   .. within the object.  Note that by default ascending vs descending order is
   .. properly selected based on the sort key of choice.

   :class:`Stats` クラスのこのメソッドは、オブジェクト内の情報のリストを逆順にソートします。
   デフォルトでは選択したキーに応じて昇順、降順が適切に選ばれることに注意してください。


   .. This method is provided primarily for compatibility with the old profiler.

   .. これは旧プロファイラとの互換性のために用意されています。


.. method:: Stats.print_stats([restriction, ...])

   .. This method for the :class:`Stats` class prints out a report as described in the
   .. :func:`profile.run` definition.

   :class:`Stats` クラスのこのメソッドは、 :func:`profile.run` の項で述べたプロファイルのレポートを出力します。


   .. The order of the printing is based on the last :meth:`sort_stats` operation done
   .. on the object (subject to caveats in :meth:`add` and :meth:`strip_dirs`).

   出力するデータの順序はオブジェクトに対し最後に行った :meth:`sort_stats` による操作に基づきます
   (:meth:`add` と :meth:`strip_dirs` による制限にも支配されます)。


   .. The arguments provided (if any) can be used to limit the list down to the
   .. significant entries.  Initially, the list is taken to be the complete set of
   .. profiled functions.  Each restriction is either an integer (to select a count of
   .. lines), or a decimal fraction between 0.0 and 1.0 inclusive (to select a
   .. percentage of lines), or a regular expression (to pattern match the standard
   .. name that is printed; as of Python 1.5b1, this uses the Perl-style regular
   .. expression syntax defined by the :mod:`re` module).  If several restrictions are
   .. provided, then they are applied sequentially.  For example:

   引数は (もし与えられると) リストを重要なエントリのみに制限するために使われます。
   初期段階でリストはプロファイルした関数の完全な情報を持っています。
   制限の指定は、 (行数を指定する) 整数、 (行のパーセンテージを指定する) 0.0 から 1.0 までの割合を指定する小数、
   (出力する standard name にマッチする) 正規表現のいずれかを使って行います。
   正規表現は Python 1.5b1 で導入された :mod:`re` モジュールで使える Perl スタイルのものです。
   複数の制限が指定された場合、指定の順に適用されます。たとえば次のようになります。


   ::

      print_stats(.1, 'foo:')


   .. would first limit the printing to first 10% of list, and then only print
   .. functions that were part of filename :file:`.\*foo:`.  In contrast, the
   .. command:

   上記の場合まず出力するリストは全体の10%に制限され、
   さらにファイル名の一部に文字列 :file:`.\*foo:` を持つ関数だけが出力されます。


   ::

      print_stats('foo:', .1)


   .. would limit the list to all functions having file names :file:`.\*foo:`, and
   .. then proceed to only print the first 10% of them.

   こちらの例の場合、リストはまずファイル名に :file:`.\*foo:` を持つ関数だけに制限され、
   その中の最初の 10% だけが出力されます。


.. method:: Stats.print_callers([restriction, ...])

   .. This method for the :class:`Stats` class prints a list of all functions that
   .. called each function in the profiled database.  The ordering is identical to
   .. that provided by :meth:`print_stats`, and the definition of the restricting
   .. argument is also identical.  Each caller is reported on its own line.  The
   .. format differs slightly depending on the profiler that produced the stats:

   :class:`Stats` クラスのこのメソッドは、プロファイルのデータベースの中から何らかの関数呼び出しを行った関数をすべて出力します。
   出力の順序は :meth:`print_stats` によって与えられるものと同じです。出力を制限する引数も同じです。
   各呼び出し側関数についてそれぞれ一行ずつ表示されます。
   フォーマットは統計を作り出したプロファイラごとに微妙に異なります。


   .. * With :mod:`profile`, a number is shown in parentheses after each caller to
   ..   show how many times this specific call was made.  For convenience, a second
   ..   non-parenthesized number repeats the cumulative time spent in the function
   ..   at the right.

   * :mod:`profile` の場合、呼び出し側関数の後に括弧で囲まれて表示される数値はその呼び出しが何回行われたかを示しています。
     利便性のため、 2番目の括弧なしで表示される数値によって、関数が消費した累積時間を表しています。


   .. * With :mod:`cProfile`, each caller is preceded by three numbers: the number of
   ..   times this specific call was made, and the total and cumulative times spent in
   ..   the current function while it was invoked by this specific caller.

   * :mod:`cProfile` の場合、各呼び出し側関数の後に3つの数字が付きます。
     呼び出しが何回行われたかと、この呼び出し側関数からの呼び出しによって現在の関数内で消費された合計時間および累積時間です。


.. method:: Stats.print_callees([restriction, ...])

   .. This method for the :class:`Stats` class prints a list of all function that were
   .. called by the indicated function.  Aside from this reversal of direction of
   .. calls (re: called vs was called by), the arguments and ordering are identical to
   .. the :meth:`print_callers` method.

   :class:`Stats` クラスのこのメソッドは、指定した関数から呼び出された関数のリストを出力します。
   呼び出し側、呼び出される側の方向は逆ですが、引数と出力の順序に関しては :meth:`print_callers` と同じです。


.. _profile-limits:

制限事項
========

.. One limitation has to do with accuracy of timing information. There is a
.. fundamental problem with deterministic profilers involving accuracy.  The most
.. obvious restriction is that the underlying "clock" is only ticking at a rate
.. (typically) of about .001 seconds.  Hence no measurements will be more accurate
.. than the underlying clock.  If enough measurements are taken, then the "error"
.. will tend to average out. Unfortunately, removing this first error induces a
.. second source of error.

一つの制限はタイミング情報の正確さに関するものです。決定論的プロファイラには正確さに関する根本的問題があります。
最も明白な制限は、 (一般に) "クロック"は .001 秒の精度しかないということです。これ以上の精度で計測することはできません。
仮に充分な精度が得られたとしても、"誤差"が計測の平均値に影響を及ぼすことがあります。
この最初の誤差を取り除いたとしても、それがまた別の誤差を引き起こす原因となります。


.. The second problem is that it "takes a while" from when an event is dispatched
.. until the profiler's call to get the time actually *gets* the state of the
.. clock.  Similarly, there is a certain lag when exiting the profiler event
.. handler from the time that the clock's value was obtained (and then squirreled
.. away), until the user's code is once again executing.  As a result, functions
.. that are called many times, or call many functions, will typically accumulate
.. this error. The error that accumulates in this fashion is typically less than
.. the accuracy of the clock (less than one clock tick), but it *can* accumulate
.. and become very significant.

もう一つの問題として、イベントを検知してからプロファイラがその時刻を実際に *取得* するまでに "いくらかの時間がかかる" ことです。
同様に、イベントハンドラが終了する時にも、時刻を取得して (そしてその値を保存して) から、
ユーザコードが処理を再開するまでの間に遅延が発生します。
結果的に多く呼び出される関数または多数の関数から呼び出される関数の情報にはこの種の誤差が蓄積する傾向にあります。
このようにして蓄積される誤差は、典型的にはクロックの精度を下回ります (1クロック以下) が、
一方でこの時間が累計して非常に大きな値になることも *あり得ます* 。


.. The problem is more important with :mod:`profile` than with the lower-overhead
.. :mod:`cProfile`.  For this reason, :mod:`profile` provides a means of
.. calibrating itself for a given platform so that this error can be
.. probabilistically (on the average) removed. After the profiler is calibrated, it
.. will be more accurate (in a least square sense), but it will sometimes produce
.. negative numbers (when call counts are exceptionally low, and the gods of
.. probability work against you :-). )  Do *not* be alarmed by negative numbers in
.. the profile.  They should *only* appear if you have calibrated your profiler,
.. and the results are actually better than without calibration.

この問題はオーバーヘッドの小さい :mod:`cProfile` よりも :mod:`profile` においてより重要です。
そのため、 :mod:`profile` は誤差が確率的に (平均値で) 減少するようにプラットフォームごとに補正する機能を備えています。
プロファイラに補正を施すと (最小二乗の意味で) 正確さが増しますが、ときには数値が負の値になってしまうこともあります
(呼び出し回数が極めて少なく、確率の神があなたに意地悪をしたとき :-) )。
プロファイルの結果に負の値が出力されても *驚かないでください* 。
これは補正を行った場合にのみ生じることで、補正を行わない場合に比べて計測結果は実際にはより正確になっているはずだからです。


.. _profile-calibration:

キャリブレーション (補正)
=========================

.. The profiler of the :mod:`profile` module subtracts a constant from each event
.. handling time to compensate for the overhead of calling the time function, and
.. socking away the results.  By default, the constant is 0. The following
.. procedure can be used to obtain a better constant for a given platform (see
.. discussion in section Limitations above).

:mod:`profile` のプロファイラは time 関数呼び出しおよびその値を保存するためのオーバーヘッドを補正するために、
各イベントの処理時間から定数を引きます。
デフォルトでこの定数の値は 0 です。以下の手順で、プラットフォームに合った、より適切な定数が得られます (前節「制限事項」の説明を参照)。


::

   import profile
   pr = profile.Profile()
   for i in range(5):
       print pr.calibrate(10000)


.. The method executes the number of Python calls given by the argument, directly
.. and again under the profiler, measuring the time for both. It then computes the
.. hidden overhead per profiler event, and returns that as a float.  For example,
.. on an 800 MHz Pentium running Windows 2000, and using Python's time.clock() as
.. the timer, the magical number is about 12.5e-6.

calibrate メソッドは引数として与えられた数だけ Python の呼び出しを行います。
直接呼び出す場合と、プロファイラを使って呼び出す場合の両方が実施され、それぞれの時間が計測されます。
その結果、プロファイラのイベントに隠されたオーバーヘッドが計算され、その値は浮動小数として返されます。
たとえば、 800 MHz の Pentium で Windows 2000 を使用、 Python の time.clock() をタイマとして使った場合、
値はおよそ 12.5e-6 となります。


.. The object of this exercise is to get a fairly consistent result. If your
.. computer is *very* fast, or your timer function has poor resolution, you might
.. have to pass 100000, or even 1000000, to get consistent results.

この手順で使用しているオブジェクトはほぼ一定の結果を返します。
*非常に* 早いコンピュータを使う場合、もしくはタイマの性能が貧弱な場合は、
一定の結果を得るために引数に 100000 や 1000000 といった大きな値を指定する必要があるかもしれません。


.. When you have a consistent answer, there are three ways you can use it: [#]_ :

一定の結果が得られたら、それを使う方法には3通りあります。 [#]_


::

   import profile

   # 1. 算出した補正値 (your_computed_bias) をこれ以降生成する
   #    Profile インスタンスに適用する。
   profile.Profile.bias = your_computed_bias

   # 2. 特定の Profile インスタンスに補正値を適用する。
   pr = profile.Profile()
   pr.bias = your_computed_bias

   # 3. インスタンスのコンストラクタに補正値を指定する。
   pr = profile.Profile(bias=your_computed_bias)


.. If you have a choice, you are better off choosing a smaller constant, and then
.. your results will "less often" show up as negative in profile statistics.

選択肢がある場合は、補正値は小さめに設定した方が良いでしょう。
プロファイルの結果に負の値が表われる"頻度が低く"なるはずです。


.. _profiler extensions:

拡張 --- プロファイラの改善
===========================

.. The :class:`Profile` class of both modules, :mod:`profile` and :mod:`cProfile`,
.. were written so that derived classes could be developed to extend the profiler.
.. The details are not described here, as doing this successfully requires an
.. expert understanding of how the :class:`Profile` class works internally.  Study
.. the source code of the module carefully if you want to pursue this.

:mod:`profile` モジュールおよび :mod:`cProfile` モジュールの :class:`Profile` クラスは、
プロファイラの機能を拡張するために派生クラスを作成することを前提に書かれています。
しかしその方法を説明するには、 :class:`Profile` の内部動作について詳細な解説が必要となるため、ここでは述べません。
もし拡張を行いたいのであれば、使用するモジュールのソースを注意深く読む必要があります。


.. If all you want to do is change how current time is determined (for example, to
.. force use of wall-clock time or elapsed process time), pass the timing function
.. you want to the :class:`Profile` class constructor:

プロファイラが時刻を取得する方法を変更したいだけなら (たとえば、実測時間やプロセスの経過時間を使いたい場合)、
時刻取得用の関数を :class:`Profile` クラスのコンストラクタに指定することができます。


::

   pr = profile.Profile(your_time_func)


.. The resulting profiler will then call :func:`your_time_func`.

この結果生成されるプロファイラは時刻取得に :func:`your_time_func` を呼び出すようになります。


:class:`profile.Profile`
   .. :func:`your_time_func` should return a single number, or a list of numbers whose
   .. sum is the current time (like what :func:`os.times` returns).  If the function
   .. returns a single time number, or the list of returned numbers has length 2, then
   .. you will get an especially fast version of the dispatch routine.

   :func:`your_time_func` は単一の数値、あるいは (:func:`os.times` と同じように)
   その合計が累計時間を示すリストを返すようになっていなければなりません。
   関数が1つの数値、あるいは長さ2の数値のリストを返すようになっていれば、
   ディスパッチルーチンには特別な高速化バージョンが使われます。


   .. Be warned that you should calibrate the profiler class for the timer function
   .. that you choose.  For most machines, a timer that returns a lone integer value
   .. will provide the best results in terms of low overhead during profiling.
   .. (:func:`os.times` is *pretty* bad, as it returns a tuple of floating point
   .. values).  If you want to substitute a better timer in the cleanest fashion,
   .. derive a class and hardwire a replacement dispatch method that best handles your
   .. timer call, along with the appropriate calibration constant.

   選択する時刻取得関数によって、プロファイラクラスを補正する必要があることに注意してください。
   多くのマシンにおいて、プロファイル時のオーバヘッドを少なくする方法として、タイマは長整数を返すのが最善です
   (:func:`os.times` は浮動小数のタプルを返すので *おすすめできません*)。
   タイマをより正確なものに置き換えたいならば、派生クラスでそのディスパッチメソッドを適切なタイマ呼び出しと適切な補正を行うように書き直す必要があります。


:class:`cProfile.Profile`
   .. :func:`your_time_func` should return a single number.  If it returns plain
   .. integers, you can also invoke the class constructor with a second argument
   .. specifying the real duration of one unit of time.  For example, if
   .. :func:`your_integer_time_func` returns times measured in thousands of seconds,
   .. you would constuct the :class:`Profile` instance as follows:

   :func:`your_time_func` は単一の数値を返さなければなりません。
   もしこれが整数を返す関数ならば、2番目の引数に単位時間当たりの実際の持続時間を指定して
   クラスのコンスタラクタを呼び出すことができます。
   たとえば、 :func:`your_integer_time_func` が1000分の1秒単位で計測した時間を返すとすると、
   :class:`Profile` インスタンスを次のように生成することができます。


   ::

      pr = profile.Profile(your_integer_time_func, 0.001)


   .. As the :mod:`cProfile.Profile` class cannot be calibrated, custom timer
   .. functions should be used with care and should be as fast as possible.  For the
   .. best results with a custom timer, it might be necessary to hard-code it in the C
   .. source of the internal :mod:`_lsprof` module.

   :mod:`cProfile.Profile` クラスはキャリブレーションができないので、自前のタイマ関数は注意を払って使う必要があり、
   またそれは可能な限り速くなければなりません。
   自前のタイマ関数で最高の結果を得るには、 :mod:`_lsprof` 内部モジュールの C ソースファイルにハードコードする必要があるかもしれません。


.. rubric:: 注記

.. .. [#] Updated and converted to LaTeX by Guido van Rossum. Further updated by Armin
..    Rigo to integrate the documentation for the new :mod:`cProfile` module of Python
..    2.5.

.. [#] アップデートと LaTeX への変換は  Guido van Rossum によるもの。さらに Python 2.5 の新しい :mod:`cProfile`
   モジュールの文書を統合するアップデートは Armin Rigo による。


.. .. [#] Prior to Python 2.2, it was necessary to edit the profiler source code to embed
..    the bias as a literal number.  You still can, but that method is no longer
..    described, because no longer needed.

.. [#] Python 2.2 より前のバージョンではプロファイラのソースコードに補正値として埋め込まれた定数を直接編集する必要がありました。
   今でも同じことは可能ですが、その方法は説明しません。なぜなら、もうソースを編集する必要がないからです。

