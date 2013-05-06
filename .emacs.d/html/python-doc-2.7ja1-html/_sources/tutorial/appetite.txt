.. _tut-intro:

****************
やる気を高めよう
****************

コンピュータを使って様々な作業をしていたら、自動化したい作業が出てくるでしょう。
たとえば、たくさんのテキストファイルで検索-置換操作を行いたい、大量の写真ファイルを
込み入ったやりかたでリネームまたは整理したいといったものです。
ひょっとすると、小さなカスタムデータベースや、何かに特化したGUIアプリケーション、シンプルなゲームを作りたいかもしれません。

.. % % Whetting Your Appetite
.. % % If you do much work on computers, eventually you find that there's
.. % % some task you'd like to automate.  For example, you may wish to
.. % % perform a search-and-replace over a large number of text files, or
.. % % rename and rearrange a bunch of photo files in a complicated way.
.. % % Perhaps you'd like to write a small custom database, or a specialized
.. % % GUI application, or a simple game.

.. % % If you're a professional software developer, you may have to work with
.. % % several C/\Cpp/Java libraries but find the usual
.. % % write/compile/test/re-compile cycle is too slow.  Perhaps you're
.. % % writing a test suite for such a library and find writing the testing
.. % % code a tedious task.  Or maybe you've written a program that could use
.. % % an extension language, and you don't want to design and implement a
.. % % whole new language for your application.

もしあなたがプロのソフト開発者なら、C/C++/Java ライブラリを扱う必要があって、
通常の write/compile/test/re-compile サイクルが遅すぎると感じるかもしれません。
ひょっとするとそのようなライブラリのテストスイートを書いていて、
テスト用のコードを書くのにうんざりしているかもしれません。
拡張言語を使えるプログラムを書いていて、アプリケーションのために新しい
言語一式の設計と実装をしたくないと思っているかもしれません。

Pythonはそんなあなたのための言語です。

.. % % Python is just the language for you.

それらの作業の幾つかは、 Unix シェルスクリプトや Windows バッチファイルで
書くこともできますが、シェルスクリプトはファイル操作やテキストデータの操作には
向いているもののGUIアプリケーションやゲームにはむいていません。
C/C++/Java プログラムを書くこともできますが、最初の試し書きにすらかなりの
時間がかかってしまいます。Pythonは簡単に利用でき、Windows、 Mac OS X、
そして Unix オペレーティングシステムで動作し、あなたの作業を素早く行う助けになるでしょう。

.. % % You could write a {\UNIX} shell script or Windows batch files for some
.. % % +of these tasks, but shell scripts are best at moving around files and
.. % % +changing text data, not well-suited for GUI applications or games.
.. % % +You could write a C/{\Cpp}/Java program, but it can take a lot of
.. % % +development time to get even a first-draft program.  Python is simpler
.. % % +to use, available on Windows, MacOS X, and {\UNIX} operating systems,
.. % % +and will help you get the job done more quickly.

.. % % Python is simple to use, but it is a real programming language,
.. % % offering much more structure and support for large programs than shell
.. % % scripts or batch files can offer.  On the other hand, Python also
.. % % offers much more error checking than C, and, being a
.. % % \emph{very-high-level language}, it has high-level data types built
.. % % in, such as flexible arrays and dictionaries.  Because of its more
.. % % general data types Python is applicable to a much larger problem
.. % % domain than Awk or even Perl, yet many things are at
.. % % least as easy in Python as in those languages.

Pyhon は簡単に利用できますが、本物のプログラミング言語であり、シェルスクリプトや
バッチファイルで提供されるよりもたくさんの、大規模プログラム向けの構造やサポートを
提供しています。
一方、 Python は C よりたくさんのエラーチェックを提供しており、
*超高級言語(very-high-level language)* であり、可変長配列や辞書などの高級な
型を組込みで持っています。
そのような型は一般的なため、 Python は Awk や Perl が扱うものより (多くの場合、
少なくともそれらの言語と同じくらい簡単に)大規模な問題に利用できます。

Python ではプログラムをモジュールに分割して他の Python プログラムで再利用できます。
Python には膨大な標準モジュールが付属していて、プログラムを作る上での基盤として、
あるいは Python プログラミングを学ぶためのサンプルとして利用できます。
組み込みモジュールではまた、ファイル I/O 、システムコール、ソケットといった機能や、
Tk のようなグラフィカルユーザインタフェースツールキットを使うためのインタフェース
なども提供しています。

.. % % Python allows you to split your program in modules that can be
.. % % reused in other Python programs.  It comes with a large collection of
.. % % standard modules that you can use as the basis of your programs --- or
.. % % as examples to start learning to program in Python.  Some of these
.. % % modules provide things like file I/O, system calls,
.. % % sockets, and even interfaces to graphical user interface toolkits like Tk.

Python はインタプリタ言語です。コンパイルやリンクが必要ないので、プログラムを開発する際にかなりの時間を節約できます。
インタプリタは対話的な使い方もできます。
インタプリタは対話的にも使えるので、言語の様々な機能について実験してみたり、書き捨てのプログラムを書いたり、
ボトムアップでプログラムを開発する際に関数をテストしたりといったことが簡単にできます。便利な電卓にもなります。

.. % % Python is an interpreted language, which can save you considerable time
.. % % during program development because no compilation and linking is
.. % % necessary.  The interpreter can be used interactively, which makes it
.. % % easy to experiment with features of the language, to write throw-away
.. % % programs, or to test functions during bottom-up program development.
.. % % It is also a handy desk calculator.

Python では、とてもコンパクトで読みやすいプログラムを書けます。
Python で書かれたプログラムは大抵、同じ機能を提供する C 言語, C++ 言語や
Java のプログラムよりもはるかに短くなります。
これには以下のようないくつかの理由があります:

* 高レベルのデータ型によって、複雑な操作を一つの実行文で表現できます。

* 実行文のグループ化を、グループの開始や終了の括弧ではなくインデントで行えます。

* 変数や引数の宣言が不要です。

Python は *拡張* できます: C 言語でプログラムを書く方法を知っているなら、
新たな組み込み関数やモジュールを簡単にインタプリタに追加できます。
これによって、処理速度を決定的に左右する操作を最大速度で動作するように実現したり、(ベンダ特有のグラフィクスライブラリのように) バイナリ
形式でしか手に入らないライブラリを Python にリンクしたりできます。
その気になれば、Python インタプリタを C で書かれたアプリケーションにリンク
して、アプリケーションに対する拡張言語や命令言語としても使えます。

.. % % Python is \emph{extensible}: if you know how to program in C it is easy
.. % % to add a new built-in function or module to the interpreter, either to
.. % % perform critical operations at maximum speed, or to link Python
.. % % programs to libraries that may only be available in binary form (such
.. % % as a vendor-specific graphics library).  Once you are really hooked,
.. % % you can link the Python interpreter into an application written in C
.. % % and use it as an extension or command language for that application.

ところで、この言語は BBC のショー番組、"モンティパイソンの空飛ぶサーカス (Monty Python's Flying Circus)"
から取ったもので、爬虫類とは関係ありません。
このドキュメントでは、モンティパイソンの寸劇への参照が許可されているだけでなく、
むしろ推奨されています！

.. % % By the way, the language is named after the BBC show ``Monty Python's
.. % % Flying Circus'' and has nothing to do with nasty reptiles.  Making
.. % % references to Monty Python skits in documentation is not only allowed,
.. % % it is encouraged!

.. % Where From Here
.. % % Now that you are all excited about Python, you'll want to examine it
.. % % in some more detail.  Since the best way to learn a language is
.. % % to use it, the tutorial invites you to play with the Python interpreter
.. % % as you read.

さて、皆さんはもう Python にワクワクして、もうちょっと詳しく調べてみたくなったはずです。
プログラミング言語を習得する最良の方法は使ってみることですから、
このチュートリアルではみなさんが読んだ内容を Python インタプリタで試してみることをおすすめします。

次の章では、まずインタプリタを使うための機微を説明します。
これはさして面白みのない情報なのですが、後に説明する例題を試してみる上で不可欠なことです。

.. % % In the next chapter, the mechanics of using the interpreter are
.. % % explained.  This is rather mundane information, but essential for
.. % % trying out the examples shown later.

チュートリアルの残りの部分では、Python プログラム言語と実行システムの様々な
機能を例題を交えて紹介します。
単純な式、実行文、データ型から始めて、関数とモジュールを経て、最後には
例外処理やユーザ定義クラスといったやや高度な概念にも触れます。

.. % % The rest of the tutorial introduces various features of the Python
.. % % language and system through examples, beginning with simple
.. % % expressions, statements and data types, through functions and modules,
.. % % and finally touching upon advanced concepts like exceptions
.. % % and user-defined classes.


