
:mod:`user` --- ユーザー設定のフック
====================================

.. module:: user
   :synopsis: ユーザー設定を参照するための標準的な方法を提供するモジュール
   :deprecated:

.. deprecated:: 2.6
   :mod:`user` モジュールはPython 3.0では削除されます。


.. index::
   pair: .pythonrc.py; file
   triple: user; configuration; file

ポリシーとして、Pythonは起動時にユーザー毎の設定を行うコードを実行することは
しません(ただし対話型セッションで環境変数 :envvar:`PYTHONSTARTUP` が設定されていた場合にはそのスクリプトを実行します。)。

.. % As a policy, Python doesn't run user-specified code on startup of
.. % Python programs.  (Only interactive sessions execute the script
.. % specified in the \envvar{PYTHONSTARTUP} environment variable if it
.. % exists).

しかしながら、プログラムやサイトによっては、プログラムが要求した時にユーザーごとの設定ファイルを実行できると便利なこともあります。このモジュー
ルはそのような機構を実装しています。この機構を利用したいプログラムでは、以下の文を実行してください。

.. % However, some programs or sites may find it convenient to allow users
.. % to have a standard customization file, which gets run when a program
.. % requests it.  This module implements such a mechanism.  A program
.. % that wishes to use the mechanism must execute the statement

::

   import user

.. index:: builtin: execfile

:mod:`user` モジュールはユーザーのホームディレクトリの :file:`.pythonrc.py` ファイルを探し、オープンできるならグローバル名前空間
で実行します(:func:`execfile` を利用します)。この段階で発生したエラーはcatchされません。 :mod:`user` モジュー
ルをimportしたプログラムに影響します。ホームディレクトリは環境変数 :envvar:`HOME` が仮定されていますが、もし設定されていなければカレントディ
レクトリが使われます。

.. % The \module{user} module looks for a file \file{.pythonrc.py} in the user's
.. % home directory and if it can be opened, executes it (using
.. % \function{execfile()}\bifuncindex{execfile}) in its own (the
.. % module \module{user}'s) global namespace.  Errors during this phase
.. % are not caught; that's up to the program that imports the
.. % \module{user} module, if it wishes.  The home directory is assumed to
.. % be named by the \envvar{HOME} environment variable; if this is not set,
.. % the current directory is used.

ユーザーの :file:`.pythonrc.py` ではPythonのバージョンに従って異なる動作を行うために ``sys.version`` のテストを行うことが
考えられます。

.. % The user's \file{.pythonrc.py} could conceivably test for
.. % \code{sys.version} if it wishes to do different things depending on
.. % the Python version.

ユーザーへの警告: :file:`.pythonrc.py` ファイルに書く内容には慎重になって
ください。どのプログラムが利用しているかわからない状況で、標準のモジュールや関数のふるまいを替えることはおすすめできません。

.. % A warning to users: be very conservative in what you place in your
.. % \file{.pythonrc.py} file.  Since you don't know which programs will
.. % use it, changing the behavior of standard modules or functions is
.. % generally not a good idea.

この機構を使おうとするプログラマへの提案:  あなたのパッケージ向けのオプションをユーザーが設定できるようにするシンプ
ルな方法は、 :file:`.pythonrc.py` ファイルで変数を定義して、あなたのプログ
ラムでテストする方法です。たとえば、 :mod:`spam` モジュールでメッセージ出力のレベルを替える
``user.spam_verbose`` 変数を参照するには以下のようにします:

.. % A suggestion for programmers who wish to use this mechanism: a simple
.. % way to let users specify options for your package is to have them
.. % define variables in their \file{.pythonrc.py} file that you test in
.. % your module.  For example, a module \module{spam} that has a verbosity
.. % level can look for a variable \code{user.spam_verbose}, as follows:

::

   import user

   verbose = bool(getattr(user, "spam_verbose", 0))

(ユーザが ``spam_verbose`` をファイル :file:`.pythonrc.py` 内で定義していない時に :func:`getattr`
の3引数形式は使われます。)

.. % (The three-argument form of \function{getattr()} is used in case
.. % the user has not defined \code{spam_verbose} in their
.. % \file{.pythonrc.py} file.)

大規模な設定の必要があるプログラムではプログラムごとの設定ファイルを作るといいです。

.. % Programs with extensive customization needs are better off reading a
.. % program-specific customization file.

セキュリティやプライバシーに配慮するプログラムではこのモジュールをimport し *ない* でください。このモジュールを使うと、ユーザーは
:file:`.pythonrc.py` に任意のコードを書くことで簡単に侵入することができてしまいます。

.. % Programs with security or privacy concerns should \emph{not} import
.. % this module; a user can easily break into a program by placing
.. % arbitrary code in the \file{.pythonrc.py} file.

汎用のモジュールではこのモジュールをimportし *ない* でください。 importしたプログラムの動作にも影響してしまいます。

.. % Modules for general use should \emph{not} import this module; it may
.. % interfere with the operation of the importing program.


.. seealso::

   Module :mod:`site`
      サイト毎のカスタマイズを行う機構

