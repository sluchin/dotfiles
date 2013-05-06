:mod:`getpass` --- 可搬性のあるパスワード入力機構
=================================================

.. module:: getpass
   :synopsis: ポータブルなパスワードとユーザーIDの検索


.. moduleauthor:: Piers Lauder <piers@cs.su.oz.au>
.. sectionauthor:: Fred L. Drake, Jr. <fdrake@acm.org>
.. Windows (& Mac?) support by Guido van Rossum.

.. The :mod:`getpass` module provides two functions:

getpassモジュールは二つの関数を提供します:


.. function:: getpass([prompt[, stream]])

   エコーなしでユーザーにパスワードを入力させるプロンプト。ユーザーは *prompt* の文字列をプロンプトに使え、
   デフォルトは ``'Password:'`` です。 Unixではプロンプトはファイルに似たオブジェクト *stream* へ
   出力されます。
   *stream* のデフォルトは、制御端末(/dev/tty)か、それが利用できない場合は ``sys.stderr`` です。
   (この引数は Windowsでは無視されます。)

   .. If echo free input is unavailable getpass() falls back to printing
      a warning message to *stream* and reading from ``sys.stdin`` and
      issuing a :exc:`GetPassWarning`.

   もしエコー無しの入力が利用できない場合は、 ``getpass()`` は *stream*
   に警告メッセージを出力し、 ``sys.stdin`` から読み込み、 :exc:`GetPassWarning`
   警告を発生させます。

   利用できるシステム: Macintosh, Unix, Windows

   .. versionchanged:: 2.5
      パラメータ *stream* の追加.

   .. versionchanged:: 2.6

      .. On Unix it defaults to using /dev/tty before falling back
         to ``sys.stdin`` and ``sys.stderr``.

      Unix ではデフォルトで、 ``sys.stdin`` と ``sys.stderr`` へfallback
      するまえに /dev/tty を利用します。

   .. note::
      IDLE から getpass を呼び出した場合、入力はIDLEのウィンドウではなく、
      IDLE を起動したターミナルから行われます。

.. exception:: GetPassWarning

   .. A :exc:`UserWarning` subclass issued when password input may be echoed.

   :exc:`UserWarning` のサブクラスで、入力がエコーされてしまった場合に発生します。


.. function:: getuser()

   ユーザーの "ログイン名"を返します。　有効性:Unix、Windows

   この関数は環境変数 :envvar:`LOGNAME` :envvar:`USER` :envvar:`LNAME`
   :envvar:`USERNAME` の順序でチェックして、最初の空ではない文字列が設定された値を返します。
   もし、なにも設定されていない場合はpwdモジュールが提供するシステム上のパスワードデータベースから返します。それ以外は、例外が上がります。

