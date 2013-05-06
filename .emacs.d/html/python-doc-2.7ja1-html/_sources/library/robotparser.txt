:mod:`robotparser` ---  robots.txt のためのパーザ
=================================================

.. module:: robotparser
   :synopsis: robots.txt ファイルを読み出し、他の URL に対する取得可能性の質問に答えるクラス。
.. sectionauthor:: Skip Montanaro <skip@mojam.com>


.. index::
   single: WWW
   single: World Wide Web
   single: URL
   single: robots.txt

..
   note::
   The :mod:`robotparser` module has been renamed :mod:`urllib.robotparser` in
   Python 3.0.
   The :term:`2to3` tool will automatically adapt imports when converting
   your sources to 3.0.

.. note::
   :mod:`robotparser` モジュールは、 Python 3.0 では :mod:`urllib.robotparser`
   にリネームされました。
   :term:`2to3` ツールが自動的にソースコードの import を修正します。

このモジュールでは単一のクラス、 :class:`RobotFileParser` を提供します。このクラスは、特定のユーザエージェントが
:file:`robots.txt` ファイルを公開している Web サイトのある URL を取得可能かどうかの質問に答えます。
:file:`robots.txt` ファイルの構造に関する詳細は http://www.robotstxt.org/orig.html を参照してください。


.. class:: RobotFileParser()

   このクラスでは単一の :file:`robots.txt` ファイルを読み出し、解釈し、ファイルの内容に関する質問の回答を得るためのメソッドを定義しています。


   .. method:: set_url(url)

      :file:`robots.txt` ファイルを参照するための URL を設定します。


   .. method:: read()

      :file:`robots.txt` URL を読み出し、パーザに入力します。


   .. method:: parse(lines)

      引数 *lines* の内容を解釈します。


   .. method:: can_fetch(useragent, url)

      解釈された :file:`robots.txt` ファイル中に記載された規則に従ったとき、 *useragent* が *url* を取得してもよい場合には
      ``True`` を返します。


   .. method:: mtime()

      ``robots.txt`` ファイルを最後に取得した時刻を返します。この値は、定期的に新たな ``robots.txt`` をチェックする必要がある、
      長時間動作する Web スパイダープログラムを実装する際に便利です。


   .. method:: modified()

      ``robots.txt`` ファイルを最後に取得した時刻を現在の時刻に設定します。

以下にRobotFileParser クラスの利用例を示します。 ::

   >>> import robotparser
   >>> rp = robotparser.RobotFileParser()
   >>> rp.set_url("http://www.musi-cal.com/robots.txt")
   >>> rp.read()
   >>> rp.can_fetch("*", "http://www.musi-cal.com/cgi-bin/search?city=San+Francisco")
   False
   >>> rp.can_fetch("*", "http://www.musi-cal.com/")
   True

