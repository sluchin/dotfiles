:mod:`rlcompleter` --- GNU readline向け補完関数
===============================================

.. module:: rlcompleter
   :platform: Unix
   :synopsis: GNU readline ライブラリ向けのPython識別子補完
.. sectionauthor:: Moshe Zadka <moshez@zadka.site.co.il>


:mod:`rlcompleter` モジュールではPythonの識別子やキーワードを定義した
:mod:`readline` モジュール向けの補完関数を定義しています。

このモジュールが Unixプラットフォームでimportされ、 :mod:`readline` が利用できるときには、 :class:`Completer`
クラスのインスタンスが自動的に作成され、 :meth:`complete` メソッドが :mod:`readline` 補完に設定されます。

使用例::

   >>> import rlcompleter
   >>> import readline
   >>> readline.parse_and_bind("tab: complete")
   >>> readline. <TAB PRESSED>
   readline.__doc__          readline.get_line_buffer(  readline.read_init_file(
   readline.__file__         readline.insert_text(      readline.set_completer(
   readline.__name__         readline.parse_and_bind(
   >>> readline.

:mod:`rlcompleter` モジュールは Pythonの対話モードで利用する為にデザインされています。ユーザは以下の命令を初期化ファイル
(環境変数 :envvar:`PYTHONSTARTUP` によって定義されます)に書き込むことで、 :kbd:`Tab` キーによる補完を利用できます::

   try:
       import readline
   except ImportError:
       print "Module readline not available."
   else:
       import rlcompleter
       readline.parse_and_bind("tab: complete")

:mod:`readline` のないプラットフォームでも、このモジュールで定義される :class:`Completer` クラスは独自の目的に使えます。


.. _completer-objects:

Completerオブジェクト
---------------------

Completerオブジェクトは以下のメソッドを持っています:


.. method:: Completer.complete(text, state)

   *text* の *state* 番目の補完候補を返します。

   もし *text* がピリオド(``'.'``)を含まない場合、 :mod:`__main__` 、 :mod:`__builtin__` で定義されて
   いる名前か、キーワード ( :mod:`keyword` モジュールで定義されている) から補完されます。

   ピリオドを含む名前の場合、副作用を出さずに名前を最後まで評価しようとしま
   す(関数を明示的に呼び出しはしませんが、 :meth:`__getattr__` を呼んでし
   まうことはあります)そして、 :func:`dir` 関数でマッチする語を見つけます。
   式を評価中に発生した全ての例外は補足して無視され、 :const:`None` を返します。
