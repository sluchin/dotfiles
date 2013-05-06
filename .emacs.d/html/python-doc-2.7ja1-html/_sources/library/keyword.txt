
:mod:`keyword` --- Python キーワードチェック
============================================

.. module:: keyword
   :synopsis: 文字列が Python のキーワードか否かを調べます。


このモジュールでは、 Python プログラムで文字列がキーワードか否かをチェックする機能を提供します。


.. function:: iskeyword(s)

   *s* が Python のキーワードであれば真を返します。


.. data:: kwlist

   インタープリタで定義している全てのキーワードのシーケンス。
   特定の :mod:`__future__` 宣言がなければ有効ではないキーワードでもこのリストに\
   は含まれます。


.. seealso::

   最新のバージョンの `keyword モジュールの Python ソースコード
   <http://svn.python.org/view/python/branches/release27-maint/Lib/keyword.py?view=markup>`_
