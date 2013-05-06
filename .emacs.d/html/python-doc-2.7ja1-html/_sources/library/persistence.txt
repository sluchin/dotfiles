
.. _persistence:

**************
データの永続化
**************

この章で解説されるモジュール群は Python データをディスクに永続的な形式で保存します。
モジュール :mod:`pickle` とモジュール :mod:`marshal` は多くの
Python データ型をバイト列に変換し、バイト列から再生成します。
様々な DBM 関連モジュールはハッシュを基にした、
文字列から他の文字列へのマップを保存するファイルフォーマット群をサポートします。
モジュール :mod:`bsddb` はディスクベースの文字列から文字列へのマッピングを、
ハッシュ、B-Tree、レコードを基にしたフォーマットで提供します。

この章で説明されるモジュールは:


.. toctree::

   pickle.rst
   copy_reg.rst
   shelve.rst
   marshal.rst
   anydbm.rst
   whichdb.rst
   dbm.rst
   gdbm.rst
   dbhash.rst
   bsddb.rst
   dumbdbm.rst
   sqlite3.rst
