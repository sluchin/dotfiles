:mod:`dumbdbm` --- 可搬性のある DBM 実装
========================================

.. module:: dumbdbm
   :synopsis: 単純な DBM インタフェースに対する可搬性のある実装。

.. note::
   :mod:`dumbdbm` モジュールは、 Python 3.0 では :mod:`dbm.dumb` にリネームされます。
   :term:`2to3` ツールが自動的にimportを修正します。

.. index:: single: databases

.. note::

   :mod:`dumbdbm` モジュールは、 :mod:`anydbm` が安定なモジュールを他に見つけることができなかった際の最後の手段とされています。
   :mod:`dumbdbm` モジュールは速度を重視して書かれているわけではなく、他のデータベースモジュールのように重い使い方をするためのものでは
   ありません。

:mod:`dumbdbm` モジュールは永続性辞書に類似したインタフェースを提供し、全て Python で書かれています。 :mod:`gdbm` や
:mod:`bsddb` といったモジュールと異なり、外部ライブラリは必要ありません。他の永続性マップ型のように、
キーおよび値は常に文字列でなければなりません。

このモジュールでは以下の内容を定義してします:


.. exception:: error

   I/O エラーのような dumbdbm 特有のエラーの際に送出されます。不正なキーを指定したときのような、一般的な対応付けエラーの際には
   :exc:`KeyError` が送出されます。


.. function:: open(filename[, flag[, mode]])

   dumbdbm データベースを開き、 dubmdbm オブジェクトを返します。 *filename* 引数はデータベースファイル名の雛型 (特定の拡張子を
   もたないもの) です。dumbdbm データベースが生成される際、 :file:`.dat` および :file:`.dir`
   の拡張子を持ったファイルが生成されます。

   オプションの *flag* 引数は現状では無視されます; データベースは常に更新のために開かれ、存在しない場合には新たに作成されます。

   オプションの *mode* 引数は Unix におけるファイルのモードで、データベースを作成する際に使われます。デフォルトでは 8 進コードの
   ``0666`` になっています (umask によって修正を受けます)。

   .. versionchanged:: 2.2
      *mode* 引数は以前のバージョンでは無視されます.


.. seealso::

   Module :mod:`anydbm`
      ``dbm`` 形式のデータベースに対する汎用インタフェース。

   Module :mod:`dbm`
      DBM/NDBM ライブラリに対する同様のインタフェース。

   Module :mod:`gdbm`
      GNU GDBM ライブラリに対する同様のインタフェース。

   Module :mod:`shelve`
      非文字列データを記録する永続化モジュール。

   Module :mod:`whichdb`
      既存のデータベースの形式を判定するために使われるユーティリティモジュール。


.. _dumbdbm-objects:

Dumbdbm オブジェクト
--------------------

:class:`UserDict.DictMixin` クラスで提供されているメソッドに加え、 :class:`dumbdbm`
オブジェクトでは以下のメソッドを提供しています。


.. method:: dumbdbm.sync()

   ディスク上の辞書とデータファイルを同期します。このメソッドは :class:`Shelve` オブジェクトの :meth:`sync` メソッドから
   呼び出されます。

