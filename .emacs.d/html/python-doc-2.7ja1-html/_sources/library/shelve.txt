:mod:`shelve` --- Python オブジェクトの永続化
=============================================

.. module:: shelve
   :synopsis: Python オブジェクトの永続化。


.. index:: module: pickle

"シェルフ (shelf, 棚)" は辞書に似た永続性を持つオブジェクトです。 "dbm" データベースとの違いは、シェルフの値 (キーではありません！)
は実質上どんな Python オブジェクトにも --- :mod:`pickle` モジュールが扱えるなら何でも ---
できるということです。これにはほとんどのクラスインスタンス、再帰的なデータ型、沢山の共有されたサブオブジェクト
を含むオブジェクトが含まれます。キーは通常の文字列です。

.. seealso::

   最新バージョンの `shelve モジュールの Python ソースコード
   <http://svn.python.org/view/python/branches/release27-maint/Lib/shelve.py?view=markup>`_

.. function:: open(filename[, flag='c'[, protocol=None[, writeback=False]]])

   永続的な辞書を開きます。指定された *filename* は、根底にあるデータベースの基本ファイル名となります。副作用として、 *filename*
   には拡張子がつけられる場合があり、ひとつ以上のファイルが生成される可能性もあります。デフォルトでは、根底にあるデータベースファイルは
   読み書き可能なように開かれます。オプションの *flag* パラメタは :func:`anydbm.open` における *flag* パラメタと同様に
   解釈されます。

   デフォルトでは、値を整列化する際にはバージョン 0 の pickle 化が用いられます。pickle 化プロトコルのバージョンは *protocol*
   パラメタで指定することができます。

   .. versionchanged:: 2.3
      *protocol* パラメタが追加されました。

   Python の意味論から、シェルフには永続的な辞書の可変エントリに対する変更を知る術が
   ありません。デフォルトでは、変更されたオブジェクトはシェルフに代入されたとき
   *だけ* 書き込まれます (:ref:`shelve-example` 参照)。
   オプションの *writeback* パラメタが  *True* に設定されていれば、
   アクセスされたすべてのエントリはメモリ上にキャッシュされ、
   :meth:`~Shelf.sync` および :meth:`~Shelf.close` を呼び出した際に書き戻されます。
   この機能は永続的な辞書上の可変の要素に対する変更を容易にしますが、
   多数のエントリがアクセスされた場合、膨大な量のメモリがキャッシュの
   ために消費され、アクセスされた全てのエントリを書き戻す
   (アクセスされたエントリが可変であるか、
   あるいは実際に変更されたかを決定する方法は存在しないのです)
   ために、ファイルを閉じる操作を非常に低速にしてしまいます。

.. warning::

   Because the :mod:`shelve` module is backed by :mod:`pickle`, it is insecure
   to load a shelf from an untrusted source.  Like with pickle, loading a shelf
   can execute arbitrary code.

   :mod:`shelve` モジュールは裏で :mod:`pickle` を使っているので、信頼できない
   ソースから shelf を読み込むのは危険です。
   pickle と同じく、 shelf の読み込みでも任意のコードを実行できるからです。

シェルフオブジェクトは辞書がサポートする全てのメソッドをサポートしています。
これにより、辞書ベースのスクリプトから永続的な記憶媒体を必要とする
スクリプトに容易に移行できるようになります。

追加でサポートされるメソッドが二つあります:

.. method:: Shelf.sync()

   シェルフが *writeback* を :const:`True` にセットして開かれている場合に、
   キャッシュ中の全てのエントリを書き戻します。また容易にできるならば、
   キャッシュを空にしてディスク上の永続的な辞書を同期します。
   このメソッドはシェルフを :meth:`close` によって閉じるとき自動的に呼び出されます。

.. method:: Shelf.close()

   永続的な *辞書* オブジェクトを同期して閉じます。
   既に閉じられているシェルフに対して呼び出すと :exc:`ValueError`
   に終わります。

.. seealso::

   .. `Persistent dictionary recipe <http://code.activestate.com/recipes/576642/>`_
      with widely supported storage formats and having the speed of native
      dictionaries.

   通常の辞書に近い速度をもち、いろいろなストレージフォーマットに対応した、
   `永続化辞書のレシピ <http://code.activestate.com/recipes/576642/>`_



制限事項
--------

  .. index::
     module: dbm
     module: gdbm
     module: bsddb

* どのデータベースパッケージが使われるか (例えば :mod:`dbm` 、 :mod:`gdbm` 、 :mod:`bsddb`) は、どのインタフェースが
  利用可能かに依存します。従って、データベースを :mod:`dbm`  を使って直接開く方法は安全ではありません。データベースはまた、 :mod:`dbm`
  が使われた場合 (不幸なことに) その制約に縛られます --- これはデータベースに記録されたオブジェクト (の pickle 化された表現) はかなり小さく
  なければならず、キー衝突が生じた場合に、稀にデータベースを更新することができなくなるということを意味します。

* :mod:`shelve` モジュールは、シェルフに置かれたオブジェクトの *並列した* 読み出し/書き込みアクセスをサポートしません
  (複数の同時読み出しアクセスは安全です)。あるプログラムが書き込みために開かれたシェルフを持っているとき、他のプログラムは
  そのシェルフを読み書きのために開いてはいけません。この問題を解決するために Unix のファイルロック機構を使うことができますが、この機構は Unix
  のバージョン間で異なり、使われているデータベースの実装について知識が必要となります。


.. class:: Shelf(dict[, protocol=None[, writeback=False]])

   :class:`UserDict.DictMixin` のサブクラスで、pickle 化された値を  *dict* オブジェクトに保存します。

   デフォルトでは、値を整列化する際にはバージョン 0 の pickle 化が用いられます。pickle 化プロトコルのバージョンは *protocol*
   パラメタで指定することができます。pickle 化プロトコルについては :mod:`pickle` のドキュメントを参照してください。

   .. versionchanged:: 2.3
      *protocol* パラメタが追加されました。

   *writeback* パラメタが *True* に設定されていれば、アクセスされたすべての
   エントリはメモリ上にキャッシュされ、ファイルを閉じる際に書き戻されます; この機能により、可変のエントリに対して自然な操作が可能になりますが、
   さらに多くのメモリを消費し、辞書をファイルと同期して閉じる際に長い時間がかかるようになります。


.. class:: BsdDbShelf(dict[, protocol=None[, writeback=False]])

   :class:`Shelf` のサブクラスで、 :meth:`first`, :meth:`!next`, 
   :meth:`previous`, :meth:`last`, :meth:`set_location` 
   メソッドを公開しています。これらのメソッドは :mod:`bsddb`
   モジュールでは利用可能ですが、他のデータベースモジュールでは利用できません。
   コンストラクタに渡された *dict* オブジェクトは上記のメソッドを
   サポートしていなくてはなりません。通常は、 :func:`bsddb.hashopen`,
   :func:`bsddb.btopen` または :func:`bsddb.rnopen` 
   のいずれかを呼び出して得られるオブジェクトが条件を満たしています。
   オプションの *protocol* および *writeback* パラメタは
   :class:`Shelf` クラスにおけるパラメタと同様に解釈されます。


.. class:: DbfilenameShelf(filename[, flag='c'[, protocol=None[, writeback=False]]])

   :class:`Shelf` のサブクラスで、辞書様オブジェクトの代わりに
   *filename* を受理します。根底にあるファイルは
   :func:`anydbm.open` を使って開かれます。
   デフォルトでは、ファイルは読み書き可能な状態で開かれます。
   オプションの *flag* パラメタは :func:`.open` 
   関数におけるパラメタと同様に解釈されます。
   オプションの *protocol* および *writeback* パラメタは
   :class:`Shelf` クラスにおけるパラメタと同様に解釈されます。

.. _shelve-example:

使用例
------

インタフェースは以下のコードに集約されています (``key`` は文字列で、 ``data`` は任意のオブジェクトです)::

   import shelve

   d = shelve.open(filename) # open -- file may get suffix added by low-level
                             # library

   d[key] = data   # store data at key (overwrites old data if
                   # using an existing key)
   data = d[key]   # retrieve a COPY of data at key (raise KeyError if no
                   # such key)
   del d[key]      # delete data stored at key (raises KeyError
                   # if no such key)
   flag = d.has_key(key)   # true if the key exists
   klist = d.keys() # a list of all existing keys (slow!)

   # as d was opened WITHOUT writeback=True, beware:
   d['xx'] = range(4)  # this works as expected, but...
   d['xx'].append(5)   # *this doesn't!* -- d['xx'] is STILL range(4)!

   # having opened d without writeback=True, you need to code carefully:
   temp = d['xx']      # extracts the copy
   temp.append(5)      # mutates the copy
   d['xx'] = temp      # stores the copy right back, to persist it

   # or, d=shelve.open(filename,writeback=True) would let you just code
   # d['xx'].append(5) and have it work as expected, BUT it would also
   # consume more memory and make the d.close() operation slower.

   d.close()       # close it


.. seealso::

   Module :mod:`anydbm`
      ``dbm`` スタイルのデータベースに対する汎用インタフェース。

   Module :mod:`bsddb`
      BSD ``db`` データベースインタフェース。

   Module :mod:`dbhash`
      :mod:`bsddb` をラップする薄いレイヤで、
      他のデータベースモジュールのように関数 :func:`~dbhash.open` を提供しています。

   Module :mod:`dbm`
      標準の Unix データベースインタフェース。

   Module :mod:`dumbdbm`
      ``dbm`` インタフェースの移植性のある実装。

   Module :mod:`gdbm`
      ``dbm`` インタフェースに基づいた GNU データベースインタフェース。

   Module :mod:`pickle`
      :mod:`shelve` によって使われるオブジェクト整列化機構。

   Module :mod:`cPickle`
      :mod:`pickle` の高速版。

