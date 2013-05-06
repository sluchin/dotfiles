
:mod:`aepack` --- Python変数とAppleEventデータコンテナ間の変換
==============================================================

.. module:: aepack
   :platform: Mac
   :synopsis: Python変数とAppleEventデータコンテナ間の変換
   :deprecated:
.. sectionauthor:: Vincent Marchetti <vincem@en.com>
.. moduleauthor:: Jack Jansen


:mod:`aepack` モジュールは、Python 変数から AppleEvent ディスクリプ\
タへの変換(パック)と、その逆に変換(アンパック)する関数を定義しています。
Python 内では AppleEvent ディスクリプタは、組み込み型である
AEDesc の Python オブジェクトとして扱われます。
:class:`AEDesc` は :mod:`Carbon.AE` モジュールで定義されています。

.. note::

   このモジュールは Python 3.x で削除されました。



:mod:`aepack` モジュールは次の関数を定義しています。


.. function:: pack(x[, forcetype])

   Python 値 *x* を変換した値を保持する :class:`AEDesc` オブジェクトを返します。
   *forcetype* を与えることで、結果のディスクリプタ型を指定できます。
   それ以外では、Python 型から Apple Eventディスクリプタ型へのデフォ\
   ルトのマッピングが使われます。マッピングは次の通りとなります。

   +-----------------+-----------------------------------+
   | Python type     | descriptor type                   |
   +=================+===================================+
   | :class:`FSSpec` | typeFSS                           |
   +-----------------+-----------------------------------+
   | :class:`FSRef`  | typeFSRef                         |
   +-----------------+-----------------------------------+
   | :class:`Alias`  | typeAlias                         |
   +-----------------+-----------------------------------+
   | integer         | typeLong (32 bit integer)         |
   +-----------------+-----------------------------------+
   | float           | typeFloat (64 bit floating point) |
   +-----------------+-----------------------------------+
   | string          | typeText                          |
   +-----------------+-----------------------------------+
   | unicode         | typeUnicodeText                   |
   +-----------------+-----------------------------------+
   | list            | typeAEList                        |
   +-----------------+-----------------------------------+
   | dictionary      | typeAERecord                      |
   +-----------------+-----------------------------------+
   | instance        | *see below*                       |
   +-----------------+-----------------------------------+

   *x* がPythonインスタンスなら、この関数は :meth:`__aepack__` メ\
   ソッドを呼びだそうとします。このメソッドは :class:`AEDesc` オブジェ\
   クトを返します。

   *x* の変換が上で定義されていない場合は、この関数は、テキストディス\
   クリプタとしてエンコードされた、値の(repr()関数による)Python文字列表現\
   が返されます。


.. function:: unpack(x[, formodulename])

   *x* は :class:`AEDesc` タイプのオブジェクトでなければいけません。
   この関数は、Apple Eventディスクリプタ *x* のデータの Python
   オブジェクト表現を返します。単純な AppleEvent データ型(整数、テキスト、
   浮動小数点数)の、対応する Python 型が返されます。Apple Event リストは
   Python リストとして返され、リストの要素は再帰的にアンパックされます。
   ``formodulename`` の指定がない場合、オブジェクト参照 (例： ``line 3 of document
   1``)が、 :class:`aetypes.ObjectSpecifier` のインスタ\
   ンスとして返されます。ディスクリプタ型が typeFSS である AppleEvent
   ディスクリプタが、 :class:`FSSpec` オブジェクトとして返されます。
   AppleEventレコードディスクリプタが、再帰的にアンパックされた、型の4
   文字キーと要素を持つPython辞書として返されます。

   オプションの ``formodulename`` 引数は :mod:`gensuitemodule` よ\
   り作成されるスタブパッケージにより利用され、オブジェクト指定子のため\
   の OSA クラスをモジュールの中で見つけられることを保証します。これは、
   例えば、ファインダがウィンドウに対してオブジェクト指定子を返す場合、 ``Finder.Window``
   のインスタンスが得られ、 ``aetypes.Window`` が得られないことを保証します。
   前者は、ファインダ上のウィンドウが持っ\
   ている、すべての特性および要素のことを知っています。一方、後者のもの\
   はそれらのことを知りません。


.. seealso::

   Module :mod:`Carbon.AE`
      Apple Eventマネージャルーチンへの組み込みアクセス

   Module :mod:`aetypes`
      Apple Eventディスクリプタ型としてコードされたPython定義
