:mod:`_winreg` -- Windows レジストリへのアクセス
================================================

.. module:: _winreg
   :platform: Windows
   :synopsis: Windows レジストリを操作するためのルーチンおよびオブジェクト。
.. sectionauthor:: Mark Hammond <MarkH@ActiveState.com>

.. note::
   :mod:`_winreg` モジュールは Python 3.0 では :mod:`winreg` にリネームされました。
   ソースコードを3.0用に変換するときは、 :term:`2to3` ツールが自動的に import を修正します。


.. versionadded:: 2.0

これらの関数は Windows レジストリ API を Python から使えるようにします。
プログラマがレジストリハンドルのクローズを失念した場合でも、確実にハンドルが
クローズされるようにするために、整数値をレジストリハンドルとして使う代わりに
:ref:`ハンドルオブジェクト <handle-object>` が使われます。

このモジュールでは以下の関数を提供します:


.. function:: CloseKey(hkey)

   以前開かれたレジストリキーを閉じます。 *hkey* 引数には以前開かれた
   レジストリキーを特定します。

   .. note::
      このメソッドを使って (または :meth:`handle.Close` によって) *hkey* が
      閉じられなかった場合、Python が *hkey* オブジェクトを破壊する際に閉じられます。


.. function:: ConnectRegistry(computer_name, key)

   他の計算機上にある既定のレジストリハンドル接続を確立し、
   :ref:`ハンドルオブジェクト <handle-object>` を返します。

   *computer_name* はリモートコンピュータの名前で、 ``r"\\computername"``
   の形式をとります。 ``None`` の場合、ローカルの計算機が使われます。

   *key* は接続したい既定のハンドルです。

   戻り値は開かれたキーのハンドルです。関数が失敗した場合、 :exc:`WindowsError`
   例外が送出されます。


.. function:: CreateKey(key, sub_key)

   特定のキーを生成するか開き、 :ref:`ハンドルオブジェクト <handle-object>`
   を返します。

   *key* はすでに開かれたキーか、既定の :ref:`HKEY_* 定数 <hkey-constants>`
   のうちの一つです。

   *sub_key* はこのメソッドが開く、または新規作成するキーの名前です。

   *key* が既定のキーの一つなら、 *sub_key* は ``None``  でかまいません。この場合、返されるハンドルは関数に渡されたのと
   同じキーハンドルです。

   キーがすでに存在する場合、この関数は既に存在するキーを開きます。

   戻り値は開かれたキーのハンドルです。この関数が失敗した場合、 :exc:`WindowsError` 例外が送出されます。


.. function:: CreateKeyEx(key, sub_key[, res[, sam]])

   指定された key を作成するか開いて、 :ref:`ハンドルオブジェクト <handle-object>`
   を返します。

   *key* はすでに開かれた key か、定義済みの :ref:`HKEY_* 定数 <hkey-constants>` です。

   *sub_key* はこのメソッドが開くまたは作成するキーの名前を表す文字列です。

   *res* は予約された整数で、 0 でなくてはなりません。デフォルト値は 0 です。

   *sam* は、 key に対して想定するセキュリティアクセスを示すアクセスマスクを
   指定します。デフォルトは :const:`KEY_ALL_ACCESS` です。
   利用可能な値については :ref:`アクセス権 <access-rights>` を参照してください。


   *key* が定義済みのキーのどれかの場合、 *sub_key* には ``None`` も指定できます。
   この場合、戻り値のハンドルはこの関数に渡されたキーのハンドルと同じです。

   key がすでに存在する場合は、この関数はそのキーを開きます。

   戻り値は開いた key のハンドルです。失敗した場合、 :exc:`WindowsError`
   例外を発生させます。

.. versionadded:: 2.7


.. function:: DeleteKey(key, sub_key)

   特定のキーを削除します。

   *key* はすでに開かれたキーか、既定の :ref:`HKEY_* 定数 <hkey-constants>`
   のうちの一つです。

   *sub_key*  は文字列で、 *key* パラメタによって特定されたキーのサブキーでなければなりません。この値は ``None`` で
   あってはならず、キーはサブキーを持っていてはなりません。

   *このメソッドはサブキーをもつキーを削除することはできません。*

   このメソッドの実行が成功すると、キー全体が、その値すべてを含めて削除されます。このメソッドが失敗した場合、 :exc:`WindowsError`
   例外が送出されます。


.. function:: DeleteKeyEx(key, sub_key[, sam[, res]])

   指定された key を削除します。

   .. note::
      :func:`DeleteKeyEx` 関数は RegDeleteKeyEx Windows API 関数を使って実装
      されています。このAPIは 64bit 版Windowsにしかありません。
      `RegDeleteKeyEx のドキュメント
      <http://msdn.microsoft.com/en-us/library/ms724847%28VS.85%29.aspx>`__
      を参照してください。

   *key* は開いたキーか、定義済みの :ref:`HKEY_* 定数 <hkey-constants>` のどれかです。

   *sub_key* は *key* 引数によって指定された key の subkey でなければなりません。
   この値は ``None`` であってはなりません。また、 key は subkey を持たないかもしれません。

   *res* は予約済みの整数で、 0 でなければなりません。デフォルトは 0 です。

   *sam* は、 key に対して想定するセキュリティアクセスを示すアクセスマスクを
   指定します。デフォルトは :const:`KEY_WOW64_64KEY` です。
   利用可能な値については :ref:`アクセス権 <access-rights>` を参照してください。


   *このメソッドはサブキーをもつキーを削除することはできません。*

   このメソッドの実行が成功すると、キー全体が、その値すべてを含めて削除されます。
   このメソッドが失敗した場合、 :exc:`WindowsError` 例外を発生させます。

   サポートされていない Windows バージョンでは、 :exc:`NotImplementedError` 例外を
   発生させます。

.. versionadded:: 2.7


.. function:: DeleteValue(key, value)

   レジストリキーから指定された名前つきの値を削除します。

   *key* はすでに開かれたキーか、既定の :ref:`HKEY_* 定数 <hkey-constants>`
   のうちの一つです。

   *value* は削除したい値を指定するための文字列です。


.. function:: EnumKey(key, index)

   開かれているレジストリキーのサブキーを列挙し、文字列で返します。

   *key* はすでに開かれたキーか、既定の :ref:`HKEY_* 定数 <hkey-constants>`
   のうちの一つです。

   *index* は整数値で、取得するキーのインデクスを特定します。

   この関数は呼び出されるたびに一つのサブキーの名前を取得します。この関数は通常、これ以上サブキーがないことを示す :exc:`WindowsError`
   例外が送出されるまで繰り返し呼び出されます。


.. function:: EnumValue(key, index)

   開かれているレジストリキーの値を列挙し、タプルで返します。

   *key* はすでに開かれたキーか、既定の :ref:`HKEY_* 定数 <hkey-constants>`
   のうちの一つです。

   *index* は整数値で、取得する値のインデクスを特定します。

   この関数は呼び出されるたびに一つの値の名前を取得します。この関数は通常、これ以上値がないことを示す :exc:`WindowsError`
   例外が送出されるまで繰り返し呼び出されます。

   結果は 3 要素のタプルになります:

   +-------+-----------------------------------------------------------------------------------+
   | Index | Meaning                                                                           |
   +=======+===================================================================================+
   | ``0`` | 値の名前を特定する文字列                                                          |
   +-------+-----------------------------------------------------------------------------------+
   | ``1`` | 値のデータを保持するためのオブジェクトで、その型は背後のレジストリ型に依存します  |
   +-------+-----------------------------------------------------------------------------------+
   | ``2`` | 値のデータ型を特定する整数です (:meth:`SetValueEx` のドキュメント内のテーブルを   |
   |       | 参照してください。                                                                |
   +-------+-----------------------------------------------------------------------------------+


.. function:: ExpandEnvironmentStrings(unicode)

   :const:`REG_EXPAND_SZ` のように、環境変数プレースホルダ ``%NAME%`` を
   Unicode 文字列で展開します。 ::

      >>> ExpandEnvironmentStrings(u"%windir%")
      u"C:\\Windows"

   .. versionadded:: 2.6


.. function:: FlushKey(key)

   キーのすべての属性をレジストリに書き込みます。

   *key* はすでに開かれたキーか、既定の :ref:`HKEY_* 定数 <hkey-constants>`
   のうちの一つです。

   キーを変更するために :func:`RegFlushKey` を呼ぶ必要はありません。
   レジストリの変更は怠惰なフラッシュ機構 (lazy flusher) を使ってフラッシュ
   されます。また、システムの遮断時にもディスクにフラッシュされます。
   :func:`CloseKey` と違って、 :func:`FlushKey` メソッドはレジストリに全ての
   データを書き終えたときにのみ返ります。アプリケーションは、レジストリへの
   変更を絶対に確実にディスク上に反映させる必要がある場合にのみ、
   :func:`FlushKey` を呼ぶべきです。

   .. note::

      :func:`FlushKey` を呼び出す必要があるかどうか分からない場合、
      おそらくその必要はありません。


.. function:: LoadKey(key, sub_key, file_name)

   指定されたキーの下にサブキーを生成し、サブキーに指定されたファイルのレジストリ情報を記録します。

   *key* は :func:`ConnectRegistry` が返したハンドルか、定数 :const:`HKEY_USERS` と
   :const:`HKEY_LOCAL_MACHINE` のどちらかです。

   *sub_key* は記録先のサブキーを指定する文字列です。

   *file_name* はレジストリデータを読み出すためのファイル名です。このファイルは :func:`SaveKey` 関数で生成されたファイルでなくては
   なりません。ファイル割り当てテーブル (FAT) ファイルシステム下では、ファイル名は拡張子を持っていてはなりません。

   この関数を呼び出しているプロセスが :const:`SE_RESTORE_PRIVILEGE` 特権を
   持たない場合には :func:`LoadKey` は失敗します。
   この特権はファイル許可とは違うので注意してください - 詳細は `RegLoadKey documentation
   <http://msdn.microsoft.com/en-us/library/ms724889%28v=VS.85%29.aspx>`__
   を参照してください。

   *key* が :func:`ConnectRegistry` によって返されたハンドルの場合、 *fileName*
   に指定されたパスは遠隔計算機に対する相対パス名になります。


.. function:: OpenKey(key, sub_key[, res[, sam]])

   指定されたキーを開き、 :ref:`ハンドルオブジェクト <handle-object>` を返します。

   *key* はすでに開かれたキーか、既定の :ref:`HKEY_* 定数 <hkey-constants>`
   のうちの一つです。

   *sub_key* は開きたいサブキーを特定する文字列です。

   *res* 予約されている整数値で、ゼロでなくてはなりません。標準の値はゼロです。

   *sam* は必要なキーへのセキュリティアクセスを記述する、アクセスマスクを
   指定する整数です。標準の値は :const:`KEY_READ` です。
   その他の利用できる値については :ref:`アクセス権限 <access-rights>`
   を参照してください。

   指定されたキーへの新しいハンドルが返されます。

   この関数が失敗すると、 :exc:`WindowsError` が送出されます。


.. function:: OpenKeyEx()

   :func:`OpenKeyEx` の機能は :func:`OpenKey` を標準の引数で使うことで
   提供されています。


.. function:: QueryInfoKey(key)

   キーに関数情報をタプルとして返します。

   *key* はすでに開かれたキーか、既定の :ref:`HKEY_* 定数 <hkey-constants>`
   のうちの一つです。

   結果は以下の 3 要素からなるタプルです:

   +------------+-------------------------------------------------------------------------+
   | インデクス | 意味                                                                    |
   +============+=========================================================================+
   | ``0``      | このキーが持つサブキーの数を表す整数。                                  |
   +------------+-------------------------------------------------------------------------+
   | ``1``      | このキーが持つ値の数を表す整数。                                        |
   +------------+-------------------------------------------------------------------------+
   | ``2``      | 最後のキーの変更が (あれば) いつだったかを表す長整数で、 1600 年 1 月 1 |
   |            | 日からの 100 ナノ秒単位で数えたもの。                                   |
   +------------+-------------------------------------------------------------------------+


.. function:: QueryValue(key, sub_key)

   キーに対する、名前付けられていない値を文字列で取得します。

   *key* はすでに開かれたキーか、既定の :ref:`HKEY_* 定数 <hkey-constants>`
   のうちの一つです。

   *sub_key* は値が関連付けられているサブキーの名前を保持する文字列です。この引数が ``None`` または空文字列の場合、この関数は *key*
   で特定されるキーに対して :func:`SetValue` メソッドで設定された値を取得します。

   レジストリ中の値は名前、型、およびデータから構成されています。
   このメソッドはあるキーのデータ中で、名前 NULL をもつ最初の値を取得します。
   しかし背後のAPI 呼び出しは型情報を返しません。
   なので、可能ならいつでも :func:`QueryValueEx` を使うべきです。


.. function:: QueryValueEx(key, value_name)

   開かれたレジストリキーに関連付けられている、指定した名前の値に対して、型およびデータを取得します。

   *key* はすでに開かれたキーか、既定の :ref:`HKEY_* 定数 <hkey-constants>`
   のうちの一つです。

   *value_name* は要求する値を指定する文字列です。

   結果は 2 つの要素からなるタプルです:

   +------------+---------------------------------------------------------------------+
   | インデクス | 意味                                                                |
   +============+=====================================================================+
   | ``0``      | レジストリ要素の名前。                                              |
   +------------+---------------------------------------------------------------------+
   | ``1``      | この値のレジストリ型を表す整数。                                    |
   |            | (:meth:`SetValueEx` のドキュメント内のテーブルを参照してください。) |
   +------------+---------------------------------------------------------------------+


.. function:: SaveKey(key, file_name)

   指定されたキーと、そのサブキー全てを指定したファイルに保存します。

   *key* はすでに開かれたキーか、既定の :ref:`HKEY_* 定数 <hkey-constants>`
   のうちの一つです。

   *file_name* はレジストリデータを保存するファイルの名前です、このファイルはすでに存在していてはいけません。このファイル名が
   拡張子を含んでいる場合、 :meth:`LoadKey` メソッドは、FAT ファイルシステムを
   使うことができません。

   *key* が遠隔の計算機上にあるキーを表す場合、 *file_name* で記述されているパスは遠隔の計算機に対して相対的なパスになります。
   このメソッドの呼び出し側は :const:`SeBackupPrivilege`  セキュリティ特権を保有していなければなりません。この特権は
   ファイルパーミッションとは異なります - 詳細は
   `Conflicts Between User Rights and Permissions documentation
   <http://msdn.microsoft.com/en-us/library/ms724878%28v=VS.85%29.aspx>`__
   を参照してください。

   この関数は *security_attributes* を NULL にして API に渡します。


.. function:: SetValue(key, sub_key, type, value)

   値を指定したキーに関連付けます。

   *key* はすでに開かれたキーか、既定の :ref:`HKEY_* 定数 <hkey-constants>`
   のうちの一つです。

   *sub_key* は値が関連付けられているサブキーの名前を表す文字列です。

   *type* はデータの型を指定する整数です。現状では、この値は :const:`REG_SZ` でなければならず、これは文字列だけが
   サポートされていることを示します。他のデータ型をサポートするには :func:`SetValueEx` を使ってください。

   *value* は新たな値を指定する文字列です。

   *sub_key* 引数で指定されたキーが存在しなければ、 SetValue 関数で生成されます。

   値の長さは利用可能なメモリによって制限されます。(2048 バイト以上の) 長い値はファイルに保存して、そのファイル名を設定レジストリに保存
   するべきです。そうすればレジストリを効率的に動作させる役に立ちます。

   *key* 引数に指定されたキーは :const:`KEY_SET_VALUE` アクセスで開かれていなければなりません。


.. function:: SetValueEx(key, value_name, reserved, type, value)

   開かれたレジストリキーの値フィールドにデータを記録します。

   *key* はすでに開かれたキーか、既定の :ref:`HKEY_* 定数 <hkey-constants>`
   のうちの一つです。

   *value_name* は値が関連付けられているサブキーの名前を表す文字列です。

   *type* はデータの型を指定する整数です。 利用できる型については
   :ref:`値の型 <value-types>` を参照してください。

   *reserved* は何もしません - API には常にゼロが渡されます。

   *value* は新たな値を指定する文字列です。

   このメソッドではまた、指定されたキーに対して、さらに別の値や型情報を設定することができます。 *key* 引数で指定されたキーは
   :const:`KEY_SET_VALUE` アクセスで開かれていなければなりません。

   キーを開くには、 :func:`CreateKey` または :func:`OpenKey`  メソッドを使ってください。

   値の長さは利用可能なメモリによって制限されます。(2048 バイト以上の) 長い値はファイルに保存して、そのファイル名を設定レジストリに保存
   するべきです。そうすればレジストリを効率的に動作させる役に立ちます。


.. function:: DisableReflectionKey(key)

   .. Disables registry reflection for 32-bit processes running on a 64-bit
      operating system.

   64ビット OS上で動作している 32bit プロセスに対するレジストリリフレクションを
   無効にします。

   .. *key* is an already open key, or one of the predefined
      :ref:`HKEY_* constants <hkey-constants>`.

   *key* はすでに開かれたキーか、既定の :ref:`HKEY_* 定数 <hkey-constants>`
   のうちの一つです。

   .. Will generally raise :exc:`NotImplemented` if executed on a 32-bit
      operating system.

   32bit OS上では一般的に :exc:`NotImplemented` 例外を発生させます。

   .. If the key is not on the reflection list, the function succeeds but has no
      effect. Disabling reflection for a key does not affect reflection of any
      subkeys.

   key がリフレクションリストに無い場合は、この関数は成功しますが効果は
   ありません。あるキーのリフレクションを無効にしても、その全てのサブキーの
   リフレクションには影響しません。

.. function:: EnableReflectionKey(key)

   .. Restores registry reflection for the specified disabled key.

   指定された、リフレクションが無効にされたキーのリフレクションを
   再び有効にします。

   *key* はすでに開かれたキーか、既定の :ref:`HKEY_* 定数 <hkey-constants>`
   のうちの一つです。

   32bit OS上では一般的に :exc:`NotImplemented` 例外を発生させます。

   あるキーのリフレクションを再開しても、その全てのサブキーには影響しません。


.. function:: QueryReflectionKey(key)

   .. Determines the reflection state for the specified key.

   指定されたキーのリフレクション状態を確認します。

   .. *key* is an already open key, or one of the predefined
      :ref:`HKEY_* constants <hkey-constants>`.

   *key* はすでに開かれたキーか、既定の :ref:`HKEY_* 定数 <hkey-constants>`
   のうちの一つです。

   .. Returns ``True`` if reflection is disabled.

   リフレクションが無効になっている場合、 ``True`` を返します。

   .. Will generally raise :exc:`NotImplemented` if executed on a 32-bit
      operating system.

   32bit OS上では一般的に :exc:`NotImplemented` 例外を発生させます。

.. _constants:

定数
------------------

.. The following constants are defined for use in many :mod:`_winreg` functions.

:mod:`_winreg` の多くの関数で利用するために以下の定数が定義されています。

.. _hkey-constants:

HKEY_* 定数
+++++++++++++

.. data:: HKEY_CLASSES_ROOT

   .. Registry entries subordinate to this key define types (or classes) of
      documents and the properties associated with those types. Shell and
      COM applications use the information stored under this key.

   このキー以下のレジストリエントリは、ドキュメントのタイプ（またはクラス）や、
   それに関連付けられたプロパティを定義しています。
   シェルと COM アプリケーションがこの情報を利用します。

.. data:: HKEY_CURRENT_USER

   .. Registry entries subordinate to this key define the preferences of
      the current user. These preferences include the settings of
      environment variables, data about program groups, colors, printers,
      network connections, and application preferences.

   このキー以下のレジストリエントリは、現在のユーザーの設定を定義します。
   この設定には、環境変数、プログラムグループに関するデータ、カラー、
   プリンター、ネットワーク接続、アプリケーション設定などが含まれます。

.. data:: HKEY_LOCAL_MACHINE

   .. Registry entries subordinate to this key define the physical state
      of the computer, including data about the bus type, system memory,
      and installed hardware and software.

   このキー以下のレジストリエントリは、コンピュータの物理的な状態を定義します。
   これには、バスタイプ、システムメモリ、インストールされているソフトウェアや
   ハードウェアが含まれます。

.. data:: HKEY_USERS

   .. Registry entries subordinate to this key define the default user
      configuration for new users on the local computer and the user
      configuration for the current user.

   このキー以下のレジストリエントリは、ローカルコンピュータの新規ユーザーの
   ためのデフォルト設定や、現在のユーザーの設定を定義しています。

.. data:: HKEY_PERFORMANCE_DATA

   .. Registry entries subordinate to this key allow you to access
      performance data. The data is not actually stored in the registry;
      the registry functions cause the system to collect the data from
      its source.

   このキー以下のレジストリエントリは、パフォーマンスデータへのアクセスを
   可能にしています。実際にはデータはレジストリには格納されていません。
   レジストリ関数がシステムにソースからデータを集めさせます。


.. data:: HKEY_CURRENT_CONFIG

   .. Contains information about the current hardware profile of the
      local computer system.

   ローカルコンピュータシステムの現在のハードウェアプロファイルに
   関する情報を含みます。

.. data:: HKEY_DYN_DATA

   ..This key is not used in versions of Windows after 98.

   このキーは Windows の 98 以降のバージョンでは利用されていません。

.. _access-rights:

アクセス権限
+++++++++++++

より詳しい情報については、 `Registry Key Security and Access
<http://msdn.microsoft.com/en-us/library/ms724878%28v=VS.85%29.aspx>`__
を参照してください。

.. data:: KEY_ALL_ACCESS

   .. Combines the STANDARD_RIGHTS_REQUIRED, :const:`KEY_QUERY_VALUE`,
      :const:`KEY_SET_VALUE`, :const:`KEY_CREATE_SUB_KEY`,
      :const:`KEY_ENUMERATE_SUB_KEYS`, :const:`KEY_NOTIFY`,
      and :const:`KEY_CREATE_LINK` access rights.

   STANDARD_RIGHTS_REQUIRED (:const:`KEY_QUERY_VALUE`,
   :const:`KEY_SET_VALUE`, :const:`KEY_CREATE_SUB_KEY`,
   :const:`KEY_ENUMERATE_SUB_KEYS`, :const:`KEY_NOTIFY`,
   :const:`KEY_CREATE_LINK`) アクセス権限の組み合わせ。


.. data:: KEY_WRITE

   STANDARD_RIGHTS_WRITE (:const:`KEY_SET_VALUE`,
   :const:`KEY_CREATE_SUB_KEY`) アクセス権限の組み合わせ。

.. data:: KEY_READ

   STANDARD_RIGHTS_READ (:const:`KEY_QUERY_VALUE`,
   :const:`KEY_ENUMERATE_SUB_KEYS`, :const:`KEY_NOTIFY`)
   アクセス権限の組み合わせ。

.. data:: KEY_EXECUTE

   :const:`KEY_READ` と同じ

.. data:: KEY_QUERY_VALUE

   .. Required to query the values of a registry key.

   レジストリキーの値を問い合わせるのに必要

.. data:: KEY_SET_VALUE

   .. Required to create, delete, or set a registry value.

   レジストリの値を作成、削除、設定するのに必要

.. data:: KEY_CREATE_SUB_KEY

   .. Required to create a subkey of a registry key.

   レジストリキーのサブキーを作るのに必要

.. data:: KEY_ENUMERATE_SUB_KEYS

   .. Required to enumerate the subkeys of a registry key.

   レジストリキーのサブキーを列挙するのに必要

.. data:: KEY_NOTIFY

   .. Required to request change notifications for a registry key or for
      subkeys of a registry key.

   レジストリキーやそのサブキーに対する変更通知を要求するのに必要

.. data:: KEY_CREATE_LINK

   .. Reserved for system use.

   システムでの利用のために予約されている


.. _64-bit-access-rights:

64-bit 特有のアクセス権
************************

より詳しい情報については、 `Accesing an Alternate Registry View
<http://msdn.microsoft.com/en-us/library/aa384129(v=VS.85).aspx>`__
を参照してください。

.. data:: KEY_WOW64_64KEY

   .. Indicates that an application on 64-bit Windows should operate on
      the 64-bit registry view.

   64 bit Windows 上のアプリケーションが、 64 bit のレジストリビュー上で
   操作する事を示します。

.. data:: KEY_WOW64_32KEY

   .. Indicates that an application on 64-bit Windows should operate on
      the 32-bit registry view.

   64 bit Windows 上のアプリケーションが、 32 bit のレジストリビュー上で
   操作する事を示します。


.. _value-types:

値の型
+++++++++++

For more information, see `Registry Value Types
<http://msdn.microsoft.com/en-us/library/ms724884%28v=VS.85%29.aspx>`__.

.. data:: REG_BINARY

   何らかの形式のバイナリデータ

.. data:: REG_DWORD

   32 ビットの数

.. data:: REG_DWORD_LITTLE_ENDIAN

   32 ビットのリトルエンディアン形式の数。

.. data:: REG_DWORD_BIG_ENDIAN

   32 ビットのビッグエンディアン形式の数。

.. data:: REG_EXPAND_SZ

   環境変数を参照している、ヌル文字で終端された文字列。 (``%PATH%``)。

.. data:: REG_LINK

   Unicode のシンボリックリンク。

.. data:: REG_MULTI_SZ

   .. A sequence of null-terminated strings, terminated by two null characters.
      (Python handles this termination automatically.)

   ヌル文字で終端された文字列からなり、二つのヌル文字で終端されている配列 (Python
   はこの終端の処理を自動的に行います)。

.. data:: REG_NONE

   .. No defined value type.

   定義されていない値の形式。

.. data:: REG_RESOURCE_LIST

   .. A device-driver resource list.

   デバイスドライバリソースのリスト。

.. data:: REG_FULL_RESOURCE_DESCRIPTOR

   .. A hardware setting.

   ハードウェアセッティング

.. data:: REG_RESOURCE_REQUIREMENTS_LIST

   .. A hardware resource list.

   ハードウェアリソースリスト

.. data:: REG_SZ

   ヌル文字で終端された文字列。


.. _handle-object:

レジストリハンドルオブジェクト
------------------------------

このオブジェクトは Windows の HKEY オブジェクトをラップし、オブジェクトが破壊されたときに自動的にハンドルを閉じます。オブジェクトの
:meth:`Close` メソッドと :func:`CloseKey` 関数のどちらも、後始末がきちんと行われることを保証するために呼び出す
ことができます。

このモジュールのレジストリ関数は全て、これらのハンドルオブジェクトの一つを返します。

このモジュールのレジストリ関数でハンドルオブジェクトを受理するものは全て整数も受理しますが、ハンドルオブジェクトを利用することを推奨します。

ハンドルオブジェクトは :meth:`__nonzero__` の意味構成を持ちます - すなわち、  ::

   if handle:
       print "Yes"

は、ハンドルが現在有効な (閉じられたり切り離されたりしていない) 場合には ``Yes`` となります。

ハンドルオブジェクトはまた、比較の意味構成もサポートしています。このため、背後の Windows ハンドル値が同じものを複数のハンドルオブジェクト
が参照している場合、それらの比較は真になります。

ハンドルオブジェクトは (例えば組み込みの :func:`int` 関数を使って) 整数に変換することができます。この場合、背後の Windows
ハンドル値が返されます、また、 :meth:`Detach` メソッドを使って整数のハンドル値を返させると同時に、ハンドルオブジェクトから Windows
ハンドルを切り離すこともできます。


.. method:: PyHKEY.Close()

   背後の Windows ハンドルを閉じます。

   ハンドルがすでに閉じられていてもエラーは送出されません。


.. method:: PyHKEY.Detach()

   ハンドルオブジェクトから Windows ハンドルを切り離します。

   切り離される以前にそのハンドルを保持していた整数 (または 64 ビット  Windows の場合には長整数) オブジェクトが返されます。
   ハンドルがすでに切り離されていたり閉じられていたりした場合、ゼロが返されます。

   この関数を呼び出した後、ハンドルは確実に無効化されますが、閉じられるわけではありません。背後の Win32 ハンドルがハンドル
   オブジェクトよりも長く維持される必要がある場合にはこの関数を呼び出すとよいでしょう。

.. method:: PyHKEY.__enter__()
            PyHKEY.__exit__(\*exc_info)

   HKEY オブジェクトは :meth:`__enter__`, :meth:`__exit__` メソッドを実装していて、
   :keyword:`with` 文のためのコンテキストプロトコルをサポートしています。 ::

      with OpenKey(HKEY_LOCAL_MACHINE, "foo") as key:
          # ... key を使った処理 ...

   このコードは、 :keyword:`with` ブロックから抜けるときに自動的に *key* を閉じます。

   .. versionadded:: 2.6

