:mod:`msilib` --- Microsoft インストーラーファイルの読み書き
============================================================

.. module:: msilib
   :platform: Windows
   :synopsis: Creation of Microsoft Installer files, and CAB files.
.. moduleauthor:: Martin v. Löwis <martin@v.loewis.de>
.. sectionauthor:: Martin v. Löwis <martin@v.loewis.de>


.. index:: single: msi

.. versionadded:: 2.5

:mod:`msilib` モジュールは Microdoft インストーラー(``.msi``)の
作成を支援します。このファイルはしばしば埋め込まれた「キャビネット」ファイル (``.cab``) を含むので、CAB ファイル作成用の API
も暴露します。現在のところ ``.cab`` ファイルの読み出しはサポートしていませんが、 ``.msi`` データベースの読み出しサポートは可能です。

このパッケージの目的は ``.msi`` ファイルにある全てのテーブルへの完全なアクセスの提供なので、提供されているものは正直に言って低レベルな API
です。このパッケージの二つの主要な応用は :mod:`distutils` の ``bdist_msi`` コマンドと、Python
インストーラーパッケージそれ自体(と言いつつ現在は別バージョンの ``msilib`` を使っているのですが)です。

パッケージの内容は大きく四つのパートに分けられます。低レベル CAB ルーチン、低レベル MSI ルーチン、少し高レベルの MSI ルーチン、
標準的なテーブル構造、の四つです。


.. function:: FCICreate(cabname, files)

   新しい CAB ファイルを *cabname* という名前で作ります。 *files* はタプルのリストで、それぞれのタプルがディスク上のファイル名と CAB
   ファイルで付けられるファイル名とからなるものでなければなりません。

   ファイルはリストに現れた順番で CAB ファイルに追加されます。全てのファイルは MSZIP 圧縮アルゴリズムを使って一つの CAB ファイルに追加されます。

   MSI 作成の様々なステップに対する Python コールバックは現在暴露されていません。


.. function:: UuidCreate()

   新しい一意識別子の文字列表現を返します。この関数は Windows API の関数 :c:func:`UuidCreate` と
   :c:func:`UuidToString` をラップしたものです。


.. function:: OpenDatabase(path, persist)

   MsiOpenDatabase を呼び出して新しいデータベースオブジェクトを返します。 *path* は MSI ファイルのファイル名です。 *persist*
   は五つの定数 ``MSIDBOPEN_CREATEDIRECT``, ``MSIDBOPEN_CREATE``, ``MSIDBOPEN_DIRECT``,
   ``MSIDBOPEN_READONLY``, ``MSIDBOPEN_TRANSACT`` のどれか一つで、フラグ
   ``MSIDBOPEN_PATCHFILE`` を含めても構いません。これらのフラグの意味は Microsoft のドキュメントを参照してください。
   フラグに依って既存のデータベースを開いたり新しいのを作ったりします。


.. function:: CreateRecord(count)

   :c:func:`MSICreateRecord` を呼び出して新しいレコードオブジェクトを返します。 *count* はレコードのフィールドの数です。


.. function:: init_database(name, schema, ProductName, ProductCode, ProductVersion, Manufacturer)

   *name* という名前の新しいデータベースを作り、 *schema* で初期化し、プロパティ *ProductName*, *ProductCode*,
   *ProductVersion*, *Manufacturer* をセットして、返します

   *schema* は ``tables`` と ``_Validation_records`` という属性を
   もったモジュールオブジェクトでなければなりません。典型的には、 :mod:`msilib.schema` を使うべきです。

   データベースはこの関数から返された時点でスキーマとバリデーションレコードだけが収められています。


.. function:: add_data(database, table, records)

   全ての *records* を *database* の *table* テーブルに追加します。

   *table* 引数は MSI スキーマで事前に定義されたテーブルでなければなりません。
   例えば、 ``'Feature'``, ``'File'``, ``'Component'``, ``'Dialog'``, ``'Control'``,
   などです。

   *records* はタプルのリストで、それぞれのタプルにはテーブルのスキーマに従った
   レコードの全てのフィールドを含んでいるものでなければなりません。オプションのフィールドには ``None`` を渡すことができます。

   フィールドの値には、整数・長整数・文字列・Binary クラスのインスタンスが使えます。


.. class:: Binary(filename)

   Binary テーブル中のエントリーを表わします。 :func:`add_data` を使ってこのクラスのオブジェクトを挿入するときには *filename*
   という名前のファイルをテーブルに読み込みます。


.. function:: add_tables(database, module)

   *module* の全てのテーブルの内容を *database* に追加します。 *module* は *tables*
   という内容が追加されるべき全てのテーブルのリストと、テーブルごとに一つある実際の内容を持っている属性とを含んでいなければなりません。

   この関数は典型的にシーケンステーブルをインストールするのに使われます。


.. function:: add_stream(database, name, path)

   *database* の ``_Stream`` テーブルに、ファイル *path* を *name* というストリーム名で追加します。


.. function:: gen_uuid()

   新しい UUID を、 MSI が通常要求する形式(すなわち、中括弧に入れ、16進数は大文字)で返します。


.. seealso::

   `FCICreateFile <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/devnotes/winprog/fcicreate.asp>`_
   `UuidCreate <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/rpc/rpc/uuidcreate.asp>`_
   `UuidToString <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/rpc/rpc/uuidtostring.asp>`_

.. _database-objects:

データベースオブジェクト
------------------------


.. method:: Database.OpenView(sql)

   :c:func:`MSIDatabaseOpenView` を呼び出してビューオブジェクトを返します。 *sql* は実行される SQL 命令です。


.. method:: Database.Commit()

   :c:func:`MSIDatabaseCommit` を呼び出して現在のトランザクションで保留されている変更をコミットします。


.. method:: Database.GetSummaryInformation(count)

   :c:func:`MsiGetSummaryInformation` を呼び出して新しいサマリー情報オブジェクトを返します。 *count*
   は更新された値の最大数です。


.. seealso::

   `MSIDatabaseOpenView <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/msidatabaseopenview.asp>`_
   `MSIDatabaseCommit <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/msidatabasecommit.asp>`_
   `MSIGetSummaryInformation <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/msigetsummaryinformation.asp>`_

.. _view-objects:

ビューオブジェクト
------------------


.. method:: View.Execute(params)

   :c:func:`MSIViewExecute` を通してビューに対する SQL 問い合わせを実行します。
   *params* が ``None`` でない場合、
   クエリ中のパラメータトークンの実際の値を与えるものです。


.. method:: View.GetColumnInfo(kind)

   :c:func:`MsiViewGetColumnInfo` の呼び出しを通してビューのカラムを説明するレコードを返します。 *kind* は
   ``MSICOLINFO_NAMES`` または ``MSICOLINFO_TYPES`` です。


.. method:: View.Fetch()

   :c:func:`MsiViewFetch` の呼び出しを通してクエリの結果レコードを返します。


.. method:: View.Modify(kind, data)

   :c:func:`MsiViewModify` を呼び出してビューを変更します。 *kind* は ``MSIMODIFY_SEEK``,
   ``MSIMODIFY_REFRESH``, ``MSIMODIFY_INSERT``, ``MSIMODIFY_UPDATE``,
   ``MSIMODIFY_ASSIGN``, ``MSIMODIFY_REPLACE``, ``MSIMODIFY_MERGE``,
   ``MSIMODIFY_DELETE``, ``MSIMODIFY_INSERT_TEMPORARY``, ``MSIMODIFY_VALIDATE``,
   ``MSIMODIFY_VALIDATE_NEW``, ``MSIMODIFY_VALIDATE_FIELD``,
   ``MSIMODIFY_VALIDATE_DELETE`` のいずれかです。

   *data* は新しいデータを表わすレコードでなければなりません。


.. method:: View.Close()

   :c:func:`MsiViewClose` を通してビューを閉じます。


.. seealso::

   `MsiViewExecute <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/msiviewexecute.asp>`_
   `MSIViewGetColumnInfo <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/msiviewgetcolumninfo.asp>`_
   `MsiViewFetch <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/msiviewfetch.asp>`_
   `MsiViewModify <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/msiviewmodify.asp>`_
   `MsiViewClose <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/msiviewclose.asp>`_

.. _summary-objects:

サマリー情報オブジェクト
------------------------


.. method:: SummaryInformation.GetProperty(field)

   :c:func:`MsiSummaryInfoGetProperty` を通してサマリーのプロパティを返します。 *field* はプロパティ名で、定数
   ``PID_CODEPAGE``, ``PID_TITLE``, ``PID_SUBJECT``, ``PID_AUTHOR``,
   ``PID_KEYWORDS``, ``PID_COMMENTS``, ``PID_TEMPLATE``, ``PID_LASTAUTHOR``,
   ``PID_REVNUMBER``, ``PID_LASTPRINTED``, ``PID_CREATE_DTM``,
   ``PID_LASTSAVE_DTM``, ``PID_PAGECOUNT``, ``PID_WORDCOUNT``, ``PID_CHARCOUNT``,
   ``PID_APPNAME``, ``PID_SECURITY`` のいずれかです。


.. method:: SummaryInformation.GetPropertyCount()

   :c:func:`MsiSummaryInfoGetPropertyCount` を通してサマリープロパティの個数を返します。


.. method:: SummaryInformation.SetProperty(field, value)

   :c:func:`MsiSummaryInfoSetProperty` を通してプロパティをセットします。 *field* は
   :meth:`GetProperty` におけるものと同じ値をとります。 *value* はプロパティの新しい値です。許される値の型は整数と文字列です。


.. method:: SummaryInformation.Persist()

   :c:func:`MsiSummaryInfoPersist` を使って変更されたプロパティをサマリー情報ストリームに書き込みます。


.. seealso::

   `MsiSummaryInfoGetProperty <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/msisummaryinfogetproperty.asp>`_
   `MsiSummaryInfoGetPropertyCount <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/msisummaryinfogetpropertycount.asp>`_
   `MsiSummaryInfoSetProperty <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/msisummaryinfosetproperty.asp>`_
   `MsiSummaryInfoPersist <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/msisummaryinfopersist.asp>`_

.. _record-objects:

レコードオブジェクト
--------------------


.. method:: Record.GetFieldCount()

   :c:func:`MsiRecordGetFieldCount` を通してレコードのフィールド数を返します。


.. method:: Record.GetInteger(field)

   *field* の値を可能なら整数として返します。 *field* は整数でなければなりません。


.. method:: Record.GetString(field)

   *field* の値を可能なら文字列として返します。 *field* は整数でなければなりません。


.. method:: Record.SetString(field, value)

   :c:func:`MsiRecordSetString` を通して *field* を *value* にセットします。 *field* は整数、 *value*
   は文字列でなければなりません。


.. method:: Record.SetStream(field, value)

   :c:func:`MsiRecordSetStream` を通して *field* を *value* という名のファイルの内容にセットします。 *field*
   は整数、 *value* は文字列でなければなりません。


.. method:: Record.SetInteger(field, value)

   :c:func:`MsiRecordSetInteger` を通して *field* を *value* にセットします。 *field* も *value*
   も整数でなければなりません。


.. method:: Record.ClearData()

   :c:func:`MsiRecordClearData` を通してレコードの全てのフィールドを 0 にセットします。


.. seealso::

   `MsiRecordGetFieldCount <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/msirecordgetfieldcount.asp>`_
   `MsiRecordSetString <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/msirecordsetstring.asp>`_
   `MsiRecordSetStream <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/msirecordsetstream.asp>`_
   `MsiRecordSetInteger <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/msirecordsetinteger.asp>`_
   `MsiRecordClear <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/msirecordclear.asp>`_

.. _msi-errors:

エラー
------

全ての MSI 関数のラッパーは :exc:`MsiError` を送出します。例外の内部の文字列がより詳細な情報を含んでいます。


.. _cab:

CAB オブジェクト
----------------


.. class:: CAB(name)

   :class:`CAB` クラスは CAB ファイルを表わすものです。MSI 構築中、ファイルは ``Files`` テーブルと CAB
   ファイルとに同時に追加されます。そして、全てのファイルを追加し終えたら、CAB ファイルは書き込まれることが可能になり、MSI ファイルに追加されます。

   *name* は MSI ファイル中の CAB ファイルの名前です。


   .. method:: append(full, logical)

      パス名 *full* のファイルを CAB ファイルに *logical* という名で追加します。 *logical*
      という名が既に存在したならば、新しいファイル名が作られます。

      ファイルの CAB ファイル中のインデクスと新しいファイル名を返します。


   .. method:: commit(database)

      CAB ファイルを作り、MSI ファイルにストリームとして追加し、 ``Media`` テーブルに送り込み、作ったファイルはディスクから削除します。


.. _msi-directory:

ディレクトリオブジェクト
------------------------


.. class:: Directory(database, cab, basedir, physical,  logical, default, [componentflags])

   新しいディレクトリを Directory テーブルに作成します。ディレクトリには各時点で現在のコンポーネントがあり、それは
   :meth:`start_component` を使って明ら様に作成されたかまたは最初にファイルが追加された際に暗黙裡に作成されたものです。
   ファイルは現在のコンポーネントと cab ファイルに追加されます。ディレクトリを作成するには親ディレクトリオブジェクト(``None`` でも可)、
   物理的ディレクトリへのパス、論理的ディレクトリ名を指定する必要があります。 *default* はディレクトリテーブルの DefaultDir
   スロットを指定します。 *componentflags* は新しいコンポーネントが得るデフォルトのフラグを指定します。


   .. method:: start_component([component[, feature[, flags[, keyfile[, uuid]]]]])

      エントリを Component テーブルに追加し、このコンポーネントをこのディレクトリの
      現在のコンポーネントにします。もしコンポーネント名が与えられなければディレクトリ名が使われます。 *feature*
      が与えられなければ、ディレクトリのデフォルトフラグが使われます。 *keyfile* が与えられなければ、Component テーブルの KeyPath は
      null のままになります。


   .. method:: add_file(file[, src[, version[, language]]])

      ファイルをディレクトリの現在のコンポーネントに追加します。このとき現在のコンポーネントが
      なければ新しいものを開始します。デフォルトではソースとファイルテーブルのファイル名は同じになります。 *src*
      ファイルが与えられたならば、それば現在のディレクトリから相対的に解釈されます。オプションで *version* と *language* を File
      テーブルのエントリ用に指定することができます。


   .. method:: glob(pattern[, exclude])

      現在のコンポーネントに glob パターンで指定されたファイルのリストを追加します。個々のファイルを *exclude* リストで除外することができます。


   .. method:: remove_pyc()

      アンインストールの際に ``.pyc`` / ``.pyo`` を削除します。


.. seealso::

   `Directory Table <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/directory_table.asp>`_
   `File Table <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/file_table.asp>`_
   `Component Table <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/component_table.asp>`_
   `FeatureComponents Table <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/featurecomponents_table.asp>`_

.. _features:

フィーチャー
------------


.. class:: Feature(database, id, title, desc, display[, level=1[, parent[, directory[,  attributes=0]]]])

   *id*, *parent.id*, *title*, *desc*, *display*, *level*, *directory*,
   *attributes* の値を使って、新しいレコードを ``Feature`` テーブルに追加します。出来上がったフィーチャーオブジェクトは
   :class:`Directory` の :meth:`start_component` メソッドに渡すことができます。


   .. method:: set_current()

      このフィーチャーを :mod:`msilib` の現在のフィーチャーにします。フィーチャーが明ら様に指定されない限り、
      新しいコンポーネントが自動的にデフォルトのフィーチャーに追加されます。


.. seealso::

   `Feature Table <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/feature_table.asp>`_

.. _msi-gui:

GUI クラス
----------

:mod:`msilib` モジュールは MSI データベースの中の GUI テーブルをラップする
幾つかのクラスを提供しています。しかしながら、標準で提供されるユーザーインタフェースはありません。インストールする Python
パッケージに対するユーザーインタフェース付きの MSI ファイルを作成するには :mod:`bdist_msi` を使ってください。


.. class:: Control(dlg, name)

   ダイアログコントロールの基底クラス。 *dlg* はコントロールの属するダイアログオブジェクト、 *name* はコントロールの名前です。


   .. method:: event(event, argument[,  condition=1[, ordering]])

      このコントロールの ``ControlEvent`` テーブルにエントリを作ります。


   .. method:: mapping(event, attribute)

      このコントロールの ``EventMapping`` テーブルにエントリを作ります。


   .. method:: condition(action, condition)

      このコントロールの ``ControlCondition`` テーブルにエントリを作ります。


.. class:: RadioButtonGroup(dlg, name, property)

   *name* という名前のラジオボタンコントロールを作成します。 *property* はラジオボタンが選ばれたときにセットされる
   インストーラープロパティです。


   .. method:: add(name, x, y, width, height, text [, value])

      グループに *name* という名前で、座標 *x*, *y* に大きさが *width*, *height* で *text* というラベルの付いた
      ラジオボタンを追加します。 *value* が省略された場合、デフォルトは *name* になります。


.. class:: Dialog(db, name, x, y, w, h, attr, title, first,  default, cancel)

   新しい :class:`Dialog` オブジェクトを返します。 ``Dialog`` テーブルの中に
   指定された座標、ダイアログ属性、タイトル、最初とデフォルトとキャンセルコントロールの名前を持ったエントリが作られます。


   .. method:: Dialog.control(name, type, x, y, width, height,  attributes, property, text, control_next, help)

      新しい :class:`Control` オブジェクトを返します。 ``Control`` テーブルに指定されたパラメータのエントリが作られます。

      これは汎用のメソッドで、特定の型に対しては特化したメソッドが提供されています。


   .. method:: text(name, x, y, width, height, attributes, text)

      ``Text`` コントロールを追加して返します。


   .. method:: bitmap(name, x, y, width, height, text)

      ``Bitmap`` コントロールを追加して返します。


   .. method:: line(name, x, y, width, height)

      ``Line`` コントロールを追加して返します。


   .. method:: pushbutton(name, x, y, width, height, attributes,  text, next_control)

      ``PushButton`` コントロールを追加して返します。


   .. method:: radiogroup(name, x, y, width, height,  attributes, property, text, next_control)

      ``RadioButtonGroup`` コントロールを追加して返します。


   .. method:: checkbox(name, x, y, width, height,  attributes, property, text, next_control)

      ``CheckBox`` コントロールを追加して返します。


.. seealso::

   `Dialog Table <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/dialog_table.asp>`_
   `Control Table <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/control_table.asp>`_
   `Control Types <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/controls.asp>`_
   `ControlCondition Table <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/controlcondition_table.asp>`_
   `ControlEvent Table <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/controlevent_table.asp>`_
   `EventMapping Table <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/eventmapping_table.asp>`_
   `RadioButton Table <http://msdn.microsoft.com/library/default.asp?url=/library/en-us/msi/setup/radiobutton_table.asp>`_

.. _msi-tables:

事前に計算されたテーブル
------------------------

:mod:`msilib` はスキーマとテーブル定義だけから成るサブパッケージをいくつか提供しています。現在のところ、これらの定義は MSI バージョン
2.0 に基づいています。


.. data:: schema

   これは MSI 2.0 用の標準 MSI スキーマで、テーブル定義のリストを提供する *tables* 変数と、MSI バリデーション用のデータを提供する
   *_Validation_records* 変数があります。


.. data:: sequence

   このモジュールは標準シーケンステーブルのテーブル内容を含んでいます。 *AdminExecuteSequence*, *AdminUISequence*,
   *AdvtExecuteSequence*, *InstallExecuteSequence*, *InstallUISequence* が含まれています。


.. data:: text

   このモジュールは標準的なインストーラーのアクションのための UIText および ActionText テーブルの定義を含んでいます。
