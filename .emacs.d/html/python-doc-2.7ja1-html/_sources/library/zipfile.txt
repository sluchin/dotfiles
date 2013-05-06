
:mod:`zipfile` --- ZIP アーカイブの処理
=======================================

.. module:: zipfile
   :synopsis: ZIP-フォーマットのアーカイブファイルを読み書きする
.. moduleauthor:: James C. Ahlstrom <jim@interet.com>
.. sectionauthor:: James C. Ahlstrom <jim@interet.com>


.. % Japanese translation by Yasushi Mausda <y.masuda@acm.org>

.. versionadded:: 1.6

ZIP は一般によく知られているアーカイブ（書庫化）および圧縮の標準ファイルフォーマットです。このモジュールでは ZIP
形式のファイルの作成、読み書き、追記、書庫内のファイル一覧の作成を行うためのツールを提供します。より高度な使い方でこのモジュールを利用したいなら、
`PKZIP Application Note <http://www.pkware.com/documents/casestudies/APPNOTE.TXT>`_. に定義されている
ZIP ファイルフォーマットを理解することが必要になるでしょう。

このモジュールは現在のところ、マルチディスク ZIP ファイルを扱うことはできません
ZIP64 拡張を利用する ZIP ファイル (サイズが 4GB を超えるような ZIP ファイル) は扱えます。
このモジュールは暗号化されたアーカイブの復号をサポートしますが、現在のところ、暗号化ファイルを作成することはできません。
C言語ではなく、Pythonで実装されているため、復号は非常に遅いです。

他のアーカイブ形式については、 :mod:`bz2` 、 :mod:`gzip` 、および、 :mod:`tarfile` モジュールを参照下さい。

このモジュールでは、以下の項目が定義されています:

.. % raise error エラーの送出


.. exception:: BadZipfile

   不備のある ZIP ファイル操作の際に送出されるエラー (旧名称: ``zipfile.error``)


.. exception:: LargeZipFile

   ZIP ファイルが ZIP64 の機能を必要とするとき、その機能が有効にされていないと送出されるエラー


.. class:: ZipFile
   :noindex:

   ZIP ファイルの読み書きのためのクラスです。コンストラクタの詳細については、 :ref:`zipfile-objects` 節)
   を参照してください。


.. class:: PyZipFile

   Python ライブラリを含む ZIP アーカイブを生成するためのクラスです。


.. class:: ZipInfo([filename[, date_time]])

   アーカイブ中のメンバに関する情報を提供するために用いられるクラスです。このクラスのインスタンスは
   :class:`ZipFile` オブジェクトの :meth:`getinfo` および :meth:`infolist` メソッドによって返されます。
   :mod:`zipfile` モジュールを利用するほとんどのユーザはこのオブジェクトを自ら生成する必要はなく、
   モジュールが生成して返すオブジェクトを利用するだけでしょう。 *filename* はアーカイブメンバの完全な名前で、
   *date_time* はファイルの最終更新時刻を記述する、6つのフィールドからなるタプルでなくてはなりません。
   各フィールドについては :ref:`zipinfo-objects` 節を参照してください。


.. function:: is_zipfile(filename)

   *filename* が正しいマジックナンバをもつ ZIP ファイルのときに ``True`` を返し、
   そうでない場合 ``False`` を返します。
   *filename* にはファイルやファイルライクオブジェクトを渡すこともできます。

   .. versionchanged:: 2.7
      ファイルやファイルライクオブジェクトをサポート


.. data:: ZIP_STORED

   アーカイブメンバが圧縮されていないことを表す数値定数です。


.. data:: ZIP_DEFLATED

   通常の ZIP 圧縮手法を表す数値定数。ZIP 圧縮は zlib モジュールを必要とします。現在のところ他の圧縮手法はサポートされていません。


.. seealso::

   `PKZIP Application Note <http://www.pkware.com/documents/casestudies/APPNOTE.TXT>`_
      ZIP ファイル形式およびアルゴリズムを作成した  Phil Katz によるドキュメント。

   `Info-ZIP Home Page <http://www.info-zip.org/>`_
      Info-ZIP プロジェクトによる ZIP アーカイブプログラム及びプログラム開発ライブラリに関する情報。


.. _zipfile-objects:

ZipFile オブジェクト
--------------------


.. class:: ZipFile(file[, mode[, compression[, allowZip64]]])

   ZIP ファイルを開きます。 *file* はファイルへのパス名 (文字列) またはファイルのように振舞うオブジェクトのどちらでもかまいません。 *mode*
   パラメタは、既存のファイルを読むためには ``'r'`` 、既存のファイルを切り詰めたり新しいファイルに書き込むためには ``'w'`` 、
   追記を行うためには ``'a'`` でなくてはなりません。 *mode* が ``'a'`` で *file* が既存の ZIP ファイルを
   参照している場合、追加するファイルは既存のファイル中の ZIP アーカイブに追加されます。 *file* が ZIP を参照していない場合、新しい ZIP
   アーカイブが生成され、既存のファイルの末尾に追加されます。
   このことは、ある ZIP ファイルを他のファイル(例えば :file:`python.exe`)に
   追加することを意味しています。

   .. versionchanged:: 2.6
      *mode* が ``'a'`` でファイルが存在しない場合、ファイルが作られるようになりました。

   *compression* はアーカイブを書き出すときの ZIP 圧縮法で、 :const:`ZIP_STORED`
   または :const:`ZIP_DEFLATED` でなくてはなりません。
   不正な値を指定すると :exc:`RuntimeError` が送出されます。
   また、 :const:`ZIP_DEFLATED` 定数が指定されているのに :mod:`zlib` を利用することが
   できない場合も、 :exc:`RuntimeError` が送出されます。
   デフォルト値は :const:`ZIP_STORED` です。
   *allowZip64* が ``True`` ならば 2GB より大きな ZIP ファイルの作成時に
   ZIP64 拡張を使用します。これが ``False`` (デフォルト) ならば、 :mod:`zipfile`
   モジュールは ZIP64 拡張が必要になる場面で例外を送出します。
   ZIP64 拡張はデフォルトでは無効にされていますが、これは Unix の :program:`zip` および
   :program:`unzip` (InfoZIP ユーティリティ) コマンドがこの拡張をサポートしていないからです。

   .. versionchanged:: 2.7.1
      ファイルが ``'a'`` か ``'w'`` モードで作成されて、アーカイブに1ファイルも
      追加しないまま :meth:`close` した場合に、空のアーカイブとして正しいZIPの
      構造がそのファイルに書かれるようになりました。

   ZipFile はコンテキストマネージャーにもなっているので、 :keyword:`with` 文をサポートしています。
   次の例では、 *myzip* は :keyword:`with` 文のブロックが終了したときに、
   (たとえ例外が発生したとしても) close されます。 ::

      with ZipFile('spam.zip', 'w') as myzip:
          myzip.write('eggs.txt')

   .. versionadded:: 2.7
      :class:`ZipFile` にコンテキストマネージャーの機能を追加しました。


.. method:: ZipFile.close()

   アーカイブファイルを閉じます。 :meth:`close` はプログラムを終了する前に必ず呼び出さなければなりません。
   さもないとアーカイブ上の重要なレコードが書き込まれません。


.. method:: ZipFile.getinfo(name)

   アーカイブメンバ *name* に関する情報を持つ :class:`ZipInfo`  オブジェクトを返します。
   アーカイブに含まれないファイル名に対して :meth:`getinfo` を呼び出すと、 :exc:`KeyError` が送出されます。

.. method:: ZipFile.infolist()

   アーカイブに含まれる各メンバの :class:`ZipInfo` オブジェクトからなるリストを返します。既存のアーカイブファイルを開いている場合、
   リストの順番は実際の ZIP ファイル中のメンバの順番と同じになります。


.. method:: ZipFile.namelist()

   アーカイブメンバの名前のリストを返します。

.. method:: ZipFile.open(name[, mode[, pwd]])

   アーカイブからメンバーを file-like オブジェクト (ZipExtFile) として展開します。 *name* は
   アーカイブに含まれるファイル名、もしくは、 :class:`ZipInfo` オブジェクトです。 *mode*
   パラメーターを指定するならば、以下のうちのどれかである必要があります: ``'r'`` (デフォルト)、
   ``'U'`` 、 ``'rU'``
   ``'U'`` か  ``'rU'`` を選ぶと、読み出し専用オブジェクトにおいて universal newline
   support が有効化されます。 *pwd* は、暗号化ファイルで使われるパスワードです。
   閉じられた ZIP ファイルに対して :meth:`open` を呼び出すと、 :exc:`RuntimeError` が送出されます。

   .. note::

      file-like オブジェクトは読み出し専用で、以下のメソッドを提供します:
      :meth:`!read`, :meth:`!readline`, :meth:`!readlines`, :meth:`!__iter__`,
      :meth:`!next`

   .. note::

      file-like オブジェクトをコンストラクターの第一引数として、 ZipFile が作成された場合、
      ZipFile のファイルポインターを使った :meth:`.open` メソッドにより、オブジェクトが返されます。
      この場合、 :meth:`.open` で返されたオブジェクトに対し、 ZipFile オブジェクトに対する追加の
      操作をしてはいけません。もし、 ZipFile が文字列 (ファイル名) をコンストラクターに対する第一引数として
      作成されたなら、 :meth:`.open` は、 ZipExtFile に含まれる、ZipFile と独立して操作することができる、
      ファイルオブジェクトを新規に作成します。


   .. note::

      :meth:`open`, :meth:`read`, および、 :meth:`extract` の各メソッドはファイル名、
      もしくは、 :class:`ZipInfo` オブジェクトを引数にとれます。
      これは、名前が重複するメンバーを持つ ZIP ファイルを読み出すときに役に立つでしょう。

   .. versionadded:: 2.6


.. method:: ZipFile.extract(member[, path[, pwd]])

   メンバーをアーカイブからカレントワーキングディレクトリに展開します。 *member* は、
   展開するファイルのフルネーム、もしくは、 :class:`ZipInfo` オブジェクトでなければなりません。
   ファイル情報は、可能な限り正確に展開されます。 *path* は展開先のディレクトリを指定します。
   *member* はファイル名、もしくは、 :class:`ZipInfo` オブジェクトです。
   *pwd* は暗号化ファイルに使われるパスワードです。

   .. versionadded:: 2.6


.. method:: ZipFile.extractall([path[, members[, pwd]]])

   すべてのメンバーをアーカイブからカレントワーキングディレクトリに展開します。 *path* は、
   展開先のディレクトリを指定します。 *members* は、オプションで、
   :meth:`namelist` で返されるリストの部分集合でなければなりません。 *pwd* は、暗号化ファイルに
   使われるパスワードです。

   .. warning
      Never extract archives from untrusted sources without prior inspection.
      It is possible that files are created outside of *path*, e.g. members
      that have absolute filenames starting with ``"/"`` or filenames with two
      dots ``".."``.

   .. warning::

      信頼できないソースからきた Zip ファイルを、事前に中身をチェックせずに
      展開してはいけません。ファイルを *path* の外側に作成することができるからです。
      例えば、 ``"/"`` で始まる絶対パスを持ったメンバーや、 2 つのドット
      ``".."`` を持つファイル名などの場合です。

   .. versionadded:: 2.6


.. method:: ZipFile.printdir()

   アーカイブの目次を ``sys.stdout`` に出力します。

.. method:: ZipFile.setpassword(pwd)

   *pwd* を展開する圧縮ファイルのデフォルトパスワードとして指定します。

   .. versionadded:: 2.6


.. method:: ZipFile.read(name[, pwd])

   アーカイブ中のファイル名 *name* の内容をバイト列にして返します。 *name* はアーカイブに含まれるファイル、
   もしくは、 :class:`ZipInfo` オブジェクトの名前です。
   アーカイブは読み込みまたは追記モードで開かれていなくてはなりません。
   *pwd* は暗号化されたファイルのパスワードで、指定された場合、 :meth:`setpassword` で指定された
   デフォルトのパスワードを上書きします。
   閉じられた ZipFile に対し :meth:`read` を呼び出すと、 :exc:`RuntimeError` が送出されます。

   .. versionchanged:: 2.6
      *pwd* が追加され、 *name* に :class:`ZipInfo` オブジェクトを指定できるようになりました。



.. method:: ZipFile.testzip()

   アーカイブ中の全てのファイルを読み、CRC チェックサムとヘッダが正常か調べます。
   最初に見つかった不正なファイルの名前を返します。不正なファイルがなければ ``None`` を返します。
   閉じた ZipFile に対して :meth:`testzip` メソッドを呼び出すと、 :exc:`RuntimeError` が送出されます。

.. method:: ZipFile.write(filename[, arcname[, compress_type]])

   *filename* に指定したファイル名を持つファイルを、アーカイブ名を *arcname* (デフォルトでは *filename* と同じですが
   ドライブレターと先頭にあるパスセパレータは取り除かれます) にしてアーカイブに収録します。 *compress_type*
   を指定した場合、コンストラクタを使って新たなアーカイブエントリを生成した際に使った *compression* パラメタを上書きします。
   アーカイブのモードは ``'w'`` または ``'a'`` でなくてはなりません。
   モードが ``'r'`` で作成された ZipFile に対し :meth:`write` メソッドを呼び出すと、
   :exc:`RuntimeError` が送出されます。閉じた ZipFile に対し :meth:`write` メソッドを呼び出すと、
   :exc:`RuntimeError` が送出されます。


   .. note::

      ZIP ファイル中のファイル名に関する公式なエンコーディング方式はありません。もしユニコードのファイル名が付けられているならば、それを
      :meth:`write` に渡す前に望ましいエンコーディングでバイト列に変換しなければなりません。 WinZip は全てのファイル名を DOS Latin
      としても知られる CP437 で解釈します。

   .. note::

      アーカイブ名はアーカイブルートに対する相対的なものでなければなりません。
      言い換えると、アーカイブ名はパスセパレータで始まってはいけません。

   .. note::

      もし、 ``arcname`` (``arcname`` が与えられない場合は、 ``filename``) が null byte を含むなら、
      アーカイブ中のファイルのファイル名は、 null byte までで、切り詰められます。

.. method:: ZipFile.writestr(zinfo_or_arcname, bytes[, compress_type])

   文字列 *bytes* をアーカイブに書き込みます。
   *zinfo_or_arcname* はアーカイブ中で指定するファイル名か、または :class:`ZipInfo` インスタンス
   を指定します。
   *zinfo_or_arcname* に :class:`ZipInfo` インスタンスを指定する場合、 *zinfo* インスタンスには
   少なくともファイル名、日付および時刻を指定しなければなりません。ファイル名を指定した場合、
   日付と時刻には現在の日付と時間が設定されます。アーカイブはモード ``'w'`` または ``'a'`` で
   開かれていなければなりません。
   閉じた ZipFile に対し :meth:`writestr` メソッドを呼び出すと :exc:`RuntimeError` が送出されます。

   *compressed_type* が指定された場合、その値はコンストラクタに与えられた *compression*
   の値か、 *zinfo_or_arcname* が :class:`ZipInfo` のインスタンスだったときはその値を
   オーバーライドします。

   .. note::

      :class:`ZipInfo` インスタンスを、引数 *zinfo_or_arcname* として与えた場合、
      与えられた :class:`ZipInfo` インスタンスのメンバーである、 *compress_type*
      で指定された圧縮方法が使われます。デフォルトでは、
      :class:`ZipInfo` コンストラクターが、このメンバーを :const:`ZIP_STORED` に設定します。

   .. versionchanged:: 2.7
      *compression_type* 引数

以下のデータ属性も利用することができます。


.. attribute:: ZipFile.debug

   使用するデバッグ出力レベル。この属性は ``0`` (デフォルト、何も出力しない) から ``3``
   (最も多くデバッグ情報を出力する) までの値に設定することができます。
   デバッグ情報は  ``sys.stdout`` に出力されます。

.. attribute:: ZipFile.comment

   ZIP ファイルの付けられたコメントです。
   モードが 'a'、または、'w'で作成された :class:`ZipFile` インスタンスにコメントを付ける場合、
   コメント 65535 byte 以下の文字列でなければなりません。コメントがそれより長い場合、
   アーカイブでは、 :meth:`ZipFile.close` メソッドが呼び出された時点で切り詰められます。

.. _pyzipfile-objects:

PyZipFile オブジェクト
----------------------

:class:`PyZipFile` コンストラクタは :class:`ZipFile` コンストラクタと同じパラメタを必要とします。インスタンスは
:class:`ZipFile` のメソッドの他に、追加のメソッドを一つ持ちます。


.. method:: PyZipFile.writepy(pathname[, basename])

   :file:`\*.py` ファイルを探し、 :file:`\*.py` ファイルに対応するファイルをアーカイブに追加します。
   対応するファイルとは、もしあれば :file:`\*.pyo` であり、そうでなければ :file:`\*.pyc` で、
   必要に応じて :file:`\*.py` からコンパイルします。
   もし pathname がファイルなら、ファイル名は :file:`.py` で終わっていなければなりません。
   また、(:file:`\*.py` に対応する :file:`\*.py[co]`) ファイルはアーカイブのトップレベルに (パス情報なしで) 追加されます。
   もし pathname が :file:`.py` で終わらないファイル名なら :exc:`RuntimeError` を送出します。
   もし pathname がディレクトリで、ディレクトリがパッケージディレクトリでないなら、
   全ての :file:`\*.py[co]` ファイルはトップレベルに追加されます。
   もしディレクトリがパッケージディレクトリなら、全ての :file:`\*.py[co]` ファイルはパッケージ名の
   名前をもつファイルパスの下に追加されます。
   サブディレクトリがパッケージディレクトリなら、それらは再帰的に追加されます *basename* はクラス内部
   での呼び出しに使用するためのものです。
   :meth:`writepy` メソッドは以下のようなファイル名を持ったアーカイブを生成します。 ::

      string.pyc                    # トップレベル名
      test/__init__.pyc             # パッケージディレクトリ
      test/test_support.pyc         # test.test_suport モジュール
      test/bogus/__init__.pyc       # サブパッケージディレクトリ
      test/bogus/myfile.pyc         # test.bogus.myfile サブモジュール


.. _zipinfo-objects:

ZipInfo オブジェクト
--------------------

:class:`ZipFile` オブジェクトの :meth:`getinfo` および :meth:`infolist` メソッドは
:class:`ZipInfo` クラスのインスタンスを返します。それぞれのインスタンスオブジェクトは ZIP アーカイブの
一個のメンバについての情報を保持しています。

   インスタンスは以下の属性を持ちます:


.. attribute:: ZipInfo.filename

   アーカイブ中のファイルの名前。


.. attribute:: ZipInfo.date_time

   アーカイブメンバの最終更新日時。この属性は6つの値からなるタプルです。:

   +-------+-------------------+
   | Index | Value             |
   +=======+===================+
   | ``0`` | 西暦年            |
   +-------+-------------------+
   | ``1`` | 月 (1 から始まる) |
   +-------+-------------------+
   | ``2`` | 日 (1 から始まる) |
   +-------+-------------------+
   | ``3`` | 時 (0 から始まる) |
   +-------+-------------------+
   | ``4`` | 分 (0 から始まる) |
   +-------+-------------------+
   | ``5`` | 秒 (0 から始まる) |
   +-------+-------------------+


.. attribute:: ZipInfo.compress_type

   アーカイブメンバの圧縮形式。


.. attribute:: ZipInfo.comment

   各アーカイブメンバに対するコメント。


.. attribute:: ZipInfo.extra

   拡張フィールドデータ。この文字列データに含まれているデータの内部構成については、 `PKZIP Application Note
   <http://www.pkware.com/documents/casestudies/APPNOTE.TXT>`_
   でコメントされています。


.. attribute:: ZipInfo.create_system

   ZIP アーカイブを作成したシステムを記述する文字列。


.. attribute:: ZipInfo.create_version

   このアーカイブを作成した PKZIP のバージョン。


.. attribute:: ZipInfo.extract_version

   このアーカイブを展開する際に必要な PKZIP のバージョン。


.. attribute:: ZipInfo.reserved

   予約領域。ゼロでなくてはなりません。


.. attribute:: ZipInfo.flag_bits

   ZIP フラグビット列。


.. attribute:: ZipInfo.volume

   ファイルヘッダのボリュームナンバ。


.. attribute:: ZipInfo.internal_attr

   内部属性。


.. attribute:: ZipInfo.external_attr

   外部ファイル属性。


.. attribute:: ZipInfo.header_offset

   ファイルヘッダへのバイト数で表したオフセット。


.. attribute:: ZipInfo.CRC

   圧縮前のファイルの CRC-32 チェックサム。


.. attribute:: ZipInfo.compress_size

   圧縮後のデータのサイズ。


.. attribute:: ZipInfo.file_size

   圧縮前のファイルのサイズ。

