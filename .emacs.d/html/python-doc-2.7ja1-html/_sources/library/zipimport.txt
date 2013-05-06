
:mod:`zipimport` --- Zip アーカイブからモジュールを import する
===============================================================

.. module:: zipimport
   :synopsis: Python モジュールを ZIP アーカイブから import する機能のサポート
.. moduleauthor:: Just van Rossum <just@letterror.com>


.. versionadded:: 2.3

このモジュールは、 Python モジュール (:file:`\ *.py`, :file:`\*.py[co]`) やパッケージを ZIP 形式のアーカイブから
import できるようにします。通常、 :mod:`zipimport` を明示的に使う必要はありません; 組み込みの :keyword:`import`
は、 ``sys.path`` の要素が ZIP  アーカイブへのパスを指している場合にこのモジュールを自動的に使います。

普通、 ``sys.path`` はディレクトリ名の文字列からなるリストです。このモジュールを使うと、 ``sys.path`` の要素に ZIP ファイル\
アーカイブを示す文字列を使えるようになります。ZIP アーカイブにはサブディレクトリ構造を含めることができ、パッケージの import を\
サポートさせしたり、アーカイブ内のパスを指定してサブディレクトリ下から import を行わせたりできます。例えば、
:file:`/tmp/example.zip/lib/` のように指定すると、アーカイブ中の :file:`lib/` サブディレクトリ下だけから
import を行います。

ZIP アーカイブ内にはどんなファイルを置いてもかまいませんが、 import できるのは :file:`.py` および :file:`.py[co]`
だけです。動的モジュール (:file:`.pyd`, :file:`.so`) の ZIP import は行えません。アーカイブ内に
:file:`.py` ファイルしか入っていない場合、 Python がアーカイブを変更して、 :file:`.py` ファイルに対応する
:file:`.pyc` や :file:`.pyo` ファイルを追加したりはしません。つまり、ZIP アーカイブ中に :file:`.pyc` が入っていない\
場合、 import はやや低速になるかもしれないので注意してください。

ZIP アーカイブからロードしたモジュールに対して組み込み関数 :func:`reload` を呼び出すと失敗します; :func:`reload` が\
必要になるということは、実行時に ZIP ファイルが置き換えられてしまうことになり、あまり起こりそうにない状況だからです。

アーカイブコメント付きの ZIP アーカイブは現在のところ未サポートです。

.. seealso::

   `PKZIP Application Note <http://www.pkware.com/documents/casestudies/APPNOTE.TXT>`_
      ZIP ファイル形式の作者であり、ZIP で使われているアルゴリズムの作者でもある
      Phil Katz による、ZIP ファイル形式についてのドキュメントです。

   :pep:`273` - Import Modules from Zip Archives
      このモジュールの実装も行った、James C. Ahlstrom による PEP です。 Python 2.3 は PEP 273 の仕様に従っていますが、
      Just van Rossum の書いた import フックによる実装を使っています。 import フックは PEP 302 で解説されています。

   :pep:`302` - New Import Hooks
      このモジュールを動作させる助けになっている import フックの追加を提案している PEP です。


このモジュールでは例外を一つ定義しています:

.. exception:: ZipImporterError

   zipimporter オブジェクトが送出する例外です。
   :exc:`ImportError` のサブクラスなので、 :exc:`ImportError` としても捕捉できます。


.. _zipimporter-objects:

zipimporter オブジェクト
------------------------

:class:`zipimporter` は ZIP ファイルを import するためのクラスです。

.. class:: zipimporter(archivepath)

   新たな zipimporter インスタンスを生成します。
   *archivepath* は ZIP ファイルへのパスかまたは
   ZIP ファイル中の特定のパスへのパスでなければなりません。
   たとえば、 :file:`foo/bar.zip/lib` という *archivepath* の場合、
   :file:`foo/bar.zip` という ZIP ファイル(が存在するものとして)の中の
   :file:`lib` ディレクトリにあるモジュールを探しに行きます。
   *archivepath* が有効な ZIP アーカイブを指していない場合、
   :exc:`ZipImportError` を送出します。


   .. method:: find_module(fullname[, path])

      *fullname* に指定したモジュールを検索します。
      *fullname* は完全指定の (ドット表記の) モジュール名でなければなりません。
      モジュールが見つかった場合には zipimporter インスタンス自体を返し、
      そうでない場合には :const:`None` を返します。
      *path* 引数は無視されます ---
      この引数は importer プロトコルとの互換性を保つためのものです。


   .. method:: get_code(fullname)

      *fullname* に指定したモジュールのコードオブジェクトを返します。
      モジュールがない場合には :class:`ZipImportError` を送出します。


   .. method:: get_data(pathname)

      *pathname* に関連付けられたデータを返します。
      該当するファイルが見つからなかった場合には :exc:`IOError` を送出します。


   .. method:: get_filename(fullname)

      指定されたモジュールが import さた場合、そのモジュールに設定した
      ``__file__`` の値を返します。
      モジュールが見つからなかった場合は :exc:`ZipImportError` 例外を発生させます。

      .. versionadded:: 2.7


   .. method:: get_source(fullname)

      *fullname* に指定したモジュールのソースコードを返します。
      モジュールが見つからなかった場合には :exc:`ZipImportError` を送出します。
      モジュールは存在するが、ソースコードがない場合には :const:`None` を返します。


   .. method:: is_package(fullname)

      *fullname* で指定されたモジュールがパッケージの場合に :const:`True` を返します。
      モジュールが見つからなかった場合には :exc:`ZipImportError` を送出します。


   .. method:: load_module(fullname)

      *fullname* に指定したモジュールをロードします。
      *fullname* は完全指定の (ドット表記の) モジュール名でなくてはなりません。
      import 済みのモジュールを返します。
      モジュールがない場合には :exc:`ZipImportError` を送出します。

   .. attribute:: archive

      importer に紐付けられた ZIP ファイルのファイル名で、サブパスは含まれません。

   .. attribute:: prefix

      ZIP ファイル中のモジュールを検索するサブパスです。
      この文字列は ZIP ファイルの根を指している zipimporter オブジェクトでは空です。

   アトリビュート :attr:`archive` と :attr:`prefix` とは、スラッシュでつなげると、
   :class:`zipimporter` コンストラクタに渡された元々の *archivepath*
   引数と等しくなります。


.. _zipimport-examples:

使用例
------

モジュールを ZIP アーカイブから import する例を以下に示します -
:mod:`zipimport` モジュールが明示的に使われていないことに注意してください。 ::

   $ unzip -l /tmp/example.zip
   Archive:  /tmp/example.zip
     Length     Date   Time    Name
    --------    ----   ----    ----
        8467  11-26-02 22:30   jwzthreading.py
    --------                   -------
        8467                   1 file
   $ ./python
   Python 2.3 (#1, Aug 1 2003, 19:54:32)
   >>> import sys
   >>> sys.path.insert(0, '/tmp/example.zip')  # パス先頭に .zip ファイル追加
   >>> import jwzthreading
   >>> jwzthreading.__file__
   '/tmp/example.zip/jwzthreading.py'

