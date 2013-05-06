
:mod:`stat` --- :func:`stat` の返す内容を解釈する
=================================================

.. module:: stat
   :synopsis: os.stat()、os.lstat() および os.fstat() の返す内容を解釈するためのユーティリティ群。
.. sectionauthor:: Skip Montanaro <skip@automatrix.com>


:mod:`stat` モジュールでは、 :func:`os.stat` 、 :func:`os.lstat` および :func:`os.fstat`
(存在すれば) の返す内容を解釈するための定数や関数を定義しています。
:c:func:`stat` 、 :c:func:`fstat` 、および :c:func:`lstat`
の関数呼び出しについての完全な記述はシステムのドキュメントを参照してください。

:mod:`stat` モジュールでは、特殊なファイル型を判別するための以下の関数を定義しています:


.. function:: S_ISDIR(mode)

   ファイルのモードがディレクトリの場合にゼロでない値を返します。


.. function:: S_ISCHR(mode)

   ファイルのモードがキャラクタ型の特殊デバイスファイルの場合にゼロでない値を返します。


.. function:: S_ISBLK(mode)

   ファイルのモードがブロック型の特殊デバイスファイルの場合にゼロでない値を返します。


.. function:: S_ISREG(mode)

   ファイルのモードが通常ファイルの場合にゼロでない値を返します。


.. function:: S_ISFIFO(mode)

   ファイルのモードが FIFO (名前つきパイプ) の場合にゼロでない値を返します。


.. function:: S_ISLNK(mode)

   ファイルのモードがシンボリックリンクの場合にゼロでない値を返します。


.. function:: S_ISSOCK(mode)

   ファイルのモードがソケットの場合にゼロでない値を返します。

より一般的なファイルのモードを操作するための二つの関数が定義されています:


.. function:: S_IMODE(mode)

   :func:`os.chmod` で設定することのできる一部のファイルモード ---
   すなわち、ファイルの許可ビット (permission bits) に加え、 (サポートされているシステムでは)
   スティッキービット (sticky bit)、実行グループ ID 設定 (set-group-id) および
   実行ユーザ ID 設定  (set-user-id) ビット --- を返します。


.. function:: S_IFMT(mode)

   ファイルの形式を記述しているファイルモードの一部 (上記の  :func:`S_IS\*` 関数で使われます) を返します。

通常、ファイルの形式を調べる場合には :func:`os.path.is\*` 関数を使うことになります; ここで挙げた関数は同じファイルに対して
複数のテストを同時に行いたいが、 :c:func:`stat` システムコールを何度も呼び出してオーバヘッドが生じるのを避けたい場合に便利です。
これらはまた、ブロック型およびキャラクタ型デバイスに対するテストのように、 :mod:`os.path` で扱うことのできないファイルの
情報を調べる際にも便利です。

例::

   import os, sys
   from stat import *

   def walktree(top, callback):
       '''recursively descend the directory tree rooted at top,
          calling the callback function for each regular file'''

       for f in os.listdir(top):
           pathname = os.path.join(top, f)
           mode = os.stat(pathname)[ST_MODE]
           if S_ISDIR(mode):
               # It's a directory, recurse into it
               walktree(pathname, callback)
           elif S_ISREG(mode):
               # It's a file, call the callback function
               callback(pathname)
           else:
               # Unknown file type, print a message
               print 'Skipping %s' % pathname

   def visitfile(file):
       print 'visiting', file

   if __name__ == '__main__':
       walktree(sys.argv[1], visitfile)

以下の全ての変数は、 :func:`os.stat` 、 :func:`os.fstat` 、または :func:`os.lstat` が返す 10
要素のタプルにおけるインデクスを単にシンボル定数化したものです。


.. data:: ST_MODE

   Iノードの保護モード。


.. data:: ST_INO

   Iノード番号。


.. data:: ST_DEV

   Iノードが存在するデバイス。


.. data:: ST_NLINK

   該当する Iノードへのリンク数。


.. data:: ST_UID

   ファイルの所持者のユーザ ID。


.. data:: ST_GID

   ファイルの所持者のグループ ID。


.. data:: ST_SIZE

   通常ファイルではバイトサイズ; いくつかの特殊ファイルでは処理待ちのデータ量。


.. data:: ST_ATIME

   最後にアクセスした時刻。


.. data:: ST_MTIME

   最後に変更された時刻。


.. data:: ST_CTIME

   オペレーティングシステムから返される"ctime"。あるOS(Unixなど)では最
   後にメタデータが更新された時間となり、別のOS(Windowsなど)では作成時間と
   なります(詳細については各プラットフォームのドキュメントを参照してください)。

"ファイルサイズ" の解釈はファイルの型によって異なります。通常のファイルの場合、サイズはファイルの大きさをバイトで表したものです。ほとんどの Unix 系
(特に Linux) における FIFO やソケットの場合、"サイズ" は :func:`os.stat` 、 :func:`os.fstat` 、あるいは
:func:`os.lstat` を呼び出した時点で読み出し待ちであったデータのバイト数になります; この値は時に有用で、特に上記の特殊なファイル
を非ブロックモードで開いた後にポーリングを行いたいといった場合に便利です。他のキャラクタ型およびブロック型デバイスにおけるサイズ
フィールドの意味はさらに異なっていて、背後のシステムコールの実装によります。

.. The variables below define the flags used in the :data:`ST_MODE` field.

以下の変数は、 :data:`ST_MODE` フィールドで使用されるフラグを定義しています。

.. Use of the functions above is more portable than use of the first set of flags:

最初に挙げる、以下のフラグを使うよりは、上記の関数を使うほうがポータブルです:

.. data:: S_IFMT

   .. Bit mask for the file type bit fields.

   ファイルタイプのビットフィールド用のビットマスク

.. data:: S_IFSOCK

   .. Socket.

   ソケット

.. data:: S_IFLNK

   .. Symbolic link.
   
   シンボリックリンク

.. data:: S_IFREG

   .. Regular file.

   通常のファイル

.. data:: S_IFBLK

   .. Block device.

   ブロックデバイス

.. data:: S_IFDIR

   .. Directory.

   ディレクトリ

.. data:: S_IFCHR

   .. Character device.

   キャラクターデバイス

.. data:: S_IFIFO

   FIFO.

.. The following flags can also be used in the *mode* argument of :func:`os.chmod`:

以下のフラグは、 :func:`os.chmod` の *mode* 引数に使うこともできます:

.. data:: S_ISUID

   .. Set UID bit.

   UID ビットを設定する

.. data:: S_ISGID

   .. Set-group-ID bit.  This bit has several special uses.  For a directory
      it indicates that BSD semantics is to be used for that directory:
      files created there inherit their group ID from the directory, not
      from the effective group ID of the creating process, and directories
      created there will also get the :data:`S_ISGID` bit set.  For a
      file that does not have the group execution bit (:data:`S_IXGRP`)
      set, the set-group-ID bit indicates mandatory file/record locking
      (see also :data:`S_ENFMT`).

   グループIDビットを設定する。このビットには幾つかの特殊ケースがあります。
   ディレクトリに対して設定されていた場合、 BSD のセマンティクスが利用される
   事を示しています。すなわち、そこに作成されるファイルは、作成したプロセスの
   有効グループID (effective group ID) ではなくそのディレクトリのグループIDを
   継承し、そこに作成されるディレクトリにも :data:`S_ISGID` ビットが設定されます。
   グループ実行ビット (:data:`S_IXGRP`) が設定されていないファイルに対して
   このビットが設定されていた場合、強制ファイル/レコードロックを意味します。
   (:data:`S_ENFMT` も参照してください。)

.. data:: S_ISVTX

   .. Sticky bit.  When this bit is set on a directory it means that a file
      in that directory can be renamed or deleted only by the owner of the
      file, by the owner of the directory, or by a privileged process.

   スティッキービット。このビットがディレクトリに対して設定されているとき、
   そのディレクトリ内のファイルは、そのファイルのオーナー、あるいはその
   ディレクトリのオーナーか特権プロセスのみが、リネームや削除をすることが
   出来ることを意味しています。

.. data:: S_IRWXU

   .. Mask for file owner permissions.

   ファイルオーナーの権限に対するマスク

.. data:: S_IRUSR

   .. Owner has read permission.

   オーナーがリード権限を持っている

.. data:: S_IWUSR

   .. Owner has write permission.

   オーナーがライト権限を持っている

.. data:: S_IXUSR

   .. Owner has execute permission.

   オーナーが実行権限を持っている

.. data:: S_IRWXG

   .. Mask for group permissions.

   グループの権限に対するマスク

.. data:: S_IRGRP

   .. Group has read permission.

   グループがリード権限を持っている

.. data:: S_IWGRP

   .. Group has write permission.

   グループがライト権限を持っている

.. data:: S_IXGRP

   .. Group has execute permission.

   グループが実行権限を持っている

.. data:: S_IRWXO

   .. Mask for permissions for others (not in group).

   その他 (グループ外) の権限に対するマスク

.. data:: S_IROTH

   .. Others have read permission.

   その他はリード権限を持っている

.. data:: S_IWOTH

   .. Others have write permission.

   その他はライト権限を持っている

.. data:: S_IXOTH

   .. Others have execute permission.

   その他は実行権限を持っている

.. data:: S_ENFMT

   .. System V file locking enforcement.  This flag is shared with :data:`S_ISGID`:
      file/record locking is enforced on files that do not have the group
      execution bit (:data:`S_IXGRP`) set.

   System V ファイルロック強制。このフラグは :data:`S_ISGID` と共有されています。
   グループ実行ビット (:data:`S_IXGRP`) が設定されていないファイルでは、
   ファイル/レコードのロックが強制されます。

.. data:: S_IREAD

   .. Unix V7 synonym for :data:`S_IRUSR`.

   :data:`S_IRUSR` の、 Unix V7 のシノニム

.. data:: S_IWRITE

   .. Unix V7 synonym for :data:`S_IWUSR`.

   :data:`S_IWUSR` の、 Unix V7 のシノニム

.. data:: S_IEXEC

   .. Unix V7 synonym for :data:`S_IXUSR`.

   :data:`S_IXUSR` の、 Unix V7 のシノニム

以下のフラグを :func:`os.chflags` の *flags* 引数として利用できます:

.. data:: UF_NODUMP

   ファイルをダンプしない

.. data:: UF_IMMUTABLE

   ファイルは変更されない

.. data:: UF_APPEND

   ファイルは追記しかされない。

.. data:: UF_OPAQUE

   ユニオンファイルシステムのスタックを通したとき、このディレクトリは不透明です。

.. data:: UF_NOUNLINK

   ファイルはリネームや削除されない。

.. data:: SF_ARCHIVED

   ファイルはアーカイブされているかもしれません。

.. data:: SF_IMMUTABLE

   ファイルは変更されないかもしれません。

.. data:: SF_APPEND

   ファイルは追記しかされないかもしれません。

.. data:: SF_NOUNLINK

   ファイルはリネームされたり削除されたりしないかもしれません。

.. data:: SF_SNAPSHOT

   このファイルはスナップショットファイルです。

詳しい情報は \*BSD か Mac OS システムの man page :manpage:`chflags(2)` を参照してください。
