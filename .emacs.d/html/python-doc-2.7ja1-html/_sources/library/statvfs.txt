:mod:`statvfs` --- :func:`os.statvfs` で使われる定数群
======================================================

.. module:: statvfs
   :synopsis: os.statvfs() の返す値を解釈するために使われる定数群。
   :deprecated:

.. deprecated:: 2.6
   :mod:`statvfs` モジュールは Python 3.0 で削除されます。

.. sectionauthor:: Moshe Zadka <moshez@zadka.site.co.il>

:mod:`statvfs` モジュールでは、 :func:`os.statvfs` の返す値
を解釈するための定数を定義しています。 :func:`os.statvfs`  は "マジックナンバ" を記憶せずにタプルを生成して返します。
このモジュールで定義されている各定数は :func:`os.statvfs` が返すタプルにおいて、特定の情報が収められている各エントリへの  *インデクス*
です。


.. data:: F_BSIZE

   選択されているファイルシステムのブロックサイズです。


.. data:: F_FRSIZE

   ファイルシステムの基本ブロックサイズです。


.. data:: F_BLOCKS

   ブロック数の総計です。


.. data:: F_BFREE

   空きブロック数の総計です。


.. data:: F_BAVAIL

   非スーパユーザが利用できる空きブロック数です。


.. data:: F_FILES

   ファイルノード数の総計です。


.. data:: F_FFREE

   空きファイルノード数の総計です。


.. data:: F_FAVAIL

   非スーパユーザが利用できる空きノード数です。


.. data:: F_FLAG

   フラグで、システム依存です: :c:func:`statvfs` マニュアルページを参照してください。


.. data:: F_NAMEMAX

   ファイル名の最大長です。

