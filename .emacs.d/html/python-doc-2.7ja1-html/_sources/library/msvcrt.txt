:mod:`msvcrt` -- MS VC++実行時システムの有用なルーチン群
========================================================

.. module:: msvcrt
   :platform: Windows
   :synopsis: MS VC++実行時システムの雑多な有用ルーチン群。
.. sectionauthor:: Fred L. Drake, Jr. <fdrake@acm.org>


このモジュールの関数は、 Windows プラットフォームの便利な機能のいくつかに対するアクセス機構を提供しています。高レベルモジュール
のいくつかは、提供するサービスを Windows で実装するために、これらの関数を使っています。例えば、 :mod:`getpass` モジュールは関数
:func:`getpass` を実装するためにこのモジュールの関数を使います。

ここに挙げた関数の詳細なドキュメントについては、プラットフォーム API ドキュメントで見つけることができます。

このモジュールは、通常版とワイド文字列版の両方のコンソールI/O APIを実装しています。
通常版のAPIはASCII文字列のためのもので、国際化アプリケーションでは利用が制限されます。
可能な限りワイド文字列版APIを利用するべきです。

.. _msvcrt-files:

ファイル操作関連
----------------


.. function:: locking(fd, mode, nbytes)

   C 言語による実行時システムにおけるファイル記述子 *fd* に基づいて、ファイルの一部にロックをかけます。ロックされるファイルの領域は、
   現在のファイル位置から *nbytes* バイトで、ファイルの末端まで延長することができます。 *mode* は以下に列挙する  :const:`LK_\*`
   のいずれか一つでなければなりません。一つのファイルの複数の領域を同時にロックすることは可能ですが、
   領域が重複してはなりません。連接する領域をまとめて指定することはできません; それらの領域は個別にロック解除しなければなりません。


.. data:: LK_LOCK
          LK_RLCK

   指定されたバイト列にロックをかけます。指定領域がロックできなかった場合、プログラムは 1 秒後に再度ロックを試みます。10 回
   再試行した後でもロックをかけられない場合、 :exc:`IOError` が送出されます。


.. data:: LK_NBLCK
          LK_NBRLCK

   指定されたバイト列にロックをかけます。指定領域がロックできなかった場合、 :exc:`IOError` が送出されます。


.. data:: LK_UNLCK

   指定されたバイト列のロックを解除します。指定領域はあらかじめロックされていなければなりません。


.. function:: setmode(fd, flags)

   ファイル記述子 *fd* に対して、行末文字の変換モードを設定します。テキストモードに設定するには、 *flags* を :const:`os.O_TEXT`
   にします; バイナリモードにするには :const:`os.O_BINARY` にします。


.. function:: open_osfhandle(handle, flags)

   C 言語による実行時システムにおけるファイル記述子をファイルハンドル *handle* から生成します。 *flags* パラメタは
   :const:`os.O_APPEND` 、 :const:`os.O_RDONLY` 、および :const:`os.O_TEXT` をビット単位で OR
   したものになります。返されるファイル記述子は :func:`os.fdopen` でファイルオブジェクトを生成するために使うことができます。


.. function:: get_osfhandle(fd)

   ファイル記述子 *fd* のファイルハンドルを返します。 *fd* が認識できない場合、 *IOError* を送出します。


.. _msvcrt-console:

コンソール I/O 関連
-------------------


.. function:: kbhit()

   読み出し待ちの打鍵イベントが存在する場合に真を返します。


.. function:: getch()

   打鍵を読み取り、読み出された文字を返します。コンソールには何もエコーバックされません。この関数呼び出しは読み出し可能な
   打鍵がない場合にはブロックしますが、文字を読み出せるようにするために :kbd:`Enter` の打鍵を待つ必要はありません。打鍵されたキーが特殊機能キー
   (function key) である場合、この関数は ``'\000'`` または ``'\xe0'`` を返します;
   キーコードは次に関数を呼び出した際に返されます。この関数で :kbd:`Control-C` の打鍵を読み出すことはできません。


.. function:: getwch()

   :func:`getch` のワイド文字列版。Unicodeの値を返します。

   .. versionadded:: 2.6


.. function:: getche()

   :func:`getch` に似ていますが、打鍵した字が印字可能な文字の場合エコーバックされます。

.. function:: getwche()

   :func:`getche` のワイド文字列版。Unicodeの値を返します。

   .. versionadded:: 2.6


.. function:: putch(char)

   キャラクタ *char* をバッファリングを行わないでコンソールに出力します。


.. function:: putwch(unicode_char)

   :func:`putch` のワイド文字列版。Unicodeの値を引数に取ります。

   .. versionadded:: 2.6


.. function:: ungetch(char)

   キャラクタ *char* をコンソールバッファに  "押し戻し (push back)" ます; これにより、押し戻された文字は :func:`getch`
   や :func:`getche` で次に読み出される文字になります。

.. function:: ungetwch(unicode_char)

   :func:`ungetch` のワイド文字列版。Unicodeの値を引数に取ります。

   .. versionadded:: 2.6


.. _msvcrt-other:

その多の関数
------------


.. function:: heapmin()

   :c:func:`malloc` されたヒープ領域を強制的に消去させて、
   未使用のメモリブロックをオペレーティングシステムに返します。
   失敗した場合、 :exc:`IOError` を送出します。
