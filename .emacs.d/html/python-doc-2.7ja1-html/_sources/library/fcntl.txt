
:mod:`fcntl` --- :func:`fcntl` および :func:`ioctl` システムコール
==================================================================

.. module:: fcntl
   :platform: Unix
   :synopsis: fcntl() および ioctl() システムコール。
.. sectionauthor:: Jaap Vermeulen


.. index::
   pair: UNIX; file control
   pair: UNIX; I/O control

このモジュールでは、ファイル記述子 (file descriptor) に基づいたファイル制御\
および I/O 制御を実現します。このモジュールは、 Unix
のルーチンである :c:func:`fcntl`  および :c:func:`ioctl` へのインタフェースです。

このモジュール内の全ての関数はファイル記述子 *fd* を最初の引数に取ります。
この値は ``sys.stdin.fileno()`` が返すような\
整数のファイル記述子でも、 ``sys.stdin`` 自体のような、純粋にファイル記述子だけを返す
:meth:`fileno` メソッドを提供しているファイルオブジェクトでもかまいません。

このモジュールでは以下の関数を定義しています:


.. function:: fcntl(fd, op[, arg])

   要求された操作をファイル記述子 *fd* (または :meth:`fileno`
   メソッドを提供しているファイルオブジェクト) に対して実行します。
   操作は *op* で定義され、オペレーティングシステム依存です。
   これらの操作コードは :mod:`fcntl` モジュール内にもあります。
   引数 *arg* はオプションで、標準では整数値 ``0`` です。
   この引数を与える場合、整数か文字列の値をとります。
   引数が無いか整数値の場合、この関数の戻り値は C 言語の
   :c:func:`fcntl` を呼び出した際の整数の戻り値になります。
   引数が文字列の場合には、 :func:`struct.pack` で作られる\
   ようなバイナリの構造体を表します。
   バイナリデータはバッファにコピーされ、そのアドレスが C 言語の
   :c:func:`fcntl` 呼び出しに渡されます。
   呼び出しが成功した後に戻される値はバッファの内容で、文字列\
   オブジェクトに変換されています。
   返される文字列は *arg* 引数と同じ長さになます。
   この値は 1024 バイトに制限されています。
   オペレーティングシステムからバッファに返される情報の長さが 1024
   バイトよりも大きい場合、大抵はセグメンテーション違反となるか、\
   より不可思議なデータの破損を引き起こします。

   :c:func:`fcntl` が失敗した場合、 :exc:`IOError` が送出されます。


.. function:: ioctl(fd, op, arg)

   この関数は :func:`fcntl` 関数と同じですが、操作が通常ライブラリモジュール
   :mod:`termios` で定義されており、引数の扱いがより複雑であるところが異なります。

   パラメタ op は32ビットに収まる値に制限されます。

   パラメタ *arg* は整数か、存在しない (整数 ``0`` と等価なものとして扱われます) か、\
   (通常の Python 文字列のような) 読み出し専用の\
   バッファインタフェースをサポートするオブジェクトか、\
   読み書きバッファインタフェースをサポートするオブジェクトです。

   最後の型のオブジェクトを除き、動作は :func:`fcntl` 関数と同じです。

   可変なバッファが渡された場合、動作は *mutate_flag* 引数の値で決定されます。

   この値が偽の場合、バッファの可変性は無視され、動作は読み出しバッファの場合と\
   同じになりますが、上で述べた 1024 バイトの制限は回避されます --
   従って、オペレーティングシステムが希望するバッファ長までであれば正しく動作します。

   *mutate_flag* が真の場合、バッファは (実際には) 根底にある :func:`ioctl`
   システムコールに渡され、後者の戻り値が呼び出し側の
   Python に引き渡され、バッファの新たな内容は  :func:`ioctl` の動作を反映します。
   この説明はやや単純化されています。
   というのは、与えられたバッファが 1024 バイト長よりも短い場合、バッファはまず
   1024 バイト長の静的なバッファにコピーされてから :func:`ioctl` に渡され、\
   その後引数で与えたバッファに戻しコピーされるからです。

   *mutate_flag* が与えられなかった場合、2.3 ではこの値は偽となります。
   この仕様は今後のいくつかのバージョンを経た Python で変更される予定\
   です: 2.4 では、 *mutate_flag* を提供し忘れると警告が出されますが\
   同じ動作を行い、2.5 ではデフォルトの値が真となるはずです。

   以下に例を示します::

      >>> import array, fcntl, struct, termios, os
      >>> os.getpgrp()
      13341
      >>> struct.unpack('h', fcntl.ioctl(0, termios.TIOCGPGRP, "  "))[0]
      13341
      >>> buf = array.array('h', [0])
      >>> fcntl.ioctl(0, termios.TIOCGPGRP, buf, 1)
      0
      >>> buf
      array('h', [13341])


.. function:: flock(fd, op)

   ファイル記述子 *fd* (:meth:`fileno` メソッドを提供しているファイルオブジェクトも含む) に対してロック操作 *op* を実行します。
   詳細は Unix マニュアルの :manpage:`flock(2)` を参照してください (システムによっては、この関数は :c:func:`fcntl`
   を使ってエミュレーションされています)。


.. function:: lockf(fd, operation, [length, [start, [whence]]])

   本質的に :func:`fcntl` によるロッキングの呼び出しをラップしたものです。
   *fd* はロックまたはアンロックするファイルのファイル記述子で、
   *operation* は以下の値のうちいずれかになります:

   * :const:`LOCK_UN` -- アンロック
   * :const:`LOCK_SH` -- 共有ロックを取得
   * :const:`LOCK_EX` -- 排他的ロックを取得

   *operation* が :const:`LOCK_SH` または :const:`LOCK_EX` の場合、
   :const:`LOCK_NB` とビット OR
   にすることでロック取得時にブロックしないようにすることができます。
   :const:`LOCK_NB` が
   使われ、ロックが取得できなかった場合、
   :exc:`IOError` が送出され、例外は *errno* 属性を持ち、その値は :const:`EACCESS`
   または :const:`EAGAIN` になります (オペレーティングシステムに依存します;
   可搬性のため、両方の値をチェックしてください)。
   少なくともいくつかのシステムでは、
   ファイル記述子が参照しているファイルが書き込みのために開かれている場合、
   :const:`LOCK_EX` だけしか使うことができません。

   *length* はロックを行いたいバイト数、
   *start* はロック領域先頭の *whence* からの相対的なバイトオフセット、
   *whence* は :func:`fileobj.seek` と同じで、具体的には:

   * :const:`0` -- ファイル先頭からの相対位置 (:const:`SEEK_SET`)
   * :const:`1` -- 現在のバッファ位置からの相対位置 (:const:`SEEK_CUR`)
   * :const:`2` -- ファイルの末尾からの相対位置 (:const:`SEEK_END`)

   *start* の標準の値は 0 で、ファイルの先頭から開始することを意味します。
   *whence* の標準の値も 0 です。

以下に (全ての SVR4 互換システムでの) 例を示します::

   import struct, fcntl, os

   f = open(...)
   rv = fcntl.fcntl(f, fcntl.F_SETFL, os.O_NDELAY)

   lockdata = struct.pack('hhllhh', fcntl.F_WRLCK, 0, 0, 0, 0, 0)
   rv = fcntl.fcntl(f, fcntl.F_SETLKW, lockdata)

最初の例では、戻り値 *rv* は整数値を保持しています; 二つ目の例では文字列値を保持しています。
*lockdata* 変数の構造体レイアウトはシステム依存です ---
従って :func:`flock` を呼ぶ方がベターです。


.. seealso::

   :mod:`os` モジュール
      もし :mod:`os` モジュールに :const:`O_SHLOCK` と :const:`O_EXLOCK` が 存在する場合
      (BSD のみ)、 :func:`os.open` 関数は :func:`lockf` や :func:`flock` 関数を代替できます。
