
:mod:`errno` --- 標準の errno システムシンボル
==============================================

.. module:: errno
   :synopsis: 標準の errno システムシンボル。


このモジュールから標準の ``errno`` システムシンボルを取得することができます。個々のシンボルの値は ``errno`` に対応する整数値です。
これらのシンボルの名前は、 :file:`linux/include/errno.h` から借用されており、かなり網羅的なはずです。


.. data:: errorcode

   errno 値を背後のシステムにおける文字列表現に対応付ける辞書です。例えば、 ``errno.errorcode[errno.EPERM]`` は
   ``'EPERM'`` に対応付けられます。

数値のエラーコードをエラーメッセージに変換するには、 :func:`os.strerror` を使ってください。

以下のリストの内、現在のプラットフォームで使われていないシンボルはモジュール上で定義されていません。定義されているシンボルだけを挙げたリストは
``errno.errorcode.keys()`` として取得することができます。取得できるシンボルには以下のようなものがあります:


.. data:: EPERM

   許可されていない操作です (Operation not permitted)


.. data:: ENOENT

   ファイルまたはディレクトリがありません (No such file or directory)


.. data:: ESRCH

   指定したプロセスが存在しません (No such process)


.. data:: EINTR

   割り込みシステムコールです (Interrupted system call)


.. data:: EIO

   I/O エラーです (I/O error)


.. data:: ENXIO

   そのようなデバイスまたはアドレスはありません (No such device or address)


.. data:: E2BIG

   引数リストが長すぎます (Arg list too long)


.. data:: ENOEXEC

   実行形式にエラーがあります (Exec format error)


.. data:: EBADF

   ファイル番号が間違っています (Bad file number)


.. data:: ECHILD

   子プロセスがありません (No child processes)


.. data:: EAGAIN

   再試行してください (Try again)


.. data:: ENOMEM

   空きメモリがありません (Out of memory)


.. data:: EACCES

   許可がありません (Permission denied)


.. data:: EFAULT

   不正なアドレスです (Bad address)


.. data:: ENOTBLK

   ブロックデバイスが必要です (Block device required)


.. data:: EBUSY

   そのデバイスまたは資源は使用中です (Device or resource busy)


.. data:: EEXIST

   ファイルがすでに存在します   (File exists)


.. data:: EXDEV

   デバイス間のリンクです (Cross-device link)


.. data:: ENODEV

   そのようなデバイスはありません (No such device)


.. data:: ENOTDIR

   ディレクトリではありません (Not a directory)


.. data:: EISDIR

   ディレクトリです (Is a directory)


.. data:: EINVAL

   無効な引数です (Invalid argument)


.. data:: ENFILE

   ファイルテーブルがオーバフローしています (File table overflow)


.. data:: EMFILE

   開かれたファイルが多すぎます (Too many open files)


.. data:: ENOTTY

   タイプライタではありません (Not a typewriter)


.. data:: ETXTBSY

   テキストファイルが使用中です (Text file busy)


.. data:: EFBIG

   ファイルが大きすぎます (File too large)


.. data:: ENOSPC

   デバイス上に空きがありません (No space left on device)


.. data:: ESPIPE

   不正なシークです (Illegal seek)


.. data:: EROFS

   読み出し専用ファイルシステムです (Read-only file system)


.. data:: EMLINK

   リンクが多すぎます (Too many links)


.. data:: EPIPE

   パイプが壊れました (Broken pipe)


.. data:: EDOM

   数学引数が関数の定義域を越えています (Math argument out of domain of func)


.. data:: ERANGE

   表現できない数学演算結果になりました (Math result not representable)


.. data:: EDEADLK

   リソースのデッドロックが起きます (Resource deadlock would occur)


.. data:: ENAMETOOLONG

   ファイル名が長すぎます (File name too long)


.. data:: ENOLCK

   レコードロッキングが利用できません (No record locks available)


.. data:: ENOSYS

   実装されていない機能です (Function not implemented)


.. data:: ENOTEMPTY

   ディレクトリが空ではありません (Directory not empty)


.. data:: ELOOP

   これ以上シンボリックリンクを追跡できません (Too many symbolic links encountered)


.. data:: EWOULDBLOCK

   操作がブロックします (Operation would block)


.. data:: ENOMSG

   指定された型のメッセージはありません (No message of desired type)


.. data:: EIDRM

   識別子が除去されました (Identifier removed)


.. data:: ECHRNG

   チャネル番号が範囲を超えました (Channel number out of range)


.. data:: EL2NSYNC

   レベル 2 で同期がとれていません (Level 2 not synchronized)


.. data:: EL3HLT

   レベル 3 で終了しました (Level 3 halted)


.. data:: EL3RST

   レベル 3 でリセットしました (Level 3 reset)


.. data:: ELNRNG

   リンク番号が範囲を超えています (Link number out of range)


.. data:: EUNATCH

   プロトコルドライバが接続されていません (Protocol driver not attached)


.. data:: ENOCSI

   CSI 構造体がありません (No CSI structure available)


.. data:: EL2HLT

   レベル 2 で終了しました (Level 2 halted)


.. data:: EBADE

   無効な変換です (Invalid exchange)


.. data:: EBADR

   無効な要求記述子です (Invalid request descriptor)


.. data:: EXFULL

   変換テーブルが一杯です (Exchange full)


.. data:: ENOANO

   陰極がありません (No anode)


.. data:: EBADRQC

   無効なリクエストコードです (Invalid request code)


.. data:: EBADSLT

   無効なスロットです (Invalid slot)


.. data:: EDEADLOCK

   ファイルロックにおけるデッドロックエラーです (File locking deadlock error)


.. data:: EBFONT

   フォントファイル形式が間違っています (Bad font file format)


.. data:: ENOSTR

   ストリーム型でないデバイスです (Device not a stream)


.. data:: ENODATA

   利用可能なデータがありません (No data available)


.. data:: ETIME

   時間切れです (Timer expired)


.. data:: ENOSR

   streams リソースを使い切りました (Out of streams resources)


.. data:: ENONET

   計算機はネットワーク上にありません (Machine is not on the network)


.. data:: ENOPKG

   パッケージがインストールされていません (Package not installed)


.. data:: EREMOTE

   対象物は遠隔にあります (Object is remote)


.. data:: ENOLINK

   リンクが切られました (Link has been severed)


.. data:: EADV

   Advertise エラーです (Advertise error)


.. data:: ESRMNT

   Srmount エラーです (Srmount error)


.. data:: ECOMM

   送信時の通信エラーです (Communication error on send)


.. data:: EPROTO

   プロトコルエラーです (Protocol error)


.. data:: EMULTIHOP

   多重ホップを試みました (Multihop attempted)


.. data:: EDOTDOT

   RFS 特有のエラーです (RFS specific error)


.. data:: EBADMSG

   データメッセージではありません (Not a data message)


.. data:: EOVERFLOW

   定義されたデータ型にとって大きすぎる値です (Value too large for defined data type)


.. data:: ENOTUNIQ

   名前がネットワーク上で一意でありません (Name not unique on network)


.. data:: EBADFD

   ファイル記述子の状態が不正です (File descriptor in bad state)


.. data:: EREMCHG

   遠隔のアドレスが変更されました (Remote address changed)


.. data:: ELIBACC

   必要な共有ライブラリにアクセスできません (Can not access a needed shared library)


.. data:: ELIBBAD

   壊れた共有ライブラリにアクセスしています (Accessing a corrupted shared library)


.. data:: ELIBSCN

   a.out の .lib セクションが壊れています (.lib section in a.out corrupted)


.. data:: ELIBMAX

   リンクを試みる共有ライブラリが多すぎます (Attempting to link in too many shared libraries)


.. data:: ELIBEXEC

   共有ライブラリを直接実行することができません (Cannot exec a shared library directly)


.. data:: EILSEQ

   不正なバイト列です (Illegal byte sequence)


.. data:: ERESTART

   割り込みシステムコールを復帰しなければなりません (Interrupted system call should be restarted)


.. data:: ESTRPIPE

   ストリームパイプのエラーです (Streams pipe error)


.. data:: EUSERS

   ユーザが多すぎます (Too many users)


.. data:: ENOTSOCK

   非ソケットに対するソケット操作です (Socket operation on non-socket)


.. data:: EDESTADDRREQ

   目的アドレスが必要です (Destination address required)


.. data:: EMSGSIZE

   メッセージが長すぎます (Message too long)


.. data:: EPROTOTYPE

   ソケットに対して不正なプロトコル型です (Protocol wrong type for socket)


.. data:: ENOPROTOOPT

   利用できないプロトコルです (Protocol not available)


.. data:: EPROTONOSUPPORT

   サポートされていないプロトコルです (Protocol not supported)


.. data:: ESOCKTNOSUPPORT

   サポートされていないソケット型です (Socket type not supported)


.. data:: EOPNOTSUPP

   通信端点に対してサポートされていない操作です (Operation not supported on transport endpoint)


.. data:: EPFNOSUPPORT

   サポートされていないプロトコルファミリです (Protocol family not supported)


.. data:: EAFNOSUPPORT

   プロトコルでサポートされていないアドレスファミリです (Address family not supported by protocol)


.. data:: EADDRINUSE

   アドレスは使用中です (Address already in use)


.. data:: EADDRNOTAVAIL

   要求されたアドレスを割り当てできません (Cannot assign requested address)


.. data:: ENETDOWN

   ネットワークがダウンしています (Network is down)


.. data:: ENETUNREACH

   ネットワークに到達できません (Network is unreachable)


.. data:: ENETRESET

   リセットによってネットワーク接続が切られました (Network dropped connection because of reset)


.. data:: ECONNABORTED

   ソフトウェアによって接続が終了されました (Software caused connection abort)


.. data:: ECONNRESET

   接続がピアによってリセットされました (Connection reset by peer)


.. data:: ENOBUFS

   バッファに空きがありません (No buffer space available)


.. data:: EISCONN

   通信端点がすでに接続されています (Transport endpoint is already connected)


.. data:: ENOTCONN

   通信端点が接続されていません (Transport endpoint is not connected)


.. data:: ESHUTDOWN

   通信端点のシャットダウン後は送信できません (Cannot send after transport endpoint shutdown)


.. data:: ETOOMANYREFS

   参照が多すぎます: 接続できません (Too many references: cannot splice)


.. data:: ETIMEDOUT

   接続がタイムアウトしました (Connection timed out)


.. data:: ECONNREFUSED

   接続を拒否されました (Connection refused)


.. data:: EHOSTDOWN

   ホストはシステムダウンしています (Host is down)


.. data:: EHOSTUNREACH

   ホストへの経路がありません (No route to host)


.. data:: EALREADY

   すでに処理中です (Operation already in progress)


.. data:: EINPROGRESS

   現在処理中です (Operation now in progress)


.. data:: ESTALE

   無効な NFS ファイルハンドルです (Stale NFS file handle)


.. data:: EUCLEAN

   (Structure needs cleaning)


.. data:: ENOTNAM

   XENIX 名前付きファイルではありません (Not a XENIX named type file)


.. data:: ENAVAIL

   XENIX セマフォは利用できません (No XENIX semaphores available)


.. data:: EISNAM

   名前付きファイルです (Is a named type file)


.. data:: EREMOTEIO

   遠隔側の I/O エラーです (Remote I/O error)


.. data:: EDQUOT

   ディスククオータを超えました (Quota exceeded)

