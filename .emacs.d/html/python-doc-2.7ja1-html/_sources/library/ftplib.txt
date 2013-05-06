:mod:`ftplib` --- FTPプロトコルクライアント
===========================================

.. module:: ftplib
   :synopsis: FTPプロトコルクライアント(ソケットを必要とします)。


.. index::
   pair: FTP; protocol
   single: FTP; ftplib (standard module)

このモジュールでは :class:`FTP` クラスと、それに関連するいくつかの項目を定義しています。
:class:`FTP` クラスは、FTPプロトコルのクライアント側の機能を備えています。
このクラスを使うとFTPのいろいろな機能の自動化、例えば他のFTPサーバのミラーリングといったことを実行するPythonプログラムを書くことができます。
また、 :mod:`urllib` モジュールもFTPを使うURLを操作するのにこのクラスを使っています。 FTP (File Transfer
Protocol)についての詳しい情報はInternet :rfc:`959` を参照して下さい。

:mod:`ftplib` モジュールを使ったサンプルを以下に示します::

   >>> from ftplib import FTP
   >>> ftp = FTP('ftp.cwi.nl')   # ホストのデフォルトポートへ接続
   >>> ftp.login()               # ユーザ名 anonymous、パスワード anonyumou
   s@
   >>> ftp.retrlines('LIST')     # ディレクトリの内容をリストアップ
   total 24418
   drwxrwsr-x   5 ftp-usr  pdmaint     1536 Mar 20 09:48 .
   dr-xr-srwt 105 ftp-usr  pdmaint     1536 Mar 21 14:32 ..
   -rw-r--r--   1 ftp-usr  pdmaint     5305 Mar 20 09:48 INDEX
    .
    .
    .
   >>> ftp.retrbinary('RETR README', open('README', 'wb').write)
   '226 Transfer complete.'
   >>> ftp.quit()


このモジュールは以下の項目を定義しています。

.. class:: FTP([host[, user[, passwd[, acct[, timeout]]]]])

   :class:`FTP` クラスの新しいインスタンスを返します。 *host* が与えられると、 ``connect(host)`` メソッドが実行されます。
   *user* が与えられると、さらに ``login(user, passwd, acct)`` メソッドが実行されます（この *passwd* と *acct* は指定され
   なければデフォルトでは空文字列です）。

   オプションの *timeout* 引数は、コネクションの接続時など、ブロックする操作におけるタイムアウト時間を秒数で指定します。
   (指定されなかった場合、グローバルのデフォルトタイムアウト設定が利用されます。)

   .. versionchanged:: 2.6
      *timeout* が追加されました。


.. class:: FTP_TLS([host[, user[, passwd[, acct[, keyfile[, certfile[, timeout]]]]]]])

   :rfc:`4217` に記述されている TLS サポートを FTP に加えた :class:`FTP` の
   サブクラスです。認証の前に FTP コントロール接続を暗示的にセキュアにし、
   通常通りに port 21 に接続します。データ接続をセキュアにするには、ユーザが
   :meth:`prot_p` メソッドを呼び出してそれを明示的に要求しなければなりません。
   *keyfile* と *certfile* は省略可できます -- これらは、SSL 接続のための、
   PEM フォーマットの秘密鍵と証明書チェーンファイル名を含むことができます。

   .. versionadded:: 2.7

   :class:`FTP_TLS` クラスを使ったサンプルセッションはこちらです:

   >>> from ftplib import FTP_TLS
   >>> ftps = FTP_TLS('ftp.python.org')
   >>> ftps.login()           # 制御チャネルをセキュアにする前に匿名でログインする
   >>> ftps.prot_p()          # セキュアなデータ接続に移行する
   >>> ftps.retrlines('LIST') # ディレクトリの内容をセキュアに列挙する
   total 9
   drwxr-xr-x   8 root     wheel        1024 Jan  3  1994 .
   drwxr-xr-x   8 root     wheel        1024 Jan  3  1994 ..
   drwxr-xr-x   2 root     wheel        1024 Jan  3  1994 bin
   drwxr-xr-x   2 root     wheel        1024 Jan  3  1994 etc
   d-wxrwxr-x   2 ftp      wheel        1024 Sep  5 13:43 incoming
   drwxr-xr-x   2 root     wheel        1024 Nov 17  1993 lib
   drwxr-xr-x   6 1094     wheel        1024 Sep 13 19:07 pub
   drwxr-xr-x   3 root     wheel        1024 Jan  3  1994 usr
   -rw-r--r--   1 root     root          312 Aug  1  1994 welcome.msg
   '226 Transfer complete.'
   >>> ftps.quit()
   >>>


.. exception:: error_reply

   サーバから想定外の応答があった時に発生する例外。


.. exception:: error_temp

   一時的エラーを表すエラーコード(400--499の範囲の応答コード)を
   受け取った時に発生する例外。


.. exception:: error_perm

   永久エラーを表すエラーコード(500--599の範囲の応答コード)を
   受け取った時に発生する例外。


.. exception:: error_proto

   File Transfer Protocol の応答仕様に適合しない、すなわち1--5の数字で
   始まらない応答コードをサーバから受け取った時に発生する例外。


.. data:: all_errors

   :class:`FTP` インスタンスのメソッド実行時、FTP接続で（プログラミングの
   エラーと考えられるメソッドの実行によって）発生する全ての例外（タプル形式）。
   この例外には以上の４つのエラーはもちろん、 :exc:`socket.error` と
   :exc:`IOError` も含まれます。


.. seealso::

   Module :mod:`netrc`
      :file:`.netrc` ファイルフォーマットのパーザ。 :file:`.netrc` ファイルは、
      FTPクライアントがユーザにプロンプトを出す前に、ユーザ認証情報を
      ロードするのによく使われます。

   .. index:: single: ftpmirror.py

   Pythonのソースディストリビューションの :file:`Tools/scripts/ftpmi rror.py` ファイルは、FTPサイトあるいはその一部をミ
   ラーリングするスクリプトで、 :mod:`ftplib` モジュールを使っています。このモジュールを適用した応用例として使うことができます。


.. _ftp-objects:

FTP オブジェクト
----------------

いくつかのコマンドは２つのタイプについて実行します：１つはテキストファイルで、もう１つはバイナリファイルを扱います。
これらのメソッドのテキストバージョンでは ``lines`` 、バイナリバージョンでは ``binary`` の語がメソッド名の終わりについています。

:class:`FTP` インスタンスには以下のメソッドがあります：


.. method:: FTP.set_debuglevel(level)

   インスタンスのデバッグレベルを設定します。この設定によってデバッグ時に出力される量を調節します。デフォルトは ``0`` で、何も出力されません。
   ``1`` なら、一般的に１つのコマンドあたり１行の適当な量のデバッグ出力を行います。
   ``2`` 以上なら、コントロール接続で受信した各行を出力して、最大のデバッグ出力をします。


.. method:: FTP.connect(host[, port[, timeout]])

   指定されたホストとポートに接続します。ポート番号のデフォルト値はFTPプロトコルの仕様で定められた ``21`` です。
   他のポート番号を指定する必要はめったにありません。この関数はひとつのインスタンスに対して一度だけ実行すべきです；
   インスタンスが作られた時にホスト名が与えられていたら、呼び出すべきではありません。これ以外の他の全てのメソッドは接続された後で実行可能となります。

   The optional *timeout* parameter specifies a timeout in seconds for the
   connection attempt. If no *timeout* is passed, the global default timeout
   setting will be used.

   オプションの *timeout* 引数は、コネクションの接続におけるタイムアウト時間を秒数で指定します。
   *timeout* が渡されなかった場合、グローバルのデフォルトタイムアウト設定が利用されます。

   .. versionchanged:: 2.6
      *timeout* が追加されました

.. method:: FTP.getwelcome()

   接続して最初にサーバから送られてくるウェルカムメッセージを返します。（このメッセージには、ユーザにとって適切な注意書きやヘルプ情報が含まれる
   ことがあります。）


.. method:: FTP.login([user[, passwd[, acct]]])

   与えられた *user* でログインします。 *passwd* と *acct* のパラメータは
   省略可能で、デフォルトでは空文字列です。
   もし *user* が指定されないなら、デフォルトで ``'anonymous'`` になります。
   もし *user* が ``'anonymous'`` なら、デフォルトの *passwd* は
   ``'anonymous@'`` になります。
   この関数は各インスタンスについて一度だけ、接続が確立した後に呼び出さなければ
   なりません。
   インスタンスが作られた時にホスト名とユーザ名が与えられていたら、この
   メソッドを実行すべきではありません。
   ほとんどのFTPコマンドはクライアントがログインした後に実行可能になります。
   *acct* 引数は "accounting information" を提供します。ほとんどのシステムは
   これを実装していません。


.. method:: FTP.abort()

   実行中のファイル転送を中止します。これはいつも機能するわけではありませんが、やってみる価値はあります。


.. method:: FTP.sendcmd(command)

   シンプルなコマンド文字列をサーバに送信して、受信した文字列を返します。


.. method:: FTP.voidcmd(command)

   シンプルなコマンド文字列をサーバに送信して、その応答を扱います。応答コードが
   成功に関係するもの(200--299の範囲にあるコード)なら何も返しません。
   それ以外は :exc:`error_reply` を発生します。


.. method:: FTP.retrbinary(command, callback[, maxblocksize[, rest]])

   バイナリ転送モードでファイルを受信します。 *command* は適切な ``RETR`` コマンド： ``'RETR filename'`` でなければなりません。
   関数 *callback* は、受信したデータブロックのそれぞれに対して、データブロックを１つの文字列の引数として呼び出されます。
   省略可能な引数 *maxblocksize* は、実際の転送を行うのに作られた低レベルのソケットオブジェクトから読み込む最大のチャンクサイズを指定します（これ
   は *callback* に与えられるデータブロックの最大サイズにもなります）。妥当なデフォルト値が設定されます。
   *rest* は、 :meth:`transfercmd` メソッドと同じものです。


.. method:: FTP.retrlines(command[, callback])

   ASCII転送モードでファイルとディレクトリのリストを受信します。
   *command* は、適切な ``RETR`` コマンド(:meth:`retrbinary` を参照)あるいは
   ``LIST``, ``NLST``, ``MLSD`` のようなコマンド(通常は文字列 ``'LIST'``)で
   なければなりません。
   ``LIST`` は、ファイルのリストとそれらのファイルに関する情報を受信します。
   ``NLST`` は、ファイル名のリストを受信します。サーバによっては、 ``MLSD`` は
   機械で読めるリストとそれらのファイルに関する情報を受信します。
   関数 *callback* は末尾のCRLFを取り除いた各行を引数にして実行されます。
   デフォルトでは *callback* は ``sys.stdout`` に各行を表示します。


.. method:: FTP.set_pasv(boolean)

   *boolean* がtrueなら"パッシブモード"をオンにし、そうでないならパッシブモードをオフにします。（Python
   2.0以前ではデフォルトでパッシブモードはオフにされていましたが、 Python 2.1以後ではデフォルトでオンになっています。）


.. method:: FTP.storbinary(command, file[, blocksize, callback, rest])

   バイナリ転送モードでファイルを転送します。 *command* は適切な ``STOR`` コマンド： ``"STOR filename"`` でなければなりません。
   *file* は開かれたファイルオブジェクトで、 :meth:`read` メソッドで EOFまで読み込まれ、ブロックサイズ *blocksize* でデータが転送されま
   す。引数 *blocksize* のデフォルト値は8192です。
   *callback* はオプションの引数で、引数を1つとる呼び出し可能オブジェクトを渡します。
   各データブロックが送信された後に、そのブロックを引数にして呼び出されます。
   *rest* は、 :meth:`transfercmd` メソッドにあるものと同じ意味です。

   .. versionchanged:: 2.1
      *blocksize* のデフォルト値が追加されました.

   .. versionchanged:: 2.6
      *callback* 引数が追加されました。

   .. versionchanged:: 2.7
      *rest* パラメタが追加されました。

.. method:: FTP.storlines(command, file[, callback])

   ASCII転送モードでファイルを転送します。
   *command* は適切な ``STOR`` コマンドでなければなりません
   (:meth:`storbinary` を参照)。
   *file* は開かれたファイルオブジェクトで、 :meth:`readline` メソッド
   でEOFまで読み込まれ、各行がデータが転送されます。
   *callback* はオプションの引数で、引数を1つとる呼び出し可能オブジェクトを渡します。
   各行が送信された後に、その行数を引数にして呼び出されます。

   .. versionchanged:: 2.6
      *callback* 引数が追加されました。

.. method:: FTP.transfercmd(cmd[, rest])

   データ接続中に転送を初期化します。もし転送中なら、 ``EPRT`` あるいは ``PORT`` コマンドと、 *cmd* で指定したコマンドを送信し、接続を続けます。
   サーバがパッシブなら、 ``EPSV`` あるいは ``PASV`` コマンドを送信して接続し、転送コマンドを開始します。
   どちらの場合も、接続のためのソケットを返します。

   省略可能な *rest* が与えられたら、 ``REST`` コマンドがサーバに送信され、 *rest* を引数として与えます。
   *rest* は普通、要求したファイルのバイトオフセット値で、最初のバイトをとばして指定したオフセット値からファイルのバイト転送を再開するよう伝えます。
   しかし、RFC 959では *rest* が印字可能なASCIIコード33から126の範囲の文字列からなることを要求していることに注意して下さい。
   したがって、 :meth:`transfercmd` メソッドは *rest* を文字列に変換しますが、文字列の内容についてチェックしません。
   もし ``REST`` コマンドをサーバが認識しないなら、例外 :exc:`error_re ply` が発生します。
   この例外が発生したら、引数 *rest* なしに :meth:`transfercmd` を実行します。


.. method:: FTP.ntransfercmd(cmd[, rest])

   :meth:`transfercmd` と同様ですが、データと予想されるサイズとのタプルを返します。
   もしサイズが計算できないなら、サイズの代わりに ``None`` が返されます。 *cmd* と *rest* は :meth:`transfercmd` のものと同じです。


.. method:: FTP.nlst(argument[, ...])

   ``NLST`` コマンドで返されるファイル名のリストを返します。省略可能な *argument* は、リストアップするディレクトリです（デフォルト
   ではサーバのカレントディレクトリです）。 ``NLST`` コマンドに非標準である複数の引数を渡すことができます。


.. method:: FTP.dir(argument[, ...])

   ``LIST`` コマンドで返されるディレクトリ内のリストを作り、標準出力へ出力します。
   省略可能な *argument* は、リストアップするディレクトリです（デフォルトではサーバのカレントディレクトリです）。
   ``LIST`` コマンドに非標準である複数の引数を渡すことができます。
   もし最後の引数が関数なら、 :meth:`retrlines` のように *callback* とし
   て使われます；デフォルトでは ``sys.stdout`` に印字します。このメソッドは ``None`` を返します。


.. method:: FTP.rename(fromname, toname)

   サーバ上のファイルのファイル名 *fromname* を *toname* へ変更します。


.. method:: FTP.delete(filename)

   サーバからファイル *filename* を削除します。成功したら応答のテキストを返し、そうでないならパーミッションエラーでは
   :exc:`error_perm` を、他のエラーでは :exc:`error_reply` を返します。


.. method:: FTP.cwd(pathname)

   サーバのカレントディレクトリを設定します。


.. method:: FTP.mkd(pathname)

   サーバ上に新たにディレクトリを作ります。


.. method:: FTP.pwd()

   サーバ上のカレントディレクトリのパスを返します。


.. method:: FTP.rmd(dirname)

   サーバ上のディレクトリ *dirname* を削除します。


.. method:: FTP.size(filename)

   サーバ上のファイル *filename* のサイズを尋ねます。成功したらファイルサイズが整数で返され、そうでないなら ``None`` が返されます。
   ``SIZE`` コマンドは標準化されていませんが、多くの普通のサーバで実装されていることに注意して下さい。


.. method:: FTP.quit()

   サーバに ``QUIT`` コマンドを送信し、接続を閉じます。これは接続を閉じるのに"礼儀正しい"方法ですが、 ``QUIT`` コマンドに反
   応してサーバの例外が発生するかもしれません。この例外は、 :meth:`close` メソッドによって :class:`FTP` インスタンスに対
   するその後のコマンド使用が不可になっていることを示しています（下記参照）。


.. method:: FTP.close()

   接続を一方的に閉じます。既に閉じた接続に対して実行すべきではありません（例えば :meth:`quit` を呼び出して成功した後など）。
   この実行の後、 :class:`FTP` インスタンスはもう使用すべきではありません（ :meth:`close` あるいは :meth:`quit` を呼び出した後で、
   :meth:`login` メソッドをもう一度実行して再び接続を開くことはできません）。


FTP_TLS オブジェクト
--------------------

:class:`FTP_TLS` クラスは :class:`FTP` を継承し、さらにオブジェクトを
定義します。

.. attribute:: FTP_TLS.ssl_version

   使用する SSL のバージョン (デフォルトは *TLSv1*) です。

.. method:: FTP_TLS.auth()

   :meth:`ssl_version` 属性で指定されたものに従って、
   TLS または SSL を使い、セキュアコントロール接続をセットアップします。

.. method:: FTP_TLS.prot_p()

   セキュアデータ接続をセットアップします。

.. method:: FTP_TLS.prot_c()

   平文データ接続をセットアップします。


