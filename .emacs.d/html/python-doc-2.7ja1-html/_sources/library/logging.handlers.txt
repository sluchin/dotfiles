:mod:`logging.handlers` --- ロギングハンドラ
============================================

.. module:: logging.handlers
   :synopsis: logging モジュールのためのハンドラ


.. moduleauthor:: Vinay Sajip <vinay_sajip@red-dove.com>
.. sectionauthor:: Vinay Sajip <vinay_sajip@red-dove.com>

.. sidebar:: Important

   このページには、リファレンス情報だけが含まれています。
   チュートリアルは、以下のページを参照してください

   * :ref:`基本チュートリアル <logging-basic-tutorial>`
   * :ref:`上級チュートリアル <logging-advanced-tutorial>`
   * :ref:`ロギングクックブック <logging-cookbook>`

.. currentmodule:: logging

このパッケージでは、以下の便利なハンドラが提供されています。なお、これらの
ハンドラのうち、3 つ (:class:`StreamHandler`, :class:`FileHandler` および
:class:`NullHandler`) は、実際には :mod:`logging` モジュール自身で
定義されていますが、他のハンドラと一緒にここでドキュメント化します。

.. _stream-handler:

StreamHandler
^^^^^^^^^^^^^

:mod:`logging` コアパッケージに含まれる :class:`StreamHandler` クラスは、
ログ出力を *sys.stdout*, *sys.stderr* あるいは何らかのファイル風 (file-like) オブジェクト
(あるいは、より正確に言えば :meth:`write` および :meth:`flush` メソッドをサポートする何らかのオブジェクト)
といったストリームに送信します。


.. class:: StreamHandler(stream=None)

   :class:`StreamHandler` クラスの新たなインスタンスを返します。
   *stream* が指定された場合、インスタンスはログ出力先として指定されたストリームを使います;
   そうでない場合、 *sys.stderr* が使われます。


   .. method:: emit(record)

      フォーマッタが指定されていれば、フォーマッタを使ってレコードを書式化します。
      次に、レコードがストリームに書き込まれ、末端に改行がつけられます。
      例外情報が存在する場合、 :func:`traceback.print_exception` を使って書式化され、
      ストリームの末尾につけられます。


   .. method:: flush()

      ストリームの :meth:`flush` メソッドを呼び出してバッファをフラッシュします。
      :meth:`close` メソッドは :class:`Handler` から継承しているため何も出力を行わないので、
      :meth:`flush` 呼び出しを明示的に行う必要があるかもしれません。

.. _file-handler:

FileHandler
^^^^^^^^^^^

:mod:`logging` コアパッケージに含まれる :class:`FileHandler` クラスは、
ログ出力をディスク上のファイルに送信します。
このクラスは出力機能を :class:`StreamHandler` から継承しています。


.. class:: FileHandler(filename, mode='a', encoding=None, delay=False)

   :class:`FileHandler` クラスの新たなインスタンスを返します。
   指定されたファイルが開かれ、ログ記録のためのストリームとして使われます。
   *mode* が指定されなかった場合、 :const:`'a'` が使われます。
   *encoding* が *None* でない場合、その値はファイルを開くときのエンコーディングとして使われます。
   *delay* が true ならば、ファイルを開くのは最初の :meth:`emit` 呼び出しまで遅らせられます。
   デフォルトでは、ファイルは無制限に大きくなりつづけます。

   .. versionchanged:: 2.6
      *delay* が追加されました。

   .. method:: close()

      ファイルを閉じます。


   .. method:: emit(record)

      *record* をファイルに出力します。


.. _null-handler:

NullHandler
^^^^^^^^^^^

.. versionadded:: 2.7

:mod:`logging` コアパッケージに含まれる :class:`NullHandler` クラスは、
いかなるフォーマット化も出力も行いません。これは本質的には、ライブラリ開発者に
使われる 'no-op' ハンドラです。

.. class:: NullHandler()

   :class:`NullHandler` クラスの新しいインスタンスを返します。

   .. method:: emit(record)

      このメソッドは何もしません。

   .. method:: handle(record)

      このメソッドは何もしません。

   .. method:: createLock()

      アクセスが特殊化される必要がある I/O が下にないので、
      このメソッドはロックに対して ``None`` を返します。


:class:`NullHandler` の使い方の詳しい情報は、 :ref:`library-config` を
参照してください。

.. _watched-file-handler:

WatchedFileHandler
^^^^^^^^^^^^^^^^^^

.. currentmodule:: logging.handlers

.. versionadded:: 2.6

:mod:`logging.handlers` モジュールに含まれる :class:`WatchedFileHandler` クラスは、
ログ記録先のファイルを監視する :class:`FileHandler` の一種です。
ファイルが変更された場合、ファイルを閉じてからファイル名を使って開き直します。

ファイルはログファイルをローテーションさせる *newsyslog* や
*logrotate* のようなプログラムを使うことで変更されることがあります。
このハンドラは、 Unix/Linux で使われることを意図していますが、
ファイルが最後にログを出力してから変わったかどうかを監視します。
(ファイルはデバイスや inode が変わることで変わったと判断します。)
ファイルが変わったら古いファイルのストリームは閉じて、
現在のファイルを新しいストリームを取得するために開きます。

このハンドラを Windows で使うことは適切ではありません。
というのも Windows では開いているログファイルを移動したり削除したりできないからです -
logging はファイルを排他的ロックを掛けて開きます - そのためこうしたハンドラは必要ないのです。
さらに、 Windows では *ST_INO* がサポートされていません
(:func:`stat` はこの値として常に 0 を返します)。


.. class:: WatchedFileHandler(filename[,mode[, encoding[, delay]]])

   :class:`WatchedFileHandler` クラスの新たなインスタンスを返します。
   指定されたファイルが開かれ、ログ記録のためのストリームとして使われます。
   *mode* が指定されなかった場合、 :const:`'a'` が使われます。
   *encoding* が *None* でない場合、その値はファイルを開くときのエンコーディングとして使われます。
   *delay* が true ならば、ファイルを開くのは最初の :meth:`emit` 呼び出しまで遅らせられます。
   デフォルトでは、ファイルは無制限に大きくなりつづけます。


   .. method:: emit(record)

      レコードをファイルに出力しますが、その前にファイルが変更されていないかチェックします。
      もし変更されていれば、レコードをファイルに出力する前に、
      既存のストリームはフラッシュして閉じられ、ファイルが再度開かれます。

.. _rotating-file-handler:

RotatingFileHandler
^^^^^^^^^^^^^^^^^^^

:mod:`logging.handlers` モジュールに含まれる :class:`RotatingFileHandler` クラスは、
ディスク上のログファイルに対するローテーション処理をサポートします。


.. class:: RotatingFileHandler(filename, mode='a', maxBytes=0, backupCount=0, encoding=None, delay=0)

   :class:`RotatingFileHandler` クラスの新たなインスタンスを返します。
   指定されたファイルが開かれ、ログ記録のためのストリームとして使われます。
   *mode* が指定されなかった場合、 :const:`'a'` が使われます。
   *encoding* が *None* でない場合、その値はファイルを開くときのエンコーディングとして使われます。
   *delay* が true ならば、ファイルを開くのは最初の :meth:`emit` 呼び出しまで遅らせられます。
   デフォルトでは、ファイルは無制限に大きくなりつづけます。

   *maxBytes* および *backupCount* 値を指定することで、
   あらかじめ決められたサイズでファイルをロールオーバ (:dfn:`rollover`) させることができます。
   指定サイズを超えそうになると、ファイルは閉じられ、暗黙のうちに新たなファイルが開かれます。
   ロールオーバは現在のログファイルの長さが *maxBytes* に近くなると常に起きます。
   *backupCount* が非ゼロの場合、システムは古いログファイルをファイル名に ".1", ".2" といった拡張子を追加して保存します。
   例えば、 *backupCount* が 5 で、基本のファイル名が :file:`app.log` なら、 :file:`app.log`,
   :file:`app.log.1`, :file:`app.log.2` ... と続き、 :file:`app.log.5` までを得ることになります。
   ログの書き込み対象になるファイルは常に :file:`app.log` です。
   このファイルが満杯になると、ファイルは閉じられ、 :file:`app.log.1` に名前が変更されます。
   :file:`app.log.1`, :file:`app.log.2` などが存在する場合、
   それらのファイルはそれぞれ :file:`app.log.2`, :file:`app.log.3` といった具合に名前が変更されます。

   .. versionchanged:: 2.6
      *delay* が追加されました。


   .. method:: doRollover()

      上述のような方法でロールオーバを行います。


   .. method:: emit(record)

      上述のようなロールオーバを行いながら、レコードをファイルに出力します。

.. _timed-rotating-file-handler:

TimedRotatingFileHandler
^^^^^^^^^^^^^^^^^^^^^^^^

:mod:`logging.handlers` モジュールに含まれる :class:`TimedRotatingFileHandler` クラスは、
特定の時間間隔でのログローテーションをサポートしています。


.. class:: TimedRotatingFileHandler(filename, when='h', interval=1, backupCount=0, encoding=None, delay=False, utc=False)

   :class:`TimedRotatingFileHandler` クラスの新たなインスタンスを返します。
   *filename* に指定したファイルを開き、ログ出力先のストリームとして使います。
   ログファイルのローテーション時には、ファイル名に拡張子 (suffix) をつけます。
   ログファイルのローテーションは *when* および *interval* の積に基づいて行います。

   *when* は *interval* の単位を指定するために使います。
   使える値は下表の通りです。大小文字の区別は行いません:

   +----------------+-------------------+
   | 値             | *interval* の単位 |
   +================+===================+
   | ``'S'``        | 秒                |
   +----------------+-------------------+
   | ``'M'``        | 分                |
   +----------------+-------------------+
   | ``'H'``        | 時間              |
   +----------------+-------------------+
   | ``'D'``        | 日                |
   +----------------+-------------------+
   | ``'W'``        | 曜日 (0=Monday)   |
   +----------------+-------------------+
   | ``'midnight'`` | 深夜              |
   +----------------+-------------------+

   古いログファイルを保存する際にロギングシステムは拡張子を付けます。
   拡張子は日付と時間に基づいて、 strftime の ``%Y-%m-%d_%H-%M-%S`` 形式かその前方の一部を、
   ロールオーバ間隔に依存した形で使います。

   最初に次のロールオーバー時間を計算するとき (ハンドラが生成されるとき)、
   次のローテーションがいつ起こるかを計算するために、
   既存のログファイルの最終変更時刻または現在の時間が使用されます。

   *utc* 引数が true の場合時刻は UTC になり、それ以外では現地時間が使われます。

   *backupCount* がゼロでない場合、保存されるファイル数は高々 *backupCount* 個で、
   それ以上のファイルがロールオーバされる時に作られるならば、一番古いものが削除されます。
   削除のロジックは interval で決まるファイルを削除するので、
   interval を変えると古いファイルが残ったままになることもあります。

   *delay* が true なら、ファイルを開くのは :meth:`emit` の最初の呼び出しまで延期されます。

   .. versionchanged:: 2.6
      *delay* が追加されました。

   .. versionchanged:: 2.7
      *utc* が追加されました。


   .. method:: doRollover()

      上述の方法でロールオーバを行います。


   .. method:: emit(record)

      Outputs the record to the file, catering for rollover as described above.
      上で説明した方法でロールオーバを行いながら、レコードをファイルに出力します。


.. _socket-handler:

SocketHandler
^^^^^^^^^^^^^

:mod:`logging.handlers` モジュールに含まれる :class:`SocketHandler` クラスは、
ログ出力をネットワークソケットに送信します。基底クラスでは TCP ソケットを用います。


.. class:: SocketHandler(host, port)

   アドレスが *host* および *port* で与えられた遠隔のマシンと通信するようにした
   :class:`SocketHandler` クラスのインスタンスを生成して返します。


   .. method:: close()

      ソケットを閉じます。


   .. method:: emit()

      レコードの属性辞書を pickle して、バイナリ形式でソケットに書き込みます。
      ソケット操作でエラーが生じた場合、暗黙のうちにパケットは捨てられます。
      事前に接続が失われていた場合、接続を再度確立します。
      受信端でレコードを unpickle して :class:`LogRecord` にするには、
      :func:`makeLogRecord` 関数を使ってください。


   .. method:: handleError()

      :meth:`emit` の処理中に発生したエラーを処理します。
      よくある原因は接続の消失です。
      次のイベント発生時に再試行できるようにソケットを閉じます。


   .. method:: makeSocket()

      サブクラスで必要なソケット形式を詳細に定義できるようにするためのファクトリメソッドです。
      デフォルトの実装では、 TCP ソケット (:const:`socket.SOCK_STREAM`) を生成します。


   .. method:: makePickle(record)

      レコードの属性辞書を pickle してから先頭に長さ情報を付けてバイナリ形式にして、
      ソケットを介して送信できるようにして返します。

      pickle が完全に安全というわけではないことに注意してください。
      セキュリティに関して心配なら、より安全なメカニズムを実装するためにこのメソッドをオーバーライドすると良いでしょう。
      例えば、 HMAC を使って pickle に署名して、受け取る側ではそれを検証することができます。
      あるいはまた、受け取る側でグローバルなオブジェクトの unpickle を無効にすることができます。


   .. method:: send(packet)

      pickle された文字列 *packet* をソケットに送信します。
      この関数はネットワークがビジーの時に発生する部分的送信に対応しています。


   .. method:: createSocket()

      ソケットの生成を試みます。失敗時には、指数的な減速アルゴリズムを
      使います。最初の失敗時には、ハンドラは送ろうとしていたメッセージを
      落とします。続くメッセージが同じインスタンスで扱われたとき、
      幾らかの時間が経過するまで接続を試みません。
      デフォルトのパラメタは、最初の遅延時間が 1 秒で、その遅延時間の後で
      それでも接続が確保できないなら、遅延時間は 2 倍づつになり、
      最大で 30 秒になります。

      この働きは、以下のハンドラ属性で制御されます:

      * ``retryStart`` (最初の遅延時間、デフォルトは 1.0 秒)。
      * ``retryFactor`` (乗数、デフォルトは 2.0)。
      * ``retryMax`` (最大遅延時間、デフォルトは 30.0 秒)。

      これは、リモートリスナがハンドラが使われた *後に* 起動すると、
      (ハンドラは遅延が経過するまで接続を試みようとさえせず、遅延時間中に黙って
      メッセージを落とすだけなので)
      メッセージが失われてしまうこともあるということです。


.. _datagram-handler:

DatagramHandler
^^^^^^^^^^^^^^^

:mod:`logging.handlers` モジュールに含まれる :class:`DatagramHandler` クラスは、
:class:`SocketHandler` を継承しており、 UDP ソケットを介したログ記録メッセージの送信をサポートしています。


.. class:: DatagramHandler(host, port)

   アドレスが *host* および *port* で与えられた遠隔のマシンと通信するようにした
   :class:`DatagramHandler` クラスのインスタンスを生成して返します。


   .. method:: emit()

      レコードの属性辞書を pickle して、バイナリ形式でソケットに書き込みます。
      ソケット操作でエラーが生じた場合、暗黙のうちにパケットは捨てられます。
      事前に接続が失われていた場合、接続を再度確立します。
      受信端でレコードを unpickle して :class:`LogRecord` にするには、
      :func:`makeLogRecord` 関数を使ってください。


   .. method:: makeSocket()

      ここで :class:`SocketHandler` のファクトリメソッドをオーバライドして、
      UDP ソケット (:const:`socket.SOCK_DGRAM`) を生成しています。


   .. method:: send(s)

      pickle された文字列をソケットに送信します。


.. _syslog-handler:

SysLogHandler
^^^^^^^^^^^^^

:mod:`logging.handlers` モジュールに含まれる :class:`SysLogHandler` クラスは、
ログ記録メッセージを遠隔またはローカルの Unix syslog に送信する機能をサポートしています。


.. class:: SysLogHandler(address=('localhost', SYSLOG_UDP_PORT), facility=LOG_USER, socktype=socket.SOCK_DGRAM)

   遠隔の Unix マシンと通信するための、 :class:`SysLogHandler` クラスの新たなインスタンスを返します。
   マシンのアドレスは ``(host, port)`` のタプル形式をとる *address* で与えられます。
   *address* が指定されない場合、 ``('localhost', 514)`` が使われます。
   アドレスは UDP ソケットを使って開かれます。
   ``(host, port)`` のタプル形式の代わりに文字列で "/dev/log" のように与えることもできます。
   この場合、 Unix ドメインソケットが syslog にメッセージを送るのに使われます。
   *facility* が指定されない場合、 :const:`LOG_USER` が使われます。
   開かれるソケットの型は、 *socktype* 引数に依り、デフォルトは
   :const:`socket.SOCK_DGRAM` で、UDP ソケットを開きます。
   (rsyslog のような新しい syslog デーモンと使うために) TCP ソケットを開く
   には、 :const:`socket.SOCK_STREAM` の値を指定してください。

   なお、あなたのサーバが UDP ポート 514 を聴取していないなら、
   :class:`SysLogHandler` が働かないことがあります。その場合は、
   あなたがドメインソケットに使うべきアドレスが何か調べてください -
   これはシステム依存です。例えば、Linux システムでは通常 '/dev/log' ですが、
   OS/X では '/var/run/syslog' です。あなたのプラットフォームを調べ、
   適切なアドレスを使うことが必要になります (あなたのアプリケーションが
   いくつかのプラットフォームで動く必要があるなら、これを実行時に
   確かめなければならないかもしれません)。
   Windows では、UDP オプションを使う必要性がかなり高いです。

   .. versionchanged:: 2.7
      *socktype* が追加されました。


   .. method:: close()

      遠隔ホストへのソケットを閉じます。


   .. method:: emit(record)

      レコードは書式化された後、 syslog サーバに送信されます。
      例外情報が存在しても、サーバには *送信されません* 。


   .. method:: encodePriority(facility, priority)

      ファシリティおよび優先度を整数に符号化します。
      値は文字列でも整数でも渡すことができます。
      文字列が渡された場合、内部の対応付け辞書が使われ、整数に変換されます。

      シンボリックな ``LOG_`` 値は :class:`SysLogHandler` で定義されています。
      これは ``sys/syslog.h`` ヘッダーファイルで定義された値を反映しています。

      **優先度**

      +--------------------------+---------------+
      | 名前 (文字列)            | シンボル値    |
      +==========================+===============+
      | ``alert``                | LOG_ALERT     |
      +--------------------------+---------------+
      | ``crit`` or ``critical`` | LOG_CRIT      |
      +--------------------------+---------------+
      | ``debug``                | LOG_DEBUG     |
      +--------------------------+---------------+
      | ``emerg`` or ``panic``   | LOG_EMERG     |
      +--------------------------+---------------+
      | ``err`` or ``error``     | LOG_ERR       |
      +--------------------------+---------------+
      | ``info``                 | LOG_INFO      |
      +--------------------------+---------------+
      | ``notice``               | LOG_NOTICE    |
      +--------------------------+---------------+
      | ``warn`` or ``warning``  | LOG_WARNING   |
      +--------------------------+---------------+

      **ファシリティ**

      +---------------+---------------+
      | 名前 (文字列) | シンボル値    |
      +===============+===============+
      | ``auth``      | LOG_AUTH      |
      +---------------+---------------+
      | ``authpriv``  | LOG_AUTHPRIV  |
      +---------------+---------------+
      | ``cron``      | LOG_CRON      |
      +---------------+---------------+
      | ``daemon``    | LOG_DAEMON    |
      +---------------+---------------+
      | ``ftp``       | LOG_FTP       |
      +---------------+---------------+
      | ``kern``      | LOG_KERN      |
      +---------------+---------------+
      | ``lpr``       | LOG_LPR       |
      +---------------+---------------+
      | ``mail``      | LOG_MAIL      |
      +---------------+---------------+
      | ``news``      | LOG_NEWS      |
      +---------------+---------------+
      | ``syslog``    | LOG_SYSLOG    |
      +---------------+---------------+
      | ``user``      | LOG_USER      |
      +---------------+---------------+
      | ``uucp``      | LOG_UUCP      |
      +---------------+---------------+
      | ``local0``    | LOG_LOCAL0    |
      +---------------+---------------+
      | ``local1``    | LOG_LOCAL1    |
      +---------------+---------------+
      | ``local2``    | LOG_LOCAL2    |
      +---------------+---------------+
      | ``local3``    | LOG_LOCAL3    |
      +---------------+---------------+
      | ``local4``    | LOG_LOCAL4    |
      +---------------+---------------+
      | ``local5``    | LOG_LOCAL5    |
      +---------------+---------------+
      | ``local6``    | LOG_LOCAL6    |
      +---------------+---------------+
      | ``local7``    | LOG_LOCAL7    |
      +---------------+---------------+

   .. method:: mapPriority(levelname)

      ログレベル名を syslog 優先度名に対応付けます。
      カスタムレベルを使用している場合や、
      デフォルトアルゴリズムがニーズに適していない場合には、
      このメソッドをオーバーライドする必要があるかもしれません。
      デフォルトアルゴリズムは、 ``DEBUG``, ``INFO``, ``WARNING``,
      ``ERROR``, ``CRITICAL`` を等価な syslog 名に、
      他のすべてのレベル名を "warning" に対応付けます。

.. _nt-eventlog-handler:

NTEventLogHandler
^^^^^^^^^^^^^^^^^

:mod:`logging.handlers` モジュールに含まれる :class:`NTEventLogHandler` クラスは、
ログ記録メッセージをローカルな Windows NT, Windows 2000, または Windows XP のイベントログに送信する機能をサポートします。
この機能を使えるようにするには、 Mark Hammond による Python 用 Win32 拡張パッケージをインストールする必要があります。


.. class:: NTEventLogHandler(appname, dllname=None, logtype='Application')

   :class:`NTEventLogHandler` クラスの新たなインスタンスを返します。
   *appname* はイベントログに表示する際のアプリケーション名を定義するために使われます。
   この名前を使って適切なレジストリエントリが生成されます。
   *dllname* はログに保存するメッセージ定義の入った .dll または .exe  ファイルへの完全修飾パス名を与えなければなりません
   (指定されない場合、 ``win32service.pyd`` が使われます -
   このライブラリは Win32 拡張とともにインストールされ、いくつかのプレースホルダとなるメッセージ定義を含んでいます)。
   これらのプレースホルダを利用すると、メッセージの発信源全体がログに記録されるため、
   イベントログは巨大になるので注意してください。
   *logtype* は ``Application``, ``System``, ``Security`` のいずれかで、
   デフォルトは ``Application`` です。


   .. method:: close()

      現時点では、イベントログエントリの発信源としてのアプリケーション名をレジストリから除去することはできます。
      しかしこれを行うと、イベントログビューアで意図した通りにログが見えなくなるでしょう -
      これはイベントログが .dll 名を取得するためにレジストリにアクセスできなければならないからです。
      現在のバージョンではこの操作を行いません。


   .. method:: emit(record)

      メッセージ ID\ 、イベントカテゴリ、イベント型を決定し、メッセージを NT イベントログに記録します。


   .. method:: getEventCategory(record)

      レコードに対するイベントカテゴリを返します。
      自作のカテゴリを指定したい場合、このメソッドをオーバライドしてください。
      このクラスのバージョンのメソッドは 0 を返します。


   .. method:: getEventType(record)

      レコードのイベント型を返します。
      自作の型を指定したい場合、このメソッドをオーバライドしてください。
      このクラスのバージョンのメソッドは、ハンドラの *typemap* 属性を使って対応付けを行います。
      この属性は :meth:`__init__` で初期化され、 :const:`DEBUG`, :const:`INFO`, :const:`WARNING`,
      :const:`ERROR`, :const:`CRITICAL` が入っています。
      自作のレベルを使っているのなら、このメソッドをオーバライドするか、
      ハンドラの *typemap* 属性に適切な辞書を配置する必要があるでしょう。


   .. method:: getMessageID(record)

      レコードのメッセージ ID を返します。
      自作のメッセージを使っているのなら、ロガーに渡される *msg* を書式化文字列ではなく ID にします。
      その上で、辞書参照を行ってメッセージ ID を得ます。
      このクラスのバージョンでは 1 を返します。
      この値は :file:`win32service.pyd` における基本メッセージ ID です。

.. _smtp-handler:

SMTPHandler
^^^^^^^^^^^

:mod:`logging.handlers` モジュールに含まれる :class:`SMTPHandler` クラスは、
SMTP を介したログ記録メッセージの送信機能をサポートします。


.. class:: SMTPHandler(mailhost, fromaddr, toaddrs, subject, credentials=None, secure=None)

   新たな :class:`SMTPHandler` クラスのインスタンスを返します。
   インスタンスは email の from および to アドレス行、および subject 行とともに初期化されます。
   *toaddrs* は文字列からなるリストでなければなりません。
   非標準の SMTP ポートを指定するには、 *mailhost* 引数に (host, port)  のタプル形式を指定します。
   文字列を使った場合、標準の SMTP ポートが使われます。
   もし SMTP サーバが認証を必要とするならば、 (username, password) のタプル形式を
   *credentials* 引数に指定することができます。
   *secure* が真であれば、ハンドラは email の伝達に TLS を使おうと試みます。

   .. versionchanged:: 2.6
      *credentials* が追加されました。

   .. versionchanged:: 2.7
      *secure* が追加されました。


   .. method:: emit(record)

      レコードを書式化し、指定されたアドレスに送信します。


   .. method:: getSubject(record)

      レコードに応じたサブジェクト行を指定したいなら、このメソッドをオーバライドしてください。

.. _memory-handler:

MemoryHandler
^^^^^^^^^^^^^

:mod:`logging.handlers` モジュールに含まれる :class:`MemoryHandler` は、
ログ記録するレコードをメモリ上にバッファリングし、
定期的にその内容をターゲット (:dfn:`target`) となるハンドラにフラッシュする機能をサポートしています。
フラッシュ処理はバッファが一杯になるか、
ある深刻度かそれ以上のレベルを持つイベントが観測された際に行われます。

:class:`MemoryHandler` はより一般的な抽象クラス、 :class:`BufferingHandler` のサブクラスです。
この抽象クラスでは、ログ記録するレコードをメモリ上にバッファリングします。
各レコードがバッファに追加される毎に、 :meth:`shouldFlush` を呼び出してバッファをフラッシュすべきかどうか調べます。
フラッシュする必要がある場合、 :meth:`flush` が必要にして十分な処理を行うものと想定しています。


.. class:: BufferingHandler(capacity)

   指定した許容量のバッファでハンドラを初期化します。


   .. method:: emit(record)

      レコードをバッファに追加します。
      :meth:`shouldFlush` が true を返す場合、バッファを処理するために :meth:`flush` を呼び出します。


   .. method:: flush()

      このメソッドをオーバライドして、自作のフラッシュ動作を実装することができます。
      このクラスのバージョンのメソッドでは、単にバッファの内容を削除して空にします。


   .. method:: shouldFlush(record)

      バッファが許容量に達している場合に true を返します。
      このメソッドは自作のフラッシュ処理方針を実装するためにオーバライドすることができます。


.. class:: MemoryHandler(capacity, flushLevel=ERROR, target=None)

   :class:`MemoryHandler` クラスの新たなインスタンスを返します。
   インスタンスはサイズ *capacity* のバッファとともに初期化されます。
   *flushLevel* が指定されていない場合、 :const:`ERROR` が使われます。
   *target* が指定されていない場合、ハンドラが何らかの意味のある処理を行う前に
   :meth:`setTarget` でターゲットを指定する必要があります。


   .. method:: close()

      :meth:`flush` を呼び出し、ターゲットを :const:`None` に設定してバッファを消去します。


   .. method:: flush()

      :class:`MemoryHandler` の場合、フラッシュ処理は単に、バッファされたレコードをターゲットがあれば送信することを意味します。
      これと異なる動作を行いたい場合、オーバライドしてください。


   .. method:: setTarget(target)

      ターゲットハンドラをこのハンドラに設定します。


   .. method:: shouldFlush(record)

      バッファが一杯になっているか、 *flushLevel* またはそれ以上のレコードでないかを調べます。


.. _http-handler:

HTTPHandler
^^^^^^^^^^^

:mod:`logging.handlers` モジュールに含まれる :class:`HTTPHandler` クラスは、
ログ記録メッセージを ``GET`` または ``POST`` セマンティクスを使って Web サーバに送信する機能をサポートしています。


.. class:: HTTPHandler(host, url, method='GET')

   :class:`HTTPHandler` クラスの新たなインスタンスを返します。
   *host* は特別なポートを使うことが必要な場合には、 ``host:port`` の形式で使うこともできます。
   *method* が指定されなかった場合 ``GET`` が使われます。


   .. method:: emit(record)

      レコードを URL エンコードされた辞書形式で Web サーバに送信します。


.. seealso::

   Module :mod:`logging`
      logging モジュールの API リファレンスです。

   Module :mod:`logging.config`
      logging モジュールの環境設定 API です。


