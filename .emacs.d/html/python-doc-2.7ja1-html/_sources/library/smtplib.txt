
:mod:`smtplib` --- SMTP プロトコルクライアント
===============================================

.. module:: smtplib
   :synopsis: SMTP プロトコルクライアント (ソケットが必要です)。
.. sectionauthor:: Eric S. Raymond <esr@snark.thyrsus.com>


.. index::
   pair: SMTP; protocol
   single: Simple Mail Transfer Protocol

:mod:`smtplib` モジュールは、SMTPまたはESMTPのリスナーデーモンを備えた
任意のインターネット上のホストにメールを送るために使用することができる SMTPクライアント・セッション・オブジェクトを定義します。
SMTPおよびESMTPオペレーションの詳細は、 :rfc:`821` (Simple Mail Transfer Protocol) や
:rfc:`1869` (SMTP Service Extensions)を調べてください。


.. class:: SMTP([host[, port[, local_hostname[, timeout]]]])

   :class:`SMTP` インスタンスはSMTPコネクションをカプセル化し、 SMTPとESMTPの命令をサポートをします。
   オプションであるhostとportを与えた場合は、 SMTPクラスのインスタンスが作成されると同時に、
   :meth:`connect` メソッドを呼び出し初期化されます。
   また、ホストから応答が無い場合は、 :exc:`SMTPConnectError` が上げられます。
   オプションの *timeout* 引数を与える場合、コネクションの接続時などのブロックする操作における
   タイムアウト時間を秒数で設定します。(指定されなかった場合は、グローバルのデフォルトタイムアウト設定が
   利用されます)

   普通に使う場合は、初期化と接続を行ってから、 :meth:`sendmail` と :meth:`quit` メソッドを呼びます。使用例は先の方で記載しています。

   .. versionchanged:: 2.6
      *timeout* が追加されました


.. class:: SMTP_SSL([host[, port[, local_hostname[, keyfile[, certfile[, timeout]]]]]])

   .. A :class:`SMTP_SSL` instance behaves exactly the same as instances of
      :class:`SMTP`. :class:`SMTP_SSL` should be used for situations where SSL is
      required from the beginning of the connection and using :meth:`starttls` is
      not appropriate. If *host* is not specified, the local host is used. If
      *port* is omitted, the standard SMTP-over-SSL port (465) is used. *keyfile*
      and *certfile* are also optional, and can contain a PEM formatted private key
      and certificate chain file for the SSL connection. The optional *timeout*
      parameter specifies a timeout in seconds for blocking operations like the
      connection attempt (if not specified, the global default timeout setting
      will be used).

   :class:`SMTP_SSL` のインスタンスは :class:`SMTP` のインスタンスと全く同じように動作します。
   :class:`SMTP_SSL` は、接続の最初の段階からSSLが要求され、 :meth:`starttls`
   では対応できない場合にのみ利用されるべきです。
   *host* が指定されなかった場合は、localhostが利用されます。
   *port* が指定されなかった場合は、標準の SMTP-over-SSL ポート(465) が利用されます。
   *keyfile* と *certfile* もオプションで、SSL接続のための、PEMフォーマットのプライベートキーと、
   証明パス(certificate chain)ファイルを指定することができます。
   オプションの *timeout* 引数を与える場合、コネクションの接続時などのブロックする操作における
   タイムアウト時間を秒数で設定します。(指定されなかった場合は、グローバルのデフォルトタイムアウト設定が
   利用されます)

   .. versionadded:: 2.6


.. class:: LMTP([host[, port[, local_hostname]]])

   .. The LMTP protocol, which is very similar to ESMTP, is heavily based on the
      standard SMTP client. It's common to use Unix sockets for LMTP, so our :meth:`connect`
      method must support that as well as a regular host:port server. To specify a
      Unix socket, you must use an absolute path for *host*, starting with a '/'.

   ESMTPに非常に似ているLMTPプロトコルは、SMTPクライアントに基づいています。
   LMTPにはよくUnixソケットが利用されるので、 :meth:`connect` メソッドは通常の
   host:port サーバーと同じようにUnixソケットもサポートしています。
   Unixソケットを指定するには、 *host* 引数に、 '/' で始まる絶対パスを指定します。

   .. Authentication is supported, using the regular SMTP mechanism. When using a Unix
      socket, LMTP generally don't support or require any authentication, but your
      mileage might vary.

   認証は、通常のSMTP機構を利用してサポートされています。
   Unixソケットを利用する場合、LMTPは通常認証をサポートしたり要求したりはしません。
   しかし、あなたが必要であれば、利用することができます。

   .. versionadded:: 2.6

このモジュールの例外には次のものがあります:


.. exception:: SMTPException

   このモジュールの例外クラスのベースクラスです。


.. exception:: SMTPServerDisconnected

   この例外はサーバが突然コネクションを切断するか、もしくは :class:`SMTP` インスタンスを生成する前にコネクションを張ろうとした場合に上げられます。


.. exception:: SMTPResponseException

   SMTPのエラーコードを含んだ例外のクラスです。これらの例外はSMTPサーバがエラーコードを返すときに生成されます。
   エラーコードは :attr:`smtp_code` 属性に格納されます。また、 :attr:`smtp_error` 属性にはエラーメッセージが格納されます。


.. exception:: SMTPSenderRefused

   送信者のアドレスが弾かれたときに上げられる例外です。全ての :exc:`SMTPResponseException` 例外に、
   SMTPサーバが弾いた'sender'アドレスの文字列がセットされます。


.. exception:: SMTPRecipientsRefused

   全ての受取人アドレスが弾かれたときに上げられる例外です。各受取人のエラーは属性 :attr:`recipients` によってアクセス可能で、
   :meth:`SMTP.sendmail` が返す辞書と同じ並びの辞書になっています。


.. exception:: SMTPDataError

   SMTPサーバが、メッセージのデータを受け入れることを拒絶した時に上げられる例外です。


.. exception:: SMTPConnectError

   サーバへの接続時にエラーが発生した時に上げられる例外です。


.. exception:: SMTPHeloError

   サーバーが ``HELO`` メッセージを弾いた時に上げられる例外です。


.. exception:: SMTPAuthenticationError

   .. SMTP authentication went wrong.  Most probably the server didn't accept the
      username/password combination provided.

   SMTP 認証が失敗しました。
   最もあり得る可能性は、サーバーがユーザ名/パスワードのペアを受付なかった事です。


.. seealso::

   :rfc:`821` - Simple Mail Transfer Protocol
      SMTP のプロトコル定義です。このドキュメントでは SMTP のモデル、操作手順、プロトコルの詳細についてカバーしています。

   :rfc:`1869` - SMTP Service Extensions
      SMTP に対する ESMTP 拡張の定義です。このドキュメントでは、新たな命令による SMTP の拡張、サーバによって提供される命令を
      動的に発見する機能のサポート、およびいくつかの追加命令定義について記述しています。


.. _smtp-objects:

SMTP オブジェクト
-----------------

:class:`SMTP` クラスインスタンスは次のメソッドを提供します:


.. method:: SMTP.set_debuglevel(level)

   コネクション間でやりとりされるメッセージ出力のレベルをセットします。メッセージの冗長さは *level* に応じて決まります。


.. method:: SMTP.connect([host[, port]])

   ホスト名とポート番号をもとに接続します。デフォルトはlocalhostの標準的なSMTPポート(25番)に接続します。
   もしホスト名の末尾がコロン(``':'``)で、後に番号がついている場合は、「ホスト名:ポート番号」として扱われます。
   このメソッドはコンストラクタにホスト名及びポート番号が指定されている場合、自動的に呼び出されます。


.. method:: SMTP.docmd(cmd, [, argstring])

   サーバへコマンド *cmd* を送信します。オプション引数 *argstring* はスペース文字でコマンドに連結します。
   戻り値は、整数値のレスポンスコードと、サーバからの応答の値をタプルで返します。 (サーバからの応答が数行に渡る場合でも一つの大きな文字列で返します。)

   通常、この命令を明示的に使う必要はありませんが、自分で拡張するする時に使用するときに役立つかもしれません。

   応答待ちのときに、サーバへのコネクションが失われると、 :exc:`SMTPServerDisconnected` が上がります。


.. method:: SMTP.helo([hostname])

   SMTPサーバに ``HELO`` コマンドで身元を示します。デフォルトではhostname引数はローカルホストを指します。
   サーバーが返したメッセージは、オブジェクトの :attr:`help_resp` 属性に格納されます。

   通常は :meth:`sendmail` が呼びだすため、これを明示的に呼び出す必要はありません。


.. method:: SMTP.ehlo([hostname])

   ``EHLO`` を利用し、ESMTPサーバに身元を明かします。
   デフォルトではhostname引数はローカルホストのFQDNです。
   また、ESMTPオプションのために応答を調べたものは、 :meth:`has_extn` に備えて保存されます。
   また、幾つかの情報を属性に保存します: サーバーが返したメッセージは :attr:`ehlo_resp`
   属性に、 :attr:`does_esmtp` 属性はサーバーがESMTPをサポートしているかどうかによって
   true か false に、 :attr:`esmtp_features` 属性は辞書で、サーバーが対応しているSMTP
   サービス拡張の名前と、もしあればそのパラメータを格納します。

   :meth:`has_extn` をメールを送信する前に使わない限り、明示的にこのメソッドを呼び出す必要があるべきではなく、
   :meth:`sendmail` が必要とした場合に呼ばれます。、


.. method:: SMTP.ehlo_or_helo_if_needed()

   .. This method call :meth:`ehlo` and or :meth:`helo` if there has been no
      previous ``EHLO`` or ``HELO`` command this session.  It tries ESMTP ``EHLO``
      first.

   このメソッドは、現在のセッションでまだ ``EHLO`` か ``HELO`` コマンドが実行されていない場合、
   :meth:`ehlo` and/or :meth:`helo` メソッドを呼び出します。
   このメソッドは先に ESMTP ``EHLO`` を試します。

   :exc:`SMTPHeloError`
      サーバーが ``HELO`` に正しく返事しなかった.

      .. The server didn't reply properly to the ``HELO`` greeting.

   .. versionadded:: 2.6

.. method:: SMTP.has_extn(name)

   *name* が拡張SMTPサービスセットに含まれている場合には ``True`` を返し、そうでなければ ``False`` を返します。大小文字は区別されません。


.. method:: SMTP.verify(address)

   ``VRFY`` を利用してSMTPサーバにアドレスの妥当性をチェックします。
   妥当である場合はコード250と完全な :rfc:`822` アドレス(人名)のタプルを返します。
   それ以外の場合は、400以上のエラーコードとエラー文字列を返します。

   .. note::

      ほとんどのサイトはスパマーの裏をかくためにSMTPの ``VRFY`` は使用不可になっています。


.. method:: SMTP.login(user, password)

   認証が必要なSMTPサーバにログインします。認証に使用する引数はユーザ名とパスワードです。
   まだセッションが無い場合は、 ``EHLO`` または ``HELO`` コマンドでセッションを作ります。ESMTPの場合は ``EHLO`` が先に試されます。
   認証が成功した場合は通常このメソッドは戻りますが、例外が起こった場合は以下の例外が上がります:

   :exc:`SMTPHeloError`
      サーバが ``HELO`` に返答できなかった。

   :exc:`SMTPAuthenticationError`
      サーバがユーザ名/パスワードでの認証に失敗した。

   :exc:`SMTPException`
      どんな認証方法も見付からなかった。


.. method:: SMTP.starttls([keyfile[, certfile]])

   TLS(Transport Layer Security)モードでSMTPコネクションを出し、全てのSMTPコマンドは暗号化されます。
   これは :meth:`ehlo` をもう一度呼びだすときにするべきです。

   *keyfile* と *certfile* が提供された場合に、 :mod:`socket` モジュールの :func:`ssl` 関数が通るようになります。

   .. If there has been no previous ``EHLO`` or ``HELO`` command this session,
      this method tries ESMTP ``EHLO`` first.

   もしまだ ``EHLO`` か ``HELO`` コマンドが実行されていない場合、
   このメソッドは ESMTP ``EHLO`` を先に試します。

   .. versionchanged:: 2.6

   :exc:`SMTPHeloError`
      サーバーが ``HELO`` に正しく返事しなかった


   :exc:`SMTPException`
      サーバーが STARTTLS 拡張に対応していない

   .. versionchanged:: 2.6

   :exc:`RuntimeError`
      実行中の Python インタプリタで、 SSL/TLS サポートが利用できない


.. method:: SMTP.sendmail(from_addr, to_addrs, msg[, mail_options, rcpt_options])

   メールを送信します。必要な引数は :rfc:`822` のfromアドレス文字列、 :rfc:`822` のtoアドレス文字列またはアドレス文字列のリスト、
   メッセージ文字列です。送信側は ``MAIL FROM`` コマンドで使用される *mail_options* の
   ESMTPオプション(``8bitmime`` のような)のリストを得るかもしれません。

   全ての ``RCPT`` コマンドで使われるべきESMTPオプション (例えば ``DSN`` コマンド)は、 *rcpt_options* を通して
   利用することができます。(もし送信先別にESMTPオプションを使う必要があれば、
   メッセージを送るために :meth:`mail` 、 :meth:`rcpt` 、 :meth:`data` といった下位レベルのメソッドを使う必要があります。)

   .. note::

      配送エージェントは *from_addr* 、 *to_addrs* 引数を使い、メッセージのエンベロープを構成します。
      :class:`SMTP` はメッセージヘッダを修正しません。

   まだセッションが無い場合は、 ``EHLO`` または ``HELO`` コマンドでセッションを作ります。ESMTPの場合は ``EHLO`` が先に試されます。
   また、サーバがESMTP対応ならば、メッセージサイズとそれぞれ指定されたオプションも渡します。(featureオプションがあればサーバの広告をセットします)
   ``EHLO`` が失敗した場合は、ESMTPオプションの無い ``HELO`` が試されます。

   このメソッドは最低でも1つの受信者にメールが受け入れられたときは普通に戻りますが、
   そうでない場合は例外を投げます。このメソッドが例外を投げられなければ、
   誰かが送信したメールを得るべきです。また、例外を投げれなかった場合は、
   拒絶された受取人ごとへの1つのエントリーと共に、辞書を返します。
   各エントリーは、サーバーによって送られたSMTPエラーコードおよびエラーメッセージの
   タプルを含んでいます。

   このメソッドは次の例外を上げることがあります:

   :exc:`SMTPRecipientsRefused`
      全ての受信を拒否され、誰にもメールが届けられませんでした。例外オブジェクトの :attr:`recipients` 属性は、
      受信拒否についての情報の入った辞書オブジェクトです。 (辞書は少なくとも一つは受信されたときに似ています)。

   :exc:`SMTPHeloError`
      サーバが ``HELP`` に返答しませんでした。

   :exc:`SMTPSenderRefused`
      サーバが *from_addr* を弾きました。

   :exc:`SMTPDataError`
      サーバが予期しないエラーコードを返しました。(受信拒否以外)

   また、この他の注意として、例外が上がった後もコネクションは開いたままになっています。


.. method:: SMTP.quit()

   SMTPセッションを終了し、コネクションを閉じます。
   SMTP ``QUIT`` コマンドの結果を返します。

   .. versionchanged:: 2.6
      結果を返すようになりました

下位レベルのメソッドは標準SMTP/ESMTPコマンド ``HELP`` 、 ``RSET`` 、
``NOOP`` 、 ``MAIL`` 、 ``RCPT`` 、 ``DATA`` に対応しています。通常これらは直接呼ぶ必要はなく、また、ドキュメントもありません。
詳細はモジュールのコードを調べてください。


.. _smtp-example:

SMTP 使用例
-----------

次の例は最低限必要なメールアドレス('To' と 'From')を含んだ
メッセージを送信するものです。この例では :rfc:`822` ヘッダの加工もしていません。メッセージに含まれるヘッダは、メッセージに含まれる必要があり、
特に、明確な'To'、と'From'アドレスはメッセージヘッダに含まれている必要があります。 ::

   import smtplib
   import string

   def prompt(prompt):
       return raw_input(prompt).strip()

   fromaddr = prompt("From: ")
   toaddrs  = prompt("To: ").split()
   print "Enter message, end with ^D (Unix) or ^Z (Windows):"

   # Add the From: and To: headers at the start!
   msg = ("From: %s\r\nTo: %s\r\n\r\n"
          % (fromaddr, ", ".join(toaddrs, ", ")))
   while 1:
       try:
           line = raw_input()
       except EOFError:
           break
       if not line:
           break
       msg = msg + line

   print "Message length is " + repr(len(msg))

   server = smtplib.SMTP('localhost')
   server.set_debuglevel(1)
   server.sendmail(fromaddr, toaddrs, msg)
   server.quit()


.. note::

   .. In general, you will want to use the :mod:`email` package's features to
      construct an email message, which you can then convert to a string and send
      via :meth:`sendmail`; see :ref:`email-examples`.

   多くの場合、 :mod:`email` パッケージの機能を使って email メッセージを構築し、
   それを文字列に変換して、 :meth:`sendmail` で送信する、という手順を用います。
   :ref:`email-examples` を参照してください。
