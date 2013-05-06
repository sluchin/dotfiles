:mod:`imaplib` --- IMAP4 プロトコルクライアント
===============================================

.. module:: imaplib
   :synopsis: IMAP4 protocol client (requires sockets).
.. moduleauthor:: Piers Lauder <piers@communitysolutions.com.au>
.. sectionauthor:: Piers Lauder <piers@communitysolutions.com.au>
.. revised by ESR, January 2000
.. changes for IMAP4_SSL by Tino Lange <Tino.Lange@isg.de>, March 2002
.. changes for IMAP4_stream by Piers Lauder <piers@communitysolutions.com.au>,
   November 2002


.. index::
   pair: IMAP4; protocol
   pair: IMAP4_SSL; protocol
   pair: IMAP4_stream; protocol

このモジュールでは三つのクラス、 :class:`IMAP4`, :class:`IMAP4_SSL` と :class:`IMAP4_stream`
を定義します。これらのクラスは IMAP4 サーバへの接続をカプセル化し、 :rfc:`2060` に定義されている IMAP4rev1
クライアントプロトコルの大規模なサブセットを実装しています。このクラスは IMAP4 (:rfc:`1730`) 準拠の
サーバと後方互換性がありますが、 ``STATUS`` コマンドは IMAP4 ではサポートされていないので注意してください。

:mod:`imaplib` モジュール内では三つのクラスを提供しており、 :class:`IMAP4` は基底クラスとなります:


.. class:: IMAP4([host[, port]])

   このクラスは実際の IMAP4 プロトコルを実装しています。インスタンスが初期化された際に接続が生成され、プロトコルバージョン (IMAP4 または
   IMAP4rev1) が決定されます。 *host* が指定されていない場合、 ``''`` (ローカルホスト) が用いられます。 *port*
   が省略された場合、標準の IMAP4 ポート番号 (143)  が用いられます。

例外は :class:`IMAP4` クラスの属性として定義されています:


.. exception:: IMAP4.error

   何らかのエラー発生の際に送出される例外です。例外の理由は文字列としてコンストラクタに渡されます。


.. exception:: IMAP4.abort

   IMAP4 サーバのエラーが生じると、この例外が送出されます。この例外は :exc:`IMAP4.error` のサブクラスです。
   通常、インスタンスを閉じ、新たなインスタンスを再び生成することで、この例外から復旧できます。


.. exception:: IMAP4.readonly

   この例外は書き込み可能なメールボックスの状態がサーバによって変更された際に送出されます。この例外は :exc:`IMAP4.error` のサブクラスです。
   他の何らかのクライアントが現在書き込み権限を獲得しており、メールボックスを開きなおして書き込み権限を再獲得する必要があります。

このモジュールではもう一つ、安全 (secure) な接続を使ったサブクラスがあります:


.. class:: IMAP4_SSL([host[, port[, keyfile[, certfile]]]])

   :class:`IMAP4` から派生したサブクラスで、SSL 暗号化ソケットを介して接続を行います (このクラスを利用するためには SSL サポート付きで
   コンパイルされた socket モジュールが必要です) 。 *host* が指定されていない場合、 ``''`` (ローカルホスト) が用いられます。
   *port* が省略された場合、標準の IMAP4-over-SSL ポート番号 (993)  が用いられます。 *keyfile* および
   *certfile* もオプションです - これらは SSL 接続のための PEM 形式の秘密鍵 (private key) と認証チェイン
   (certificate chain) ファイルです。

さらにもう一つのサブクラスは、子プロセスで確立した接続を使用する場合に使用します。


.. class:: IMAP4_stream(command)

   :class:`IMAP4` から派生したサブクラスで、 *command* を ``os.popen2()`` に渡して作成される
   ``stdin/stdout`` ディスクリプタと接続します。

   .. versionadded:: 2.3

以下のユーティリティ関数が定義されています:


.. function:: Internaldate2tuple(datestr)

   IMAP4 の ``INTERNALDATE`` 文字列を解析してそれに相当するローカルタイムを返します。
   戻り値は :class:`time.struct_time` のインスタンスか、文字列のフォーマットが不正な
   場合は None です。


.. function:: Int2AP(num)

   整数を [``A`` .. ``P``] からなる文字集合を用いて表現した文字列に変換します。


.. function:: ParseFlags(flagstr)

   IMAP4 ``FLAGS`` 応答を個々のフラグからなるタプルに変換します。


.. function:: Time2Internaldate(date_time)

   *date_time* を IMAP4 の ``INTERNALDATE`` 表現形式に変換します。
   戻り値は ``"DD-Mmm-YYYY HH:MM:SS +HHMM"`` (ダブルクォートを含む) の形をした
   文字列です。
   *date_time* 引数は (:func:`time.time` が返す) epoch からの経過秒数を表す数値
   (int か float) か、(:func:`time.localtime` が返す) ローカルタイムを表現する
   9要素タプルか、ダブルクォートされた文字列です。文字列だった場合、それがすでに
   正しいフォーマットになっていると仮定されます。

IMAP4 メッセージ番号は、メールボックスに対する変更が行われた後には変化します; 特に、 ``EXPUNGE`` 命令はメッセージの削除を
行いますが、残ったメッセージには再度番号を振りなおします。従って、メッセージ番号ではなく、 UID 命令を使い、その UID を利用するよう強く勧めます。

モジュールの末尾に、より拡張的な使用例が収められたテストセクションがあります。


.. seealso::

   プロトコルに関する記述、およびプロトコルを実装したサーバのソースとバイナリは、全てワシントン大学の *IMAP Information Center*
   (http://www.washington.edu/imap/) にあります。


.. _imap4-objects:

IMAP4 オブジェクト
------------------

全ての IMAP4rev1 命令は、同じ名前のメソッドで表されており、大文字のものも小文字のものもあります。

命令に対する引数は全て文字列に変換されます。例外は ``AUTHENTICATE`` の引数と ``APPEND`` の最後の引数で、これは IMAP4
リテラルとして渡されます。必要に応じて (IMAP4 プロトコルが感知対象としている文字が文字列に入っており、かつ丸括弧か二重引用符で囲われていなかった
場合) 文字列はクオートされます。しかし、 ``LOGIN`` 命令の  *password* 引数は常にクオートされます。文字列がクオートされないようにしたい
(例えば ``STORE`` 命令の *flags* 引数) 場合、文字列を丸括弧で囲んでください (例: ``r'(\Deleted)'``)。

各命令はタプル: ``(type, [data, ...])`` を返し、 *type* は通常 ``'OK'`` または ``'NO'`` です。
*data* は命令に対する応答をテキストにしたものか、命令に対する実行結果です。各 *data* は文字列かタプルとなります。タプルの場合、
最初の要素はレスポンスのヘッダで、次の要素にはデータが格納されます。 (ie: 'literal' value)

以下のコマンドにおける *message_set* オプションは、操作の対象となるひとつあるいは複数のメッセージを指す文字列です。単一のメッセージ番号
(``'1'``) かメッセージ番号の範囲 (``'2:4'``)、あるいは連続していないメッセージをカンマでつなげたもの (``'1:3,6:9'``)
となります。範囲指定でアスタリスクを使用すると、上限を無限とすることができます (``'3:*'``)。

:class:`IMAP4` のインスタンスは以下のメソッドを持っています:


.. method:: IMAP4.append(mailbox, flags, date_time, message)

   指定された名前のメールボックスに *message* を追加します。


.. method:: IMAP4.authenticate(mechanism, authobject)

   認証命令です --- 応答の処理が必要です。

   *mechanism* は利用する認証メカニズムを与えます。認証メカニズムはインスタンス変数 ``capabilities`` の中に
   ``AUTH=mechanism`` という形式で現れる必要があります。

   *authobject* は呼び出し可能なオブジェクトである必要があります。 ::

      data = authobject(response)

   これはサーバで継続応答を処理するためによばれます。これは(おそらく)暗号化されて、サーバへ送られた ``data`` を返します。もしクライアントが中断応答
   ``*`` を送信した場合にはこれは ``None`` を返します。


.. method:: IMAP4.check()

   サーバ上のメールボックスにチェックポイントを設定します。 Checkpoint mailbox on server.


.. method:: IMAP4.close()

   現在選択されているメールボックスを閉じます。削除されたメッセージは書き込み可能メールボックスから除去されます。 ``LOGOUT`` 前に
   実行することを勧めます。


.. method:: IMAP4.copy(message_set, new_mailbox)

   *message_set* で指定したメッセージ群を *new_mailbox* の末尾にコピーします。


.. method:: IMAP4.create(mailbox)

   *mailbox* と名づけられた新たなメールボックスを生成します。


.. method:: IMAP4.delete(mailbox)

   *mailbox* と名づけられた古いメールボックスを削除します。


.. method:: IMAP4.deleteacl(mailbox, who)

   mailbox における who についてのACLを削除(権限を削除)します。

   .. versionadded:: 2.4


.. method:: IMAP4.expunge()

   選択されたメールボックスから削除された要素を永久に除去します。各々の削除されたメッセージに対して、 ``EXPUNGE`` 応答を
   生成します。返されるデータには ``EXPUNGE`` メッセージ番号を受信した順番に並べたリストが入っています。


.. method:: IMAP4.fetch(message_set, message_parts)

   メッセージ (の一部) を取りよせます。 *message_parts* はメッセージパートの名前を表す文字列を丸括弧で囲ったもので、例えば: ``"(UID
   BODY[TEXT])"`` のようになります。返されるデータはメッセージパートのエンベロープ情報とデータからなるタプルです。


.. method:: IMAP4.getacl(mailbox)

   *mailbox* に対する ``ACL`` を取得します。このメソッドは非標準ですが、 ``Cyrus`` サーバでサポートされています。


.. method:: IMAP4.getannotation(mailbox, entry, attribute)

   *mailbox* に対する ``ANNOTATION`` を取得します。このメソッドは非標準ですが、 ``Cyrus`` サーバでサポートされています。

   .. versionadded:: 2.5


.. method:: IMAP4.getquota(root)

   ``quota`` *root* により、リソース使用状況と制限値を取得します。このメソッドは :rfc:`2087` で定義されている IMAP4
   QUOTA 拡張の一部です。

   .. versionadded:: 2.3


.. method:: IMAP4.getquotaroot(mailbox)

   *mailbox* に対して ``quota`` *root* を実行した結果のリストを取得します。このメソッドは :rfc:`2087` で定義されている
   IMAP4 QUOTA 拡張の一部です。

   .. versionadded:: 2.3


.. method:: IMAP4.list([directory[, pattern]])

   *pattern* にマッチする *directory* メールボックス名を列挙します。 *directory* の標準の設定値は最上レベルのメールフォルダで、
   *pattern* は標準の設定では全てにマッチします。返されるデータには ``LIST`` 応答のリストが入っています。


.. method:: IMAP4.login(user, password)

   平文パスワードを使ってクライアントを照合します。 *password* はクオートされます。


.. method:: IMAP4.login_cram_md5(user, password)

   パスワードの保護のため、クライアント認証時に ``CRAM-MD5`` だけを使用します。これは、 ``CAPABILITY`` レスポンスに
   ``AUTH=CRAM-MD5`` が含まれる場合のみ有効です。

   .. versionadded:: 2.3


.. method:: IMAP4.logout()

   サーバへの接続を遮断します。サーバからの ``BYE`` 応答を返します。


.. method:: IMAP4.lsub([directory[, pattern]])

   購読しているメールボックス名のうち、ディレクトリ内でパターンにマッチするものを列挙します。 *directory*
   の標準の設定値は最上レベルのメールフォルダで、 *pattern* は標準の設定では全てにマッチします。返されるデータには
   返されるデータはメッセージパートエンベロープ情報とデータからなるタプルです。


.. method:: IMAP4.myrights(mailbox)

   mailboxにおける自分のACLを返します。(すなわち自分がmailboxで持っている権限を返します。)

   .. versionadded:: 2.4


.. method:: IMAP4.namespace()

   RFC2342で定義されるIMAP名前空間を返します。

   .. versionadded:: 2.3


.. method:: IMAP4.noop()

   サーバに ``NOOP`` を送信します。


.. method:: IMAP4.open(host, port)

   *host* 上の *port* に対するソケットを開きます。
   このメソッドは :class:`IMAP4` のコンストラクタから暗黙的に呼び出されます。
   このメソッドで確立された接続オブジェクトは ``read``,
   ``readline``, ``send``, ``shutdown`` メソッドで使われます。
   このメソッドはオーバライドすることができます。


.. method:: IMAP4.partial(message_num, message_part, start, length)

   メッセージの後略された部分を取り寄せます。返されるデータはメッセージパートエンベロープ情報とデータからなるタプルです。


.. method:: IMAP4.proxyauth(user)

   *user* として認証されたものとします。認証された管理者がユーザの代理としてメールボックスにアクセスする際に使用します。

   .. versionadded:: 2.3


.. method:: IMAP4.read(size)

   遠隔のサーバから *size* バイト読み出します。このメソッドはオーバライドすることができます。


.. method:: IMAP4.readline()

   遠隔のサーバから一行読み出します。このメソッドはオーバライドすることができます。


.. method:: IMAP4.recent()

   サーバに更新を促します。新たなメッセージがない場合応答は ``None`` になり、そうでない場合 ``RECENT`` 応答の値になります。


.. method:: IMAP4.rename(oldmailbox, newmailbox)

   *oldmailbox* という名前のメールボックスを *newmailbox* に名称変更します。


.. method:: IMAP4.response(code)

   応答 *code* を受信していれば、そのデータを返し、そうでなければ ``None`` を返します。通常の形式 (usual type)
   ではなく指定したコードを返します。


.. method:: IMAP4.search(charset, criterion[, ...])

   条件に合致するメッセージをメールボックスから検索します。 *charset* は ``None`` でもよく、この場合にはサーバへの要求内に
   ``CHARSET`` は指定されません。IMAP プロトコルは少なくとも一つの条件 (criterion) が指定されるよう要求しています;
   サーバがエラーを返した場合、例外が送出されます。

   例::

      # M is a connected IMAP4 instance...
      typ, msgnums = M.search(None, 'FROM', '"LDJ"')

      # or:
      typ, msgnums = M.search(None, '(FROM "LDJ")')


.. method:: IMAP4.select([mailbox[, readonly]])

   メールボックスを選択します。返されるデータは *mailbox* 内のメッセージ数 (``EXISTS`` 応答) です。標準の設定では *mailbox*
   は ``'INBOX'`` です。 *readonly* が設定された場合、メールボックスに対する変更はできません。


.. method:: IMAP4.send(data)

   遠隔のサーバに ``data`` を送信します。このメソッドはオーバライドすることができます。


.. method:: IMAP4.setacl(mailbox, who, what)

   ``ACL`` を *mailbox* に設定します。このメソッドは非標準ですが、 ``Cyrus`` サーバでサポートされています。


.. method:: IMAP4.setannotation(mailbox, entry, attribute[, ...])

   ``ANNOTATION`` を *mailbox* に設定します。このメソッドは非標準ですが、 ``Cyrus`` サーバでサポートされています。

   .. versionadded:: 2.5


.. method:: IMAP4.setquota(root, limits)

   ``quota`` *root* のリソースを *limits* に設定します。このメソッドは :rfc:`2087` で定義されている IMAP4
   QUOTA 拡張の一部です。

   .. versionadded:: 2.3


.. method:: IMAP4.shutdown()

   ``open`` で確立された接続を閉じます。
   :meth:`IMAP4.logout` は暗黙的にこのメソッドを呼び出します。
   このメソッドはオーバライドすることができます。


.. method:: IMAP4.socket()

   サーバへの接続に使われているソケットインスタンスを返します。


.. method:: IMAP4.sort(sort_criteria, charset, search_criterion[, ...])

   ``sort`` 命令は ``search`` に結果の並べ替え (sort) 機能をつけた
   変種です。返されるデータには、条件に合致するメッセージ番号をスペースで分割したリストが入っています。 sort 命令は *search_criterium*
   の前に二つの引数を持ちます;  *sort_criteria* のリストを丸括弧で囲ったものと、検索時の *charset* です。 ``search``
   と違って、検索時の *charset* は必須です。 ``uid sort`` 命令もあり、 ``search`` に対する ``uid search``
   と同じように ``sort`` 命令に対応します。 ``sort`` 命令はまず、charset 引数の指定に従って searching criteria
   の文字列を解釈し、メールボックスから与えられた検索条件に合致するメッセージを探します。次に、合致したメッセージの数を返します。

   ``IMAP4rev1`` 拡張命令です。


.. method:: IMAP4.status(mailbox, names)

   *mailbox* の指定ステータス名の状態情報を要求します。


.. method:: IMAP4.store(message_set, command, flag_list)

   メールボックス内のメッセージ群のフラグ設定を変更します。 *command* は :rfc:`2060` のセクション 6.4.6 で指定されているもので、
   "FLAGS", "+FLAGS", あるいは "-FLAGS" のいずれかとなります。オプションで末尾に ".SILENT" がつくこともあります。

   たとえば、すべてのメッセージに削除フラグを設定するには次のようにします。 ::

      typ, data = M.search(None, 'ALL')
      for num in data[0].split():
         M.store(num, '+FLAGS', '\\Deleted')
      M.expunge()


.. method:: IMAP4.subscribe(mailbox)

   新たなメールボックスを購読 (subscribe) します。


.. method:: IMAP4.thread(threading_algorithm, charset, search_criterion[, ...])

   ``thread`` コマンドは ``search`` にスレッドの概念を加えた変形版です。
   返されるデータは空白で区切られたスレッドメンバのリストを含んでいます。

   各スレッドメンバは0以上のメッセージ番号からなり、空白で区切られており、親子関係を示しています。

   ``thread`` コマンドは *search_criterion* 引数の前に2つの引数を持っています。
   *threading_algorithm* と *charset* です。 ``search`` コマンドとは違い、 *charset* は必須です。
   ``search`` に対する ``uid search`` と同様に、 ``thread`` にも ``uid thread`` があります。

   ``thread`` コマンドはまずメールボックス中のメッセージを、charsetを用いた検索条件で検索します。その後マッチしたメッセージを指定された
   スレッドアルゴリズムでスレッド化して返します.

   これは ``IMAP4rev1`` の拡張コマンドです。

   .. versionadded:: 2.4


.. method:: IMAP4.uid(command, arg[, ...])

   command args を、メッセージ番号ではなく UID で指定されたメッセージ群に対して実行します。命令内容に応じた応答を返します。少なくとも
   一つの引数を与えなくてはなりません; 何も与えない場合、サーバはエラーを返し、例外が送出されます。


.. method:: IMAP4.unsubscribe(mailbox)

   古いメールボックスの購読を解除 (unsubscribe) します。


.. method:: IMAP4.xatom(name[, arg[, ...]])

   サーバから ``CAPABILITY`` 応答で通知された単純な拡張命令を許容 (allow) します。

:class:`IMAP4_SSL` のインスタンスは追加のメソッドを一つだけ持ちます:


.. method:: IMAP4_SSL.ssl()

   サーバへの安全な接続に使われる SSLObject インスタンスを返します。

以下の属性が :class:`IMAP4` のインスタンス上で定義されています:


.. attribute:: IMAP4.PROTOCOL_VERSION

   サーバから返された ``CAPABILITY`` 応答にある、サポートされている最新のプロトコルです。


.. attribute:: IMAP4.debug

   デバッグ出力を制御するための整数値です。初期値はモジュール変数 ``Debug`` から取られます。3 以上の値にすると各命令をトレースします。


.. _imap4-example:

IMAP4 の使用例
--------------

以下にメールボックスを開き、全てのメッセージを取得して印刷する最小の (エラーチェックをしない) 使用例を示します::

   import getpass, imaplib

   M = imaplib.IMAP4()
   M.login(getpass.getuser(), getpass.getpass())
   M.select()
   typ, data = M.search(None, 'ALL')
   for num in data[0].split():
       typ, data = M.fetch(num, '(RFC822)')
       print 'Message %s\n%s\n' % (num, data[0][1])
   M.close()
   M.logout()

