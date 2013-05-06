:mod:`poplib` --- POP3 プロトコルクライアント
=============================================

.. module:: poplib
   :synopsis: POP3 プロトコルクライアント (socketsを必要とする)
.. sectionauthor:: Andrew T. Csillag
.. revised by ESR, January 2000

.. index:: pair: POP3; protocol

このモジュールは、 :class:`POP3` クラスを定義します。これはPOP3サーバへの接続と、 :rfc:`1725`
に定められたプロトコルを実装します。 :class:`POP3` クラスは minimalとoptinalという2つのコマンドセットをサポートします。
モジュールは :class:`POP3_SSL` クラスも提供します。このクラスは下位のプロトコルレイヤーにSSLを使ったPOP3サーバへの接続を提供します。

POP3についての注意事項は、それが広くサポートされているにもかかわらず、既に時代遅れだということです。幾つも実装されているPOP3サーバーの品質は、
貧弱なものが多数を占めています。もし、お使いのメールサーバーがIMAPをサポートしているなら、 ``imaplib や IMAP4`` が使えます。
IMAPサーバーは、より良く実装されている傾向があります。

:mod:`poplib`  モジュールでは、ひとつのクラスが提供されています。


.. class:: POP3(host[, port[, timeout]])

   このクラスが、実際にPOP3プロトコルを実装します。インスタンスが初期化されるときに、コネクションが作成されます。 *port*
   が省略されると、POP3標準のポート(110)が使われます。
   オプションの *timeout* 引数は、接続時のタイムアウト時間を秒数で指定します。
   (指定されなかった場合は、グローバルのデフォルトタイムアウト設定が利用されます。)

   .. versionchanged:: 2.6
      *timeout* が追加されました


.. class:: POP3_SSL(host[, port[, keyfile[, certfile]]])

   :class:`POP3` クラスのサブクラスで、SSLでカプセル化されたソケットによる POPサーバへの接続を提供します。 *port*
   が指定されていない場合、 POP3-over-SSL標準の995番ポートが使われます。 *keyfile* と *certfile* もオプションで -
   SSL接続に使われる PEMフォーマットの秘密鍵と信頼された＃＃を含みます。

   .. versionadded:: 2.4

1つの例外が、 :mod:`poplib` モジュールのアトリビュートとして定義されています。


.. exception:: error_proto

   例外は、このモジュール内で起こったすべてのエラーで発生します。(:mod:`socket`
   モジュールからのエラーは捕まえず、そのまま伝播します)
   例外の理由は文字列としてコンストラクタに渡されます。


.. seealso::

   Module :mod:`imaplib`
      The standard Python IMAP module.

   `Frequently Asked Questions About Fetchmail <http://www.catb.org/~esr/fetchmail/fetchmail-FAQ.html>`_
      POP/IMAPクライアント :program:`fetchmail` のFAQ。POPプロトコルを
      ベースにしたアプリケーションを書くときに有用な、POP3サーバの種類や RFCへの適合度といった情報を収集しています。


.. _pop3-objects:

POP3 オブジェクト
-----------------

POP3コマンドはすべて、それと同じ名前のメソッドとしてlower-caseで表現されます。そしてそのほとんどは、サーバからのレスポンスとなる
テキストを返します。

:class:`POP3` クラスのインスタンスは以下のメソッドを持ちます。


.. method:: POP3.set_debuglevel(level)

   インスタンスのデバッグレベルを指定します。これはデバッギングアウトプットの表示量をコントロールします。デフォルト値の ``0`` は、デバッギング
   アウトプットを表示しません。値を ``1`` とすると、デバッギングアウトプットの表示量を適当な量にします。これは大体、リクエストごと1行になります。値を
   ``2`` 以上にすると、デバッギングアウトプットの表示量を最大にします。コントロール中の接続で送受信される各行をログに出力します。


.. method:: POP3.getwelcome()

   POP3サーバーから送られるグリーティングメッセージを返します。


.. method:: POP3.user(username)

   userコマンドを送出します。応答はパスワード要求を表示します。


.. method:: POP3.pass_(password)

   パスワードを送出します。応答は、メッセージ数とメールボックスのサイズを含みます。注：サーバー上のメールボックスは :meth:`quit`
   が呼ばれるまでロックされます。


.. method:: POP3.apop(user, secret)

   POP3サーバーにログオンするのに、よりセキュアなAPOP認証を使用します。


.. method:: POP3.rpop(user)

   POP3サーバーにログオンするのに、（UNIXのr-コマンドと同様の）RPOP認証を使用します。


.. method:: POP3.stat()

   メールボックスの状態を得ます。結果は2つのintegerからなるタプルとなります。 ``(message count, mailbox size)``.


.. method:: POP3.list([which])

   メッセージのリストを要求します。結果は ``(response, ['mesg_num octets', ...], octets)``
   という形式で表されます。
   *which* が与えられると、それによりメッセージを指定します。


.. method:: POP3.retr(which)

   *which* 番のメッセージ全体を取り出し、そのメッセージに既読フラグを立てます。結果は ``(response, ['line', ...],
   octets)`` という形式で表されます。


.. method:: POP3.dele(which)

   *which* 番のメッセージに削除のためのフラグを立てます。ほとんどのサーバで、QUITコマンドが実行されるまでは実際の削除は行われません
   （もっとも良く知られた例外は Eudora QPOPで、その配送メカニズムはRFCに違反しており、どんな切断状況でも削除操作を未解決にしています）。


.. method:: POP3.rset()

   メールボックスの削除マークすべてを取り消します。


.. method:: POP3.noop()

   何もしません。接続保持のために使われます。


.. method:: POP3.quit()

   Signoff:  commit changes, unlock mailbox, drop connection.
   サインオフ：変更をコミットし、メールボックスをアンロックして、接続を破棄します。


.. method:: POP3.top(which, howmuch)

   メッセージヘッダと *howmuch* で指定した行数のメッセージを、 *which* で指定したメッセージ分取り出します。結果は以下のような形式となります。
   ``(response, ['line', ...], octets)``.

   このメソッドはPOP3のTOPコマンドを利用し、RETRコマンドのように、メッセージに
   既読フラグをセットしません。残念ながら、TOPコマンドはRFCでは貧弱な仕様しか定義されておらず、しばしばノーブランドのサーバーでは（その仕様が）守られて
   いません。このメソッドを信用してしまう前に、実際に使用するPOPサーバーでテストをしてください。


.. method:: POP3.uidl([which])

   （ユニークIDによる）メッセージダイジェストのリストを返します。 *which* が設定されている場合、結果はユニークIDを含みます。それは
   ``'response mesgnum uid`` という形式のメッセージ、または
   ``(response, ['mesgnum uid', ...], octets)`` という形式のリストとなります。

:class:`POP3_SSL` クラスのインスタンスは追加のメソッドを持ちません。このサブクラスのインターフェイスは親クラスと同じです。


.. _pop3-example:

POP3 の例
---------

これは（エラーチェックもない）最も小さなサンプルで、メールボックスを開いて、すべてのメッセージを取り出し、プリントします。 ::

   import getpass, poplib

   M = poplib.POP3('localhost')
   M.user(getpass.getuser())
   M.pass_(getpass.getpass())
   numMessages = len(M.list()[1])
   for i in range(numMessages):
       for j in M.retr(i+1)[1]:
           print j

モジュールの末尾に、より広い範囲の使用例となるtestセクションがあります。

