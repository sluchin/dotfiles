
:mod:`nntplib` --- NNTP プロトコルクライアント
==============================================

.. module:: nntplib
   :synopsis: NNTP プロトコルクライアント (ソケットを必要とします)。


.. index::
   pair: NNTP; protocol
   single: Network News Transfer Protocol

このモジュールでは、クラス :class:`NNTP` を定義しています。このクラスは NNTP
プロトコルのクライアント側を実装しています。このモジュールを使えば、ニュースリーダや記事投稿プログラム、または自動的にニュース記事を
処理するプログラムを実装することができます。NNTP (Network News Transfer Protocol、ネットニュース転送プロトコル)
の詳細については、インターネット :rfc:`977` を参照してください。

以下にこのモジュールの使い方の小さな例を二つ示します。ニュースグループに関する統計情報を列挙し、最新 10 件の記事を出力するには以下のようにします::

   >>> s = NNTP('news.gmane.org')
   >>> resp, count, first, last, name = s.group('gmane.comp.python.committers')
   >>> print 'Group', name, 'has', count, 'articles, range', first, 'to', last
   Group gmane.comp.python.committers has 1071 articles, range 1 to 1071
   >>> resp, subs = s.xhdr('subject', first + '-' + last)
   >>> for id, sub in subs[-10:]: print id, sub
   ...
   1062 Re: Mercurial Status?
   1063 Re: [python-committers]  (Windows) buildbots on 3.x
   1064 Re: Mercurial Status?
   1065 Re: Mercurial Status?
   1066 Python 2.6.6 status
   1067 Commit Privileges for Ask Solem
   1068 Re: Commit Privileges for Ask Solem
   1069 Re: Commit Privileges for Ask Solem
   1070 Re: Commit Privileges for Ask Solem
   1071 2.6.6 rc 2
   >>> s.quit()
   '205 Bye!'

ファイルから記事を投稿するには、以下のようにします。
(この例では記事番号は有効な番号を指定していて、あなたがそのニュースグループに投稿する
権限を持っていると仮定しています) ::

   >>> s = NNTP('news.gmane.org')
   >>> f = open('/tmp/article')
   >>> s.post(f)
   '240 Article posted successfully.'
   >>> s.quit()
   '205 Bye!'

このモジュール自体では以下の内容を定義しています:


.. class:: NNTP(host[, port [, user[, password [, readermode] [, usenetrc]]]])

   ホスト *host* 上で動作し、ポート番号 *port* で要求待ちをしている NNTP サーバとの接続を表現する新たな :class:`NNTP`
   クラスのインスタンスを返します。標準の *port* は 119 です。オプションの *user* および *password* が与えられているか、
   または :file:`/.netrc` に適切な認証情報が指定されていて *usenetrc* が真 (デフォルト) の場合、 ``AUTHINFO USER``
   および ``AUTHINFO PASS`` 命令を使ってサーバに対して身元証明および認証を行います。オプションのフラグ *readermode*
   が真の場合、認証の実行に先立って ``mode reader``  命令が送信されます。reader モードは、ローカルマシン上の NNTP サーバ
   に接続していて、 ``group`` のような reader 特有の命令を呼び出したい場合に便利なことがあります。予期せず
   :exc:`NNTPPermanentError` に遭遇したなら、 *readermode* を設定する必要があるかもしれません。 *readermode*
   のデフォルト値は ``None`` です。 *usenetrc* のデフォルト値は ``True`` です。

   .. versionchanged:: 2.4
      *usenetrc* 引数を追加しました.


.. exception:: NNTPError

   標準の例外 :exc:`Exception` から派生しており、 :mod:`nntplib` モジュールが送出する全ての例外の基底クラスです。


.. exception:: NNTPReplyError

   期待はずれの応答がサーバから返された場合に送出される例外です。以前のバージョンとの互換性のために、 ``error_reply``
   はこのクラスと等価になっています。


.. exception:: NNTPTemporaryError

   エラーコードの範囲が 400-499 のエラーを受信した場合に送出される例外です。以前のバージョンとの互換性のために、 ``error_temp``
   はこのクラスと等価になっています。


.. exception:: NNTPPermanentError

   エラーコードの範囲が 500-599 のエラーを受信した場合に送出される例外です。以前のバージョンとの互換性のために、 ``error_perm``
   はこのクラスと等価になっています。


.. exception:: NNTPProtocolError

   サーバから返される応答が 1--5 の範囲の数字で始まっていない場合に送出される例外です。以前のバージョンとの互換性のために、 ``error_proto``
   はこのクラスと等価になっています。


.. exception:: NNTPDataError

   応答データ中に何らかのエラーが存在する場合に送出される例外です。以前のバージョンとの互換性のために、 ``error_data``
   はこのクラスと等価になっています。


.. _nntp-objects:

NNTP オブジェクト
-----------------

NNTP インスタンスは以下のメソッドを持っています。全てのメソッドにおける戻り値のタプルで最初の要素となる *response* は、サーバの応答です:
この文字列は 3 桁の数字からなるコードで始まります。サーバの応答がエラーを示す場合、上記のいずれかの例外が送出されます。


.. method:: NNTP.getwelcome()

   サーバに最初に接続した際に送信される応答中のウェルカムメッセージを返します。(このメッセージには時に、ユーザにとって重要な免責事項や
   ヘルプ情報が入っています。)


.. method:: NNTP.set_debuglevel(level)

   インスタンスのデバッグレベルを設定します。このメソッドは印字されるデバッグ出力の量を制御します。標準では ``0`` に設定されていて、
   これはデバッグ出力を全く印字しません。 ``1`` はそこそこの量、一般に NNTP 要求や応答あたり 1 行のデバッグ出力を生成します。値が ``2``
   やそれ以上の場合、(メッセージテキストを含めて) NNTP 接続上で送受信された全ての内容を一行ごとにログ出力する、最大限のデバッグ出力を生成します。


.. method:: NNTP.newgroups(date, time, [file])

   ``NEWSGROUPS`` 命令を送信します。 *date* 引数は ``'yymmdd'`` の形式を取り、日付を表します。 *time* 引数は
   ``'hhmmss'`` の形式をとり、時刻を表します。与えられた日付および時刻以後新たに出現したニュースグループ名のリストを *groups* として、
   ``(response, groups)`` を返します。 *file* 引数が指定されている場合、 ``NEWGROUPS`` コマンドの出力結果は
   ファイルに格納されます。 *file* が文字列の場合、この文字列をファイル名としてファイルをオープンし、書き込み後にクローズします。 *file* がファ
   イルオブジェクトの場合、オブジェクトの :meth:`write` メソッドを呼び出して出力結果を格納します。 *file* が指定されている場合は戻り値として空の
   リストを返します。


.. method:: NNTP.newnews(group, date, time, [file])

   ``NEWNEWS`` 命令を送信します。ここで、 *group* はグループ名または ``'*'`` で、 *date* および *time* は
   :meth:`newsgrups` における引数と同じ意味を持ちます。 ``(response, articles)`` からなるペアを返し、
   *articles* はメッセージ ID のリストです。 *file* 引数が指定されている場合、 ``NEWNEWS`` コマンドの出力結果は
   ファイルに格納されます。 *file* が文字列の場合、この文字列をファイル名としてファイルをオープンし、書き込み後にクローズします。 *file* がファ
   イルオブジェクトの場合、オブジェクトの :meth:`write` メソッドを呼び出して出力結果を格納します。 *file* が指定されている場合は戻り値として空の
   リストを返します。


.. method:: NNTP.list([file])

   ``LIST`` 命令を送信します。 ``(response, list)``  からなるペアを返します。 *list* はタプルからなるリストです。各タプルは
   ``(group, last, first, flag)`` の形式をとり、 *group* がグループ名、 *last* および *first*
   はそれぞれ最新および最初の記事の記事番号 (を表す文字列)、そして *flag* は投稿が可能な場合には ``'y'``, そうでない場合には
   ``'n'``, グループがモデレート (moderated) されている場合には ``'m'`` となります。(順番に注意してください: *last*,
   *first* の順です。) *file* 引数が指定されている場合、 ``LIST`` コマンドの出力結果は
   ファイルに格納されます。 *file* が文字列の場合、この文字列をファイル名としてファイルをオープンし、書き込み後にクローズします。 *file* がファ
   イルオブジェクトの場合、オブジェクトの :meth:`write` メソッドを呼び出して出力結果を格納します。 *file* が指定されている場合は戻り値として空の
   リストを返します。


.. method:: NNTP.descriptions(grouppattern)

   ``LIST NEWSGROUPS`` 命令を送信します。 *grouppattern* は RFC2980 の定義に従う wildmat 文字列です
   (実際には、 DOS や UNIX のシェルワイルドカード文字列と同じです)。 ``(response,list)`` からなるペアを返し、 *list*
   はタプル ``(name, title)`` リストになります。

   .. versionadded:: 2.4


.. method:: NNTP.description(group)

   単一のグループ *group* から説明文字列を取り出します。 ('group' が実際には wildmat 文字列で) 複数のグループがマッチした場合、
   最初にマッチしたものを返します。何もマッチしなければ空文字列を返します。

   このメソッドはサーバからの応答コードを省略します。応答コードが必要なら、 :meth:`descriptions` を使ってください。

   .. versionadded:: 2.4


.. method:: NNTP.group(name)

   ``GROUP`` 命令を送信します。 *name* はグループ名です。タプル  ``(response, count, first, last, name)``
   を返します。 *count* はグループ中の記事数 (の推定値) で、 *first* はグループ中の最初の記事番号、 *last* はグループ中の
   最新の記事番号、 *name* はグループ名です。記事番号は文字列で返されます。


.. method:: NNTP.help([file])

   ``HELP`` 命令を送信します。 ``(response, list)``  からなるペアを返します。 *list* はヘルプ文字列からなるリストです。
   *file* 引数が指定されている場合、 ``HELP`` コマンドの出力結果はファイルに格納されます。 *file* が文字列の場合、この文字列をファイル名
   としてファイルをオープンし、書き込み後にクローズします。 *file* がファイルオブジェクトの場合、オブジェクトの :meth:`write` メソッドを呼び出し
   て出力結果を格納します。 *file* が指定されている場合は戻り値として空のリストを返します。


.. method:: NNTP.stat(id)

   ``STAT`` 命令を送信します。 *id* は (``'<'`` と ``'>'`` に囲まれた形式の) メッセージ ID か、 (文字列の) 記事番号です。
   三つ組み ``(response, number, id)`` を返します。 *number* は (文字列の) 記事番号で、 *id* は (``'<'`` と
   ``'>'`` に囲まれた形式の) メッセージ ID です。


.. method:: NNTP.next()

   ``NEXT`` 命令を送信します。 :meth:`stat` のような応答を返します。


.. method:: NNTP.last()

   ``LAST`` 命令を送信します。 :meth:`stat` のような応答を返します。


.. method:: NNTP.head(id)

   ``HEAD`` 命令を送信します。 *id* は :meth:`stat` におけるのと同じ意味を持ちます。 ``(response, number, id,
   list)`` からなるタプルを返します。最初の 3 要素は :meth:`stat` と同じもので、 *list* は記事のヘッダからなるリスト
   (まだ解析されておらず、末尾の改行が取り去られたヘッダ行のリスト) です。


.. method:: NNTP.body(id,[file])

   ``BODY`` 命令を送信します。 *id* は :meth:`stat` におけるのと同じ意味を持ちます。 *file* 引数が与えられている場合、記事本体
   (body) はファイルに保存されます。 *file* が文字列の場合、このメソッドはその名前を持つファイルオブジェクトを
   開き、記事を書き込んで閉じます。 *file* がファイルオブジェクトの場合、 :meth:`write` を呼び出して記事本体を記録します。
   :meth:`head` のような戻り値を返します。 *file* が与えられていた場合、返される *list* は空のリストになります。


.. method:: NNTP.article(id)

   ``ARTICLE`` 命令を送信します。 *id* は :meth:`stat` におけるのと同じ意味を持ちます。 :meth:`head`
   のような戻り値を返します。


.. method:: NNTP.slave()

   ``SLAVE`` 命令を送信します。サーバの *response* を返します。


.. method:: NNTP.xhdr(header, string, [file])

   ``XHDR`` 命令を送信します、この命令は RFC には定義されていませんが、一般に広まっている拡張です。 *header* 引数は、例えば
   ``'subject'`` といったヘッダキーワードです。 *string* 引数は  ``'first-last'`` の形式でなければならず、ここで
   *first* および *last* は検索の対象とする記事範囲の最初と最後の記事番号です。 ``(response, list)`` のペアを返します。
   *list* は ``(id, text)`` のペアからなるリストで、 *id* が (文字列で表した) 記事番号、 *text* がその記事の
   ヘッダテキストです。 *file* 引数が指定されている場合、 ``XHDR`` コマンドの出力結果は
   ファイルに格納されます。 *file* が文字列の場合、この文字列をファイル名としてファイルをオープンし、書き込み後にクローズします。 *file* がファ
   イルオブジェクトの場合、オブジェクトの :meth:`write` メソッドを呼び出して出力結果を格納します。 *file* が指定されている場合は戻り値として空の
   リストを返します。


.. method:: NNTP.post(file)

   ``POST`` 命令を使って記事をポストします。 *file* 引数は開かれているファイルオブジェクトで、その内容は :meth:`readline`
   メソッドを使って EOF まで読み出されます。内容は必要なヘッダを含め、正しい形式のニュース記事でなければなりません。 :meth:`post` メソッドは
   ``.`` で始まる行を自動的にエスケープします。


.. method:: NNTP.ihave(id, file)

   ``IHAVE`` 命令を送信します。 *id* は (``'<'`` と ``'>'`` に囲まれた) メッセージ ID です。
   応答がエラーでない場合、 *file* を :meth:`post` と全く同じように扱います。


.. method:: NNTP.date()

   タプル ``(response, date, time)`` を返します。このタプルには :meth:`newnews` および
   :meth:`newgroups` メソッドに合った形式の、現在の日付および時刻が入っています。これはオプションの NNTP
   拡張なので、全てのサーバでサポートされているとは限りません。


.. method:: NNTP.xgtitle(name, [file])

   ``XGTITLE`` 命令を処理し、 ``(response, list)`` からなるペアを返します。 *list* は ``(name, title)``
   を含むタプルのリストです。 *file* 引数が指定されている場合、 ``XHDR`` コマンドの出力結果は
   ファイルに格納されます。 *file* が文字列の場合、この文字列をファイル名としてファイルをオープンし、書き込み後にクローズします。 *file* がファ
   イルオブジェクトの場合、オブジェクトの :meth:`write` メソッドを呼び出して出力結果を格納します。 *file* が指定されている場合は戻り値として空の
   リストを返します。これはオプションの NNTP 拡張なので、全てのサーバでサポートされているとは限りません。

   RFC2980 では、 "この拡張は撤廃すべきである" と主張しています。 :meth:`descriptions` または
   :meth:`description` を使うようにしてください。


.. method:: NNTP.xover(start, end, [file])

   ``(resp, list)`` からなるペアを返します。 *list* はタプルからなるリストで、各タプルは記事番号 *start*  および *end*
   の間に区切られた記事です。各タプルは ``(article number, subject, poster, date, id, references,
   size, lines)`` の形式をとります。 *file* 引数が指定されている場合、 ``XHDR`` コマンドの出力結果は
   ファイルに格納されます。 *file* が文字列の場合、この文字列をファイル名としてファイルをオープンし、書き込み後にクローズします。 *file* がファ
   イルオブジェクトの場合、オブジェクトの :meth:`write` メソッドを呼び出して出力結果を格納します。 *file* が指定されている場合は戻り値として空の
   リストを返します。これはオプションの NNTP 拡張なので、全てのサーバでサポートされているとは限りません。


.. method:: NNTP.xpath(id)

   ``(resp, path)`` からなるペアを返します。 *path* はメッセージ ID が *id* である記事のディレクトリパスです。
   これはオプションの NNTP 拡張なので、全てのサーバでサポートされているとは限りません。


.. method:: NNTP.quit()

   ``QUIT`` 命令を送信し、接続を閉じます。このメソッドを呼び出した後は、NTTP オブジェクトの他のいかなるメソッドも呼び出してはいけません。

