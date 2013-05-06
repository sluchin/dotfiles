:mod:`email`: 例外及び障害クラス
--------------------------------

.. module:: email.errors
   :synopsis: email パッケージで使われる例外クラス


:mod:`email.errors` モジュールでは、以下の例外クラスが定義されています:


.. exception:: MessageError()

   これは :mod:`email` パッケージが発生しうるすべての例外の基底クラスです。
   これは標準の :exc:`Exception` クラスから派生しており、
   追加のメソッドはまったく定義されていません。


.. exception:: MessageParseError()

   これは :class:`~email.parser.Parser` クラスが発生しうる例外の基底クラスです。
   :exc:`MessageError` から派生しています。


.. exception:: HeaderParseError()

   メッセージの :rfc:`2822` ヘッダを解析している途中にある条件でエラーがおこると\
   発生します。これは :exc:`MessageParseError` から派生しています。
   この例外が起こる可能性があるのは :meth:`Parser.parse` メソッドと :meth:`Parser.parsestr`
   メソッドです。

   この例外が発生するのはメッセージ中で最初の :rfc:`2822` ヘッダが現れたあとに\
   エンベロープヘッダが見つかったとか、最初の :rfc:`2822`
   ヘッダが現れる前に前のヘッダからの継続行が見つかったとかいう状況を含みます。
   あるいはヘッダでも継続行でもない行がヘッダ中に見つかった場合でも\
   この例外が発生します。


.. exception:: BoundaryError()

   メッセージの :rfc:`2822` ヘッダを解析している途中にある条件でエラーがおこると\
   発生します。これは :exc:`MessageParseError` から派生しています。
   この例外が起こる可能性があるのは :meth:`Parser.parse` メソッドと
   :meth:`Parser.parsestr` メソッドです。

   この例外が発生するのは、厳格なパーズ方式が用いられているときに、
   :mimetype:`multipart/*`
   形式の開始あるいは終了の文字列が見つからなかった場合などです。


.. exception:: MultipartConversionError()

   この例外は、 :class:`Message` オブジェクトに :meth:`add_payload` メソッドを使って
   ペイロードを追加するとき、そのペイロードがすでに単一の値である
   (訳注: リストでない) にもかかわらず、そのメッセージの
   :mailheader:`Content-Type` ヘッダのメインタイプがすでに設定されていて、
   それが :mimetype:`multipart` 以外になってしまっている場合にこの例外が発生します。
   :exc:`MultipartConversionError` は
   :exc:`MessageError` と組み込みの :exc:`TypeError` を両方継承しています。

   :meth:`Message.add_payload` はもはや推奨されないメソッドのため、
   この例外はめったに発生しません。しかしこの例外は
   :meth:`attach` メソッドが :class:`~email.mime.nonmultipart.MIMENonMultipart`
   から派生したクラスのインスタンス (例: :class:`~email.mime.image.MIMEImage`
   など) に対して呼ばれたときにも発生することがあります。

以下は :class:`~email.mime.FeedParser` がメッセージの解析中に検出する障害
(defect) の一覧です。
これらの障害は、問題が見つかったメッセージに追加されるため、たとえば
:mimetype:`multipart/alternative`
内にあるネストしたメッセージが異常なヘッダをもっていた場合には、
そのネストしたメッセージが障害を持っているが、その親メッセージには障害はないと
みなされることに注意してください。

すべての障害クラスは :class:`email.errors.MessageDefect` のサブクラスですが、
これは例外とは *違います* ので注意してください。

.. versionadded:: 2.4
   全ての障害クラスが追加された。

* :class:`NoBoundaryInMultipartDefect` -- メッセージが multipart だと宣言されているのに、
  :mimetype:`boundary` パラメータがない。

* :class:`StartBoundaryNotFoundDefect` -- :mailheader:`Content-Type` ヘッダで宣言された
  開始境界がない。

* :class:`FirstHeaderLineIsContinuationDefect` -- メッセージの最初のヘッダが\
  継続行から始まっている。

* :class:`MisplacedEnvelopeHeaderDefect` -- ヘッダブロックの途中に "Unix From" ヘッダがある。

* :class:`MalformedHeaderDefect` -- コロンのないヘッダがある、あるいはそれ以外の異常なヘッダである。

* :class:`MultipartInvariantViolationDefect` -- メッセージが :mimetype:`multipart`
  だと宣言されているのに、サブパートが存在しない。
  注意: メッセージがこの障害を持っているとき、 :meth:`is_multipart` メソッドは\
  たとえその content-type が :mimetype:`multipart` であっても false を返すこと\
  があります。

