:mod:`email`: MIME 文書を生成する
---------------------------------

.. module:: email.generator
   :synopsis: メッセージ構造からフラットな電子メールテキストを生成する。


よくある作業のひとつは、メッセージオブジェクト構造から\
フラットな電子メールテキストを生成することです。この作業は :mod:`smtplib` や
:mod:`nntplib` モジュールを使って\
メッセージを送信したり、メッセージをコンソールに出力したりするときに\
必要になります。あるメッセージオブジェクト構造をとってきて、
そこからフラットなテキスト文書を生成するのは :class:`Generator`
クラスの仕事です。

繰り返しになりますが、 :mod:`email.parser` モジュールと同じく、
この機能は既存の Generator だけに限られるわけではありません。
これらはご自身でゼロから作りあげることもできます。
しかし、既存のジェネレータはほとんどの電子メールを標準に沿ったやり方で\
生成する方法を知っていますし、MIME メッセージも非 MIME メッセージも\
扱えます。さらにこれはフラットなテキストから :class:`~email.parser.Parser`
クラスを使ってメッセージ構造に変換し、それをまたフラットなテキストに戻しても、
結果が冪等 [#]_ になるよう設計されています。
一方で、プログラムによって構成された :class:`~email.message.Message`
の Generator を使う場合、デフォルトの挿入によって :class:`~email.message.Message`
オブジェクトを変えてしまうかもしれません。

:mod:`email.generator` モジュールからインポートされる :class:`Generator`
クラスで公開されているメソッドには、以下のようなものがあります:


.. class:: Generator(outfp[, mangle_from_[, maxheaderlen]])

   :class:`Generator` クラスのコンストラクタは *outfp* と呼ばれる
   ストリーム形式  [#]_ のオブジェクトひとつを引数にとります。
   *outfp* は :meth:`write` メソッドをサポートし、 Python 拡張 print
   文の出力ファイルとして使えるようになっている必要があります。

   オプション引数 *mangle_from_* はフラグで、
   ``True`` のときはメッセージ本体に現れる行頭のすべての ``From``
   という文字列の最初に ``>`` という文字を追加します。これは、このような行が
   Unix の mailbox 形式のエンペローブヘッダ区切り文字列として誤認識されるの\
   を防ぐための、移植性ある唯一の方法です (詳しくは `WHY THE CONTENT-LENGTH
   FORMAT IS BAD (なぜ Content-Length 形式が有害か)
   <http://www.jwz.org/doc/content-length.html>`_
   を参照してください)。デフォルトでは *mangle_from_* は ``True`` になっていますが、
   Unix の mailbox 形式ファイルに出力しないのならば\
   これは ``False`` に設定してもかまいません。

   オプション引数 *maxheaderlen* は連続していないヘッダの最大長を
   指定します。ひとつのヘッダ行が *maxheaderlen*
   (これは文字数です、tab は空白 8文字に展開されます) よりも長い場合、
   ヘッダは :class:`~email.header.Header` クラスで定義されているように途中で
   折り返され、間にはセミコロンが挿入されます。
   もしセミコロンが見つからない場合、そのヘッダは放置されます。
   ヘッダの折り返しを禁止するにはこの値にゼロを指定してください。
   デフォルトは 78 文字で、 :rfc:`2822` で推奨されている
   (ですが強制ではありません) 値です。

  これ以外のパブリックな :class:`Generator` メソッドは以下のとおりです:


  .. method:: flatten(msg[, unixfrom])

      *msg* を基点とするメッセージオブジェクト構造体の\
      文字表現を出力します。出力先のファイルにはこの :class:`Generator` インスタンスが\
      作成されたときに指定されたものが使われます。各 subpart は深さ優先順序
      (depth-first) で出力され、得られるテキストは適切に MIME
      エンコードされたものになっています。

      オプション引数 *unixfrom* は、基点となるメッセージオブジェクトの\
      最初の :rfc:`2822` ヘッダが現れる前に、エンペローブヘッダ区切り文字列を\
      出力することを強制するフラグです。そのメッセージオブジェクトが\
      エンペローブヘッダをもたない場合、標準的なエンペローブヘッダが自動的に\
      作成されます。デフォルトではこの値は ``False`` に設定されており、
      エンペローブヘッダ区切り文字列は出力されません。

      注意: 各 subpart に関しては、エンペローブヘッダは出力されません。

      .. versionadded:: 2.2.2


  .. method:: clone(fp)

      この :class:`Generator` インスタンスの独立したクローンを生成し返します。
      オプションはすべて同一になっています。

      .. versionadded:: 2.2.2


  .. method:: write(s)

      文字列 *s* を既定のファイルに出力します。
      ここでいう出力先は :class:`Generator` コンストラクタに渡した *outfp*
      のことをさします。この関数はただ単に\
      拡張 print 文で使われる :class:`Generator` インスタンスに対して\
      ファイル操作風の API を提供するためだけのものです。

ユーザの便宜をはかるため、メソッド :meth:`Message.as_string` と
``str(aMessage)`` (つまり :meth:`Message.__str__` のことです) をつかえば\
メッセージオブジェクトを特定の書式でフォーマットされた文字列に簡単に変換\
することができます。
詳細は :mod:`email.message` を参照してください。

:mod:`email.generator` モジュールはひとつの派生クラスも提供しています。
これは :class:`DecodedGenerator` と呼ばれるもので、
:class:`Generator` 基底クラスと似ていますが、非 :mimetype:`text` 型の subpart
を特定の書式でフォーマットされた表現形式で置きかえるところが違っています。


.. class:: DecodedGenerator(outfp[, mangle_from_[, maxheaderlen[, fmt]]])

   このクラスは :class:`Generator` から派生したもので、メッセージの
   subpart をすべて渡り歩きます。subpart の主形式が :mimetype:`text`
   だった場合、これはその subpart のペイロードをデコードして出力します。
   オプション引数 *_mangle_from_* および *maxheaderlen* の意味は基底\
   クラス :class:`Generator` のそれと同じです。

   Subpart の主形式が :mimetype:`text` ではない場合、オプション引数 *fmt*
   がそのメッセージペイロードのかわりのフォーマット文字列として使われます。 *fmt* は ``%(keyword)s`` のような形式を展開し、
   以下のキーワードを認識します:

  * ``type`` -- 非 :mimetype:`text` 型 subpart の MIME 形式

  * ``maintype`` -- 非 :mimetype:`text` 型 subpart の MIME 主形式 (maintype)

  * ``subtype`` -- 非 :mimetype:`text` 型 subpart の MIME 副形式 (subtype)

  * ``filename`` -- 非 :mimetype:`text` 型 subpart のファイル名

  * ``description`` -- 非 :mimetype:`text` 型 subpart につけられた説明文字列

  * ``encoding`` -- 非 :mimetype:`text` 型 subpart の Content-transfer-encoding

   *fmt* のデフォルト値は ``None`` です。こうすると以下の形式で出力します::

      [Non-text (%(type)s) part of message omitted, filename %(filename)s]

   .. versionadded:: 2.2.2

.. versionchanged:: 2.5
   以前の非推奨メソッド :meth:`__call__` は削除されました。

.. rubric:: 注記

.. [#] 訳注: idempotent、その操作を何回くり返しても 1回だけ行ったのと\
       結果が同じになること。

.. [#] 訳注: file-like object

