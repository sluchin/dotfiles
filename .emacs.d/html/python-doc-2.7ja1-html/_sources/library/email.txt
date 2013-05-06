:mod:`email` --- 電子メールと MIME 処理のためのパッケージ
=========================================================

.. module:: email
   :synopsis: 電子メールのメッセージを解析、操作および生成を支援するパッケージ。
    これには MIME 文書もふくまれる。
.. moduleauthor:: Barry A. Warsaw <barry@python.org>
.. sectionauthor:: Barry A. Warsaw <barry@python.org>
.. Copyright (C) 2001-2007 Python Software Foundation


.. versionadded:: 2.2

:mod:`email` パッケージは電子メールのメッセージを管理するライブラリです。
これには MIME やそれ以外の :rfc:`2822` ベースのメッセージ文書もふくまれます。
このパッケージはいくつかの古い標準パッケージ、 :mod:`rfc822`, :mod:`mimetools`,
:mod:`multifile` などにふくまれていた機能のほとんどを持ち、くわえて標準ではなかった
:mod:`mimecntl` などの機能もふくんでいます。
このパッケージは、とくに電子メールのメッセージを SMTP (:rfc:`2821`)、
NNTP、その他のサーバに送信するために作られているというわけでは *ありません* 。
それは :mod:`smtplib`, :mod:`nntplib` モジュールなどの機能です。
:mod:`email` パッケージは :rfc:`2822` に加えて、
:rfc:`2045`, :rfc:`2046`, :rfc:`2047` および :rfc:`2231` など MIME 関連の
RFC をサポートしており、できるかぎり RFC に準拠することをめざしています。

:mod:`email` パッケージの一番の特徴は、電子メールの内部表現である
*オブジェクトモデル* と、電子メールメッセージの解析および生成とを分離していることです。
:mod:`email` パッケージを使うアプリケーションは基本的にはオブジェクトを\
処理することができます。メッセージに子オブジェクトを追加したり、メッセージから\
子オブジェクトを削除したり、内容を完全に並べかえたり、といったことができます。
フラットなテキスト文書からオブジェクトモデルへの変換、またそこからフラットな文書へと\
戻す変換はそれぞれ別々の解析器 (パーサ) と生成器 (ジェネレータ) が担当しています。
また、一般的な MIME オブジェクトタイプのいくつかについては手軽なサブクラスが存在しており、\
メッセージフィールド値を抽出したり解析したり、
RFC 準拠の日付を生成したりなどのよくおこわれるタスクについてはいくつかの\
雑用ユーティリティもついています。

以下の節では :mod:`email` パッケージの機能を説明します。
説明の順序は多くのアプリケーションで一般的な使用順序にもとづいています。
まず、電子メールメッセージをファイルあるいはその他のソースからフラットな\
テキスト文書として読み込み、つぎにそのテキストを解析して電子メールの\
オブジェクト構造を作成し、その構造を操作して、最後にオブジェクトツリーを\
フラットなテキストに戻す、という順序になっています。

このオブジェクト構造は、まったくのゼロから作りだしたものであってもいっこうにかまいません。
この場合も上と似たような作業順序になるでしょう。

またここには :mod:`email` パッケージが提供するすべてのクラスおよび\
モジュールに関する説明と、 :mod:`email` パッケージを使っていくうえで\
遭遇するかもしれない例外クラス、いくつかの補助ユーティリティ、そして少々の\
サンプルも含まれています。
古い :mod:`mimelib` や前バージョンの :mod:`email` パッケージのユーザのために、
現行バージョンとの違いと移植についての節も設けてあります。

.. toctree::

   email.message.rst
   email.parser.rst
   email.generator.rst
   email.mime.rst
   email.header.rst
   email.charset.rst
   email.encoders.rst
   email.errors.rst
   email.util.rst
   email.iterators.rst
   email-examples.rst


.. seealso::

   Module :mod:`smtplib`
      SMTP プロトコルクライアント

   Module :mod:`nntplib`
      NNTP プロトコルクライアント




.. _email-pkg-history:

パッケージの履歴
----------------

このテーブルは email パッケージのリリース履歴を表しています。
それぞれのバージョンと、それが同梱された Python のバージョンとの関連が示されています。
このドキュメントでの、追加/変更されたバージョンの表記は email パッケージのバージョン
*ではなく* 、Pythonのバージョンです。
このテーブルは Python の各バージョン間の email パッケージの互換性も示しています。

+------------------+------------------------------+--------------------------+
| email バージョン | 配布                         | 互換                     |
+==================+==============================+==========================+
| :const:`1.x`     | Python 2.2.0 to Python 2.2.1 | *もうサポートされません* |
+------------------+------------------------------+--------------------------+
| :const:`2.5`     | Python 2.2.2+ and Python 2.3 | Python 2.1 から 2.5      |
+------------------+------------------------------+--------------------------+
| :const:`3.0`     | Python 2.4                   | Python 2.3 から 2.5      |
+------------------+------------------------------+--------------------------+
| :const:`4.0`     | Python 2.5                   | Python 2.3 から 2.5      |
+------------------+------------------------------+--------------------------+

以下は :mod:`email` バージョン4と3の間のおもな差分です。

* 全モジュールが :pep:`8` 標準にあわせてリネームされました。
  たとえば、version 3 でのモジュール :mod:`email.Message` は
  version 4 では :mod:`email.message` になりました。

* 新しいサブパッケージ :mod:`email.mime` が追加され、
  version 3 の :mod:`email.MIME*` は、
  :mod:`email.mime` のサブパッケージにまとめられました。
  たとえば、version 3 での :mod:`email.MIMEText` は、
  :mod:`email.mime.text` になりました。

  *Python 2.6までは version 3 の名前も有効です。*

* :mod:`email.mime.application` モジュールが追加されました。これは
  :class:`MIMEApplication` クラスを含んでいます。

* version 3 で推奨されないとされた機能は削除されました。
  これらは :meth:`Generator.__call__`,
  :meth:`Message.get_type`, :meth:`Message.get_main_type`,
  :meth:`Message.get_subtype` を含みます。

* :rfc:`2331` サポートの修正が追加されました。
  これは :func:`Message.get_param` などの関数の返り値を変更します。
  いくつかの環境では、3つ組のタプルで返されていた値が1つの文字列で返されます
  (とくに、全ての拡張パラメータセグメントがエンコードされていな\
  かった場合、予測されていた language や charset の指定がないと、
  返り値は単純な文字列になります)。過去の版では % デコードが\
  エンコードされているセグメントおよびエンコードされていないセグメントに対して行われました\
  が、エンコードされたセグメントのみで行われるようになりました。

:mod:`email` バージョン 3 とバージョン 2 との違いは以下のようなものです:

* :class:`FeedParser` クラスが新しく導入され、 :class:`Parser` クラスは :class:`FeedParser`
  を使って実装されるようになりました。このパーザは non-strict なものであり、解析はベストエフォート方式でおこなわれ
  解析中に例外を発生させることはありません。解析中に発見された問題は
  そのメッセージの *defect* (障害) 属性に保存されます。

* バージョン 2 で :exc:`DeprecationWarning` を発生していた API は
  すべて撤去されました。以下のものが含まれています:
  :class:`MIMEText` コンストラクタに渡す引数 *_encoder* 、
  :meth:`Message.add_payload` メソッド、
  :func:`Utils.dump_address_pair` 関数、そして :func:`Utils.decode` と
  :func:`Utils.encode` です。

* 新しく以下の関数が :exc:`DeprecationWarning` を発生するようになりました:
  :meth:`Generator.__call__`,
  :meth:`Message.get_type`, :meth:`Message.get_main_type`,
  :meth:`Message.get_subtype`, そして :class:`Parser` クラスに対する
  *strict* 引数です。これらは email の将来のバージョンで撤去される予定です。

* Python 2.3 以前はサポートされなくなりました。

:mod:`email` バージョン 2 とバージョン 1 との違いは以下のようなものです:

* :mod:`email.Header` モジュールおよび :mod:`email.Charset` モジュールが
  追加されています。

* :class:`Message` インスタンスの Pickle 形式が変わりました。
  が、これは正式に定義されたことは一度もないので (そしてこれからも)、
  この変更は互換性の欠如とはみなされていません。ですがもし\
  お使いのアプリケーションが :class:`Message` インスタンスを pickle あるいは
  unpickle しているなら、現在 :mod:`email` バージョン 2 では
  プライベート変数 *_charset* および
  *_default_type* を含むようになったということに注意してください。

* :class:`Message` クラス中のいくつかのメソッドは推奨されなくなったか、
  あるいは呼び出し形式が変更になっています。また、多くの新しいメソッドが\
  追加されています。詳しくは :class:`Message` クラスの文書を参照してください。
  これらの変更は完全に下位互換になっているはずです。

* :mimetype:`message/rfc822` 形式のコンテナは、
  見た目上のオブジェクト構造が変わりました。
  :mod:`email` バージョン 1 では
  この content type はスカラー形式のペイロードとして表現されていました。
  つまり、コンテナメッセージの
  :meth:`is_multipart` は false を返し、
  :meth:`get_payload` はリストオブジェクトではなく単一の
  :class:`Message` インスタンスを直接返すようになっていたのです。

  この構造はパッケージ中のほかの部分と整合がとれていなかったため、
  :mimetype:`message/rfc822` 形式のオブジェクト表現形式が
  変更されました。 :mod:`email` バージョン 2 では、コンテナは
  :meth:`is_multipart` に *True を返し* ます。また
  :meth:`get_payload` はひとつの :class:`Message` インスタンスを
  要素とするリストを返すようになりました。

  注意: ここは下位互換が完全には成りたたなくなっている部分のひとつです。
  けれどもあらかじめ :meth:`get_payload` が返すタイプをチェックするように\
  なっていれば問題にはなりません。ただ :mimetype:`message/rfc822` 形式のコンテナを
  :class:`Message` インスタンスにじかに :meth:`set_payload`
  しないようにさえすればよいのです。

* :class:`Parser` コンストラクタに *strict* 引数が追加され、
  :meth:`parse` および :meth:`parsestr`
  メソッドには *headersonly* 引数がつきました。
  *strict* フラグはまた :func:`email.message_from_file` と
  :func:`email.message_from_string` にも追加されています。

* :meth:`Generator.__call__` はもはや推奨されなくなりました。
  かわりに :meth:`Generator.flatten` を使ってください。また、
  :class:`Generator` クラスには :meth:`clone` メソッドが追加されています。

* :mod:`email.generator` モジュールに :class:`DecodedGenerator` クラスが\
  加わりました。

* 中間的な基底クラスである :class:`MIMENonMultipart` および :class:`MIMEMultipart`
  がクラス階層の中に追加され、ほとんどの MIME 関係の派生クラスがこれを介するようになっています。

* :class:`MIMEText` コンストラクタの *_encoder* 引数は推奨されなくなりました。
  いまやエンコーダは *_charset* 引数にもとづいて暗黙のうちに決定されます。

* :mod:`email.utils` モジュールにおける以下の関数は推奨されなくなりました:
  :func:`dump_address_pairs`, :func:`decode`, および :func:`encode` 。
  また、このモジュールには以下の関数が追加されています:
  :func:`make_msgid`, :func:`decode_rfc2231`, :func:`encode_rfc2231` そして
  :func:`decode_params` 。

* Public ではない関数 :func:`email.iterators._structure` が追加されました。


:mod:`mimelib` との違い
-----------------------

:mod:`email` パッケージはもともと `mimelib <http://mimelib.sf.net/>`_ と
呼ばれる個別のライブラリからつくられたものです。その後変更が加えられ、
メソッド名がより一貫したものになり、いくつかのメソッドやモジュールが\
加えられたりはずされたりしました。いくつかのメソッドでは、
その意味も変更されています。しかしほとんどの部分において、 :mod:`mimelib`
パッケージで使うことのできた機能は、ときどきその方法が変わってはいるものの
:mod:`email` パッケージでも使用可能です。 :mod:`mimelib`
パッケージと :mod:`email` パッケージの間の下位互換性はあまり優先はされませんでした。

以下では :mod:`mimelib` パッケージと :mod:`email` パッケージにおける\
違いを簡単に説明し、それに沿ってアプリケーションを移植するさいの\
指針を述べています。

おそらく 2つのパッケージのもっとも明らかな違いは、パッケージ名が
:mod:`email` に変更されたことでしょう。
さらにトップレベルのパッケージが以下のように変更されました:

* :func:`messageFromString` は :func:`message_from_string` に名前が変更されました。

* :func:`messageFromFile` は :func:`message_from_file` に名前が変更されました。

:class:`Message` クラスでは、以下のような違いがあります:

* :meth:`asString` メソッドは :meth:`as_string` に名前が変更されました。

* :meth:`ismultipart` メソッドは :meth:`is_multipart` に名前が変更されました。

* :meth:`get_payload` メソッドはオプション引数として *decode* をとるようになりました。

* :meth:`getall` メソッドは :meth:`get_all` に名前が変更されました。

* :meth:`addheader` メソッドは :meth:`add_header` に名前が変更されました。

* :meth:`gettype` メソッドは :meth:`get_type` に名前が変更されました。

* :meth:`getmaintype` メソッドは :meth:`get_main_type` に名前が変更されました。

* :meth:`getsubtype` メソッドは :meth:`get_subtype` に名前が変更されました。

* :meth:`getparams` メソッドは :meth:`get_params` に名前が変更されました。
  また、従来の :meth:`getparams` は文字列のリストを返していましたが、
  :meth:`get_params` は 2-タプルのリストを返すようになっています。
  これはそのパラメータのキーと値の組が、 ``'='`` 記号によって分離されたものです。

* :meth:`getparam` メソッドは :meth:`get_param`.

* :meth:`getcharsets` メソッドは :meth:`get_charsets` に名前が変更されました。

* :meth:`getfilename` メソッドは :meth:`get_filename` に名前が変更されました。

* :meth:`getboundary` メソッドは :meth:`get_boundary` に名前が変更されました。

* :meth:`setboundary` メソッドは :meth:`set_boundary` に名前が変更されました。

* :meth:`getdecodedpayload` メソッドは廃止されました。
  これと同様の機能は :meth:`get_payload` メソッドの
  *decode* フラグに 1 を渡すことで実現できます。

* :meth:`getpayloadastext` メソッドは廃止されました。
  これと同様の機能は :mod:`email.Generator` モジュールの
  :class:`DecodedGenerator` クラスによって提供されます。

* :meth:`getbodyastext` メソッドは廃止されました。
  これと同様の機能は :mod:`email.iterators` モジュールにある
  :func:`typed_subpart_iterator` を使ってイテレータを作ることにより実現できます。

:class:`Parser` クラスは、その public なインターフェイスは変わっていませんが、
これはより一層かしこくなって :mimetype:`message/delivery-status` 形式のメッセージを\
認識するようになりました。これは配送状態通知  [#]_
において、各ヘッダブロックを表す独立した :class:`Message` パートを含む\
ひとつの :class:`Message` インスタンスとして表現されます。

:class:`Generator` クラスは、その public なインターフェイスは変わっていませんが、
:mod:`email.generator` モジュールに新しいクラスが加わりました。
:class:`DecodedGenerator` と呼ばれるこのクラスは以前
:meth:`Message.getpayloadastext` メソッドで使われていた\
機能のほとんどを提供します。

また、以下のモジュールおよびクラスが変更されています:

* :class:`MIMEBase` クラスのコンストラクタ引数 *_major* と *_minor* は、
  それぞれ *_maintype* と *_subtype* に変更されています。

* ``Image`` クラスおよびモジュールは ``MIMEImage`` に名前が変更されました。
  *_minor* 引数も *_subtype* に名前が変更されています。

* ``Text`` クラスおよびモジュールは ``MIMEText`` に名前が変更されました。
  *_minor* 引数も *_subtype* に名前が変更されています。

* ``MessageRFC822`` クラスおよびモジュールは ``MIMEMessage`` に名前が変更されました。
  注意: 従来バージョンの :mod:`mimelib` では、このクラスおよびモジュールは
  ``RFC822`` という名前でしたが、これは大文字小文字を区別しないファイルシステムでは
  Python の標準ライブラリモジュール :mod:`rfc822` と名前がかち合ってしまっていました。

  また、 :class:`MIMEMessage` クラスはいまや :mimetype:`message`
  main type をもつあらゆる種類の MIME メッセージを表現できるようになりました。
  これはオプション引数として、MIME subtype を指定する *_subtype* 引数をとることができる\
  ようになっています。デフォルトでは、 *_subtype* は :mimetype:`rfc822` になります。

:mod:`mimelib` では、 :mod:`address` および :mod:`date` モジュールで\
いくつかのユーティリティ関数が提供されていました。これらの関数はすべて
:mod:`email.utils` モジュールの中に移されています。

``MsgReader`` クラスおよびモジュールは廃止されました。
これにもっとも近い機能は :mod:`email.iterators` モジュール中の
:func:`body_line_iterator` 関数によって提供されています。


.. rubric:: 注記

.. [#] 配送状態通知 (Delivery Status Notifications, DSN) は :rfc:`1894` によって定義されています。

