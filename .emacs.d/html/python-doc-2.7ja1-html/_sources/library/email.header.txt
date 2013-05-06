:mod:`email`: 国際化されたヘッダ
--------------------------------

.. module:: email.header
   :synopsis: 非ASCII形式のヘッダを表現する


:rfc:`2822` は電子メールメッセージの形式を規定する基本規格です。
これはほとんどの電子メールが ASCII 文字のみで構成されていたころ普及した
:rfc:`822` 標準から発展したものです。
:rfc:`2822` は電子メールがすべて 7-bit ASCII
文字のみから構成されていると仮定して作られた仕様です。

もちろん、電子メールが世界的に普及するにつれ、この仕様は国際化されてきました。
今では電子メールに言語依存の文字セットを使うことができます。
基本規格では、まだ電子メールメッセージを 7-bit ASCII 文字のみを\
使って転送するよう要求していますので、多くの RFC でどうやって非ASCII
の電子メールを :rfc:`2822` 準拠な形式にエンコードするかが\
記述されています。これらの RFC は以下のものを含みます: :rfc:`2045` 、
:rfc:`2046` 、 :rfc:`2047` 、および :rfc:`2231` 。
:mod:`email` パッケージは、 :mod:`email.header` および
:mod:`email.charset` モジュールでこれらの規格をサポートしています。

ご自分の電子メールヘッダ、たとえば :mailheader:`Subject` や :mailheader:`To`
などのフィールドに非ASCII文字を入れたい場合、
:class:`Header` クラスを使う必要があります。
:class:`~email.message.Message` オブジェクトの該当フィールドに文字列ではなく、
:class:`Header` インスタンスを使うのです。
:class:`Header` クラスは :mod:`email.header` モジュールから\
インポートしてください。たとえば::

   >>> from email.message import Message
   >>> from email.header import Header
   >>> msg = Message()
   >>> h = Header('p\xf6stal', 'iso-8859-1')
   >>> msg['Subject'] = h
   >>> print msg.as_string()
   Subject: =?iso-8859-1?q?p=F6stal?=



:mailheader:`Subject` フィールドに非ASCII文字をふくめていることに\
注目してください。ここでは、含めたいバイト列がエンコードされている\
文字セットを指定して :class:`Header` インスタンスを作成することによって\
実現しています。のちにこの :class:`~email.message.Message` インスタンスから\
フラットなテキストを生成するさいに、この :mailheader:`Subject` フィールドは :rfc:`2047`
準拠の適切な形式にエンコードされます。MIME 機能のついている\
メーラなら、このヘッダに埋めこまれた ISO-8859-1 文字をただしく表示するでしょう。

.. versionadded:: 2.2.2

以下は :class:`Header` クラスの説明です:


.. class:: Header([s[, charset[, maxlinelen[, header_name[, continuation_ws[, errors]]]]]])

   別の文字セットの文字列をふくむ MIME準拠なヘッダを作成します。

   オプション引数 *s* はヘッダの値の初期値です。
   これが ``None`` の場合 (デフォルト)、ヘッダの初期値は設定されません。
   この値はあとから :meth:`append` メソッドを呼びだすことによって\
   追加することができます。 *s* はバイト文字列か、あるいは Unicode
   文字列でもかまいません。この意味については :meth:`append` の項を参照してください。

   オプション引数 *charset* には 2つの目的があります。
   ひとつは :meth:`append` メソッドにおける *charset* 引数と同じものです。
   もうひとつの目的は、これ以降 *charset* 引数を省略した :meth:`append`
   メソッド呼び出しすべてにおける、デフォルト文字セットを決定するものです。
   コンストラクタに *charset* が与えられない場合 (デフォルト)、
   初期値の *s* および以後の :meth:`append` 呼び出しにおける文字セットとして
   ``us-ascii`` が使われます。

   行の最大長は *maxlinelen* によって明示的に指定できます。
   最初の行を (:mailheader:`Subject` などの *s* に含まれない
   フィールドヘッダの責任をとるため) 短く切りとる場合、
   *header_name* にそのフィールド名を指定してください。
   *maxlinelen* のデフォルト値は 76 であり、
   *header_name* のデフォルト値は ``None`` です。
   これはその最初の行を長い、切りとられたヘッダとして扱わないことを意味します。

   オプション引数 *continuation_ws* は :rfc:`2822` 準拠の折り返し用余白文字で、
   ふつうこれは空白か、ハードウェアタブ文字 (hard tab) である必要があります。
   ここで指定された文字は複数にわたる行の行頭に挿入されます。
   *continuation_ws* のデフォルト値は1つのスペース文字(" ")です。

   オプション引数 *errors* は、 :meth:`append` メソッドにそのまま渡されます。


   .. method:: append(s[, charset[, errors]])

      この MIME ヘッダに文字列 *s* を追加します。

      オプション引数 *charset* がもし与えられた場合、これは
      :class:`~email.charset.Charset` インスタンス (:mod:`email.charset` を参照) か、
      あるいは文字セットの名前でなければなりません。この場合は :class:`~email.charset.Charset`
      インスタンスに変換されます。この値が ``None`` の場合 (デフォルト)、
      コンストラクタで与えられた *charset* が使われます。

      *s* はバイト文字列か、Unicode 文字列です。
      これがバイト文字列 (``isinstance(s, str)`` が真) の場合、
      *charset* はその文字列のエンコーディングであり、
      これが与えられた文字セットでうまくデコードできないときは
      :exc:`UnicodeError` が発生します。

      いっぽう *s* が Unicode 文字列の場合、 *charset* はその文字列の文\
      字セットを決定するためのヒントとして使われます。この場合、
      :rfc:`2822` 準拠のヘッダは :rfc:`2047` の規則をもちいて作成され、
      Unicode 文字列は以下の文字セットを (この優先順位で) 適用してエンコー\
      ドされます: ``us-ascii`` 、 *charset* で与えられたヒント、それもなけ\
      れば ``utf-8`` 。最初の文字セットは :exc:`UnicodeError` をなるべくふ\
      せぐために使われます。

      オプション引数 *errors* は :func:`unicode` 又は :func:`ustr.encode`
      の呼び出し時に使用し、デフォルト値は "strict" です。


   .. method:: encode([splitchars])

      メッセージヘッダを RFC に沿ったやり方でエンコードします。
      おそらく長い行は折り返され、非ASCII部分は base64 または quoted-printable
      エンコーディングで包含されるでしょう。オプション引数 *splitchars*
      には長いASCII行を分割する文字の文字列を指定し、
      :rfc:`2822` の *highest level syntactic breaks* の\
      大まかなサポートの為に使用します。この引数は
      :rfc:`2047` でエンコードされた行には影響しません。

   :class:`Header` クラスは、標準の演算子や組み込み関数を\
   サポートするためのメソッドもいくつか提供しています。


   .. method:: __str__()

      :meth:`Header.encode` と同じです。 ``str(aHeader)`` などとすると有用でしょう。


   .. method:: __unicode__()

      組み込みの :func:`unicode` 関数の補助です。
      ヘッダを Unicode 文字列として返します。


   .. method:: __eq__(other)

      このメソッドは、ふたつの :class:`Header` インスタンスどうしが等しいかどうか\
      判定するのに使えます。


   .. method:: __ne__(other)

      このメソッドは、ふたつの :class:`Header` インスタンスどうしが異なっているか\
      どうかを判定するのに使えます。

さらに、 :mod:`email.header` モジュールは以下のような便宜的な関数も提供しています。


.. function:: decode_header(header)

   文字セットを変換することなしに、メッセージのヘッダをデコードします。
   ヘッダの値は *header* に渡します。

   この関数はヘッダのそれぞれのデコードされた部分ごとに、
   ``(decoded_string, charset)`` という形式の 2要素タプルからなる\
   リストを返します。 *charset* はヘッダのエンコードされていない部分に\
   対しては ``None`` を、それ以外の場合はエンコードされた文字列が\
   指定している文字セットの名前を小文字からなる文字列で返します。

   以下はこの使用例です::

      >>> from email.header import decode_header
      >>> decode_header('=?iso-8859-1?q?p=F6stal?=')
      [('p\xf6stal', 'iso-8859-1')]


.. function:: make_header(decoded_seq[, maxlinelen[, header_name[, continuation_ws]]])

   :func:`decode_header` によって返される 2要素タプルのリストから
   :class:`Header` インスタンスを作成します。

   :func:`decode_header` はヘッダの値をとってきて、
   ``(decoded_string, charset)`` という形式の 2要素タプルからなる\
   リストを返します。ここで *decoded_string* はデコードされた文字列、
   *charset* はその文字セットです。

   この関数はこれらのリストの項目から、
   :class:`Header` インスタンスを返します。オプション引数
   *maxlinelen* 、 *header_name* および *continuation_ws* は :class:`Header`
   コンストラクタに与えるものと同じです。

