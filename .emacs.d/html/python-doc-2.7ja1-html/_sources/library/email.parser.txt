:mod:`email`: 電子メールメッセージを解析(パース)する
----------------------------------------------------

.. module:: email.parser
   :synopsis: 電子メールメッセージのフラットなテキストを解析し、
    メッセージオブジェクト構造体を生成する。


メッセージオブジェクト構造体をつくるには 2つの方法があります。
ひとつはまったくのスクラッチから :class:`~email.message.Message` を生成して、これを
:meth:`attach` と :meth:`set_payload` 呼び出しを介してつなげていく方法で、
もうひとつは電子メールメッセージのフラットなテキスト表現を\
解析 (parse、パーズ) する方法です。

:mod:`email` パッケージでは、MIME 文書をふくむ、
ほとんどの電子メールの文書構造に対応できる標準的なパーザ (解析器) を提供しています。
このパーザに文字列あるいはファイルオブジェクトを渡せば、パーザは\
そのオブジェクト構造の基底となる (root の) :class:`~email.message.Message`
インスタンスを返します。
簡単な非MIMEメッセージであれば、この基底オブジェクトのペイロードは\
たんにメッセージのテキストを格納する文字列になるでしょう。
MIMEメッセージであれば、基底オブジェクトはその :meth:`is_multipart`
メソッドに対して ``True`` を返します。
そして、その各 subpart に :meth:`get_payload` メソッドおよび
:meth:`walk` メソッドを介してアクセスすることができます。

実際には 2つのパーザインターフェイスが使用可能です。
ひとつは旧式の :class:`Parser` API であり、もうひとつはインクリメンタルな
:class:`FeedParser` API です。
旧式の :class:`Parser` API はメッセージ全体のテキストが文字列としてすでに\
メモリ上にあるか、それがローカルなファイルシステム上に存在しているときには\
問題ありません。 :class:`FeedParser` はメッセージを読み込むときに、そのストリームが\
入力待ちのためにブロックされるような場合 (ソケットから email メッセージを読み込む時など)
に、より有効です。 :class:`FeedParser` はインクリメンタルにメッセージを読み込み、
解析します。パーザを close したときには根っこ
(root) のオブジェクトのみが返されます [#]_ 。

このパーザは、ある制限された方法で拡張できます。
また、もちろん自分でご自分のパーザを完全に無から実装することもできます。 :mod:`email`
パッケージについているパーザと :class:`~email.message.Message` クラスの間に\
隠された秘密の関係はなにもありませんので、ご自分で実装されたパーザも、
それが必要とするやりかたでメッセージオブジェクトツリーを作成することができます。


FeedParser API
^^^^^^^^^^^^^^

.. versionadded:: 2.4

:mod:`email.feedparser` モジュールからインポートされる :class:`FeedParser` は email
メッセージをインクリメンタルに解析するのに向いた API を提供します。
これは email メッセージのテキストを (ソケットなどの) 読み込みがブロックされる\
可能性のある情報源から入力するときに必要となります。もちろん :class:`FeedParser` は
文字列またはファイルにすべて格納されている email メッセージを解析するのにも\
使うことができますが、このような場合には旧式の :class:`Parser` API のほうが\
便利かもしれません。これら 2つのパーザ API の意味論と得られる結果は同一です。

:class:`FeedParser` API は簡単です。まずインスタンスをつくり、それに\
テキストを (それ以上テキストが必要なくなるまで) 流しこみます。その後\
パーザを close すると根っこ (root) のメッセージオブジェクトが返されます。
標準に従ったメッセージを解析する場合、 :class:`FeedParser` は非常に\
正確であり、標準に従っていないメッセージでもちゃんと動きます。
そのさい、これはメッセージがどのように壊れていると認識されたかについての\
情報を残します。これはメッセージオブジェクトの *defects* 属性に\
リストとして現れ、メッセージ中に発見された問題が記録されます。
パーザが検出できる障害 (defect) については :mod:`email.errors`
モジュールを参照してください。

以下は :class:`FeedParser` の API です:


.. class:: FeedParser([_factory])

   :class:`FeedParser` インスタンスを作成します。オプション引数 *_factory* には\
   引数なしの callable を指定し、これはつねに新しいメッセージオブジェクトの作成が\
   必要になったときに呼び出されます。デフォルトでは、これは
   :class:`email.message.Message` クラスになっています。


   .. method:: feed(data)

      :class:`FeedParser` にデータを供給します。 *data* は 1行または複数行からなる\
      文字列を渡します。渡される行は完結していなくてもよく、その場合 :class:`FeedParser`
      は部分的な行を適切につなぎ合わせます。文字列中の各行は標準的な 3種類の\
      行末文字 (復帰 CR、改行 LF、または CR+LF)
      どれかの組み合わせでよく、これらが混在してもかまいません。


   .. method:: FeedParser.close()

      :class:`FeedParser` を close し、それまでに渡されたすべてのデータの\
      解析を完了させ根っこ (root) のメッセージオブジェクトを返します。
      :class:`FeedParser` を close したあとにさらにデータを feed した場合の\
      挙動は未定義です。


Parser クラス API
^^^^^^^^^^^^^^^^^

:mod:`email.parser` モジュールからインポートされる :class:`Parser` クラ\
スは、メッセージを表すテキストが文字列またはファイルの形で\
完全に使用可能なときメッセージを解析するのに使われる API を提供します。
:mod:`email.Parser` モジュールはまた、 :class:`HeaderParser` と呼ばれる\
2番目のクラスも提供しています。これはメッセージのヘッダのみを処理したい場合に\
使うことができ、ずっと高速な処理がおこなえます。なぜならこれはメッセージ本体を\
解析しようとはしないからです。かわりに、そのペイロードにはメッセージ本体の\
生の文字列が格納されます。 :class:`HeaderParser` クラスは
:class:`Parser` クラスと同じ API をもっています。


.. class:: Parser([_class])

   :class:`Parser` クラスのコンストラクタです。
   オプション引数 *_class* をとることができます。
   これは呼び出し可能なオブジェクト (関数やクラス) でなければならず、
   メッセージ内コンポーネント (sub-message object) が作成されるときは\
   常にそのファクトリクラスとして使用されます。
   デフォルトではこれは :class:`~email.message.Message` になっています
   (:mod:`email.message` 参照)。このファクトリクラスは引数なしで呼び出されます。

   オプション引数 *strict* は無視されます。

   .. deprecated:: 2.4
      :class:`Parser` は Python 2.4 で新しく導入された :class:`FeedParser` の\
      後方互換性のための API ラッパで、 *すべての* 解析が事実上 non-strict です。
      :class:`Parser` コンストラクタに *strict* フラグを渡す必要はありません。

   .. versionchanged:: 2.2.2
      *strict* フラグが追加されました.

   .. versionchanged:: 2.4
      *strict* フラグは推奨されなくなりました.

   それ以外の :class:`Parser` メソッドは以下のとおりです:


   .. method:: parse(fp[, headersonly])

      ファイルなどストリーム形式  [#]_ のオブジェクト *fp* から\
      すべてのデータを読み込み、得られたテキストを解析して基底 (root) メッセージ\
      オブジェクト構造を返します。
      *fp* はストリーム形式のオブジェクトで :meth:`readline` および
      :meth:`read` 両方のメソッドをサポートしている必要があります。

      *fp* に格納されているテキスト文字列は、一連の :rfc:`2822` 形式の\
      ヘッダおよびヘッダ継続行 (header continuation lines) によって構成されている\
      必要があります。オプションとして、最初にエンペローブヘッダが来ることもできます。
      ヘッダ部分はデータの終端か、ひとつの空行によって終了したとみなされます。
      ヘッダ部分に続くデータはメッセージ本体となります (MIME エンコードされた
      subpart を含んでいるかもしれません)。

      オプション引数 *headersonly* はヘッダ部分を解析しただけで終了するか\
      否かを指定します。デフォルトの値は ``False``
      で、これはそのファイルの内容すべてを解析することを意味しています。

   .. versionchanged:: 2.2.2
      *headersonly* フラグが追加されました.


   .. method:: Parser.parsestr(text[, headersonly])

      :meth:`parse` メソッドに似ていますが、ファイルなどのストリーム形式のかわりに\
      文字列を引数としてとるところが違います。文字列に対してこのメソッドを\
      呼ぶことは、 *text* を :class:`StringIO` インスタンスとして作成して
      :meth:`parse` を適用するのと同じです。

      オプション引数 *headersonly* は :meth:`parse` メソッドと同じです。

   .. versionchanged:: 2.2.2
      *headersonly* フラグが追加されました.

ファイルや文字列からメッセージオブジェクト構造を作成するのは\
かなりよくおこなわれる作業なので、便宜上次のような 2つの関数が\
提供されています。これらは :mod:`email` パッケージのトップレベルの\
名前空間で使用できます。

.. currentmodule:: email

.. function:: message_from_string(s[, _class[, strict]])

   文字列からメッセージオブジェクト構造を作成し返します。
   これは ``Parser().parsestr(s)`` とまったく同じです。オプション引数
   *_class* および *strict* は :class:`Parser` クラスの\
   コンストラクタと同様に解釈されます。

   .. versionchanged:: 2.2.2
      *strict* フラグが追加されました.


.. function:: message_from_file(fp[, _class[, strict]])

   Open されたファイルオブジェクトからメッセージオブジェクト構造を作成し返します。
   これは ``Parser().parse(fp)`` とまったく同じです。
   オプション引数 *_class* および *strict* は :class:`Parser` クラスの\
   コンストラクタと同様に解釈されます。

   .. versionchanged:: 2.2.2
      *strict* フラグが追加されました.

対話的な Python プロンプトでこの関数を使用するとすれば、このようになります::

   >>> import email
   >>> msg = email.message_from_string(myString)


追加事項
^^^^^^^^

以下はテキスト解析の際に適用されるいくつかの規約です:

* ほとんどの非 :mimetype:`multipart` 形式のメッセージは単一の文字列ペイロードをもつ
  単一のメッセージオブジェクトとして解析されます。このオブジェクトは
  :meth:`is_multipart` に対して ``False`` を返します。
  このオブジェクトに対する :meth:`get_payload` メソッドは文字列オブジェクトを返します。

* :mimetype:`multipart` 形式のメッセージはすべてメッセージ内\
  コンポーネント (sub-message object) のリストとして解析されます。
  外側のコンテナメッセージオブジェクトは :meth:`is_multipart` に対して ``True``
  を返し、このオブジェクトに対する :meth:`get_payload` メソッドは
  :class:`~email.message.Message` subpart のリストを返します。

* :mimetype:`message/\*` の Content-Type をもつほとんどのメッセージ (例:
  :mimetype:`message/delivery-status` や :mimetype:`message/rfc822` など) も
  コンテナメッセージオブジェクトとして解析されますが、
  ペイロードのリストの長さは 1 になります。このオブジェクトは :meth:`is_multipart`
  メソッドに対して ``True`` を返し、
  リスト内にあるひとつだけの要素がメッセージ内のコンポーネントオブジェクトになります。

* いくつかの標準的でないメッセージは、 :mimetype:`multipart` の使い方に\
  統一がとれていない場合があります。このようなメッセージは
  :mailheader:`Content-Type` ヘッダに :mimetype:`multipart`
  を指定しているものの、その :meth:`is_multipart` メソッドは ``False``
  を返すことがあります。もしこのようなメッセージが
  :class:`FeedParser` によって解析されると、その *defects* 属性のリスト中には
  :class:`MultipartInvariantViolationDefect` クラスの\
  インスタンスが現れます。詳しい情報については
  :mod:`email.errors` を参照してください。

.. rubric:: 注記

.. [#] Python 2.4 から導入された email パッケージバージョン 3.0 では、
   旧式の :class:`Parser` は :class:`FeedParser` によって書き直されました。
   そのためパーザの意味論と得られる結果は 2つのパーザで同一のものになります。

.. [#] file-like object
