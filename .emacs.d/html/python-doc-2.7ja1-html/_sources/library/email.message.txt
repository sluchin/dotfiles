:mod:`email`: 電子メールメッセージの表現
----------------------------------------------------

.. module:: email.message
   :synopsis: 電子メールのメッセージを表現する基底クラス


:class:`Message` クラスは、 :mod:`email` パッケージの中心となるクラスです。
これは :mod:`email` オブジェクトモデルの基底クラスになっています。
:class:`Message` はヘッダフィールドを検索したりメッセージ本体にアクセスするための
核となる機能を提供します。

概念的には、(:mod:`email.message` モジュールからインポートされる)
:class:`Message` オブジェクトには *ヘッダ* と *ペイロード* が\
格納されています。ヘッダは、 :rfc:`2822` 形式のフィールド名およびフィールド値が\
コロンで区切られたものです。コロンはフィールド名またはフィールド値の\
どちらにも含まれません。

ヘッダは大文字小文字を区別した形式で保存されますが、
ヘッダ名が一致するかどうかの検査は大文字小文字を区別せずにおこなうことができます。
*Unix-From* ヘッダまたは ``From_`` ヘッダとして知られる\
エンベロープヘッダがひとつ存在することもあります。
ペイロードは、単純なメッセージオブジェクトの場合は単なる文字列ですが、
MIME コンテナ文書 (:mimetype:`multipart/\*` または
:mimetype:`message/rfc822` など) の場合は :class:`Message` オブジェクトの\
リストになっています。

:class:`Message` オブジェクトは、メッセージヘッダにアクセスするための\
マップ (辞書) 形式のインタフェイスと、ヘッダおよびペイロードの両方に\
アクセスするための明示的なインタフェイスを提供します。
これにはメッセージオブジェクトツリーからフラットなテキスト文書を\
生成したり、一般的に使われるヘッダのパラメータにアクセスしたり、また\
オブジェクトツリーを再帰的にたどったりするための便利なメソッドを含みます。

:class:`Message` クラスのメソッドは以下のとおりです:


.. class:: Message()

   コンストラクタは引数をとりません。


   .. method:: as_string([unixfrom])

      メッセージ全体をフラットな文字列として返します。オプション *unixfrom* が
      ``True`` の場合、返される文字列にはエンベロープヘッダも含まれます。
      *unixfrom* のデフォルトは ``False`` です。
      もし、文字列への変換を完全に行うためにデフォルト値を埋める必要がある場合、
      メッセージのフラット化は :class:`Message` の変更を引き起こす可能性があります
      (例えば、MIME の境界が生成される、変更される等)。

      このメソッドは手軽に利用する事ができますが、必ずしも期待通りにメッセージを\
      フォーマットするとは限りません。たとえば、これはデフォルトでは ``From`` で\
      始まる行を変更してしまいます。以下の例のように :class:`~email.generator.Generator`
      のインスタンスを生成して :meth:`flatten` メソッドを直接呼び出せば\
      より柔軟な処理を行う事ができます。 ::

         from cStringIO import StringIO
      	 from email.generator import Generator
      	 fp = StringIO()
      	 g = Generator(fp, mangle_from_=False, maxheaderlen=60)
      	 g.flatten(msg)
      	 text = fp.getvalue()


   .. method:: __str__()

      :meth:`as_string(unixfrom=True)` と同じです。


   .. method:: is_multipart()

      メッセージのペイロードが子 :class:`Message` オブジェクトからなる\
      リストであれば ``True`` を返し、そうでなければ ``False``
      を返します。 :meth:`is_multipart` が False を返した場合は、ペイロードは\
      文字列オブジェクトである必要があります。


   .. method:: set_unixfrom(unixfrom)

      メッセージのエンベロープヘッダを *unixfrom* に設定します。
      これは文字列である必要があります。


   .. method:: get_unixfrom()

      メッセージのエンベロープヘッダを返します。
      エンベロープヘッダが設定されていない場合は ``None`` が返されます。


   .. method:: attach(payload)

      与えられた *payload* を現在のペイロードに追加します。
      この時点でのペイロードは ``None`` か、あるいは :class:`Message`
      オブジェクトのリストである必要があります。
      このメソッドの実行後、ペイロードは必ず :class:`Message`
      オブジェクトのリストになります。ペイロードに\
      スカラーオブジェクト (文字列など) を格納したい場合は、かわりに :meth:`set_payload`
      を使ってください。


   .. method:: get_payload([i[, decode]])

      現在のペイロードへの参照を返します。これは :meth:`is_multipart` が
      ``True`` の場合 :class:`Message` オブジェクトのリストになり、
      :meth:`is_multipart` が ``False`` の場合は文字列になります。
      ペイロードがリストの場合、
      リストを変更することはそのメッセージのペイロードを変更することになります。

      オプション引数の *i* がある場合、
      :meth:`is_multipart` が ``True`` ならば :meth:`get_payload` は\
      ペイロード中で 0 から数えて *i* 番目の要素を返します。
      *i* が 0 より小さい場合、あるいはペイロードの個数以上の場合は
      :exc:`IndexError` が発生します。
      ペイロードが文字列 (つまり :meth:`is_multipart` が ``False``)
      にもかかわらず *i* が与えられたときは :exc:`TypeError` が発生します。

      オプションの *decode* はそのペイロードが
      :mailheader:`Content-Transfer-Encoding` ヘッダに従って\
      デコードされるべきかどうかを指示するフラグです。
      この値が ``True`` でメッセージが multipart ではない場合、
      ペイロードはこのヘッダの値が ``quoted-printable`` または ``base64``
      のときにかぎりデコードされます。これ以外のエンコーディングが\
      使われている場合、 :mailheader:`Content-Transfer-Encoding` ヘッダが\
      ない場合、あるいは曖昧なbase64データが含まれる場合は、ペイロードはそのまま
      (デコードされずに) 返されます。もしメッセージが multipart で
      *decode* フラグが ``True`` の場合は ``None`` が返されます。
      *decode* のデフォルト値は ``False`` です。


   .. method:: set_payload(payload[, charset])

      メッセージ全体のオブジェクトのペイロードを *payload* に設定します。
      ペイロードの形式をととのえるのは呼び出し側の責任です。オプションの
      *charset* はメッセージのデフォルト文字セットを設定します。
      詳しくは :meth:`set_charset` を参照してください。

      .. versionchanged:: 2.2.2
         *charset* 引数の追加.


   .. method:: set_charset(charset)

      ペイロードの文字セットを *charset* に変更します。
      ここには :class:`~email.charset.Charset` インスタンス (:mod:`email.charset` 参照)、
      文字セット名をあらわす文字列、あるいは ``None`` のいずれかが指定できます。
      文字列を指定した場合、これは :class:`~email.charset.Charset` インスタンスに変換されます。
      *charset* が ``None`` の場合、 ``charset`` パラメータは
      :mailheader:`Content-Type` ヘッダから除去されます
      (それ以外にメッセージの変形は行われません)。
      これ以外のものを文字セットとして指定した場合、 :exc:`TypeError`
      が発生します。

      :mailheader:`MIME-Version` ヘッダが存在しなければ、追加されます。
      :mailheader:`Content-Type` ヘッダが存在しなければ、
      :mimetype:`text/plain` を値として追加されます。
      :mailheader:`Content-Type` が存在していてもいなくても、
      ``charset`` パラメタは *charset.output_charset* に設定されます。
      *charset.input_charset* と *charset.output_charset* が異なるなら、
      ペイロードは *output_charset* に再エンコードされます。
      :mailheader:`Content-Transfer-Encoding` ヘッダが存在しなければ、
      ペイロードは、必要なら指定された :class:`~email.charset.Charset` 
      を使って transfer エンコードされ、適切な値のヘッダが追加されます。
      :mailheader:`Content-Transfer-Encoding` ヘッダがすでに存在すれば、
      ペイロードはすでにその :mailheader:`Content-Transfer-Encoding` によって
      正しくエンコードされたものと見なされ、変形されません。

      ここでいうメッセージとは、unicode 文字列か *charset.input_charset* でエンコードされた
      ペイロードを持つ :mimetype:`text/\*` 形式のものを仮定しています。これは、もし必要とあらば\
      プレーンテキスト形式を変換するさいに *charset.output_charset* の
      トランスファーエンコードに変換されます。MIME ヘッダ (:mailheader:`MIME-Version`,
      :mailheader:`Content-Type`,
      :mailheader:`Content-Transfer-Encoding`) は必要に応じて追加されます。

      .. versionadded:: 2.2.2


   .. method:: get_charset()

      そのメッセージ中のペイロードの :class:`~email.charset.Charset` インスタンスを返します。

      .. versionadded:: 2.2.2

   以下のメソッドは、メッセージの :rfc:`2822` ヘッダにアクセスするための\
   マップ (辞書) 形式のインタフェイスを実装したものです。
   これらのメソッドと、通常のマップ (辞書) 型はまったく同じ意味をもつわけでは\
   ないことに注意してください。たとえば辞書型では、同じキーが複数あることは\
   許されていませんが、ここでは同じメッセージヘッダが複数ある場合があります。\
   また、辞書型では :meth:`keys` で返されるキーの順序は保証されていませんが、
   :class:`Message` オブジェクト内のヘッダはつねに元のメッセージ中に\
   現れた順序、あるいはそのあとに追加された順序で返されます。削除され、その後\
   ふたたび追加されたヘッダはリストの一番最後に現れます。

   こういった意味のちがいは意図的なもので、最大の利便性をもつようにつくられています。

   注意: どんな場合も、メッセージ中のエンベロープヘッダはこのマップ形式の\
   インタフェイスには含まれません。


   .. method:: __len__()

      複製されたものもふくめてヘッダ数の合計を返します。


   .. method:: __contains__(name)

      メッセージオブジェクトが *name* という名前のフィールドを持っていれば
      true を返します。この検査では名前の大文字小文字は区別されません。
      *name* は最後にコロンをふくんでいてはいけません。このメソッドは以下のように
      ``in`` 演算子で使われます::

         if 'message-id' in myMessage:
             print 'Message-ID:', myMessage['message-id']


   .. method:: __getitem__(name)

      指定された名前のヘッダフィールドの値を返します。
      *name* は最後にコロンをふくんでいてはいけません。そのヘッダがない場合は ``None``
      が返され、 :exc:`KeyError` 例外は発生しません。

      注意: 指定された名前のフィールドがメッセージのヘッダに2回以上現れている場合、
      どちらの値が返されるかは未定義です。ヘッダに存在するフィールドの値をすべて\
      取り出したい場合は :meth:`get_all` メソッドを使ってください。


   .. method:: __setitem__(name, val)

      メッセージヘッダに *name* という名前の *val* という値をもつ\
      フィールドをあらたに追加します。このフィールドは現在メッセージに\
      存在するフィールドのいちばん後に追加されます。

      注意: このメソッドでは、すでに同一の名前で存在するフィールドは\
      上書き *されません* 。もしメッセージが名前 *name* をもつ\
      フィールドをひとつしか持たないようにしたければ、最初にそれを除去してください。
      たとえば::

         del msg['subject']
      	 msg['subject'] = 'PythonPythonPython!'


   .. method:: __delitem__(name)

      メッセージのヘッダから、 *name* という名前をもつフィールドをすべて除去します。
      たとえこの名前をもつヘッダが存在していなくても例外は発生しません。


   .. method:: has_key(name)

      メッセージが *name* という名前をもつヘッダフィールドを持っていれば真を、\
      そうでなければ偽を返します。


   .. method:: keys()

      メッセージ中にあるすべてのヘッダのフィールド名のリストを返します。


   .. method:: values()

      メッセージ中にあるすべてのフィールドの値のリストを返します。


   .. method:: items()

      メッセージ中にあるすべてのヘッダのフィールド名とその値を
      2-タプルのリストとして返します。


   .. method:: get(name[, failobj])

      指定された名前をもつフィールドの値を返します。
      これは指定された名前がないときにオプション引数の *failobj*
      (デフォルトでは ``None``) を返すことをのぞけば、
      :meth:`__getitem__` と同じです。

   さらに、役に立つメソッドをいくつか紹介します:


   .. method:: get_all(name[, failobj])

      *name* の名前をもつフィールドのすべての値からなるリストを返します。
      該当する名前のヘッダがメッセージ中に含まれていない場合は *failobj*
      (デフォルトでは ``None``) が返されます。


   .. method:: add_header(_name, _value, **_params)

      拡張ヘッダ設定。このメソッドは :meth:`__setitem__` と似ていますが、
      追加のヘッダ・パラメータをキーワード引数で指定できるところが違っています。
      *_name* に追加するヘッダフィールドを、 *_value* にそのヘッダの
      *最初の* 値を渡します。

      キーワード引数辞書 *_params* の各項目ごとに、
      そのキーがパラメータ名として扱われ、キー名にふくまれる\
      アンダースコアはハイフンに置換されます
      (なぜならハイフンは通常の Python 識別子としては使えないからです)。
      ふつう、パラメータの値が ``None`` 以外のときは、
      ``key="value"`` の形で追加されます。
      パラメータの値が ``None`` のときはキーのみが追加されます。
      値が非 ASCII 文字を含むなら、それは ``(CHARSET, LANGUAGE, VALUE)`` の
      形式の 3 タプルでなくてはなりません。
      ここで ``CHARSET`` はその値をエンコードするのに使われる文字セットを
      指名する文字列で、 ``LANGUAGE`` は通常 ``None`` か空文字列にでき
      (その他の可能性は :RFC:`2231` を参照してください)、 ``VALUE`` は
      非 ASCII コードポイントを含む文字列値です。
      

      例を示しましょう::

         msg.add_header('Content-Disposition', 'attachment', filename='bud.gif')

      こうするとヘッダには以下のように追加されます。 ::

         Content-Disposition: attachment; filename="bud.gif"

      非 ASCII 文字を使った例::

         msg.add_header('Content-Disposition', 'attachment',
                        filename=('iso-8859-1', '', 'Fußballer.ppt'))

      は、以下のようになります::

         Content-Disposition: attachment; filename*="iso-8859-1''Fu%DFballer.ppt"


   .. method:: replace_header(_name, _value)

      ヘッダの置換。
      *_name* と一致するヘッダで最初に見つかったものを置き換えます。
      このときヘッダの順序とフィールド名の大文字小文字は保存されます。
      一致するヘッダがない場合、 :exc:`KeyError` が発生します。

      .. versionadded:: 2.2.2


   .. method:: get_content_type()

      そのメッセージの content-type を返します。
      返された文字列は強制的に小文字で :mimetype:`maintype/subtype`
      の形式に変換されます。
      メッセージ中に :mailheader:`Content-Type` ヘッダがない場合、
      デフォルトの content-type は :meth:`get_default_type`
      が返す値によって与えられます。 :rfc:`2045` によればメッセージはつねにデフォルトの
      content-type をもっているので、 :meth:`get_content_type`
      はつねになんらかの値を返すはずです。

      :rfc:`2045` はメッセージのデフォルト content-type を、
      それが :mimetype:`multipart/digest`
      コンテナに現れているとき以外は :mimetype:`text/plain` に規定しています。
      あるメッセージが
      :mimetype:`multipart/digest` コンテナ中にある場合、その content-type は
      :mimetype:`message/rfc822` になります。
      もし :mailheader:`Content-Type` ヘッダが適切でない
      content-type 書式だった場合、 :rfc:`2045` はそれのデフォルトを
      :mimetype:`text/plain` として扱うよう定めています。

      .. versionadded:: 2.2.2


   .. method:: Message.get_content_maintype()

      そのメッセージの主 content-type を返します。
      これは :meth:`get_content_type` によって返される文字列の
      :mimetype:`maintype` 部分です。

      .. versionadded:: 2.2.2


   .. method:: Message.get_content_subtype()

      そのメッセージの副 content-type (sub content-type、subtype) を返します。
      これは :meth:`get_content_type` によって返される文字列の
      :mimetype:`subtype` 部分です。

      .. versionadded:: 2.2.2


   .. method:: get_default_type()

      デフォルトの content-type を返します。
      ほどんどのメッセージではデフォルトの content-type は
      :mimetype:`text/plain` ですが、メッセージが :mimetype:`multipart/digest`
      コンテナに含まれているときだけ例外的に :mimetype:`message/rfc822` になります。

      .. versionadded:: 2.2.2


   .. method:: set_default_type(ctype)

      デフォルトの content-type を設定します。
      *ctype* は :mimetype:`text/plain` あるいは
      :mimetype:`message/rfc822` である必要がありますが、強制ではありません。
      デフォルトの content-type はヘッダの
      :mailheader:`Content-Type` には格納されません。

      .. versionadded:: 2.2.2


   .. method:: get_params([failobj[, header[, unquote]]])

      メッセージの :mailheader:`Content-Type` パラメータをリストとして返します。
      返されるリストはキー/値の組からなる2要素タプルが連なったものであり、
      これらは ``'='`` 記号で分離されています。
      ``'='`` の左側はキーになり、右側は値になります。パラメータ中に
      ``'='`` がなかった場合、値の部分は空文字列になり、そうでなければその値は
      :meth:`get_param` で説明されている形式になります。
      また、オプション引数 *unquote* が ``True`` (デフォルト) である場合、
      この値は unquote されます。

      オプション引数 *failobj* は、 :mailheader:`Content-Type` ヘッダが\
      存在しなかった場合に返すオブジェクトです。オプション引数
      *header* には :mailheader:`Content-Type` のかわりに検索すべきヘッダを\
      指定します。

      .. versionchanged:: 2.2.2
         *unquote* が追加されました.


   .. method:: get_param(param[, failobj[, header[, unquote]]])

      メッセージの :mailheader:`Content-Type` ヘッダ中のパラメータ *param* を\
      文字列として返します。そのメッセージ中に
      :mailheader:`Content-Type` ヘッダが存在しなかった場合、
      *failobj*  (デフォルトは ``None``) が返されます。

      オプション引数 *header* が与えられた場合、 :mailheader:`Content-Type`
      のかわりにそのヘッダが使用されます。

      パラメータのキー比較は常に大文字小文字を区別しません。
      返り値は文字列か 3 要素のタプルで、タプルになるのはパラメータが :rfc:`2231`
      エンコードされている場合です。3 要素タプルの場合、各要素の値は
      ``(CHARSET, LANGUAGE, VALUE)`` の形式になっています。
      ``CHARSET`` と ``LAGUAGE`` は ``None`` になることがあり、
      その場合 ``VALUE`` は ``us-ascii`` 文字セットでエンコードされているとみなさねば\
      ならないので注意してください。普段は ``LANGUAGE`` を無視できます。

      この関数を使うアプリケーションが、パラメータが :rfc:`2231` 形式で\
      エンコードされているかどうかを気にしないのであれば、
      :func:`email.utils.collapse_rfc2231_value` に
      :meth:`get_param` の返り値を渡して呼び出すことで、
      このパラメータをひとつにまとめることができます。
      この値がタプルならばこの関数は適切にデコードされた Unicode 文字列を返し、
      そうでない場合は unquote された元の文字列を返します。たとえば::

         rawparam = msg.get_param('foo')
         param = email.utils.collapse_rfc2231_value(rawparam)

      いずれの場合もパラメータの値は (文字列であれ3要素タプルの ``VALUE`` 項目であれ)
      つねに unquote されます。ただし、
      *unquote* が ``False`` に指定されている場合は unquote されません。

      .. versionchanged:: 2.2.2
         *unquote* 引数の追加、3要素タプルが返り値になる可能性あり。


   .. method:: set_param(param, value[, header[, requote[, charset[, language]]]])

      :mailheader:`Content-Type` ヘッダ中のパラメータを設定します。
      指定されたパラメータがヘッダ中にすでに存在する場合、その値は
      *value* に置き換えられます。
      :mailheader:`Content-Type` ヘッダがまだこのメッセージ中に存在していない場合、
      :rfc:`2045` にしたがいこの値には :mimetype:`text/plain`
      が設定され、新しいパラメータ値が末尾に追加されます。

      オプション引数 *header* が与えられた場合、
      :mailheader:`Content-Type` のかわりにそのヘッダが使用されます。オプション引数
      *unquote* が ``False`` でない限り、
      この値は unquote されます (デフォルトは ``True``)。

      オプション引数 *charset* が与えられると、
      そのパラメータは :rfc:`2231` に従ってエンコードされます。オプション引数
      *language* は RFC 2231 の言語を指定しますが、
      デフォルトではこれは空文字列となります。 *charset* と *language*
      はどちらも文字列である必要があります。

      .. versionadded:: 2.2.2


   .. method:: del_param(param[, header[, requote]])

      指定されたパラメータを :mailheader:`Content-Type` ヘッダ中から完全に\
      とりのぞきます。ヘッダはそのパラメータと値がない状態に書き換えられます。
      *requote* が ``False`` でない限り (デフォルトでは
      ``True`` です)、すべての値は必要に応じて quote されます。
      オプション変数 *header* が与えられた場合、
      :mailheader:`Content-Type` のかわりにそのヘッダが使用されます。

      .. versionadded:: 2.2.2


   .. method:: set_type(type[, header][, requote])

      :mailheader:`Content-Type` ヘッダの maintype と subtype を設定します。
      *type* は :mimetype:`maintype/subtype` という形の文字列でなければなりません。
      それ以外の場合は :exc:`ValueError` が発生します。

      このメソッドは :mailheader:`Content-Type` ヘッダを置き換えますが、
      すべてのパラメータはそのままにします。
      *requote* が ``False`` の場合、
      これはすでに存在するヘッダを quote せず放置しますが、そうでない場合は\
      自動的に quote します (デフォルト動作)。

      オプション変数 *header* が与えられた場合、
      :mailheader:`Content-Type` のかわりにそのヘッダが使用されます。
      :mailheader:`Content-Type` ヘッダが設定される場合には、
      :mailheader:`MIME-Version` ヘッダも同時に付加されます。

      .. versionadded:: 2.2.2


   .. method:: get_filename([failobj])

      そのメッセージ中の :mailheader:`Content-Disposition` ヘッダにある、
      ``filename`` パラメータの値を返します。
      目的のヘッダに ``filename`` パラメータがない場合には
      :mailheader:`Content-Type` ヘッダにある ``name``
      パラメータを探します。
      それも無い場合またはヘッダが無い場合には *failobj* が返されます。
      返される文字列はつねに :meth:`email.utils.unquote` によって unquote されます。


   .. method:: get_boundary([failobj])

      そのメッセージ中の :mailheader:`Content-Type` ヘッダにある、
      ``boundary`` パラメータの値を返します。
      目的のヘッダが欠けていたり、 ``boundary`` パラメータがない場合には
      *failobj* が返されます。
      返される文字列はつねに :meth:`email.utils.unquote` によって unquote されます。


   .. method:: set_boundary(boundary)

      メッセージ中の :mailheader:`Content-Type` ヘッダにある、 ``boundary``
      パラメータに値を設定します。
      :meth:`set_boundary` は必要に応じて *boundary* を quote します。
      そのメッセージが :mailheader:`Content-Type` ヘッダを含んでいない場合、
      :exc:`HeaderParseError` が発生します。

      注意: このメソッドを使うのは、古い :mailheader:`Content-Type` ヘッダを\
      削除して新しい boundary をもったヘッダを :meth:`add_header` で\
      足すのとは少し違います。 :meth:`set_boundary` は一連のヘッダ中での
      :mailheader:`Content-Type` ヘッダの位置を保つからです。しかし、これは元の
      :mailheader:`Content-Type` ヘッダ中に存在していた\
      連続する行の順番までは *保ちません* 。


   .. method:: get_content_charset([failobj])

      そのメッセージ中の :mailheader:`Content-Type` ヘッダにある、 ``charset``
      パラメータの値を返します。値はすべて小文字に変換されます。
      メッセージ中に :mailheader:`Content-Type` がなかったり、このヘッダ中に
      ``boundary`` パラメータがない場合には *failobj* が返されます。

      注意: これは :meth:`get_charset` メソッドとは異なります。
      こちらのほうは文字列のかわりに、そのメッセージボディのデフォルト\
      エンコーディングの :class:`~email.charset.Charset` インスタンスを返します。

      .. versionadded:: 2.2.2


   .. method:: get_charsets([failobj])

      メッセージ中に含まれる文字セットの名前をすべてリストにして返します。
      そのメッセージが :mimetype:`multipart` である場合、返されるリストの\
      各要素がそれぞれの subpart のペイロードに対応します。それ以外の場合、
      これは長さ 1 のリストを返します。

      リスト中の各要素は文字列であり、これは対応する subpart 中の\
      それぞれの :mailheader:`Content-Type` ヘッダにある
      ``charset`` の値です。しかし、その subpart が
      :mailheader:`Content-Type` をもってないか、
      ``charset`` がないか、あるいは MIME maintype が :mimetype:`text` でない\
      いずれかの場合には、リストの要素として *failobj* が返されます。


   .. method:: walk()

      :meth:`walk` メソッドは多目的のジェネレータで、
      これはあるメッセージオブジェクトツリー中のすべての part および subpart を\
      わたり歩くのに使えます。順序は深さ優先です。おそらく典型的な用法は、
      :meth:`walk` を ``for`` ループ中でのイテレータとして\
      使うことでしょう。ループを一回まわるごとに、次の subpart が返されるのです。

      以下の例は、 multipart メッセージのすべての part において、
      その MIME タイプを表示していくものです。 ::

         >>> for part in msg.walk():
      	 ...     print part.get_content_type()
      	 multipart/report
      	 text/plain
      	 message/delivery-status
      	 text/plain
      	 text/plain
      	 message/rfc822

   .. versionchanged:: 2.5
      以前の非推奨メソッド :meth:`get_type` 、 :meth:`get_main_type` 、
      :meth:`get_subtype` は削除されました。

   :class:`Message` オブジェクトはオプションとして 2つのインスタンス属性を\
   とることができます。これはある MIME メッセージからプレーンテキストを\
   生成するのに使うことができます。


   .. attribute:: preamble

      MIME ドキュメントの形式では、
      ヘッダ直後にくる空行と最初の multipart 境界をあらわす文字列のあいだに\
      いくらかのテキスト (訳注: preamble, 序文) を埋めこむことを許しています。
      このテキストは標準的な MIME の範疇からはみ出しているので、 MIME
      形式を認識するメールソフトからこれらは通常まったく見えません。
      しかしメッセージのテキストを生で見る場合、あるいはメッセージを MIME
      対応していないメールソフトで見る場合、このテキストは目に見えることになります。

      *preamble* 属性は MIME ドキュメントに加えるこの最初の MIME
      範囲外テキストを含んでいます。 :class:`~email.parser.Parser`
      があるテキストをヘッダ以降に発見したが、それはまだ最初の MIME
      境界文字列が現れる前だった場合、パーザはそのテキストをメッセージの *preamble*
      属性に格納します。 :class:`Generator` がある MIME メッセージから\
      プレーンテキスト形式を生成するとき、これはそのテキストをヘッダと最初の MIME
      境界の間に挿入します。詳細は :mod:`email.parser` および
      :mod:`email.Generator` を参照してください。

      注意: そのメッセージに preamble がない場合、
      *preamble* 属性には ``None`` が格納されます。


   .. attribute:: epilogue

      *epilogue* 属性はメッセージの最後の MIME 境界文字列から\
      メッセージ末尾までのテキストを含むもので、それ以外は *preamble*
      属性と同じです。

      .. versionchanged:: 2.5
         :class:`Generator` でファイル終端に改行を出力するため、
      	 epilogue に空文字列を設定する必要はなくなりました。


   .. attribute:: defects

      *defects* 属性はメッセージを解析する途中で検出されたすべての問題点
      (defect、障害) のリストを保持しています。解析中に発見されうる障害に\
      ついてのより詳細な説明は :mod:`email.errors` を参照してください。

      .. versionadded:: 2.4

