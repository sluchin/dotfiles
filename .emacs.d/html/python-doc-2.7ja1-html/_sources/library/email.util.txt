:mod:`email`: 雑用ユーティリティ
--------------------------------

.. module:: email.utils
   :synopsis: 電子メールパッケージの雑多なユーティリティ。


:mod:`email.utils` モジュールではいくつかの便利なユーティリティを提供しています。


.. function:: quote(str)

   文字列 *str* 内のバックスラッシュをバックスラッシュ2つに置換した\
   新しい文字列を返します。また、ダブルクォートはバックスラッシュ +
   ダブルクォートに置換されます。


.. function:: unquote(str)

   文字列 *str* を *逆クォート* した新しい文字列を返します。
   もし *str* の先頭あるいは末尾がダブルクォートだった場合、
   これらは単に切りおとされます。
   同様にもし *str* の先頭あるいは末尾が角ブラケット (<、>) だった場合も切りおとされます。


.. function:: parseaddr(address)

   アドレスをパーズします。
   :mailheader:`To` や :mailheader:`Cc` のような\
   アドレスをふくんだフィールドの値を与えると、構成部分の\
   *実名* と *電子メールアドレス* を取り出します。
   パーズに成功した場合、これらの情報をタプル ``(realname, email_address)``
   にして返します。失敗した場合は 2要素のタプル ``('', '')`` を返します。


.. function:: formataddr(pair)

   :meth:`parseaddr` の逆で、実名と電子メールアドレスからなる 2要素のタプル\
   ``(realname, email_address)`` を引数にとり、
   :mailheader:`To` あるいは :mailheader:`Cc` ヘッダに適した形式の文字列を\
   返します。タプル *pair* の第1要素が偽である場合、第2要素の値をそのまま返します。


.. function:: getaddresses(fieldvalues)

   このメソッドは 2要素タプルのリストを ``parseaddr()`` と同じ形式で返します。
   *fieldvalues* はたとえば :meth:`Message.get_all` が返すような、
   ヘッダのフィールド値からなるシーケンスです。以下はある電子メールメッセージから\
   すべての受け取り人を得る一例です::

      from email.utils import getaddresses

      tos = msg.get_all('to', [])
      ccs = msg.get_all('cc', [])
      resent_tos = msg.get_all('resent-to', [])
      resent_ccs = msg.get_all('resent-cc', [])
      all_recipients = getaddresses(tos + ccs + resent_tos + resent_ccs)


.. function:: parsedate(date)

   :rfc:`2822` に記された規則にもとづいて日付を解析します。
   しかし、メイラーによってはここで指定された規則に従っていないものがあり、そのような場合
   :func:`parsedate` はなるべく正しい日付を推測しようとします。
   *date* は :rfc:`2822` 形式の日付を保持している文字列で、
   ``"Mon, 20 Nov 1995 19:12:08 -0500"`` のような形をしています。
   日付の解析に成功した場合、 :func:`parsedate` は関数 :func:`time.mktime`
   に直接渡せる形式の9要素からなるタプルを返し、失敗した場合は ``None`` を返します。
   返されるタプルの 6、7、8番目は有効ではないので注意してください。


.. function:: parsedate_tz(date)

   :func:`parsedate` と同様の機能を提供しますが、
   ``None`` または 10要素のタプルを返すところが違います。
   最初の 9つの要素は :func:`time.mktime` に直接渡せる形式のものであり、
   最後の 10番目の要素は、その日付の時間帯の UTC
   (グリニッジ標準時の公式な呼び名です) に対するオフセットです [#]_ 。
   入力された文字列に時間帯が指定されていなかった場合、10番目の要素には
   ``None`` が入ります。
   タプルの 6、7、8番目は有効ではないので注意してください。


.. function:: mktime_tz(tuple)

   :func:`parsedate_tz` が返す 10要素のタプルを UTC の\
   タイムスタンプに変換します。与えられた時間帯が ``None`` である場合、
   時間帯として現地時間 (localtime) が仮定されます。
   マイナーな欠点: :func:`mktime_tz` はまず *tuple* の最初の 8要素を
   localtime として変換し、つぎに時間帯の差を加味しています。
   夏時間を使っている場合には、これは通常の使用にはさしつかえないものの、
   わずかな誤差を生じるかもしれません。


.. function:: formatdate([timeval[, localtime][, usegmt]])

   日付を :rfc:`2822` 形式の文字列で返します。例::

      Fri, 09 Nov 2001 01:08:47 -0000

   オプションとして float 型の値をもつ引数 *timeval* が与えられた場合、
   これは :func:`time.gmtime` および :func:`time.localtime` に\
   渡されます。それ以外の場合、現在の時刻が使われます。

   オプション引数 *localtime* はフラグです。
   これが ``True`` の場合、この関数は *timeval* を解析したあと UTC
   のかわりに現地時間 (localtime) の時間帯をつかって変換します。
   おそらく夏時間も考慮に入れられるでしょう。デフォルトではこの値は
   ``False`` で、UTC が使われます。

   オプション引数 *usegmt* が ``True`` のときは、タイムゾーンを表すのに\
   数値の ``-0000`` ではなく ascii文字列である ``GMT`` が使われます。
   これは (HTTP などの) いくつかのプロトコルで必要です。
   この機能は *localtime* が ``False`` のときのみ適用されます。
   デフォルトは ``False`` です。

   .. versionadded:: 2.4


.. function:: make_msgid([idstring])

   :rfc:`2822` 準拠形式の :mailheader:`Message-ID` ヘッダに適した\
   文字列を返します。オプション引数 *idstring* が文字列として\
   与えられた場合、これはメッセージ ID の一意性を高めるのに利用されます。


.. function:: decode_rfc2231(s)

   :rfc:`2231` に従って文字列 *s* をデコードします。


.. function:: encode_rfc2231(s[, charset[, language]])

   :rfc:`2231` に従って *s* をエンコードします。
   オプション引数 *charset* および *language* が与えられた場合、
   これらは文字セット名と言語名として使われます。
   もしこれらのどちらも与えられていない場合、 *s* はそのまま返されます。
   *charset* は与えられているが *language* が与えられていない場合、
   文字列 *s* は *language* の空文字列を使ってエンコードされます。


.. function:: collapse_rfc2231_value(value[, errors[, fallback_charset]])

   ヘッダのパラメータが :rfc:`2231` 形式でエンコードされている場合、
   :meth:`Message.get_param` は 3要素からなるタプルを返すことがあります。
   ここには、そのパラメータの文字セット、言語、および値の順に格納されています。
   :func:`collapse_rfc2231_value` はこのパラメータをひとつの Unicode 文字列に\
   まとめます。オプション引数 *errors* は built-in である :func:`unicode` 関数の\
   引数 *errors* に渡されます。このデフォルト値は ``replace`` となっています。
   オプション引数 *fallback_charset* は、もし :rfc:`2231` ヘッダの使用している\
   文字セットが Python の知っているものではなかった場合の非常用文字セットとして\
   使われます。デフォルトでは、この値は ``us-ascii`` です。

   便宜上、 :func:`collapse_rfc2231_value` に渡された引数 *value* が\
   タプルでない場合には、これは文字列である必要があります。
   その場合には unquote された文字列が返されます。


.. function:: decode_params(params)

   :rfc:`2231` に従ってパラメータのリストをデコードします。
   *params* は ``(content-type, string-value)``
   のような形式の 2要素からなるタプルです。

.. versionchanged:: 2.4
   :func:`dump_address_pair` 関数は撤去されました。かわりに
   :func:`formataddr` 関数を使ってください。

.. versionchanged:: 2.4
   :func:`decode` 関数は撤去されました。かわりに
   :meth:`Header.decode_header` メソッドを使ってください。

.. versionchanged:: 2.4
   :func:`encode` 関数は撤去されました。かわりに
   :meth:`Header.encode` メソッドを使ってください。

.. rubric:: 注記

.. [#] 注意: この時間帯のオフセット値は ``time.timezone`` の値と\
   符号が逆です。これは ``time.timezone`` が POSIX
   標準に準拠しているのに対して、こちらは :rfc:`2822` に準拠しているからです。
