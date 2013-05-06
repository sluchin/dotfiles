
:mod:`quopri` --- MIME quoted-printable 形式データのエンコードおよびデコード
============================================================================

.. module:: quopri
   :synopsis: MIME quoted-printable 形式ファイルのエンコードおよびデコード。


.. index::
   pair: quoted-printable; encoding
   single: MIME; quoted-printable encoding

このモジュールは :rfc:`1521`: "MIME (Multipurpose Internet Mail Extensions) Part One:
Mechanisms for Specifying and Describing the Format of Internet Message Bodies"
で定義されている quoted-printable による伝送のエンコードおよびデコードを行います。
quoted-printable エンコーディングは比較的印字不可能な文字の少ないデータのために設計\
されています; 画像ファイルを送るときのように印字不可能な文字がたくさんある\
場合には、 :mod:`base64` モジュールで利用できる base64 エンコーディングのほうがより\
コンパクトになります。

.. seealso::

   最新バージョンの `quopri モジュールの Python ソースコード
   <http://svn.python.org/view/python/branches/release27-maint/Lib/quopri.py?view=markup>`_

.. function:: decode(input, output[,header])

   ファイル *input* の内容をデコードして、デコードされたバイナリ\
   データをファイル *output* に書き出します。 *input* および
   *output* はファイルか、ファイルオブジェクトの\
   インタフェースを真似たオブジェクトでなければなりません。 *input* は
   ``input.readline()`` が空文字列を返すまで\
   読みつづけられます。オプション引数 *header* が存在し、かつその\
   値が真である場合、アンダースコアは空白文字にデコードされます。
   これは :rfc:`1522`: "MIME (Multipurpose Internet
   Mail Extensions) Part Two: Message Header Extensions for Non-ASCII Text"
   で記述されているところの "Q"-エンコードされたヘッダをデコードするの\
   に使われます。


.. function:: encode(input, output, quotetabs)

   ファイル *input* の内容をエンコードして、quoted-printable 形式に\
   エンコードされたデータをファイル *output* に書き出します。
   *input* および *output* はファイルか、ファイルオブジェクトの\
   インタフェースを真似たオブジェクトでなければなりません。 *input* は
   ``input.readline()`` が空文字列を返すまで読みつづけられます。
   *quotetabs* はデータ中に埋め込まれた空白文字やタブを変換するか\
   どうか制御するフラグです; この値が真なら、それらの空白をエンコード\
   します。偽ならエンコードせずそのままにしておきます。行末のスペースや\
   タブは :rfc:`1521` に従って常に変換されるので注意してください。


.. function:: decodestring(s[,header])

   :func:`decode` に似ていますが、文字列を入力として受け取り、\
   デコードされた文字列を返します。


.. function:: encodestring(s[, quotetabs])

   :func:`encode` に似ていますが、文字列を入力として受け取り、\
   エンコードされた文字列を返します。 *quotetabs* はオプション
   (デフォルトは 0 です) で、この値はそのまま :func:`encode` に\
   渡されます。


.. seealso::

   :mod:`mimify` モジュール
      MIME メッセージを処理するための汎用ユーティリティ。

   :mod:`base64` モジュール
      MIME base64 形式データのエンコードおよびデコード

