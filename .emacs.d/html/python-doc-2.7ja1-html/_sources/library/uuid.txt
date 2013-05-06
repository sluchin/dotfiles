
:mod:`uuid` --- RFC 4122 に準拠した UUID オブジェクト
=====================================================

.. module:: uuid
   :synopsis: RFC 4122 に準拠した UUID オブジェクト（汎用一意識別子）
.. moduleauthor:: Ka-Ping Yee <ping@zesty.ca>
.. sectionauthor:: George Yoshida <quiver@users.sourceforge.net>


.. versionadded:: 2.5

このモジュールでは immutable（変更不能）な :class:`UUID` オブジェクト（ :class:`UUID` クラス）と :rfc:`4122`
の定めるバージョン 1、3、4、5 の UUID を生成するための :func:`uuid1`, :func:`uuid2`, :func:`uuid3`,
:func:`uuid4`, :func:`uuid`, が提供されています。

もしユニークな ID が必要なだけであれば、おそらく :func:`uuid1` か :func:`uuid4` をコールすれば良いでしょう。
:func:`uuid1` はコンピュータのネットワークアドレスを含む UUID を生成するために
プライバシーを侵害するかもしれない点に注意してください。 :func:`uuid4` はランダムな UUID を生成します。


.. class:: UUID([hex[, bytes[, bytes_le[, fields[, int[, version]]]]]])

   32 桁の 16 進数文字列、 *bytes* に 16 バイトの文字列、 *bytes_le* 引数に 16 バイトのリトルエンディアンの文字列、 *field*
   引数に 6 つの整数のタプル（32ビット *time_low*, 16 ビット *time_mid*, 16ビット *time_hi_version*, 8ビット
   *clock_seq_hi_variant*, 8ビット *clock_seq_low*, 48ビット *node* ）、または *int* に一つの 128
   ビット整数のいずれかから UUID を生成します。16 進数が与えられた時、波括弧、ハイフン、それと URN 接頭辞は無視されます。
   例えば、これらの表現は全て同じ UUID を払い出します。 ::

      UUID('{12345678-1234-5678-1234-567812345678}')
      UUID('12345678123456781234567812345678')
      UUID('urn:uuid:12345678-1234-5678-1234-567812345678')
      UUID(bytes='\x12\x34\x56\x78'*4)
      UUID(bytes_le='\x78\x56\x34\x12\x34\x12\x78\x56' +
                    '\x12\x34\x56\x78\x12\x34\x56\x78')
      UUID(fields=(0x12345678, 0x1234, 0x5678, 0x12, 0x34, 0x567812345678))
      UUID(int=0x12345678123456781234567812345678)

   *hex*, *bytes*, *bytes_le*, *fields*, または *int* のうち、どれかただ一つだけが与えられなければいけません。
   *version* 引数はオプションです；与えられた場合、結果の UUID は与えられた *hex*, *bytes*,
   *bytes_le*, *fields*, または *int* をオーバーライドして、 RFC 4122 に準拠した variant と version
   ナンバーのセットを持つことになります。 *bytes_le*, *fields*, or *int*.

:class:`UUID` インスタンスは以下の読み取り専用属性を持ちます：


.. attribute:: UUID.bytes

   16 バイト文字列（バイトオーダーがビッグエンディアンの 6 つの整数フィールドを持つ）のUUID。


.. attribute:: UUID.bytes_le

   16 バイト文字列（ *time_low*, *time_mid*, *time_hi_version* をリトルエンディアンで持つ）の UUID。


.. attribute:: UUID.fields

   UUID の 6 つの整数フィールドを持つタプルで、これは 6 つの個別の属性と 2 つの派生した属性としても取得可能です。

   +------------------------------+---------------------------+
   | フィールド                   | 意味                      |
   +==============================+===========================+
   | :attr:`time_low`             | UUID の最初の 32 ビット   |
   +------------------------------+---------------------------+
   | :attr:`time_mid`             | UUID の次の 16 ビット     |
   +------------------------------+---------------------------+
   | :attr:`time_hi_version`      | UUID の次の 16 ビット     |
   +------------------------------+---------------------------+
   | :attr:`clock_seq_hi_variant` | UUID の次の 8 ビット      |
   +------------------------------+---------------------------+
   | :attr:`clock_seq_low`        | UUID の次の 8 ビット      |
   +------------------------------+---------------------------+
   | :attr:`node`                 | UUID の最後の 48 ビット   |
   +------------------------------+---------------------------+
   | :attr:`time`                 | 60 ビットのタイムスタンプ |
   +------------------------------+---------------------------+
   | :attr:`clock_seq`            | 14 ビットのシーケンス番号 |
   +------------------------------+---------------------------+


.. attribute:: UUID.hex

   32 文字の 16 進数文字列での UUID。


.. attribute:: UUID.int

   128 ビット整数での UUID。


.. attribute:: UUID.urn

   RFC 4122 で規定される URN での UUID。


.. attribute:: UUID.variant

   UUID の内部レイアウトを決定する UUID の variant。これは整数の定数 The UUID variant, which determines
   the internal layout of the UUID. This will be one of the integer constants
   :const:`RESERVED_NCS`, :const:`RFC_4122`, :const:`RESERVED_MICROSOFT`, 又は
   :const:`RESERVED_FUTURE` のいずれかになります。


.. attribute:: UUID.version

   UUID の version 番号（1 から 5、variant が :const:`RFC_4122` である場合だけ意味があります）。

The :mod:`uuid` モジュールには以下の関数があります：


.. function:: getnode()

   48 ビットの正の整数としてハードウェアアドレスを取得します。最初にこれを起動すると、別個のプログラムが立ち上がって非常に遅くなることがあります。
   もしハードウェアを取得する試みが全て失敗すると、ランダムな 48 ビットに RFC 4122 で推奨されているように 8 番目のビットを 1
   に設定した数を使います。 "ハードウェアアドレス" とはネットワークインターフェースの MAC アドレスを指し、
   複数のネットワークインターフェースを持つマシンの場合、それらのどれか一つの MAC アドレスが返るでしょう。

.. index:: single: getnode


.. function:: uuid1([node[, clock_seq]])

   UUID をホスト ID、シーケンス番号、現在時刻から生成します。 *node* が与えられなければ、 :func:`getnode` がハードウェアアドレス
   取得のために使われます。 *clock_seq* が与えられると、これはシーケンス番号として使われます；さもなくば 14
   ビットのランダムなシーケンス番号が選ばれます。

.. index:: single: uuid1


.. function:: uuid3(namespace, name)

   UUID を名前空間識別子（これは UUID です）と名前（文字列です）の MD5 ハッシュから生成します。

.. index:: single: uuid3


.. function:: uuid4()

   ランダムな UUID を生成します。

.. index:: single: uuid4


.. function:: uuid5(namespace, name)

   名前空間識別子（これは UUID です）と名前（文字列です）の SHA-1 ハッシュから生成します。

.. index:: single: uuid5

:mod:`uuid` モジュールは :func:`uuid3` または :func:`uuid5` で利用するために次の名前空間識別子を定義しています。


.. data:: NAMESPACE_DNS

   この名前空間が指定された場合、 *name* 文字列は完全修飾ドメイン名です。


.. data:: NAMESPACE_URL

   この名前空間が指定された場合、 *name* 文字列は URL です。


.. data:: NAMESPACE_OID

   この名前空間が指定された場合、 *name* 文字列は ISO OID です。


.. data:: NAMESPACE_X500

   この名前空間が指定された場合、 *name* 文字列は X.500 DN の DER またはテキスト出力形式です。

The :mod:`uuid` モジュールは以下の定数を :attr:`variant` 属性が取りうる値として定義しています：


.. data:: RESERVED_NCS

   NCS 互換性のために予約されています。


.. data:: RFC_4122

   :rfc:`4122` で与えられた UUID レイアウトを指定します。


.. data:: RESERVED_MICROSOFT

   Microsoft の互換性のために予約されています。


.. data:: RESERVED_FUTURE

   将来のために予約されています。


.. seealso::

   :rfc:`4122` - A Universally Unique IDentifier (UUID) URN Namespace
      この仕様は UUID のための Uniform Resource Name 名前空間、 UUID の内部フォーマットと UUID の生成方法を定義しています。


.. _uuid-example:

例
--

典型的な :mod:`uuid` モジュールの利用方法を示します：  ::

   >>> import uuid

   # UUID をホスト ID と現在時刻に基づいて生成します
   >>> uuid.uuid1()
   UUID('a8098c1a-f86e-11da-bd1a-00112444be1e')

   # 名前空間 UUID と名前の MD5 ハッシュを使って UUID を生成します
   >>> uuid.uuid3(uuid.NAMESPACE_DNS, 'python.org')
   UUID('6fa459ea-ee8a-3ca4-894e-db77e160355e')

   # ランダムな UUID を作成します
   >>> uuid.uuid4()
   UUID('16fd2706-8baf-433b-82eb-8c7fada847da')

   # 名前空間 UUID と名前の SHA-1 ハッシュを使って UUID を生成します
   >>> uuid.uuid5(uuid.NAMESPACE_DNS, 'python.org')
   UUID('886313e1-3b8a-5372-9b90-0c9aee199e5d')

   # 16 進数文字列から UUID を生成します（波括弧とハイフンは無視されます）
   >>> x = uuid.UUID('{00010203-0405-0607-0809-0a0b0c0d0e0f}')

   # UUID を標準的な 16 進数の文字列に変換します
   >>> str(x)
   '00010203-0405-0607-0809-0a0b0c0d0e0f'

   # 生の 16 バイトの UUID を取得します
   >>> x.bytes
   '\x00\x01\x02\x03\x04\x05\x06\x07\x08\t\n\x0b\x0c\r\x0e\x0f'

   # 16 バイトの文字列から UUID を生成します
   >>> uuid.UUID(bytes=x.bytes)
   UUID('00010203-0405-0607-0809-0a0b0c0d0e0f')

