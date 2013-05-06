:mod:`json` --- JSON エンコーダおよびデコーダ
=============================================

.. module:: json
   :synopsis: JSON 形式のエンコードおよびデコード
.. moduleauthor:: Bob Ippolito <bob@redivi.com>
.. sectionauthor:: Bob Ippolito <bob@redivi.com>
.. versionadded:: 2.6

`JSON (JavaScript Object Notation) <http://json.org>`_ は JavaScript 文法
(ECMA-262 3rd edition) のサブセットで軽量のデータ交換形式として使われています。

:mod:`json` の API は標準ライブラリの :mod:`marshal` や :mod:`pickle`
のユーザに馴染み深いものです。

基本的な Python オブジェクト階層のエンコーディング::

    >>> import json
    >>> json.dumps(['foo', {'bar': ('baz', None, 1.0, 2)}])
    '["foo", {"bar": ["baz", null, 1.0, 2]}]'
    >>> print json.dumps("\"foo\bar")
    "\"foo\bar"
    >>> print json.dumps(u'\u1234')
    "\u1234"
    >>> print json.dumps('\\')
    "\\"
    >>> print json.dumps({"c": 0, "b": 0, "a": 0}, sort_keys=True)
    {"a": 0, "b": 0, "c": 0}
    >>> from StringIO import StringIO
    >>> io = StringIO()
    >>> json.dump(['streaming API'], io)
    >>> io.getvalue()
    '["streaming API"]'

コンパクトなエンコーディング::

    >>> import json
    >>> json.dumps([1,2,3,{'4': 5, '6': 7}], separators=(',',':'))
    '[1,2,3,{"4":5,"6":7}]'

見やすい表示::

    >>> import json
    >>> print json.dumps({'4': 5, '6': 7}, sort_keys=True, indent=4)
    {
        "4": 5,
        "6": 7
    }

JSON のデコーディング::

    >>> import json
    >>> json.loads('["foo", {"bar":["baz", null, 1.0, 2]}]')
    [u'foo', {u'bar': [u'baz', None, 1.0, 2]}]
    >>> json.loads('"\\"foo\\bar"')
    u'"foo\x08ar'
    >>> from StringIO import StringIO
    >>> io = StringIO('["streaming API"]')
    >>> json.load(io)
    [u'streaming API']

JSON オブジェクトのデコーディング方法を誂える::

    >>> import json
    >>> def as_complex(dct):
    ...     if '__complex__' in dct:
    ...         return complex(dct['real'], dct['imag'])
    ...     return dct
    ...
    >>> json.loads('{"__complex__": true, "real": 1, "imag": 2}',
    ...     object_hook=as_complex)
    (1+2j)
    >>> import decimal
    >>> json.loads('1.1', parse_float=decimal.Decimal)
    Decimal('1.1')

:class:`JSONEncoder` の拡張::

    >>> import json
    >>> class ComplexEncoder(json.JSONEncoder):
    ...     def default(self, obj):
    ...         if isinstance(obj, complex):
    ...             return [obj.real, obj.imag]
    ...         return json.JSONEncoder.default(self, obj)
    ...
    >>> dumps(2 + 1j, cls=ComplexEncoder)
    '[2.0, 1.0]'
    >>> ComplexEncoder().encode(2 + 1j)
    '[2.0, 1.0]'
    >>> list(ComplexEncoder().iterencode(2 + 1j))
    ['[', '2.0', ', ', '1.0', ']']


.. highlight:: none

シェルから json.tool を使って妥当性チェックをして見やすく表示::

    $ echo '{"json":"obj"}' | python -mjson.tool
    {
        "json": "obj"
    }
    $ echo '{ 1.2:3.4}' | python -mjson.tool
    Expecting property name: line 1 column 2 (char 2)

.. highlight:: python

.. note::

   このモジュールのデフォルト設定で生成される JSON は YAML のサブセットですので、
   その直列化にも使うことができるでしょう。


基本的な使い方
---------------

.. function:: dump(obj, fp[, skipkeys[, ensure_ascii[, check_circular[, allow_nan[, cls[, indent[, separators[, encoding[, default[, **kw]]]]]]]]]])

   *obj* を JSON 形式の *fp* (``.write()`` をサポートするファイル的オブジェクト)
   へのストリームとして直列化します。

   *skipkeys* が ``True`` (デフォルトは ``False``) ならば、基本型
   (:class:`str`, :class:`unicode`, :class:`int`, :class:`long`,
   :class:`float`, :class:`bool`, ``None``) 以外の辞書のキーは
   :exc:`TypeError` を送出せずに読み飛ばされます。

   *ensure_ascii* が ``False`` (デフォルトは ``True``) ならば、
   *fp* へ書き込まれるチャンクは通常の Python における :class:`str` から
   :class:`unicode` への型強制ルールに従って :class:`unicode`
   インスタンスになることがあります。 ``fp.write()`` が
   (:func:`codecs.getwriter` のように) 前もって :class:`unicode`
   に対応していると判っているもの以外では恐らくこれによってエラーが起こるでしょう。

   *check_circular* が ``False`` (デフォルトは ``True``) ならば、
   コンテナ型の循環参照チェックが省かれ、循環参照があれば :exc:`OverflowError`
   (またはもっと悪い結果) に終わります。

   *allow_nan* が ``False`` (デフォルトは ``True``) ならば、許容範囲外の
   :class:`float` 値 (``nan``, ``inf``, ``-inf``) を直列化しようとした場合、
   JSON 仕様を厳格に守って JavaScript の等価なもの (``NaN``, ``Infinity``,
   ``-Infinity``) を使うことなく :exc:`ValueError` になります。

   *indent* が非負の整数であれば、JSON の配列要素とオブジェクトメンバはその\
   インデントレベルで見やすく表示されます。インデントレベルが 0 か負であれば
   改行だけが挿入されます。 ``None`` (デフォルト)
   では最もコンパクトな表現が選択されます。

   *separators* がタプル ``(item_separator, dict_separator)`` ならば、
   デフォルト区切り文字 ``(', ', ': ')`` の代わりに使われます。
   ``(',', ':')`` が最もコンパクトな JSON の表現です。

   *encoding* は文字列のエンコーディングで、デフォルトは UTF-8 です。

   *default(obj)* は関数で、 *obj* の直列化可能なバージョンを返すか、さもなくば
   :exc:`TypeError` を送出します。デフォルトでは単に :exc:`TypeError`
   を送出します。

   カスタマイズされた :class:`JSONEncoder` のサブクラス
   (たとえば追加の型を直列化するように :meth:`default` メソッドをオーバーライドしたもの)
   を使うには、 *cls* キーワード引数に指定します;
   指定しなければ :class:`JSONEncoder` が使われます。

   .. note::

      :mod:`pickle` や :mod:`marshal` と違って、JSON は枠付きのプロトコル (framed protocol) ではないので、
      同じ *fp* に対して繰り返し :func:`dump` を呼び、
      たくさんのオブジェクトを直列化しようとすると、不正な JSON ファイルが作られてしまいます。

.. function:: dumps(obj[, skipkeys[, ensure_ascii[, check_circular[, allow_nan[, cls[, indent[, separators[, encoding[, default[, **kw]]]]]]]]]])

   *obj* を JSON 形式の :class:`str` に直列化します。

   *ensure_ascii* が ``False`` ならば、返値は :class:`unicode`
   インスタンスになります。その他の引数は :func:`dump` におけるものと同じ意味です。


.. function:: load(fp[, encoding[, cls[, object_hook[, parse_float[, parse_int[, parse_constant[, object_pairs_hook[, **kw]]]]]]]])

   直列化された *fp* (``.read()`` をサポートするファイル的オブジェクトで
   JSON 文書を収めたもの) の内容を Python オブジェクトに戻します。

   *fp* の内容が ASCII に基づいたしかし UTF-8 ではないエンコーディング (たとえば
   latin-1) を使っているならば、適切な *encoding* 名が指定されなければなりません。
   エンコーディングが ASCII に基づかないもの (UCS-2 など) であることは許されないので、
   ``codecs.getreader(encoding)(fp)`` というように包むか、または単に
   :class:`unicode` オブジェクトにデコードしたものを :func:`loads` に渡して下さい。

   *object_hook* はオプションで渡す関数で、全てのオブジェクトリテラルのデコード結果
   (:class:`dict`) に対して呼ばれます。 *object_hook* の返り値は :class:`dict`
   の代わりに使われます。この機能は独自のデコーダ (たとえば JSON-RPC クラスヒンティング)
   を実装するのに使えます。

   *object_pairs_hook* はオプションで渡す関数で、ペアの順序付きリストのデコード結果に対して呼ばれます。
   *object_pairs_hook* の返り値は :class:`dict` の代わりに使われます。
   この機能はキーと値のデコードされる順序に依存する独自のデコーダ (たとえば :func:`collections.OrderedDict` は挿入の順序を記憶します) を実装するのに使えます。
   *object_hook* も定義されている場合は、 *object_pairs_hook* が優先して使用されます。

   .. versionchanged:: 2.7
      *object_pairs_hook* のサポートを追加しました。

   *parse_float* は、もし指定されれば、全てのデコードされる JSON
   の浮動小数点数文字列に対して呼ばれます。デフォルトでは、 ``float(num_str)``
   と等価です。これは JSON 浮動小数点数に対して他のデータ型やパーサ
   (たとえば :class:`decimal.Decimal`) を使うのに使えます。

   *parse_int* は、もし指定されれば、全てのデコードされる JSON
   の整数文字列に対して呼ばれます。デフォルトでは、 ``int(num_str)``
   と等価です。これは JSON 整数に対して他のデータ型やパーサ
   (たとえば :class:`float`) を使うのに使えます。

   *parse_constant* は、もし指定されれば、次の文字列に対して呼ばれます:
   ``'-Infinity'``, ``'Infinity'``, ``'NaN'``, ``'null'``, ``'true'``,
   ``'false'`` 。これは不正な JSON 数値に遭遇したときに例外を送出するのに使えます。

   カスタマイズされた :class:`JSONDecoder` のサブクラスを使うには、
   *cls* キーワード引数に指定します; 指定しなかった場合は :class:`JSONDecoder` が使われます。
   追加のキーワード引数はこのクラスのコンストラクタに引き渡されます。


.. function:: loads(s[, encoding[, cls[, object_hook[, parse_float[, parse_int[, parse_constant[, object_pairs_hook[, **kw]]]]]]]])

   直列化された *s* (:class:`str` または :class:`unicode` インスタンスで
   JSON 文書を含むもの) を Python オブジェクトに戻します。

   *s* が ASCII に基づいたしかし UTF-8 ではないエンコーディング (たとえば
   latin-1) でエンコードされた :class:`str` ならば、適切な *encoding*
   名が指定されなければなりません。エンコーディングが ASCII に基づかないもの
   (UCS-2 など) であることは許されないので、まず :class:`unicode`
   にデコードして下さい。

   その他の引数は :func:`load` におけるものと同じ意味です。


エンコーダおよびデコーダ
-------------------------

.. class:: JSONDecoder([encoding[, object_hook[, parse_float[, parse_int[, parse_constant[, strict[, object_pairs_hook]]]]]]])

   単純な JSON デコーダ。

   デフォルトではデコーディングの際、以下の変換を行います。

   +---------------+-------------------+
   | JSON          | Python            |
   +===============+===================+
   | object        | dict              |
   +---------------+-------------------+
   | array         | list              |
   +---------------+-------------------+
   | string        | unicode           |
   +---------------+-------------------+
   | number (int)  | int, long         |
   +---------------+-------------------+
   | number (real) | float             |
   +---------------+-------------------+
   | true          | True              |
   +---------------+-------------------+
   | false         | False             |
   +---------------+-------------------+
   | null          | None              |
   +---------------+-------------------+

   また、このデコーダは ``NaN``, ``Infinity``, ``-Infinity`` を対応する
   ``float`` の値として、JSON の仕様からは外れますが、理解します。

   *encoding* はこのインスタンスでデコードされる :class:`str`
   オブジェクトを解釈するために使われるエンコーディング (デフォルトは UTF-8)
   を定めます。 :class:`unicode` オブジェクトのデコーディングには影響を与えません。

   注意して欲しいのは現状では ASCII のスーパーセットであるようなエンコーディングだけ
   うまく動くということです。他のエンコーディングの文字列は :class:`unicode`
   にして渡して下さい。

   *object_hook* は、もし指定されれば、全てのデコードされた JSON
   オブジェクトに対して呼ばれその返値は与えられた :class:`dict`
   の代わりに使われます。この機能は独自の脱直列化 (たとえば JSON-RPC
   クラスヒンティングをサポートするような) を提供するのに使えます。

   *object_pairs_hook* は、もし指定されれば、全てのペアの順序付きリストに
   デコードされた JSON オブジェクトに対して呼ばれます。
   *object_pairs_hook* の返り値は :class:`dict` の代わりに使われます。
   この機能はキーと値のデコードされる順序に依存する独自のデコーダ (たとえば :func:`collections.OrderedDict` は挿入の順序を記憶します) を実装するのに使えます。
   *object_hook* も定義されている場合は、 *object_pairs_hook* が優先して使用されます。

   .. versionchanged:: 2.7
      *object_pairs_hook* のサポートを追加しました。

   *parse_float* は、もし指定されれば、全てのデコードされる JSON
   の浮動小数点数文字列に対して呼ばれます。デフォルトでは、 ``float(num_str)``
   と等価です。これは JSON 浮動小数点数に対して他のデータ型やパーサ
   (たとえば :class:`decimal.Decimal`) を使うのに使えます。

   *parse_int* は、もし指定されれば、全てのデコードされる JSON
   の整数文字列に対して呼ばれます。デフォルトでは、 ``int(num_str)``
   と等価です。これは JSON 整数に対して他のデータ型やパーサ
   (たとえば :class:`float`) を使うのに使えます。

   *parse_constant* は、もし指定されれば、次の文字列に対して呼ばれます:
   ``'-Infinity'``, ``'Infinity'``, ``'NaN'``, ``'null'``, ``'true'``,
   ``'false'`` 。これは不正な JSON 数値に遭遇したときに例外を送出するのに使えます。

   *strict* が ``False`` (デフォルトは ``True``) の場合、制御文字を文字列に含めることができます。
   ここで言う制御文字とは、 ``'\t'`` (タブ)、 ``'\n'`` 、 ``'\r'`` 、 ``'0'`` を含む 0-31 の範囲のコードを持つ文字のことです。

   .. method:: decode(s)

      *s* (:class:`str` または :class:`unicode` インスタンスで
      JSON 文書を含むもの) の Python 表現を返します。

   .. method:: raw_decode(s)

      *s* (:class:`str` または :class:`unicode` インスタンスで
      JSON 文書で始まるもの) から JSON 文書をデコードし、Python 表現と
      *s* の文書の終わるところのインデックスからなる 2 要素のタプルを返します。

      このメソッドは後ろに余分なデータを従えた文字列から JSON
      文書をデコードするのに使えます。


.. class:: JSONEncoder([skipkeys[, ensure_ascii[, check_circular[, allow_nan[, sort_keys[, indent[, separators[, encoding[, default]]]]]]]]])

   Python データ構造に対する拡張可能な JSON エンコーダ。

   デフォルトでは以下のオブジェクトと型をサポートします:

   +-------------------+---------------+
   | Python            | JSON          |
   +===================+===============+
   | dict              | object        |
   +-------------------+---------------+
   | list, tuple       | array         |
   +-------------------+---------------+
   | str, unicode      | string        |
   +-------------------+---------------+
   | int, long, float  | number        |
   +-------------------+---------------+
   | True              | true          |
   +-------------------+---------------+
   | False             | false         |
   +-------------------+---------------+
   | None              | null          |
   +-------------------+---------------+

   このクラスを拡張して他のオブジェクトも認識するようにするには、
   サブクラスを作って :meth:`default` メソッドを次のように実装します。
   もう一つ別のメソッドでオブジェクト ``o`` に対する直列化可能なオブジェクトを\
   返すものを呼び出すようにします。
   変換できない時はスーパークラスの実装を (:exc:`TypeError` を送出させるために)
   呼ばなければなりません。

   *skipkeys* が ``False`` (デフォルト) ならば、str, int, long, float, None
   以外のキーをエンコードする試みは :exc:`TypeError` に終わります。
   *skipkeys* が ``True`` の場合は、それらのアイテムは単に読み飛ばされます。

   *ensure_ascii* が ``True`` (デフォルト) ならば、入ってくるユニコード文字が\
   全てエスケープされた :class:`str` オブジェクトが出力になることが保証されます。
   *ensure_ascii* が ``False`` の場合は、出力はユニコードオブジェクトです。

   *check_circular* が ``True`` (デフォルト) ならば、リスト、辞書および\
   自作でエンコードしたオブジェクトは循環参照がないかエンコード中にチェックされ、
   無限再帰 (これは :exc:`OverflowError` を引き起こします) を防止します。
   ``True`` でない場合は、そういったチェックは施されません。

   *allow_nan* が ``True`` (デフォルト) ならば、 ``NaN``, ``Infinity``,
   ``-Infinity`` はそのままエンコードされます。この振る舞いは JSON
   仕様に従っていませんが、大半の JavaScript ベースのエンコーダ、デコーダと
   矛盾しません。 ``True`` でない場合は、そのような浮動小数点数をエンコードすると
   :exc:`ValueError` が送出されます。

   *sort_keys* が ``True`` (デフォルトは ``False``) ならば、辞書の出力がキーでソートされます。
   これは JSON の直列化がいつでも比較できるようになるので回帰試験の際に便利です。

   *indent* が非負の整数であれば (デフォルトでは ``None`` です)、JSON
   の配列要素とオブジェクトメンバはそのインデントレベルで見やすく表示されます。
   インデントレベルが 0 ならば改行だけが挿入されます。 ``None``
   では最もコンパクトな表現が選択されます。

   *separators* はもし指定するなら ``(item_separator, key_separator)``
   というタプルでなければなりません。デフォルトは ``(', ', ': ')`` です。
   最もコンパクトな JSON の表現を得たければ空白を削った ``(',', ':')``
   を指定すればいいでしょう。

   *default* はもし指定するなら関数で、それがなければ直列化できないオブジェクトに\
   対して呼び出されます。その関数はオブジェクトを JSON でエンコードできるバージョンに\
   して返すか、さもなければ :exc:`TypeError` を送出しなければなりません。

   *encoding* が ``None`` でなければ、入力文字列は全て JSON
   エンコーディングに先立ってこのエンコーディングでユニコードに変換されます。
   デフォルトは UTF-8 です。


   .. method:: default(o)

      このメソッドをサブクラスで実装する際には *o*
      に対して直列化可能なオブジェクトを返すか、基底クラスの実装を
      (:exc:`TypeError` を送出するために) 呼び出すかします。

      たとえば、任意のイテレータをサポートするために、次のように実装します::

         def default(self, o):
            try:
                iterable = iter(o)
            except TypeError:
                pass
            else:
                return list(iterable)
            return JSONEncoder.default(self, o)


   .. method:: encode(o)

      Python データ構造 *o* の JSON 文字列表現を返します。たとえば::

        >>> JSONEncoder().encode({"foo": ["bar", "baz"]})
        '{"foo": ["bar", "baz"]}'


   .. method:: iterencode(o)

      与えられたオブジェクト *o* をエンコードし、得られた文字列表現ごとに
      yield します。たとえば::

            for chunk in JSONEncoder().iterencode(bigobject):
                mysocket.write(chunk)
