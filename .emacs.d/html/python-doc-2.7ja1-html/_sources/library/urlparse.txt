:mod:`urlparse` --- URL を解析して構成要素にする
================================================

.. module:: urlparse
   :synopsis: URL の解析と構築

.. index::
   single: WWW
   single: World Wide Web
   single: URL
   pair: URL; parsing
   pair: relative; URL

.. .. note::
   The :mod:`urlparse` module is renamed to :mod:`urllib.parse` in Python 3.0.
   The :term:`2to3` tool will automatically adapt imports when converting
   your sources to 3.0.

.. note::
   :mod:`urlparse` モジュールは Python 3 では :mod:`urllib.parse` にリネームされました。
   :term:`2to3` ツールはソースコードのimportを自動的に Python 3 用に修正します。


このモジュールでは URL (Uniform Resource Locator) 文字列をその構成要素 (アドレススキーム、ネットワーク上の位置、パスその他)
に分解したり、構成要素を URL に組みなおしたり、"相対 URL (relative URL)" を指定した "基底 URL (base URL)"
に基づいて絶対 URL に変換するための標準的なインタフェースを定義しています。

このモジュールは相対 URL のインターネット RFC に対応するように設計されました (そして RFC の初期ドラフトのバグを発見しました！)。
サポートされる URL スキームは以下の通りです: ``file``, ``ftp``, ``gopher``, ``hdl``, ``http``,
``https``, ``imap``, ``mailto``, ``mms``, ``news``,  ``nntp``, ``prospero``,
``rsync``, ``rtsp``, ``rtspu``,  ``sftp``, ``shttp``, ``sip``, ``sips``,
``snews``, ``svn``,  ``svn+ssh``, ``telnet``, ``wais`` 。

.. versionadded:: 2.5
   ``sftp`` および ``sips`` スキームのサポートが追加されました.

.. seealso::

   最新バージョンの `urlparse module Python source code
   <http://svn.python.org/view/python/branches/release27-maint/Lib/urlparse.py?view=markup>`_

:mod:`urlparse` モジュールには以下の関数が定義されています。


.. function:: urlparse(urlstring[, scheme[, allow_fragments]])

   URL を解釈して 6 つの構成要素にし、6 要素のタプルを返します。このタプルは URL の一般的な構造:
   ``scheme://netloc/path;parameters?query#fragment`` に対応しています。
   各タプル要素は文字列で、空の場合もあります。構成要素がさらに小さい要素に分解されることはありません (例えば
   ネットワーク上の位置は単一の文字列になります)。また % によるエスケープは展開されません。上で示された区切り文字がタプルの各要素の一部分
   として含まれることはありませんが、 *path* 要素の先頭のスラッシュがある場合には例外です。たとえば以下のようになります。 ::

      >>> from urlparse import urlparse
      >>> o = urlparse('http://www.cwi.nl:80/%7Eguido/Python.html')
      >>> o   # doctest: +NORMALIZE_WHITESPACE
      ParseResult(scheme='http', netloc='www.cwi.nl:80', path='/%7Eguido/Python.html',
                  params='', query='', fragment='')
      >>> o.scheme
      'http'
      >>> o.port
      80
      >>> o.geturl()
      'http://www.cwi.nl:80/%7Eguido/Python.html'


   :rfc:`1808` にある文法仕様に基づき、 urlparse は '//' で始まる場合にのみ
   netloc を認識します。それ以外の場合は、入力は相対URLであると推定され、
   path 部分で始まることになります。

       >>> from urlparse import urlparse
       >>> urlparse('//www.cwi.nl:80/%7Eguido/Python.html')
       ParseResult(scheme='', netloc='www.cwi.nl:80', path='/%7Eguido/Python.html',
                  params='', query='', fragment='')
       >>> urlparse('www.cwi.nl:80/%7Eguido/Python.html')
       ParseResult(scheme='', netloc='', path='www.cwi.nl:80/%7Eguido/Python.html',
                  params='', query='', fragment='')
       >>> urlparse('help/Python.html')
       ParseResult(scheme='', netloc='', path='help/Python.html', params='',
                  query='', fragment='')

   *scheme* 引数が指定されている場合、標準のアドレススキームを表し、アドレススキームを指定していない URL に対してのみ
   使われます。この引数の標準の値は空文字列です。

   *allow_fragments* 引数が偽の場合、URL のアドレススキームがフラグメント指定をサポートしていても指定できなくなります。
   この引数の標準の値は :const:`True` です。

   戻り値は実際には :class:`tuple` のサブクラスのインスタンスです。このクラスには以下の読み出し専用の便利な属性が追加されています。

   +------------------+------------+-------------------------------------+--------------------------+
   | 属性             | インデクス | 値                                  | 指定されなかった場合の値 |
   +==================+============+=====================================+==========================+
   | :attr:`scheme`   | 0          | URL スキーム                        | 空文字列                 |
   +------------------+------------+-------------------------------------+--------------------------+
   | :attr:`netloc`   | 1          | ネットワーク上の位置                | 空文字列                 |
   +------------------+------------+-------------------------------------+--------------------------+
   | :attr:`path`     | 2          | 階層的パス                          | 空文字列                 |
   +------------------+------------+-------------------------------------+--------------------------+
   | :attr:`params`   | 3          | 最後のパス要素に対するパラメータ    | 空文字列                 |
   +------------------+------------+-------------------------------------+--------------------------+
   | :attr:`query`    | 4          | クエリ要素                          | 空文字列                 |
   +------------------+------------+-------------------------------------+--------------------------+
   | :attr:`fragment` | 5          | フラグメント指定子                  | 空文字列                 |
   +------------------+------------+-------------------------------------+--------------------------+
   | :attr:`username` |            | ユーザ名                            | :const:`None`            |
   +------------------+------------+-------------------------------------+--------------------------+
   | :attr:`password` |            | パスワード                          | :const:`None`            |
   +------------------+------------+-------------------------------------+--------------------------+
   | :attr:`hostname` |            | ホスト名 (小文字)                   | :const:`None`            |
   +------------------+------------+-------------------------------------+--------------------------+
   | :attr:`port`     |            | ポート番号を表わす整数 (もしあれば) | :const:`None`            |
   +------------------+------------+-------------------------------------+--------------------------+

   結果オブジェクトのより詳しい情報は :ref:`urlparse-result-object` 節を参照してください。

   .. versionchanged:: 2.5
      戻り値に属性が追加されました.

   .. versionchanged:: 2.7
      IPv6 の URL パースに対応しました。



.. function:: parse_qs(qs[, keep_blank_values[, strict_parsing]])

   文字列引数として渡されたクエリ文字列  (:mimetype:`application/x-www-form-urlencoded` 型のデータ)
   を解釈します。解釈されたデータを辞書として返します。
   辞書のキーは一意なクエリ変数名で、値は各変数名に対する値からなるリストです。

   オプションの引数 *keep_blank_values* は、パーセントエンコードされたクエリの中の
   値が入っていないクエリの値を空白文字列と見なすかどうかを示すフラグです。
   値が真であれば、値の入っていないフィールドは空文字列のままになります。標準では偽で、値の入っていない
   フィールドを無視し、そのフィールドはクエリに含まれていないものとして扱います。

   オプションの引数 *strict_pasing* はパース時のエラーをどう扱うかを決めるフラグです。値が偽なら (標準の設定です)、
   エラーは暗黙のうちに無視します。値が真なら :exc:`ValueError`  例外を送出します。

   辞書等をクエリ文字列に変換する場合は :func:`urllib. urlencode` 関数を使用してください。

   .. versionadded:: 2.6
      :mod:`cgi` モジュールからコピーされてきました。


.. function:: parse_qsl(qs[, keep_blank_values[, strict_parsing]])

   文字列引数として渡されたクエリ文字列  (:mimetype:`application/x-www-form-urlencoded` 型のデータ) を
   解釈します。解釈されたデータは名前と値のペアからなるリストです。

   オプションの引数 *keep_blank_values* は、パーセントエンコードされたクエリ中で値の入っていないものを空文字列と見なすかどうか
   を示すフラグです。値が真であれば、値の入っていないフィールドは空文字列のままになります。標準では偽で、値の入っていない
   フィールドを無視し、そのフィールドはクエリに含まれていないものとして扱います。

   オプションの引数 *strict_pasing* はパース時のエラーをどう扱うかを決めるフラグです。値が偽なら (標準の設定です)、
   エラーは暗黙のうちに無視します。値が真なら :exc:`ValueError`  例外を送出します。

   ペアのリストからクエリ文字列を生成する場合には :mod:`urllib`.urlencode() 関数を使用します。

   .. versionadded:: 2.6
      :mod:`cgi` モジュールからコピーされてきました。


.. function:: urlunparse(parts)

   ``urlparse()`` が返すような形式のタプルから URL を構築します。
   *parts* 引数は任意の 6 要素イテラブルです。
   解析された元の URL が、不要な区切り文字を持っていた場合には、多少違いはあるが等価な URL になるかもしれません。 (例えばクエリ内容が空の ?
   のようなもので、RFC はこれらを等価だと述べています。)


.. function:: urlsplit(urlstring[, scheme[, allow_fragments]])

   :func:`urlparse` に似ていますが、URL から params を切り離しません。このメソッドは通常、URL の *path*
   部分において、各セグメントにパラメタ指定をできるようにした最近の URL 構文 (:rfc:`2396` 参照) が必要な
   場合に、 :func:`urlparse` の代わりに使われます。パスセグメントとパラメタを分割するためには分割用の関数が必要です。この関数は 5
   要素のタプル: (アドレススキーム、ネットワーク上の位置、パス、クエリ、フラグメント指定子)  を返します。

   戻り値は実際には :class:`tuple` のサブクラスのインスタンスです。このクラスには以下の読み出し専用の便利な属性が追加されています。

   +------------------+------------+-------------------------------------+--------------------------+
   | 属性             | インデクス | 値                                  | 指定されなかった場合の値 |
   +==================+============+=====================================+==========================+
   | :attr:`scheme`   | 0          | URL スキーム                        | 空文字列                 |
   +------------------+------------+-------------------------------------+--------------------------+
   | :attr:`netloc`   | 1          | ネットワーク上の位置                | 空文字列                 |
   +------------------+------------+-------------------------------------+--------------------------+
   | :attr:`path`     | 2          | 階層的パス                          | 空文字列                 |
   +------------------+------------+-------------------------------------+--------------------------+
   | :attr:`query`    | 3          | クエリ要素                          | 空文字列                 |
   +------------------+------------+-------------------------------------+--------------------------+
   | :attr:`fragment` | 4          | フラグメント指定子                  | 空文字列                 |
   +------------------+------------+-------------------------------------+--------------------------+
   | :attr:`username` |            | ユーザ名                            | :const:`None`            |
   +------------------+------------+-------------------------------------+--------------------------+
   | :attr:`password` |            | パスワード                          | :const:`None`            |
   +------------------+------------+-------------------------------------+--------------------------+
   | :attr:`hostname` |            | ホスト名 (小文字)                   | :const:`None`            |
   +------------------+------------+-------------------------------------+--------------------------+
   | :attr:`port`     |            | ポート番号を表わす整数 (もしあれば) | :const:`None`            |
   +------------------+------------+-------------------------------------+--------------------------+

   結果オブジェクトのより詳しい情報は :ref:`urlparse-result-object` 節を参照してください。

   .. versionadded:: 2.2

   .. versionchanged:: 2.5
      戻り値に属性が追加されました.


.. function:: urlunsplit(parts)

   ``urlsplit()`` が返すような形式のタプル中のエレメントを組み合わせて、文字列の完全な URL にします。 *parts* 引数は任意の 5
   要素イテラブルです。解析された元の URL が、不要な区切り文字を持っていた場合には、多少違いはあるが等価な URL になるかもしれません。
   (例えばクエリ内容が空の ? のようなもので、RFC はこれらを等価だと述べています。)

   .. versionadded:: 2.2


.. function:: urljoin(base, url[, allow_fragments])

   "基底 URL"(*base*)と別のURL(*url*)を組み合わせて、完全な URL ("絶対 URL") を構成します。
   ぶっちゃけ、この関数は基底 URL の要素、特にアドレススキーム、ネットワーク上の位置、およびパス (の一部) を使って、相対 URL に
   ない要素を提供します。以下の例のようになります。 ::

      >>> from urlparse import urljoin
      >>> urljoin('http://www.cwi.nl/%7Eguido/Python.html', 'FAQ.html')
      'http://www.cwi.nl/%7Eguido/FAQ.html'

   *allow_fragments* 引数は ``urlparse()`` における引数と同じ意味とデフォルトを持ちます。

   .. note::

      .. If *url* is an absolute URL (that is, starting with ``//`` or ``scheme://``),
         the *url*'s host name and/or scheme will be present in the result.  For example:

      *url* が(``//`` か ``scheme://`` で始まっている)絶対URLであれば、
      その *url* のホスト名と/もしくは scheme は、結果に反映されます。例えば:

   .. doctest::

      >>> urljoin('http://www.cwi.nl/%7Eguido/Python.html',
      ...         '//www.python.org/%7Eguido')
      'http://www.python.org/%7Eguido'

   .. If you do not want that behavior, preprocess the *url* with :func:`urlsplit` and
      :func:`urlunsplit`, removing possible *scheme* and *netloc* parts.

   もしこの動作が望みのものでない場合は、 *url* を :func:`urlsplit` と :func:`urlunsplit`
   で先に処理して、 *scheme* と *netloc* を削除してください。



.. function:: urldefrag(url)

   *url* がフラグメント指定子を含む場合、フラグメント指定子を持たないバージョンに修正された *url* と、別の文字列に分割
   されたフラグメント指定子を返します。 *url* 中にフラグメント指定子がない場合、そのままの *url* と空文字列を返します。


.. seealso::

   :rfc:`3986` - Uniform Resource Identifiers
      これが現在の標準規格(STD66)です。 urlparse モジュールに対するすべての変更は、
      この規格を確認しなければなりません。
      後方互換性のため、あるいは、メジャーなブラウザに見られる事実上標準となった
      URL 解析への要求のために、この規格から外れている部分があります。

   :rfc:`2732` - Format for Literal IPv6 Addresses in URL's.
      この規格は IPv6 の URL を解析するときの要求事項を記述しています。

   :rfc:`2396` - Uniform Resource Identifiers (URI): Generic Syntax
      この RFC では Uniform Resource Name (URN) と Uniform Resource Locator (URL)
      の両方に対する一般的な文法的要求事項を記述しています。

   :rfc:`2368` - The mailto URL scheme.
      mailto URL スキームに対する文法的要求事項.
 
   :rfc:`1808` - Relative Uniform Resource Locators
      この RFC には絶対 URL と相対 URL を結合するための規則がボーダケースの取扱い方を決定する "異常な例" つきで収められています。

   :rfc:`1738` - Uniform Resource Locators (URL)
      この RFC では絶対 URL の形式的な文法と意味付けを仕様化しています。


.. _urlparse-result-object:

:func:`urlparse` および :func:`urlsplit` の結果
-----------------------------------------------

:func:`urlparse` および :func:`urlsplit` から得られる結果オブジェクトはそれぞれ :class:`tuple`
型のサブクラスです。これらのクラスはそれぞれの関数の説明の中で述べたような属性とともに、追加のメソッドを一つ提供しています。


.. method:: ParseResult.geturl()

   再結合された形で元の URL の文字列を返します。この文字列は元の URL とは次のような点で異なるかもしれません。スキームは常に小文字に正規化されます。
   また空の要素は省略されます。特に、空のパラメータ、クエリ、フラグメント識別子は取り除かれます。

   このメソッドの結果は再び解析に回されたとしても不動点となります。

      >>> import urlparse
      >>> url = 'HTTP://www.Python.org/doc/#'

      >>> r1 = urlparse.urlsplit(url)
      >>> r1.geturl()
      'http://www.Python.org/doc/'

      >>> r2 = urlparse.urlsplit(r1.geturl())
      >>> r2.geturl()
      'http://www.Python.org/doc/'

   .. versionadded:: 2.5

以下のクラスが解析結果の実装を提供します。


.. class:: BaseResult

   具体的な結果クラスたちの基底クラスです。このクラスがほとんどの属性の定義を与えます。しかし :meth:`geturl` メソッドは提供しません。この
   クラスは :class:`tuple` から派生していますが、 :meth:`__init__` や :meth:`__new__` をオーバーライドしませ
   ん。


.. class:: ParseResult(scheme, netloc, path, params, query, fragment)

   :func:`urlparse` の結果のための具体クラスです。 :meth:`__new__` メソッドをオーバーライドして正しい個数の引数が
   引き渡されたことを確認するようにしています。


.. class:: SplitResult(scheme, netloc, path, query, fragment)

   :func:`urlsplit` の結果のための具体クラスです。 :meth:`__new__` メソッドをオーバーライドして正しい個数の引数が
   引き渡されたことを確認するようにしています。

