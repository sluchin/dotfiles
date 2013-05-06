==========================================================================
 :mod:`sqlite3` --- SQLite データベースに対する DB-API 2.0 インタフェース
==========================================================================

.. module:: sqlite3
   :synopsis: A DB-API 2.0 implementation using SQLite 3.x.
.. sectionauthor:: Gerhard Haring <gh@ghaering.de>


.. versionadded:: 2.5

SQLite は、別にサーバプロセスは必要とせずデータベースのアクセスに SQL 
問い合わせ言語の非標準的な一種を使える軽量なディスク上のデータベースを
提供する C ライブラリです。ある種のアプリケーションは内部データ保存に
SQLite を使えます。また、SQLite を使ってアプリケーションのプロトタイプを
作りその後そのコードを PostgreSQL や Oracle のような大規模データベースに
移植するということも可能です。

sqlite3 は Gerhard Häring によって書かれ、
:pep:`249` に記述された DB-API 2.0 仕様に準拠した SQL
インタフェースを提供するものです。

このモジュールを使うには、最初にデータベースを表す :class:`Connection`
オブジェクトを作ります。ここではデータはファイル
:file:`/tmp/example` に格納されているものとします。 ::

   conn = sqlite3.connect('/tmp/example')

特別な名前である ``:memory:`` を使うと RAM 上にデータベースを作ることもできます。

:class:`Connection` があれば、 :class:`Cursor` オブジェクトを作りその
:meth:`~Cursor.execute` メソッドを呼んで
SQL コマンドを実行することができます。 ::

   c = conn.cursor()

   # Create table
   c.execute('''create table stocks
   (date text, trans text, symbol text,
    qty real, price real)''')

   # Insert a row of data
   c.execute("""insert into stocks
             values ('2006-01-05','BUY','RHAT',100,35.14)""")

   # Save (commit) the changes
   conn.commit()

   # We can also close the cursor if we are done with it
   c.close()

たいてい、SQL 操作は Python 変数の値を使う必要があります。この時、クエリーを Python
の文字列操作を使って構築することは、安全とは言えないので、すべきではありません。そのようなことをするとプログラムが SQL インジェク
ション攻撃に対し脆弱になりかねません。

代わりに、DB-API のパラメータ割り当てを使います。 ``?`` を変数の値を使いたいところに埋めておきます。その上で、値のタプルをカーソルの
:meth:`~Cursor.execute` メソッドの第2引数として引き渡します。(他のデータベースモジュールでは変数の場所を示すのに ``%s`` や ``:1``
などの異なった表記を用いることがあります。) 例を示します。 ::

   # Never do this -- insecure!
   symbol = 'IBM'
   c.execute("... where symbol = '%s'" % symbol)

   # Do this instead
   t = (symbol,)
   c.execute('select * from stocks where symbol=?', t)

   # Larger example
   for t in [('2006-03-28', 'BUY', 'IBM', 1000, 45.00),
             ('2006-04-05', 'BUY', 'MSOFT', 1000, 72.00),
             ('2006-04-06', 'SELL', 'IBM', 500, 53.00),
            ]:
       c.execute('insert into stocks values (?,?,?,?,?)', t)

SELECT 文を実行した後データを取得する方法は3つありどれを使っても構いません。
一つはカーソルをイテレータ(:term:`iterator`)として扱う、一つはカーソルの
:meth:`~Cursor.fetchone` メソッドを呼んで一致した内の一行を取得する、もう一つは :meth:`~Cursor.fetchall`
メソッドを呼んで一致した全ての行のリストとして受け取る、という3つです。

以下の例ではイテレータの形を使います。 ::

   >>> c = conn.cursor()
   >>> c.execute('select * from stocks order by price')
   >>> for row in c:
   ...    print row
   ...
   (u'2006-01-05', u'BUY', u'RHAT', 100, 35.14)
   (u'2006-03-28', u'BUY', u'IBM', 1000, 45.0)
   (u'2006-04-06', u'SELL', u'IBM', 500, 53.0)
   (u'2006-04-05', u'BUY', u'MSOFT', 1000, 72.0)
   >>>


.. seealso::

   http://code.google.com/p/pysqlite/
      pysqlite のウェブページ -- sqlite3 は「pysqlite」という名の下、
      外部で開発されています。

   http://www.sqlite.org
      SQLite のウェブページ。ここの文書ではサポートされる SQL 
      方言の文法と使えるデータ型を説明しています

   :pep:`249` - Database API Specification 2.0
      Marc-Andre Lemburg により書かれた PEP

   http://www.python.jp/doc/contrib/peps/pep-0249.txt
      訳注: PEP 249 の日本語訳があります


.. _sqlite3-module-contents:

モジュールの関数と定数
======================


.. data:: PARSE_DECLTYPES

   この定数は :func:`connect` 関数の *detect_types* パラメータとして使われます。

   .. Setting it makes the :mod:`sqlite3` module parse the declared type for each
      column it returns.  It will parse out the first word of the declared type,
      i. e.  for "integer primary key", it will parse out "integer", or for
      "number(10)" it will parse out "number". Then for that column, it will look
      into the converters dictionary and use the converter function registered for
      that type there.

   この定数を設定すると :mod:`sqlite3` モジュールは戻り値のカラムの宣言された型を読み取るようになります。
   意味を持つのは宣言の最初の単語です。
   すなわち、"integer primary key" においては "integer" が読み取られます。
   また、 "number(10)" では、 "number" が読み取られます。
   そして、そのカラムに対して、変換関数の辞書を探してその型に対して登録された関数を使うようにします。


.. data:: PARSE_COLNAMES

   この定数は :func:`connect` 関数の *detect_types* パラメータとして使われます。

   この定数を設定すると SQLite のインタフェースは戻り値のそれぞれのカラムの名前を読み取るようになります。文字列の中の [mytype]
   といった形の部分を探し、'mytype' がそのカラムの名前であると判断します。そして 'mytype' のエントリを変換関数辞書
   の中から見つけ、見つかった変換関数を値を返す際に用います。 :attr:`Cursor.description`
   で見つかるカラム名はその最初の単語だけです。すなわち、もし ``'as "x [datetime]"'`` のようなものを SQL の中で使っていたとすると、
   読み取るのはカラム名の中の最初の空白までの全てですので、カラム名として使われるのは単純に "x" ということになります。


.. function:: connect(database[, timeout, detect_types, isolation_level, check_same_thread, factory, cached_statements])

   ファイル *database* の SQLite データベースへの接続を開きます。 ``":memory:"`` という名前を使うことでディスクの代わりに
   RAM 上のデータベースへの接続を開くこともできます。

   データベースが複数の接続からアクセスされている状況で、その内の一つがデータベースに変更を加えたとき、SQLite データベースはそのトランザクションが
   コミットされるまでロックされます。 *timeout* パラメータで、例外を送出するまで接続がロックが解除されるのをどれだけ待つかを決めます。デフォルトは
   5.0 (5秒) です。

   .. For the *isolation_level* parameter, please see the
      :attr:`Connection.isolation_level` property of :class:`Connection` objects.

   *isolation_level* パラメータについては、 :class:`Connection` オブジェクトの、
   :attr:`Connection.isolation_level` 属性を参照してください。

   SQLite がネイティブにサポートするのは TEXT, INTEGER, FLOAT, BLOB および NULL
   型だけです。もし他の型を使いたければ、その型のためのサポートを自分で追加しなければなりません。 *detect_types* パラメータを、モジュールレベルの
   :func:`register_converter` 関数で登録した自作の **変換関数** と一緒に使えば、簡単にできます。

   パラメータ *detect_types* のデフォルトは 0 (つまりオフ、型検知無し)です。
   型検知を有効にするためには、 :const:`PARSE_DECLTYPES` と :const:`PARSE_COLNAMES`
   の適当な組み合わせをこのパラメータにセットします。

   デフォルトでは、 :mod:`sqlite3` モジュールは connect の呼び出しの際にモジュールの :class:`Connection`
   クラスを使います。しかし、 :class:`Connection` クラスを継承したクラスを *factory* パラメータに渡して
   :func:`connect` にそのクラスを使わせることもできます。詳しくはこのマニュアルの
   :ref:`sqlite3-types` 節を参考にしてください。

   :mod:`sqlite3` モジュールは SQL 解析のオーバーヘッドを避けるために内部で文キャッシュを使っています。接続に対してキャッシュされる文の数を自
   分で指定したいならば、 *cached_statements* パラメータに設定してください。現在の実装ではデフォルトでキャッシュされる SQL 文の数を
   100 にしています。


.. function:: register_converter(typename, callable)

   データベースから得られるバイト列を希望する Python の型に変換する呼び出し可能オブジェクト (callable) を登録します。その呼び出し可能オブジェ
   クトは型が *typename* である全てのデータベース上の値に対して呼び出されます。型検知がどのように働くかについては :func:`connect` 関
   数の *detect_types* パラメータの説明も参照してください。注意が必要なのは *typename* はクエリの中の型名と大文字小文字も一致しなけ
   ればならないということです。


.. function:: register_adapter(type, callable)

   自分が使いたい Python の型 *type* を SQLite がサポートしている型に変換する呼び出し可能オブジェクト (callable)
   を登録します。その呼び出し可能オブジェクト *callable* はただ一つの引数に Python の値を受け取り、int, long, float,
   (UTF-8 でエンコードされた) str, unicode または buffer のいずれかの型の値を返さなければなりません


.. function:: complete_statement(sql)

   もし文字列 *sql* がセミコロンで終端された一つ以上の完全な SQL 文を含んでいれば、 :const:`True` を返します。判定は SQL
   文として文法的に正しいかではなく、閉じられていない文字列リテラルが無いことおよびセミコロンで終端されていることだけで行なわれます。

   この関数は以下の例にあるような SQLite のシェルを作る際に使われます。

   .. literalinclude:: ../includes/sqlite3/complete_statement.py


.. function:: enable_callback_tracebacks(flag)

   デフォルトでは、ユーザ定義の関数、集計関数、変換関数、認可コールバックなどはトレースバックを出力しません。デバッグの際にはこの関数を *flag* に
   :const:`True` を指定して呼び出します。そうした後は先に述べたような関数のトレースバックが ``sys.stderr`` に出力されま
   す。元に戻すには :const:`False` を使います。

   .. % authorizer callbacks = 認可コールバック?


.. _sqlite3-connection-objects:

Connection オブジェクト
=======================

.. class:: Connection

   .. A SQLite database connection has the following attributes and methods:

   SQLite データベースコネクション。以下の属性やメソッドを持ちます。


.. attribute:: Connection.isolation_level

   現在の分離レベルを取得または設定します。
   :const:`None` で自動コミットモードまたは "DEFERRED", "IMMEDIATE", "EXLUSIVE"
   のどれかです。
   より詳しい説明は :ref:`sqlite3-controlling-transactions` 節を参照してください。

.. method:: Connection.cursor([cursorClass])

   cursor メソッドはオプション引数 *cursorClass* を受け付けます。
   これを指定するならば、指定されたクラスは
   :class:`sqlite3.Cursor` を継承したカーソルクラスでなければなりません。

.. method:: Connection.commit()

   .. This method commits the current transaction. If you don't call this method,
      anything you did since the last call to ``commit()`` is not visible from from
      other database connections. If you wonder why you don't see the data you've
      written to the database, please check you didn't forget to call this method.

   このメソッドは現在のトランザクションをコミットします。
   このメソッドを呼ばないと、前回 ``commit()`` を呼び出してから行ったすべての変更は、
   他のデータベースコネクションから見ることができません。
   もし、データベースに書き込んだはずのデータが見えなくて悩んでいる場合は、
   このメソッドの呼び出しを忘れていないかチェックしてください。

.. method:: Connection.rollback()

   .. This method rolls back any changes to the database since the last call to
      :meth:`commit`.

   このメソッドは最後に行った :meth:`commit` 後の全ての変更をロールバックします。

.. method:: Connection.close()

   .. This closes the database connection. Note that this does not automatically
      call :meth:`commit`. If you just close your database connection without
      calling :meth:`commit` first, your changes will be lost!

   このメソッドはデータベースコネクションを閉じます。
   このメソッドが自動的に :meth:`commit` を呼び出さないことに注意してください。
   :meth:`commit` をせずにコネクションを閉じると、変更が消えてしまいます。

.. method:: Connection.execute(sql, [parameters])

   このメソッドは非標準のショートカットで、cursor メソッドを呼び出して
   中間的なカーソルオブジェクトを作り、そのカーソルの :meth:`execute<Cursor.execute>`
   メソッドを与えられたパラメータと共に呼び出します。


.. method:: Connection.executemany(sql, [parameters])

   このメソッドは非標準のショートカットで、cursor メソッドを呼び出して
   中間的なカーソルオブジェクトを作り、そのカーソルの
   :meth:`executemany<Cursor.executemany>` メソッドを
   与えられたパラメータと共に呼び出します。


.. method:: Connection.executescript(sql_script)

   このメソッドは非標準のショートカットで、cursor メソッドを呼び出して
   中間的なカーソルオブジェクトを作り、そのカーソルの
   :meth:`executescript<Cursor.executescript>` メソッドを
   与えられたパラメータと共に呼び出します。


.. method:: Connection.create_function(name, num_params, func)

   後から SQL 文中で *name* という名前の関数として使えるユーザ定義関数を作成します。
   *num_params* は関数が受け付ける引数の数、
   *func* は SQL 関数として使われる Python の呼び出し可能オブジェクトです。

   関数は SQLite でサポートされている任意の型を返すことができます。
   具体的には unicode, str, int, long, float, buffer および None です。

   例:

   .. literalinclude:: ../includes/sqlite3/md5func.py


.. method:: Connection.create_aggregate(name, num_params, aggregate_class)

   ユーザ定義の集計関数を作成します。

   集計クラスにはパラメータ *num_params* で指定される個数の引数を取る
   ``step`` メソッドおよび最終的な集計結果を返す
   ``finalize`` メソッドを実装しなければなりません。

   ``finalize`` メソッドは SQLite でサポートされている任意の型を返すことができます。
   具体的には unicode, str, int, long, float, buffer および None です。

   例:

   .. literalinclude:: ../includes/sqlite3/mysumaggr.py


.. method:: Connection.create_collation(name, callable)

   *name* と *callable* で指定される照合順序を作成します。
   呼び出し可能オブジェクトには二つの文字列が渡されます。
   一つめのものが二つめのものより低く順序付けられるならば -1 を返し、
   等しければ 0 を返し、一つめのものが二つめのものより高く順序付けられるならば
   1 を返すようにしなければなりません。
   この関数はソート(SQL での ORDER BY)をコントロールするもので、
   比較を行なうことは他の SQL 操作には影響を与えないことに注意しましょう。

   また、呼び出し可能オブジェクトに渡される引数は Python のバイト文字列として
   渡されますが、それは通常 UTF-8 で符号化されたものになります。

   以下の例は「間違った方法で」ソートする自作の照合順序です:

   .. literalinclude:: ../includes/sqlite3/collation_reverse.py

   照合順序を取り除くには ``create_collation`` を callable として 
   None を渡して呼び出します::

      con.create_collation("reverse", None)


.. method:: Connection.interrupt()

   このメソッドを別スレッドから呼び出して接続上で現在実行中であろうクエリを
   中断させられます。クエリが中断されると呼び出し元は例外を受け取ります。


.. method:: Connection.set_authorizer(authorizer_callback)

   このルーチンはコールバックを登録します。コールバックはデータベースのテーブルのカラムにアクセスしようとするたびに呼び出されます。コールバッ
   クはアクセスが許可されるならば :const:`SQLITE_OK` を、SQL 文全体がエラーとともに中断されるべきならば
   :const:`SQLITE_DENY` を、カラムが NULL 値として扱われるべきなら :const:`SQLITE_IGNORE` を返さなけ
   ればなりません。これらの定数は :mod:`sqlite3` モジュールに用意されています。

   コールバックの第一引数はどの種類の操作が許可されるかを決めます。第二第三引数には第一引数に依存して本当に使われる引数か :const:`None` かが渡
   されます。第四引数はもし適用されるならばデータベースの名前("main", "temp", etc.)です。第五引数はアクセスを試みる要因となった最も内側のトリ
   ガまたはビューの名前、またはアクセスの試みが入力された SQL コードに直接起因するものならば :const:`None` です。

   第一引数に与えることができる値や、その第一引数によって決まる第二第三引数の意味については、SQLite の文書を参考にしてください。必要な定数は全て
   :mod:`sqlite3` モジュールに用意されています。


.. method:: Connection.set_progress_handler(handler, n)

   .. versionadded:: 2.6

   .. This routine registers a callback. The callback is invoked for every *n*
      instructions of the SQLite virtual machine. This is useful if you want to
      get called from SQLite during long-running operations, for example to update
      a GUI.

   このメソッドはコールバックを登録します。
   コールバックは SQLite 仮想マシン上の *n* 個の命令を実行するごとに呼び出されます。
   これは、GUI 更新などのために、長時間かかる処理中に SQLite
   からの呼び出しが欲しい場合に便利です。

   .. If you want to clear any previously installed progress handler, call the
      method with :const:`None` for *handler*.

   以前登録した progress handler をクリアしたい場合は、このメソッドを、
   *handler* 引数に :const:`None` を渡して呼び出してください。


.. method:: Connection.enable_load_extension(enabled)

   .. versionadded:: 2.7

   このメソッドは SQLite エンジンが共有ライブラリから SQLite 拡張を読み込むのを許可したり、禁止したりします。
   SQLite 拡張は新しい関数や集計関数や仮想テーブルの実装を定義できます。
   1つの有名な拡張は SQLite によって頒布されている全テキスト検索拡張です。

   .. literalinclude:: ../includes/sqlite3/load_extension.py

.. method:: Connection.load_extension(path)

   .. versionadded:: 2.7

   このメソッドは共有ライブラリから SQLite 拡張を読み込みます。
   このメソッドを使う前に :meth:`enable_load_extension` で拡張の読み込みを許可しておかなくてはなりません。

.. attribute:: Connection.row_factory

   この属性を、カーソルとタプルの形での元の行のデータを受け取り最終的な行を表す
   オブジェクトを返す呼び出し可能オブジェクトに、変更することができます。
   これによって、より進んだ結果の返し方を実装することができます。
   例えば、カラムの名前で各データにアクセスできるようなオブジェクトを返したりできます。

   例:

   .. literalinclude:: ../includes/sqlite3/row_factory.py

   タプルを返すのでは物足りず、名前に基づいたカラムへのアクセスが行ないたい場合は、
   高度に最適化された :class:`sqlite3.Row` 型を
   :attr:`row_factory` にセットすることを考えてはいかがでしょうか。 :class:`Row`
   クラスでは添字でも大文字小文字を無視した名前でもカラムにアクセスでき、
   しかもほとんどメモリーを浪費しません。
   おそらく、辞書を使うような独自実装のアプローチよりも、
   もしかすると db の行に基づいた解法よりも良いものかもしれません。

   .. XXX what's a db_row-based solution?


.. attribute:: Connection.text_factory

   この属性を使って ``TEXT`` データ型をどのオブジェクトで返すかを制御できます。
   デフォルトではこの属性は :class:`unicode` に設定されており、
   :mod:`sqlite3` モジュールは ``TEXT`` を Unicode オブジェクトで返します。
   もしバイト列で返したいならば、 :class:`str` に設定してください。

   効率の問題を考えて、非ASCIIデータに限って Unicode オブジェクトを返し、
   その他の場合にはバイト列を返す方法もあります。これを有効にしたければ、
   この属性を :const:`sqlite3.OptimizedUnicode` に設定してください。

   バイト列を受け取って望みの型のオブジェクトを返すような
   呼び出し可能オブジェクトを何でも設定して構いません。

   以下の説明用のコード例を参照してください:

   .. literalinclude:: ../includes/sqlite3/text_factory.py


.. attribute:: Connection.total_changes

   データベース接続が開始されて以来の行の変更・挿入・削除がなされた行の総数を返します。


.. attribute:: Connection.iterdump

   .. Returns an iterator to dump the database in an SQL text format.  Useful when
      saving an in-memory database for later restoration.  This function provides
      the same capabilities as the :kbd:`.dump` command in the :program:`sqlite3`
      shell.

   データベースをSQL testフォーマットでダンプするためのイテレータを返します。
   in-memory データベースの内容を、後でリストアするための保存する場合に便利です。
   この関数は :program:`sqlite3` シェルの中の :kbd:`.dump` コマンドと同じ機能を持っています。

   .. versionadded:: 2.6

   例::

      # existing_db.db ファイルを dump.sql ファイルにダンプする
      import sqlite3, os

      con = sqlite3.connect('existing_db.db')
      with open('dump.sql', 'w') as f:
          for line in con.iterdump():
              f.write('%s\n' % line)



.. _sqlite3-cursor-objects:

カーソルオブジェクト
====================

.. class:: Cursor

   :class:`Cursor` インスタンスは以下の属性やメソッドを持ちます。

.. method:: Cursor.execute(sql, [parameters])

   SQL 文を実行します。SQL 文はパラメータ化できます(すなわち SQL リテラルの代わりの場所確保文字 (placeholder) を入れておけます)。
   :mod:`sqlite3` モジュールは2種類の場所確保記法をサポートします。一つは疑問符(qmark スタイル)、もう一つは名前(named
   スタイル)です。

   まず最初の例は qmark スタイルのパラメータを使った書き方を示します:

   .. literalinclude:: ../includes/sqlite3/execute_1.py

   次の例は named スタイルの使い方です:

   .. literalinclude:: ../includes/sqlite3/execute_2.py

   :meth:`execute` は一つの SQL 文しか実行しません。二つ以上の文を実行しようとすると、Warning を発生させます。複数の SQL
   文を一つの呼び出しで実行したい場合は :meth:`executescript` を使ってください。


.. method:: Cursor.executemany(sql, seq_of_parameters)

   SQL 文 *sql* を *seq_of_parameters* シーケンス(またはマッピング)に含まれる全てのパラメータに対して実行します。
   :mod:`sqlite3` モジュールでは、シーケンスの代わりにパラメータの組を作り出すイテレータ使うことが許されています。

   .. literalinclude:: ../includes/sqlite3/executemany_1.py

   もう少し短いジェネレータ(:term:`generator`)を使った例です:

   .. literalinclude:: ../includes/sqlite3/executemany_2.py


.. method:: Cursor.executescript(sql_script)

   これは非標準の便宜メソッドで、一度に複数の SQL 文を実行することができます。
   メソッドは最初に ``COMMIT`` 文を発行し、次いで引数として渡された SQLスクリプトを実行します。

   *sql_script* はバイト文字列または Unicode 文字列です。

   例:

   .. literalinclude:: ../includes/sqlite3/executescript.py


.. method:: Cursor.fetchone()

   .. Fetches the next row of a query result set, returning a single sequence,
      or :const:`None` when no more data is available.

   クエリ結果から次の row をフェッチして、1つのシーケンスを返します。
   これ以上データがない場合は :const:`None` を返します。


.. method:: Cursor.fetchmany([size=cursor.arraysize])

   .. Fetches the next set of rows of a query result, returning a list.  An empty
      list is returned when no more rows are available.

   クエリ結果から次の幾つかの row をフェッチして、リストを返します。
   これ以上データがない場合は空のリストを返します。

   .. The number of rows to fetch per call is specified by the *size* parameter.
      If it is not given, the cursor's arraysize determines the number of rows
      to be fetched. The method should try to fetch as many rows as indicated by
      the size parameter. If this is not possible due to the specified number of
      rows not being available, fewer rows may be returned.

   一回の呼び出しで返される row の数は、 *size* 引数で指定できます。
   この引数が与えられない場合、 cursor の arraysize 属性が利用されます。
   このメソッドは可能な限り指定された *size* の数の row を fetch しようとするべきです。
   もし、指定された数の row が利用可能でない場合、それより少ない数の row が返されます。

   .. Note there are performance considerations involved with the *size* parameter.
      For optimal performance, it is usually best to use the arraysize attribute.
      If the *size* parameter is used, then it is best for it to retain the same
      value from one :meth:`fetchmany` call to the next.

   *size* 引数とパフォーマンスの関係についての注意です。
   パフォーマンスを最適化するためには、大抵、 arraysize 属性を利用するのがベストです。
   *size* 引数を利用したのであれば、次の :meth:`fetchmany` の呼び出しでも同じ数を利用するのがベストです。

.. method:: Cursor.fetchall()

   .. Fetches all (remaining) rows of a query result, returning a list.  Note that
      the cursor's arraysize attribute can affect the performance of this operation.
      An empty list is returned when no rows are available.

   全ての(残りの)クエリ結果の row をフェッチして、リストを返します。
   cursor の arraysize 属性がこの操作のパフォーマンスに影響することに気をつけてください。
   これ以上の row がない場合は、空のリストが返されます。


.. attribute:: Cursor.rowcount

   一応 :mod:`sqlite3` モジュールの :class:`Cursor` クラスはこの属性を実
   装していますが、データベースエンジン自身の「影響を受けた行」/「選択された行」の決定方法は少し風変わりです。

   ``DELETE`` 文では、条件を付けずに ``DELETE FROM table`` とすると SQLite は :attr:`rowcount` を 0
   と報告します。

   :meth:`executemany` では、変更数が :attr:`rowcount` に合計されます。

   Python DB API 仕様で求められているように、 :attr:`rowcount` 属性は「現在のカーソルがまだ ``executeXXX()``
   を実行していない場合や、データベースインタフェースから最後に行った操作の結果行数を決定できない場合には、この属性は -1 となります。」

   ``SELECT`` 文でも、全ての行を取得し終えるまで全部で何行になったか決められないので :attr:`rowcount` はいつでも -1 です。

.. attribute:: Cursor.lastrowid

   .. This read-only attribute provides the rowid of the last modified row. It is
      only set if you issued a ``INSERT`` statement using the :meth:`execute`
      method. For operations other than ``INSERT`` or when :meth:`executemany` is
      called, :attr:`lastrowid` is set to :const:`None`.

   この読み込み専用の属性は、最後に変更した row の rowid を提供します。
   この属性は、 :meth:`execute` メソッドを利用して ``INSERT`` 文を実行したときのみ設定されます。
   ``INSERT`` 以外の操作や、 :meth:`executemany` メソッドを利用した場合は、 :attr:`lastrowid`
   は :const:`None` に設定されます。

.. attribute:: Cursor.description

   .. This read-only attribute provides the column names of the last query. To
      remain compatible with the Python DB API, it returns a 7-tuple for each
      column where the last six items of each tuple are :const:`None`.

   この読み込み専用の属性は、最後のクエリの結果のカラム名を提供します。
   Python DB API との互換性を維持するために、各カラムに対して 7つのタプルを返しますが、
   タプルの後ろ6つの要素は全て :const:`None` です。

   .. It is set for ``SELECT`` statements without any matching rows as well.

   この属性は ``SELECT`` 文にマッチする row が1つもなかった場合でもセットされます。


.. _sqlite3-row-objects:

Row オブジェクト
================

.. class:: Row

   .. A :class:`Row` instance serves as a highly optimized
      :attr:`~Connection.row_factory` for :class:`Connection` objects.
      It tries to mimic a tuple in most of its features.

   :class:`Row` インスタンスは、 :class:`Connection` オブジェクトの
   :attr:`~Connection.row_factory` として高度に最適化されています。
   タプルによく似た機能を持つ row を作成します。

   .. It supports mapping access by column name and index, iteration,
      representation, equality testing and :func:`len`.

   カラム名とインデックスによる要素へのアクセス, イテレーション, 
   repr(), 同値テスト, :func:`len` をサポートしています。

   .. If two :class:`Row` objects have exactly the same columns and their
      members are equal, they compare equal.

   もし、2つの :class:`Row` オブジェクトが完全に同じカラムと値を持っていた場合、
   それらは同値になります。

   .. versionchanged:: 2.6
      .. Added iteration and equality (hashability).

      イテレーションと同値性、ハッシュ可能

   .. method:: keys

      .. This method returns a tuple of column names. Immediately after a query,
         it is the first member of each tuple in :attr:`Cursor.description`.

      このメソッドはカラム名のタプルを返します。
      クエリ直後から、これは :attr:`Cursor.description` の各タプルの最初のメンバになります。

      .. versionadded:: 2.6

.. Let's assume we initialize a table as in the example given above::

Rowの例のために、まずサンプルのテーブルを初期化します。 ::

    conn = sqlite3.connect(":memory:")
    c = conn.cursor()
    c.execute('''create table stocks
    (date text, trans text, symbol text,
     qty real, price real)''')
    c.execute("""insert into stocks
              values ('2006-01-05','BUY','RHAT',100,35.14)""")
    conn.commit()
    c.close()

.. Now we plug :class:`Row` in::

そして、 :class:`Row` を使ってみます。 ::

    >>> conn.row_factory = sqlite3.Row
    >>> c = conn.cursor()
    >>> c.execute('select * from stocks')
    <sqlite3.Cursor object at 0x7f4e7dd8fa80>
    >>> r = c.fetchone()
    >>> type(r)
    <type 'sqlite3.Row'>
    >>> r
    (u'2006-01-05', u'BUY', u'RHAT', 100.0, 35.14)
    >>> len(r)
    5
    >>> r[2]
    u'RHAT'
    >>> r.keys()
    ['date', 'trans', 'symbol', 'qty', 'price']
    >>> r['qty']
    100.0
    >>> for member in r: print member
    ...
    2006-01-05
    BUY
    RHAT
    100.0
    35.14


.. _sqlite3-types:

SQLite と Python の型
=====================


入門編
------

SQLite が最初からサポートしているのは次の型です。

   ``NULL``, ``INTEGER``, ``REAL``, ``TEXT``, ``BLOB``

したがって、次の Python の型は問題なく SQLite に送り込めます:

+-----------------------------+-------------+
| Python の型                 | SQLite の型 |
+=============================+=============+
| :const:`None`               | ``NULL``    |
+-----------------------------+-------------+
| :class:`int`                | ``INTEGER`` |
+-----------------------------+-------------+
| :class:`long`               | ``INTEGER`` |
+-----------------------------+-------------+
| :class:`float`              | ``REAL``    |
+-----------------------------+-------------+
| :class:`str` (UTF8-encoded) | ``TEXT``    |
+-----------------------------+-------------+
| :class:`unicode`            | ``TEXT``    |
+-----------------------------+-------------+
| :class:`buffer`             | ``BLOB``    |
+-----------------------------+-------------+

SQLite の型から Python の型へのデフォルトでの変換は以下の通りです:

+-------------+-------------------------------------------------------+
| SQLite の型 | Python の型                                           |
+=============+=======================================================+
| ``NULL``    | :const:`None`                                         |
+-------------+-------------------------------------------------------+
| ``INTEGER`` | :class:`int` または :class:`long`,                    |
|             | (サイズによる)                                        |
+-------------+-------------------------------------------------------+
| ``REAL``    | :class:`float`                                        |
+-------------+-------------------------------------------------------+
| ``TEXT``    | :attr:`~Connection.text_factory` に依存する。         |
|             | デフォルトでは :class:`unicode`                       |
+-------------+-------------------------------------------------------+
| ``BLOB``    | :class:`buffer`                                       |
+-------------+-------------------------------------------------------+

:mod:`sqlite3` モジュールの型システムは二つの方法で拡張できます。一つはオブジェクト適合(adaptation)を通じて追加された Python
の型を SQLite に格納することです。もう一つは変換関数(converter)を通じて :mod:`sqlite3` モジュールに SQLite
の型を違った Python の型に変換させることです。


追加された Python の型を SQLite データベースに格納するために適合関数を使う
--------------------------------------------------------------------------

既に述べたように、SQLite が最初からサポートする型は限られたものだけです。それ以外の Python の型を SQLite で使うには、その型を
:mod:`sqlite3` モジュールがサポートしている型の一つに **適合** させなくてはなりません。サポートしている型というのは、NoneType,
int, long, float, str, unicode, buffer です。

:mod:`sqlite3` モジュールは :pep:`246` に述べられているような Python オブジェクト適合を用います。使われるプロトコルは
:class:`PrepareProtocol` です。

:mod:`sqlite3` モジュールで望みの Python の型をサポートされている型の一つに適合させる方法は二つあります。


オブジェクト自身で適合するようにする
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

自分でクラスを書いているならばこの方法が良いでしょう。次のようなクラスがあるとします::

   class Point(object):
       def __init__(self, x, y):
           self.x, self.y = x, y

さてこの点を SQLite の一つのカラムに収めたいと考えたとしましょう。最初にしなければならないのはサポートされている型の中から点を表現するのに使
えるものを選ぶことです。ここでは単純に文字列を使うことにして、座標を区切るのにはセミコロンを使いましょう。次に必要なのはクラスに変換された値を返す
``__conform__(self, protocol)`` メソッドを追加することです。引数 *protocol* は
:class:`PrepareProtocol` になります。

.. literalinclude:: ../includes/sqlite3/adapter_point_1.py


適合関数を登録する
~~~~~~~~~~~~~~~~~~

もう一つの可能性は型を文字列表現に変換する関数を作り :meth:`register_adapter` でその関数を登録することです。

.. note::

   適合させる型/クラスは新スタイルクラス(:term:`new-style class`)でなければなりません。
   すなわち、 :class:`object` を基底クラスの一つとしていなければなりません。

.. literalinclude:: ../includes/sqlite3/adapter_point_2.py

:mod:`sqlite3` モジュールには二つの Python 標準型 :class:`datetime.date` と
:class:`datetime.datetime` に対するデフォルト適合関数があります。いま :class:`datetime.datetime`
オブジェクトを ISO 表現でなく Unix タイムスタンプとして格納したいとしましょう。

.. literalinclude:: ../includes/sqlite3/adapter_datetime.py


SQLite の値を好きな Python 型に変換する
---------------------------------------

適合関数を書くことで好きな Python 型を SQLite に送り込めるようになりました。しかし、本当に使い物になるようにするには Python から
SQLite さらに Python へという往還(roundtrip)の変換ができる必要があります。

そこで変換関数(converter)です。

:class:`Point` クラスの例に戻りましょう。x, y 座標をセミコロンで区切った文字列として SQLite に格納したのでした。

まず、文字列を引数として取り :class:`Point` オブジェクトをそれから構築する変換関数を定義します。

.. note::

   変換関数は SQLite に送り込んだデータ型に関係なく **常に** 文字列を渡されます。

::

   def convert_point(s):
       x, y = map(float, s.split(";"))
       return Point(x, y)

次に :mod:`sqlite3` モジュールにデータベースから取得したものが本当に点であることを教えなければなりません。二つの方法があります:

* 宣言された型を通じて暗黙的に

* カラム名を通じて明示的に

どちらの方法も :ref:`sqlite3-module-contents` 節の中で説明されています。それぞれ
:const:`PARSE_DECLTYPES` 定数と :const:`PARSE_COLNAMES` 定数の項目です。

以下の例で両方のアプローチを紹介します。

.. literalinclude:: ../includes/sqlite3/converter_point.py


デフォルトの適合関数と変換関数
------------------------------

datetime モジュールの date 型および datetime 型のためのデフォルト適合関数があります。これらの型は ISO 日付 / ISO
タイムスタンプとして SQLite に送られます。

デフォルトの変換関数は :class:`datetime.date` 用が "date" という名前で、 :class:`datetime.datetime`
用が "timestamp" という名前で登録されています。

これにより、多くの場合特別な細工無しに Python の日付 / タイムスタンプを使えます。適合関数の書式は実験的な SQLite の date/time
関数とも互換性があります。

以下の例でこのことを確かめます。

.. literalinclude:: ../includes/sqlite3/pysqlite_datetime.py


.. _sqlite3-controlling-transactions:

トランザクション制御
====================

デフォルトでは、 :mod:`sqlite3` モジュールはデータ変更言語(DML)文(すなわち
``INSERT``/``UPDATE``/``DELETE``/``REPLACE``)の前に暗黙のうちに
トランザクションを開始し、非DML、非クエリ文(すなわち
``SELECT`` や上記のいずれでもないもの。)の前にトランザクションをコミットします。

ですから、もしトランザクション中に ``CREATE TABLE ...``, ``VACUUM``, ``PRAGMA``
といったコマンドを発行すると、
:mod:`sqlite3` モジュールはそのコマンドの実行前に暗黙のうちにコミットします。
このようにする理由は二つあります。
第一にこうしたコマンドのうちの幾つかはトランザクション中ではうまく動きません。
第二に sqlite3 はトランザクションの状態(トランザクションが掛かっているかどうか)
を追跡する必要があるからです。

sqlite3 が暗黙のうちに実行する ``BEGIN`` 文の種類(またはそういうものを使わないこと)を
:func:`connect` 呼び出しの
*isolation_level* パラメータを通じて、または接続の :attr:`isolation_level`
プロパティを通じて、制御することができます。

もし **自動コミットモード** が使いたければ、
:attr:`isolation_level` は None にしてください。

そうでなければデフォルトのまま ``BEGIN`` 文を使い続けるか、
SQLite がサポートする分離レベル "DEFERRED", "IMMEDIATE" または
"EXCLUSIVE" を設定してください。


:mod:`sqlite3` の効率的な使い方
===============================


ショートカットメソッドを使う
----------------------------

:class:`Connection` オブジェクトの非標準的なメソッド :meth:`execute`,
:meth:`executemany`, :meth:`executescript` を使うことで、
(しばしば余計な) :class:`Cursor` オブジェクトをわざわざ作り出さずに済むので、
コードをより簡潔に書くことができます。 :class:`Cursor` オブジェクトは暗黙裡に
生成されショートカットメソッドの戻り値として受け取ることができます。この方法を使えば、
``SELECT`` 文を実行してその結果について反復することが、
:class:`Connection` オブジェクトに対する呼び出し一つで行なえます。

.. literalinclude:: ../includes/sqlite3/shortcut_methods.py


位置ではなく名前でカラムにアクセスする
--------------------------------------

:mod:`sqlite3` モジュールの有用な機能の一つに、行生成関数として使われるための
:class:`sqlite3.Row` クラスがあります。

このクラスでラップされた行は、位置インデクス(タプルのような)でも大文字小文字を
区別しない名前でもアクセスできます:

.. literalinclude:: ../includes/sqlite3/rowclass.py


.. Using the connection as a context manager

コネクションをコンテキストマネージャーとして利用する
----------------------------------------------------

.. versionadded:: 2.6

.. Connection objects can be used as context managers
   that automatically commit or rollback transactions.  In the event of an
   exception, the transaction is rolled back; otherwise, the transaction is
   committed:

Connection オブジェクトはコンテキストマネージャーとして利用して、
トランザクションを自動的にコミットしたりロールバックすることができます。
例外が発生したときにトランザクションはロールバックされ、それ以外の場合、
トランザクションはコミットされます。

.. literalinclude:: ../includes/sqlite3/ctx_manager.py


既知の問題
==========

マルチスレッド
--------------

古いバージョンの SQLite はスレッド間でのコネクションの共有に問題がありました。
その理由は、Python のモジュールではスレッド間のコネクションとカーソルの共有ができないためです。
依然としてそのようなことをしようとすると、実行時に例外を受け取るでしょう。

唯一の例外は :meth:`~Connection.interrupt` メソッドで、これだけが異なるスレッドから呼び出せます。
