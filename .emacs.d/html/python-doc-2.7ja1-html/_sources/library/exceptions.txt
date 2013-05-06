.. _bltin-exceptions:

組み込み例外
============

.. module:: exceptions
   :synopsis: 標準の例外クラス群


例外はクラスオブジェクトです。例外はモジュール :mod:`exceptions`
で定義されています。このモジュールを明示的にインポートする必要はありません。
例外は :mod:`exceptions` モジュールと同様に組み込み名前空間で与えられます。


.. index::
   statement: try
   statement: except


:keyword:`try` 文の中で、 :keyword:`except` 節を使って特定の例外クラス
について記述した場合、その節は指定した例外クラスから派生したクラスも
扱います (指定した例外クラスの派生元のクラスは含みません)。
サブクラス化の関係にない 2 つの例外クラスは、それらが同じ名前だったとしても
等しくなることはありません。


.. index:: statement: raise

以下に列挙した組み込み例外はインタプリタや組み込み関数によって生成され
ます。特に注記しないかぎり、これらの例外はエラーの詳しい原因を示してい
る、 "関連値 (associated value)" を持ちます。この値は文字列または複数
の情報 (例えばエラーコードや、エラーコードを説明する文字列) を含むタプ
ルです。この関連値は :keyword:`raise` 文の 2 番目の引数です。
例外が標準のルートクラスである :exc:`BaseException` の派生クラスであれば、
関連値は例外インスタンスの :attr:`args` 属性中に置かれます。


ユーザによるコードも組み込み例外を送出することができます。これは例外処
理をテストしたり、インタプリタがある例外を送出する状況と "ちょうど同じ
ような" エラー条件であることを報告させるために使うことができます。しか
し、ユーザが適切でないエラーを送出するようコードすることを妨げる方法は
ないので注意してください。


組み込み例外クラスは新たな例外を定義するためにサブクラス化することができます。
新しい例外は、少なくとも :exc:`Exception` クラスから派生することをお勧めします。
:exc:`BaseException` からは派生しないで下さい。例外を定義する上での詳しい情報は、
Python チュートリアルの :ref:`tut-userexceptions` の項目にあります。


以下の例外クラスは他の例外クラスの基底クラスとしてのみ使われます。


.. exception:: BaseException

   全ての組み込み例外のルートクラスです。ユーザ定義例外が直接このクラ
   スを継承することは意図していません (そうした場合は
   :exc:`Exception` を使ってください)。このクラスに対して :func:`str`
   や :func:`unicode` が呼ばれた場合、引数の文字列表現か、引数が無
   い時には空文字列が返されます。


   .. versionadded:: 2.5

   .. attribute:: args

      例外コンストラクタに与えられた引数のタプルです。
      組み込み例外は普通、エラーメッセージを与える一つの文字列だけを
      引数として呼ばれますが、中には (:exc:`IOError` など) いくつかの引数を
      必要とし、このタプルの要素に特別な意味を込めるものもあります。

.. exception:: Exception

   全ての組み込み例外のうち、システム終了でないものはこのクラスから導
   出されています。全てのユーザ定義例外はこのクラスの派生クラスであるべき
   です。

   .. versionchanged:: 2.5
      :exc:`BaseException` を継承するように変更されました.


.. exception:: StandardError

   :exc:`StopIteration`, :exc:`SystemExit`, :exc:`KeyboardInterrupt`,
   :exc:`SystemExit` 以外の、全ての組み込み例外の基底クラスです。
   :exc:`StandardError` 自体は :exc:`Exception` の派生クラスです。


.. exception:: ArithmeticError

   算術上の様々なエラーにおいて送出される組み込み例外
   :exc:`OverflowError`, :exc:`ZeroDivisionError`,
   :exc:`FloatingPointError` の基底クラスです。


.. exception:: BufferError

   バッファ (:ref:`buffer <bufferobjects>`) に関連する演算が行えなかったときに
   送出されます。


.. exception:: LookupError

   マップ型またはシーケンス型に使われたキーやインデックスが無効な場合
   に送出される例外 :exc:`IndexError` および :exc:`KeyError` の基底クラス
   です。 :func:`codecs.lookup` によって直接送出されることもあります。


.. exception:: EnvironmentError

   Python システムの外部で起こる可能性のある例外 :exc:`IOError` および
   :exc:`OSError` の基底クラスです。この型の例外が 2 つの要素を持つ
   タプルで生成された場合、 1 番目の要素はインスタンスの :attr:`errno`
   属性で得ることができます (この値はエラー番号と見なされます)。 2 番目
   の要素は :attr:`strerror` 属性です (この値は通常、エラーに関連する
   メッセージです)。タプル自体は :attr:`args` 属性から得ることもできます。


   .. versionadded:: 1.5.2

   :exc:`EnvironmentError` 例外が 3 要素のタプルで生成された場合、最初
   の 2 つの要素は上記と同様に値を得ることができ、さらに 3 番目の要素は
   :attr:`filename` 属性で得ることができます。しかしながら、以前のバー
   ジョンとの互換性のために、 :attr:`args` 属性にはコンストラクタに渡
   した最初の 2 つの引数からなる 2 要素のタプルしか含みません。


   この例外が 3 つ以外の引数で生成された場合、 :attr:`filename` 属性は
   ``None`` になります。この例外が 2 つまたは 3 つ以外の引数で生成された
   場合、 :attr:`errno` および :attr:`strerror` 属性も ``None`` になり
   ます。後者のケースでは、 :attr:`args` がコンストラクタに与えた引数
   をそのままタプルの形で含んでいます。


以下の例外は実際に送出される例外です。


.. exception:: AssertionError

   .. index:: statement: assert

   :keyword:`assert` 文が失敗した場合に送出されます。


.. exception:: AttributeError

   属性の参照 (:ref:`attribute-references` を参照) や代入が失敗
   した場合に送出されます (オブジェクトが属性の参照や属性の代入をまっ
   たくサポートしていない場合には :exc:`TypeError` が送出されます)。


.. exception:: EOFError

   組み込み関数 (:func:`input` または :func:`raw_input`) のいずれかで、
   データを全く読まないうちにファイルの終端 (EOF) に到達した場合に送出
   されます (注意: :meth:`file.read` および :meth:`file.readline` メソッド
   の場合、データを読まないうちに EOF にたどり着くと空の文字列を返します)。


.. exception:: FloatingPointError

   浮動小数点演算が失敗した場合に送出されます。この例外は Python
   のどのバージョンでも常に定義されていますが、 Python が
   ``--with-fpectl`` オプションを有効にしてコンパイルされているか、
   :file:`pyconfig.h` ファイルにシンボル :const:`WANT_SIGFPE_HANDLER`
   が定義されている場合にのみ送出されます。


.. exception:: GeneratorExit

   ジェネレータ (:term:`generator`) の :meth:`close` メソッドが呼び出
   されたときに送出されます。この例外は厳密にはエラーではないので、
   :exc:`StandardError` ではなく :exc:`BaseException` を直接継承しています。


   .. versionadded:: 2.5

   .. versionchanged:: 2.6
      :exc:`BaseException` を継承するように変更されました。


.. exception:: IOError

   (:keyword:`print` 文、組み込みの :func:`open` またはファイルオブジェ
   クトに対するメソッドといった) I/O 操作が、例えば "ファイルが存在し
   ません" や "ディスクの空き領域がありません" といった I/O に関連した
   理由で失敗した場合に送出されます。


   このクラスは :exc:`EnvironmentError` の派生クラスです。この例外
   クラスのインスタンス属性に関する情報は上記の
   :exc:`EnvironmentError` に関する議論を参照してください。


   .. versionchanged:: 2.6
      :exc:`socket.error` は、この例外を基底クラスとして使うように変更されました。


.. exception:: ImportError

   :keyword:`import` 文でモジュール定義を見つけられなかった場合や、
   ``from ... import`` 文で指定した名前をインポートすることができなかった場合に送出されます。


.. exception:: IndexError

   シーケンスのインデックス指定がシーケンスの範囲を超えている場合に送出
   されます　(スライスのインデックスはシーケンスの範囲に収まるように暗黙
   のうちに調整されます; インデックスが通常の整数でない場合、
   :exc:`TypeError` が送出されます)。


   .. XXX xref to sequences


.. exception:: KeyError

   マップ型 (辞書型) オブジェクトのキーが、オブジェクトのキー集合内に
   見つからなかった場合に送出されます。


   .. XXX xref to mapping objects?


.. exception:: KeyboardInterrupt

   ユーザが割り込みキー (通常は :kbd:`Control-C` または :kbd:`Delete` キー)
   を押した場合に送出されます。割り込みが起きたかどうかはインタプリタの
   実行中に定期的に調べられます。組み込み関数 :func:`input` や
   :func:`raw_input` がユーザの入力を待っている間に割り込みキーを押しても
   この例外が送出されます。この例外は :exc:`Exception` を処理するコードに
   誤って捕捉されてインタプリタの終了が阻害されないように :exc:`BaseException`
   を継承しています。


   .. versionchanged:: 2.5
      :exc:`BaseException` を継承するように変更されました.


.. exception:: MemoryError

   ある操作中にメモリが不足したが、その状況は (オブジェクトをいくつか
   消去することで) まだ復旧可能かもしれない場合に送出されます。例外の
   関連値は、どんな種類の (内部) 操作がメモリ不足になっているか
   を示す文字列です。背後にあるメモリ管理アーキテクチャ (C の
   :c:func:`malloc` 関数) によっては、インタプリタが常にその状況を完璧
   に復旧できるとはかぎらないので注意してください; プログラムの暴走が
   原因の場合にも、やはり実行スタックの追跡結果を出力できるようにする
   ために例外が送出されます。


.. exception:: NameError

   ローカルまたはグローバルの名前が見つからなかった場合に送出されます。
   これは非限定の名前のみに適用されます。関連値は見つからなかった名前を
   含むエラーメッセージです。


.. exception:: NotImplementedError

   この例外は :exc:`RuntimeError` から派生しています。ユーザ定義の基底
   クラスにおいて、抽象メソッドが派生クラスでオーバライドされることを
   要求する場合、この例外を送出しなくてはなりません。

   .. versionadded:: 1.5.2


.. exception:: OSError

   .. index:: module: errno

   このクラスは :exc:`EnvironmentError` から派生しています。
   関数がシステムに関連したエラーを返した場合に送出されます
   (引数の型が間違っている場合や、他の偶発的なエラーは除きます)。
   :attr:`errno` 属性は :c:data:`errno` に基づく数字のエラーコードで、
   :attr:`strerror` 属性は C の :c:func:`perror` 関数で表示されるような
   文字列です。
   オペレーティングシステムに依存したエラーコードの定義と名前については、
   :mod:`errno` モジュールを参照して下さい。

   ファイルシステムのパスに関係する例外 (:func:`chdir` や
   :func:`unlink` など) では、例外インスタンスは 3 番目の属性
   :attr:`filename` を持ちます。これは関数に渡されたファイル名です。

   .. versionadded:: 1.5.2


.. exception:: OverflowError

   算術演算の結果が表現できない大きな値になった場合に送出されます。
   これは long integer の演算と通常の整数に関するほとんどの操作では
   起こりません (long integer の演算ではむしろ :exc:`MemoryError` が
   送出されることになるでしょう)。整数に関するほとんどの操作では、
   代わりに long integer が返されます。
   C の浮動小数点演算の例外処理は標準化されていないので、ほとんどの
   浮動小数点演算もチェックされません。


.. exception:: ReferenceError

   :func:`weakref.proxy` によって生成された弱参照 (weak reference)
   プロキシを使って、ガーベジコレクションによって回収された後の参照対象
   オブジェクトの属性にアクセスした場合に送出されます。弱参照については
   :mod:`weakref` モジュールを参照してください。


   .. versionadded:: 2.2
      以前は :exc:`weakref.ReferenceError` 例外として知られていました。


.. exception:: RuntimeError

   他のカテゴリに分類できないエラーが検出された場合に送出されます。
   関連値は、何が問題だったのかをより詳細に示す文字列です
   (この例外はほとんど過去のバージョンのインタプリタにおける遺物です。
   今ではあまり使われることはありません)。


.. exception:: StopIteration

   イテレータ (:term:`iterator`) の :meth:`~iterator.next` メソッドにより、それ
   以上要素がないことを知らせるために送出されます。
   この例外は、通常の利用方法ではエラーとはみなされないため、
   :exc:`StandardError` ではなく :exc:`Exception` から派生しています。

   .. versionadded:: 2.2


.. exception:: SyntaxError

   パーザが構文エラーに遭遇した場合に送出されます。この例外は
   :keyword:`import` 文、 :keyword:`exec` 文、組み込み関数
   :func:`evel` や :func:`input` 、初期化スクリプトの読み込みや標準入
   力で (対話的な実行時にも) 起こる可能性があります。


   このクラスのインスタンスは、例外の詳細に簡単にアクセスできるように
   するために、属性 :attr:`filename`, :attr:`lineno`,
   :attr:`offset`, :attr:`text` を持ちます。例外インスタンスに
   対する :func:`str` はメッセージのみを返します。


.. exception:: IndentationError

   正しくないインデントに関する構文エラーの基底クラスです。これは
   :exc:`SyntaxError` のサブクラスです。


.. exception:: TabError

   タブとスペースを一貫しない方法でインデントに使っているときに送出されます。
   これは :exc:`IndentationError` のサブクラスです。


.. exception:: SystemError

   インタプリタが内部エラーを発見したが、その状況は全ての望みを棄てさ
   せるほど深刻ではないように思われる場合に送出されます。関連値は
   (下位層の言葉で) 何がまずいのかを示す文字列です。


   Python の作者か、あなたの Python インタプリタを保守している人にこの
   エラーを報告してください。このとき、 Python インタプリタのバージョン
   (``sys.version``; Python の対話的セッションを開始した際にも出力
   されます)、正確なエラーメッセージ (例外の関連値) を忘れずに報告して
   ください。そしてもし可能ならエラーを引き起こしたプログラムの
   ソースコードを報告してください。


.. exception:: SystemExit

   この例外は :func:`sys.exit` 関数によって送出されます。この例外が
   処理されなかった場合、スタックのトレースバックを全く表示することなく
   Python インタプリタは終了します。関連値が通常の整数であれば、
   システム終了ステータスを表します (:c:func:`exit` 関数に渡されます)。
   値が ``None`` の場合、終了ステータスは 0 です。 (文字列のような) 他の
   型の場合、そのオブジェクトの値が表示され、終了ステータスは 1 になります。

   この例外のインスタンスは属性 :attr:`code` を持ちます。この値は終了
   ステータスまたはエラーメッセージ (標準では ``None``) に設定されます。
   また、この例外は厳密にはエラーではないため、 :exc:`StandardError`
   ではなく :exc:`BaseException` から派生しています。


   :func:`sys.exit` は、クリーンアップのための処理 (:keyword:`try` 文の
   :keyword:`finally` 節) が実行されるようにするため、またデバッガが制
   御不能になるリスクを冒さずにスクリプトを実行できるようにするために
   例外に翻訳されます。即座に終了することが真に強く必要であるとき (例
   えば、 :func:`fork` を呼んだ後の子プロセス内) には :func:`os._exit`
   関数を使うことができます。


   この例外は :exc:`Exception` を捕まえるコードに間違って捕まえられな
   いように、 :exc:`StandardError` や :exc:`Exception` からではなく
   :exc:`BaseException` を継承しています。これにより、この例外は着
   実に呼出し元の方に伝わっていってインタプリタを終了させます。


   .. versionchanged:: 2.5
      :exc:`BaseException` を継承するように変更されました。


.. exception:: TypeError

   組み込み演算または関数が適切でない型のオブジェクトに対して適用され
   た際に送出されます。関連値は型の不整合に関して詳細を述べた文字列です。


.. exception:: UnboundLocalError

   関数やメソッド内のローカルな変数に対して参照を行ったが、その変数に
   は値が代入されていなかった場合に送出されます。 :exc:`NameError`
   のサブクラスです。

   .. versionadded:: 2.0


.. exception:: UnicodeError

   Unicode に関するエンコードまたはデコードのエラーが発生した際に送出
   されます。 :exc:`ValueError` のサブクラスです。

   .. versionadded:: 2.0


.. exception:: UnicodeEncodeError

   Unicode 関連のエラーがエンコード中に発生した際に送出されます。
   :exc:`UnicodeError` のサブクラスです。

   .. versionadded:: 2.3


.. exception:: UnicodeDecodeError

   Unicode 関連のエラーがデコード中に発生した際に送出されます。
   :exc:`UnicodeError` のサブクラスです。

   .. versionadded:: 2.3


.. exception:: UnicodeTranslateError

   Unicode 関連のエラーがコード翻訳に発生した際に送出されます。
   :exc:`UnicodeError` のサブクラスです。

   .. versionadded:: 2.3


.. exception:: ValueError

   組み込み演算や関数が、正しい型だが適切でない値を受け取った場合や、
   :exc:`IndexError` のような詳細な説明のできない状況で送出されます。


.. exception:: VMSError

   VMS においてのみ利用可能です。 VMS 特有のエラーが起こったときに送出されます。


.. exception:: WindowsError

   Windows 特有のエラーか、エラー番号が :c:data:`errno` 値に対応しない
   場合に送出されます。 :attr:`winerrno` および :attr:`strerror` の値は
   Windows プラットフォーム API の関数 :c:func:`GetLastError` と
   :c:func:`FormatMessage` の戻り値から生成されます。 :attr:`errno` の
   値は :attr:`winerror` の値を対応する ``errno.h`` の値にマップしたものです。
   :exc:`OSError` のサブクラスです。


   .. versionadded:: 2.0

   .. versionchanged:: 2.5
      以前のバージョンは :c:func:`GetLastError` のコードを
      :attr:`errno` に入れていました。


.. exception:: ZeroDivisionError

   除算またモジュロ演算における 2 番目の引数が 0 であった場合に送出されます。
   関連値は文字列で、その演算における被演算子と演算子の型を示します。


以下の例外は警告カテゴリとして使われます。詳細については
:mod:`warnings` モジュールを参照してください。


.. exception:: Warning

   警告カテゴリの基底クラスです。


.. exception:: UserWarning

   ユーザコードによって生成される警告の基底クラスです。


.. exception:: DeprecationWarning

   廃止された機能に対する警告の基底クラスです。


.. exception:: PendingDeprecationWarning

   将来廃止される予定の機能に対する警告の基底クラスです。


.. exception:: SyntaxWarning

   曖昧な構文に対する警告の基底クラスです。


.. exception:: RuntimeWarning

   あいまいなランタイム挙動に対する警告の基底クラスです。


.. exception:: FutureWarning

   将来意味構成が変わることになっている文の構成に対する警告の基底クラスです。


.. exception:: ImportWarning

   モジュールインポートの誤りと思われるものに対する警告の基底クラスです。

   .. versionadded:: 2.5


.. exception:: UnicodeWarning

   Unicode に関連した警告の基底クラスです。

   .. versionadded:: 2.5


例外のクラス階層
-------------------

組み込み例外のクラス階層は以下のようになっています:


.. literalinclude:: ../includes/exception_hierarchy.txt

