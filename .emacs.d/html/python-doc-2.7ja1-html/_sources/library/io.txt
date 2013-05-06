:mod:`io` --- ストリームを扱うコアツール
=================================================

.. module:: io
   :synopsis: Core tools for working with streams.
.. moduleauthor:: Guido van Rossum <guido@python.org>
.. moduleauthor:: Mike Verdone <mike.verdone@gmail.com>
.. moduleauthor:: Mark Russell <mark.russell@zen.co.uk>
.. moduleauthor:: Antoine Pitrou <solipsis@pitrou.net>
.. moduleauthor:: Amaury Forgeot d'Arc <amauryfa@gmail.com>
.. moduleauthor:: Benjamin Peterson <benjamin@python.org>
.. sectionauthor:: Benjamin Peterson <benjamin@python.org>
.. versionadded:: 2.6

:mod:`io` モジュールはストリーム処理を行う Python インタフェースを提供します。
Python 2.x では、これは組み込みの :class:`file` オブジェクトの代わりとして
提案されていますが、Python 3.x では、これがファイルやストリームの
デフォルトインタフェースです。

.. note::

   このモジュールはもともと、Python 3.x のために設計されたものなので、
   このドキュメントの中で使われるすべての "bytes" は  (:class:`bytes` が
   エイリアスとなる) :class:`str` 型のことで、すべての "text" は
   :class:`unicode` 型のことです。さらに、 :mod:`io` API では、
   この 2 つの型は入れ替えられません。

I/O 階層の最上位には抽象基底クラスの :class:`IOBase` があります。
:class:`IOBase` ではストリームに対して基本的なインタフェースを定義しています。
しかしながら、ストリームに対する読み込みと書き込みが分離されていないことに注意してください。
実装においては与えられた操作をサポートしない場合は :exc:`IOError` を送出することが許されています。

:class:`IOBase` の拡張は、単純なストリームに対する生のバイト列の読み書きを扱う :class:`RawIOBase` です。
:class:`FileIO` は、 :class:`RawIOBase` を継承してマシンのファイルシステム中のファイルへのインタフェースを提供します。

:class:`BufferedIOBase` では生のバイトストリーム (:class:`RawIOBase`) 上にバッファ処理を追加します。
そのサブクラスの :class:`BufferedWriter`, :class:`BufferedReader`, :class:`BufferedRWPair` では、
それぞれ読み込み専用、書き込み専用、読み書き可能なストリームをバッファします。
:class:`BufferedRandom` ではランダムアクセスストリームに対してバッファされたインタフェースを提供します。
:class:`BytesIO` はインメモリバイトへのシンプルなストリームです。

もう一つの :class:`IOBase` のサブクラスである :class:`TextIOBase` は、
テキストを表すバイトストリームを扱い、 :class:`unicode` エンコードやデコードといった処理を行います。
:class:`TextIOWrapper` はその拡張で、バッファ付き生ストリーム (:class:`BufferedIOBase`) へのバッファされたテキストインタフェースです。
最後に :class:`StringIO` は Unicode テキストに対するインメモリストリームです。

引数名は規約に含まれていません。
そして :func:`.open` の引数だけがキーワード引数として用いられることが意図されています。


モジュールインタフェース
------------------------

.. data:: DEFAULT_BUFFER_SIZE

   モジュールのバッファ I/O クラスで使用されるデフォルトのバッファサイズを指定する整数値です。
   :func:`.open` は可能であればファイル全体のサイズ (:func:`os.stat` で取得されます) を使用します。


.. function:: open(file, mode='r', buffering=-1, encoding=None, errors=None, newline=None, closefd=True)

   *file* を開き、対応するストリームを返します。
   ファイルが開けなかった場合、 :exc:`IOError` が発生します。

   *file* は開きたいファイルの (絶対、またはカレントワーキングディレクトリに
   対する相対) パス名を示す文字列か、
   開きたいファイルがラップされているファイル記述子です。
   (ファイル記述子が与えられた場合、
   *closefd* が ``False`` に設定されていない限り、
   返された I/O オブジェクトが閉じられたときにそのファイル識別子も閉じられます)

   *mode* はオプションの文字列です。これによってファイルをどのようなモードで開くか明示することができます。
   デフォルトは ``'r'`` でテキストモードで読み取り専用で開くことを指します。
   他にも ``'w'`` は書き込み専用 (もしファイルが存在していた場合は上書きになります) となり、 ``'a'`` では追記モードとなります。
   (``'a'`` は *いくつかの* Unixシステムでは *すべての* 書き込みがシーク位置に関係なくファイルの末尾に追記されることを意味します)
   テキストモードでは、もし *encoding* が指定されていなかった場合、エンコーディングはプラットフォーム依存となります。
   (生のバイトデータの読み込みと書き込みはバイナリモードを用いて、 *encoding* は未指定のままとします)
   指定可能なモードは次の表の通りです。

   ========= ===============================================================
   文字       意味
   --------- ---------------------------------------------------------------
   ``'r'``   読み込み専用で開く (デフォルト)
   ``'w'``   書き込み専用で開く。ファイルは最初に切り詰められる。
   ``'a'``   書き込み専用で開く。ファイルが存在する場合は末尾に追記する。
   ``'b'``   バイナリモード
   ``'t'``   テキストモード (デフォルト)
   ``'+'``   ファイルを更新用に開く (読み込み／書き込み)
   ``'U'``   ユニバーサルニューラインモード (後方互換性のためのモードで、
             新規コードでは使用すべきではありません)
   ========= ===============================================================

   デフォルトモードは ``'rt'`` です (テキストを読み込み専用で開きます)。
   バイナリのランダムアクセスでは ``'w+b'`` はファイルを開き、ファイルを 0 バイトに切り詰めます。
   一方で ``'r+b'`` でファイルを開くとサイズの切り詰めは行われません。

   Python ではバイナリモードで開かれたファイルとテキストモードで開かれたファイルは区別されます。
   オペレーティングシステムが区別しない場合でもこの区別は適用されます。
   バイナリモードで開かれたファイル (つまり *mode* 引数に ``'b'`` が含まれるとき) では、
   中身を :class:`bytes` オブジェクトとして返し、一切のデコードを行いません。
   テキストモード (デフォルトか *mode* 引数に ``'t'`` が含まれている場合) では、
   ファイルの内容は :class:`unicode` 文字列として返され、
   バイト列はプラットフォーム依存のエンコーディングか、
   *encoding* が指定された場合は指定されたエンコーディングを使ってデコードされます。

   オプションの *buffering* はバッファ用の設定を行う整数値です。
   0 を設定することでバッファがオフになります (バイナリモードでのみ有効です)。
   1 の場合は 1 行ごとのバッファリングを行い (テキストモードでのみ利用可能です)、
   1 より大きい場合は固定サイズチャンクバッファのサイズを表します。
   *buffering* 引数が与えられなければ、デフォルトのバッファリングポリシーは以下のように働きます:

   * バイナリファイルは固定サイズのチャンクでバッファリングされます。
     バッファサイズは、背後のデバイスの「ブロックサイズ」を決定するヒューリスティックを用いて選択され、
     それが不可能な場合は代わりに :attr:`DEFAULT_BUFFER_SIZE` が使われます。
     多くのシステムでは、典型的なバッファサイズは 4096 か 8192 バイト長になるでしょう。

   * 「対話的な」テキストファイル (:meth:`isatty` が True を返すファイル) は行バッファリングを使用します。
     その他のテキストファイルは、上で説明されたバイナリファイルのためのポリシーを使用します。

   *encoding* はファイルをエンコードあるいはデコードするために使われるエンコーディング名です。
   このオプションはテキストモードでのみ使用されるべきです。
   デフォルトエンコーディングはプラットフォーム依存
   (:func:`locale.getpreferredencoding` が返すもの) ですが、Pythonでサポートされているエンコーディングはどれでも使えます。
   詳しくは :mod:`codecs` モジュール内のサポートしているエンコーディングのリストを参照してください。

   *errors* はエンコードやデコードの際のエラーをどのように扱うかを指定する文字列で、
   バイナリモードでは使えません。
   ``'strict'`` を指定すると、エンコードエラーがあった場合 :exc:`ValueError` 例外が発生します
   (デフォルトである ``None`` は同様の処理を行います)。 ``'ignore'`` を指定した場合はエラーを無視します。
   ``'replace'`` を指定した場合は正常に変換されなかった文字の代わりにマーカ (例えば ``'?'`` のような文字) を挿入します。
   書き込みの際には ``'xmlcharrefreplace'`` (適切なXML文字参照に置き換える) か
   ``'backslashreplace'`` (バックスラッシュによるエスケープシーケンスに置き換える) のどちらかが使用できます。
   :func:`codecs.register_error` に登録されている他のエラー処理名も指定できます。

   *newline* ではユニバーサルニューラインの挙動を制御しています (テキストモードのみ有効です)。
   ``None``\ 、 ``''``\ 、 ``'\n'``\ 、 ``'\r'``\ 、 ``'\r\n'`` が指定できます。
   以下のように動作します：

   * 入力時、 *newline* が ``None`` の場合はユニバーサルニューラインモードが有効になります。
     入力行の行末は ``'\n'`` \、 ``'\r'`` \、 ``'\r\n'`` のいずれかで、
     それらは呼び出し元に戻される前に ``'\n'`` に変換されます。
     もし ``''`` だった場合はユニバーサルニューラインモードは有効になりますが、行末は変換されずに呼び出し元に戻されます。
     他の適切な値が指定された場合は、入力行は与えられた文字列で分断され、行末は変換されずに呼び出し元に戻されます。

   * 出力時、 *newline* が ``None`` の場合は、すべての ``'\n'`` 文字はシステムのデフォルト行区切り文字 :data:`os.linesep` に変換されます。
     もし *newline* が ``''`` の場合、変換は起こりません。
     もし *newline* に他の適切な値が指定された場合は、 ``'\n'`` 文字は与えられた文字に変換されます。

   もし *closefd* が ``False`` で、ファイル名ではなくてファイル記述子が与えられていた場合、
   処理中のファイル記述子はファイルが閉じられた後も開いたままとなります。
   もしファイル名が与えられていた場合は、 *closefd* は関係ありません。しかし ``True`` でなければなりません。(デフォルト値)

   :func:`.open` によって返されるファイルオブジェクトの型はモードに依存します。
   :func:`.open` がテキストモードでファイルを開くために使われた場合
   (``'w'``\ 、 ``'r'``\ 、 ``'wt'``\ 、 ``'rt'`` など) :class:`TextIOBase` の
   サブクラス (具体的には :class:`TextIOWrapper`) が返されます。
   バッファリングをしてバイナリモードでファイルを開く場合、
   :class:`BufferedIOBase` のサブクラスが返されます。具体的なクラスは多様です。
   もし読み取り専用のバイナリモードだった場合は :class:`BufferedReader` が返されます。
   書き込み専用のバイナリモードだった場合は :class:`BufferedWriter` が返されます。
   読み書き可能なバイナリモードの場合は :class:`BufferedRandom` が返されます。
   バッファリングが無効な場合、raw ストリーム、 :class:`RawIOBase` のサブクラス、
   :class:`FileIO` が返されます。

   :class:`unicode` 文字列や :class:`bytes` 文字列をファイルとして読み書きすることも可能です。
   :class:`unicode` 文字列では :class:`StringIO` を使えばテキストモードで開いたファイルのように扱えます。
   :class:`bytes`  では :class:`BytesIO` を使えばバイナリモードで開いたファイルのように扱えます。


.. exception:: BlockingIOError

   非ブロッキングストリームでブロック処理が起きた場合に発生するエラーです。
   :exc:`IOError` を継承しています。

   :exc:`IOError` で持っている属性以外に :exc:`BlockingIOError` では次の属性を持っています。


   .. attribute:: characters_written

      ブロック前にストリームに書き込まれる文字数を保持する整数値です。


.. exception:: UnsupportedOperation

   :exc:`IOError` と :exc:`ValueError` を継承した例外で、ストリームに未サポートの操作が行われた場合に発生します。


I/O 基底クラス
----------------

.. class:: IOBase

   すべての I/O クラスの抽象基底クラスです。バイトストリームへの操作を行います。
   パブリックなコンストラクタはありません。

   このクラスでは、継承先のクラスが選択的にオーバライドできるように多くの空の抽象実装が提供されます。
   デフォルトの実装では、読み込み、書き込み、シークができないファイルを表現します。

   :class:`IOBase` では :meth:`read`, :meth:`readinto`, :meth:`write` が宣言されていませんが、
   これはシグナチャが変化するためで、実装やクライアントはこれらのメソッドをインタフェースの一部として考えるべきです。
   また、実装はサポートしていない操作を呼び出されたときは :exc:`IOError` を発生させるかもしれません。

   ファイルへの読み書きに用いられるバイナリデータに使われる基本型は
   (:class:`str` としても知られている) :class:`bytes` です。
   :class:`bytearray` も利用可能で、いくつかのケース (たとえば :class:`readinto`) では必須です。
   テキスト I/O クラスは :class:`unicode` データを扱います。

   閉じられたストリームに対するメソッド呼び出しは (問い合わせであっても) 未定義です。
   この場合、実装は :exc:`IOError` を発生させることがあります。

   IOBase (とそのサブクラス) はイテレータプロトコルをサポートします。
   すなわち :class:`IOBase` オブジェクトはストリーム内の行を yield を使って
   イテレートすることができます。
   行は、ストリームが (:class:`bytes` を与える) バイナリストリームか
   (:class:`unicode` 文字列を与える) テキストストリームかに依って、
   少し違う定義がされています。下の :meth:`readline` を参照してください。

   IOBase はコンテキストマネージャでもあります。そのため :keyword:`with` 構文をサポートします。
   次の例では、 :keyword:`with` 構文が終わった後で---たとえ例外が発生した場合でも、 *file* は閉じられます。


   ::

      with io.open('spam.txt', 'w') as file:
          file.write(u'Spam and eggs!')


   :class:`IOBase` は以下のデータ属性とメソッドを提供します:


   .. method:: close()

      このストリームをフラッシュして閉じます。このメソッドはファイルが既に閉じられていた場合は
      特に何の効果もありません。
      いったんファイルが閉じられると、すべてのファイルに対する操作 (例えば読み込みや書き込み) で :exc:`ValueError` が発生します。

      利便性のために、このメソッドを複数回呼ぶことは許可されています。
      しかし、効果があるのは最初の1回だけです。


   .. attribute:: closed

      ストリームが閉じられていた場合 True になります。


   .. method:: fileno()

      ストリームが保持しているファイル記述子 (整数値) が存在する場合はそれを返します。
      もし IO オブジェクトがファイル記述子を使っていない場合は :exc:`IOError` が発生します。


   .. method:: flush()

      適用可能であればストリームの書き込みバッファをフラッシュします。
      読み込み専用や非ブロッキングストリームでは何もしません。


   .. method:: isatty()

      ストリームが対話的であれば (つまりターミナルや tty デバイスにつながっている場合)
      ``True`` を返します。


   .. method:: readable()

      ストリームが読み込める場合 ``True`` を返します。
      False の場合は :meth:`read` は :exc:`IOError` を発生させます。


   .. method:: readline(limit=-1)

      ストリームから 1 行読み込んで返します。
      もし *limit* が指定された場合、最大で *limit* バイトが読み込まれます。

      バイナリファイルでは行末文字は常に ``b'\n'`` となります。
      テキストファイルでは、認識される行末文字を選択するために :func:`.open` に対する *newlines* 引数が使われます。


   .. method:: readlines(hint=-1)

      ストリームから行のリストを読み込んで返します。
      *hint* を指定することで、読み込む行数を制御できます。
      もし読み込んだすべての行のサイズ (バイト数、もしくは文字数) が
      *hint* の値を超えた場合、読み込みをそこで終了します。


   .. method:: seek(offset, whence=SEEK_SET)

      ストリーム位置を指定された *offset* バイトに変更します。
      *offset* は *whence* で指定された位置からの相対位置として解釈されます。
      *whence* に指定できる値は：

      * :data:`SEEK_SET` または ``0`` -- ストリームの先頭 (デフォルト)。 *offset* は 0 もしくは正の値でなければなりません。
      * :data:`SEEK_CUR` または ``1`` -- 現在のストリーム位置。 *offset* は負の値も可能です。
      * :data:`SEEK_END` または ``2`` -- ストリームの末尾。 *offset* は通常負の値です。

      新しい絶対位置を返します。

      .. versionadded:: 2.7
         ``SEEK_*`` 定数


   .. method:: seekable()

      もしストリームがランダムアクセスをサポートしていた場合 ``True`` を返します。
      ``False`` の場合は :meth:`seek`\ 、 :meth:`tell`\ 、 :meth:`truncate` は :exc:`IOError` を発生させます。


   .. method:: tell()

      現在のストリーム位置を返します。


   .. method:: truncate(size=None)

      指定された *size* バイト (または *size* が指定されなければ現在の位置) にストリームをリサイズします。
      現在のストリーム位置は変更されません。
      このリサイズは、現在のファイルサイズを拡大または縮小させることができます。
      拡大の場合には、新しいファイル領域の内容はプラットホームに依存します
      (ほとんどのシステムでは、追加のバイトが 0 で埋められます。 Windowsでは不定です)。
      新しいファイルサイズが返されます。


   .. method:: writable()

      ストリームが書き込みをサポートしている場合 ``True`` を返します。
      ``False`` の場合は :meth:`write`\ 、 :meth:`truncate` は :exc:`IOError` を返します。


   .. method:: writelines(lines)

      ストリームに複数行書き込みます。
      行区切り文字は付与されないので、通常書き込む各行の行末には行区切り文字があります。


.. class:: RawIOBase

   生のバイナリ I/O への基底クラスです。 :class:`IOBase` を継承しています。
   パブリックコンストラクタはありません。

   生のバイナリ I/O は典型的に、下にある OS デバイスや API への、
   低レベルなアクセスを提供し、高レベルな基本要素へとカプセル化しようとは
   しません (これはこのページで後述する Buffered I/O や Text I/O に任せます)。

   :class:`IOBase` の属性やメソッドに加えて、 RawIOBase は次のメソッドを提供します：


   .. method:: read(n=-1)

      オブジェクトを *n* バイトまで読み込み、それを返します。
      簡単のため、 *n* が指定されていないか -1 なら、
      :meth:`readall` が呼び出されます。そうでなければ、システムコール呼び出しが一度だけ行われます。
      既に EOF に達していたら空のバイトオブジェクトが返されます。
      オペレーティングシステムコールが返したものがが *n* バイトより少なければ、
      *n* バイトより少なく返されることがあります。

      0 バイトが返って、 *n* が 0 でなければ、それはファイルの終端を表します。
      オブジェクトがノンブロッキングモードで、
      1 バイトも読み込めなければ、 ``None`` が返されます。


   .. method:: readall()

      EOF までストリームからすべてのバイトを読み込みます。
      必要な場合はストリームに対して複数の呼び出しをします。


   .. method:: readinto(b)

      bytearray *b* に最大 len(b) バイト分読み込み、読み込んだバイト数を返します。
      オブジェクトがノンブロッキングモードで、
      1 バイトも読み込めなければ、 ``None`` が返されます。


   .. method:: write(b)

      与えられた bytes または bytearray オブジェクト *b* を生ストリームに書き込み、書き込んだバイト数を返します。
      これは、根底の生ストリームの性質や、特にノンブロッキングである場合に、
      ``len(b)`` より小さくなり得ます。
      生ストリームがブロックされないように設定されていて、
      1 バイトも読み込めるように書きこまれなければ、 ``None`` が返されます。


.. class:: BufferedIOBase

   何らかのバッファリングをサポートするバイナリストリームの基底クラスです。
   :class:`IOBase` を継承します。
   パブリックなコンストラクタはありません。

   :class:`RawIOBase` との主な違いは、メソッド :meth:`read`, :meth:`readinto`
   および :meth:`write` メソッドは (それぞれ) 要求されただけの入力を
   読み込もうとしたり、システムコールを、必要なら複数回、する費用として
   全ての与えられた出力消費しようとすることです。

   加えて、元になる生ストリームが非ブロッキングモードでかつ準備ができていない場合に、
   これらのメソッドは、 :exc:`BlockingIOError` を送出するかもしれません。
   対応する :class:`RawIOBase` バージョンと違って、 ``None`` を返すことはありません。

   さらに、 :meth:`read` メソッドは、 :meth:`readinto` に従うデフォルト実装を持ちません。

   通常の :class:`BufferedIOBase` 実装は :class:`RawIOBase` 実装を継承せずに、
   :class:`BufferedWriter` と :class:`BufferedReader` がするようにこれを
   ラップすべきです。

   :class:`BufferedIOBase` は :class:`IOBase` からのメンバに加えて、
   以下のメソッドを提供もしくはオーバーライドします:

   .. attribute:: raw

      :class:`BufferedIOBase` が扱う根底の生ストリーム (:class:`RawIOBase`
      インスタンス) を返します。これは :class:`BufferedIOBase` API には
      含まれず、よって実装に含まれないことがあります。

   .. method:: detach()

      根底の生ストリームをバッファから分離して返します。

      生ストリームが取り外された後、バッファは使用不能状態になります。

      バッファには、 :class:`BytesIO` など、このメソッドで返される
      単体のストリームという概念を持たないものがあります。これらは
      :exc:`UnsupportedOperation` を送出します。

      .. versionadded:: 2.7


   .. method:: read(n=-1)

      最大で *n* バイト読み込み、返します。
      引数が省略されるか、 ``None`` か、または負の値であった場合、
      データは EOF に到達するまで読み込まれます。
      ストリームが既に EOF に到達していた場合は空の bytes オブジェクトが返されます。

      引数が正で、元になる生ストリームが対話的でなければ、
      必要なバイト数を満たすように複数回の生 read が発行されるかもしれません
      (先に EOF に到達しない限りは)。
      対話的な場合は、最大で一回の raw read しか発行されず、
      短い結果でも EOF に達したことを意味しません。

      元になる生ストリームがノンブロッキングモードで、呼び出された時点で
      データを持っていなければ、
      :exc:`BlockingIOError` が送出されます。

   .. method:: read1(n=-1)

      根底の生ストリームの :meth:`~RawIOBase.read` メソッドを
      高々 1 回呼び出し、最大で *n* バイト読み込み、返します。
      これは、 :class:`BufferedIOBase` オブジェクトの上に
      独自のバッファリングを実装するときに便利です。

   .. method:: readinto(b)

      bytearray *b* に最大 len(b) バイト読み込み、何バイト読んだかを返します。

      :meth:`read` と同様、元になる生ストリームが '対話的' でない限り、
      複数回の read が発行されるかもしれません。

      元になる生ストリームが呼び出された時点でデータを持っていなければ、
      :exc:`BlockingIOError` が送出されます。


   .. method:: write(b)

      与えられた bytes または bytearray オブジェクト *b* を書き込み、書き込んだバイト数を返します
      (これは決して ``len(b)`` よりも小さくなることはありません。
      なぜなら、もし書き込みに失敗した場合は :exc:`IOError` が発生するからです)。
      実際の実装に依って、これらのバイトは根底のストリームに読めるように
      書きこまれたり、パフォーマンスとレイテンシの理由でバッファに
      保持されたりします。

      ノンブロッキングモードであるとき、バッファが満杯で根底の生ストリームが
      書き込み時点でさらなるデータを受け付けられない場合
      :exc:`BlockingIOError` が送出されます。


生ファイルI/O
--------------

.. class:: FileIO(name, mode='r', closefd=True)

   :class:`FileIO` はバイトデータを含む OS レベルのファイルを表します。
   :class:`RawIOBase` インタフェースを (したがって :class:`IOBase` インタフェースも) 実装しています。

   *name* はこの 2 つのいずれかに出来ます:

   * 開くファイルのパスを表す文字列
   * 結果の :class:`FileIO` オブジェクトがアクセスを与える、
     既存の OS レベルファイルディスクリプタの数を表す整数

   *mode* はそれぞれ読み込み (デフォルト)、書き込み、追記を表す ``'r'``\ 、 ``'w'``\ 、 ``'a'`` にすることができます。
   ファイルは書き込みまたは追記モードで開かれたときに存在しなければ作成されます。
   書き込みモードでは存在したファイル内容は消されます。
   読み込みと書き込みを同時に行いたければ ``'+'`` をモードに加えて下さい。

   このクラスの :meth:`read` (正の引数で呼び出されたとき), :meth:`readinto`
   および :meth:`write` メソッドは、単にシステムコールを一度呼び出します。

   :class:`IOBase` および :class:`RawIOBase` から継承した属性とメソッドに加えて、
   :class:`FileIO` は以下のデータ属性とメソッドを提供しています:

   .. attribute:: mode

      コンストラクタに渡されたモードです。


   .. attribute:: name

      ファイル名。
      コンストラクタに名前が渡されなかったときはファイル記述子になります。


バッファ付きストリーム
----------------------

バッファ付き I/O ストリームは、I/O デバイスに生 I/O より高レベルな
インタフェースを提供します。

.. class:: BytesIO([initial_bytes])

   インメモリの bytes バッファを利用したストリームの実装。
   :class:`BufferedIOBase` を継承します。

   引数 *initial_bytes* は省略可能な :class:`bytes` の初期値です。

   :class:`BytesIO` は :class:`BufferedIOBase` または :class:`IOBase` からのメソッドに加えて、
   以下のメソッドを提供もしくはオーバーライドします:

   .. method:: getvalue()

      バッファの全内容を保持した ``bytes`` を返します。


   .. method:: read1()

      :class:`BytesIO` においては、このメソッドは :meth:`read` と同じです。


.. class:: BufferedReader(raw, buffer_size=DEFAULT_BUFFER_SIZE)

   読み込み可能でシーケンシャルな :class:`RawIOBase` オブジェクトへの、高レベルな
   アクセスを提供するバッファです。 :class:`BufferedIOBase` を継承します。
   このオブジェクトからデータを読み込むとき、根底の生ストリームから
   より大きい量のデータが要求されることがあり、内部バッファに保存されます。
   バッファされたデータは、続く読み込み時に直接返されます。

   このコンストラクタは与えられた *raw* ストリームと *buffer_size* に対し :class:`BufferedReader` を生成します。
   *buffer_size* が省略された場合、代わりに :data:`DEFAULT_BUFFER_SIZE` が使われます。

   :class:`BufferedReader` は :class:`BufferedIOBase` または :class:`IOBase` からのメソッドに加えて、
   以下のメソッドを提供もしくはオーバーライドします:

   .. method:: peek([n])

      バイト列をストリームから位置を変更せずに読んで返します。
      これを果たすために生ストリームに対して行われる read は高々一度だけです。
      返されるバイト数は、要求されたより少なくまたは多くなるかもしれません。


   .. method:: read([n])

      *n* バイトを読み込んで返します。
      *n* が与えられないかまたは負の値ならば、EOF まで、
      または非ブロッキングモード中で read 呼び出しがブロックされるまでを返します。


   .. method:: read1(n)

      生ストリームに対しただ一度の呼び出しで最大 *n* バイトを読み込んで返します。
      少なくとも 1 バイトがバッファされていれば、バッファされているバイト列だけが返されます。
      それ以外の場合にはちょうど一回生ストリームに read 呼び出しが行われます。


.. class:: BufferedWriter(raw, buffer_size=DEFAULT_BUFFER_SIZE)

   書き込み可能でシーケンシャルな :class:`RawIOBase` オブジェクトへの、高レベルな
   アクセスを提供するバッファです。 :class:`BufferedIOBase` を継承します。
   このオブジェクトに書き込むとき、データは通常内部バッファに保持されます。
   このバッファは、以下のような種々の状況で根底の
   :class:`RawIOBase` オブジェクトに書きこまれます:

   * 未解決のデータに対してバッファが足りなくなったとき
   * :meth:`flush()` が呼び出されたとき
   * :meth:`seek()` が (:class:`BufferedRandom` オブジェクトに対して)
     呼び出されたとき;
   * :class:`BufferedWriter` オブジェクトが閉じられたり破棄されたりしたとき

   このコンストラクタは与えられた書き込み可能な *raw* ストリームに対し :class:`BufferedWriter` を生成します。
   *buffer_size* が省略された場合、 :data:`DEFAULT_BUFFER_SIZE` がデフォルトになります。

   第三引数 *max_buffer_size* が提供されていますが、使われず、非推奨です。

   :class:`BufferedWriter` は :class:`BufferedIOBase` または :class:`IOBase` からのメソッドに加えて、
   以下のメソッドを提供もしくはオーバーライドします:

   .. method:: flush()

      バッファに保持されたバイト列を生ストリームに流し込みます。
      生ストリームがブロックした場合 :exc:`BlockingIOError` が送出されます。

   .. method:: write(b)

      bytes または bytearray オブジェクト *b* を書き込み、書き込んだバイト数を返します。
      ノンブロッキング時、バッファが書き込まれるべきなのに生ストリームが
      ブロックした場合 :exc:`BlockingIOError` が送出されます。


.. class:: BufferedRWPair(reader, writer, buffer_size=DEFAULT_BUFFER_SIZE)

   書き込み可能と書き込み可能の二つのシーケンシャルな
   :class:`RawIOBase` オブジェクトへの、くみあわさった高レベルなアクセスを
   提供する、バッファ付き I/O オブジェクトです。
   これは、単方向のコミニュケーションチャネルの対 (パイプなど) に便利です。
   :class:`BufferedIOBase` を継承しています。

   *reader* と *writer* はそれぞれ読み込み可能、書き込み可能な :class:`RawIOBase` オブジェクトです。
   *buffer_size* が省略された場合 :data:`DEFAULT_BUFFER_SIZE` がデフォルトになります。

   第四引数 *max_buffer_size* が提供されていますが、使われず、非推奨です。

   :class:`BufferedRWPair` は、 :exc:`UnsupportedOperation` を送出する
   :meth:`~BufferedIOBase.detach` を除く、
   :class:`BufferedIOBase` の全てのメソッドを実装します。


.. class:: BufferedRandom(raw, buffer_size=DEFAULT_BUFFER_SIZE)

   ランダムアクセスストリームへのバッファ付きインタフェース。
   :class:`BufferedReader` および :class:`BufferedWriter` を継承し、さらに
   :meth:`seek` および :meth:`tell` をサポートしています。

   このコンストラクタは第一引数として与えられるシーク可能な生ストリームに対し、リーダーおよびライターを作成します。
   *buffer_size* が省略された場合、 :data:`DEFAULT_BUFFER_SIZE` がデフォルトになります。

   第三引数 *max_buffer_size* が提供されていますが、使われず、非推奨です。

   :class:`BufferedRandom` は :class:`BufferedReader` や :class:`BufferedWriter` にできることは何でもできます。


テキスト I/O
------------

.. class:: TextIOBase

   テキストストリームの基底クラスです。
   このクラスはストリーム I/O への Unicode 文字と行に基づいたインタフェースを提供します。
   Python の :class:`unicode` 文字列は変更不可能なので、 :meth:`readinto` メソッドは存在しません。
   :class:`IOBase` を継承します。
   パブリックなコンストラクタはありません。

   :class:`IOBase` から継承した属性とメソッドに加えて、
   :class:`TextIOBase` は以下のデータ属性とメソッドを提供しています:

   .. attribute:: encoding

      エンコーディング名で、ストリームのバイト列を文字列にデコードするとき、
      また文字列をバイト列にエンコードするときに使われます。

   .. attribute:: errors

      このエンコーダやデコーダのエラー設定です。

   .. attribute:: newlines

      文字列、文字列のタプル、または ``None`` で、改行がどのように読み換えられるかを指定します。
      実装や内部コンストラクタのフラグに依って、これは利用できないことがあります。

   .. attribute:: buffer

      :class:`TextIOBase` が扱う根底のバイナリバッファ (:class:`BufferedIOBase`
      インスタンス) です。これは :class:`TextIOBase` API には
      含まれず、よって実装に含まれないことがあります。

   .. method:: detach()

      根底のバイナリバッファを :class:`TextIOBase` から分離して返します。

      根底のバッファが取り外された後、 :class:`TextIOBase` は
      使用不能状態になります。

      :class:`TextIOBase` 実装には、 :class:`StringIO` など、
      根底のバッファという概念を持たないものがあります。これらを呼び出すと
      :exc:`UnsupportedOperation` を送出します。

      .. versionadded:: 2.7

   .. method:: read(n)

      最大 *n* 文字をストリームから読み込み、一つの :class:`unicode` にして返します。
      *n* が負の値または ``None`` ならば、 EOF まで読みます。

   .. method:: readline()

      改行または EOF まで読み込み、一つの ``unicode`` を返します。
      ストリームが既に EOF に到達している場合、空文字列が返されます。

   .. method:: write(s)

      :class:`unicode` 文字列 *s* をストリームに書き込み、書き込まれた文字数を返します。


.. class:: TextIOWrapper(buffer, encoding=None, errors=None, newline=None, line_buffering=False)

   :class:`BufferedIOBase` バイナリストリーム上のバッファ付きテキストストリーム。
   :class:`TextIOBase` を継承します。

   *encoding* にはストリームをデコードしたりそれを使ってエンコードしたりするエンコーディング名を渡します。
   デフォルトは :func:`locale.getpreferredencoding` です。

   *errors* はオプションの文字列で、エンコードやデコードの際のエラーをどのように扱うかを指定します。
   エンコードエラーがあったら :exc:`ValueError` 例外を送出させるには ``'strict'`` を渡します(デフォルトの ``None`` でも同じです)。
   エラーを無視させるには ``'ignore'`` です。
   (注意しなければならないのは、エンコーディングエラーを無視するとデータ喪失につながる可能性があるということです。)
   ``'replace'`` は正常に変換されなかった文字の代わりにマーカ (たとえば ``'?'``) を挿入させます。
   書き込み時には ``'xmlcharrefreplace'`` (適切な XML 文字参照に置き換え) や
   ``'backslashreplace'`` (バックスラッシュによるエスケープシーケンスに置き換え) も使えます。
   他にも :func:`codecs.register_error` で登録されたエラー処理名が有効です。

   *newline* は ``None``\ 、 ``''``\ 、 ``'\n'``\ 、 ``'\r'``\ 、 ``'\r\n'`` のいずれかです。
   行末の扱いを制御します。
   ``None`` では、ユニバーサルニューラインが有効になります。
   これが有効になると、入力時、行末の ``'\n'``\ 、 ``'\r'``\ 、 ``'\r\n'`` は
   ``'\n'`` に変換されて呼び出し側に返されます。
   逆に出力時は ``'\n'`` がシステムのデフォルト行区切り文字 (:data:`os.linesep`) に変換されます。
   *newline* が他の適切な値の場合には、ファイル読み込みの際にその改行で改行されるようになり、変換は行われません。
   出力時には ``'\n'`` が *newline* に変換されます。

   *line_buffering* が ``True`` の場合、 write への呼び出しが改行文字を含んでいれば
   :meth:`flush` がそれに伴って呼び出されます。


   .. :class:`TextIOWrapper` provides these data attributes in addition to those of
   .. :class:`TextIOBase` and its parents:

   :class:`TextIOBase` およびその親クラスの属性に加えて、
   :class:`TextIOWrapper` は以下の属性を提供しています:

   .. attribute:: line_buffering

      行バッファリングが有効かどうか。


.. class:: StringIO(initial_value=u'', newline=None)

   Unicode テキストのためのインメモリストリーム。
   :class:`TextIOWrapper` を継承します。

   このバッファの初期値は (デフォルトでは空の Unicode 文字列で)、
   *initial_value* を与えることで設定できます。

   :class:`TextIOWrapper` およびその親クラスから継承したメソッドに加えて
   :class:`StringIO` は以下のメソッドを提供しています:

   .. method:: getvalue()

      :class:`StringIO` オブジェクトの :meth:`close` のバッファの全内容を保持した ``unicode`` を返します。

   使用例::

      import io

      output = io.StringIO()
      output.write(u'First line.\n')
      output.write(u'Second line.\n')

      # Retrieve file contents -- this will be
      # u'First line.\nSecond line.\n'
      contents = output.getvalue()

      # Close object and discard memory buffer --
      # .getvalue() will now raise an exception.
      output.close()


.. class:: IncrementalNewlineDecoder

   ユニバーサルニューラインモード向けに改行をデコードする補助コーデック。
   :class:`codecs.IncrementalDecoder` を継承します。


進んだ話題
----------

ここで、上述の I/O 実装に関係するいくつかの進んだ話題について議論します。

パフォーマンス
^^^^^^^^^^^^^^

バイナリ I/O
""""""""""""

バッファ付き I/O は、ユーザが 1 バイトだけ要求したときでさえ、
データを大きな塊でのみ読み書きします。
これにより、オペレーティングシステムのバッファ無し I/O ルーチンを
呼び出して実行する非効率性をすべて隠しています。
その成果は、OS と処理される I/O の種類に本当にとても大きく依存します
(例えば、Linux のような 現行の OS では、バッファ無しディスク I/O
がバッファ付き I/O と同じくらい早いことがあります)。
しかし、最低でも、バッファ付き I/O は予測できるパフォーマンスを提供します。
ですから、ほとんどいつも、バッファ無し I/O より、バッファ付きの I/O を
使うほうが望ましいです。

テキスト I/O
""""""""""""

(ファイルなどの) バイナリストレージ上のテキスト I/O は、同じストレージ上の
バイナリ I/O より非常に遅いです。なぜならこれは、文字コーデックを使った
Unicode からバイナリデータへの変換を暗示しているからです。これは
大量のテキストデータ (例えば非常に大きなログファイル) を扱うときに
顕著に成り得ます。同様に、 :meth:`TextIOWrapper.tell` や
:meth:`TextIOWrapper.seek` はどちらも、使われている復元アルゴリズムのために
遅くなります。

しかし :class:`StringIO` は、ネイティブなインメモリ Unicode コンテナで、
:class:`BytesIO` と同程度の速度を示します。

マルチスレッディング
^^^^^^^^^^^^^^^^^^^^

(Unix における ``read(2)`` のような)
オペレーティングシステムコールの、それがラッピングしているものが
スレッドセーフであるような範囲内では、
:class:`FileIO` オブジェクトもまた、スレッドセーフです。

バイナリバッファ付きオブジェクト (:class:`BufferedReader`,
:class:`BufferedWriter`, :class:`BufferedRandom` および :class:`BufferedRWPair`
のインスタンス) は、その内部構造をロックを使って保護します。
このため、これらを複数のスレッドから同時に呼び出しても安全です。

:class:`TextIOWrapper` オブジェクトはスレッドセーフではありません。

リエントラント性
^^^^^^^^^^^^^^^^

バイナリバッファ付きオブジェクト (:class:`BufferedReader`,
:class:`BufferedWriter`, :class:`BufferedRandom` および :class:`BufferedRWPair`
のインスタンス) は、リエントラントではありません。
リエントラントな呼び出しは普通の状況では起こりませんが、
I/O を :mod:`signal` ハンドラで行なっているときに起こりえます。
バッファ化されたオブジェクトに、すでに *同じスレッドから* アクセスされている
のにもかかわらず、再び入ろうとすると :exc:`RuntimeError` が
送出されます。

:func:`open()` 関数は
:class:`TextIOWrapper` 内部のバッファ付きオブジェクトをラップするため、
テキストファイルにも暗黙に拡張されます。
これは、標準ストリームを含むので、組み込み関数 :func:`print()` にも
同様に影響します。

