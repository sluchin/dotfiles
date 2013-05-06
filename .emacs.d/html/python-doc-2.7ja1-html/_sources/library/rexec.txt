:mod:`rexec` --- 制限実行のフレームワーク
=========================================

.. module:: rexec
   :synopsis: 基本的な制限実行フレームワーク。
   :deprecated:

.. deprecated:: 2.6
   :mod:`rexec` モジュールは Python 3.0 で削除されました。

.. versionchanged:: 2.3
   Disabled module.

.. warning::

   このドキュメントは、 :mod:`rexec` モジュールを使用している古いコードを読む際の参照用として残されています。

このモジュールには :class:`RExec` クラスが含まれています。このクラスは、 :meth:`r_eval` 、 :meth:`r_execfile` 、
:meth:`r_exec` および :meth:`r_import` メソッドをサポートし、これらは標準の Python 関数 :meth:`eval` 、
:meth:`execfile` および :keyword:`exec` と :keyword:`import` 文の制限されたバージョンです。
この制限された環境で実行されるコードは、安全であると見なされたモジュールや関数だけにアクセスします; :class:`RExec` をサブクラス化すれば、
望むように能力を追加および削除できます。

.. warning::

   :mod:`rexec` モジュールは、下記のように動作するべく設計されてはいますが、注意深く書かれたコードなら利用できてしまうかもしれない、
   既知の脆弱性がいくつかあります。従って、"製品レベル" のセキュリティを要する状況では、 :mod:`rexec` の動作をあてにするべきではありません。
   製品レベルのセキュリティを求めるなら、サブプロセスを介した実行や、あるいは処理するコードとデータの両方に対する非常に注意深い  "浄化"
   が必要でしょう。上記の代わりに、 :mod:`rexec` の既知の脆弱性に対するパッチ当ての手伝いも歓迎します。

.. note::

   :class:`RExec` クラスは、プログラムコードによるディスクファイルの読み書きや TCP/IP ソケットの利用といった、
   安全でない操作の実行を防ぐことができます。しかし、プログラムコードよる非常に大量のメモリや処理時間の消費に対して防御することはできません。


.. class:: RExec([hooks[, verbose]])

   :class:`RExec` クラスのインスタンスを返します。

   *hooks* は、 :class:`RHooks` クラスあるいはそのサブクラスのインスタンスです。 *hooks* が省略されているか ``None``
   であれば、デフォルトの :class:`RHooks` クラスがインスタンス化されます。 :mod:`rexec` モジュールが (組み込みモジュールを含む)
   あるモジュールを探したり、あるモジュールのコードを読んだりする時は常に、 :mod:`rexec` がじかにファイルシステムに出て行くことはありません。
   その代わり、あらかじめ :class:`RHooks` クラスに渡しておいたり、コンストラクタで生成された :class:`RHooks`
   インスタンスのメソッドを呼び出します。

   (実際には、 :class:`RExec` オブジェクトはこれらを呼び出しません ---  呼び出しは、 :class:`RExec`
   オブジェクトの一部であるモジュールローダオブジェクトによって行われます。これによって別のレベルの柔軟性が実現されます。この柔軟性は、制限された\
   環境内で :keyword:`import` 機構を変更する時に役に立ちます。 )

   代替の :class:`RHooks` オブジェクトを提供することで、モジュールをインポートする際に行われるファイルシステムへのアクセスを制御する\
   ことができます。このとき、各々のアクセスが行われる順番を制御する実際のアルゴリズムは変更されません。例えば、 :class:`RHooks`
   オブジェクトを置き換えて、ILU のようなある種の RPC メカニズムを介することで、全てのファイルシステムの要求を\
   どこかにあるファイルサーバに渡すことができます。 Grail のアプレットローダは、アプレットを URL からディレクトリ上に import
   する際にこの機構を使っています。

   もし *verbose* が true であれば、追加のデバッグ出力が標準出力に送られます。

制限された環境で実行するコードも、やはり :func:`sys.exit` 関数を呼ぶことができることを知っておくことは大事なことです。制限された\
コードがインタプリタから抜けだすことを許さないためには、いつでも、制限されたコードが、 :exc:`SystemExit` 例外をキャッチする
:keyword:`try` / :keyword:`except` 文とともに実行するように、呼び出しを防御します。制限された環境から
:func:`sys.exit` 関数を除去するだけでは不十分です -- 制限されたコードは、やはり ``raise SystemExit``
を使うことができてしまいます。 :exc:`SystemExit` を取り除くことも、合理的なオプションではありません;
いくつかのライブラリコードはこれを使っていますし、これが利用できなくなると中断してしまうでしょう。


.. seealso::

   `Grail のホームページ <http://grail.sourceforge.net/>`_
      Grail はすべて Python で書かれた Web ブラウザです。これは、 :mod:`rexec` モジュールを、Python
      アプレットをサポートするのに使っていて、このモジュールの使用例として使うことができます。


.. _rexec-objects:

RExec オブジェクト
------------------

:class:`RExec` インスタンスは以下のメソッドをサポートします:

.. method:: RExec.r_eval(code)

   *code* は、Python の式を含む文字列か、あるいはコンパイルされたコードオブジェクトのどちらかでなければなりません。そしてこれらは制限された環境の
   :mod:`__main__` モジュールで評価されます。式あるいはコードオブジェクトの値が返されます。


.. method:: RExec.r_exec(code)

   *code* は、1行以上の Python コードを含む文字列か、コンパイルされたコードオブジェクトのどちらかでなければなりません。そしてこれらは、
   制限された環境の :mod:`__main__` モジュールで実行されます。


.. method:: RExec.r_execfile(filename)

   ファイル *filename* 内の Python コードを、制限された環境の :mod:`__main__` モジュールで実行します。

名前が ``s_`` で始まるメソッドは、 ``r_`` で始まる関数と同様ですが、そのコードは、標準 I/O ストリーム ``sys.stdin`` 、
``sys.stderr`` および  ``sys.stdout`` の制限されたバージョンへのアクセスが許されています。


.. method:: RExec.s_eval(code)

   *code* は、Python 式を含む文字列でなければなりません。そして制限された環境で評価されます。


.. method:: RExec.s_exec(code)

   *code* は、1行以上のPython コードを含む文字列でなければなりません。そして制限された環境で実行されます。


.. method:: RExec.s_execfile(code)

   ファイル *filename* に含まれた Python コードを制限された環境で実行します。

:class:`RExec` オブジェクトは、制限された環境で実行されるコードによって暗黙のうちに呼ばれる、さまざまなメソッドもサポートしなければなりません。
これらのメソッドをサブクラス内でオーバライドすることによって、制限された環境が強制するポリシを変更します。


.. method:: RExec.r_import(modulename[, globals[, locals[, fromlist]]])

   モジュール *modulename* をインポートし、もしそのモジュールが安全でないとみなされるなら、 :exc:`ImportError` 例外を発生します。


.. method:: RExec.r_open(filename[, mode[, bufsize]])

   :func:`open` が制限された環境で呼ばれるとき、呼ばれるメソッドです。引数は :func:`open` のものと同じであり、ファイルオブジェクト
   (あるいはファイルオブジェクトと互換性のあるクラスインスタンス)が返されます。 :class:`RExec` のデフォルトの動作は、任意のファイルを\
   読み取り用にオープンすることを許可しますが、ファイルに書き込もうとすることは許しません。より制限の少ない :meth:`r_open` の実装については、
   以下の例を見て下さい。


.. method:: RExec.r_reload(module)

   モジュールオブジェクト *module* を再ロードして、それを再解析し再初期化します。


.. method:: RExec.r_unload(module)

   モジュールオブジェクト *module* をアンロードします (それを制限された環境の ``sys.modules`` 辞書から取りのぞきます)。

および制限された標準 I/O ストリームへのアクセスが可能な同等のもの:


.. method:: RExec.s_import(modulename[, globals[, locals[, fromlist]]])

   モジュール *modulename* をインポートし、もしそのモジュールが安全でないとみなされるなら、 :exc:`ImportError` 例外を発生します。


.. method:: RExec.s_reload(module)

   モジュールオブジェクト *module* を再ロードして、それを再解析し再初期化します。


.. method:: RExec.s_unload(module)

   モジュールオブジェクト *module* をアンロードします。

   .. % XXX これのセマンティクスはどうなりますか？


.. _rexec-extension:

制限された環境を定義する
------------------------

:class:`RExec` クラスには以下のクラス属性があります。それらは、 :meth:`__init__` メソッドが使います。それらを既存の\
インスタンス上で変更しても何の効果もありません; そうする代わりに、 :class:`RExec` のサブクラスを作成して、そのクラス定義でそれらに\
新しい値を割り当てます。そうすると、新しいクラスのインスタンスは、これらの新しい値を使用します。これらの属性のすべては、文字列のタプルです。


.. attribute:: RExec.nok_builtin_names

   制限された環境で実行するプログラムでは利用でき *ない* であろう、組み込み関数の名前を格納しています。 :class:`RExec` に対する値は、
   ``('open', 'reload', '__import__')`` です。 (これは例外です。というのは、組み込み関数のほとんど大多数は\
   無害だからです。この変数をオーバライドしたいサブクラスは、基本クラスからの値から始めて、追加した許されない関数を連結していかなければなりません --
   危険な関数が新しく Python に追加された時は、それらも、このモジュールに追加します。)


.. attribute:: RExec.ok_builtin_modules

   安全にインポートできる組み込みモジュールの名前を格納しています。 :class:`RExec` に対する値は、 ``('audioop', 'array',
   'binascii', 'cmath', 'errno', 'imageop', 'marshal', 'math', 'md5', 'operator',
   'parser', 'regex', 'select', 'sha', '_sre', 'strop', 'struct', 'time')``
   です。この変数をオーバーライドする場合も、同様な注意が適用されます -- 基本クラスからの値を使って始めます。


.. attribute:: RExec.ok_path

   :keyword:`import` が制限された環境で実行される時に検索されるディレクトリーを格納しています。
   :class:`RExec` に対する値は、(モジュールがロードされた時は) 制限されないコードの ``sys.path`` と同一です。


.. attribute:: RExec.ok_posix_names

   制限された環境で実行するプログラムで利用できる、 :mod:`os` モジュール内の関数の名前を格納しています。 :class:`RExec` に対する値は、
   ``('error', 'fstat', 'listdir', 'lstat', 'readlink', 'stat', 'times', 'uname',
   'getpid', 'getppid', 'getcwd', 'getuid', 'getgid', 'geteuid', 'getegid')`` です。

   .. これは ok_os_names と呼ばれるべきでしょうか?


.. attribute:: RExec.ok_sys_names

   制限された環境で実行するプログラムで利用できる、 :mod:`sys` モジュール内の関数名と変数名を格納しています。
   :class:`RExec` に対する値は、 ``('ps1', 'ps2', 'copyright', 'version', 'platform',
   'exit', 'maxint')`` です。


.. attribute:: RExec.ok_file_types

   モジュールがロードすることを許されているファイルタイプを格納しています。各ファイルタイプは、 :mod:`imp` モジュールで定義された整数定数です。
   意味のある値は、 :const:`PY_SOURCE` 、 :const:`PY_COMPILED` および :const:`C_EXTENSION`
   です。 :class:`RExec` に対する値は、 ``(C_EXTENSION, PY_SOURCE)`` です。サブクラスで
   :const:`PY_COMPILED` を追加することは推奨されません; 攻撃者が、バイトコンパイルしたでっちあげのファイル(:file:`.pyc`)を、
   例えば、あなたの公開 FTP サーバの :file:`/tmp` に書いたり、 :file:`/incoming`
   にアップロードしたりして、とにかくあなたのファイルシステム内に置くことで、制限された実行モードから抜け出ることができるかもしれないからです。


例
--

標準の :class:`RExec` クラスよりも、若干、もっと緩めたポリシを望んでいるとしましょう。例えば、もし :file:`/tmp`
内のファイルへの書き込みを喜んで許すならば、 :class:`RExec` クラスを次のようにサブクラス化できます::

   class TmpWriterRExec(rexec.RExec):
       def r_open(self, file, mode='r', buf=-1):
           if mode in ('r', 'rb'):
               pass
           elif mode in ('w', 'wb', 'a', 'ab'):
               # ファイル名をチェックします :  /tmp/ で始まらなければなりません
               if file[:5]!='/tmp/':
                   raise IOError(" /tmp 以外へは書き込みできません")
               elif (string.find(file, '/../') >= 0 or
                    file[:3] == '../' or file[-3:] == '/..'):
                   raise IOError("ファイル名の '..' は禁じられています")
           else: raise IOError("open() モードが正しくありません")
           return open(file, mode, buf)

上のコードは、完全に正しいファイル名でも、時には禁止する場合があることに
注意して下さい; 例えば、制限された環境でのコードでは、 :file:`/tmp/foo/../bar`
というファイルはオープンできないかもしれません。これを修正するには、 :meth:`r_open` メソッドが、そのファイル名を
:file:`/tmp/bar` に単純化しなければなりません。そのためには、ファイル名を分割して、それにさまざまな
操作を行う必要があります。セキュリティが重大な場合には、より複雑で微妙なセキュリティホールを抱え込むかもしれない一般性のあるコードよりも、
制限が余りにあり過ぎるとしても単純なコードを書く方が、望ましいでしょう。
