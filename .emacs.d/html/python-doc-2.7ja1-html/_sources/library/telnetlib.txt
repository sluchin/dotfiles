
:mod:`telnetlib` --- Telnet クライアント
========================================

.. module:: telnetlib
   :synopsis: Telnet クライアントクラス
.. sectionauthor:: Skip Montanaro <skip@pobox.com>


.. index:: single: protocol; Telnet

:mod:`telnetlib` モジュールでは、Telnet プロトコルを実装している :class:`Telnet` クラスを提供します。Telnet
プロトコルについての詳細は :rfc:`854` を参照してください。加えて、このモジュールでは Telnet プロトコルにおける制御文字
(下を参照してください) と、telnet オプションに対するシンボル定数を提供しています。telnet オプションに対するシンボル名は
``arpa/telnet.h`` の ``TELOPT_`` がない状態での定義に従います。伝統的に ``arpa/telnet.h`` に含められて
いない telnet オプションのシンボル名については、このモジュールのソースコード自体を参照してください。

telnet コマンドのシンボル定数は、IAC、DONT、DO、WONT、WILL、SE (サブネゴシエーション終了)、NOP (何もしない)、DM
(データマーク)、 BRK (ブレーク)、IP (プロセス割り込み)、AO (出力中断)、 AYT (応答確認)、EC (文字削除)、EL (行削除)、GA
(進め)、SB ( サブネゴシエーション開始) です。


.. class:: Telnet([host[, port[, timeout]]])

   :class:`Telnet` は Telnet サーバへの接続を表現します。
   デフォルトでは、 :class:`Telnet` クラスのインスタンスは最初はサーバに
   接続していません。接続を確立するには :meth:`open` を使わなければなりません。
   別の方法として、コンストラクタに *host* とオプションの *port* を
   渡すことができます。この場合はコンストラクタの呼び出しが返る以前にサーバへの
   接続が確立されます。
   オプション引数の *timeout* が渡された場合、コネクション接続時のタイムアウト時間を秒数で指定します。
   (指定されなかった場合は、グローバルのデフォルトタイムアウト設定が利用されます。)

   すでに接続の開かれているンスタンスを再度開いてはいけません。

   このクラスは多くの :meth:`read_\*` メソッドを持っています。これらのメソッドのいくつかは、接続の終端を示す文字を読み込んだ場合に
   :exc:`EOFError` を送出するので注意してください。例外を送出するのは、これらの関数が終端に到達しなくても空の文字列を返す可能性
   があるからです。詳しくは下記の個々の説明を参照してください。

   .. versionchanged:: 2.6
      *timeout* が追加されました


.. seealso::

   :rfc:`854` - Telnet プロトコル仕様 (Telnet Protocol Specification)
      Telnet プロトコルの定義。


.. _telnet-objects:

Telnet オブジェクト
-------------------

:class:`Telnet` インスタンスは以下のメソッドを持っています:


.. method:: Telnet.read_until(expected[, timeout])

   *expected* で指定された文字列を読み込むか、 *timeout* で指定された秒数が経過するまで読み込みます。

   与えられた文字列に一致する部分が見つからなかった場合、読み込むことができたもの全てを返します。これは空の文字列になる可能性が
   あります。接続が閉じられ、転送処理済みのデータが得られない場合には :exc:`EOFError` が送出されます。


.. method:: Telnet.read_all()

   EOFに到達するまでの全てのデータを読み込みます; 接続が閉じられるまでブロックします。


.. method:: Telnet.read_some()

   EOF に到達しない限り、少なくとも 1 バイトの転送処理済みデータを読み込みます。EOF に到達した場合は ``''`` を返します。
   すぐに読み出せるデータが存在しない場合にはブロックします。


.. method:: Telnet.read_very_eager()

   I/O によるブロックを起こさずに読み出せる全てのデータを読み込みます (eager モード)。

   接続が閉じられており、転送処理済みのデータとして読み出せるものがない場合には :exc:`EOFError` が送出されます。それ以外の
   場合で、単に読み出せるデータがない場合には ``''`` を返します。 IAC シーケンス操作中でないかぎりブロックしません。


.. method:: Telnet.read_eager()

   現在すぐに読み出せるデータを読み出します。

   接続が閉じられており、転送処理済みのデータとして読み出せるものがない場合には :exc:`EOFError` が送出されます。それ以外の
   場合で、単に読み出せるデータがない場合には ``''`` を返します。 IAC シーケンス操作中でないかぎりブロックしません。


.. method:: Telnet.read_lazy()

   すでにキューに入っているデータを処理して返します (lazy モード)。

   接続が閉じられており、読み出せるデータがない場合には :exc:`EOFError` を送出します。それ以外の場合で、転送処理済みの
   データで読み出せるものがない場合には ``''`` を返します。 IAC シーケンス操作中でないかぎりブロックしません。


.. method:: Telnet.read_very_lazy()

   すでに処理済みキューに入っているデータを処理して返します (very lazy モード)。

   接続が閉じられており、読み出せるデータがない場合には :exc:`EOFError` を送出します。それ以外の場合で、転送処理済みの
   データで読み出せるものがない場合には ``''`` を返します。このメソッドは決してブロックしません。


.. method:: Telnet.read_sb_data()

   SB/SE ペア (サブオプション開始／終了) の間に収集されたデータを返します。 ``SE`` コマンドによって起動されたコールバック関数はこれらのデータ
   にアクセスしなければなりません。

   このメソッドはけっしてブロックしません。

   .. versionadded:: 2.3


.. method:: Telnet.open(host[, port])

   サーバホストに接続します。第二引数はオプションで、ポート番号を指定します。標準の値は通常の Telnet ポート番号 (23) です。
   オプション引数の *timeout* が渡された場合、コネクション接続時などのブロックする操作のタイムアウト時間を秒数で指定します。
   (指定されなかった場合は、グローバルのデフォルトタイムアウト設定が利用されます。)

   すでに接続しているインスタンスで再接続を試みてはいけません。

   .. versionchanged:: 2.6
      *timeout* が追加されました


.. method:: Telnet.msg(msg[, *args])

   デバッグレベルが ``>`` 0 のとき、デバッグ用のメッセージを出力します。追加の引数が存在する場合、標準の文字列書式化演算子 ``%`` を使って
   *msg* 中の書式指定子に代入されます。


.. method:: Telnet.set_debuglevel(debuglevel)

   デバッグレベルを設定します。 *debuglevel* が大きくなるほど、 (``sys.stdout`` に) デバッグメッセージがたくさん出力されます。


.. method:: Telnet.close()

   接続を閉じます。


.. method:: Telnet.get_socket()

   内部的に使われているソケットオブジェクトです。


.. method:: Telnet.fileno()

   内部的に使われているソケットオブジェクトのファイル記述子です。


.. method:: Telnet.write(buffer)

   ソケットに文字列を書き込みます。このとき IAC 文字については  2 度送信します。接続がブロックした場合、書き込みがブロックする
   可能性があります。接続が閉じられた場合、 :exc:`socket.error`  が送出されるかもしれません。


.. method:: Telnet.interact()

   非常に低機能の telnet クライアントをエミュレートする対話関数です。


.. method:: Telnet.mt_interact()

   :meth:`interact` のマルチスレッド版です。


.. method:: Telnet.expect(list[, timeout])

   正規表現のリストのうちどれか一つにマッチするまでデータを読みます。

   第一引数は正規表現のリストです。コンパイルされたもの  (:class:`re.RegexObject` のインスタンス) でも、コンパイルされていないもの
   (文字列) でもかまいません。オプションの第二引数はタイムアウトで、単位は秒です; 標準の値は無期限に設定されています。

   3 つの要素からなるタプル: 最初にマッチした正規表現のインデクス; 返されたマッチオブジェクト;
   マッチ部分を含む、マッチするまでに読み込まれたテキストデータ、を返します。

   ファイル終了子が見つかり、かつ何もテキストデータが読み込まれなかった場合、 :exc:`EOFError` が送出されます。そうでない
   場合で何もマッチしなかった場合には ``(-1, None, text)`` が返されます。ここで *text* はこれまで受信したテキストデータです
   (タイムアウトが発生した場合には空の文字列になる場合もあります)。

   正規表現の末尾が (``.*`` のような) 貪欲マッチングになっている場合や、入力に対して
   1 つ以上の正規表現がマッチする場合には、その結果は決定不能で、I/O
   のタイミングに依存するでしょう。


.. method:: Telnet.set_option_negotiation_callback(callback)

   telnet オプションが入力フローから読み込まれるたびに、 *callback* が (設定されていれば) 以下の引数形式: callback(telnet
   socket, command (DO/DONT/WILL/WONT), option) で呼び出されます。その後 telnet オプションに対しては
   telnetlib  は何も行いません。


.. _telnet-example:

Telnet Example
--------------

.. sectionauthor:: Peter Funk <pf@artcom-gmbh.de>


典型的な使い方を表す単純な例を示します::

   import getpass
   import sys
   import telnetlib

   HOST = "localhost"
   user = raw_input("Enter your remote account: ")
   password = getpass.getpass()

   tn = telnetlib.Telnet(HOST)

   tn.read_until("login: ")
   tn.write(user + "\n")
   if password:
       tn.read_until("Password: ")
       tn.write(password + "\n")

   tn.write("ls\n")
   tn.write("exit\n")

   print tn.read_all()

