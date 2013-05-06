
:mod:`termios` --- POSIX スタイルの端末制御
===========================================

.. module:: termios
   :platform: Unix
   :synopsis: POSIX スタイルの端末制御。


.. index::
   pair: POSIX; I/O control
   pair: tty; I/O control

このモジュールでは端末 I/O 制御のための POSIX 準拠の関数呼び出しインタフェースを提供します。
これら呼び出しのための完全な記述については、
POSIX または Unix マニュアルページを参照してください。
POSIX *termios* 形式の端末制御をサポートする Unix のバージョンで
(かつインストール時に指定した場合に) のみ利用可能です。

このモジュールの関数は全て、ファイル記述子 *fd* を最初の引数としてとります。
この値は、 ``sys.stdin.fileno()`` が返すような
整数のファイル記述子でも、 ``sys.stdin`` 自体のようなファイルオブジェクトでもかまいません。

このモジュールではまた、モジュールで提供されている関数を使う上で必要となる全ての定数を定義しています;
これらの定数は C の対応する関数と同じ名前を持っています。
これらの端末制御インタフェースを利用する上でのさらなる情報については、あなたのシステムの
ドキュメンテーションを参考にしてください。

このモジュールでは以下の関数を定義しています:


.. function:: tcgetattr(fd)

   ファイル記述子 *fd* の端末属性を含むリストを返します。
   その形式は: ``[iflag, oflag, cflag, lflag, ispeed, ospeed, cc]`` です。
   *cc* は端末特殊文字のリストです (それぞれ長さ 1 の文字列です。ただしインデクス
   :const:`VMIN` および  :const:`VTIME` の内容は、それらのフィールドが定義され\
   ていた場合整数の値となります)。
   端末設定フラグおよび端末速度の解釈、および配列 *cc* のインデクス検索は、
   :mod:`termios` で定義されているシンボル定数を使って行わなければなりません。


.. function:: tcsetattr(fd, when, attributes)

   ファイル記述子 *fd* の端末属性を *attributes* から取り出して設定します。
   *attributes* は :func:`tcgetattr`
   が返すようなリストです。引数 *when* は属性がいつ変更されるかを決定します:
   :const:`TCSANOW` は即時変更を行い、 :const:`TCSAFLUSH` は現在キュー\
   されている出力を全て転送し、全てのキューされている入力を無視した後に変更を行います。


.. function:: tcsendbreak(fd, duration)

   ファイル記述子 *fd* にブレークを送信します。
   *duration* をゼロにすると、 0.25--0.5 秒間のブレークを送信します;
   *duration* の値がゼロでない場合、その意味はシステム依存です。


.. function:: tcdrain(fd)

   ファイル記述子 *fd* に書き込まれた全ての出力が転送されるまで待ちます。


.. function:: tcflush(fd, queue)

   ファイル記述子 *fd* にキューされたデータを無視します。
   どのキューかは *queue* セレクタで指定します: :const:`TCIFLUSH`
   は入力キュー、 :const:`TCOFLUSH` は出力キュー、 :const:`TCIOFLUSH`
   は両方のキューです。


.. function:: tcflow(fd, action)

   ファイル記述子 *fd* の入力または出力をサスペンドしたりレジュームしたりします。
   引数 *action* は出力をサスペンドする
   :const:`TCOOFF` 、出力をレジュームする :const:`TCOON` 、入力をサスペンドする
   :const:`TCIOFF` 、入力をレジュームする :const:`TCION` をとることができます。


.. seealso::

   :mod:`tty` モジュール
      一般的な端末制御操作のための便利な関数。


使用例
------

.. _termios_example:

以下はエコーバックを切った状態でパスワード入力を促す関数です。
ユーザの入力に関わらず以前の端末属性を正確に回復するために、二つの
:func:`tcgetattr` と :keyword:`try` ... :keyword:`finally`
文によるテクニックが使われています::

   def getpass(prompt="Password: "):
       import termios, sys
       fd = sys.stdin.fileno()
       old = termios.tcgetattr(fd)
       new = termios.tcgetattr(fd)
       new[3] = new[3] & ~termios.ECHO          # lflags
       try:
           termios.tcsetattr(fd, termios.TCSADRAIN, new)
           passwd = raw_input(prompt)
       finally:
           termios.tcsetattr(fd, termios.TCSADRAIN, old)
       return passwd

