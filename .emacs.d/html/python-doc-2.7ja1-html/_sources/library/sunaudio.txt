
:mod:`sunaudiodev` --- Sun オーディオハードウェアへのアクセス
=============================================================

.. module:: sunaudiodev
   :platform: SunOS
   :synopsis: Sun オーディオハードウェアへのアクセス
   :deprecated:

.. deprecated:: 2.6
   :mod:`sunaudiodev` モジュールは Python 3.0 での削除に向け非推奨になりました。



.. index:: single: u-LAW

このモジュールを使うと、Sun のオーディオインターフェースにアクセスできます。
Sun オーディオハードウェアは、1秒あたり8kのサンプリングレート、\
u-LAW フォーマットでオーディオデータを録音、再生できます。
完全な説明文書はマニュアルページ :manpage:`audio(7I)` にあります。

.. index:: module: SUNAUDIODEV

モジュール :mod:`SUNAUDIODEV` には、このモジュールで使われる定数が定義されています。

このモジュールには、以下の変数と関数が定義されています：


.. exception:: error

   この例外は、全てのエラーについて発生します。引数は誤りを説明する文字列です。


.. function:: open(mode)

   この関数はオーディオデバイスを開き、Sun オーディオデバイスのオブジェクト\
   を返します。こうすることで、オブジェクトが I/O に使用できるようになります。
   パラメータ *mode* は次のうちのいずれか一つで、録音のみには ``'r'`` 、
   再生のみには ``'w'`` 、録音と再生両方には ``'rw'`` 、
   コントロールデバイスへのアクセスには ``'control'`` です。
   レコーダーやプレーヤーには同時に１つのプロセスしかアクセスが許されていな\
   いので、必要な動作についてだけデバイスをオープンするのがいい考えです。
   詳しくは :manpage:`audio(7I)` を参照してください。
   マニュアルページにあるように、このモジュールは環境変数
   ``AUDIODEV`` の中のベースオーディオデバイスファイルネームを初めに参照\
   します。見つからない場合は :file:`/dev/audio` を参照します。
   コントロールデバイスについては、ベースオーディオデバイスに"ctl"を\
   加えて扱われます。


.. _audio-device-objects:

オーディオデバイスオブジェクト
------------------------------

オーディオデバイスオブジェクトは :func:`.open` で返され、このオブジェ\
クトには以下のメソッドが定義されています
(``control`` オブジェクトは除きます。これには :meth:`getinfo` 、
:meth:`setinfo` 、 :meth:`fileno` 、 :meth:`drain` だけが定義されています）：


.. method:: audio device.close()

   このメソッドはデバイスを明示的に閉じます。
   オブジェクトを削除しても、それを参照しているものがあって、すぐに閉じてく\
   れない場合に便利です。
   閉じられたデバイスを使うことはできません。


.. method:: audio device.fileno()

   デバイスに関連づけられたファイルディスクリプタを返します。
   これは、後述の ``SIGPOLL`` の通知を組み立てるのに使われます。


.. method:: audio device.drain()

   このメソッドは全ての出力中のプロセスが終了するまで待って、それから制御が\
   戻ります。このメソッドの呼び出しはそう必要ではありません：
   オブジェクトを削除すると自動的にオーディオデバイスを閉じて、暗黙のうちに\
   吐き出します。


.. method:: audio device.flush()

   このメソッドは全ての出力中のものを捨て去ります。
   ユーザの停止命令に対する反応の遅れ（1秒までの音声のバッファリングによっ\
   て起こります）を避けるのに使われます。


.. method:: audio device.getinfo()

   このメソッドは入出力のボリューム値などの情報を引き出して、オーディオス\
   テータスのオブジェクト形式で返します。
   このオブジェクトには何もメソッドはありませんが、現在のデバイスの状態を示\
   す多くの属性が含まれます。
   属性の名称と意味は ``<sun/audioio.h>`` と :manpage:`audio(7I)` に記載があ\
   ります。
   メンバー名は相当する C のものとは少し違っています：
   ステータスオブジェクトは１つの構造体です。
   その中の構造体である :c:data:`play` のメンバーには名前の初めに ``o_`` がつ\
   いていて、 :c:data:`record` には ``i_`` がついています。
   そのため、C のメンバーである :c:data:`play.sample_rate` は
   :attr:`o_sample_rate` として、 :c:data:`record.gain` は :attr:`i_gain`
   として参照され、
   :c:data:`monitor_gain` はそのまま :attr:`monitor_gain` で参照されます。


.. method:: audio device.ibufcount()

   このメソッドは録音側でバッファリングされるサンプル数を返します。
   つまり、プログラムは同じ大きさのサンプルに対する :func:`read` の\
   呼び出しをブロックしません。


.. method:: audio device.obufcount()

   このメソッドは再生側でバッファリングされるサンプル数を返します。
   残念ながら、この数値はブロックなしに書き込めるサンプル数を調べるのには\
   使えません。というのは、カーネルの出力キューの長さは可変だからです。


.. method:: audio device.read(size)

   このメソッドはオーディオ入力から *size* のサイズのサンプルを読み込ん\
   で、Pythonの文字列として返します。
   この関数は必要なデータが得られるまで他の操作をブロックします。


.. method:: audio device.setinfo(status)

   このメソッドはオーディオデバイスのステータスパラメータを設定します。
   パラメータ *status* は :func:`getinfo` で返されたり、
   プログラムで変更されたオーディオステータスオブジェクトです。


.. method:: audio device.write(samples)

   パラメータとしてオーディオサンプルをPython文字列を受け取り、再生します。
   もし十分なバッファの空きがあればすぐに制御が戻り、そうでないならブロック\
   されます。

オーディオデバイスは SIGPOLL を介して様々なイベントの非同期通知に対応して\
います。 Python でこれをどのようにしたらできるか、例を挙げます： ::

   def handle_sigpoll(signum, frame):
       print 'I got a SIGPOLL update'

   import fcntl, signal, STROPTS

   signal.signal(signal.SIGPOLL, handle_sigpoll)
   fcntl.ioctl(audio_obj.fileno(), STROPTS.I_SETSIG, STROPTS.S_MSG)


:mod:`SUNAUDIODEV` --- :mod:`sunaudiodev` で使われる定数
========================================================

.. module:: SUNAUDIODEV
   :platform: SunOS
   :synopsis: sunaudiodevで使われる定数。
   :deprecated:

.. deprecated:: 2.6
   :mod:`SUNAUDIODEV` モジュールは Python 3.0 での削除に向け非推奨になりました。


.. index:: module: sunaudiodev

これは :mod:`sunaudiodev` に付随するモジュールで、
:const:`MIN_GAIN` 、 :const:`MAX_GAIN` 、
:const:`SPEAKER` などの便利なシンボル定数を定義しています。
定数の名前は C の include ファイル ``<sun/audioio.h>`` のものと同じで、
初めの文字列 ``AUDIO_`` を除いたものです。

