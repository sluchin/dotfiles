
:mod:`aifc` --- AIFFおよびAIFCファイルの読み書き
================================================

.. module:: aifc
   :synopsis: AIFFあるいはAIFCフォーマットのオーディオファイルの読み書き


このモジュールはAIFFとAIFF-Cファイルの読み書きをサポートします。 AIFF（Audio Interchange File
Format）はデジタルオーディオサンプルをファイルに保存するためのフォーマットです。
AIFF-CはAIFFの新しいバージョンで、オーディオデータの圧縮に対応しています。

.. index::
   single: Audio Interchange File Format
   single: AIFF
   single: AIFF-C

.. note::

   操作のいくつかはIRIX上でのみ動作します；そういう操作ではIRIXでのみ利用できる :mod:`cl` モジュールをインポート
   しようとして、 :exc:`ImportError` を発生します。

オーディオファイルには、オーディオデータについて記述したパラメータがたくさん含まれています。
サンプリングレートあるいはフレームレートは、1秒あたりのオーディオサンプル数です。チャンネル数は、モノラル、ステレオ、4チャンネルかどうかを示します。
フレームはそれぞれ、チャンネルごとに一つのサンプルからなります。サンプルサイズは、一つのサンプルの大きさをバイト数で示したものです。
したがって、一つのフレームは *nchannels* \* *samplesize* バイト
からなり、1秒間では *nchannels* \* *samplesize* \* *framerate* バイトで構成されます。

例えば、CD品質のオーディオは2バイト（16ビット）のサンプルサイズを持っていて、2チャンネル（ステレオ）であり、44,100フレーム／秒のフレーム
レートを持っています。そのため、フレームサイズは4バイト（2\*2）で、 1秒間では2\*2\*44100バイト（176,400バイト）になります。

:mod:`aifc` モジュールは以下の関数を定義しています：


.. function:: open(file[, mode])

   AIFFあるいはAIFF-Cファイルを開き、後述するメソッドを持つインスタンスを返します。
   引数 *file* はファイルを示す文字列か、ファイルオブジェクトのいずれかです。
   *mode* は、読み込み用に開くときには ``'r'`` か ``'rb'`` のどちらか
   で、書き込み用に開くときには ``'w'`` か ``'wb'`` のどちらかでなければなりません。
   もし省略されたら、 ``file.mode`` が存在すればそれが使用され、なければ ``'rb'`` が使われます。
   書き込み用にこのメソッドを使用するときには、これから全部でどれだけのサンプル数を書き込むのか分からなかったり、 :meth:`writeframesraw` と
   :meth:`setnframes` を使わないなら、ファイルオブジェクトはシーク可能でなければなりません。

ファイルが :func:`.open` によって読み込み用に開かれたときに返されるオブジェクトには、以下のメソッドがあります：


.. method:: aifc.getnchannels()

   オーディオチャンネル数（モノラルなら1、ステレオなら2）を返します。


.. method:: aifc.getsampwidth()

   サンプルサイズをバイト数で返します。


.. method:: aifc.getframerate()

   サンプリングレート（1秒あたりのオーディオフレーム数）を返します。


.. method:: aifc.getnframes()

   ファイルの中のオーディオフレーム数を返します。


.. method:: aifc.getcomptype()

   オーディオファイルで使用されている圧縮形式を示す4文字の文字列を返します。AIFFファイルでは ``'NONE'`` が返されます。


.. method:: aifc.getcompname()

   オーディオファイルの圧縮形式を人に判読可能な形にしたものを返します。 AIFFファイルでは ``'not compressed'`` が返されます。


.. method:: aifc.getparams()

   以上の全ての値を上の順に並べたタプルを返します。


.. method:: aifc.getmarkers()

   オーディオファイルのマーカーのリストを返します。一つのマーカーは三つの要素のタプルです。
   要素の1番目はマークID（整数）、2番目はマーク位置のフレーム数をデータの始めから数えた値（整数）、3番目はマークの名称（文字列）です。


.. method:: aifc.getmark(id)

   与えられた *id* のマークの要素を :meth:`getmarkers` で述べたタプルで返します。


.. method:: aifc.readframes(nframes)

   オーディオファイルの次の *nframes* 個のフレームを読み込んで返します。返されるデータは、全チャンネルの圧縮されていないサンプルをフレームごとに
   文字列にしたものです。


.. method:: aifc.rewind()

   読み込むポインタをデータの始めに巻き戻します。次に :meth:`readframes` を使用すると、データの始めから読み込みます。


.. method:: aifc.setpos(pos)

   指定したフレーム数の位置にポインタを設定します。


.. method:: aifc.tell()

   現在のポインタのフレーム位置を返します。


.. method:: aifc.close()

   AIFFファイルを閉じます。このメソッドを呼び出したあとでは、オブジェクトはもう使用できません。

ファイルが :func:`.open` によって書き込み用に開かれたときに返されるオ
ブジェクトには、 :meth:`readframes` と :meth:`setpos` を除く上述の全てのメソッドがあります。
さらに以下のメソッドが定義されています。 :meth:`get\*` メソッドは、対応する :meth:`set\*` を呼び出したあとでのみ呼び出し可能です。
最初に :meth:`writeframes` あるいは :meth:`writeframesraw` を呼び出す
前に、フレーム数を除く全てのパラメータが設定されていなければなりません。


.. method:: aifc.aiff()

   AIFFファイルを作ります。デフォルトではAIFF-Cファイルが作られますが、ファイル名が ``'.aiff'`` で
   終わっていればAIFFファイルが作られます。


.. method:: aifc.aifc()

   AIFF-Cファイルを作ります。デフォルトではAIFF-Cファイルが作られますが、ファイル名が ``'.aiff'`` で
   終わっていればAIFFファイルが作られます。


.. method:: aifc.setnchannels(nchannels)

   オーディオファイルのチャンネル数を設定します。


.. method:: aifc.setsampwidth(width)

   オーディオのサンプルサイズをバイト数で設定します。


.. method:: aifc.setframerate(rate)

   サンプリングレートを1秒あたりのフレーム数で設定します。


.. method:: aifc.setnframes(nframes)

   オーディオファイルに書き込まれるフレーム数を設定します。もしこのパラメータが設定されていなかったり正しくなかったら、ファイルは
   シークに対応していなければなりません。


.. method:: aifc.setcomptype(type, name)

   .. index::
      single: u-LAW
      single: A-LAW
      single: G.722

   圧縮形式を設定します。もし設定しなければ、オーディオデータは圧縮されません。 AIFFファイルは圧縮できません。
   変数nameは圧縮形式を人に判読可能にしたもので、変数typeは4文字の文字列でなければなりません。現在のところ、以下の圧縮形式がサポートされています：
   NONE, ULAW, ALAW, G722。


.. method:: aifc.setparams(nchannels, sampwidth, framerate, com ptype, compname)

   上の全パラメータを一度に設定します。引数はそれぞれのパラメータからなるタプルです。
   つまり、 :meth:`setparams` の引数として、 :meth:`getparams` を呼び出した結果を使うことができます。


.. method:: aifc.setmark(id, pos, name)

   指定したID（1以上）、位置、名称でマークを加えます。このメソッドは、 :meth:`close` の前ならいつでも呼び出すことができます。


.. method:: aifc.tell()

   出力ファイルの現在の書き込み位置を返します。 :meth:`setmark` との組み合わせで使うと便利です。


.. method:: aifc.writeframes(data)

   出力ファイルにデータを書き込みます。このメソッドは、オーディオファイルのパラメータを設定したあとでのみ呼び出し可能です。


.. method:: aifc.writeframesraw(data)

   オーディオファイルのヘッダ情報が更新されないことを除いて、 :meth:`writeframes` と同じです。


.. method:: aifc.close()

   AIFFファイルを閉じます。ファイルのヘッダ情報は、オーディオデータの実際のサイズを反映して更新されます。
   このメソッドを呼び出したあとでは、オブジェクトはもう使用できません。

