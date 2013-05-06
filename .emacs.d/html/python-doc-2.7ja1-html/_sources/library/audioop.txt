
:mod:`audioop` --- 生の音声データを操作する
===========================================

.. module:: audioop
   :synopsis: 生の音声データを操作する


:mod:`audioop` モジュールは音声データを操作する関数を収録しています。このモジュールは、Python 文字列型中に入っている 8,  16,
32 ビットの符号付き整数でできた音声データ、すなわち :mod:`al` および :mod:`sunaudiodev`
で使われているのと同じ形式の音声データを操作します。特に指定の無いかぎり、スカラ量を表す要素はすべて整数型になっています。

.. index::
   single: Intel/DVI ADPCM
   single: ADPCM, Intel/DVI
   single: u-LAW

このモジュールはa-LAW、u-LAWそしてIntel/DVI ADPCMエンコードをサポートしています。

.. This para is mostly here to provide an excuse for the index entries...

複雑な操作のうちいくつかはサンプル幅が 16 ビットのデータに対してのみ働きますが、それ以外は常にサンプル幅を操作のパラメタとして (バイト単位で)
渡します。

このモジュールでは以下の変数と関数を定義しています：


.. exception:: error

   この例外は、未知のサンプル当たりのバイト数を指定した時など、全般的なエラーに対して送出されます。


.. function:: add(fragment1, fragment2, width)

   パラメタに渡した 2 つのデータを加算した結果を返します。 *width* はサンプル幅をバイトで表したもので、
   ``1`` 、 ``2`` 、 ``4`` のうちいずれかです。 2 つのデータは同じサンプル幅でなければなりません。


.. function:: adpcm2lin(adpcmfragment, width, state)

   Intel/DVI ADPCM 形式のデータをリニア (linear) 形式にデコードします。 ADPCM
   符号化方式の詳細については :func:`lin2adpcm` の説明を参照して下さい。 ``(sample, newstate)`` からなる
   タプルを返し、サンプルは *width* に指定した幅になります。


.. function:: alaw2lin(fragment, width)

   a-LAW形式のデータをリニア (linear) 形式に変換します。 a-LAW形式は常に 8 ビットのサンプルを使用するので、ここでは *width*
   は単に出力データのサンプル幅となります。

   .. versionadded:: 2.5


.. function:: avg(fragment, width)

   データ中の全サンプルの平均値を返します。


.. function:: avgpp(fragment, width)

   データ中の全サンプルの平均 peak-peak 振幅を返します。フィルタリングを行っていない場合、このルーチンの有用性は疑問です。


.. function:: bias(fragment, width, bias)

   元データの各サンプルにバイアスを加えたデータを返します。


.. function:: cross(fragment, width)

   引数に渡したデータ中のゼロ交差回数を返します。


.. function:: findfactor(fragment, reference)

   ``rms(add(fragment, mul(reference, -F)))`` を最小にするような係数 *F* 、すなわち、 *reference*
   に乗算したときにもっとも *fragment* に近くなるような値を返します。 *fragment* と *reference* のサンプル幅はいずれも 2バイト
   でなければなりません。

   このルーチンの実行に要する時間は ``len(fragment)`` に比例します。


.. function:: findfit(fragment, reference)

   *reference* を可能な限り *fragment* に一致させようとします (*fragment* は *reference*
   より長くなければなりません)。この処理は (概念的には) *fragment* からスライスをいくつか取り出し、
   それぞれについて :func:`findfactor` を使って最良な一致を計算し、誤差が最小の結果を選ぶことで実現します。
   *fragment* と *reference* のサンプル幅は両方とも2バイトでなければなりません。 ``(offset, factor)``
   からなるタプルを返します。 *offset* は最適な一致箇所が始まる *fragment* のオフセット値（整
   数）で、 *factor* は :func:`findfactor` の返す係数 (浮動小数点数) です。


.. function:: findmax(fragment, length)

   *fragment* から、長さが *length* サンプル (バイトではありません!) で最大のエネルギーを持つスライス、
   すなわち、 ``rms(fragment[i *2:(i+length)* 2])`` を最大にするようなスライスを探し、 *i* を返します。
   データのはサンプル幅は 2バイトでなければなりません。

   このルーチンの実行に要する時間は ``len(fragment)`` に比例します。


.. function:: getsample(fragment, width, index)

   データ中の *index* サンプル目の値を返します。


.. function:: lin2adpcm(fragment, width, state)

   データを 4 ビットの Intel/DVI ADPCM 符号化方式に変換します。 ADPCM 符号化方式とは適応符号化方式の一つで、あるサンプルと (可変の)
   ステップだけ離れたその次のサンプルとの差を 4 ビットの整数で表現する方式です。 Intel/DVI ADPCMアルゴリズムは IMA  (国際MIDI協会)
   に採用されているので、おそらく標準になるはずです。

   *state* はエンコーダの内部状態が入ったタプルです。エンコーダは ``(adpcmfrag, newstate)`` のタプルを返し、次に
   :func:`lin2adpcm` を呼び出す時に *newstate* を渡さねばなりません。最初に呼び出す時には *state* に ``None`` を渡しても
   かまいません。 *adpcmfrag* は ADPCMで符号化されたデータで、バイト当たり 2 つの4ビット値がパックされています。


.. function:: lin2alaw(fragment, width)

   音声データの各サンプルを a-LAW 符号でエンコードし、Python文字列として返します。a-LAW とは音声符号化方式の一つで、約 13 ビットに相当する
   ダイナミックレンジをわずか 8 ビットで実現できます。 Sun やその他の音声ハードウェアで使われています。

   .. versionadded:: 2.5


.. function:: lin2lin(fragment, width, newwidth)

   サンプル幅を 1、2、4 バイト形式の間で変換します。

   .. note::

      .. In some audio formats, such as .WAV files, 16 and 32 bit samples are
         signed, but 8 bit samples are unsigned.  So when converting to 8 bit wide
         samples for these formats, you need to also add 128 to the result::

      .WAV のような幾つかのオーディオフォーマットでは、16bitと32bitのサンプルは符号付きですが、
      8bitのサンプルは符号なしです。
      そのため、そのようなフォーマットで8bitに変換する場合は、変換結果に128を足さなければなりません。

         new_frames = audioop.lin2lin(frames, old_width, 1)
         new_frames = audioop.bias(new_frames, 1, 128)

      .. The same, in reverse, has to be applied when converting from 8 to 16 or 32
         bit width samples.

      逆に、8bitから16bitや32bitに変換する場合も、同じことが言えます。



.. function:: lin2ulaw(fragment, width)

   音声データの各サンプルを u-LAW 符号でエンコードし、Python文字列として返します。 u-LAW とは音声符号化方式の一つで、約 14
   ビットに相当するダイナミックレンジをわずか 8 ビットで実現できます。 Sun やその他の音声ハードウェアで使われています。


.. function:: minmax(fragment, width)

   音声データ全サンプル中における最小値と最大値からなるタプルを返します。


.. function:: max(fragment, width)

   音声データ全サンプルの *絶対値* の最大値を返します。


.. function:: maxpp(fragment, width)

   音声データの最大 peak-peak 振幅を返します。


.. function:: mul(fragment, width, factor)

   元のデータの全サンプルに浮動小数点数 *factor* を掛けたデータを返します。オーバフローが起きても例外を送出せず無視します。


.. function:: ratecv(fragment, width, nchannels, inrate, outrate, state[, weightA[, weightB]])

   入力したデータのフレームレートを変換します。

   *state* は変換ルーチンの内部状態を入れたタプルです。変換ルーチンは ``(newfragment, newstate)``
   を返し、次に :func:`ratecv` を呼び出す時には *newstate* を渡さなねばなりません。最初の呼び出しでは ``None`` を渡します。

   引数 *weightA* と *weightB* は単純なデジタルフィルタのパラメタで、デフォルト値はそれぞれ ``1`` と ``0`` です。


.. function:: reverse(fragment, width)

   データ内のサンプルの順序を逆転し、変更されたデータを返します。


.. function:: rms(fragment, width)

   データの自乗平均根(root-mean-square)、すなわち ``sqrt(sum(S_i^2)/n``
   を返します。これはオーディオ信号の強度 (power) を測る一つの目安です。


.. function:: tomono(fragment, width, lfactor, rfactor)

   ステレオ音声データをモノラル音声データに変換します。左チャネルのデータに *lfactor* 、右チャネルのデータに *rfactor*
   を掛けた後、二つのチャネルの値を加算して単一チャネルの信号を生成します。


.. function:: tostereo(fragment, width, lfactor, rfactor)

   モノラル音声データをステレオ音声データに変換します。ステレオ音声データの各サンプル対は、モノラル音声データの各サンプルをそれぞれ左チャネルは
   *lfactor* 倍、右チャネルは *rfactor* 倍して生成します。


.. function:: ulaw2lin(fragment, width)

   u-LAW で符号化されている音声データを線形に符号化された音声データに変換します。 u-LAW 符号化は常にサンプル当たり 8 ビットを使うため、
   *width* は出力音声データのサンプル幅にしか使われません。

:func:`.mul` や :func:`.max` といった操作はモノラルとステレオを区別しない、すなわち全てのデータを平等に扱うという
ことに注意してください。この仕様が問題になるようなら、あらかじめステレオ音声データを二つのモノラル音声データに分割しておき、
操作後に再度統合してください。そのような例を以下に示します::

   def mul_stereo(sample, width, lfactor, rfactor):
       lsample = audioop.tomono(sample, width, 1, 0)
       rsample = audioop.tomono(sample, width, 0, 1)
       lsample = audioop.mul(lsample, width, lfactor)
       rsample = audioop.mul(rsample, width, rfactor)
       lsample = audioop.tostereo(lsample, width, 1, 0)
       rsample = audioop.tostereo(rsample, width, 0, 1)
       return audioop.add(lsample, rsample, width)

ADPCM エンコーダを使って音声データの入ったネットワークパケットを構築する際、自分のプロトコルを (パケットロスに耐えられるように) ステートレス
(stateless) にしたいなら、データだけでなく状態変数 (state) も伝送せねばなりません。このとき、伝送するのはエンコード後状態
(エンコーダの返す値) ではなく、エンコーダの初期状態  (:func:`lin2adpcm` に渡した値) *initial* なので注意してください。
:func:`struct.struct` を使って状態変数をバイナリ形式で保存したいなら、最初の要素  (予測値) は 16 ビットで、次の値 (デルタ係数:
delta index) は 8 ビットで符号化できます。

このモジュールの ADPCM 符号のテストは自分自身に対してのみ行っており、他の ADPCM 符号との間では行っていません。作者が仕様を誤解している
部分もあるかもしれず、それぞれの標準との間で相互運用できない場合もあり得ます。

:func:`find\*` ルーチンは一見滑稽に見えるかもしれません。これらの関数の主な目的はエコー除去 (echo cancellation)
にあります。エコー除去を十分高速に行うには、出力サンプル中から最も大きなエネルギーを持った部分を取り出し、この部分が入力サンプル中の
どこにあるかを調べ、入力サンプルから出力サンプル自体を減算します::

   def echocancel(outputdata, inputdata):
       pos = audioop.findmax(outputdata, 800)    # 1/10秒
       out_test = outputdata[pos*2:]
       in_test = inputdata[pos*2:]
       ipos, factor = audioop.findfit(in_test, out_test)
       # Optional (for better cancellation):
       # factor = audioop.findfactor(in_test[ipos*2:ipos*2+len(out_test)],
       #              out_test)
       prefill = '\0'*(pos+ipos)* 2
       postfill = '\0'*(len(inputdata)-len(prefill)-len(outputdata))
       outputdata = prefill + audioop.mul(outputdata,2,-factor) + postfill
       return audioop.add(inputdata, outputdata, 2)

