
:mod:`gl` --- *Graphics Library* インターフェース
=================================================

.. module:: gl
   :platform: IRIX
   :synopsis: Silicon Graphics のGraphics Library の関数。
   :deprecated:


.. deprecated:: 2.6
    :mod:`gl` モジュールは Python 3.0 での削除に向けて非推奨になりました。


このモジュールは Silicon Graphics の *Graphics Library* へのアクセスを提供します。
Silicon Graphics マシン上だけで利用可能です。

.. warning::

   GL ライブラリの不適切な呼び出しによっては、Python インタープリタがコアを\
   吐き出すことがあります。
   特に、GL のほとんどの関数では最初のウィンドウを開く前に呼び出すのは安全で\
   はありません。

このモジュールはとても大きいので、ここに全てを記述することはできません\
が、以下の説明で出発点としては十分でしょう。
C の関数のパラメータは、以下のような決まりに従って Python に翻訳されます：

* 全て（short、long、unsigned）の整数値（int）は Python の整数に相当します。

* 全ての浮動小数点数と倍精度浮動小数点数は Python の浮動小数点数に相当します。
  たいていの場合、Python の整数も使えます。

* 全ての配列は Python の一次元のリストに相当します。
  たいていの場合、タプルも使えます。

* 全ての文字列と文字の引数は、Python の文字列に相当します。
  例えば、 ``winopen('Hi There!')`` と ``rotate(900, 'z')`` 。

* 配列である引数の長さを特定するためだけに使われる全て
  （short、long、unsigned）の整数値の引数あるいは返り値は、無視されます。
  例えば、Cの呼び出しで、 ::

     lmdef(deftype, index, np, props)

  これは Python では、こうなります。 ::

     lmdef(deftype, index, props)

* 出力のための引数は、引数のリストから省略されています；
  代わりにこれらは関数の返り値として渡されます。
  もし１つ以上の値が返されるのなら、返り値はタプルです。
  もし C の関数が通常の返り値（先のルールによって省略されません）と、出力の\
  ための引数の両方を取るなら、返り値はタプルの最初に来ます。
  例：Cの呼び出しで、 ::

     getmcolor(i, &red, &green, &blue)

  これは Python ではこうなります。 ::

     red, green, blue = getmcolor(i)

以下の関数は一般的でないか、引数に特別な決まりを持っています：


.. function:: varray(argument)

   ``v3d()`` の呼び出しに相当しますが、それよりも速いです。
   *argument* は座標のリスト（あるいはタプル）です。
   各座標は ``(x, y, z)`` あるいは ``(x, y)`` のタプルでなければなりません。
   座標は２次元あるいは３次元が可能ですが、全て同次元でなければなりません。
   ですが、浮動小数点数と整数を混合して使えます。
   座標は（マニュアルページにあるように）必要であれば ``z = 0.0`` と
   仮定して、常に３次元の精密な座標に変換され、各座標について ``v3d()`` が呼び出されます。

   .. XXX the argument-argument added


.. function:: nvarray()

   ``n3f`` と ``v3f`` の呼び出しに相当しますが、それらよりも速いです。
   引数は法線と座標とのペアからなるシーケンス（リストあるいはタプル）です。
   各ペアは座標と、その座標からの法線とのタプルです。各座標と各法線は
   ``(x, y, z)`` からなるタプルでなければなりません。
   ３つの座標が渡されなければなりません。浮動小数点数と整数を混合して使えます。
   各ペアについて、法線に対して ``n3f()`` が呼び出され、座標に対して
   ``v3f()`` が呼び出されます。


.. function:: vnarray()

   ``nvarray()`` と似ていますが、各ペアは始めに座標を、２番目に法線を持っています。


.. function:: nurbssurface(s_k, t_k, ctl, s_ord, t_ord, type)

   nurbs（非均一有理Bスプライン）曲面を定義します。
   ``ctl[][]`` の次元は以下のように計算されます： ``[len(s_k) - s_ord]`` 、
   ``[len(t_k) - t_ord]`` 。

   .. XXX s_k[], t_k[], ctl[][]


.. function:: nurbscurve(knots, ctlpoints, order, type)

   nurbs（非均一有理Bスプライン）曲線を定義します。
   ctlpointsの長さは、 ``len(knots) - order`` です。


.. function:: pwlcurve(points, type)

   区分線形曲線（piecewise-linear curve）を定義します。
   *points* は座標のリストです。
   *type* は ``N_ST`` でなければなりません。


.. function:: pick(n)
              select(n)

   これらの関数はただ一つの引数を取り、pick/select に使うバッファのサイズを設定します。


.. function:: endpick()
              endselect()

   これらの関数は引数を取りません。 pick/select に使われているバッファの大きさを\
   示す整数のリストを返します。バッファがあふれているのを検出するメソッドはありません。

小さいですが完全なPythonのGLプログラムの例をここに挙げます： ::

   import gl, GL, time

   def main():
       gl.foreground()
       gl.prefposition(500, 900, 500, 900)
       w = gl.winopen('CrissCross')
       gl.ortho2(0.0, 400.0, 0.0, 400.0)
       gl.color(GL.WHITE)
       gl.clear()
       gl.color(GL.RED)
       gl.bgnline()
       gl.v2f(0.0, 0.0)
       gl.v2f(400.0, 400.0)
       gl.endline()
       gl.bgnline()
       gl.v2f(400.0, 0.0)
       gl.v2f(0.0, 400.0)
       gl.endline()
       time.sleep(5)

   main()


.. seealso::

   `PyOpenGL: PythonのOpenGLとの結合 <http://pyopengl.sourceforge.net/>`_
      .. index::
         single: OpenGL
         single: PyOpenGL

      OpenGL へのインタフェースが利用できます；詳しくは **PyOpenGL** プロジェクト
      http://pyopengl.sourceforge.net/ から情報を入手できます。
      これは、SGI のハードウェアが1996年頃より前である必要がないので、
      OpenGL の方が良い選択かもしれません。


:mod:`DEVICE` --- :mod:`gl` モジュールで使われる定数
====================================================

.. module:: DEVICE
   :platform: IRIX
   :synopsis: glモジュールで使われる定数。
   :deprecated:


.. deprecated:: 2.6
    :mod:`DEVICE` モジュールは Python 3.0 での削除に向けて非推奨になりました。


このモジュールには、Silicon Graphics の *Graphics Library* で使われる\
定数が定義されています。これらはCのプログラマーがヘッダーファイル
``<gl/device.h>`` の中から使っているものです。
詳しくはモジュールのソースファイルをご覧ください。


:mod:`GL` --- :mod:`gl` モジュールで使われる定数
================================================

.. module:: GL
   :platform: IRIX
   :synopsis: glモジュールで使われる定数。
   :deprecated:


.. deprecated:: 2.6
    :mod:`GL` モジュールは Python 3.0 での削除に向けて非推奨になりました。


このモジュールには Silicon Graphics の *Graphics Library* で使われる
C のヘッダーファイル ``<gl/gl.h>`` の定数が定義されています。
詳しくはモジュールのソースファイルをご覧ください。

