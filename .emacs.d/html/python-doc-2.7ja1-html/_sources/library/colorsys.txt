:mod:`colorsys` --- 色体系間の変換
==================================

.. module:: colorsys
   :synopsis: RGB 他の色体系間の変換。
.. sectionauthor:: David Ascher <da@python.net>


:mod:`colorsys` モジュールは、計算機のディスプレイモニタで使われている RGB (Red Green Blue) 色空間で表された色と、他の
3 種類の色座標系: YIQ, HLS (Hue Lightness Saturation: 色相、彩度、飽和) および HSV (Hue
Saturation Value: 色相、彩度、明度) との間の双方向の色値変換を定義します。これらの色空間における色座標系は全て浮動小数点数で表されます。
YIQ 空間では、Y 軸は 0 から 1 ですが、 I および Q 軸は正の値も負の値もとり得ます。他の色空間では、各軸は全て 0 から 1 の値を
とります。

.. seealso::

   色空間に関するより詳細な情報は
   http://www.poynton.com/ColorFAQ.html と
   http://www.cambridgeincolour.com/tutorials/color-spaces.htm
   にあります。

:mod:`colorsys` モジュールでは、以下の関数が定義されています:


.. function:: rgb_to_yiq(r, g, b)

   RGB から YIQ に変換します。


.. function:: yiq_to_rgb(y, i, q)

   YIQ から RGB に変換します。


.. function:: rgb_to_hls(r, g, b)

   RGB から HLS に変換します。


.. function:: hls_to_rgb(h, l, s)

   HLS から RGB に変換します。


.. function:: rgb_to_hsv(r, g, b)

   RGB から HSV に変換します。


.. function:: hsv_to_rgb(h, s, v)

   HSV から RGB に変換します。

サンプルコード::

   >>> import colorsys
   >>> colorsys.rgb_to_hsv(.3, .4, .2)
   (0.25, 0.5, 0.4)
   >>> colorsys.hsv_to_rgb(0.25, 0.5, 0.4)
   (0.3, 0.4, 0.2)

