
:mod:`jpeg` --- JPEGファイルの読み書きを行う
============================================

.. module:: jpeg
   :platform: IRIX
   :synopsis: JPEGファイルの読み書きを行います。
   :deprecated:


.. deprecated:: 2.6
    :mod:`jpeg` モジュールは Python 3.0 での削除に向けて非推奨になりました。


.. index:: single: Independent JPEG Group

この :mod:`jpeg` モジュールは Independent JPEG Group (IJG)
によって書かれた JEPG 圧縮及び展開アルゴリズムを提供します。
JPEG 形式は写真等の画像圧縮で標準的に利用され、ISO 10918で定義されてます。
JPEG、あるいは Independent JPEG Group ソフトウェアの詳細は、
標準JPEG、若しくは提供されるソフトウェアのドキュメントを参照してください。

.. index::
   single: Python Imaging Library
   single: PIL (the Python Imaging Library)
   single: Lundh, Fredrik

JPEGファイルを扱うポータブルなインタフェースは Fredrik Lundh による
Python Imaging Library (PIL)があります。
また、PILの情報は http://www.pythonware.com/products/pil/ で見つけることができます。

モジュール :mod:`jpeg` では、一つの例外といくつかの関数を定義しています。


.. exception:: error

   関数 :func:`compress` または :func:`decompress` のエラーで上げられる例外です。


.. function:: compress(data, w, h, b)

   .. index:: single: JFIF

   イメージファイルの幅 *w* 、高さ *h* 、1ピクセルあたりのバイト数 *b*
   を引数として扱います。データは SGI GL 順になっていて、
   最初のピクセルは左下端になります。また、これは :func:`gl.lrectread`
   が返す値をすぐに :func:`compress` にかけるためです。
   現在は、1 バイト若しくは 4 バイトのピクセルを取り扱うことができます。
   前者はグレースケール、後者はRGBカラーを扱います。
   :func:`compress` は、圧縮された JFIF 形式のイメージが含まれた文字列を返します。


.. function:: decompress(data)

   .. index:: single: JFIF

   データは圧縮された JFIF 形式のイメージが含まれた文字列で、
   この関数はタプル ``(data, width, height, bytesperpixel)`` を返します。
   また、そのデータは :func:`gl.lrectwrite` を通過します。


.. function:: setoption(name, value)

   :func:`compress` と :func:`decompress` を呼ぶための様々な\
   オプションをセットします。次のオプションが利用できます:

   +-----------------+------------------------------------------------------------------------+
   | オプション      | 効果                                                                   |
   +=================+========================================================================+
   | ``'forcegray'`` | 入力がRGBでも強制的にグレースケールを出力します。                      |
   +-----------------+------------------------------------------------------------------------+
   | ``'quality'``   | 圧縮後イメージの品質を ``0`` から ``100`` の間の値で指定します         |
   |                 | (デフォルトは ``75`` です)。これは圧縮にのみ影響します。               |
   +-----------------+------------------------------------------------------------------------+
   | ``'optimize'``  | ハフマンテーブルを最適化します。時間がかかりますが、高圧縮になります。 |
   |                 | これは圧縮にのみ影響します。                                           |
   +-----------------+------------------------------------------------------------------------+
   | ``'smooth'``    | 圧縮されていないイメージ上でインターブロックスムーシングを行います。   |
   |                 | 低品質イメージに役立ちます。これは展開にのみ影響します。               |
   +-----------------+------------------------------------------------------------------------+


.. seealso::

   JPEG Still Image Data Compression Standard
      The  canonical reference for the JPEG image format, by Pennebaker and Mitchell.

   `Information Technology - Digital Compression and Coding of Continuous-tone Still Images - Requirements and Guidelines <http://www.w3.org/Graphics/JPEG/itu-t81.pdf>`_
      The ISO standard for JPEG is also published as ITU T.81.  This is available
      online in PDF form.

