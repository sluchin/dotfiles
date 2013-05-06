
:mod:`sndhdr` --- サウンドファイルの識別
========================================

.. module:: sndhdr
   :synopsis: サウンドファイルの識別
.. sectionauthor:: Fred L. Drake, Jr. <fdrake@acm.org>

.. index::
   single: A-LAW
   single: u-LAW


:mod:`sndhdr` モジュールには、ファイルに保存されたサウンドデータの形式を識別するのに便利な関数が定義されています。
どんな形式のサウンドデータがファイルに保存されているのか識別可能な場合、これらの関数は
``(type, sampling_rate, channels, frames, bits_per_sample)`` のタプルを返します。
*type* はデータの形式を示す文字列で、 ``'aifc'``, ``'aiff'``,
``'au'``, ``'hcom'``, ``'sndr'``, ``'sndt'``, ``'voc'``,
``'wav'``, ``'8svx'``, ``'sb'``, ``'ub'``, ``'ul'`` のうちの一つです。
*sampling_rate* は実際のサンプリングレート値で、未知の場合や読み取ることが出来なかった場合は ``0`` です。
同様に、 *channels* はチャンネル数で、識別できない場合や読み取ることが出来なかった場合は ``0`` です。
*frames* はフレーム数で、識別できない場合は ``-1`` です。
タプルの最後の要素 *bits_per_sample* はサンプルサイズを示すビット数ですが、A-LAWなら ``'A'``, u-LAWなら ``'U'`` です。

.. % Based on comments in the module source file.


.. function:: what(filename)

   :func:`whathdr` を使って、ファイル *filename* に保存されたサウンドデータの形式を識別します。
   識別可能なら上記のタプルを返し、識別できない場合は ``None`` を返します。


.. function:: whathdr(filename)

   ファイルのヘッダ情報をもとに、保存されたサウンドデータの形式を識別します。ファイル名は *filename* で渡されます。
   識別可能なら上記のタプルを返し、識別できない場合は ``None`` を返します。

