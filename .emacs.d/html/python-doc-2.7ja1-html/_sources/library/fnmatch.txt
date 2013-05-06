
:mod:`fnmatch` --- Unix ファイル名のパターンマッチ
==================================================

.. module:: fnmatch
   :synopsis: Unix シェル形式のファイル名のパターンマッチ。


.. index:: single: filenames; wildcard expansion

.. index:: module: re

このモジュールは Unix のシェル形式のワイルドカードへの対応を提供しますが、(:mod:`re` モジュールでドキュメント化されている)
正規表現と同じでは *ありません* 。シェル形式のワイルドカードで使われる特別な文字は、

+------------+-------------------------------------------+
| Pattern    | Meaning                                   |
+============+===========================================+
| ``*``      | すべてにマッチします                      |
+------------+-------------------------------------------+
| ``?``      | 任意の一文字にマッチします                |
+------------+-------------------------------------------+
| ``[seq]``  | *seq* にある任意の文字にマッチします      |
+------------+-------------------------------------------+
| ``[!seq]`` | *seq* にない任意の文字にマッチします      |
+------------+-------------------------------------------+

.. index:: module: glob

ファイル名のセパレーター(Unixでは ``'/'``)はこのモジュールに固有なものでは *ない* ことに注意してください。パス名展開については、
:mod:`glob` モジュールを参照してください (:mod:`glob` はパス名の部分にマッチさせるのに :func:`fnmatch` を使っ
ています)。同様に、ピリオドで始まるファイル名はこのモジュールに固有ではなくて、 ``*`` と ``?`` のパターンでマッチします。

.. seealso::

   最新バージョンの `fnmatch の Python ソースコード
   <http://svn.python.org/view/python/branches/release27-maint/Lib/fnmatch.py?view=markup>`_

.. function:: fnmatch(filename, pattern)

   filenameの文字列がpatternの文字列にマッチするかテストして、 :const:`True` 、 :const:`False` のいずれかを返します。
   オペレーティングシステムが大文字、小文字を区別しない場合、比較を行う前に、両方のパラメタを全て大文字、または全て小文字に揃えます。
   オペレーティングシステムが標準でどうなっているかに関係なく、大小文字を区別して比較する場合には、 :func:`fnmatchcase` が使えます。

   .. This example will print all file names in the current directory with the
      extension ``.txt``::

   次の例では、カレントディレクトリにある、拡張子が ``.txt`` である全てのファイルを表示しています。 ::

      import fnmatch
      import os

      for file in os.listdir('.'):
          if fnmatch.fnmatch(file, '*.txt'):
              print file

.. function:: fnmatchcase(filename, pattern)

   *filename* が *pattern* にマッチするかテストして、 :const:`True` 、 :const:`False` を返します。比較は大文字、小文字を区別します。


.. function:: filter(names, pattern)

   *pattern* にマッチする *names* のリストの部分集合を返します。
   ``[n for n in names if fnmatch(n, pattern)]``
   と同じですが、もっと効率よく実装しています。

   .. versionadded:: 2.2


.. function:: translate(pattern)

   シェルスタイルの *pattern* を、正規表現に変換して返します。

   メタ文字をクォートする方法が無いことに気を付けてください。

   例:

      >>> import fnmatch, re
      >>>
      >>> regex = fnmatch.translate('*.txt')
      >>> regex
      '.*\\.txt$'
      >>> reobj = re.compile(regex)
      >>> reobj.match('foobar.txt')
      <_sre.SRE_Match object at 0x...>


.. seealso::

   Module :mod:`glob`
      Unix シェル形式のパス展開。

