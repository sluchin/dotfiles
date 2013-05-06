
:mod:`glob` --- Unix 形式のパス名のパターン展開
===============================================

.. module:: glob
   :synopsis: Unix シェル形式のパス名のパターン展開。


.. index:: single: filenames; pathname expantion

:mod:`glob` モジュールはUnix シェルで使われているルールに従って指定されたパターンにマッチするすべてのパス名を見つけ出します。
チルダ展開は使えませんが、 ``*`` 、 ``?`` と ``[]`` で表される文字範囲には正しくマッチします。これは :func:`os.listdir`
関数と :func:`fnmatch.fnmatch` 関数を一緒に使って実行されていて、実際に subshell を呼び出しているわけではありま
せん。(チルダ展開とシェル変数展開を利用したければ、 :func:`os.path.expanduser`
と :func:`os.path.expandvars` を使ってください。)

.. seealso::

   最新バージョンの `glob モジュールの Python ソースコード
   <http://svn.python.org/view/python/branches/release27-maint/Lib/glob.py?view=markup>`_

.. function:: glob(pathname)

   *pathname* (パスの指定を含んだ文字列でなければいけません。)にマッチする空の可能性のあるパス名のリストを返します。

   *pathname* は( :file:`/usr/src/Python-1.5/Makefile` のように)絶対パスでも
   いいし、(:file:`../../Tools/\*/\*.gif` のように)相対パスでもよくて、シェル形式のワイルドカードを含んでいてもかまいません。
   結果には(シェルと同じく)壊れたシンボリックリンクも含まれます。


.. function:: iglob(pathname)

   実際には一度に全てを格納せずに、 :func:`glob` と同じ値を順に生成するイテレータを返します。

   .. versionadded:: 2.5

たとえば、次の三つのファイルだけがあるディレクトリを考えてください: :file:`1.gif` 、 :file:`2.txt` 、 :file:`card.gif` 。
:func:`glob` は次のような結果になります。パスに接頭するどの部分が保たれているかに注意してください。 ::

   >>> import glob
   >>> glob.glob('./[0-9].*')
   ['./1.gif', './2.txt']
   >>> glob.glob('*.gif')
   ['1.gif', 'card.gif']
   >>> glob.glob('?.gif')
   ['1.gif']


.. seealso::

   Module :mod:`fnmatch`
      シェル形式の(パスではない)ファイル名展開

