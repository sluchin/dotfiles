:mod:`filecmp` --- ファイルおよびディレクトリの比較
===================================================

.. module:: filecmp
   :synopsis: ファイル群を効率的に比較します。
.. sectionauthor:: Moshe Zadka <moshez@zadka.site.co.il>


:mod:`filecmp` モジュールでは、ファイルおよびディレクトリを比較するため、様々な時間／正確性のトレードオフに関するオプションを
備えた関数を定義しています。
ファイルの比較については、 :mod:`differ` モジュールも参照してください。

.. seealso::

   最新バージョンの `filecmp Python ソースコード
   <http://svn.python.org/view/python/branches/release27-maint/Lib/filecmp.py?view=markup>`_

:mod:`filecmp` モジュールでは以下の関数を定義しています:


.. function:: cmp(f1, f2[, shallow])

   名前が *f1* および *f2* のファイルを比較し、二つのファイルが同じらしければ ``True`` を返し、そうでなければ ``false`` を
   返します。

   *shallow* が与えられておりかつ偽でなければ、 :func:`os.stat` の返すシグネチャが一致するファイルは同じであると見なされます。

   この関数で比較されたファイルは :func:`os.stat` シグネチャが変更されるまで再び比較されることはありません。 *use_statcache*
   を真にすると、キャッシュ無効化機構を失敗させます --- そのため、 :mod:`statcache` のキャッシュから古いファイル stat 値が
   使われます。

   可搬性と効率のために、個の関数は外部プログラムを一切呼び出さないので注意してください。


.. function:: cmpfiles(dir1, dir2, common[, shallow])

   *dir1* と *dir2* ディレクトリの中の、 *common* で指定されたファイルを比較します。

   ファイル名からなる3つのリスト: *match*, *mismatch*, *errors* を返します。
   *match* には双方のディレクトリで一致したファイルのリストが含まれ、
   *mismatch* にはそうでないファイル名のリストが入ります。
   そして *errors* は比較されなかったファイルが列挙されます。
   *errors* になるのは、片方あるいは両方のディレクトリに存在しなかった、ユーザーにそのファイルを読む権限がなかった、
   その他何らかの理由で比較を完了することができなかった場合です。

   引数 *shallow* はその意味も標準の設定も :func:`filecmp.cmp` と同じです。

   .. For example, ``cmpfiles('a', 'b', ['c', 'd/e'])`` will compare ``a/c`` with
      ``b/c`` and ``a/d/e`` with ``b/d/e``.  ``'c'`` and ``'d/e'`` will each be in
      one of the three returned lists.

   例えば、 ``cmpfiles('a', 'b', ['c', 'd/e'])`` は ``a/c`` を ``b/c`` と、
   ``a/d/e`` を ``b/d/e`` と、それぞれ比較します。 ``'c'`` と ``'d/e'``
   はそれぞれ、返される3つのリストのいずれかに登録されます。

例::

   >>> import filecmp
   >>> filecmp.cmp('undoc.rst', 'undoc.rst')
   True
   >>> filecmp.cmp('undoc.rst', 'index.rst')
   False


.. _dircmp-objects:

:class:`dircmp` クラス
----------------------

:class:`dircmp` のインスタンスは以下のコンストラクタで生成されます:


.. class:: dircmp(a, b[, ignore[, hide]])

   ディレクトリ *a* および *b* を比較するための新しいディレクトリ比較オブジェクトを生成します。 *ignore* は比較の際に無視する
   ファイル名のリストで、標準の設定では ``['RCS', 'CVS', 'tags']`` です。 *hide* は表示しない名前のリストで、標準の設定では
   ``[os.curdir, os.pardir]`` です。

   :class:`dircmp` クラスは以下のメソッドを提供しています:


   .. method:: report()

      *a* および *b* の間の比較結果を (``sys.stdout`` に) 出力します。


   .. method:: report_partial_closure()

      *a* および *b* およびそれらの直下にある共通のサブディレクトリ間での比較結果を出力します。


   .. method:: report_full_closure()

      *a* および *b* およびそれらの共通のサブディレクトリ間での比較結果を (再帰的に比較して) 出力します。

   :class:`dircmp` は、比較しているディレクトリツリーに関する様々な種類の情報を取得するために使えるような、多くの興味深い属性を提供しています。

   :meth:`__getattr__` フックを経由すると、全ての属性をのろのろと計算するため、速度上のペナルティを受けないのは
   計算処理の軽い属性を使ったときだけなので注意してください。


   .. attribute:: left_list

      *a* にあるファイルおよびサブディレクトリです。 *hide* および *ignore* でフィルタされています。


   .. attribute:: right_list

      *b* にあるファイルおよびサブディレクトリです。 *hide* および *ignore* でフィルタされています。


   .. attribute:: common

      *a* および *b* の両方にあるファイルおよびサブディレクトリです。


   .. attribute:: left_only

      *a* だけにあるファイルおよびサブディレクトリです。


   .. attribute:: right_only

      *b* だけにあるファイルおよびサブディレクトリです。


   .. attribute:: common_dirs

      *a* および *b* の両方にあるサブディレクトリです。


   .. attribute:: common_files

      *a* および *b* の両方にあるファイルです。


   .. attribute:: common_funny

      *a* および *b* の両方にあり、ディレクトリ間でタイプが異なるか、 :func:`os.stat` がエラーを報告するような名前です。


   .. attribute:: same_files

      *a* および *b* 両方にあり、一致するファイルです。


   .. attribute:: diff_files

      *a* および *b* 両方にあるが、一致しないファイルです。


   .. attribute:: funny_files

      *a* および *b* 両方にあるが、比較されなかったファイルです。


   .. attribute:: subdirs

      :attr:`common_dirs` のファイル名を :class:`dircmp` オブジェクトに対応付けた辞書です。

