
:mod:`dircache` --- キャッシュされたディレクトリ一覧の生成
==========================================================

.. module:: dircache
   :synopsis: キャッシュメカニズムを備えたディレクトリ一覧生成。
   :deprecated:

.. deprecated:: 2.6
   :mod:`dircache` モジュールは Python 3.0 で削除されました。


.. sectionauthor:: Moshe Zadka <moshez@zadka.site.co.il>


:mod:`durcache` モジュールはキャッシュされた情報を使って\
ディレクトリ一覧を読み出すための関数を定義しています。
キャッシュはディレクトリの *mtime* に応じて無効化されます。
さらに、一覧中のディレクトリにスラッシュ ('/') を追加することで\
ディレクトリであると分かるようにするための関数も定義しています。

:mod:`dircache` モジュールは以下の関数を定義しています:


.. function:: reset()

   ディレクトリキャッシュをリセットします。


.. function:: listdir(path)

   :func:`os.listdir` によって得た *path* のディレクトリ一覧を\
   返します。 *path* を変えない限り、以降の :func:`listdir`
   を呼び出してもディレクトリ構造を読み込みなおすことはしないので\
   注意してください。

   返されるリストは読み出し専用であると見なされるので注意してください
   (おそらく将来のバージョンではタプルを返すように変更されるはず ? です)。


.. function:: opendir(path)

   :func:`listdir` と同じです。以前のバージョンとの互換性のために\
   定義されています。


.. function:: annotate(head, list)

   *list* を *head* の相対パスからなるリストとして、
   各パスがディレクトリを指す場合には ``'/'`` をパス名の後ろ\
   に追加したものに置き換えます。

::

   >>> import dircache
   >>> a = dircache.listdir('/')
   >>> a = a[:] # Copy the return value so we can change 'a'
   >>> a
   ['bin', 'boot', 'cdrom', 'dev', 'etc', 'floppy', 'home', 'initrd', 'lib', 'lost+
   found', 'mnt', 'proc', 'root', 'sbin', 'tmp', 'usr', 'var', 'vmlinuz']
   >>> dircache.annotate('/', a)
   >>> a
   ['bin/', 'boot/', 'cdrom/', 'dev/', 'etc/', 'floppy/', 'home/', 'initrd/', 'lib/
   ', 'lost+found/', 'mnt/', 'proc/', 'root/', 'sbin/', 'tmp/', 'usr/', 'var/', 'vm
   linuz']

