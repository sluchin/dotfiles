
:mod:`platform` ---  実行中プラットフォームの固有情報を参照する
===============================================================

.. module:: platform
   :synopsis: 実行中プラットフォームからできるだけ多くの固有情報を取得する
.. moduleauthor:: Marc-Andre Lemburg <mal@egenix.com>
.. sectionauthor:: Bjorn Pettersen <bpettersen@corp.fairisaac.com>


.. versionadded:: 2.3

.. note::

   プラットフォーム毎にアルファベット順に並べています。Linuxについては Unixセクションを参照してください。


クロスプラットフォーム
-----------------------


.. function:: architecture(executable=sys.executable, bits='', linkage='')

   *executable* で指定した実行可能ファイル（省略時はPythonインタープリタのバイナリ）の各種アーキテクチャ情報を調べます。

   戻り値はタプル ``(bits, linkage)`` で、アーキテクチャのビット数と実行可能ファイルのリンク形式を示します。どちらの値も文字列で返ります。

   値が不明な場合は、パラメータで指定した値が返ります。 *bits* を
   ``''`` と指定した場合、ビット数として :c:func:`sizeof(pointer)` が返
   ります。（Pythonのバージョンが1.5.2以下の場合は、サポートされているポインタサイズとして :c:func:`sizeof(long)` を使用します。）

   この関数は、システムの :file:`file` コマンドを使用します。 :file:`file` はほ
   とんどのUnixプラットフォームと一部の非Unixプラットフォームで利用
   可能ですが、 :file:`file` コマンドが利用できず、かつ *executable* が Pythonインタープリタでない場合には適切なデフォルト値が返ります。

   .. note::

      Mac OS X (とひょっとすると他のプラットフォーム) では、実行可能ファイルは
      複数のアーキテクチャを含んだユニバーサル形式かもしれません。

      現在のインタプリタが "64-bit" であるかどうかを調べるには、 :attr:`sys.maxsize`
      の方が信頼できます。 ::

         is_64bits = sys.maxsize > 2**32


.. function:: machine()

   ``'i386'`` のような、機種を返します。不明な場合は空文字列を返します。


.. function:: node()

   コンピュータのネットワーク名を返します。ネットワーク名は完全修飾名とは限りません。不明な場合は空文字列を返します。


.. function:: platform(aliased=0, terse=0)

   実行中プラットフォームを識別する文字列を返します。この文字列には、有益な情報をできるだけ多く付加しています。

   戻り値は機械で処理しやすい形式ではなく、 *人間にとって読みやすい* 形式となっています。異なったプラットフォームでは異なった戻り値となるようになっています。

   *aliased* が真なら、システムの名称として一般的な名称ではなく、別名を使用して結果を返します。たとえば、SunOS は Solaris
   となります。この機能は :func:`system_alias` で実装されています。

   *terse* が真なら、プラットフォームを特定するために最低限必要な情報だけを返します。


.. function:: processor()

   ``'amdk6'`` のような、（現実の）プロセッサ名を返します。

   不明な場合は空文字列を返します。NetBSDのようにこの情報を提供しない、または :func:`machine` と同じ値しか返さないプラットフォームも多く存在
   しますので、注意してください。


.. function:: python_build()

   Pythonのビルド番号と日付を、 ``(buildno, builddate)`` のタプルで返します。


.. function:: python_compiler()

   Pythonをコンパイルする際に使用したコンパイラを示す文字列を返します。


.. function:: python_branch()

   .. Returns a string identifying the Python implementation SCM branch.

   Python実装のバージョン管理システム上のブランチを特定する文字列を返します。

   .. versionadded:: 2.6


.. function:: python_implementation()

   .. Returns a string identifying the Python implementation. Possible return values
      are: 'CPython', 'IronPython', 'Jython'

   Python実装を指定する文字列を返します。
   戻り値は: 'CPython', 'IronPython', 'Jython', 'PyPy' のいずれかです。

   .. versionadded:: 2.6


.. function:: python_revision()

   .. Returns a string identifying the Python implementation SCM revision.

   Python実装のバージョン管理システム上のリビジョンを特定する文字列を返します。

   .. versionadded:: 2.6


.. function:: python_version()

   Pythonのバージョンを、 ``'major.minor.patchlevel'`` 形式の文字列で返します。

   ``sys.version`` と異なり、patchlevel（デフォルトでは0)も必ず含まれています。


.. function:: python_version_tuple()

   Pythonのバージョンを、文字列のタプル ``(major, minor, patchlevel)``  で返します。

   ``sys.version`` と異なり、patchlevel（デフォルトでは ``0``)も必ず含まれています。


.. function:: release()

   ``'2.2.0'`` や ``'NT'`` のような、システムのリリース情報を返します。不明な場合は空文字列を返します。


.. function:: system()

   ``'Linux'``, ``'Windows'``, ``'Java'`` のような、システム/OS 名を返します。不明な場合は空文字列を返します。


.. function:: system_alias(system, release, version)

   マーケティング目的で使われる一般的な別名に変換して ``(system, release, version)`` を返します。混乱を避けるために、情報を
   並べなおす場合があります。


.. function:: version()

   ``'#3 on degas'`` のような、システムのリリース情報を返します。不明な場合は空文字列を返します。


.. function:: uname()

   非常に可搬性の高い uname インターフェースで、実行中プラットフォームを示す情報を、
   文字列のタプル ``(system, node, release, version, machine, processor)``
   で返します。

   :func:`os.uname` と異なり、複数のプロセッサ名が候補としてタプルに追加される場合があります。

   不明な項目は ``''`` となります。


Java プラットフォーム
---------------------


.. function:: java_ver(release='', vendor='', vminfo=('','',''), osinfo=('','',''))

   Jython用のバージョンインターフェースです。
   
   タプル ``(release, vendor, vminfo, osinfo)`` を返します。 *vminfo* は
   タプル ``(vm_name, vm_release, vm_vendor)`` 、 *osinfo* はタプル ``(os_name, os_version,
   os_arch)`` です。不明な項目は引数で指定した値(デフォルトは ``''``) となります。


Windows プラットフォーム
------------------------


.. function:: win32_ver(release='', version='', csd='', ptype='')

   Windowsのレジストリからバージョン情報を取得し、バージョン番号/CSDレベル/OSタイプ（シングルプロセッサ又はマルチプロセッサ）をタプル
   ``(version, csd, ptype)`` で返します。

   参考： *ptype* はシングルプロセッサのNT上では ``'Uniprocessor Free'`` 、マルチプロセッサでは
   ``'Multiprocessor Free'`` となります。
   *'Free'* がついている場合はデバッグ用のコードが含まれていないことを示し、 *'Checked'* がつい
   ていれば引数や範囲のチェックなどのデバッグ用コードが含まれていることを示します。

   .. note::

      この関数は、Mark Hammondの :mod:`win32all` がインストールされた環境で
      良く動作しますが、Python 2.3 以上なら一応動作します。(Python 2.6から
      サポートされました)
      もちろん、この関数が使えるのはWin32互換プラットフォームのみです。


Win95/98 固有
^^^^^^^^^^^^^

.. function:: popen(cmd, mode='r', bufsize=None)

   可搬性の高い :func:`popen` インターフェースで、可能なら
   :func:`win32pipe.popen` を使用します。 :func:`win32pipe.popen` はWindows
   NTでは利用可能ですが、Windows 9xではハングしてしまいます。


Mac OS プラットフォーム
-----------------------


.. function:: mac_ver(release='', versioninfo=('','',''), machine='')

   Mac OSのバージョン情報を、タプル ``(release, versioninfo, machine)`` で返します。 *versioninfo* は、タ
   プル ``(version, dev_stage, non_release_version)`` です。

   不明な項目は ``''`` となります。タプルの要素は全て文字列です。

   この関数で使用している :c:func:`gestalt` API については、
   http://www.rgaros.nl/gestalt/ を参照してください。


Unix プラットフォーム
---------------------


.. function:: dist(distname='', version='', id='', supported_dists=('SuSE','debian','redhat','mandrake',...))

   この関数は、現在 :func:`linux_distribution` が提供している機能の古い
   バージョンです。新しいコードを書くときは、 :func:`linux_distribution`
   を利用してください。

   :func:`linux_distribution` との違いは、 ``dist()`` は常に ``supported_dists``
   引数から取った短い名前を返す事です。

.. function:: linux_distribution(distname='', version='', id='', supported_dists=('SuSE','debian','redhat','mandrake',...), full_distribution_name=1)

   .. Tries to determine the name of the Linux OS distribution name.

   OSディストリビューション名の取得を試みます。

   .. ``supported_dists`` may be given to define the set of Linux distributions to
      look for. It defaults to a list of currently supported Linux distributions
      identified by their release file name.

   ``supported_dists`` は、検索するLinuxディストリビューションを定義するために利用します。
   デフォルトでは、リリースファイル名で定義されている、
   現在サポートされているLinuxディストリビューションのリストです。

   .. If ``full_distribution_name`` is true (default), the full distribution read
      from the OS is returned. Otherwise the short name taken from
      ``supported_dists`` is used.

   ``full_distribution_name`` が ``True`` (デフォルト)の場合、
   OSから読み込まれた完全なディストリビューション名が返されます。
   それ以外の場合、 ``supported_dists`` で利用された短い名前が返されます。

   .. Returns a tuple ``(distname,version,id)`` which defaults to the args given as
      parameters.  ``id`` is the item in parentheses after the version number.  It
      is usually the version codename.

   戻り値はタプル ``(distname, version, id)`` で、不明な項目は引数で指定した値となります。


   .. versionadded:: 2.6

.. function:: libc_ver(executable=sys.executable, lib='', version='', chunksize=2048)

   executableで指定したファイル（省略時はPythonインタープリタ）がリンクしているlibcバージョンの取得を試みます。戻り値は文字列のタプル
   ``(lib, version)`` で、不明な項目は引数で指定した値となります。

   この関数は、実行形式に追加されるシンボルの細かな違いによって、libcのバージョンを特定します。この違いは :program:`gcc` でコンパイルされた実行
   可能ファイルでのみ有効だと思われます。

   *chunksize* にはファイルから情報を取得するために読み込むバイト数を指定します。

