.. _library-index:

###############################
  Python 標準ライブラリ
###############################

:Release: |version|
:Date: |today|

:ref:`reference-index` ではプログラミング言語 Python の厳密な構文と\
セマンティクスについて説明されていますが、このライブラリリファレンスマニュアル\
では Python とともに配付されている標準ライブラリについて説明します。
また Python 配布物に収められていることの多いオプションのコンポーネント\
についても説明します。

Python の標準ライブラリはとても拡張性があり、下の長い目次のリストで判るように\
幅広いものを用意しています。このライブラリには、\
例えばファイル I/O のように、Python プログラマが直接アクセスできない\
システム機能へのアクセス機能を提供する (Cで書かれた) 組み込みモジュールや、\
日々のプログラミングで生じる多くの問題に標準的な解決策を提供する\
Python で書かれたモジュールが入っています。これら数多くの\
モジュールには、プラットフォーム固有の事情をプラットフォーム独立な API
へと昇華させることにより、Pythonプログラムに移植性を持たせ、それを高めるという\
明確な意図があります。

Windows 向けの Python インストーラはたいてい標準ライブラリのすべてを含み、
しばしばそれ以外の追加のコンポーネントも含んでいます。Unix 系の\
オペレーティングシステムの場合は Python は一揃いのパッケージとして提供されるのが\
普通で、オプションのコンポーネントを手に入れるにはオペレーティングシステムの\
パッケージツールを使うことになるでしょう。

標準ライブラリに加えて、数千のコンポーネントが (独立したプログラムやモジュールから\
パッケージ、アプリケーション開発フレームワークまで) 成長し続けるコレクションとして\
`Python Package Index <http://pypi.python.org/pypi>`_ から入手可能です。


.. toctree::
   :maxdepth: 2
   :numbered:

   intro.rst
   functions.rst
   constants.rst
   stdtypes.rst
   exceptions.rst

   strings.rst
   datatypes.rst
   numeric.rst
   filesys.rst
   persistence.rst
   archiving.rst
   fileformats.rst
   crypto.rst
   allos.rst
   someos.rst
   ipc.rst
   netdata.rst
   markup.rst
   internet.rst
   mm.rst
   i18n.rst
   frameworks.rst
   tk.rst
   development.rst
   debug.rst
   python.rst
   custominterp.rst
   restricted.rst
   modules.rst
   language.rst
   compiler.rst
   misc.rst
   windows.rst
   unix.rst
   mac.rst
   macosa.rst
   sgi.rst
   sun.rst
   undoc.rst

   jptranslation.rst
