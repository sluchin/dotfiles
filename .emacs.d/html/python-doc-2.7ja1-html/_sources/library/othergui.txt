.. _other-gui-packages:

他のグラフィカルユーザインタフェースパッケージ
==============================================

:mod:`Tkinter` に付け加えられるたくさんの拡張ウィジェットがあります。

.. seealso::

   `Python メガウィジェット <http://pmw.sourceforge.net/>`_
      :mod:`Tkinter` モジュールを使い Python で高レベルの複合ウィジェッ\
      トを構築するためのツールキットです。基本クラスとこの基礎の上に構\
      築された柔軟で拡張可能なメガウィジェットから構成されています。こ\
      れらのメガウィジェットはノートブック、コンボボックス、選択ウィ\
      ジェット、ペインウィジェット、スクロールするウィジェット、ダイア\
      ログウィンドウなどを含みます。BLT に対する Pmw.Blt インタフェースを\
      持ち、busy、graph、stripchart、tabset および vector コマンドが利用\
      できます。

      Pmwの最初のアイディアは、Michael McLennan による Tk ``itcl`` 拡張
      ``[incr Tk]`` と Mark Ulferts による ``[incr Widgets]`` から得ま\
      した。メガウィジェットのいくつかは itcl から Python へ直接変換した\
      ものです。 ``[incr Widgets]`` が提供するウィジェットとほぼ同等のも\
      のを提供します。そして、Tix と同様にほぼ完成しています。しかしなが\
      ら、ツリーを描くための Tix の高速な :class:`HList` ウィジェットが\
      欠けています。

   `Tkinter3000 Widget Construction Kit (WCK) <http://tkinter.effbot.org/>`_
      は、新しい Tkinter ウィジェットを、
      Python で書けるようにするライブラリです。WCK フレームワークは、
      ウィジェットの生成、設定、スクリーンの外観、イベント操作における、
      完全な制御を提供します。Tk/Tcl レイヤーを通してデータ転送する必要\
      がなく、直接 Python のデータ構造を操作することができるので、 WCK
      ウィジェットは非常に高速で軽量になり得ます。

主要なクロスプラットフォーム (Windows, Mac OS X, Unix 系) GUI ツールキット\
で Python でも使えるものは:

.. seealso::

   `PyGTK <http://www.pygtk.org/>`_
      は `GTK <http://www.gtk.org/>`_ ウィジェットセットのための一連のバイ\
      ンディングです。C のものより少しだけ高レベルなオブジェクト指向イン\
      タフェースを提供します。Tkinter が提供するよりも沢山のウィジェットが\
      あり、Python に特化した参考資料も良いものがあります。
      `GNOME <http://www.gnome.org>`_ に対しても、バインディング\
      があります。良く知られた PyGTK アプリケーションとしては、
      `PythonCAD <http://www.pythoncad.org/>`_ 。オンライン `チュートリアル
      <http://www.pygtk.org/pygtk2tutorial/index.html>`_ が手に入ります。

   `PyQt <http://www.riverbankcomputing.co.uk/software/pyqt/>`_
      PyQtは :program:`sip` でラップされた Qt ツールキットへのバインディ\
      ングです。Qt は Unix、Windows および Mac OS X で利用できる大規模な
      C++ GUI ツールキットです。 :program:`sip` は Python クラスとして C++
      ライブラリに対するバインディングを生成するためのツールキットで、特\
      に Python 用に設計されています。 *PyQt3* バインディング向けの\
      書籍に Boudewijn Rempt 著 `GUI Programming with Python: QT Edition
      <http://www.commandprompt.com/community/pyqt/>`_ があります。
      *PyQt4* 向けにも Mark Summerfield 著 `Rapid GUI Programming
      with Python and Qt <http://www.qtrac.eu/pyqtbook.html>`_
      があります。

   `wxPython <http://www.wxpython.org>`_
      wxPython はクロスプラットフォームの Python 用 GUI ツールキットで、
      人気のある `wxWidgets <http://www.wxwidgets.org/>`_
      (旧名 wxWindows) C++ ツールキットに基づいて作られています。
      このツールキットは Windows, Mac OS X および Unix システム\
      のアプリケーションに、それぞれのプラットフォームのネイティブなウィ\
      ジェットを可能ならば利用して (Unix系のシステムではGTK+)、ネイティ\
      ブなルック＆フィールを提供します。多彩なウィジェットの他に、オン\
      ラインドキュメントや場面に応じたヘルプ、印刷、HTML 表示、低級デ\
      バイスコンテキスト描画、ドラッグ＆ドロップ、システムクリップボー\
      ドへのアクセス、XML に基づいたリソースフォーマット、さらにユーザ\
      寄贈のモジュールからなる成長し続けているライブラリ等々を\
      wxPython は提供しています。wxPython を扱った書籍として
      Noel Rappin、Robin Dunn 著 `wxPython in Action
      <http://www.amazon.com/exec/obidos/ASIN/1932394621>`_
      があります。

PyGTK、PyQt および wxPython は全て現代的なルック＆フィールを具え Tkinter
より豊富なウィジェットがあります。これらに加えて、他にも Python 用 GUI
ツールキットが、クロスプラットフォームのもの、プラットフォーム固有のものを含め、
沢山あります。Python Wiki の `GUI Programming
<http://wiki.python.org/moin/GuiProgramming>`_ ページも参照してください。
もっとずっと完全なリストや、GUI ツールキット同士の比較をしたドキュメントへの\
リンクがあります。
