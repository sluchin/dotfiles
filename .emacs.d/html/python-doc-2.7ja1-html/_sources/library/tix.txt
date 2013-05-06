:mod:`Tix` --- Tkの拡張ウィジェット
===================================

.. module:: Tix
   :synopsis: Tkinter用のTk拡張ウィジェット
.. sectionauthor:: Mike Clarkson <mikeclarkson@users.sourceforge.net>


.. index:: single: Tix

:mod:`Tix` (Tk Interface Extension) モジュールは豊富な追加ウィジェットを提供します。
標準 Tk ライブラリには多くの有用なウィジェットがありますが、完全では決してありません。
:mod:`Tix` ライブラリは標準 Tk に欠けている一般的に必要とされるウィジェットの大部分を\
提供します:
:class:`HList` 、 :class:`ComboBox` 、 :class:`Control`
(別名SpinBox) および各種のスクロール可能なウィジェット。
:mod:`Tix` には、一般的に幅広い用途に役に立つたくさんのウィジェットも含まれています:
:class:`NoteBook` 、 :class:`FileEntry` 、 :class:`PanedWindow` など。
それらは40以上あります。

これら全ての新しいウィジェットと使うと、より便利でより直感的なユーザインタフェース作成し、\
あなたは新しい相互作用テクニックをアプリケーションに導入することができます。
アプリケーションとユーザに特有の要求に合うように、\
大部分のアプリケーションウィジェットを選ぶことによって、アプリケーションを設計できます。

.. note::

   :mod:`Tix` は Python 3.0 で :mod:`tkinter.tix` と改名されました。
   :term:`2to3` ツールはソースの変換時に自動的に import を対応させます。

.. seealso::

   `Tix Homepage <http://tix.sourceforge.net/>`_
      :mod:`Tix` のホームページ。
      ここには追加ドキュメントとダウンロードへのリンクがあります。

   `Tix Man Pages <http://tix.sourceforge.net/dist/current/man/>`_
      manページと参考資料のオンライン版。

   `Tix Programming Guide <http://tix.sourceforge.net/dist/current/docs/tix-book/tix.book.html>`_
      プログラマ用参考資料のオンライン版。

   `Tix Development Applications <http://tix.sourceforge.net/Tide/>`_
      Tix と Tkinter プログラムの開発のための Tix アプリケーション。
      Tideアプリケーションは Tk または Tkinter に基づいて動作します。
      また、リモートで Tix/Tk/Tkinter アプリケーションを変更やデバグするための\
      インスペクタ :program:`TixInspect` が含まれます。


Tixを使う
---------


.. class:: Tix(screenName[, baseName[, className]])

   たいていはアプリケーションのメインウィンドウを表すTixのトップレベル\
   ウィジェット。それには Tcl インタープリタが付随します。

   :mod:`Tix` モジュールのクラスは :mod:`Tkinter` モジュールのクラスを\
   サブクラス化します。前者は後者をインポートします。だから、Tkinter と\
   一緒に :mod:`Tix` を使うためにやらなければならないのは、モジュールを\
   一つインポートすることだけです。一般的に、 :mod:`Tix` をインポートし、
   トップレベルでの :class:`Tkinter.Tk` の呼び出しを :class:`Tix.Tk` に\
   置き換えるだけでよいのです::

      import Tix
      from Tkconstants import *
      root = Tix.Tk()

:mod:`Tix` を使うためには、通常 Tk ウィジェットのインストールと平行して、
:mod:`Tix` ウィジェットをインストールしなければなりません。インストー\
ルをテストするために、次のことを試してください::

   import Tix
   root = Tix.Tk()
   root.tk.eval('package require Tix')

これが失敗した場合は、先に進む前に解決しなければならない問題が Tk
のインストールにあることになります。インストールされた :mod:`Tix`
ライブラリを指定するためには環境変数 :envvar:`TIX_LIBRARY` を使ってください。
Tk 動的オブジェクトライブラリ (:file:`tk8183.dll` または :file:`libtk8183.so`)
を含むディレクトリと同じディレクトリに、動的オブジェクトライブラリ
(:file:`tix8183.dll` または :file:`libtix8183.so`)があるかどうかを\
確かめてください。動的オブジェクトライブラリのあるディレクトリには、
:file:`pkgIndex.tcl` (大文字、小文字を区別します) という名前のファイルも\
含まれているべきで、それには次の一行が含まれます::

   package ifneeded Tix 8.1 [list load "[file join $dir tix8183.dll]" Tix]


Tixウィジェット
---------------

`Tix <http://tix.sourceforge.net/dist/current/man/html/TixCmd/TixIntro.htm>`_
は40個以上のウィジェットクラスを :mod:`Tkinter` のレパートリーに導入します。
標準配布の :file:`Demo/tix` ディレクトリには、
:mod:`Tix` ウィジェットのデモがあります。

.. The Python sample code is still being added to Python, hence commented out


基本ウィジェット
^^^^^^^^^^^^^^^^


.. class:: Balloon()

   ヘルプを提示するためにウィジェット上にポップアップする `Balloon
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixBalloon.htm>`_\
   。ユーザがカーソルをBalloonウィジェットが束縛されているウィジェット\
   内部へ移動させたとき、説明のメッセージが付いた小さなポップアップウィ\
   ンドウがスクリーン上に表示されます。

.. Python Demo of:
.. \ulink{Balloon}{http://tix.sourceforge.net/dist/current/demos/samples/Balloon.tcl}


.. class:: ButtonBox()

   `ButtonBox
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixButtonBox.htm>`_
   ウィジェットは、 ``Ok Cancel`` のためだけに普通は使われるようなボタン\
   ボックスを作成します。


.. Python Demo of:
.. \ulink{ButtonBox}{http://tix.sourceforge.net/dist/current/demos/samples/BtnBox.tcl}


.. class:: ComboBox()

   `ComboBox
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixComboBox.htm>`_
   ウィジェットはMS
   Windowsのコンボボックスコントロールに似ています。ユーザはエントリ・サブウィジェットでタイプするか、リストボックス・サブウィジェットから選択するかのどちらかで選択肢を選びます。

.. Python Demo of:
.. \ulink{ComboBox}{http://tix.sourceforge.net/dist/current/demos/samples/ComboBox.tcl}


.. class:: Control()

   `Control
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixControl.htm>`_
   ウィジェットは :class:`SpinBox` ウィジェットとしても知られています。\
   ユーザは二つの矢印ボタンを押すか、またはエントリに直接値を入力して値を\
   調整します。新しい値をユーザが定義した上限と下限に対してチェックします。

.. Python Demo of:
.. \ulink{Control}{http://tix.sourceforge.net/dist/current/demos/samples/Control.tcl}


.. class:: LabelEntry()

   `LabelEntry
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixLabelEntry.htm>`_
   ウィジェットはエントリウィジェットとラベルを一つのメガウィジェットに\
   まとめたものです。"記入形式"型のインタフェースの作成を簡単に行うため\
   に使うことができます。

.. Python Demo of:
.. \ulink{LabelEntry}{http://tix.sourceforge.net/dist/current/demos/samples/LabEntry.tcl}


.. class:: LabelFrame()

   `LabelFrame
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixLabelFrame.htm>`_
   ウィジェットはフレームウィジェットとラベルを一つのメガウィジェットに\
   まとめたものです。LabelFrameウィジェット内部にウィジェットを作成する\
   ためには、 :attr:`frame` サブウィジェットに対して新しいウィジェット\
   を作成し、それらを :attr:`frame` サブウィジェット内部で取り扱います。

.. Python Demo of:
.. \ulink{LabelFrame}{http://tix.sourceforge.net/dist/current/demos/samples/LabFrame.tcl}


.. class:: Meter()

   `Meter
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixMeter.htm>`_
   ウィジェットは実行に時間のかかるバックグラウンド・ジョブの進み具合を\
   表示するために使用できます。

.. Python Demo of:
.. \ulink{Meter}{http://tix.sourceforge.net/dist/current/demos/samples/Meter.tcl}


.. class:: OptionMenu()

   `OptionMenu
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixOptionMenu.htm>`_
   はオプションのメニューボタンを作成します。

.. Python Demo of:
.. \ulink{OptionMenu}{http://tix.sourceforge.net/dist/current/demos/samples/OptMenu.tcl}


.. class:: PopupMenu()

   `PopupMenu
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixPopupMenu.htm>`_
   ウィジェットは ``tk_popup`` コマンドの代替品として使用できま\
   す。 :mod:`Tix` :class:`PopupMenu` ウィジェットの利点は、操作するため\
   により少ないアプリケーション・コードしか必要としないことです。

.. Python Demo of:
.. \ulink{PopupMenu}{http://tix.sourceforge.net/dist/current/demos/samples/PopMenu.tcl}


.. class:: Select()

   `Select
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixSelect.htm>`_
   ウィジェットはボタン・サブウィジェットのコンテナです。ユーザに対する\
   選択オプションのラジオボックスまたはチェックボックス形式を提供するた\
   めに利用することができます。

.. Python Demo of:
.. \ulink{Select}{http://tix.sourceforge.net/dist/current/demos/samples/Select.tcl}


.. class:: StdButtonBox()

   `StdButtonBox
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixStdButtonBox.htm>`_
   ウィジェットは、Motif に似たダイアログボックスのための標準的なボタン\
   のグループです。

.. Python Demo of:
.. \ulink{StdButtonBox}{http://tix.sourceforge.net/dist/current/demos/samples/StdBBox.tcl}


ファイルセレクタ
^^^^^^^^^^^^^^^^


.. class:: DirList()

   `DirList
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixDirList.htm>`_
   ウィジェットは、ディレクトリのリストビュー(その前のディレクトリとサ\
   ブディレクトリ)を表示します。ユーザはリスト内の表示されたディレクト\
   リの一つを選択したり、あるいは他のディレクトリへ変更したりできます。

.. Python Demo of:
.. \ulink{DirList}{http://tix.sourceforge.net/dist/current/demos/samples/DirList.tcl}


.. class:: DirTree()

   `DirTree
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixDirTree.htm>`_
   ウィジェットはディレクトリのツリービュー(その前のディレクトリとその\
   サブディレクトリ)を表示します。ユーザはリスト内に表示されたディレク\
   トリの一つを選択したり、あるいは他のディレクトリに変更したりできます。

.. Python Demo of:
.. \ulink{DirTree}{http://tix.sourceforge.net/dist/current/demos/samples/DirTree.tcl}


.. class:: DirSelectDialog()

   `DirSelectDialog
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixDirSelectDialog.htm>`_
   ウィジェットは、ダイアログウィンドウにファイルシステム内のディレクト\
   リを提示します。望みのディレクトリを選択するために、ユーザはファイル\
   システムを介して操作するこのダイアログウィンドウを利用できます。

.. Python Demo of:
.. \ulink{DirSelectDialog}{http://tix.sourceforge.net/dist/current/demos/samples/DirDlg.tcl}


.. class:: DirSelectBox()

   :class:`DirSelectBox` は標準 Motif(TM) ディレクトリ選択ボックスに似て\
   います。ユーザがディレクトリを選択するために一般的に使われます。\
   DirSelectBox は主に最近 ComboBox ウィジェットに選択されたディレクトリを\
   保存し、すばやく再選択できるようにします。


.. class:: ExFileSelectBox()

   `ExFileSelectBox
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixExFileSelectBox.htm>`_
   ウィジェットは、たいてい tixExFileSelectDialog ウィジェット内に組み込\
   まれます。ユーザがファイルを選択するのに便利なメソッドを提供します。
   :class:`ExFileSelectBox` ウィジェットのスタイルは、MS Windows
   3.1 の標準ファイルダイアログにとてもよく似ています。

.. Python Demo of:
.. \ulink{ExFileSelectDialog}{http://tix.sourceforge.net/dist/current/demos/samples/EFileDlg.tcl}


.. class:: FileSelectBox()

   `FileSelectBox
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixFileSelectBox.htm>`_
   は標準的な Motif(TM) ファイル選択ボックスに似ています。ユーザがファイ\
   ルを選択するために一般的に使われます。FileSelectBox は主に最近
   :class:`ComboBox` ウィジェットに選択されたファイルを保存し、素早く再\
   選択できるようにします。

.. Python Demo of:
.. \ulink{FileSelectDialog}{http://tix.sourceforge.net/dist/current/demos/samples/FileDlg.tcl}


.. class:: FileEntry()

   `FileEntry
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixFileEntry.htm>`_
   ウィジェットはファイル名を入力するために使うことができます。ユーザは\
   手でファイル名をタイプできます。その代わりに、ユーザはエントリの横に\
   並んでいるボタンウィジェットを押すことができます。それはファイル選択\
   ダイアログを表示します。

.. Python Demo of:
.. \ulink{FileEntry}{http://tix.sourceforge.net/dist/current/demos/samples/FileEnt.tcl}


ハイアラキカルリストボックス
^^^^^^^^^^^^^^^^^^^^^^^^^^^^


.. class:: HList()

   `HList
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixHList.htm>`_
   ウィジェットは階層構造をもつどんなデータ(例えば、ファイルシステムディ\
   レクトリツリー)でも表示するために使用できます。リストエントリは字下\
   げされ、階層のそれぞれの場所に応じて分岐線で接続されます。

.. Python Demo of:
.. \ulink{HList}{http://tix.sourceforge.net/dist/current/demos/samples/HList1.tcl}


.. class:: CheckList()

   `CheckList
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixCheckList.htm>`_
   ウィジェットは、ユーザが選ぶ項目のリストを表示します。CheckList は
   Tk のチェックリストやラジオボタンより多くの項目を扱うことができるこ\
   とを除いて、チェックボタンあるいはラジオボタンウィジェットと同じよ\
   うに動作します。

.. Python Demo of:
.. \ulink{ CheckList}{http://tix.sourceforge.net/dist/current/demos/samples/ChkList.tcl}
.. Python Demo of:
.. \ulink{ScrolledHList (1)}{http://tix.sourceforge.net/dist/current/demos/samples/SHList.tcl}
.. Python Demo of:
.. \ulink{ScrolledHList (2)}{http://tix.sourceforge.net/dist/current/demos/samples/SHList2.tcl}


.. class:: Tree()

   `Tree
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixTree.htm>`_
   ウィジェットは階層的なデータをツリー形式で表示するために使うことがで\
   きます。ユーザはツリーの一部を開いたり閉じたりすることによって、ツリー\
   の見えを調整できます。

.. Python Demo of:
.. \ulink{Tree}{http://tix.sourceforge.net/dist/current/demos/samples/Tree.tcl}
.. Python Demo of:
.. \ulink{Tree (Dynamic)}{http://tix.sourceforge.net/dist/current/demos/samples/DynTree.tcl}


タビュラーリストボックス
^^^^^^^^^^^^^^^^^^^^^^^^


.. class:: TList()

   `TList
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixTList.htm>`_
   ウィジェットは、表形式でデータを表示するために使うことができます。
   :class:`TList` ウィジェットのリスト・エントリは、Tkのリストボックス・\
   ウィジェットのエントリに似ています。主な差は、(1) :class:`TList` ウィ\
   ジェットはリスト・エントリを二次元形式で表示でき、(2) リスト・エント\
   リに対して複数の色やフォントだけでなく画像も使うことができるというこ\
   とです。

.. Python Demo of:
.. \ulink{ScrolledTList (1)}{http://tix.sourceforge.net/dist/current/demos/samples/STList1.tcl}
.. Python Demo of:
.. \ulink{ScrolledTList (2)}{http://tix.sourceforge.net/dist/current/demos/samples/STList2.tcl}
.. Grid has yet to be added to Python
.. \subsubsection{Grid Widget}
.. Python Demo of:
.. \ulink{Simple Grid}{http://tix.sourceforge.net/dist/current/demos/samples/SGrid0.tcl}
.. Python Demo of:
.. \ulink{ScrolledGrid}{http://tix.sourceforge.net/dist/current/demos/samples/SGrid1.tcl}
.. Python Demo of:
.. \ulink{Editable Grid}{http://tix.sourceforge.net/dist/current/demos/samples/EditGrid.tcl}


管理ウィジェット
^^^^^^^^^^^^^^^^


.. class:: PanedWindow()

   `PanedWindow
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixPanedWindow.htm>`_
   ウィジェットは、ユーザがいくつかのペインのサイズを対話的に操作できる\
   ようにします。ペインは垂直または水平のどちらかに配置されます。ユーザ\
   は二つのペインの間でリサイズ・ハンドルをドラッグしてペインの大きさを\
   変更します。

.. Python  Demo of:
.. \ulink{PanedWindow}{http://tix.sourceforge.net/dist/current/demos/samples/PanedWin.tcl}


.. class:: ListNoteBook()

   `ListNoteBook
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixListNoteBook.htm>`_
   ウィジェットは、 :class:`TixNoteBook` ウィジェットにとてもよく似てい\
   ます。ノートのメタファを使って限られた空間をに多くのウィンドウを表示\
   するために使われます。ノートはたくさんのページ(ウィンドウ)に分けられ\
   ています。ある時には、これらのページの一つしか表示できません。ユーザ\
   は :attr:`hlist` サブウィジェットの中の望みのページの名前を選択する\
   ことによって、これらのページを切り替えることができます。

.. Python  Demo of:
.. \ulink{ListNoteBook}{http://tix.sourceforge.net/dist/current/demos/samples/ListNBK.tcl}


.. class:: NoteBook()

   `NoteBook
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixNoteBook.htm>`_
   ウィジェットは、ノートのメタファを多くのウィンドウを表示することがで\
   きます。ノートはたくさんのページに分けられています。ある時には、これ\
   らのページの一つしか表示できません。ユーザは NoteBook ウィジェットの\
   一番上にある目に見える"タブ"を選択することで、これらのページを切り替\
   えることができます。

.. Python  Demo of:
.. \ulink{NoteBook}{http://tix.sourceforge.net/dist/current/demos/samples/NoteBook.tcl}

.. \subsubsection{Scrolled Widgets}
.. Python  Demo of:
.. \ulink{ScrolledListBox}{http://tix.sourceforge.net/dist/current/demos/samples/SListBox.tcl}
.. Python  Demo of:
.. \ulink{ScrolledText}{http://tix.sourceforge.net/dist/current/demos/samples/SText.tcl}
.. Python  Demo of:
.. \ulink{ScrolledWindow}{http://tix.sourceforge.net/dist/current/demos/samples/SWindow.tcl}
.. Python  Demo of:
.. \ulink{Canvas Object View}{http://tix.sourceforge.net/dist/current/demos/samples/CObjView.tcl}


画像タイプ
^^^^^^^^^^

:mod:`Tix` モジュールは次のものを追加します:

* 全ての :mod:`Tix` と :mod:`Tkinter` ウィジェットに対して XPM ファイル\
  からカラー画像を作成する `pixmap
  <http://tix.sourceforge.net/dist/current/man/html/TixCmd/pixmap.htm>`_
  機能。

  .. Python  Demo of:
  .. \ulink{XPM Image In Button}{http://tix.sourceforge.net/dist/current/demos/samples/Xpm.tcl}
  .. Python  Demo of:
  .. \ulink{XPM Image In Menu}{http://tix.sourceforge.net/dist/current/demos/samples/Xpm1.tcl}

* `Compound
  <http://tix.sourceforge.net/dist/current/man/html/TixCmd/compound.htm>`_
  画像タイプは複数の水平方向の線から構成される画像を作成するために使う\
  ことができます。それぞれの線は左から右に並べられた一連のアイテム(テキ\
  スト、ビットマップ、画像あるいは空白)から作られます。例えば、Tk の\
  :class:`Button` ウィジェットの中にビットマップとテキスト文字列を同時\
  に表示するためにcompound画像は使われます。

  .. Python  Demo of:
  .. \ulink{Compound Image In Buttons}{http://tix.sourceforge.net/dist/current/demos/samples/CmpImg.tcl}
  .. Python  Demo of:
  .. \ulink{Compound Image In NoteBook}{http://tix.sourceforge.net/dist/current/demos/samples/CmpImg2.tcl}
  .. Python  Demo of:
  .. \ulink{Compound Image Notebook Color Tabs}{http://tix.sourceforge.net/dist/current/demos/samples/CmpImg4.tcl}
  .. Python  Demo of:
  .. \ulink{Compound Image Icons}{http://tix.sourceforge.net/dist/current/demos/samples/CmpImg3.tcl}


その他のウィジェット
^^^^^^^^^^^^^^^^^^^^


.. class:: InputOnly()

   `InputOnly
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixInputOnly.htm>`_
   ウィジェットは、ユーザから入力を受け付けます。それは、 ``bind``
   コマンドを使って行われます(Unixのみ)。


ジオメトリマネジャを作る
^^^^^^^^^^^^^^^^^^^^^^^^

加えて、 :mod:`Tix` は次のものを提供することで :mod:`Tkinter` を補強します:


.. class:: Form()

   Tkウィジェットに対する接続ルールに基づいたジオメトリマネジャを `作成(Form)します
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tixForm.htm>`_ 。



Tixコマンド
-----------


.. class:: tixCommand()

   `tixコマンド
   <http://tix.sourceforge.net/dist/current/man/html/TixCmd/tix.htm>`_
   は :mod:`Tix` の内部状態と :mod:`Tix` アプリケーション・コンテキスト\
   のいろいろな要素へのアクセスを提供します。これらのメソッドによって操\
   作される情報の大部分は、特定のウィンドウというよりむしろアプリケーショ\
   ン全体かスクリーンあるいはディスプレイに関するものです。

   現在の設定を見るための一般的な方法は、  ::

      import Tix
      root = Tix.Tk()
      print root.tix_configure()


.. method:: tixCommand.tix_configure([cnf,] **kw)

   Tix アプリケーション・コンテキストの設定オプションを問い合わせたり、\
   変更したりします。オプションが指定されなければ、利用可能なオプション\
   すべてのディクショナリを返します。オプションが値なしで指定された場合\
   は、メソッドは指定されたオプションを説明するリストを返します(このリ\
   ストはオプションが指定されていない場合に返される値に含まれている、指\
   定されたオプションに対応するサブリストと同一です)。一つ以上のオプショ\
   ン-値のペアが指定された場合は、メソッドは与えられたオプションが与え\
   られた値を持つように変更します。この場合は、メソッドは空文字列を返し\
   ます。オプションは設定オプションのどれでも構いません。


.. method:: tixCommand.tix_cget(option)

   *option* によって与えられた設定オプションの現在の値を返します。オプ\
   ションは設定オプションのどれでも構いません。


.. method:: tixCommand.tix_getbitmap(name)

   ビットマップディレクトリの一つの中の ``name.xpm`` または ``name`` と\
   言う名前のビットマップファイルの場所を見つけ出します
   (:meth:`tix_addbitmapdir` メソッドを参照してくださ\
   い)。 :meth:`tix_getbitmap` を使うことで、アプリケーションにビットマッ\
   プファイルのパス名をハードコーディングすることを避けることができます。
   成功すれば、文字 ``@`` を先頭に付けたビットマップファイルの完全なパ\
   ス名を返します。戻り値をTkとTixウィジェットの ``bitmap`` オプション\
   を設定するために使うことができます。


.. method:: tixCommand.tix_addbitmapdir(directory)

   Tix は :meth:`tix_getimage` と :meth:`tix_getbitmap` メソッドが画像ファ\
   イルを検索するディレクトリのリストを保持しています。標準ビットマップ\
   ディレクトリは :file:`$TIX_LIBRARY/bitmaps` です。
   :meth:`tix_addbitmapdir` メソッドは *directory* をこのリストに追加し\
   ます。そのメソッドを使うことによって、アプリケーションの画像ファイル\
   を :meth:`tix_getimage` または :meth:`tix_getbitmap` メソッドを使っ\
   て見つけることができます。


.. method:: tixCommand.tix_filedialog([dlgclass])

   このアプリケーションからの異なる呼び出しの間で共有される可能性がある\
   ファイル選択ダイアログを返します。最初に呼ばれた時に、このメソッドは\
   ファイル選択ダイアログ・ウィジェットを作成します。このダイアログはそ\
   の後のすべての :meth:`tix_filedialog` への呼び出しで返されます。オプ\
   ションの dlgclass パラメータは、要求されているファイル選択ダイアログ・\
   ウィジェットの型を指定するために文字列として渡されます。指定可能なオ\
   プションは ``tix`` 、 ``FileSelectDialog`` あるいは
   ``tixExFileSelectDialog`` です。


.. method:: tixCommand.tix_getimage(self, name)

   ビットマップディレクトリの一つの中の :file:`name.xpm` 、
   :file:`name.xbm` または :file:`name.ppm` という名前の画像ファイルの\
   場所を見つけ出します(上の :meth:`tix_addbitmapdir` メソッドを参照し\
   てください)。同じ名前(だが異なる拡張子)のファイルが一つ以上ある場合\
   は、画像のタイプがXディスプレイの深さに応じて選択されます。xbm画像は\
   モノクロディスプレイの場合に選択され、カラー画像はカラーディスプレイ\
   の場合に選択されます。 :meth:`tix_getimage` を使うことによって、アプ\
   リケーションに画像ファイルのパス名をハードコーディングすることを避け\
   られます。成功すれば、このメソッドは新たに作成した画像の名前を返し、\
   Tk と Tix ウィジェットの ``image`` オプションを設定するためにそれを使う\
   ことができます。


.. method:: tixCommand.tix_option_get(name)

   Tixのスキーム・メカニズムによって保持されているオプションを得ます。


.. method:: tixCommand.tix_resetoptions(newScheme, newFontSet[, newScmPrio])

   Tix アプリケーションのスキームとフォントセットを *newScheme* と
   *newFontSet* それぞれへと再設定します。これはこの呼び出し後に作成さ\
   れたそれらのウィジェットだけに影響します。そのため、Tix アプリケーショ\
   ンのどんなウィジェットを作成する前に resetoptions メソッドを呼び出すの\
   が最も良いのです。

   オプション・パラメータ *newScmPrio* を、Tix スキームによって設定され\
   る Tk オプションの優先度レベルを再設定するために与えることができます。

   Tk が X オプションデータベースを扱う方法のため、Tix がインポートされ初期\
   化された後に、カラースキームとフォントセットを :meth:`tix_config` メ\
   ソッドを使って再設定することができません。その代わりに、
   :meth:`tix_resetoptions` メソッドを使わなければならないのです。


