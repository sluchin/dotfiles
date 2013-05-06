
:mod:`EasyDialogs` --- 基本的な Macintosh ダイアログ
====================================================

.. module:: EasyDialogs
   :platform: Mac
   :synopsis: 基本的な Macintosh ダイアログ。
   :deprecated:


:mod:`EasyDialogs` モジュールには、Macintosh で単純なダイアログ操作\
を行うためのルーチンが入っています。ダイアログはドックに現れる別の\
アプリケーションとして起動され、ダイアログが表示されるにはクリック\
されなければなりません。
全てのルーチンは、オプションとしてリソース ID パラメタ *id* をとり\
ます。デフォルトの :const:`DLOG` のリソース (タイプとアイテムナンバの\
両方) が一致するようなダイアログがあれば、 *id* を使ってダイアログ\
操作に使われるダイアログオブジェクト情報を上書きできます。詳細は\
ソースコードを参照してください。

:mod:`EasyDialogs` モジュールでは以下の関数を定義しています。

.. note::

   このモジュールは Python 3.x では削除されます。

.. function:: Message(str[, id[, ok]])

   メッセージテキスト *str* 付きのモーダルダイアログを表示します。テキス\
   トの長さは最大255文字です。
   ボタンのテキストはデフォルトでは"OK"ですが、文字列の引数 *ok* を\
   指定して変更できます。ユーザが"OK"ボタンをクリックすると処理を戻します。


.. function:: AskString(prompt[, default[, id[, ok[, cancel]]]])

   ユーザに文字列値の入力を促すモーダルダイアログを表示します。
   *prompt* はプロンプトメッセージで、オプションの *default* 引数は\
   入力文字列の初期値です（指定しなければ ``""`` を使います)。
   "OK"と"Cancel"ボタンの文字列は *ok* と *cancel* の引数で\
   変更できます。文字列の長さは全て最大255文字です。
   入力された文字列か、ユーザがキャンセルした場合には :const:`None` を返します。


.. function:: AskPassword(prompt[, default[, id[, ok[, cancel]]]])

   ユーザに文字列値の入力を促すモーダルダイアログを表示します。
   :func:`AskString` に似ていますが、ユーザの入力したテキストは点で\
   表示されます。引数は :func:`AskString` のものと同じ意味です。


.. function:: AskYesNoCancel(question[, default[, yes[, no[, cancel[, id]]]]])

   プロンプト *question* と"Yes"、"No"、"Cancel"というラベルの3つ\
   ボタンが付いたダイアログを表示します。ユーザが\
   "Yes"を押した場合には ``1`` を、"No" ならば ``0`` を、
   "Cancel" ならば ``-1`` を返します。
   :kbd:`RETURN` キーを押した場合は *default* の値 (*default* を指定\
   しない場合は ``0``)を返します。
   ボタンのテキストはそれぞれ引数 *yes* 、 *no* 、 *cancel* で変更できます。
   ボタンを表示したくなければ引数に ``""`` を指定します。


.. function:: ProgressBar([title[, maxval[, label[, id]]]])

   プログレスバー付きのモードレスダイアログを表示します。
   これは後で述べる :class:`ProgressBar` クラスのコンストラクタです。
   *title* はダイアログに表示するテキスト文字列 (デフォルトの値は "Working...") で、
   *maxval* は処理が完了するときの値です
   (デフォルトは ``0`` で、残りの作業量が不確定であることを示します)。
   *label* はプログレスバー自体の上に表示するテキストです。


.. function:: GetArgv([optionlist[ commandlist[, addoldfile[, addnewfile[, addfolder[, id]]]]]])

   コマンドライン引数リストの作成を補助するためのダイアログを表示します。
   得られた引数リストを ``sys.argv`` の形式にします。これは
   :func:`getopt.getopt` の引数として渡すのに適した形式です。
   *addoldfile* 、 *addnewfile* 、 *addfolder* はブール型の引数\
   です。これらの引数が真の場合、それぞれ実在のファイル、まだ (おそらく) 存在\
   しないファイル、フォルダへのパスをコマンドラインのパスとして設定できます。
   (注意: :func:`getopt.getopt` がファイルやフォルダ引数を認識できる\
   ようにするためには、オブションの引数がそれらより前に現れるように\
   しなければなりません。)
   空白を含む引数は、空白をシングルクォートあるいはダブルクォートで囲んで\
   指定できます。

   ユーザが"Cancel"ボタンを押した場合、 :exc:`SystemExit` 例外を\
   送出します。

   *optionlist* には、ポップアップメニューで選べる選択肢を定義した\
   リストを指定します。ポップアップメニューの要素には、次の2つの形式、
   *optstr* または ``(optstr, descr)`` があります。
   *descr* に短い説明文字列を指定すると、該当の選択肢をポップアップ\
   メニューで選択しいる間その文字列をダイアログに表示します。
   *optstr* とコマンドライン引数の対応を以下に示します:

   +----------------------------+------------------------------------------+
   | *optstr* format            | Command-line format                      |
   +============================+==========================================+
   | ``x``                      | :option:`-x` (短いオプション)            |
   +----------------------------+------------------------------------------+
   | ``x:`` あるいは ``x=``     | :option:`-x` (値を持つ短いオプション)    |
   +----------------------------+------------------------------------------+
   | ``xyz``                    | :option:`--xyz`  (長いオプション)        |
   +----------------------------+------------------------------------------+
   | ``xyz:`` あるいは ``xyz=`` | :option:`--xyz` (値を持つ長いオプション) |
   +----------------------------+------------------------------------------+

   *commandlist* は *cmdstr* あるいは ``(cmdstr, descr)``
   の形のアイテムからなるリストです。 *descr* は上と同じです。
   *cmdstr* はポップアップメニューに表示されます。
   メニューを選択すると *cmdstr* はコマンドラインに追加されますが、それに続く ``':'``
   や ``'='`` は (存在していれば) 取り除かれます。

   .. versionadded:: 2.0


.. function:: AskFileForOpen( [message] [, typeList] [, defaultLocation] [, defaultOptionFlags] [, location] [, clientName] [, windowTitle] [, actionButtonLabel] [, cancelButtonLabel] [, preferenceKey] [, popupExtension] [, eventProc] [, previewProc] [, filterProc] [, wanted] )

   どのファイルを開くかをユーザに尋ねるダイアログを表示し、ユーザが選択した\
   ファイルを返します。ユーザがダイアログをキャンセルした場合には
   :const:`None` を返します。
   *message* はダイアログに表示するテキストメッセージです。
   *typeList* は選択できるファイルタイプを表す 4 文字の文字列からなるリスト、
   *defaultLocation* は最初に表示すルフォルダで、パス名、
   :class:`FSSpec` あるいは :class:`FSRef` で指定します。
   *location* はダイアログを表示するスクリーン上の位置 ``(x, y)`` です。
   *actionButtonLabel* はOKボタンの位置に"Open"の代わり\
   に表示する文字列、
   *cancelButtonLabel* は"Cancel"ボタンの位置に"Cancel"の代わりに表示する文字列です。
   *wanted* は返したい値のタイプで、 :class:`str` 、 :class:`unicode` 、
   :class:`FSSpec` 、 :class:`FSRef` およびそれらのサブタイプを指定できます。

   .. index:: single: Navigation Services

   その他の引数の説明についてはApple Navigation Servicesのドキュメントと
   :mod:`EasyDialogs` のソースコードを参照してください。


.. function:: AskFileForSave( [message] [, savedFileName] [, defaultLocation] [, defaultOptionFlags] [, location] [, clientName] [, windowTitle] [, actionButtonLabel] [, cancelButtonLabel] [, preferenceKey] [, popupExtension] [, fileType] [, fileCreator] [, eventProc] [, wanted] )

   保存先のファイルをユーザに尋ねるダイアログを表示して、ユーザが選択した\
   ファイルを返します。
   ユーザがダイアログをキャンセルした場合には :const:`None` を返します。
   *savedFileName* は保存先のファイル名 (戻り値) のデフォルト値です。
   その他の引数の説明については :func:`AskFileForOpen` を参照してください。


.. function:: AskFolder( [message] [, defaultLocation] [, defaultOptionFlags] [, location] [, clientName] [, windowTitle] [, actionButtonLabel] [, cancelButtonLabel] [, preferenceKey] [, popupExtension] [, eventProc] [, filterProc] [, wanted] )

   フォルダの選択をユーザに促すダイアログを表示して、ユーザが選択したフォルダ\
   を返します。ユーザがダイアログをキャンセルした場合には :const:`None` を\
   返します。引数についての説明は :func:`AskFileForOpen` を参照してください。


.. seealso::

   `Navigation Services Reference <http://developer.apple.com/documentation/Carbon/Reference/Navigation_Services_Ref/>`_
      Programmer's reference documentation の Carbon framework の Navigation Services
      の項。


.. _progressbar-objects:

プログレスバーオブジェクト
--------------------------

:class:`ProgressBar` オブジェクトでは、モードレスなプログレスバーダイアログ\
のサポートを提供しています。定量プログレスバー (温度計スタイル)
と不定量プログレスバー (床屋の螺旋看板スタイル) がサポートされています。
プログレスバーの最大値がゼロ以上の場合には定量インジケータに、
そうでない場合は不定量インジケータになります。

.. versionchanged:: 2.2
   不定量プログレスバーのサポートを追加しました。

ダイアログは作られるとすぐに表示されます。
ダイアログの"Cancel"ボタンを押すか、 :kbd:`Cmd-.` (コマンドキーを押し\
ながらピリオド(``'.'``)を押す) か、あるいは :kbd:`ESC` をタイプ\
すると、ダイアログウィンドウを非表示にして :exc:`KeyboardInterrupt` を送出します
(ただし、この応答は次にプログレスバーを更新するときまで、すなわち次に
:meth:`inc` または :meth:`set` を呼び出してダイアログを\
更新するまで発生しません) 。
それ以外の場合、プログレスバーは :class:`ProgressBar` オブジェクトを廃棄する\
まで表示されたままになります。

:class:`ProgressBar` オブジェクトには以下の属性とメソッドがあります。


.. attribute:: ProgressBar.curval

   プログレスバーの現在の値 (整数型あるいは長整数型) です。
   プログレスバーの通常のアクセスのメソッドによって :attr:`curval` を
   ``0`` と :attr:`maxval` の間にします。この属性を直接変更してはなりません。


.. attribute:: ProgressBar.maxval

   プログレスバーの最大値　(整数型あるいは長整数型) です;
   プログレスバー (温度計, thermometer) では、 :attr:`curval` が
   :attr:`maxval` に等しい時に全量に到達します。
   :attr:`maxval` が ``0`` の場合、不定量プログレスバー (床屋の螺旋看板,
   barbar pole) になります。この属性を直接変更してはなりません。


.. method:: ProgressBar.title([newstr])

   プログレスダイアログのタイトルバーのテキストを *newstr* に設定します。


.. method:: ProgressBar.label([newstr])

   プログレスダイアログ中のプログレスボックスのテキストを *newstr* に設定します。


.. method:: ProgressBar.set(value[, max])

   プログレスバーの現在値 :attr:`curval` を *value* に設定します。
   *max* も指定した場合、 :attr:`maxval` を *max* にします。
   *value* は前もって 0 と :attr:`maxval` の間になるよう強制的に設定\
   されます。温度計バーの場合、変更内容を反映するよう表示を更新します。
   変更によって定量プログレスバーから不定量プログレスバーへ、あるいは\
   その逆への推移が起こります。


.. method:: ProgressBar.inc([n])

   プログレスバーの :attr:`curval` を *n* だけ増やします。
   *n* を指定しなければ ``1`` だけ増やします。
   (*n* は負にもでき、その場合は :attr:`curval` を減少させます。)
   変更内容を反映するようプログレスバーの表示を更新します。プログレスバーが\
   不定量プログレスバーの場合、床屋の螺旋看板 (barbar pole) 模様を 1 度\
   「回転」させます。
   増減によって :attr:`curval` が 0 から :attr:`maxval` までの範囲を越えた場合、
   0 と :attr:`maxval` の範囲に収まるよう強制的に値を設定します。
