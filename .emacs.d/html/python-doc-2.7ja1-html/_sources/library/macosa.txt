
.. _mac-scripting:

************************
MacPython OSA モジュール
************************

この章では、オープンスクリプティングアーキテクチャ
(Open Scripting Architecture、OSA、一般的には
AppleScript として知られている) の現在の Python 用実装について説明します。
これを使うとスクリプト可能なアプリケーションを Python プログラムから
実に python らしいインタフェースとともに制御することができます。
このモジュール群の開発は停止しました。
もっと新しい AppleScript サポートの実装については、サードパーティのライブラリ
である py-appscript プロジェクトを参照してください:
<http://pypi.python.org/pypi/appscript/>

AppleScript や OSA の様々なコンポーネントの説明、
およびそのアーキテクチャや用語の理解のために、
Apple のドキュメントを読んでおく方がよいでしょう。
"Applescript Language Guide" は概念モデルと用語を説明し、
標準スイートについて文書にまとめてあります。
"Open Scripting Architecture" はアプリケーションプログラマの視点から、
OSA を使用する方法について説明しています。
Apple ヘルプビューワにおいてこれらは Developer Documentation, Core Technologies
セクションで見つかります。

アプリケーションでスクリプト制御する例として、以下の AppleScript コードは、
もっとも手前にある :program:`Finder` のウィンドウの名前を取得して表示させます::

   tell application "Finder"
       get name of window 1
   end tell

Python では、以下のコードで同じ事ができます::

   import Finder

   f = Finder.Finder()
   print f.get(f.window(1).name)

配布されている Python ライブラリには、標準スイートを実装したパッケージに加えて、
いくつかのよくあるアプリケーションへのインタフェースを実装したパッケージが含まれています。

AppleEvent をアプリケーションに送るためには、最初にアプリケーションの用語
(:program:`Script Editor` が「辞書」と呼んでいるもの)
を話せる Python パッケージを作らなければなりません。
この作業は :program:`PythonIDE` の中から行うこともできますし、
コマンドラインから :file:`gensuitemodule.py` モジュールをスタンドアロンの\
プログラムとして実行することでもできます。

作成されるのはいくつものモジュールからなるパッケージで、
それぞれのモジュールはプログラムで使われるスイートであり
:mod:`__init__` モジュールがそれらを取りまとめています。
Python の継承グラフは AppleScript の継承グラフに従っていますので、
プログラムの辞書が標準スイートのサポートを含みつつ、
一つ二つ動詞を追加の引数で拡張するように指定しているならば、
出力されるスイートは :mod:`Standard_Suite` という
:mod:`StdSuites.Standard_Suite` からすべてをインポートしてエクスポートし直しつつ
追加された機能を持つようにメソッドをオーバーライドしたモジュールを含みます。
:mod:`gensuitemodule` の出力は非常に読み易く、
また元々の AppleScript 辞書にあったドキュメントを Python 文書化文字列 (docstring)
中に含みますので、それを読むことは有用な情報源となります。

出力されたパッケージはパッケージと同じ名前のメインクラスを実装しており、
これは全ての AppleScript 動詞を直接のオブジェクトは第1引数で、
オプションのパラメータはキーワード引数で受けるメソッドとして含みます。
AppleScript クラスも Python クラスとして実装されたり、その他諸々も同様です。

動詞を実装しているメインの Python クラスはまた AppleScript の "application"
クラスで宣言されたプロパティおよび要素へのアクセスも許します。
現在のリリースではこれはオブジェクト指向的というには程遠く、
上の例で見たように ``f.get(f.window(1).name)`` と書かねばならず、
より Python らしい ``f.window(1).name.get()`` という書き方はできません。

AppleScript の識別子が Python の識別子として扱えない場合以下の少数のルールで変換します:

* 空白はアンダースコアに置き換えられます

* その他の英数字以外の文字は ``_xx_`` に置き換えられます。ここで ``xx``
  はその文字の16進値です。

* Python の予約語にはアンダースコアが後ろに付けられます

Python はスクリプト可能なアプリケーションを Python で作成することもサポートしていますが、
以下のモジュールは MacPython の AppleScript サポートに関係するモジュールのみです:

.. toctree::

   gensuitemodule.rst
   aetools.rst
   aepack.rst
   aetypes.rst
   miniaeframe.rst


他に、以下のサポートモジュールが事前に生成されています:
:mod:`Finder`, :mod:`Terminal`, :mod:`Explorer`, :mod:`Netscape`,
:mod:`CodeWarrior`, :mod:`SystemEvents`, :mod:`StdSuites` 。
