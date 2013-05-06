
:mod:`gensuitemodule` --- OSA スタブ作成パッケージ
==================================================

.. module:: gensuitemodule
   :platform: Mac
   :synopsis: OSA辞書からスタブパッケージを作成します。
.. sectionauthor:: Jack Jansen <Jack.Jansen@cwi.nl>
.. moduleauthor:: Jack Jansen

:mod:`gensuitemodule` モジュールは AppleScript 辞書によって特定のア\
プリケーションに実装されている AppleScript 群のためのスタブコードを実\
装した Python パッケージを作成します。

このモジュールは、通常は :program:`PythonIDE` からユーザによって起動さ\
れますが、コマンドラインからスクリプトとして実行する(オプションとして\
ヘルプに :option:`--help` を与えてみてください)こともできますし、Python\
コードでインポートして利用する事もできます。使用例として、どのようにし\
て標準ライブラリに含まれているスタブパッケージを生成するか、
:file:`Mac/scripts/genallsuites.py` にあるソースを見てください。

このモジュールは次の関数を定義しています。


.. function:: is_scriptable(application)

   ``application`` としてパス名を与えたアプリケーションがスクリプト可\
   能でありそうな場合、真を返します。返り値はやや不確実な場合があります。
   :program:`Internet Explorer` はスクリプト不可能なように見えてしまいます\
   が、実際はスクリプト可能です。


.. function:: processfile(application[, output, basepkgname,  edit_modnames, creatorsignature, dump, verbose])

   パス名として渡された ``application`` のためのスタブパッケージを作成\
   します。 :file:`.app` として一つのパッケージにまとめてあるプログラム群の\
   ために内部の実行プログラムそのものではなくパッケージへのパス名を渡すだ\
   けでよくなっています。パッケージ化されていないCFM アプリケーションでは\
   アプリケーションバイナリのファイル名を渡す事もできます。

   この関数は、アプリケーションの OSA 用語リソースを捜し、これらのリソー\
   スを読み取り、その結果データをクライアントスタブを実装したPython コー\
   ドパッケージを作成するために使用します。

   ``output`` は作成結果のパッケージを保存するパス名で、指定しない場合
   は標準の「別名で保存(save file as)」ダイアログが表示されます。
   ``basepkgname`` はこのパッケージの基盤となるパッケージを指定します。
   デフォルトは :mod:`StdSuites` になります。 :mod:`StdSuites` 自体を
   生成する場合だけ、このオプションを指定する必要があります。 ``edit_modnames``
   は自動生成によって作成されてあまり綺麗ではないモ
   ジュール名を変更するために使用することができる辞書です。 ``creator_signature``
   はパッケージ中の :file:`PkgInfo` ファイル、あ
   るいは CFM ファイルクリエータ署名から通常得られる4文字クリエータコード
   を無視するために使用することができます。 ``dump`` にはファイルオブジェ
   クトを与えます、これを指定するとリソースを読取った後に停止して
   ``processfile`` がコード化した用語リソースの Python 表現をダンプします。
   ``verbose`` にもまたファイルオブジェクトを与え、これを指定すると
   ``processfile`` の行なっている処理の詳細を出力します。


.. function:: processfile_fromresource(application[, output,  basepkgname, edit_modnames, creatorsignature, dump, verbose])

   この関数は、用語リソースを得るのに異なる方法を使用する以外は、
   ``processfile`` と同じです。この関数では、リソースファイルとして
   ``application`` を開き、このファイルから  ``"aete"`` および  ``"aeut"``
   リソースをすべて読み込む事で、AppleScript 用語リソース読み\
   込みを行ないます。
