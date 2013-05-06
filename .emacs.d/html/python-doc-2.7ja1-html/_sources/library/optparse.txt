:mod:`optparse` --- コマンドラインオプション解析器
============================================================

.. module:: optparse
   :synopsis: コマンドラインオプション解析機
   :deprecated:

.. deprecated:: 2.7
   :mod:`optparse` モジュールは廃止予定で、今後開発はされません。
   今後の開発は :mod:`argparse` モジュールに対して行われます。

.. moduleauthor:: Greg Ward <gward@python.net>

.. versionadded:: 2.3

.. sectionauthor:: Greg Ward <gward@python.net>


:mod:`optparse` モジュールは、昔からある :mod:`getopt` よりも簡便で、
柔軟性に富み、かつ強力なコマンドライン解析ライブラリです。
:mod:`optparse` では、より宣言的なスタイルのコマンドライン解析手法、すなわち
:class:`OptionParser` のインスタンスを作成してオプションを追加してゆき、その
インスタンスでコマンドラインを解析するという手法をとっています。
:mod:`optparse` を使うと、GNU/POSIX 構文でオプションを指定できるだけでなく、
使用法やヘルプメッセージの生成も行えます。

:mod:`optparse` を使った簡単なスクリプトの例を以下に示します。 ::

   from optparse import OptionParser

   [...]
   parser = OptionParser()
   parser.add_option("-f", "--file", dest="filename",
                     help="write report to FILE", metavar="FILE")
   parser.add_option("-q", "--quiet",
                     action="store_false", dest="verbose", default=True,
                     help="don't print status messages to stdout")

   (options, args) = parser.parse_args()

このようにわずかな行数のコードによって、スクリプトのユーザはコマンドライン上で
例えば以下のような「よくある使い方」を実行できるようになります。 ::

   <yourscript> --file=outfile -q

コマンドライン解析の中で、 :mod:`optparse` はユーザの指定した
コマンドライン引数値に応じて :meth:`parse_args` の返す
``options`` の属性値を設定してゆきます。 :meth:`parse_args` がコマンドライン解析から処理を戻したとき、
``options.filename`` は ``"outfile"`` に、 ``options.verbose`` は ``False``
になっているはずです。 :mod:`optparse` は長い形式と短い形式の両方のオプション表記をサポートしており、
短い形式は結合して指定できます。
また、様々な形でオプションに引数値を関連付けられます。
従って、以下のコマンドラインは全て上の例と同じ意味になります。 ::

   <yourscript> -f outfile --quiet
   <yourscript> --quiet --file outfile
   <yourscript> -q -foutfile
   <yourscript> -qfoutfile

さらに、ユーザが ::

   <yourscript> -h
   <yourscript> --help

のいずれかを実行すると、 :mod:`optparse` はスクリプトのオプションについて簡単にまとめた内容を出力します。

.. code-block:: text

   Usage: <yourscript> [options]

   Options:
     -h, --help            show this help message and exit
     -f FILE, --file=FILE  write report to FILE
     -q, --quiet           don't print status messages to stdout

*yourscript* の中身は実行時に決まります (通常は ``sys.argv[0]`` になります)。


.. _optparse-background:

背景
------

:mod:`optparse` は、素直で慣習に則ったコマンドラインインタフェースを備えたプログラムの作成を援助する目的で設計されました。その結果、Unix
で慣習的に使われているコマンドラインの構文や機能だけをサポートするに留まっています。こうした慣習に詳しくなければ、
よく知っておくためにもこの節を読んでおきましょう。


.. _optparse-terminology:

用語集
^^^^^^^^

引数 (argument)
   コマンドラインでユーザが入力するテキストの塊で、シェルが :c:func:`execl` や :c:func:`execv` に引き渡すものです。Python
   では、引数は ``sys.argv[1:]`` の要素となります。(``sys.argv[0]``
   は実行しようとしているプログラムの名前です。引数解析に関しては、この要素はあまり重要ではありません。) Unix シェルでは、「語 (word)」と
   いう用語も使います。

   場合によっては ``sys.argv[1:]`` 以外の引数リストを代入する方が望ましいことがあるので、「引数」は「 ``sys.argv[1:]``
   または ``sys.argv[1:]`` の代替として提供される別のリストの要素」と読むべきでしょう。

オプション (option)
   追加的な情報を与えるための引数で、プログラムの実行に対する教示やカスタマイズを行います。
   オプションには多様な文法が存在します。伝統的な Unix
   における書法はハイフン ("-") の後ろに一文字が続くもので、例えば ``-x`` や ``-F`` です。
   また、伝統的な Unix における書法では、複数のオプションを一つの引数にまとめられます。
   例えば ``-x -F`` は ``-xF`` と等価です。 GNU プロジェクトでは
   ``--`` の後ろにハイフンで区切りの語を指定する方法、例えば ``--file`` や
   ``--dry-run`` も提供しています。
   :mod:`optparse` は、これら二種類のオプション書法だけをサポートしています。

   他に見られる他のオプション書法には以下のようなものがあります:

   * ハイフンの後ろに数個の文字が続くもので、例えば ``-pf``
     (このオプションは複数のオプションを一つにまとめたものとは *違います*)

   * ハイフンの後ろに語が続くもので、例えば ``-file``
     (これは技術的には上の書式と同じですが、通常同じプログラム上で一緒に
     使うことはありません)

   * プラス記号の後ろに一文字、数個の文字、または語を続けたもので、例えば
     ``+f``, ``+rgb``

   * スラッシュ記号の後ろに一文字、数個の文字、または語を続けたもので、例えば
     ``/f``, ``/file``

   上記のオプション書法は :mod:`optparse` ではサポートしておらず、今後もサポートする予定はありません。これは故意によるものです:
   最初の三つはどの環境の標準でもなく、最後の一つは VMS や MS-DOS, そして Windows を対象にしているときにしか意味をなさないからです。

オプション引数 (option argument)
   あるオプションの後ろに続く引数で、そのオプションに密接な関連をもち、オプションと同時に引数リストから取り出されます。 :mod:`optparse`
   では、オプション引数は以下のように別々の引数にできます。

   .. code-block:: text

      -f foo
      --file foo

   また、一つの引数中にも入れられます。

   .. code-block:: text

      -ffoo
      --file=foo

   通常、オプションは引数をとることもとらないこともあります。あるオプションは引数をとることがなく、
   またあるオプションは常に引数をとります。
   多くの人々が「オプションのオプション引数」機能を欲しています。
   これは、あるオプションが引数が指定されている場合には引数をとり、そうでない場合には引数を
   もたないようにするという機能です。この機能は引数解析をあいまいにするため、議論の的となっています: 例えば、もし
   ``-a`` がオプション引数をとり、 ``-b`` がまったく別のオプションだとしたら、
   ``-ab`` をどうやって解析すればいいのでしょうか？
   こうした曖昧さが存在するため、 :mod:`optparse` は今のところこの機能をサポートしていません。

固定引数 (positional argument)
   他のオプションが解析される、すなわち他のオプションとその引数が解析されて引数リストから除去された後に引数リストに置かれているものです。

必須のオプション (required option)
   コマンドラインで与えなければならないオプションです; 「必須なオプション (required
   option)」という語は、英語では矛盾した言葉です。 :mod:`optparse`
   では必須オプションの実装を妨げてはいませんが、とりたてて実装上役立つこともしていません。

例えば、下記のような架空のコマンドラインを考えてみましょう::

   prog -v --report /tmp/report.txt foo bar

``-v`` と ``--report`` はどちらもオプションです。 ``--report`` オプションが引数をとるとすれば、
``/tmp/report.txt`` はオプションの引数です。
``foo`` と ``bar`` は固定引数になります。


.. _optparse-what-options-for:

オプションとは何か
^^^^^^^^^^^^^^^^^^

オプションはプログラムの実行を調整したり、カスタマイズしたりするための補助的な
情報を与えるために使います。もっとはっきりいうと、オプションはあくまでもオプション
(省略可能)であるということです。本来、プログラムはともかくもオプションなしでうまく実行できてしかるべきです。(Unix やGNU
ツールセットのプログラムをランダムにピックアップしてみてください。オプションを全く指定しなくてもちゃんと動くでしょう？例外は ``find``,
``tar``, ``dd`` くらいです---これらの例外は、オプション文法が標準的でなく、インタフェースが混乱を招くと酷評されてきた変種の
はみ出しものなのです)

多くの人が自分のプログラムに「必須のオプション」を持たせたいと考えます。しかしよく考えてください。必須なら、それは *オプション(省略可能) ではないのです！*
プログラムを正しく動作させるのに絶対的に必要な情報があるとすれば、そこには固定引数を割り当てるべきなのです。

良くできたコマンドラインインタフェース設計として、ファイルのコピーに使われる ``cp`` ユーティリティのことを考えてみましょう。ファイルのコピーでは、
コピー先を指定せずにファイルをコピーするのは無意味な操作ですし、少なくとも一つのコピー元が必要です。従って、 ``cp`` は引数無しで実行すると失敗します。
とはいえ、 ``cp`` はオプションを全く必要としない柔軟で便利なコマンドライン文法を備えています::

   cp SOURCE DEST
   cp SOURCE ... DEST-DIR

まだあります。ほとんどの ``cp`` の実装では、ファイルモードや変更時刻を変えずに
コピーする、シンボリックリンクの追跡を行わない、すでにあるファイルを上書きする前に
ユーザに尋ねる、など、ファイルをコピーする方法をいじるための一連のオプションを実装
しています。しかし、こうしたオプションは、一つのファイルを別の場所にコピーする、または複数のファイルを別のディレクトリにコピーするという、 ``cp``
の中心的な処理を乱すことはないのです。


.. _optparse-what-positional-arguments-for:

固定引数とは何か
^^^^^^^^^^^^^^^^

固定引数とは、プログラムを動作させる上で絶対的に必要な情報となる引数です。

よいユーザインタフェースとは、可能な限り少ない固定引数をもつものです。プログラムを正しく動作させるために 17 個もの別個の情報が必要だとしたら、
その *方法* はさして問題にはなりません ---ユーザはプログラムを正しく動作させられないうちに諦め、立ち去ってしまうからです。
ユーザインタフェースがコマンドラインでも、設定ファイルでも、GUI やその他の何であっても同じです: 多くの要求をユーザに押し付ければ、ほとんどのユーザはただ
音をあげてしまうだけなのです。

要するに、ユーザが絶対に提供しなければならない情報だけに制限する --- そして可能な限りよく練られたデフォルト設定を使うよう試みてください。
もちろん、プログラムには適度な柔軟性を持たせたいとも望むはずですが、それこそがオプションの果たす役割です。繰り返しますが、設定ファイルのエントリであろうが、
GUI でできた「環境設定」ダイアログ上のウィジェットであろうが、コマンドラインオプションであろうが関係ありません ---
より多くのオプションを実装すればプログラムはより柔軟性を持ちますが、実装はより難解になるのです。高すぎる柔軟性はユーザを閉口させ、コードの維持を
より難しくするのです。


.. _optparse-tutorial:

チュートリアル
----------------

:mod:`optparse` はとても柔軟で強力でありながら、ほとんどの場合には簡単に利用できます。この節では、 :mod:`optparse`
ベースのプログラムで広く使われているコードパターンについて述べます。

まず、 :class:`OptionParser` クラスを import しておかねばなりません。次に、プログラムの冒頭で
:class:`OptionParser` インスタンスを生成しておきます::

   from optparse import OptionParser
   [...]
   parser = OptionParser()

これでオプションを定義できるようになりました。基本的な構文は以下の通りです::

   parser.add_option(opt_str, ...,
                     attr=value, ...)

各オプションには、 ``-f`` や ``--file`` のような一つまたは複数の
オプション文字列と、パーザがコマンドライン上のオプションを見つけた際に、何を準備し、何を行うべきかを :mod:`optparse`
に教えるためのオプション属性 (option attribute)がいくつか入ります。

通常、各オプションには短いオプション文字列と長いオプション文字列があります。例えば::

   parser.add_option("-f", "--file", ...)

といった具合です。

オプション文字列は、(ゼロ文字の場合も含め)いくらでも短く、またいくらでも長くできます。ただしオプション文字列は少なくとも一つなければなりません。

:meth:`add_option` に渡されたオプション文字列は、実際にはこの関数で定義したオプションに対するラベルになります。簡単のため、以後では
コマンドライン上で *オプションを見つける* という表現をしばしば使いますが、これは実際には :mod:`optparse`
がコマンドライン上の *オプション文字列* を見つけ、対応づけされているオプションを捜し出す、という処理に相当します。

オプションを全て定義したら、 :mod:`optparse` にコマンドラインを解析するように指示します::

   (options, args) = parser.parse_args()

(お望みなら、 :meth:`parse_args` に自作の引数リストを渡してもかまいません。とはいえ、実際にはそうした必要はほとんどないでしょう:
:mod:`optionparser` はデフォルトで ``sys.argv[1:]`` を使うからです。)

:meth:`parse_args` は二つの値を返します:

* 全てのオプションに対する値の入ったオブジェクト ``options`` --- 例えば、 ``--file``
  が単一の文字列引数をとる場合、 ``options.file`` はユーザが指定したファイル名になります。オプションを指定しなかった場合には ``None``
  になります。

* オプションの解析後に残った固定引数からなるリスト ``args`` 。

このチュートリアルの節では、最も重要な四つのオプション属性:
:attr:`~Option.action`, :attr:`~Option.type`, :attr:`~Option.dest`
(destination), :attr:`~Option.help` についてしか触れません。
このうち最も重要なのは :attr:`~Option.action` です。


.. _optparse-understanding-option-actions:

オプション・アクションを理解する
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

アクション(action)は :mod:`optparse` がコマンドライン上にあるオプションを
見つけたときに何をすべきかを指示します。 :mod:`optparse` には押し着せのアクションのセットがハードコードされています。
新たなアクションの追加は上級者向けの話題であり、 :ref:`optparse-extending-optparse` で触れます。
ほとんどのアクションは、値を何らかの変数に記憶するよう :mod:`optparse` に指示します ---
例えば、文字列をコマンドラインから取り出して、 ``options`` の属性の中に入れる、といった具合にです。

オプション・アクションを指定しない場合、 :mod:`optparse` のデフォルトの動作は ``store`` になります。


.. _optparse-store-action:

store アクション
^^^^^^^^^^^^^^^^

もっとも良く使われるアクションは ``store`` です。このアクションは次の引数 (あるいは現在の引数の残りの部分) を取り出し、正しい型の値か確かめ、
指定した保存先に保存するよう :mod:`optparse` に指示します。

例えば::

   parser.add_option("-f", "--file",
                     action="store", type="string", dest="filename")

のように指定しておき、偽のコマンドラインを作成して :mod:`optparse` に解析させてみましょう::

   args = ["-f", "foo.txt"]
   (options, args) = parser.parse_args(args)

オプション文字列 ``-f`` を見つけると、 :mod:`optparse` は次の引数である ``foo.txt`` を消費し、その値を
``options.filename`` に保存します。従って、この :meth:`parse_args` 呼び出し後には
``options.filename`` は ``"foo.txt"`` になっています。

オプションの型として、 :mod:`optparse` は他にも ``int`` や ``float`` をサポートしています。

整数の引数を想定したオプションの例を示します::

   parser.add_option("-n", type="int", dest="num")

このオプションには長い形式のオプション文字列がないため、設定に問題がないということに注意してください。また、デフォルトのアクションは ``store``
なので、ここでは action を明示的に指定していません。

架空のコマンドラインをもう一つ解析してみましょう。今度は、オプション引数を
オプションの右側にぴったりくっつけて一緒くたにします: ``-n42``
(一つの引数のみ) は ``-n 42`` (二つの引数からなる) と等価になるので、 ::

   (options, args) = parser.parse_args(["-n42"])
   print options.num

は ``42`` を出力します。

型を指定しない場合、 :mod:`optparse` は引数を ``string`` であると仮定します。デフォルトのアクションが ``store``
であることも併せて考えると、最初の例はもっと短くなります::

   parser.add_option("-f", "--file", dest="filename")

保存先 (destination) を指定しない場合、 :mod:`optparse` はデフォルト値としてオプション文字列から気のきいた名前を設定します:
最初に指定した長い形式のオプション文字列が ``--foo-bar`` であれば、デフォルトの保存先は ``foo_bar``
になります。長い形式のオプション文字列がなければ、 :mod:`optparse` は最初に指定した短い形式のオプション文字列を探します:
例えば、 ``-f`` に対する保存先は ``f`` になります。

:mod:`optparse` では、 ``long`` や ``complex`` といった組み込み型も取り入れています。型の追加は
:ref:`optparse-extending-optparse` で触れています。


.. _optparse-handling-boolean-options:

ブール値 (フラグ) オプションの処理
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

フラグオプション---特定のオプションに対して真または偽の値の値を設定するオプション--- はよく使われます。 :mod:`optparse`
では、二つのアクション、 ``store_true`` および ``store_false`` をサポートしています。例えば、 ``verbose``
というフラグを ``-v`` で有効にして、 ``-q`` で無効にしたいとします::

   parser.add_option("-v", action="store_true", dest="verbose")
   parser.add_option("-q", action="store_false", dest="verbose")

ここでは二つのオプションに同じ保存先を指定していますが、全く問題ありません (下記のように、デフォルト値の設定を少し注意深く行わねばならないだけです)

``-v`` をコマンドライン上に見つけると、 :mod:`optparse` は ``options.verbose`` を ``True``
に設定します。 ``-q`` を見つければ、 ``options.verbose`` は ``False`` にセットされます。


.. _optparse-other-actions:

その他のアクション
^^^^^^^^^^^^^^^^^^

この他にも、 :mod:`optparse` は以下のようなアクションをサポートしています:

``"store_const"``
   定数値を保存します。

``"append"``
   オプションの引数を指定のリストに追加します。

``"count"``
   指定のカウンタを 1 増やします。

``"callback"``
   指定の関数を呼び出します。

これらのアクションについては、 :ref:`optparse-reference-guide` 節の「リファレンスガイド」および
:ref:`optparse-option-callbacks` 節で触れます。


.. _optparse-default-values:

デフォルト値
^^^^^^^^^^^^

上記の例は全て、何らかのコマンドラインオプションが見つかった時に何らかの変数 (保存先: destination) に値を設定していました。
では、該当するオプションが見つからなかった場合には何が起きるのでしょうか？デフォルトは全く与えていないため、これらの値は全て ``None`` になります。
たいていはこれで十分ですが、もっときちんと制御したい場合もあります。 :mod:`optparse` では各保存先に対してデフォルト値を指定し、コマンドライン
の解析前にデフォルト値が設定されるようにできます。

まず、 verbose/quiet の例について考えてみましょう。 :mod:`optparse` に対して、 ``-q`` がない限り
``verbose`` を ``True`` に設定させたいなら、以下のようにします::

   parser.add_option("-v", action="store_true", dest="verbose", default=True)
   parser.add_option("-q", action="store_false", dest="verbose")

デフォルトの値は特定のオプションではなく *保存先* に対して適用されます。また、これら二つのオプションはたまたま同じ保存先を持っているにすぎないため、
上のコードは下のコードと全く等価になります::

   parser.add_option("-v", action="store_true", dest="verbose")
   parser.add_option("-q", action="store_false", dest="verbose", default=True)

下のような場合を考えてみましょう::

   parser.add_option("-v", action="store_true", dest="verbose", default=False)
   parser.add_option("-q", action="store_false", dest="verbose", default=True)

やはり ``verbose`` のデフォルト値は ``True`` になります; 特定の目的変数に対するデフォルト値として有効なのは、最後に指定した値だからです。

デフォルト値をすっきりと指定するには、 :class:`OptionParser` の :meth:`set_defaults`
メソッドを使います。このメソッドは :meth:`parse_args` を呼び出す前ならいつでも使えます::

   parser.set_defaults(verbose=True)
   parser.add_option(...)
   (options, args) = parser.parse_args()

前の例と同様、あるオプションの値の保存先に対するデフォルトの値は最後に指定した
値になります。コードを読みやすくするため、デフォルト値を設定するときには両方のやり方を混ぜるのではなく、片方だけを使うようにしましょう。


.. _optparse-generating-help:

ヘルプの生成
^^^^^^^^^^^^

:mod:`optparse` にはヘルプと使い方の説明 (usage text) を生成する機能があり、
ユーザに優しいコマンドラインインタフェースを作成する上で役立ちます。
やらなければならないのは、各オプションに対する :attr:`~Option.help` の値と、
必要ならプログラム全体の使用法を説明する短いメッセージを与えることだけです。

ユーザフレンドリな (ドキュメント付きの) オプションを追加した :class:`OptionParser` を以下に示します::

   usage = "usage: %prog [options] arg1 arg2"
   parser = OptionParser(usage=usage)
   parser.add_option("-v", "--verbose",
                     action="store_true", dest="verbose", default=True,
                     help="make lots of noise [default]")
   parser.add_option("-q", "--quiet",
                     action="store_false", dest="verbose",
                     help="be vewwy quiet (I'm hunting wabbits)")
   parser.add_option("-f", "--filename",
                     metavar="FILE", help="write output to FILE")
   parser.add_option("-m", "--mode",
                     default="intermediate",
                     help="interaction mode: novice, intermediate, "
                          "or expert [default: %default]")

:mod:`optparse` がコマンドライン上で ``-h`` や ``--help`` を
見つけた場合や、 :meth:`parser.print_help` を呼び出した場合、この :class:`OptionParser`
は以下のようなメッセージを標準出力に出力します。

.. code-block:: text

   Usage: <yourscript> [options] arg1 arg2

   Options:
     -h, --help            show this help message and exit
     -v, --verbose         make lots of noise [default]
     -q, --quiet           be vewwy quiet (I'm hunting wabbits)
     -f FILE, --filename=FILE
                           write output to FILE
     -m MODE, --mode=MODE  interaction mode: novice, intermediate, or
                           expert [default: intermediate]

(help オプションでヘルプを出力した場合、 :mod:`optparse` は出力後にプログラムを終了します。)

:mod:`optparse` ができるだけうまくメッセージを生成するよう手助けするには、他にもまだまだやるべきことがあります:

* スクリプト自体の利用法を表すメッセージを定義します::

     usage = "usage: %prog [options] arg1 arg2"

  :mod:`optparse` は ``%prog`` を現在のプログラム名、すなわち ``os.path.basename(sys.argv[0])``
  と置き換えます。この文字列は詳細なオプションヘルプの前に展開され出力されます。

  usage の文字列を指定しない場合、 :mod:`optparse` は型どおりとはいえ気の利いたデフォルト値、
  ``"Usage: %prog [options]"`` を使います。固定引数をとらないスクリプトの場合はこれで十分でしょう。

* 全てのオプションにヘルプ文字列を定義します。行の折り返しは気にしなくてかまいません --- :mod:`optparse`
  は行の折り返しに気を配り、見栄えのよいヘルプ出力を生成します。

* オプションが値をとるということは自動的に生成されるヘルプメッセージの中で分かります。例えば、"mode" option の場合には::

     -m MODE, --mode=MODE

  のようになります。

  ここで "MODE" はメタ変数 (meta-variable) と呼ばれます: メタ変数は、ユーザが
  ``-m``/``--mode`` に対して指定するはずの引数を表します。デフォルトでは、 :mod:`optparse`
  は保存先の変数名を大文字だけにしたものをメタ変数に使います。これは時として期待通りの結果になりません ---
  例えば、上の例の ``--filename`` オプションでは明示的に ``metavar="FILE"`` を設定しており、その結果自動生成された
  オプション説明テキストは::

     -f FILE, --filename=FILE

  のようになります。

  この機能の重要さは、単に表示スペースを節約するといった理由にとどまりません:
  上の例では、手作業で書いたヘルプテキストの中でメタ変数として ``FILE`` を
  使っています。その結果、ユーザに対してやや堅苦しい表現の書法 ``-f FILE`` と、
  より平易に意味付けを説明した "write output to FILE"
  との間に対応があるというヒントを与えています。これは、エンドユーザにとってより明解で便利なヘルプテキストを作成する単純でありながら効果的な手法なのです。

.. versionadded:: 2.4
   デフォルト値を持つオプションのヘルプ文字列には ``%default`` を入れられます --- :mod:`optparse`
   は ``%default`` をデフォルト値の :func:`str` で置き換えます。該当するオプションにデフォルト値がない場合 (あるいはデフォルト値が
   ``None`` である場合) ``%default`` の展開結果は ``none`` になります。

.. Grouping Options

オプションをグループ化する
+++++++++++++++++++++++++++

たくさんのオプションを扱う場合、オプションをグループ分けするとヘルプ出力が\
見やすくなります。 :class:`OptionParser` は、複数のオプションをまとめた\
オプショングループを複数持つことができます。

オプションのグループは、 :class:`OptionGroup` を使って作成します:

.. class:: OptionGroup(parser, title, description=None)

   * *parser* は、このグループが属する  :class:`OptionParser` のインスタンスです
   * *title* はグループのタイトルです
   * *description* はオプションで、グループの長い説明です

:class:`OptionGroup` は (:class:`OptionParser` のように) :class:`OptionContainer`
を継承していて、オプションをグループに追加するために :meth:`add_option`
メソッドを利用できます。

全てのオプションを定義したら、 :class:`OptionParser` の :meth:`add_option_group`
メソッドを使ってグループを定義済みのパーサーに追加します。

前のセクションで定義したパーサーに、続けて :class:`OptionGroup` を
追加します::

    group = OptionGroup(parser, "Dangerous Options",
                        "Caution: use these options at your own risk.  "
                        "It is believed that some of them bite.")
    group.add_option("-g", action="store_true", help="Group option.")
    parser.add_option_group(group)

.. This would result in the following help output::

この結果のヘルプ出力は次のようになります。

.. code-block:: text

   Usage: <yourscript> [options] arg1 arg2

   Options:
     -h, --help            show this help message and exit
     -v, --verbose         make lots of noise [default]
     -q, --quiet           be vewwy quiet (I'm hunting wabbits)
     -f FILE, --filename=FILE
                           write output to FILE
     -m MODE, --mode=MODE  interaction mode: novice, intermediate, or
                           expert [default: intermediate]

     Dangerous Options:
       Caution: use these options at your own risk.  It is believed that some
       of them bite.

       -g                  Group option.

さらにサンプルを拡張して、複数のグループを使うようにしてみます::

    group = OptionGroup(parser, "Dangerous Options",
                        "Caution: use these options at your own risk.  "
                        "It is believed that some of them bite.")
    group.add_option("-g", action="store_true", help="Group option.")
    parser.add_option_group(group)

    group = OptionGroup(parser, "Debug Options")
    group.add_option("-d", "--debug", action="store_true",
                     help="Print debug information")
    group.add_option("-s", "--sql", action="store_true",
                     help="Print all SQL statements executed")
    group.add_option("-e", action="store_true", help="Print every action done")
    parser.add_option_group(group)

出力結果は次のようになります:

.. code-block:: text

   Usage: <yourscript> [options] arg1 arg2

   Options:
     -h, --help            show this help message and exit
     -v, --verbose         make lots of noise [default]
     -q, --quiet           be vewwy quiet (I'm hunting wabbits)
     -f FILE, --filename=FILE
                           write output to FILE
     -m MODE, --mode=MODE  interaction mode: novice, intermediate, or expert
                           [default: intermediate]

     Dangerous Options:
       Caution: use these options at your own risk.  It is believed that some
       of them bite.

       -g                  Group option.

     Debug Options:
       -d, --debug         Print debug information
       -s, --sql           Print all SQL statements executed
       -e                  Print every action done

もう1つの、特にオプショングループをプログラムから操作するときに利用できる
メソッドがあります:

.. method:: OptionParser.get_option_group(opt_str)

   *opt_str* と同じ title か long description を持っている :class:`OptionGroup`
   があれば返します。

.. _optparse-printing-version-string:

バージョン番号の出力
^^^^^^^^^^^^^^^^^^^^

:mod:`optparse` では、使用法メッセージと同様にプログラムのバージョン文字列を出力できます。 :class:`OptionParser`
の ``version`` 引数に文字列を渡します::

   parser = OptionParser(usage="%prog [-f] [-q]", version="%prog 1.0")

``%prog`` は *usage* と同じような展開を受けます。その他にも ``version`` には何でも好きな内容を入れられます。
``version`` を指定した場合、 :mod:`optparse` は自動的に ``--version`` オプションをパーザに渡します。
コマンドライン中に ``--version`` が見つかると、 :mod:`optparse` は ``version`` 文字列を展開して
(``%prog`` を置き換えて) 標準出力に出力し、プログラムを終了します。

例えば、 ``/usr/bin/foo`` という名前のスクリプトなら::

   $ /usr/bin/foo --version
   foo 1.0

のようになります。

.. The following two methods can be used to print and get the ``version`` string:

以下の2つのメソッドを、 ``version`` 文字列を表示するために利用できます。

.. method:: OptionParser.print_version(file=None)

   現在のプログラムのバージョン (``self.version``) を *file* (デフォルト: stdout)
   へ表示します。 :meth:`print_usage` と同じく、 ``self.version`` の中の全ての
   ``%prog`` が現在のプログラム名に置き換えられます。
   ``self.version`` が空文字列だだったり未定義だったときは何もしません。

.. method:: OptionParser.get_version()

   .. Same as :meth:`print_version` but returns the version string instead of
      printing it.

   :meth:`print_version` と同じですが、バージョン文字列を表示する代わりに
   返します。


.. _optparse-how-optparse-handles-errors:

:mod:`optparse` のエラー処理法
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:mod:`optparse` を使う場合に気を付けねばならないエラーには、大きく分けてプログラマ側のエラーとユーザ側のエラーという二つの種類があります。
プログラマ側のエラーの多くは、例えば不正なオプション文字列や定義されていないオプション属性の指定、あるいはオプション属性を指定し忘れるといった、
誤った ``OptionParser.add_option()`` 呼び出しによるものです。
こうした誤りは通常通りに処理されます。すなわち、例外(:exc:`optparse.OptionError` や :exc:`TypeError`)
を送出して、プログラムをクラッシュさせます。

もっと重要なのはユーザ側のエラーの処理です。というのも、ユーザの操作エラーという\
ものはコードの安定性に関係なく起こるからです。 :mod:`optparse` は、誤ったオプション引数の指定 (整数を引数にとるオプション
``-n`` に対して ``-n 4x`` と指定してしまうなど) や、引数を指定し忘れた場合 (``-n``
が何らかの引数をとるオプションであるのに、 ``-n`` が引数の末尾に来ている場合) といった、ユーザによるエラーを自動的に\
検出します。また、アプリケーション側で定義されたエラー条件が起きた場合、
:func:`OptionParser.error` を呼び出してエラーを通知できます::

   (options, args) = parser.parse_args()
   [...]
   if options.a and options.b:
       parser.error("options -a and -b are mutually exclusive")

いずれの場合にも :mod:`optparse` はエラーを同じやり方で処理します。すなわち、
プログラムの使用法メッセージとエラーメッセージを標準エラー出力に出力して、終了ステータス 2 でプログラムを終了させます。

上に挙げた最初の例、すなわち整数を引数にとるオプションにユーザが ``4x`` を指定した場合を考えてみましょう::

   $ /usr/bin/foo -n 4x
   Usage: foo [options]

   foo: error: option -n: invalid integer value: '4x'

値を全く指定しない場合には、以下のようになります::

   $ /usr/bin/foo -n
   Usage: foo [options]

   foo: error: -n option requires an argument

:mod:`optparse` は、常にエラーを引き起こしたオプションについて説明の入ったエラーメッセージを生成するよう気を配ります;
従って、 :func:`OptionParser.error` をアプリケーションコードから呼び出す場合にも、同じようなメッセージになるようにしてください。

:mod:`optparse` のデフォルトのエラー処理動作が気に入らないのなら、 :class:`OptionParser`
をサブクラス化して、 :meth:`~OptionParser.exit` かつ/または
:meth:`~OptionParser.error` をオーバライドする必要があります。


.. _optparse-putting-it-all-together:

全てをつなぎ合わせる
^^^^^^^^^^^^^^^^^^^^

:mod:`optparse` を使ったスクリプトは、通常以下のようになります::

   from optparse import OptionParser
   [...]
   def main():
       usage = "usage: %prog [options] arg"
       parser = OptionParser(usage)
       parser.add_option("-f", "--file", dest="filename",
                         help="read data from FILENAME")
       parser.add_option("-v", "--verbose",
                         action="store_true", dest="verbose")
       parser.add_option("-q", "--quiet",
                         action="store_false", dest="verbose")
       [...]
       (options, args) = parser.parse_args()
       if len(args) != 1:
           parser.error("incorrect number of arguments")
       if options.verbose:
           print "reading %s..." % options.filename
       [...]

   if __name__ == "__main__":
       main()


.. _optparse-reference-guide:

リファレンスガイド
------------------


.. _optparse-creating-parser:

.. Creating the parser

parserを作る
^^^^^^^^^^^^^^^^^^^

:mod:`optparse` を使う最初の一歩は OptionParser インスタンスを作ることです。

.. class:: OptionParser(...)

OptionParser のコンストラクタの引数はどれも必須ではありませんが、いくつものキーワード引数がオプションとして使えます。これらはキーワード引数と\
して渡さなければなりません。すなわち、引数が宣言されている順番に頼ってはいけません。

   ``usage`` (デフォルト: ``"%prog [options]"``)
      プログラムが間違った方法で実行されるかまたはヘルプオプションを付けて実行された場合に表示される使用法です。 :mod:`optparse` は使用法の文\
      字列を表示する際に ``%prog`` を ``os.path.basename(sys.argv[0])`` (または ``prog``
      キーワード引数が指定されていればその値) に展開します。使用法メッセージを抑制するためには特別な
      :data:`optparse.SUPPRESS_USAGE` という値を指定します。

   ``option_list`` (デフォルト: ``[]``)
      パーザに追加する Option オブジェクトのリストです。 ``option_list`` の中のオプションは ``standard_option_list``
      (OptionParser のサブクラスでセットされる可能性のあるクラス属性) の後に追加されますが、バージョンやヘルプのオプションよりは前になります。
      このオプションの使用は推奨されません。パーザを作成した後で、 :meth:`add_option` を使って追加してください。

   ``option_class`` (デフォルト: optparse.Option)
      :meth:`add_option` でパーザにオプションを追加するときに使用されるクラス。

   ``version`` (デフォルト: ``None``)
      ユーザがバージョンオプションを与えたときに表示されるバージョン文字列です。 ``version`` に真の値を与えると、 :mod:`optparse`
      は自動的に単独のオプション文字列 ``--version`` とともにバージョンオプションを追加します。部分文字列 ``%prog`` は
      ``usage`` と同様に展開されます。

   ``conflict_handler`` (デフォルト: ``"error"``)
      オプション文字列が衝突するようなオプションがパーザに追加されたときにどうするかを指定します。
      :ref:`optparse-conflicts-between-options` 節を参照して下さい。

   ``description`` (デフォルト: ``None``)
      プログラムの概要を表す一段落のテキストです。 :mod:`optparse` はユーザがヘルプを要求したときにこの概要を現在のターミナルの幅に合わせて\
      整形し直して表示します (``usage`` の後、オプションリストの前に表示されます)。

   ``formatter`` (デフォルト: 新しい :class:`IndentedHelpFormatter`)
      ヘルプテキストを表示する際に使われる optparse.HelpFormatter のインスタンスです。 :mod:`optparse`
      はこの目的のためにすぐ使えるクラスを二つ提供しています。 IndentedHelpFormatter と TitledHelpFormatter がそれです。

   ``add_help_option`` (デフォルト: ``True``)
      もし真ならば、 :mod:`optparse` はパーザにヘルプオプションを (オプション文字列
      ``-h`` と ``--help`` とともに)追加します。

   ``prog``
      ``usage`` や ``version`` の中の ``%prog`` を展開するときに
      ``os.path.basename(sys.argv[0])`` の代わりに使われる文字列です。

   ``epilog`` (default: ``None``)

      .. A paragraph of help text to print after the option help.

      オプションのヘルプの後に表示されるヘルプテキスト.


.. _optparse-populating-parser:

パーザへのオプション追加
^^^^^^^^^^^^^^^^^^^^^^^^

パーザにオプションを加えていくにはいくつか方法があります。
推奨するのは :ref:`optparse-tutorial` 節で示したような
meth:``OptionParser.add_option()`` を使う方法です。
:meth:`add_option` は以下の二つのうちいずれかの方法で呼び出せます。

* (:func:`make_option` などが返す) :class:`Option` インスタンスを渡します。

* :func:`make_option` に (すなわち :class:`Option` のコンストラクタに)
  固定引数とキーワード引数の組み合わせを渡して、 :class:`Option` インスタンスを生成させます。

もう一つの方法は、あらかじめ作成しておいた :class:`Option` インスタンスからなるリストを、以下のようにして
:class:`OptionParser` のコンストラクタに渡すというものです::

   option_list = [
       make_option("-f", "--filename",
                   action="store", type="string", dest="filename"),
       make_option("-q", "--quiet",
                   action="store_false", dest="verbose"),
       ]
   parser = OptionParser(option_list=option_list)

(:func:`make_option` は :class:`Option` インスタンスを生成するファクトリ関数です;
現在のところ、この関数は :class:`Option` のコンストラクタの\
別名にすぎません。 :mod:`optparse` の将来のバージョンでは、 :class:`Option` を\
複数のクラスに分割し、 :func:`make_option` は適切なクラスを選んで\
インスタンスを生成するようになる予定です。従って、 :class:`Option` を直接インスタンス化しないでください。)


.. _optparse-defining-options:

オプションの定義
^^^^^^^^^^^^^^^^

各々の :class:`Option` インスタンス、は ``-f`` や ``--file``
といった同義のコマンドラインオプションからなる集合を表現しています。
一つの :class:`Option` には任意の数のオプションを短い形式でも長い形式でも指定できます。
ただし、少なくとも一つは指定せねばなりません。

正しい方法で :class:`Option` インスタンスを生成するには、 :class:`OptionParser`
の :meth:`add_option` を使います。

.. method:: OptionParser.add_option(opt_str[, ...], attr=value, ...)

   短い形式のオプション文字列を一つだけ持つようなオプションを生成するには
   次のようにします。 ::

      parser.add_option("-f", attr=value, ...)


   また、長い形式のオプション文字列を一つだけ持つようなオプションの定義は
   次のようになります。 ::

      parser.add_option("--foo", attr=value, ...)

   .. The keyword arguments define attributes of the new Option object.  The most
      important option attribute is :attr:`~Option.action`, and it largely
      determines which other attributes are relevant or required.  If you pass
      irrelevant option attributes, or fail to pass required ones, :mod:`optparse`
      raises an :exc:`OptionError` exception explaining your mistake.

   キーワード引数は新しい :class:`Option` オブジェクトの属性を定義します。
   オプションの属性のうちでもっとも重要なのは :attr:`~Option.action` です。
   この属性は、他のどの属性と関連があるか、そしてどの属性が必要かに大きく
   作用します。関係のないオプション属性を指定したり、必要な属性を指定し忘れたり
   すると、 :mod:`optparse` は誤りを解説した :exc:`OptionError` 例外を送出します。

   コマンドライン上にあるオプションが見つかったときの :mod:`optparse` の振舞いを
   決定しているのは *アクション(action)* です。 :mod:`optparse` でハードコード
   されている標準的なアクションには以下のようなものがあります:

   ``"store"``
      オプションの引数を保存します (デフォルトの動作です)

   ``"store_const"``
      定数を保存します

   ``"store_true"``
      真 (:const:`True`) を保存します

   ``"store_false"``
      偽 (:const:`False`) を保存します

   ``"append"``
      オプションの引数をリストに追加します

   ``"append_const"``
      定数をリストに追加します

   ``"count"``
      カウンタを一つ増やします

   ``"callback"``
      指定された関数を呼び出します

   ``"help"``
      全てのオプションとそのドキュメントの入った使用法メッセージを出力します。

   (アクションを指定しない場合、デフォルトは ``"store"`` になります。
   このアクションでは、 :attr:`~Option.type` および :attr:`~Option.dest`
   オプション属性を指定できます。
   :ref:`optparse-standard-option-actions` を参照してください。)

すでにお分かりのように、ほとんどのアクションはどこかに値を保存したり、
値を更新したりします。この目的のために、 :mod:`optparse`
は常に特別なオブジェクトを作り出し、それは通常 ``options`` と呼ばれます
(:class:`optparse.Values` のインスタンスになっています)。オプションの引数
(や、その他の様々な値) は、 :attr:`~Option.dest` (保存先:  destination)
オプション属性に従って、 *options* の属性として保存されます。

例えば、 ::

   parser.parse_args()

を呼び出した場合、 :mod:`optparse` はまず ``options`` オブジェクトを生成します::

   options = Values()

パーザ中で以下のようなオプション ::

   parser.add_option("-f", "--file", action="store", type="string", dest="filename")

が定義されていて、パーズしたコマンドラインに以下のいずれかが入っていた場合::

   -ffoo
   -f foo
   --file=foo
   --file foo

:mod:`optparse` はこのオプションを見つけて、 ::

   options.filename = "foo"

と同等の処理を行います。

:attr:`~Option.type` および :attr:`~Option.dest` オプション属性は
:attr:`~Option.action` と同じくらい重要ですが、 *全ての*
オプションで意味をなすのは :attr:`~Option.action` だけなのです。


.. _optparse-option-attributes:

オプション属性
^^^^^^^^^^^^^^

以下のオプション属性は :meth:`parser.add_option` へのキーワード引数として渡す
ことができます。特定のオプションに無関係なオプション属性を渡した場合、または
必須のオプションを渡しそこなった場合、 :mod:`optparse` は :exc:`OptionError`
を送出します。

.. attribute:: Option.action

   (デフォルト: ``"store"``)

   このオプションがコマンドラインにあった場合に :mod:`optparse` に何を
   させるかを決めます。
   取りうるオプションについては
   :ref:`こちら <optparse-standard-option-actions>` を参照してください。

.. attribute:: Option.type

   (デフォルト: ``"string"``)

   このオプションに与えられる引数の型 (たとえば ``"string"`` や ``"int"``)
   です。
   取りうるオプションについては
   :ref:`こちら <optparse-standard-option-types>` を参照してください。

.. attribute:: Option.dest

   (デフォルト: オプション文字列を使う)

   このオプションのアクションがある値をどこかに書いたり書き換えたりを意味する
   場合、これは :mod:`optparse` にその書く場所を教えます。詳しく言えば
   :attr:`~Option.dest` には :mod:`optparse` がコマンドラインを解析しながら
   組み立てる ``options`` オブジェクトの属性の名前を指定します。

.. attribute:: Option.default

   コマンドラインに指定がなかったときにこのオプションの対象に使われる値です。
   :meth:`OptionParser.set_defaults` も参照してください。

.. attribute:: Option.nargs

   (デフォルト: 1)

   このオプションがあったときに幾つの :attr:`~Option.type` 型の引数が
   消費されるべきかを指定します。 1 より大きい場合、 :mod:`optparse` は
   :attr:`~Option.dest` に値のタプルを格納します。

.. attribute:: Option.const

   定数を格納する動作のための、その定数です。

.. attribute:: Option.choices

   ``"choice"`` 型オプションに対してユーザが選べる選択肢となる文字列の
   リストです。

.. attribute:: Option.callback

   アクションが ``"callback"`` であるオプションに対し、このオプションがあった
   ときに呼ばれる呼び出し可能オブジェクトです。呼び出し時に渡される引数の
   詳細については、 :ref:`optparse-option-callbacks` を参照してください。

.. attribute:: Option.callback_args
               Option.callback_kwargs

   ``callback`` に渡される標準的な4つのコールバック引数の後ろに追加する、
   位置指定引数とキーワード引数。

.. attribute:: Option.help

   ユーザが :attr:`~Option.help` オプション(``--help`` のような)を指定
   したときに表示される、使用可能な全オプションのリストの中のこのオプションに
   関する説明文です。説明文を提供しておかなければ、オプションは説明文なしで表示されます。
   オプションを隠すには特殊な値 :data:`optparse.SUPPRESS_HELP` を使います。

.. attribute:: Option.metavar

   (デフォルト: オプション文字列から)

   説明文を表示する際にオプションの引数の身代わりになるものです。
   例は :ref:`optparse-tutorial` 節を参照してください。


.. _optparse-standard-option-actions:

標準的なオプション・アクション
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

様々なオプション・アクションにはどれも互いに少しづつ異なった条件と作用が
あります。ほとんどのアクションに関連するオプション属性がいくつかあり、
値を指定して :mod:`optparse` の挙動を操作できます。いくつかのアクションには
必須の属性があり、必ず値を指定せねばなりません。

* ``"store"`` [関連: :attr:`~Option.type`, :attr:`~Option.dest`,
  :attr:`~Option.nargs`, :attr:`~Option.choices`]

  オプションの後には必ず引数が続きます。引数は :attr:`~Option.type` に従って
  値に変換されて :attr:`~Option.dest` に保存されます。
  :attr:`~Option.nargs` > 1 の場合、複数の引数をコマンドラインから取り出します。
  引数は全て :attr:`~Option.type` に従って変換され、 :attr:`~Option.dest`
  にタプルとして保存されます。 :ref:`optparse-standard-option-types` 節を
  参照してください。

  :attr:`~Option.choices` を(文字列のリストかタプルで) 指定した場合、型の
  デフォルト値は ``"choice"`` になります。

  :attr:`~Option.type` を指定しない場合、デフォルトの値は ``"string"`` です。

  :attr:`~Option.dest` を指定しない場合、 :mod:`optparse` は保存先を最初の
  長い形式のオプション文字列から導出します
  (例えば、 ``--foo-bar`` は ``foo_bar`` になります)。長い形式の
  オプション文字列がない場合、 :mod:`optparse` は最初の短い形式のオプション
  から保存先の変数名を導出します (``-f`` は ``f`` になります)。

  例えば::

     parser.add_option("-f")
     parser.add_option("-p", type="float", nargs=3, dest="point")

  とすると、以下のようなコマンドライン::

     -f foo.txt -p 1 -3.5 4 -fbar.txt

  を解析した場合、 :mod:`optparse` は  ::

     options.f = "foo.txt"
     options.point = (1.0, -3.5, 4.0)
     options.f = "bar.txt"

  のように設定を行います。

* ``"store_const"`` [関連: :attr:`~Option.const`; 関連:
  :attr:`~Option.dest`]

  値 :attr:`~Option.cost` を :attr:`dest` に保存します。

  例えば::

     parser.add_option("-q", "--quiet",
                       action="store_const", const=0, dest="verbose")
     parser.add_option("-v", "--verbose",
                       action="store_const", const=1, dest="verbose")
     parser.add_option("--noisy",
                       action="store_const", const=2, dest="verbose")

  とします。 ``--noisy`` が見つかると、 :mod:`optparse` は  ::

     options.verbose = 2

  のように設定を行います。

* ``"store_true"`` [関連: :attr:`~Option.dest`]

  ``"store_const"`` の特殊なケースで、真 (True) を :attr:`dest` に保存します。

* ``"store_false"`` [関連::attr:`~Option.dest`]

  ``"store_true"`` と似ていて、偽 (False) を保存します。

  例::

     parser.add_option("--clobber", action="store_true", dest="clobber")
     parser.add_option("--no-clobber", action="store_false", dest="clobber")

* ``"append"`` [関連: :attr:`~Option.type`, :attr:`~Option.dest`,
  :attr:`~Option.nargs`, :attr:`~Option.choices`]

  このオプションの後ろには必ず引数が続きます。引数は :attr:`~Option.dest` の
  リストに追加されます。 :attr:`~Option.dest` のデフォルト値を指定しなかった場合、
  :mod:`optparse` がこのオプションを最初にみつけた時点で空のリストを自動的に生成します。
  :attr:`~Option.nargs` > 1 の場合、複数の引数をコマンドラインから取り出し、
  長さ :attr:`~Option.nargs` のタプルを生成して :attr:`~Option.dest` に追加します。

  :attr:`~Option.type` および :attr:`~Option.dest` のデフォルト値は ``"store"``
  アクションと同じです。

  例::

     parser.add_option("-t", "--tracks", action="append", type="int")

  ``-t3`` がコマンドライン上で見つかると、 :mod:`optparse` は::

     options.tracks = []
     options.tracks.append(int("3"))

  と同等の処理を行います。

  その後、 ``--tracks=4`` が見つかると::

     options.tracks.append(int("4"))

  を実行します。

* ``"append_const"`` [関連: :attr:`~Option.const`; 関連:
  :attr:`~Option.dest`]

  ``"store_const"`` と同様ですが、 :attr:`~Option.const` の値は
  :attr:`~Option.dest` に追加(append)されます。 ``"append"``
  の場合と同じように :attr:`~Option.dest` のデフォルトは ``None``
  ですがこのオプションを最初にみつけた時点で空のリストを自動的に生成します。

* ``"count"`` [関連: :attr:`~Option.dest`]

  :attr:`~Option.dest` に保存されている整数値をインクリメントします。
  :attr:`~Option.dest` は (デフォルトの値を指定しない限り)
  最初にインクリメントを行う前にゼロに設定されます。

  例::

     parser.add_option("-v", action="count", dest="verbosity")

  コマンドライン上で最初に ``-v`` が見つかると、 :mod:`optparse` は::

     options.verbosity = 0
     options.verbosity += 1

  と同等の処理を行います。

  以後、 ``-v`` が見つかるたびに、  ::

     options.verbosity += 1

  を実行します。

* ``"callback"`` [必須: :attr:`~Option.callback`; 関連:
  :attr:`~Option.type`, :attr:`~Option.nargs`, :attr:`~Option.callback_args`,
  :attr:`~Option.callback_kwargs`]

  :attr:`~Option.callback` に指定された関数を次のように呼び出します。  ::

     func(option, opt_str, value, parser, *args, **kwargs)

  詳細は、 :ref:`optparse-option-callbacks` 節を参照してください。

* ``"help"``

  現在のオプションパーザ内の全てのオプションに対する完全なヘルプメッセージを出力します。
  ヘルプメッセージは :class:`OptionParser`
  のコンストラクタに渡した ``usage``  文字列と、各オプションに渡した
  :attr:`~Option.help` 文字列から生成します。

  オプションに :attr:`~Option.help` 文字列が指定されていなくても、オプションは
  ヘルプメッセージ中に列挙されます。オプションを完全に表示させないようにするには、
  特殊な値 :data:`optparse.SUPPRESS_HELP` を使ってください。

  :mod:`optparse` は全ての :class:`OptionParser` に自動的に :attr:`~Option.help`
  オプションを追加するので、通常自分で生成する必要はありません。

  例::

     from optparse import OptionParser, SUPPRESS_HELP

     # 通常、 help オプションは自動的に追加されますが、
     # add_help_option 引数を使って抑制することができます。
     parser = OptionParser(add_help_option=False)

     parser.add_option("-h", "--help", action="help")
     parser.add_option("-v", action="store_true", dest="verbose",
                       help="Be moderately verbose")
     parser.add_option("--file", dest="filename",
                       help="Input file to read data from")
     parser.add_option("--secret", help=SUPPRESS_HELP)

  :mod:`optparse` がコマンドライン上に ``-h`` または  ``--help`` を
  見つけると、以下のようなヘルプメッセージを標準出力に出力します
  (``sys.argv[0]`` は ``"foo.py"`` だとします)。

  .. code-block:: text

     Usage: foo.py [options]

     Options:
       -h, --help        Show this help message and exit
       -v                Be moderately verbose
       --file=FILENAME   Input file to read data from

  ヘルプメッセージの出力後、 :mod:`optparse` は ``sys.exit(0)`` で
  プロセスを終了します。

* ``"version"``

  :class:`OptionParser` に指定されているバージョン番号を標準出力に出力して
  終了します。バージョン番号は、実際には :class:`OptionParser` の
  :meth:`print_version` メソッドで書式化されてから出力されます。
  通常、 :class:`OptionParser` のコンストラクタに ``version`` 引数が指定された
  ときのみ関係のあるアクションです。 :attr:`~Option.help` オプションと同様、
  :mod:`optparse` はこのオプションを必要に応じて自動的に追加するので、
  ``version`` オプションを作成することはほとんどないでしょう。


.. _optparse-standard-option-types:

標準のオプション型
^^^^^^^^^^^^^^^^^^

:mod:`optparse` には、 ``"string"``, ``"int"``, ``"long"``, ``"choice"``,
``"float"``, ``"complex"`` の 6 種類のビルトインのオプション型があります。
新たなオプションの型を追加したければ、 :ref:`optparse-extending-optparse`
節を参照してください。

文字列オプションの引数はチェックや変換を一切受けません: コマンドライン上のテキストは保存先にそのまま保存されます (またはコールバックに渡されます)。

整数引数 (``"int"`` 型や ``"long"`` 型) は次のように読み取られます。

* 数が ``0x`` から始まるならば、16進数として読み取られます

* 数が ``0`` から始まるならば、8進数として読み取られます

* 数が ``0b`` から始まるならば、2進数として読み取られます

* それ以外の場合、数は10進数として読み取られます


変換は適切な底(2, 8, 10, 16 のどれか)とともに :func:`int` または :func:`long`
を呼び出すことで行なわれます。
この変換が失敗した場合 :mod:`optparse` の処理も失敗に終わりますが、
より役に立つエラーメッセージを出力します。

``"float"`` および ``"complex"`` のオプション引数は直接 :func:`float` や
:func:`complex` で変換されます。エラーは同様の扱いです。

``"choice"`` オプションは ``"string"`` オプションのサブタイプです。
:attr:`~Option.choice` オプションの属性 (文字列からなるシーケンス) には、
利用できるオプション引数のセットを指定します。 :func:`optparse.check_choice`
はユーザの指定したオプション引数とマスタリストを比較して、無効な文字列が
指定された場合には :exc:`OptionValueError` を送出します。


.. _optparse-parsing-arguments:

引数の解析
^^^^^^^^^^

OptionParser を作成してオプションを追加していく上で大事なポイントは、 :meth:`parse_args` メソッドの呼び出しです。  ::

   (options, args) = parser.parse_args(args=None, values=None)

ここで入力パラメータは

``args``
   処理する引数のリスト (デフォルト: ``sys.argv[1:]``)

``values``
   オプション引数を格納する :class:`optparse.Values` のオブジェクト
   (デフォルト: 新しい :class:`Values` のインスタンス) --
   既存のオブジェクトを指定した場合、オプションのデフォルトは
   初期化されません。

であり、戻り値は

``options``
   ``values`` に渡されたものと同じオブジェクト、または :mod:`optparse`
   によって生成された optparse.Values インスタンス

``args``
   全てのオプションの処理が終わった後で残った位置引数

です。

一番普通の使い方は一切キーワード引数を使わないというものです。
``values`` を指定した場合、それは繰り返される :func:`setattr`
の呼び出し (大雑把に言うと保存される各オプション引数につき一回ずつ) で更新されていき、 :meth:`parse_args` で返されます。

:meth:`parse_args` が引数リストでエラーに遭遇した場合、 OptionParser の :meth:`error`
メソッドを適切なエンドユーザ向けのエラーメッセージとともに呼び出します。この呼び出しにより、最終的に終了ステータス 2 (伝統的な Unix
におけるコマンドラインエラーの終了ステータス) でプロセスを終了させることになります。


.. _optparse-querying-manipulating-option-parser:

オプション解析器への問い合わせと操作
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

オプションパーザのデフォルトの振る舞いは、ある程度カスタマイズすることができます。
また、オプションパーザの中を調べることもできます。
:class:`OptionParser` は幾つかのヘルパーメソッドを提供しています。

.. method:: OptionParser.disable_interspersed_args()

   オプションで無い最初の引数を見つけた時点でパースを止めるように設定します。
   例えば、 ``-a`` と ``-b`` が両方とも引数を取らないシンプルなオプション
   だったとすると、 :mod:`optparse` は通常次の構文を受け付け、 ::

      prog -a arg1 -b arg2

   .. and treats it as equivalent to :

   それを次と同じように扱います。 ::

      prog -a -b arg1 arg2

   .. To disable this feature, call :meth:`disable_interspersed_args`.  This
      restores traditional Unix syntax, where option parsing stops with the first
      non-option argument.

   この機能を無効にしたいときは、 :meth:`disable_interspersed_args` メソッドを
   呼び出してください。古典的な Unix システムのように、最初のオプションでない
   引数を見つけたときにオプションの解析を止めるようになります。

   .. Use this if you have a command processor which runs another command which has
      options of its own and you want to make sure these options don't get
      confused.  For example, each command might have a different set of options.

   別のコマンドを実行するコマンドをプロセッサを作成する際、別のコマンドの
   オプションと自身のオプションが混ざるのを防ぐために利用することができます。
   例えば、各コマンドがそれぞれ異なるオプションのセットを持つ場合などに有効です。

.. method:: OptionParser.enable_interspersed_args()

  オプションで無い最初の引数を見つけてもパースを止めないように設定します。
  オプションとコマンド引数の順序が混ざっても良いようになります。
  これはデフォルトの動作です。

.. method:: OptionParser.get_option(opt_str)

   オプション文字列 ``opt_str`` に対する :class:`Option` インスタンスを
   返します。該当するオプションがなければ ``None`` を返します。

.. method:: OptionParser.has_option(opt_str)

   :class:`OptionParser` に(``-q`` や ``--verbose`` のような) オプション
   ``opt_str`` がある場合、真を返します。

.. method:: OptionParser.remove_option(opt_str)

   :class:`OptionParser` に ``opt_str`` に対応するオプションがある場合、
   そのオプションを削除します。該当するオプションに他のオプション文字列が
   指定されていた場合、それらのオプション文字列は全て無効になります。
   *opt_str* がこの :class:`OptionParser` オブジェクトのどのオプションにも
   属さない場合、 :exc:`ValueError` を送出します。


.. _optparse-conflicts-between-options:

オプション間の衝突
^^^^^^^^^^^^^^^^^^

注意が足りないと、衝突するオプションを定義してしまうことがあります。 ::

   parser.add_option("-n", "--dry-run", ...)
   [...]
   parser.add_option("-n", "--noisy", ...)

(とりわけ、 :class:`OptionParser` から標準的なオプションを備えた自前のサブクラスを定義してしまった場合にはよく起きます。)

ユーザがオプションを追加するたびに、 :mod:`optparse` は既存のオプションとの衝突
がないかチェックします。何らかの衝突が見付かると、現在設定されている衝突処理メカニズムを呼び出します。衝突処理メカニズムはコンストラクタ中で呼び出せます::

   parser = OptionParser(..., conflict_handler=handler)

個別にも呼び出せます::

   parser.set_conflict_handler(handler)

衝突時の処理をおこなうハンドラ(handler)には、以下のものが利用できます:

   ``"error"`` (デフォルトの設定)
      オプション間の衝突をプログラム上のエラーとみなし、
      :exc:`OptionConflictError` を送出します。

   ``"resolve"``
      オプション間の衝突をインテリジェントに解決します (下記参照)。


一例として、衝突をインテリジェントに解決する :class:`OptionParser` を定義し、衝突を起こすようなオプションを追加してみましょう::

   parser = OptionParser(conflict_handler="resolve")
   parser.add_option("-n", "--dry-run", ..., help="do no harm")
   parser.add_option("-n", "--noisy", ..., help="be noisy")

この時点で、 :mod:`optparse` はすでに追加済のオプションがオプション文字列 ``-n`` を使っていることを検出します。
``conflict_handler`` が ``"resolve"`` なので、 :mod:`optparse` は既に追加済のオプションリストの方から
``-n`` を除去して問題を解決します。従って、 ``-n`` の除去されたオプションは ``--dry-run`` だけでしか有効にできなく
なります。ユーザがヘルプ文字列を要求した場合、問題解決の結果を反映したメッセージが出力されます::

   Options:
     --dry-run     do no harm
     [...]
     -n, --noisy   be noisy

これまでに追加したオプション文字列を跡形もなく削り去り、ユーザがそのオプションをコマンドラインから起動する手段をなくせます。
この場合、 :mod:`optparse` はオプションを完全に除去してしまうので、こうしたオプションはヘルプテキストやその他のどこにも表示されなくなります。
例えば、現在の :class:`OptionParser` の場合、以下の操作::

   parser.add_option("--dry-run", ..., help="new dry-run option")

を行った時点で、最初の ``-n``/``--dry-run`` オプションはもはやアクセスできなくなります。このため、 :mod:`optparse` は
オプションを消去してしまい、ヘルプテキスト::

   Options:
     [...]
     -n, --noisy   be noisy
     --dry-run     new dry-run option

だけが残ります。


.. _optparse-cleanup:

クリーンアップ
^^^^^^^^^^^^^^

OptionParser インスタンスはいくつかの循環参照を抱えています。
このことは Python のガーベジコレクタにとって問題になるわけではありませんが、
使い終わった OptionParser に対して :meth:`~OptionParser.destroy`
を呼び出すことでこの循環参照を意図的に断ち切るという方法を選ぶこともできます。
この方法は特に長時間実行するアプリケーションで OptionParser から大きな
オブジェクトグラフが到達可能になっているような場合に有用です。


.. _optparse-other-methods:

その他のメソッド
^^^^^^^^^^^^^^^^

OptionParser にはその他にも幾つかの公開されたメソッドがあります:

.. method:: OptionParser.set_usage(usage)

  上で説明したコンストラクタの ``usage`` キーワード引数での規則に従った使用法の
  文字列をセットします。 ``None`` を渡すとデフォルトの使用法文字列が使われる
  ようになり、 :data:`optparse.SUPPRESS_USAGE` によって使用法メッセージを抑制
  できます。

.. method:: OptionParser.print_usage(file=None)

   現在のプログラムの使用法メッセージ (``self.usage``) を *file* (デフォルト:
   stdout) に表示します。 ``self.usage`` 内にある全ての ``%prog`` という文字列は
   現在のプログラム名に置換されます。 ``self.usage`` が空もしくは未定義の時は
   何もしません。

.. method:: OptionParser.get_usage()

   .. Same as :meth:`print_usage` but returns the usage string instead of
      printing it.

   :meth:`print_usage` と同じですが、使用法メッセージを表示する代わりに
   文字列として返します。

.. method:: OptionParser.set_defaults(dest=value, ...)

   .. Set default values for several option destinations at once.  Using
      :meth:`set_defaults` is the preferred way to set default values for options,
      since multiple options can share the same destination.  For example, if
      several "mode" options all set the same destination, any one of them can set
      the default, and the last one wins:

   幾つかの保存先に対してデフォルト値をまとめてセットします。
   :meth:`set_defaults` を使うのは複数のオプションにデフォルト値をセットする
   好ましいやり方です。複数のオプションが同じ保存先を共有することがあり得るからです。
   たとえば幾つかの "mode" オプションが全て同じ保存先をセットするものだったとすると、
   どのオプションもデフォルトをセットすることができ、しかし最後に指定したもの
   だけが有効になります。 ::

      parser.add_option("--advanced", action="store_const",
                        dest="mode", const="advanced",
                        default="novice")    # overridden below
      parser.add_option("--novice", action="store_const",
                        dest="mode", const="novice",
                        default="advanced")  # overrides above setting

   .. To avoid this confusion, use :meth:`set_defaults`:

   こうした混乱を避けるために :meth:`set_defaults` を使います。 ::

      parser.set_defaults(mode="advanced")
      parser.add_option("--advanced", action="store_const",
                        dest="mode", const="advanced")
      parser.add_option("--novice", action="store_const",
                        dest="mode", const="novice")



.. _optparse-option-callbacks:

オプション処理コールバック
--------------------------

:mod:`optparse` の組み込みのアクションや型が望みにかなったものでない場合、二つの選択肢があります: 一つは :mod:`optparse`
の拡張、もう一つは callback オプションの定義です。 :mod:`optparse` の拡張は汎用性に富んでいますが、単純なケースに対して
いささか大げさでもあります。大体は簡単なコールバックで事足りるでしょう。

``callback`` オプションの定義は二つのステップからなります:

* ``"callback"`` アクションを使ってオプション自体を定義する。

* コールバックを書く。コールバックは少なくとも後で説明する 4 つの引数をとる関数
  (またはメソッド) でなければなりません。


.. _optparse-defining-callback-option:

callbackオプションの定義
^^^^^^^^^^^^^^^^^^^^^^^^

callback オプションを最も簡単に定義するには、 :meth:`OptionParser.add_option`
メソッドを使います。 :attr:`~Option.action` の他に指定しなければならない属性は
``callback`` すなわちコールバックする関数自体です::

   parser.add_option("-c", action="callback", callback=my_callback)

``callback`` は関数 (または呼び出し可能オブジェクト)なので、callback オプションを定義する時にはあらかじめ
``my_callback()`` を定義しておかねばなりません。この単純なケースでは、 :mod:`optparse` は ``-c`` が
何らかの引数をとるかどうか判別できず、通常は ``c`` が引数を伴わないことを意味します --- 知りたいことはただ単に
:option:`-c` がコマンドライン上に現れたどうかだけです。とはいえ、場合によっては、自分のコールバック関数に
任意の個数のコマンドライン引数を消費させたいこともあるでしょう。これがコールバック関数をトリッキーなものにしています;
これについてはこの節の後の方で説明します。

:mod:`optparse` は常に四つの引数をコールバックに渡し、その他には
:attr:`~Optioncallback_args` および :attr:`callback_kwargs` で指定した
追加引数しか渡しません。従って、最小のコールバック関数シグネチャは::

   def my_callback(option, opt, value, parser):

のようになります。

コールバックの四つの引数については後で説明します。

callback オプションを定義する場合には、他にもいくつかオプション属性を指定できます:

:attr:`~Option.type`
   他で使われているのと同じ意味です: ``store`` や ``append`` アクションの時と同じく、
   この属性は :mod:`optparse` に引数を一つ消費して :attr:`~Option.type` で
   指定した型に変換させます。 :mod:`optparse` は変換後の値をどこかに保存する
   代わりにコールバック関数に渡します。

:attr:`~Option.nargs`
   これも他で使われているのと同じ意味です: このオプションが指定されていて、かつ
   ``nargs`` > 1 である場合、 :mod:`optparse` は ``nargs`` 個の引数を消費
   します。このとき各引数は :attr:`type` 型に変換できねばなりません。
   変換後の値はタプルとしてコールバックに渡されます。

:attr:`~Option.callback_args`
   その他の位置指定引数からなるタプルで、コールバックに渡されます。

:attr:`~Option.callback_kwargs`
   その他のキーワード引数からなる辞書で、コールバックに渡されます。


.. _optparse-how-callbacks-called:

コールバック関数はどのように呼び出されるか
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

コールバックは全て以下の形式で呼び出されます::

   func(option, opt_str, value, parser, *args, **kwargs)

ここで、

``option``
   コールバックを呼び出している :class:`Option` のインスタンスです。

``opt_str``
   は、コールバック呼び出しのきっかけとなったコマンドライン上のオプション文字列です。 (長い形式のオプションに対する省略形が使われている場合、 *opt*
   は完全な、正式な形のオプション文字列となります ---  例えば、ユーザが :option:`--foobar` の短縮形として ``--foo``
   をコマンドラインに入力した時には、 *opt_str*  は ``"--foobar"`` となります。)

``value``
   オプションの引数で、コマンドライン上に見つかったものです。
   :mod:`optparse` は、 :attr:`~Option.type` が設定されている場合、単一の
   引数しかとりません。 ``value`` の型はオプションの型として指定された型に
   なります。このオプションに対する :attr:`~Option.type` が ``None`` で
   ある(引数なしの) 場合、 ``value`` は ``None`` になります。
   :attr:`~Option.nargs` > 1 であれば、 ``value`` は適切な型をもつ値の
   タプルになります。

``parser``
   現在のオプション解析の全てを駆動している :class:`OptionParser`  インスタンスです。この変数が有用なのは、この値を介してインスタンス属性と
   していくつかの興味深いデータにアクセスできるからです:

   ``parser.largs``
      現在放置されている引数、すなわち、すでに消費されたものの、オプションでもオプション引数でもない引数からなるリストです。 ``parser.largs``
      は自由に変更でき、たとえば引数を追加したりできます (このリストは ``args`` 、すなわち :meth:`parse_args`
      の二つ目の戻り値になります)

   ``parser.rargs``
      現在残っている引数、すなわち、 ``opt_str`` および ``value`` があれば除き、それ以外の引数が残っているリストです。
      ``parser.rargs`` は自由に変更でき、例えばさらに引数を消費したりできます。

   ``parser.values``
      オプションの値がデフォルトで保存されるオブジェクト (``optparse.OptionValues`` のインスタンス)
      です。この値を使うと、コールバック関数がオプションの値を記憶するために、他の :mod:`optparse`
      と同じ機構を使えるようにするため、グローバル変数や閉包 (closure) を台無しにしないので便利です。
      コマンドライン上にすでに現れているオプションの値にもアクセスできます。

``args``
   :attr:`~Option.callback_args` オプション属性で与えられた任意の固定引数
   からなるタプルです。

``kwargs``
   :attr:`~Option.callback_kwargs` オプション属性で与えられた任意の
   キーワード引数からなるタプルです。


.. _optparse-raising-errors-in-callback:

コールバック中で例外を送出する
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

オプション自体か、あるいはその引数に問題がある場合、コールバック関数は :exc:`OptionValueError`
を送出せねばなりません。 :mod:`optparse` はこの例外をとらえてプログラムを終了させ、ユーザが指定しておいたエラーメッセージを
標準エラー出力に出力します。エラーメッセージは明確、簡潔かつ正確で、どのオプションに誤りがあるかを示さねばなりません。さもなければ、ユーザは自分の
操作のどこに問題があるかを解決するのに苦労することになります。


.. _optparse-callback-example-1:

コールバックの例 1: ありふれたコールバック
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

引数をとらず、発見したオプションを単に記録するだけのコールバックオプションの例を以下に示します::

   def record_foo_seen(option, opt_str, value, parser):
       parser.values.saw_foo = True

   parser.add_option("--foo", action="callback", callback=record_foo_seen)

もちろん、 ``"store_true"`` アクションを使っても実現できます。


.. _optparse-callback-example-2:

コールバックの例 2: オプションの順番をチェックする
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

もう少し面白みのある例を示します: この例では、 ``-b`` を発見して、その後で
``-a`` がコマンドライン中に現れた場合にはエラーになります。
::

   def check_order(option, opt_str, value, parser):
       if parser.values.b:
           raise OptionValueError("can't use -a after -b")
       parser.values.a = 1
   [...]
   parser.add_option("-a", action="callback", callback=check_order)
   parser.add_option("-b", action="store_true", dest="b")


.. _optparse-callback-example-3:

コールバックの例 3: オプションの順番をチェックする (汎用的)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

このコールバック (フラグを立てるが、 ``-b`` が既に指定されていればエラーになる)
を同様の複数のオプションに対して再利用したければ、もう少し作業する必要があります: エラーメッセージとセットされるフラグを一般化しなければなりません。  ::

   def check_order(option, opt_str, value, parser):
       if parser.values.b:
           raise OptionValueError("can't use %s after -b" % opt_str)
       setattr(parser.values, option.dest, 1)
   [...]
   parser.add_option("-a", action="callback", callback=check_order, dest='a')
   parser.add_option("-b", action="store_true", dest="b")
   parser.add_option("-c", action="callback", callback=check_order, dest='c')


.. _optparse-callback-example-4:

コールバックの例 4: 任意の条件をチェックする
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

もちろん、単に定義済みのオプションの値を調べるだけにとどまらず、コールバックには任意の条件を入れられます。例えば、満月でなければ呼び出してはならないオプション
があるとしましょう。やらなければならないことはこれだけです::

   def check_moon(option, opt_str, value, parser):
       if is_moon_full():
           raise OptionValueError("%s option invalid when moon is full"
                                  % opt_str)
       setattr(parser.values, option.dest, 1)
   [...]
   parser.add_option("--foo",
                     action="callback", callback=check_moon, dest="foo")

(``is_moon_full()`` の定義は読者への課題としましょう。


.. _optparse-callback-example-5:

コールバックの例5: 固定引数
^^^^^^^^^^^^^^^^^^^^^^^^^^^

決まった数の引数をとるようなコールパックオプションを定義するなら、問題はやや
興味深くなってきます。引数をとるようコールバックに指定するのは、 ``"store"``
や ``"append"`` オプションの定義に似ています。 :attr:`~Option.type` を定義して
いれば、そのオプションは引数を受け取ったときに該当する型に変換できねば
なりません。さらに :attr:`~Option.nargs` を指定すれば、オプションは
:attr:`~Option.nargs` 個の引数を受け取ります。

標準の ``"store"`` アクションをエミュレートする例を以下に示します::

   def store_value(option, opt_str, value, parser):
       setattr(parser.values, option.dest, value)
   [...]
   parser.add_option("--foo",
                     action="callback", callback=store_value,
                     type="int", nargs=3, dest="foo")

:mod:`optparse` は 3 個の引数を受け取り、それらを整数に変換するところまで
面倒をみてくれます。ユーザは単にそれを保存するだけです。
(他の処理もできます; いうまでもなく、この例にはコールバックは必要ありません)


.. _optparse-callback-example-6:

コールバックの例6: 可変個の引数
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

あるオプションに可変個の引数を持たせたいと考えているなら、問題はいささか手強くなってきます。この場合、 :mod:`optparse`
では該当する組み込みのオプション解析機能を提供していないので、自分でコールバックを書かねばなりません。さらに、 :mod:`optparse`
が普段処理している、伝統的な Unix コマンドライン解析における難題を自分で解決せねばなりません。とりわけ、コールバック関数では引数が裸の ``--``
や ``-`` の場合における慣習的な処理規則:

* either ``--`` or ``-`` can be option arguments

* 裸の ``--`` (何らかのオプションの引数でない場合): コマンドライン処理を停止し、 ``--`` を無視します。

* 裸の ``-`` (何らかのオプションの引数でない場合): コマンドライン処理を停止しますが、 ``-`` は残します
  (``parser.largs`` に追加します)。

を実装せねばなりません。

オプションが可変個の引数をとるようにさせたいなら、いくつかの巧妙で厄介な問題に配慮しなければなりません。どういう実装を
とるかは、アプリケーションでどのようなトレードオフを考慮するかによります (このため、 :mod:`optparse` では可変個の引数に
関する問題を直接的に取り扱わないのです)。

とはいえ、可変個の引数をもつオプションに対するスタブ (stub、仲介インタフェース) を以下に示しておきます::

    def vararg_callback(option, opt_str, value, parser):
        assert value is None
        value = []

        def floatable(str):
            try:
                float(str)
                return True
            except ValueError:
                return False

        for arg in parser.rargs:
            # stop on --foo like options
            if arg[:2] == "--" and len(arg) > 2:
                break
            # stop on -a, but not on -3 or -3.0
            if arg[:1] == "-" and len(arg) > 1 and not floatable(arg):
                break
            value.append(arg)

        del parser.rargs[:len(value)]
        setattr(parser.values, option.dest, value)

   [...]
   parser.add_option("-c", "--callback", dest="vararg_attr",
                     action="callback", callback=vararg_callback)


.. _optparse-extending-optparse:

:mod:`optparse` の拡張
----------------------

:mod:`optparse` がコマンドラインオプションをどのように解釈するかを決める二つの重要な要素はそれぞれのオプションのアクションと型なので、拡張
の方向は新しいアクションと型を追加することになると思います。


.. _optparse-adding-new-types:

新しい型の追加
^^^^^^^^^^^^^^

新しい型を追加するためには、 :mod:`optparse` の :class:`Option` クラスの
サブクラスを自身で定義する必要があります。このクラスには
:mod:`optparse` における型を定義する一対の属性があります。それは
:attr:`~Option.TYPES` と :attr:`~Option.TYPE_CHECKER` です。

.. attribute:: Option.TYPES

   :attr:`TYPES` は型名のタプルです。新しく作るサブクラスでは、タプル
   :attr:`TYPES` を単純に標準のものを利用して新しく定義すると良いでしょう。

.. attribute:: Option.TYPE_CHECKER

   :attr:`TYPE_CHECKER` は辞書で型名を型チェック関数に対応付けるものです。
   型チェック関数は以下のようなシグネチャを持ちます。 ::

      def check_mytype(option, opt, value)

   ここで ``option`` は :class:`Option` のインスタンスであり、 ``opt`` は
   オプション文字列(たとえば ``-f``)で、 ``value`` は望みの型として
   チェックされ変換されるべくコマンドラインで与えられる文字列です。
   ``check_mytype()`` は想定されている型 ``mytype`` のオブジェクトを
   返さなければなりません。型チェック関数から返される値は
   :meth:`OptionParser.parse_args` で返されるOptionValues インスタンスに
   収められるか、またはコールバックに ``value`` パラメータとして渡されます。

   型チェック関数は何か問題に遭遇したら :exc:`OptionValueError` を
   送出しなければなりません。
   :exc:`OptionValueError` は文字列一つを引数に取り、それはそのまま
   :class:`OptionParser` の :meth:`error` メソッドに渡され、そこで
   プログラム名と文字列 ``"error:"`` が前置されてプロセスが終了する前に
   stderr に出力されます。

馬鹿馬鹿しい例ですが、Python スタイルの複素数を解析する ``"complex"``
オプション型を作ってみせることにします。(:mod:`optparse` 1.3 が複素数の
サポートを組み込んでしまったため以前にも増して馬鹿らしくなりましたが、
気にしないでください。)

最初に必要な import 文を書きます。 ::

   from copy import copy
   from optparse import Option, OptionValueError

まずは型チェック関数を定義しなければなりません。これは後で(これから定義する
Option のサブクラスの :attr:`~Option.TYPE_CHECKER` クラス属性の中で)
参照されることになります。 ::

   def check_complex(option, opt, value):
       try:
           return complex(value)
       except ValueError:
           raise OptionValueError(
               "option %s: invalid complex value: %r" % (opt, value))

最後に Option のサブクラスです。  ::

   class MyOption (Option):
       TYPES = Option.TYPES + ("complex",)
       TYPE_CHECKER = copy(Option.TYPE_CHECKER)
       TYPE_CHECKER["complex"] = check_complex

(もしここで :attr:`Option.TYPE_CHECKER` に :func:`copy` を適用しなければ、
:mod:`optparse` の Option クラスの :attr:`~Option.TYPE_CHECKER` 属性を
いじってしまうことになります。
Python の常として、良いマナーと常識以外にそうすることを止めるものはありません。)

これだけです! もう新しいオプション型を使うスクリプトを他の :mod:`optparse` に基づいた
スクリプトとまるで同じように書くことができます。ただし、 OptionParser に Option でなく MyOption
を使うように指示しなければなければなりません。  ::

   parser = OptionParser(option_class=MyOption)
   parser.add_option("-c", type="complex")

別のやり方として、オプションリストを構築して OptionParser に渡すという方法もあります。 :meth:`add_option`
を上でやったように使わないならば、OptionParser にどのクラスを使うのか教える必要はありません。  ::

   option_list = [MyOption("-c", action="store", type="complex", dest="c")]
   parser = OptionParser(option_list=option_list)


.. _optparse-adding-new-actions:

新しいアクションの追加
^^^^^^^^^^^^^^^^^^^^^^

新しいアクションの追加はもう少しトリッキーです。というのも :mod:`optparse`  が使っている二つのアクションの分類を理解する必要があるからです。

"store" アクション
   :mod:`optparse` が値を現在の OptionValues の属性に格納することになる
   アクションです。この種類のオプションは Option のコンストラクタに
   :attr:`~Option.dest` 属性を与えることが要求されます。

"typed" アクション
   コマンドラインから引数を受け取り、それがある型であることが期待されているアクションです。もう少しはっきり言えば、その型に変換される文字列を受け取るものです。
   この種類のオプションは Option のコンストラクタに :attr:`type` 属性を与えることが要求されます。

この分類には重複する部分があります。デフォルトの "store" アクションには
``"store"``, ``"store_const"``, ``"append"``, ``"count"`` などがありますが、
デフォルトの "typed" オプションは ``"store"``, ``"append"``, ``"callback"``
の三つです。

アクションを追加する際に、以下の Option のクラス属性(全て文字列のリストです)
の中の少なくとも一つに付け加えることでそのアクションを分類する必要があります。

.. attribute:: Option.ACTIONS

   全てのアクションは ACTIONS にリストされていなければなりません

.. attribute:: Option.STORE_ACTIONS

   "store" アクションはここにもリストされます

.. attribute:: Option.TYPED_ACTIONS

   "typed" アクションはここにもリストされます

.. attribute:: Option.ALWAYS_TYPED_ACTIONS

   型を取るアクション (つまりそのオプションが値を取る) はここにもリストされます。このことの唯一の効果は :mod:`optparse`
   が、型の指定が無くアクションが :attr:`ALWAYS_TYPED_ACTIONS` のリストにあるオプションに、デフォルト型 ``"string"``
   を割り当てるということだけです。

実際に新しいアクションを実装するには、Option の :meth:`take_action`
メソッドをオーバライドしてそのアクションを認識する場合分けを追加しなければなりません。

例えば、 ``"extend"`` アクションというのを追加してみましょう。このアクションは標準的な ``"append"``
アクションと似ていますが、コマンドラインから一つだけ値を読み取って既存のリストに追加するのではなく、複数の値をコンマ区切りの文字列として
読み取ってそれらで既存のリストを拡張します。すなわち、もし ``--names`` が ``"string"`` 型の ``"extend"``
オプションだとすると、次のコマンドライン  ::

   --names=foo,bar --names blah --names ding,dong

の結果は次のリストになります。  ::

   ["foo", "bar", "blah", "ding", "dong"]

再び Option のサブクラスを定義します。  ::

   class MyOption(Option):

       ACTIONS = Option.ACTIONS + ("extend",)
       STORE_ACTIONS = Option.STORE_ACTIONS + ("extend",)
       TYPED_ACTIONS = Option.TYPED_ACTIONS + ("extend",)
       ALWAYS_TYPED_ACTIONS = Option.ALWAYS_TYPED_ACTIONS + ("extend",)

       def take_action(self, action, dest, opt, value, values, parser):
           if action == "extend":
               lvalue = value.split(",")
               values.ensure_value(dest, []).extend(lvalue)
           else:
               Option.take_action(
                   self, action, dest, opt, value, values, parser)

注意すべきは次のようなところです。

* ``"extend"`` はコマンドラインの値を予期していると同時にその値をどこかに格納しますので、 :attr:`~Option.STORE_ACTIONS` と
  :attr:`~Option.TYPED_ACTIONS` の両方に入ります。

* :mod:`optparse` が ``extend`` アクションに ``"string"`` 型を割り当てるように ``"extend"`` アクションは
  :attr:`~Option.ALWAYS_TYPED_ACTIONS` にも入れてあります。

* :meth:`MyOption.take_action` にはこの新しいアクション一つの扱いだけを実装してあり、他の標準的な
  :mod:`optparse` のアクションについては :meth:`Option.take_action` に制御を戻すようにしてあります。

* ``values`` は optparse_parser.Values クラスのインスタンスであり、非常に有用な
  :meth:`ensure_value` メソッドを提供しています。 :meth:`ensure_value` は本質的に安全弁付きの
  :func:`getattr` です。次のように呼び出します。  ::

     values.ensure_value(attr, value)

  ``values`` に ``attr`` 属性が無いか None だった場合に、 :meth:`ensure_value` は最初に ``value``
  をセットし、それから ``value`` を返します。この振る舞いは ``"extend"``, ``"append"``, ``"count"``
  のように、データを変数に集積し、またその変数がある型 (最初の二つはリスト、最後のは整数) であると期待されるアクション
  を作るのにとても使い易いものです。 :meth:`ensure_value` を使えば、
  作ったアクションを使うスクリプトはオプションに保存先にデフォルト値をセットすることに煩わされずに済みます。デフォルトを None にしておけば
  :meth:`ensure_value` がそれが必要になったときに適当な値を返してくれます。

