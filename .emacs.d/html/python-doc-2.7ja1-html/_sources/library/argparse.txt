.. :mod:`argparse` --- Parser for command-line options, arguments and sub-commands

:mod:`argparse` --- コマンドラインオプション、引数、サブコマンドのパーサー
===============================================================================

.. module:: argparse
   :synopsis: コマンドラインオプションと引数のパーサーライブラリ
.. moduleauthor:: Steven Bethard <steven.bethard@gmail.com>
.. versionadded:: 2.7
.. sectionauthor:: Steven Bethard <steven.bethard@gmail.com>


:mod:`argparse` モジュールはユーザーフレンドリーなコマンドラインインタフェースの
作成を簡単にします。
プログラムがどんな引数を必要としているのかを定義すると、 :mod:`argparse` が
:data:`sys.argv` からそのオプションをパースする部分の面倒を見ます。
:mod:`argparse` モジュールは自動的にヘルプと使用方法メッセージを生成し、
ユーザーが不正な引数をプログラムに指定したときにエラーを発生させます。


.. Example

例
-------

次のコードは、整数のリストを受け取って合計か最大値を返す Python プログラムです::

   import argparse

   parser = argparse.ArgumentParser(description='Process some integers.')
   parser.add_argument('integers', metavar='N', type=int, nargs='+',
                      help='an integer for the accumulator')
   parser.add_argument('--sum', dest='accumulate', action='store_const',
                      const=sum, default=max,
                      help='sum the integers (default: find the max)')

   args = parser.parse_args()
   print args.accumulate(args.integers)

上の Python コードが ``prog.py`` という名前のファイルに保存されたと仮定します。
コマンドラインから実行して、便利なヘルプメッセージを提供することができます。 ::

   $ prog.py -h
   usage: prog.py [-h] [--sum] N [N ...]

   Process some integers.

   positional arguments:
    N           an integer for the accumulator

   optional arguments:
    -h, --help  show this help message and exit
    --sum       sum the integers (default: find the max)

妥当な引数を与えて実行された場合、このプログラムはコマンドライン引数の整数列の
合計か最大値を表示します::

   $ prog.py 1 2 3 4
   4

   $ prog.py 1 2 3 4 --sum
   10

不正な引数が与えられた場合、エラーを発生させます::

   $ prog.py a b c
   usage: prog.py [-h] [--sum] N [N ...]
   prog.py: error: argument N: invalid int value: 'a'

以降のセクションでは、この例をひと通り解説して行きます。


.. Creating a parser

パーサーを作る
^^^^^^^^^^^^^^^^^

:mod:`argparse` を使う最初のステップは、 :class:`ArgumentParser`
オブジェクトを生成することです::

   >>> parser = argparse.ArgumentParser(description='Process some integers.')

:class:`ArgumentParser` オブジェクトはコマンドラインを解析して Python データ型にする
ために必要な全ての情報を保持します。


.. Adding arguments

引数を追加する
^^^^^^^^^^^^^^^^

:class:`ArgumentParser` にプログラム引数の情報を与えるために、
:meth:`~ArgumentParser.add_argument` メソッドを呼び出します。
一般的に、このメソッドの呼び出しは :class:`ArgumentParser` に、コマンドラインの
文字列を受け取ってそれをオブジェクトにする方法を教えます。
この情報は保存され、 :meth:`~ArgumentParser.parse_args` が呼び出されたときに
利用されます。例えば::

   >>> parser.add_argument('integers', metavar='N', type=int, nargs='+',
   ...                     help='an integer for the accumulator')
   >>> parser.add_argument('--sum', dest='accumulate', action='store_const',
   ...                     const=sum, default=max,
   ...                     help='sum the integers (default: find the max)')

あとで、 :meth:`~ArgumentParser.parse_args` を呼び出すと、 ``integers`` と
``accumulate`` という2つの属性を持ったオブジェクトを返します。
``integers`` 属性は1つ以上の整数のリストで、 ``accumulate`` 属性はコマンドラインから
``--sum`` が指定された場合は :func:`sum` 関数、それ以外の場合は :func:`max` 関数に
なります。


.. Parsing arguments

引数をパースする
^^^^^^^^^^^^^^^^^

:class:`ArgumentParser` は引数を :meth:`~ArgumentParser.parse_args`
メソッドでパースします。
このメソッドはコマンドラインを調べ、各引数を正しい型に変換して、適切なアクションを
実行します。ほとんどの場合、これはシンプルな namespace オブジェクトを
コマンドラインの解析結果から構築することを意味します::

   >>> parser.parse_args(['--sum', '7', '-1', '42'])
   Namespace(accumulate=<built-in function sum>, integers=[7, -1, 42])

スクリプトでは、 :meth:`~ArgumentParser.parse_args` は典型的には引数なしで
呼び出され、 :class:`ArgumentParser` は自動的に :data:`sys.argv` から
コマンドライン引数を取得します。


.. ArgumentParser objects

ArgumentParser オブジェクト
----------------------------

.. class:: ArgumentParser([description], [epilog], [prog], [usage], [add_help], [argument_default], [parents], [prefix_chars], [conflict_handler], [formatter_class])

   新しい :class:`ArgumentParser` オブジェクトを生成します。
   各引数についてはあとで詳しく説明しますが、簡単に言うと:

   * description_ - 引数のヘルプの前に表示されるテキスト

   * epilog_ - 引数のヘルプの後で表示されるテキスト

   * add_help_ - -h/--help オプションをパーサーに追加する (デフォルト: ``True``)

   * argument_default_ - 引数にグローバルのデフォルト値を設定する
     (デフォルト: ``None``)

   * parents_ - :class:`ArgumentParser` オブジェクトのリストで、このオブジェクトの
     引数が追加される

   * prefix_chars_ - オプションの引数の prefix になる文字集合
     (デフォルト: '-')

   * fromfile_prefix_chars_ - 追加の引数を読み込むファイルの prefix になる文字集合
     (デフォルト: ``None``)

   * formatter_class_ - ヘルプ出力をカスタマイズするためのクラス

   * conflict_handler_ - 衝突するオプションを解決する方法を定義する。
     通常は利用する必要はありません。

   * prog_ - プログラム名 (デフォルト: :data:`sys.argv[0]`)

   * usage_ - プログラムの利用方法を解説する文字列 (デフォルト: 生成される)

以下のセクションでは各オプションの利用方法を解説します。


description
^^^^^^^^^^^

多くの場合、 :class:`ArgumentParser` のコンストラクタを呼び出すときに
``description=`` キーワード引数が利用されます。
この引数はプログラムが何をしてどう動くのかについての短い説明です。
ヘルプメッセージで、この description はコマンドラインの利用法と引数の
ヘルプメッセージの間に表示されます::

   >>> parser = argparse.ArgumentParser(description='A foo that bars')
   >>> parser.print_help()
   usage: argparse.py [-h]

   A foo that bars

   optional arguments:
    -h, --help  show this help message and exit

デフォルトでは、 description は行ラップされるので、与えられたスペースに
マッチします。この挙動を変更するには、 formatter_class_ 引数を参照してください。


epilog
^^^^^^

いくつかのプログラムは、プログラムについての追加の説明を引数の解説の
後に表示します。このテキストは :class:`ArgumentParser` の ``epilog=`` 引数に
指定することができます::

   >>> parser = argparse.ArgumentParser(
   ...     description='A foo that bars',
   ...     epilog="And that's how you'd foo a bar")
   >>> parser.print_help()
   usage: argparse.py [-h]

   A foo that bars

   optional arguments:
    -h, --help  show this help message and exit

   And that's how you'd foo a bar

description_ 引数と同じく、 ``epilog=`` テキストもデフォルトで行ラップされ、
:class:`ArgumentParser` の formatter_class_ 引数で動作を調整することができます。


add_help
^^^^^^^^

デフォルトでは、 ArgumentParser オブジェクトはシンプルにパーサーの
ヘルプメッセージを表示するオプションを自動的に追加します。
例えば、以下のコードを含む ``myprogram.py`` ファイルについて
考えてください::

   import argparse
   parser = argparse.ArgumentParser()
   parser.add_argument('--foo', help='foo help')
   args = parser.parse_args()

コマンドラインに ``-h`` か ``--help`` が指定された場合、 ArgumentParser の
help が表示されます::

   $ python myprogram.py --help
   usage: myprogram.py [-h] [--foo FOO]

   optional arguments:
    -h, --help  show this help message and exit
    --foo FOO   foo help

必要に応じて、この help オプションを無効にする場合があります。
これは :class:`ArgumentParser` の ``add_help=`` 引数に ``False``
を渡すことで可能です::

   >>> parser = argparse.ArgumentParser(prog='PROG', add_help=False)
   >>> parser.add_argument('--foo', help='foo help')
   >>> parser.print_help()
   usage: PROG [--foo FOO]

   optional arguments:
    --foo FOO  foo help

ヘルプオプションは通常 ``-h/--help`` です。例外は ``prefix_chars=``
が指定されてその中に ``'-'`` が無かった場合で、その場合は ``-h`` と
``--help`` は有効なオプションではありません。
この場合、 ``prefix_chars`` の最初の文字がヘルプオプションの prefix
として利用されます::

   >>> parser = argparse.ArgumentParser(prog='PROG', prefix_chars='+/')
   >>> parser.print_help()
   usage: PROG [+h]

   optional arguments:
     +h, ++help  show this help message and exit


prefix_chars
^^^^^^^^^^^^

ほとんどのコマンドラインオプションは、 ``-f/--foo`` のように prefix に ``'-'``
を使います。
``+f`` や ``/foo`` のような、他の、あるいは追加の prefix 文字をサポートしなければ
ならない場合、 ArgumentParser のコンストラクタの ``prefix_chars=`` 引数を指定します::

   >>> parser = argparse.ArgumentParser(prog='PROG', prefix_chars='-+')
   >>> parser.add_argument('+f')
   >>> parser.add_argument('++bar')
   >>> parser.parse_args('+f X ++bar Y'.split())
   Namespace(bar='Y', f='X')

``prefix_chars=`` 引数のデフォルトは ``'-'`` です。
``'-'`` を含まない文字集合を指定すると、 ``-f/--foo`` オプションが許可されなくなります。


fromfile_prefix_chars
^^^^^^^^^^^^^^^^^^^^^

ときどき、例えば非常に長い引数リストを扱う場合に、その引数リストを毎回コマンドラインに
タイプする代わりにファイルに置いておきたい場合があります。
:class:`ArgumentParser` のコンストラクタに ``fromfile_prefix_chars=`` 引数が指定された
場合、指定された文字のいずれかで始まる引数はファイルとして扱われ、そのファイルに
含まれる引数リストに置換されます。例えば::

   >>> with open('args.txt', 'w') as fp:
   ...    fp.write('-f\nbar')
   >>> parser = argparse.ArgumentParser(fromfile_prefix_chars='@')
   >>> parser.add_argument('-f')
   >>> parser.parse_args(['-f', 'foo', '@args.txt'])
   Namespace(f='bar')

ファイルから読み込まれる引数は、デフォルトでは1行に1つ(ただし、
:meth:`~ArgumentParser.convert_arg_line_to_args` も参照してください)で、
コマンドライン上でファイルを参照する引数があった場所にその引数があったものとして
扱われます。なので、上の例では、 ``['-f', 'foo', '@args.txt']`` は
``['-f', 'foo', '-f', 'bar']`` と等価になります。

``fromfile_prefix_chars=`` 引数のデフォルト値は ``None`` で、
引数がファイル参照だとして扱われることが無いことを意味しています。


argument_default
^^^^^^^^^^^^^^^^

一般的には、引数のデフォルト値は :meth:`~ArgumentParser.add_argument` メソッドに
デフォルト値を渡すか、 :meth:`~ArgumentParser.set_defaults` メソッドに
name-value ペアを渡すことで指定します。
しかしまれに、1つのパーサー全体に適用されるデフォルト引数が便利なことがあります。
これをするには、 :class:`ArgumentParser` に ``argument_default=`` キーワード
引数を渡します。例えば、全体で :meth:`~ArgumentParser.parse_args` メソッド呼び出しの
属性の生成を抑制するには、 ``argument_default=SUPPRESS`` を指定します::

   >>> parser = argparse.ArgumentParser(argument_default=argparse.SUPPRESS)
   >>> parser.add_argument('--foo')
   >>> parser.add_argument('bar', nargs='?')
   >>> parser.parse_args(['--foo', '1', 'BAR'])
   Namespace(bar='BAR', foo='1')
   >>> parser.parse_args([])
   Namespace()


parents
^^^^^^^

ときどき、いくつかのパーサーが共通の引数セットを共有することがあります。
それらの引数を繰り返し定義する代わりに、全ての共通引数を持った parser を
:class:`ArgumentParser` の ``parents=`` 引数に渡すことができます。
``parents=`` 引数は :class:`ArgumentParser` オブジェクトのリストを受け取り、
全ての位置アクションとオプションのアクションをそれらから集め、
そのアクションを構築中の :class:`ArgumentParser` オブジェクトに追加します::

   >>> parent_parser = argparse.ArgumentParser(add_help=False)
   >>> parent_parser.add_argument('--parent', type=int)

   >>> foo_parser = argparse.ArgumentParser(parents=[parent_parser])
   >>> foo_parser.add_argument('foo')
   >>> foo_parser.parse_args(['--parent', '2', 'XXX'])
   Namespace(foo='XXX', parent=2)

   >>> bar_parser = argparse.ArgumentParser(parents=[parent_parser])
   >>> bar_parser.add_argument('--bar')
   >>> bar_parser.parse_args(['--bar', 'YYY'])
   Namespace(bar='YYY', parent=None)

一番親になるパーサーに ``add_help=False`` を指定していることに注目してください。
こうしないと、 :class:`ArgumentParser` は2つの ``-h/--help`` オプションを
与えられる (1つは親から、もうひとつは子から) ことになり、エラーを発生させます。

.. note::
   ``parents=`` に渡す前にパーサーを完全に初期化する必要があります。
   子パーサーを作成してから親パーサーを変更した場合、その変更は子パーサーに
   反映されません。


formatter_class
^^^^^^^^^^^^^^^

:class:`ArgumentParser` オブジェクトは代わりのフォーマットクラスを指定することで
ヘルプのフォーマットをカスタマイズすることができます。
現在、3つのフォーマットクラスがあります:

.. class:: RawDescriptionHelpFormatter
           RawTextHelpFormatter
           ArgumentDefaultsHelpFormatter

最初の2つは説明のテキストがどう表示されるかについてより制御できるようになっており、
残りの1つは引数のデフォルト値についての情報を自動的に追加します。

デフォルトでは、 :class:`ArgumentParser` オブジェクトはコマンドラインのヘルプ
メッセージ中で description_ と epilog_ を行ラップします::

   >>> parser = argparse.ArgumentParser(
   ...     prog='PROG',
   ...     description='''this description
   ...         was indented weird
   ...             but that is okay''',
   ...     epilog='''
   ...             likewise for this epilog whose whitespace will
   ...         be cleaned up and whose words will be wrapped
   ...         across a couple lines''')
   >>> parser.print_help()
   usage: PROG [-h]

   this description was indented weird but that is okay

   optional arguments:
    -h, --help  show this help message and exit

   likewise for this epilog whose whitespace will be cleaned up and whose words
   will be wrapped across a couple lines

``formatter_class=`` に :class:`~argparse.RawDescriptionHelpFormatter` を渡すと、
description_ と epilog_ がすでに正しくフォーマット済みで、行ラップしてはいけない
ことを指定できます::

   >>> parser = argparse.ArgumentParser(
   ...     prog='PROG',
   ...     formatter_class=argparse.RawDescriptionHelpFormatter,
   ...     description=textwrap.dedent('''\
   ...         Please do not mess up this text!
   ...         --------------------------------
   ...             I have indented it
   ...             exactly the way
   ...             I want it
   ...         '''))
   >>> parser.print_help()
   usage: PROG [-h]

   Please do not mess up this text!
   --------------------------------
      I have indented it
      exactly the way
      I want it

   optional arguments:
    -h, --help  show this help message and exit

:class:`RawTextHelpFormatter` は引数の説明を含めて全ての種類のヘルプテキストで
空白を維持します。

残りの利用できるフォーマットクラスである :class:`ArgumentDefaultsHelpFormatter`
は、各引数のデフォルト値に関する情報を追加します::

   >>> parser = argparse.ArgumentParser(
   ...     prog='PROG',
   ...     formatter_class=argparse.ArgumentDefaultsHelpFormatter)
   >>> parser.add_argument('--foo', type=int, default=42, help='FOO!')
   >>> parser.add_argument('bar', nargs='*', default=[1, 2, 3], help='BAR!')
   >>> parser.print_help()
   usage: PROG [-h] [--foo FOO] [bar [bar ...]]

   positional arguments:
    bar         BAR! (default: [1, 2, 3])

   optional arguments:
    -h, --help  show this help message and exit
    --foo FOO   FOO! (default: 42)


conflict_handler
^^^^^^^^^^^^^^^^

:class:`ArgumentParser` オブジェクトは同じオプション文字列に対して複数のアクションを
許可していません。デフォルトでは、 :class:`ArgumentParser` オブジェクトは、
すでに利用されているオプション文字列を使って新しい引数をつくろうとしたときに
例外を発生させます::

   >>> parser = argparse.ArgumentParser(prog='PROG')
   >>> parser.add_argument('-f', '--foo', help='old foo help')
   >>> parser.add_argument('--foo', help='new foo help')
   Traceback (most recent call last):
    ..
   ArgumentError: argument --foo: conflicting option string(s): --foo

ときどき (例えば、 parents_ を利用する場合など), 古い引数を同じオプション文字列で
上書きするほうが便利な場合があります。この動作をするには、 :class:`ArgumentParser`
の ``conflict_handler=`` 引数に ``'resolve'`` を渡します::

   >>> parser = argparse.ArgumentParser(prog='PROG', conflict_handler='resolve')
   >>> parser.add_argument('-f', '--foo', help='old foo help')
   >>> parser.add_argument('--foo', help='new foo help')
   >>> parser.print_help()
   usage: PROG [-h] [-f FOO] [--foo FOO]

   optional arguments:
    -h, --help  show this help message and exit
    -f FOO      old foo help
    --foo FOO   new foo help

:class:`ArgumentParser` オブジェクトは、全てのオプション文字列がオーバーライド
サれた場合にだけアクションを削除することに注目してください。上の例では、
``--foo`` オプション文字列だけがオーバーライドされているので、
古い ``-f/--foo`` アクションは ``-f`` アクションとして残っています。


prog
^^^^

デフォルトでは、 :class:`ArgumentParser` オブジェクトはヘルプメッセージ中に表示する
プログラム名を ``sys.argv[0]`` から取得します。このデフォルトの動作は、プログラムが
コマンドライン上でどう起動されたにヘルプメッセージをマッチさせるので、か多くの場合に
正しい挙動です。例えば、 ``myprogram.py`` という名前のファイルに次のコードがあるとします::

   import argparse
   parser = argparse.ArgumentParser()
   parser.add_argument('--foo', help='foo help')
   args = parser.parse_args()

このプログラムのヘルプは、プログラム名として (プログラムがどこから起動されたのかに
関わらず) ``myprogram.py`` を表示します::

   $ python myprogram.py --help
   usage: myprogram.py [-h] [--foo FOO]

   optional arguments:
    -h, --help  show this help message and exit
    --foo FOO   foo help
   $ cd ..
   $ python subdir\myprogram.py --help
   usage: myprogram.py [-h] [--foo FOO]

   optional arguments:
    -h, --help  show this help message and exit
    --foo FOO   foo help

このデフォルトの動作を変更するには、 :class:`ArgumentParser` の ``prog=``
引数に他の値を指定します::

   >>> parser = argparse.ArgumentParser(prog='myprogram')
   >>> parser.print_help()
   usage: myprogram [-h]

   optional arguments:
    -h, --help  show this help message and exit

プログラム名は、 ``sys.argv[0]`` から取られた場合でも ``prog=`` 引数で与えられた場合でも、
ヘルプメッセージ中では ``%(prog)s`` フォーマット指定で利用することができます。

::

   >>> parser = argparse.ArgumentParser(prog='myprogram')
   >>> parser.add_argument('--foo', help='foo of the %(prog)s program')
   >>> parser.print_help()
   usage: myprogram [-h] [--foo FOO]

   optional arguments:
    -h, --help  show this help message and exit
    --foo FOO   foo of the myprogram program


usage
^^^^^

デフォルトでは、 :class:`ArgumentParser` は使用法メッセージを、もっている
引数から生成します::

   >>> parser = argparse.ArgumentParser(prog='PROG')
   >>> parser.add_argument('--foo', nargs='?', help='foo help')
   >>> parser.add_argument('bar', nargs='+', help='bar help')
   >>> parser.print_help()
   usage: PROG [-h] [--foo [FOO]] bar [bar ...]

   positional arguments:
    bar          bar help

   optional arguments:
    -h, --help   show this help message and exit
    --foo [FOO]  foo help

デフォルトのメッセージは ``usage=`` キーワード引数でオーバーライドできます::

   >>> parser = argparse.ArgumentParser(prog='PROG', usage='%(prog)s [options]')
   >>> parser.add_argument('--foo', nargs='?', help='foo help')
   >>> parser.add_argument('bar', nargs='+', help='bar help')
   >>> parser.print_help()
   usage: PROG [options]

   positional arguments:
    bar          bar help

   optional arguments:
    -h, --help   show this help message and exit
    --foo [FOO]  foo help

``%(prog)s`` フォーマット指定子を、使用法メッセージ中でプログラム名として利用できます。


.. The add_argument() method

add_argument() メソッド
-------------------------

.. method:: ArgumentParser.add_argument(name or flags..., [action], [nargs], [const], [default], [type], [choices], [required], [help], [metavar], [dest])

   1つのコマンドライン引数がどう解析されるかを定義します。
   各引数についての解説は以下で行いますが、簡潔には:

   * `name or flags`_ - 名前か、オプション文字列のリスト (例: ``foo`` か
     ``-f, --foo``)

   * action_ - コマンドラインにこの引数があった時のアクション

   * nargs_ - 消費するべきコマンドライン引数の数

   * const_ - いくつかの action_ と nargs_ の組み合わせで利用される定数

   * default_ - コマンドラインに引数がなかった場合に生成される値

   * type_ - コマンドライン引数が変換されるべき型

   * choices_ - 引数として許される値のコンテナ

   * required_ - コマンドラインオプションが省略可能かどうか(オプション引数のみ)

   * help_ - 引数が何なのかを示す簡潔な説明

   * metavar_ - 使用法メッセージの中で使われる引数の名前

   * dest_ - :meth:`parse_args` が返すオブジェクトに追加される属性名

以下のセクションではこれらの使い方を説明します。


name or flags
^^^^^^^^^^^^^

:meth:`~ArgumentParser.add_argument` メソッドは、指定されているのが
``-f`` や ``--foo`` のようなオプション引数なのか、ファイル名リストなどの
位置引数なのかを知る必要があります。そのため、 :meth:`~ArgumentParser.add_argument`
の第1引数は、フラグのリストか、シンプルな引数名のどちらかになります。
例えば、オプション引数は次のようにして作ります::

   >>> parser.add_argument('-f', '--foo')

一方、位置引数は次のようにして作ります::

   >>> parser.add_argument('bar')

:meth:`~ArgumentParser.parse_args` が呼ばれた時、オプション引数は ``-`` prefix
により識別され、それ以外の引数は位置引数として扱われます::

   >>> parser = argparse.ArgumentParser(prog='PROG')
   >>> parser.add_argument('-f', '--foo')
   >>> parser.add_argument('bar')
   >>> parser.parse_args(['BAR'])
   Namespace(bar='BAR', foo=None)
   >>> parser.parse_args(['BAR', '--foo', 'FOO'])
   Namespace(bar='BAR', foo='FOO')
   >>> parser.parse_args(['--foo', 'FOO'])
   usage: PROG [-h] [-f FOO] bar
   PROG: error: too few arguments


action
^^^^^^

:class:`ArgumentParser` オブジェクトはコマンドライン引数にアクションを割り当てます。
このアクションは、割り当てられたコマンドライン引数に関してどんな処理でもできますが、
ほとんどのアクションは単に :meth:`~ArgumentParser.parse_args` が返すオブジェクトに
属性を追加するだけです。 ``action`` キーワード引数は、コマンドライン引数がどう
処理されるかを指定します。サポートされているアクションは:

* ``'store'`` - これは単に引数の値を格納します。これはデフォルトのアクションです。
  例えば:

    >>> parser = argparse.ArgumentParser()
    >>> parser.add_argument('--foo')
    >>> parser.parse_args('--foo 1'.split())
    Namespace(foo='1')

* ``'store_const'`` - このアクションは const_ キーワード引数で指定された値を
  格納します。 (const_ キーワード引数のデフォルト値はあまり役に立たない ``None``
  であることに注意) ``'store_const'`` アクションは、何かの種類のフラグを
  指定するオプション引数によく使われます。例えば::

    >>> parser = argparse.ArgumentParser()
    >>> parser.add_argument('--foo', action='store_const', const=42)
    >>> parser.parse_args('--foo'.split())
    Namespace(foo=42)

* ``'store_true'``, ``'store_false'`` - これらのアクションはそれぞれ ``True``
  と ``False`` を格納します。これらは ``'store_const'`` の特別版になります。
  例えば::

    >>> parser = argparse.ArgumentParser()
    >>> parser.add_argument('--foo', action='store_true')
    >>> parser.add_argument('--bar', action='store_false')
    >>> parser.parse_args('--foo --bar'.split())
    Namespace(bar=False, foo=True)

* ``'append'`` - このアクションはリストを格納して、各引数の値をそのリストに
  追加します。このアクションは複数回指定することができるオプションに便利です。
  利用例::

    >>> parser = argparse.ArgumentParser()
    >>> parser.add_argument('--foo', action='append')
    >>> parser.parse_args('--foo 1 --foo 2'.split())
    Namespace(foo=['1', '2'])

* ``'append_const'`` - このアクションはリストを格納して、 const_ キーワード引数に
  与えられた値をそのリストに追加します。(const_ キーワード引数のデフォルト値は
  あまり役に立たない ``None`` であることに注意) ``'append_const'`` アクションは、
  定数を同じリストに複数回格納する場合に便利です。例えば::

    >>> parser = argparse.ArgumentParser()
    >>> parser.add_argument('--str', dest='types', action='append_const', const=str)
    >>> parser.add_argument('--int', dest='types', action='append_const', const=int)
    >>> parser.parse_args('--str --int'.split())
    Namespace(types=[<type 'str'>, <type 'int'>])

* ``'version'`` - このアクションは :meth:`~ArgumentParser.add_argument` の呼び出しに
  ``version=`` キーワード引数を期待します。指定されたときはバージョン情報を表示して
  終了します。 ::

    >>> import argparse
    >>> parser = argparse.ArgumentParser(prog='PROG')
    >>> parser.add_argument('--version', action='version', version='%(prog)s 2.0')
    >>> parser.parse_args(['--version'])
    PROG 2.0

Action API を実装したオブジェクトを渡すことで、任意のアクションを指定することもできます。
独自のアクションを作る一番手軽な方法は :class:`argparse.Action` を継承して、
適切な ``__call__`` メソッドを実装することです。 ``__call__`` メソッドは
4つの引数を受け取らなければなりません:

* ``parser`` - このアクションを持っている ArgumentParser オブジェクト

* ``namespace`` - :meth:`~ArgumentParser.parse_args` が返す namespace オブジェクト。
  ほとんどのアクションはこのオブジェクトに属性を追加します。

* ``values`` - 型変換が適用された後の、関連付けられたコマンドライン引数。
  (型変換は :meth:`~ArgumentParser.add_argument` メソッドの type_ キーワード引数で
  指定されます)

* ``option_string`` - このアクションを実行したオプション文字列。 ``option_string``
  引数はオプションで、アクションが位置引数に関連付けられた場合は渡されません。

カスタムアクションの例です::

   >>> class FooAction(argparse.Action):
   ...     def __call__(self, parser, namespace, values, option_string=None):
   ...         print '%r %r %r' % (namespace, values, option_string)
   ...         setattr(namespace, self.dest, values)
   ...
   >>> parser = argparse.ArgumentParser()
   >>> parser.add_argument('--foo', action=FooAction)
   >>> parser.add_argument('bar', action=FooAction)
   >>> args = parser.parse_args('1 --foo 2'.split())
   Namespace(bar=None, foo=None) '1' None
   Namespace(bar='1', foo=None) '2' '--foo'
   >>> args
   Namespace(bar='1', foo='2')


nargs
^^^^^

ArgumentParser オブジェクトは通常1つのコマンドライン引数を1つのアクションに渡します。
``nargs`` キーワード引数は1つのアクションにそれ以外の数のコマンドライン引数を
割り当てます。指定できる値は:

* N (整数).  N 個の引数がコマンドラインから集められ、リストに格納されます。
  例えば::

     >>> parser = argparse.ArgumentParser()
     >>> parser.add_argument('--foo', nargs=2)
     >>> parser.add_argument('bar', nargs=1)
     >>> parser.parse_args('c --foo a b'.split())
     Namespace(bar=['c'], foo=['a', 'b'])

  ``nargs=1`` は1要素のリストを作ることに注意してください。これはデフォルトの、
  要素がそのまま属性になる動作とは異なります。

* ``'?'``. 可能なら1つの引数がコマンドラインから取られ、1つのアイテムを作ります。
  コマンドライン引数が存在しない場合、 default_ の値が生成されます。
  オプション引数の場合、さらにオプション引数がしていされ、その後にコマンドライン
  引数が無いというケースもありえます。この場合は const_ の値が生成されます。
  この動作の例です::

     >>> parser = argparse.ArgumentParser()
     >>> parser.add_argument('--foo', nargs='?', const='c', default='d')
     >>> parser.add_argument('bar', nargs='?', default='d')
     >>> parser.parse_args('XX --foo YY'.split())
     Namespace(bar='XX', foo='YY')
     >>> parser.parse_args('XX --foo'.split())
     Namespace(bar='XX', foo='c')
     >>> parser.parse_args(''.split())
     Namespace(bar='d', foo='d')

  ``nargs='?'`` のよくある利用例の1つは、入出力ファイルの指定オプションです::

     >>> parser = argparse.ArgumentParser()
     >>> parser.add_argument('infile', nargs='?', type=argparse.FileType('r'),
     ...                     default=sys.stdin)
     >>> parser.add_argument('outfile', nargs='?', type=argparse.FileType('w'),
     ...                     default=sys.stdout)
     >>> parser.parse_args(['input.txt', 'output.txt'])
     Namespace(infile=<open file 'input.txt', mode 'r' at 0x...>,
               outfile=<open file 'output.txt', mode 'w' at 0x...>)
     >>> parser.parse_args([])
     Namespace(infile=<open file '<stdin>', mode 'r' at 0x...>,
               outfile=<open file '<stdout>', mode 'w' at 0x...>)

* ``'*'``. 全てのコマンドライン引数がリストに集められます。複数の位置引数が
  ``nargs='*'`` を持つことにあまり意味はありませんが、複数のオプション引数が
  ``nargs='*'`` を持つことはありえます。例えば::

     >>> parser = argparse.ArgumentParser()
     >>> parser.add_argument('--foo', nargs='*')
     >>> parser.add_argument('--bar', nargs='*')
     >>> parser.add_argument('baz', nargs='*')
     >>> parser.parse_args('a b --foo x y --bar 1 2'.split())
     Namespace(bar=['1', '2'], baz=['a', 'b'], foo=['x', 'y'])

* ``'+'``. ``'*'`` と同じように、全てのコマンドライン引数をリストに集めます。
  加えて、最低でも1つのコマンドライン引数が存在しない場合にエラーメッセージを
  生成します。例えば::

     >>> parser = argparse.ArgumentParser(prog='PROG')
     >>> parser.add_argument('foo', nargs='+')
     >>> parser.parse_args('a b'.split())
     Namespace(foo=['a', 'b'])
     >>> parser.parse_args(''.split())
     usage: PROG [-h] foo [foo ...]
     PROG: error: too few arguments

``nargs`` キーワード引数が指定されない場合、消費される引数の数は action_ によって
決定されます。通常これは、1つのコマンドライン引数は1つのアイテムになる(リストには
ならない)ことを意味します。


const
^^^^^

:meth:`~ArgumentParser.add_argument` の ``const`` 引数は、コマンドライン引数から
読み込まれないけれども :class:`ArgumentParser` のいくつかのアクションで必要と
される値のために使われます。この引数の2つのよくあるユースケースは:

* :meth:`~ArgumentParser.add_argument` が ``action='store_const'`` か
  ``action='append_const'`` で呼び出された時、これらのアクションは ``const``
  の値を :meth:`~ArgumentParser.parse_args` が返すオブジェクトの属性に追加します。
  サンプルは action_ の解説を参照してください。

* :meth:`~ArgumentParser.add_argument` がオプション文字列 (``-f`` や ``--foo``)
  と ``nargs='?'`` で呼び出された場合。この場合0個か1つのコマンドライン引数を
  取るオプション引数が作られます。オプション引数にコマンドライン引数が続かなかった
  場合、 ``const`` の値が代わりに利用されます。
  サンプルは nargs_ の解説を参照してください。

``const`` キーワード引数のデフォルト値は ``None`` です。


default
^^^^^^^

全てのオプション引数といくつかの位置引数はコマンドライン上で省略される
ことがあります。 :meth:`~ArgumentParser.add_argument` の ``default``
キーワード引数(デフォルト: ``None``)は、コマンドライン引数が存在しなかった
場合に利用する値を指定します。オプション引数では、オプション文字列が
コマンドライン上に存在しなかったときに ``default`` の値が利用されます::

   >>> parser = argparse.ArgumentParser()
   >>> parser.add_argument('--foo', default=42)
   >>> parser.parse_args('--foo 2'.split())
   Namespace(foo='2')
   >>> parser.parse_args(''.split())
   Namespace(foo=42)

位置引数では、 nargs_ ``='?'`` か ``'*'`` で、コマンドライン引数が存在
しなかったときに ``default`` 値が利用されます::

   >>> parser = argparse.ArgumentParser()
   >>> parser.add_argument('foo', nargs='?', default=42)
   >>> parser.parse_args('a'.split())
   Namespace(foo='a')
   >>> parser.parse_args(''.split())
   Namespace(foo=42)


``default=argparse.SUPPRESS`` を渡すと、コマンドライン引数が存在しないときに
属性の追加をしなくなります::

   >>> parser = argparse.ArgumentParser()
   >>> parser.add_argument('--foo', default=argparse.SUPPRESS)
   >>> parser.parse_args([])
   Namespace()
   >>> parser.parse_args(['--foo', '1'])
   Namespace(foo='1')


type
^^^^

デフォルトでは、 ArgumentParser オブジェクトはコマンドライン引数を
単なる文字列として読み込みます。しかし、コマンドラインの文字列は
:class:`float`, :class:`int`, :class:`file` など別の型として
扱うべき事がよくあります。 :meth:`~ArgumentParser.add_argument` の
``type`` キーワード引数により型チェックと型変換を行うことができます。
たくさんのよく使われるビルトイン型を ``type`` 引数の値として直接
指定することができます::

   >>> parser = argparse.ArgumentParser()
   >>> parser.add_argument('foo', type=int)
   >>> parser.add_argument('bar', type=file)
   >>> parser.parse_args('2 temp.txt'.split())
   Namespace(bar=<open file 'temp.txt', mode 'r' at 0x...>, foo=2)

いろいろな種類のファイルを簡単に扱うために、 argparse モジュールは ``mode=``
と ``bufsize=`` 引数を取る FileType ファクトリを提供しています。
例えば、書き込み可能なファイルを作るために ``FileType('w')`` を利用できます::

   >>> parser = argparse.ArgumentParser()
   >>> parser.add_argument('bar', type=argparse.FileType('w'))
   >>> parser.parse_args(['out.txt'])
   Namespace(bar=<open file 'out.txt', mode 'w' at 0x...>)

``type=`` には1つの文字列を引数に受け取って型変換結果を返すような任意の callable
を渡すことができます::

   >>> def perfect_square(string):
   ...     value = int(string)
   ...     sqrt = math.sqrt(value)
   ...     if sqrt != int(sqrt):
   ...         msg = "%r is not a perfect square" % string
   ...         raise argparse.ArgumentTypeError(msg)
   ...     return value
   ...
   >>> parser = argparse.ArgumentParser(prog='PROG')
   >>> parser.add_argument('foo', type=perfect_square)
   >>> parser.parse_args('9'.split())
   Namespace(foo=9)
   >>> parser.parse_args('7'.split())
   usage: PROG [-h] foo
   PROG: error: argument foo: '7' is not a perfect square

さらに、 choices_ キーワード引数を使って、値の範囲をチェックすることもできます::

   >>> parser = argparse.ArgumentParser(prog='PROG')
   >>> parser.add_argument('foo', type=int, choices=xrange(5, 10))
   >>> parser.parse_args('7'.split())
   Namespace(foo=7)
   >>> parser.parse_args('11'.split())
   usage: PROG [-h] {5,6,7,8,9}
   PROG: error: argument foo: invalid choice: 11 (choose from 5, 6, 7, 8, 9)

詳細は choices_ セクションを参照してください。


choices
^^^^^^^

コマンドライン引数をいくつかの選択肢のなかから選ばせたい場合があります。
これは :meth:`~ArgumentParser.add_argument` に ``choices`` キーワード引数を
渡すことで可能です。コマンドラインを解析する時、引数の値がチェックされ、
その値が選択肢の中に含まれていない場合はエラーメッセージを表示します::

   >>> parser = argparse.ArgumentParser(prog='PROG')
   >>> parser.add_argument('foo', choices='abc')
   >>> parser.parse_args('c'.split())
   Namespace(foo='c')
   >>> parser.parse_args('X'.split())
   usage: PROG [-h] {a,b,c}
   PROG: error: argument foo: invalid choice: 'X' (choose from 'a', 'b', 'c')

``choices`` コンテナに含まれているかどうかのチェックは、 type_ による型変換が
実行された後であることに注意してください。なので、 ``choices`` に格納する
オブジェクトの型は指定された type_ にマッチしている必要があります::

   >>> parser = argparse.ArgumentParser(prog='PROG')
   >>> parser.add_argument('foo', type=complex, choices=[1, 1j])
   >>> parser.parse_args('1j'.split())
   Namespace(foo=1j)
   >>> parser.parse_args('-- -4'.split())
   usage: PROG [-h] {1,1j}
   PROG: error: argument foo: invalid choice: (-4+0j) (choose from 1, 1j)

``in`` 演算をサポートしている任意のオブジェクトを ``choices`` に渡すことができます。
なので、 :class:`dict`, :class:`set`, その他カスタムコンテナなどは全てサポート
しています。


required
^^^^^^^^

通常、 :mod:`argparse` モジュールは ``-f`` や ``--bar`` といったフラグは
*オプション(optional)* 引数だと仮定し、コマンドライン上になくても良いものとして
扱います。オプションを *要求(required)* するには、 :meth:`~ArgumentParser.add_argument`
の ``required=`` キーワード引数に ``True`` を指定します::

   >>> parser = argparse.ArgumentParser()
   >>> parser.add_argument('--foo', required=True)
   >>> parser.parse_args(['--foo', 'BAR'])
   Namespace(foo='BAR')
   >>> parser.parse_args([])
   usage: argparse.py [-h] [--foo FOO]
   argparse.py: error: option --foo is required

上の例のように、オプションが ``required`` と指定されると、 :meth:`~ArgumentParser.parse_args`
はそのオプションがコマンドラインに存在しないときにエラーを表示します。

.. note::

    ユーザーは *option* は *自由に選択できる(optional)* だと期待するので、
    required option は一般的には悪いもので、できる限り避けるべきです。


help
^^^^

``help`` の値はその引数の簡潔な説明を含む文字列です。
ユーザーが(コマンドライン上で ``-h`` か ``--help`` を指定するなどして)
ヘルプを要求したとき、この ``help`` の説明が各引数に表示されます::

   >>> parser = argparse.ArgumentParser(prog='frobble')
   >>> parser.add_argument('--foo', action='store_true',
   ...         help='foo the bars before frobbling')
   >>> parser.add_argument('bar', nargs='+',
   ...         help='one of the bars to be frobbled')
   >>> parser.parse_args('-h'.split())
   usage: frobble [-h] [--foo] bar [bar ...]

   positional arguments:
    bar     one of the bars to be frobbled

   optional arguments:
    -h, --help  show this help message and exit
    --foo   foo the bars before frobbling

``help`` 文字列には、プログラム名や引数の default_ などを繰り返し記述するのを
避けるためのフォーマット指定子を含めることができます。利用できる指定子には、
プログラム名 ``%(prog)s`` と、 ``%(default)s`` や ``%(type)s`` など
:meth:`~ArgumentParser.add_argument` のキーワード引数の多くが含まれます::

   >>> parser = argparse.ArgumentParser(prog='frobble')
   >>> parser.add_argument('bar', nargs='?', type=int, default=42,
   ...         help='the bar to %(prog)s (default: %(default)s)')
   >>> parser.print_help()
   usage: frobble [-h] [bar]

   positional arguments:
    bar     the bar to frobble (default: 42)

   optional arguments:
    -h, --help  show this help message and exit


metavar
^^^^^^^

:class:`ArgumentParser` がヘルプメッセージを出力する時、各引数に対してなんらかの
参照方法が必要です。デフォルトでは、 ArgumentParser オブジェクトは各オブジェクトの
"名前" として dest_ を利用します。デフォルトでは、位置引数には dest_ の値をそのまま
利用し、オプション引数については dest_ の値を大文字に変換して利用します。
なので、1つの ``dest='bar'`` である位置引数は ``bar`` として参照されます。
1つのオプション引数 ``--foo`` が1つのコマンドライン引数を要求するときは、その引数は
``FOO`` として参照されます。例です::

   >>> parser = argparse.ArgumentParser()
   >>> parser.add_argument('--foo')
   >>> parser.add_argument('bar')
   >>> parser.parse_args('X --foo Y'.split())
   Namespace(bar='X', foo='Y')
   >>> parser.print_help()
   usage:  [-h] [--foo FOO] bar

   positional arguments:
    bar

   optional arguments:
    -h, --help  show this help message and exit
    --foo FOO

代わりの名前を、 ``metavar`` として指定できます::

   >>> parser = argparse.ArgumentParser()
   >>> parser.add_argument('--foo', metavar='YYY')
   >>> parser.add_argument('bar', metavar='XXX')
   >>> parser.parse_args('X --foo Y'.split())
   Namespace(bar='X', foo='Y')
   >>> parser.print_help()
   usage:  [-h] [--foo YYY] XXX

   positional arguments:
    XXX

   optional arguments:
    -h, --help  show this help message and exit
    --foo YYY

``metavar`` は *表示される* 名前だけを変更することに注意してください。
:meth:`~ArgumentParser.parse_args` の返すオブジェクトの属性名は dest_
の値のままです。

``nargs`` を指定した場合、 metavar が複数回利用されるかもしれません。
``metavar`` にタプルを渡すと、各引数に対して異なる名前を指定できます::

   >>> parser = argparse.ArgumentParser(prog='PROG')
   >>> parser.add_argument('-x', nargs=2)
   >>> parser.add_argument('--foo', nargs=2, metavar=('bar', 'baz'))
   >>> parser.print_help()
   usage: PROG [-h] [-x X X] [--foo bar baz]

   optional arguments:
    -h, --help     show this help message and exit
    -x X X
    --foo bar baz


dest
^^^^

ほとんどの :class:`ArgumentParser` のアクションは :meth:`~ArgumentParser.parse_args`
が返すオブジェクトに対する属性として値を追加します。
この属性の名前は :meth:`~ArgumentParser.add_argument` の ``dest`` キーワード
引数によって決定されます。位置引数のアクションについては、 ``dest`` は通常
:meth:`~ArgumentParser.add_argument` の第一引数として渡します::

   >>> parser = argparse.ArgumentParser()
   >>> parser.add_argument('bar')
   >>> parser.parse_args('XXX'.split())
   Namespace(bar='XXX')

オプション引数のアクションについては、 ``dest`` の値は通常オプション文字列から
生成されます。 :class:`ArgumentParser` は最初の長いオプション文字列を選択し、
先頭の ``'--'`` を除去することで ``dest`` の値を生成します。
長いオプション文字列が指定されていない場合、最初の短いオプション文字列から
先頭の ``'-'`` 文字を除去することで ``dest`` を生成します。
先頭以外の全ての ``'-'`` 文字は、妥当な属性名になるように ``'_'`` 文字へ
変換されます。次の例はこの動作を示しています::

   >>> parser = argparse.ArgumentParser()
   >>> parser.add_argument('-f', '--foo-bar', '--foo')
   >>> parser.add_argument('-x', '-y')
   >>> parser.parse_args('-f 1 -x 2'.split())
   Namespace(foo_bar='1', x='2')
   >>> parser.parse_args('--foo 1 -y 2'.split())
   Namespace(foo_bar='1', x='2')

``dest`` にカスタムの属性名を与えることも可能です::

   >>> parser = argparse.ArgumentParser()
   >>> parser.add_argument('--foo', dest='bar')
   >>> parser.parse_args('--foo XXX'.split())
   Namespace(bar='XXX')


parse_args() メソッド
-----------------------

.. method:: ArgumentParser.parse_args(args=None, namespace=None)

   引数の文字列をオブジェクトに変換し、 namespace オブジェクトの
   属性に代入します。結果の namespace オブジェクトを返します。

   事前の :meth:`add_argument` メソッドの呼び出しが、どのオブジェクトが
   生成されてどう代入されるかを決定します。
   詳細は :meth:`add_argument` のドキュメントを参照してください。

   デフォルトでは、引数文字列は :data:`sys.argv` から取られ、
   新しい空の :class:`Namespace` オブジェクトが属性のために作られます。


.. Option value syntax

オプション値の文法
^^^^^^^^^^^^^^^^^^^

:meth:`~ArgumentParser.parse_args` メソッドはオプションの値(があれば)
を指定する複数の方法をサポートしています。
一番シンプルな方法は、オプションとその値は2つの別々の引数として渡されます::

   >>> parser = argparse.ArgumentParser(prog='PROG')
   >>> parser.add_argument('-x')
   >>> parser.add_argument('--foo')
   >>> parser.parse_args('-x X'.split())
   Namespace(foo=None, x='X')
   >>> parser.parse_args('--foo FOO'.split())
   Namespace(foo='FOO', x=None)

長いオプション (1文字よりも長い名前を持ったオプション) では、
オプションとその値は ``=`` で区切られた1つのコマンドライン引数として
渡すこともできます::

   >>> parser.parse_args('--foo=FOO'.split())
   Namespace(foo='FOO', x=None)

短いオプション (1文字のオプション) では、オプションとその値は連結して渡す
ことができます::

   >>> parser.parse_args('-xX'.split())
   Namespace(foo=None, x='X')

複数の短いオプションは、最後の1つ(か、0個)のオプションだけが値を
要求する場合には、1つの ``-`` prefix だけで連結することができます::

   >>> parser = argparse.ArgumentParser(prog='PROG')
   >>> parser.add_argument('-x', action='store_true')
   >>> parser.add_argument('-y', action='store_true')
   >>> parser.add_argument('-z')
   >>> parser.parse_args('-xyzZ'.split())
   Namespace(x=True, y=True, z='Z')


.. Invalid arguments

不正な引数
^^^^^^^^^^^^^^^^^

:meth:`~ArgumentParser.parse_args` は、コマンドラインの解析中に、
曖昧なオプション、不正な型、不正なオプション、位置引数の数の不一致などの
エラーを検証します。
それらのエラーが発生した場合、エラーメッセージと使用法メッセージを
表示して終了します::

   >>> parser = argparse.ArgumentParser(prog='PROG')
   >>> parser.add_argument('--foo', type=int)
   >>> parser.add_argument('bar', nargs='?')

   >>> # invalid type
   >>> parser.parse_args(['--foo', 'spam'])
   usage: PROG [-h] [--foo FOO] [bar]
   PROG: error: argument --foo: invalid int value: 'spam'

   >>> # invalid option
   >>> parser.parse_args(['--bar'])
   usage: PROG [-h] [--foo FOO] [bar]
   PROG: error: no such option: --bar

   >>> # wrong number of arguments
   >>> parser.parse_args(['spam', 'badger'])
   usage: PROG [-h] [--foo FOO] [bar]
   PROG: error: extra arguments found: badger


.. Arguments containing ``"-"``

``"-"`` を含む引数
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:meth:`~ArgumentParser.parse_args` メソッドは、ユーザーが明らかなミスを
した場合はエラーを表示しますが、いくつか本質的に曖昧な場面があります。
例えば、コマンドライン引数 ``'-1'`` は、オプションの指定かもしれませんし
位置引数かもしれません。 :meth:`~ArgumentParser.parse_args` メソッドは
これを次のように扱います: 負の数として解釈でき、パーサーに負の数のように
解釈できるオプションが存在しない場合にのみ、 ``'-'`` で始まる位置引数
になりえます::

   >>> parser = argparse.ArgumentParser(prog='PROG')
   >>> parser.add_argument('-x')
   >>> parser.add_argument('foo', nargs='?')

   >>> # no negative number options, so -1 is a positional argument
   >>> parser.parse_args(['-x', '-1'])
   Namespace(foo=None, x='-1')

   >>> # no negative number options, so -1 and -5 are positional arguments
   >>> parser.parse_args(['-x', '-1', '-5'])
   Namespace(foo='-5', x='-1')

   >>> parser = argparse.ArgumentParser(prog='PROG')
   >>> parser.add_argument('-1', dest='one')
   >>> parser.add_argument('foo', nargs='?')

   >>> # negative number options present, so -1 is an option
   >>> parser.parse_args(['-1', 'X'])
   Namespace(foo=None, one='X')

   >>> # negative number options present, so -2 is an option
   >>> parser.parse_args(['-2'])
   usage: PROG [-h] [-1 ONE] [foo]
   PROG: error: no such option: -2

   >>> # negative number options present, so both -1s are options
   >>> parser.parse_args(['-1', '-1'])
   usage: PROG [-h] [-1 ONE] [foo]
   PROG: error: argument -1: expected one argument

``'-'`` で始まる位置引数があって、それが負の数として解釈できない場合、
ダミーの引数 ``'--'`` を挿入して、 :meth:`~ArgumentParser.parse_args` に
それ以降の全てが位置引数だと教えることができます::

   >>> parser.parse_args(['--', '-f'])
   Namespace(foo='-f', one=None)


.. Argument abbreviations

引数の短縮形
^^^^^^^^^^^^^^^^^^^^^^

:meth:`~ArgumentParser.parse_args` メソッドは、長いオプションを、
曖昧さが無い範囲で短縮することを許可しています::

   >>> parser = argparse.ArgumentParser(prog='PROG')
   >>> parser.add_argument('-bacon')
   >>> parser.add_argument('-badger')
   >>> parser.parse_args('-bac MMM'.split())
   Namespace(bacon='MMM', badger=None)
   >>> parser.parse_args('-bad WOOD'.split())
   Namespace(bacon=None, badger='WOOD')
   >>> parser.parse_args('-ba BA'.split())
   usage: PROG [-h] [-bacon BACON] [-badger BADGER]
   PROG: error: ambiguous option: -ba could match -badger, -bacon

引数が複数のオプションになり得る場合はエラーになります。


.. Beyond ``sys.argv``

``sys.argv`` 以外
^^^^^^^^^^^^^^^^^^^

ArgumentParser が :data:`sys.argv` 以外の引数をパースできると役に立つ場合があります。
その場合は文字列のリストを :meth:`~ArgumentParser.parse_args` に渡します。
これはインタラクティブプロンプトからテストするときに便利です::

   >>> parser = argparse.ArgumentParser()
   >>> parser.add_argument(
   ...     'integers', metavar='int', type=int, choices=xrange(10),
   ...  nargs='+', help='an integer in the range 0..9')
   >>> parser.add_argument(
   ...     '--sum', dest='accumulate', action='store_const', const=sum,
   ...   default=max, help='sum the integers (default: find the max)')
   >>> parser.parse_args(['1', '2', '3', '4'])
   Namespace(accumulate=<built-in function max>, integers=[1, 2, 3, 4])
   >>> parser.parse_args('1 2 3 4 --sum'.split())
   Namespace(accumulate=<built-in function sum>, integers=[1, 2, 3, 4])


.. The Namespace object

Namespace オブジェクト
^^^^^^^^^^^^^^^^^^^^^^^

デフォルトでは、 :meth:`~ArgumentParser.parse_args` は :class:`Namespace`
の新しいオブジェクトに必要な属性を設定して返します。このクラスはシンプルに
設計されており、単に読みやすい文字列表現を持った :class:`object` のサブクラスです。
もし属性を辞書のように扱える方が良ければ、 :func:`vars` を使う標準的な
Python のイディオムを利用することができます::


   >>> parser = argparse.ArgumentParser()
   >>> parser.add_argument('--foo')
   >>> args = parser.parse_args(['--foo', 'BAR'])
   >>> vars(args)
   {'foo': 'BAR'}

:class:`ArgumentParser` が、新しい :class:`Namespace` オブジェクトではなく、
既存のオブジェクトに属性を設定する方が良い場合があります。
これは ``namespace=`` キーワード引数を指定することで可能です::

   >>> class C(object):
   ...     pass
   ...
   >>> c = C()
   >>> parser = argparse.ArgumentParser()
   >>> parser.add_argument('--foo')
   >>> parser.parse_args(args=['--foo', 'BAR'], namespace=c)
   >>> c.foo
   'BAR'


.. Other utilities

その他のユーティリティ
------------------------

.. Sub-commands

サブコマンド
^^^^^^^^^^^^^^

.. method:: ArgumentParser.add_subparsers()

   多くのプログラムは、その機能をサブコマンドへと分割します。
   例えば ``svn`` プログラムは ``svn checkout``, ``svn update``, ``svn commit``
   などのサブコマンドを利用できます。
   機能をサブコマンドに分割するのは、プログラムがいくつかの異なった機能を持っていて、
   それぞれが異なるコマンドライン引数を必要とする場合には良いアイデアです。
   :class:`ArgumentParser` は :meth:`add_subparsers` メソッドによりサブコマンドを
   サポートしています。 :meth:`add_subparsers` メソッドは通常引数なしに呼び出され、
   特殊なアクションオブジェクトを返します。このオブジェクトには1つのメソッド
   :meth:`~ArgumentParser.add_parser` があり、コマンド名と :class:`ArgumentParser`
   コンストラクタの任意の引数を受け取り、通常の方法で操作できる
   :class:`ArgumentParser` オブジェクトを返します。

   いくつかの使用例::

     >>> # create the top-level parser
     >>> parser = argparse.ArgumentParser(prog='PROG')
     >>> parser.add_argument('--foo', action='store_true', help='foo help')
     >>> subparsers = parser.add_subparsers(help='sub-command help')
     >>>
     >>> # create the parser for the "a" command
     >>> parser_a = subparsers.add_parser('a', help='a help')
     >>> parser_a.add_argument('bar', type=int, help='bar help')
     >>>
     >>> # create the parser for the "b" command
     >>> parser_b = subparsers.add_parser('b', help='b help')
     >>> parser_b.add_argument('--baz', choices='XYZ', help='baz help')
     >>>
     >>> # parse some arg lists
     >>> parser.parse_args(['a', '12'])
     Namespace(bar=12, foo=False)
     >>> parser.parse_args(['--foo', 'b', '--baz', 'Z'])
     Namespace(baz='Z', foo=True)

   :meth:`parse_args` が返すオブジェクトにはメインパーサーとコマンドラインで
   選択されたサブパーサーによる属性だけが設定されており、選択されなかった
   サブコマンドのパーサーの属性が設定されていないことに注意してください。
   なので、上の例では、 ``"a"`` コマンドが指定されたときは ``foo``, ``bar``
   属性だけが存在し、 ``"b"`` コマンドが指定されたときは ``foo``, ``baz``
   属性だけが存在しています。

   同じように、サブパーサーにヘルプメッセージが要求された場合は、そのパーサーに
   対するヘルプだけが表示されます。ヘルプメッセージには親パーサーや兄弟パーサーの
   ヘルプメッセージを表示しません。
   (ただし、各サブパーサーコマンドのヘルプメッセージは、上の例にもあるように
   :meth:`add_parser` の ``help=`` 引数によって指定できます)

   ::

     >>> parser.parse_args(['--help'])
     usage: PROG [-h] [--foo] {a,b} ...

     positional arguments:
       {a,b}   sub-command help
     a     a help
     b     b help

     optional arguments:
       -h, --help  show this help message and exit
       --foo   foo help

     >>> parser.parse_args(['a', '--help'])
     usage: PROG a [-h] bar

     positional arguments:
       bar     bar help

     optional arguments:
       -h, --help  show this help message and exit

     >>> parser.parse_args(['b', '--help'])
     usage: PROG b [-h] [--baz {X,Y,Z}]

     optional arguments:
       -h, --help     show this help message and exit
       --baz {X,Y,Z}  baz help

   :meth:`add_subparsers` メソッドは ``title`` と ``description`` キーワード
   引数もサポートしています。どちらかが存在する場合、サブパーサーのコマンドは
   ヘルプ出力でそれぞれのグループの中に表示されます。例えば::

     >>> parser = argparse.ArgumentParser()
     >>> subparsers = parser.add_subparsers(title='subcommands',
     ...                                    description='valid subcommands',
     ...                                    help='additional help')
     >>> subparsers.add_parser('foo')
     >>> subparsers.add_parser('bar')
     >>> parser.parse_args(['-h'])
     usage:  [-h] {foo,bar} ...

     optional arguments:
       -h, --help  show this help message and exit

     subcommands:
       valid subcommands

       {foo,bar}   additional help


   サブコマンドを扱う1つの便利な方法は :meth:`add_subparsers` メソッドと
   :meth:`set_defaults` を組み合わせて、各サブパーサーにどの Python 関数を
   実行するかを教えることです。例えば::

     >>> # sub-command functions
     >>> def foo(args):
     ...     print args.x * args.y
     ...
     >>> def bar(args):
     ...     print '((%s))' % args.z
     ...
     >>> # create the top-level parser
     >>> parser = argparse.ArgumentParser()
     >>> subparsers = parser.add_subparsers()
     >>>
     >>> # create the parser for the "foo" command
     >>> parser_foo = subparsers.add_parser('foo')
     >>> parser_foo.add_argument('-x', type=int, default=1)
     >>> parser_foo.add_argument('y', type=float)
     >>> parser_foo.set_defaults(func=foo)
     >>>
     >>> # create the parser for the "bar" command
     >>> parser_bar = subparsers.add_parser('bar')
     >>> parser_bar.add_argument('z')
     >>> parser_bar.set_defaults(func=bar)
     >>>
     >>> # parse the args and call whatever function was selected
     >>> args = parser.parse_args('foo 1 -x 2'.split())
     >>> args.func(args)
     2.0
     >>>
     >>> # parse the args and call whatever function was selected
     >>> args = parser.parse_args('bar XYZYX'.split())
     >>> args.func(args)
     ((XYZYX))

   こうすると、 :meth:`parse_args` が引数の解析が終わってから適切な関数を
   呼び出すようになります。このように関数をアクションに関連付けるのは大抵
   サブパーサーごとに異なるアクションを扱う最も簡単な方法です。
   ただし、実行されたサブパーサーの名前を確認する必要がある場合は、
   :meth:`add_subparsers` を呼び出すときに ``dest`` キーワードを指定する
   ことができます::

     >>> parser = argparse.ArgumentParser()
     >>> subparsers = parser.add_subparsers(dest='subparser_name')
     >>> subparser1 = subparsers.add_parser('1')
     >>> subparser1.add_argument('-x')
     >>> subparser2 = subparsers.add_parser('2')
     >>> subparser2.add_argument('y')
     >>> parser.parse_args(['2', 'frobble'])
     Namespace(subparser_name='2', y='frobble')


.. FileType objects

FileType オブジェクト
^^^^^^^^^^^^^^^^^^^^^^

.. class:: FileType(mode='r', bufsize=None)

   :class:`FileType` ファクトリは :meth:`ArgumentParser.add_argument` の
   type 引数に渡すことができるオブジェクトを生成します。
   type が :class:`FileType` オブジェクトである引数はコマンドライン引数を、
   指定されたモードとバッファサイズでファイルとして開きます:

   >>> parser = argparse.ArgumentParser()
   >>> parser.add_argument('--output', type=argparse.FileType('wb', 0))
   >>> parser.parse_args(['--output', 'out'])
   Namespace(output=<open file 'out', mode 'wb' at 0x...>)

   FileType オブジェクトは擬似引数 ``'-'`` を識別し、読み込み用の :class:`FileType`
   であれば ``sys.stdin`` を、書き込み用の :class:`FileType` であれば ``sys.stdout``
   に変換します:

   >>> parser = argparse.ArgumentParser()
   >>> parser.add_argument('infile', type=argparse.FileType('r'))
   >>> parser.parse_args(['-'])
   Namespace(infile=<open file '<stdin>', mode 'r' at 0x...>)


.. Argument groups

引数グループ
^^^^^^^^^^^^^^^

.. method:: ArgumentParser.add_argument_group(title=None, description=None)

   デフォルトでは、 :class:`ArgumentParser` はヘルプメッセージを表示するときに、
   コマンドライン引数を "positional arguments"(位置引数) と
   "optional arguments"(オプション引数) にグループ化します。
   このデフォルトの動作よりも良い引数のグループ化方法がある場合、
   :meth:`add_argument_group` メソッドで適切なグループを作成できます::

     >>> parser = argparse.ArgumentParser(prog='PROG', add_help=False)
     >>> group = parser.add_argument_group('group')
     >>> group.add_argument('--foo', help='foo help')
     >>> group.add_argument('bar', help='bar help')
     >>> parser.print_help()
     usage: PROG [--foo FOO] bar

     group:
       bar    bar help
       --foo FOO  foo help

   :meth:`add_argument_group` メソッドは、通常の :class:`ArgumentParser`
   と同じような :meth:`~ArgumentParser.add_argument` メソッドを持つ
   引数グループオブジェクトを返します。
   引数がグループに追加された時、パーサーはその引数を通常の引数のように扱いますが、
   ヘルプメッセージではその引数を分離されたグループの中に表示します。
   :meth:`add_argument_group` メソッドには、この表示をカスタマイズするための
   *title* と *description* 引数があります::

     >>> parser = argparse.ArgumentParser(prog='PROG', add_help=False)
     >>> group1 = parser.add_argument_group('group1', 'group1 description')
     >>> group1.add_argument('foo', help='foo help')
     >>> group2 = parser.add_argument_group('group2', 'group2 description')
     >>> group2.add_argument('--bar', help='bar help')
     >>> parser.print_help()
     usage: PROG [--bar BAR] foo

     group1:
       group1 description

       foo    foo help

     group2:
       group2 description

       --bar BAR  bar help

   ユーザー定義グループ以外の全ての引数は通常の "positional arguments" と
   "optional arguments" セクションに表示されます。


.. Mutual exclusion

相互排他
^^^^^^^^^^^^

.. method:: add_mutually_exclusive_group(required=False)

   相互排他グループを作ります。 :mod:`argparse` は相互排他グループの中で
   ただ1つの引数のみが存在することを確認します::

     >>> parser = argparse.ArgumentParser(prog='PROG')
     >>> group = parser.add_mutually_exclusive_group()
     >>> group.add_argument('--foo', action='store_true')
     >>> group.add_argument('--bar', action='store_false')
     >>> parser.parse_args(['--foo'])
     Namespace(bar=True, foo=True)
     >>> parser.parse_args(['--bar'])
     Namespace(bar=False, foo=False)
     >>> parser.parse_args(['--foo', '--bar'])
     usage: PROG [-h] [--foo | --bar]
     PROG: error: argument --bar: not allowed with argument --foo

   :meth:`add_mutually_exclusive_group` メソッドは、その相互排他引数のどれか
   1つを選ぶことが要求されることを示す *required* 引数を取ります::

     >>> parser = argparse.ArgumentParser(prog='PROG')
     >>> group = parser.add_mutually_exclusive_group(required=True)
     >>> group.add_argument('--foo', action='store_true')
     >>> group.add_argument('--bar', action='store_false')
     >>> parser.parse_args([])
     usage: PROG [-h] (--foo | --bar)
     PROG: error: one of the arguments --foo --bar is required

   現在のところ、相互排他引数グループは
   :meth:`~ArgumentParser.add_argument_group` の *title* と *description*
   引数をサポートしていません。


.. Parser defaults

パーサーのデフォルト値
^^^^^^^^^^^^^^^^^^^^^^^^^

.. method:: ArgumentParser.set_defaults(**kwargs)

   ほとんどの場合、 :meth:`parse_args` が返すオブジェクトの属性はコマンドライン
   引数の内容と引数のアクションによってのみ決定されます。 :meth:`set_defaults`
   を使うと与えられたコマンドライン引数の内容によらず追加の属性を決定する
   ことが可能です::

     >>> parser = argparse.ArgumentParser()
     >>> parser.add_argument('foo', type=int)
     >>> parser.set_defaults(bar=42, baz='badger')
     >>> parser.parse_args(['736'])
     Namespace(bar=42, baz='badger', foo=736)

   パーサーレベルのデフォルト値は常に引数レベルのデフォルト値をオーバーライドします::

     >>> parser = argparse.ArgumentParser()
     >>> parser.add_argument('--foo', default='bar')
     >>> parser.set_defaults(foo='spam')
     >>> parser.parse_args([])
     Namespace(foo='spam')

   パーサーレベルの default は、複数のパーサーを扱うときに特に便利です。
   このタイプの例については :meth:`~ArgumentParser.add_subparsers` メソッドを
   参照してください。

.. method:: ArgumentParser.get_default(dest)

   :meth:`~ArgumentParser.add_argument` か :meth:`~ArgumentParser.set_defaults`
   によって指定された、 namespace の属性のデフォルト値を取得します::

     >>> parser = argparse.ArgumentParser()
     >>> parser.add_argument('--foo', default='badger')
     >>> parser.get_default('foo')
     'badger'


.. Printing help

ヘルプの表示
^^^^^^^^^^^^^

ほとんどの典型的なアプリケーションでは、 :meth:`~ArgumentParser.parse_args`
が使用法やエラーメッセージのフォーマットと表示について面倒を見ます。
しかし、いくつかのフォーマットメソッドが利用できます:

.. method:: ArgumentParser.print_usage(file=None)

   :class:`ArgumentParser` がコマンドラインからどう実行されるべきかの
   短い説明を表示します。
   *file* が ``None`` の時は、 :data:`sys.stdout` に出力されます。

.. method:: ArgumentParser.print_help(file=None)

   プログラムの使用法と :class:`ArgumentParser` に登録された引数についての
   情報を含むヘルプメッセージを表示します。
   *file* が ``None`` の時は、 :data:`sys.stdout` に出力されます。

これらのメソッドの、表示する代わりにシンプルに文字列を返すバージョンも
あります:

.. method:: ArgumentParser.format_usage()

   :class:`ArgumentParser` がコマンドラインからどう実行されるべきかの
   短い説明を格納した文字列を返します。

.. method:: ArgumentParser.format_help()

   プログラムの使用法と :class:`ArgumentParser` に登録された引数についての
   情報を含むヘルプメッセージを格納した文字列を返します。


.. Partial parsing

部分解析
^^^^^^^^^^^^^^^

.. method:: ArgumentParser.parse_known_args(args=None, namespace=None)

ときどき、スクリプトがコマンドライン引数のいくつかだけを解析し、残りの引数は
別のスクリプトやプログラムに渡すことがあります。こういった場合、
:meth:`~ArgumentParser.parse_known_args` メソッドが便利です。
これは :meth:`~ArgumentParser.parse_args` と同じように動作しますが、
余分な引数が存在してもエラーを生成しません。代わりに、評価された namespace
オブジェクトと、残りの引数文字列のリストからなる2要素タプルを返します。

::

   >>> parser = argparse.ArgumentParser()
   >>> parser.add_argument('--foo', action='store_true')
   >>> parser.add_argument('bar')
   >>> parser.parse_known_args(['--foo', '--badger', 'BAR', 'spam'])
   (Namespace(bar='BAR', foo=True), ['--badger', 'spam'])


.. Customizing file parsing

ファイル解析のカスタマイズ
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. method:: ArgumentParser.convert_arg_line_to_args(arg_line)

   ファイルから引数を読み込む場合(:class:`ArgumentParser` コンストラクタの
   *fromfile_prefix_chars* キーワード引数を参照)、1行につき1つの引数を
   読み込みます。
   :meth:`convert_arg_line_to_args` をオーバーライドしてこの動作を
   カスタマイズすることができます。

   このメソッドは、引数ファイルから読まれた文字列である1つの引数 *arg_line*
   を受け取ります。そしてその文字列を解析した結果の引数のリストを返します。
   このメソッドはファイルから1行読みこむごとに、順番に呼ばれます。

   このメソッドをオーバーライドする便利な例として、スペース区切りのワードを
   1つの引数として扱います::

    def convert_arg_line_to_args(self, arg_line):
        for arg in arg_line.split():
            if not arg.strip():
                continue
            yield arg


.. Exiting methods

終了メソッド
^^^^^^^^^^^^^^^

.. method:: ArgumentParser.exit(status=0, message=None)

   このメソッドは、 *message* が指定されていればそれを表示した後、
   指定された終了ステータス *status* でプログラムを終了します。

.. method:: ArgumentParser.error(message)

   このメソッドは *message* を含む使用法メッセージを標準出力に表示して、
   終了ステータス 2 でプログラムを終了します。


.. _argparse-from-optparse:

optparse からのアップグレード
--------------------------------

もともと、 :mod:`argparse` モジュールを :mod:`optparse` モジュールとの
互換性を保って開発しようという試みがありました。しかし、特に新しい ``nargs=``
指定子とより良い使用法メッセージのために必要な変更のために、 :mod:`optparse`
を透過的に拡張することは難しかったのです。 :mod:`optparse` のほとんどすべてが
コピーペーストされたりモンキーパッチを当てられたりしたとき、もはや後方互換性を
保とうとすることは現実的ではありませんでした。

:mod:`optparse` から :mod:`argparse` への現実的なアップグレード・パス:

* 全ての :meth:`optparse.OptionParser.add_option` の呼び出しを、
  :meth:`ArgumentParser.add_argument` の呼び出しに置き換える。

* ``options, args = parser.parse_args()`` を ``args = parser.parse_args()``
  に置き換え、位置引数については追加で :meth:`ArgumentParser.add_argument`
  を呼び出す。

* コールバック・アクションと ``callback_*`` キーワード引数を
  ``type`` や ``action`` 引数に置き換える。

* ``type`` キーワード引数に渡していた文字列の名前を、それに応じたオブジェクト
  (例: int, float, complex, ...) に置き換える。

* :class:`optparse.Values` を :class:`Namespace` に置き換え、
  :exc:`optparse.OptionError` と :exc:`optparse.OptionValueError` を
  :exc:`ArgumentError` に置き換える。

* ``%default`` や ``%prog`` などの暗黙の引数を含む文字列を、
  ``%(default)s`` や ``%(prog)s`` などの、通常の Python で辞書を
  使う場合のフォーマット文字列に置き換える。

* OptionParser のコンストラクタの ``version`` 引数を、
  ``parser.add_argument('--version', action='version', version='<the version>')``
  に置き換える。
