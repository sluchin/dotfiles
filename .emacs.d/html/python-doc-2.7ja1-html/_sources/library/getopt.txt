
:mod:`getopt` --- C言語スタイルのコマンドラインオプションパーサ
===============================================================

.. module:: getopt
   :synopsis: ポータブルなコマンドラインオプションのパーサ。
              長短の両方の形式をサポートします。

.. note::
   :mod:`getopt` モジュールは、 C言語の :c:func:`getopt` 関数に慣れ親しんだ人の
   ためにデザインされたAPIを持つコマンドラインパーサです。
   C言語の :c:func:`getopt` に慣れ親しんでいない人や、コードを少なくしたい場合、
   より良いヘルプメッセージとエラーメッセージが欲しい場合は、代わりに :mod:`argparse`
   モジュールの利用を検討してください。

このモジュールは ``sys.argv`` に入っているコマンドラインオプションの構文解析を支援します。 '``-``' や '``--``'
の特別扱いも含めて、 Unixの :c:func:`getopt` と同じ記法をサポートしています。 3番目の引数(省略可能)を設定することで、
GNUのソフトウェアでサポートされているような長形式のオプションも利用することができます。

.. A more convenient, flexible, and powerful alternative is the
   :mod:`optparse` module.

より便利で、柔軟性があり、強力な代替として、 :mod:`optparse` モジュールがあります。

このモジュールは2つの関数と1つの例外を提供しています:


.. function:: getopt(args, options[, long_options])

   コマンドラインオプションとパラメータのリストを構文解析します。 *args* は構文解析の対象になる引数リストです。これは
   先頭のプログラム名を除いたもので、通常 ``sys.argv[1:]`` で与えられます。 *options*
   はスクリプトで認識させたいオプション文字と、引数が必要な場合にはコロン(``':'``)をつけます。つまりUnixの
   :c:func:`getopt` と同じフォーマットになります。

   .. note::

      GNUの :c:func:`getopt` とは違って、オプションでない引数の後は全て
      オプションではないと判断されます。これは GNUでない、Unixシステムの挙動に
      近いものです。

   *long_options* は長形式のオプションの名前を示す文字列のリストです。
   名前には、先頭の ``'--'`` は含めません。引数が必要な場合には名前の
   最後に等号(``'='``)を入れます。オプション引数はサポートしていません。
   長形式のオプションだけを受けつけるためには、 *options* は空文字列である
   必要があります。
   長形式のオプションは、該当するオプションを一意に決定できる長さまで入力されて
   いれば認識されます。たとえば、 *long_options* が ``['foo', 'frob']`` の場合、
   ``--fo`` は ``--foo`` にマッチしますが、 ``--f`` では一意に
   決定できないので、 :exc:`GetoptError` が発生します。

   返り値は2つの要素から成っています: 最初は ``(option, value)`` のタプルのリスト、
   2つ目はオプションリストを取り除いたあとに残ったプログラムの引数リストです
   (*args* の末尾部分のスライスになります)。
   それぞれの引数と値のタプルの最初の要素は、短形式の時はハイフン 1つで始まる文字列
   (例: ``'-x'``)、長形式の時はハイフン2つで始まる文字列(例: ``'--long-option'``)
   となり、引数が2番目の要素になります。引数をとらない場合には空文字列が入ります。
   オプションは見つかった順に並んでいて、複数回同じオプションを指定することができます。
   長形式と短形式のオプションは混在させることができます。


.. function:: gnu_getopt(args, options[, long_options])

   この関数はデフォルトでGNUスタイルのスキャンモードを使う以外は :func:`getopt` と同じように動作します。つまり、オプションと
   オプションでない引数とを混在させることができます。 :func:`getopt` 関数はオプションでない引数を見つけると解析をやめてしまいます。

   オプション文字列の最初の文字を ``'+'`` にするか、環境変数
   :envvar:`POSIXLY_CORRECT` を設定することで、
   オプションでない引数を見つけると解析をやめるように振舞いを変えることができます。

   .. versionadded:: 2.3


.. exception:: GetoptError

   引数リストの中に認識できないオプションがあった場合か、引数が必要なオプションに引数が与えられなかった場合に発生します。例外の引数は原因を示す文字
   列です。長形式のオプションについては、不要な引数が与えられた場合にもこ
   の例外が発生します。 :attr:`msg` 属性と :attr:`opt` 属性で、エラーメッセー
   ジと関連するオプションを取得できます。特に関係するオプションが無い場合には :attr:`opt` は空文字列となります。

   .. % This is raised when an unrecognized option is found in the argument
   .. % list or when an option requiring an argument is given none.
   .. % The argument to the exception is a string indicating the cause of the
   .. % error.  For long options, an argument given to an option which does
   .. % not require one will also cause this exception to be raised.  The
   .. % attributes \member{msg} and \member{opt} give the error message and
   .. % related option; if there is no specific option to which the exception
   .. % relates, \member{opt} is an empty string.

   .. versionchanged:: 1.6
      :exc:`GetoptError` は :exc:`error` の別名として導入されました。


.. exception:: error

   :exc:`GetoptError` へのエイリアスです。後方互換性のために残されています。

Unixスタイルのオプションを使った例です:

   >>> import getopt
   >>> args = '-a -b -cfoo -d bar a1 a2'.split()
   >>> args
   ['-a', '-b', '-cfoo', '-d', 'bar', 'a1', 'a2']
   >>> optlist, args = getopt.getopt(args, 'abc:d:')
   >>> optlist
   [('-a', ''), ('-b', ''), ('-c', 'foo'), ('-d', 'bar')]
   >>> args
   ['a1', 'a2']

長形式のオプションを使っても同様です:

   >>> s = '--condition=foo --testing --output-file abc.def -x a1 a2'
   >>> args = s.split()
   >>> args
   ['--condition=foo', '--testing', '--output-file', 'abc.def', '-x', 'a1', 'a2']
   >>> optlist, args = getopt.getopt(args, 'x', [
   ...     'condition=', 'output-file=', 'testing'])
   >>> optlist
   [('--condition', 'foo'), ('--testing', ''), ('--output-file', 'abc.def'), ('-x', '')]
   >>> args
   ['a1', 'a2']

スクリプト中での典型的な使い方は以下のようになります::

   import getopt, sys

   def main():
       try:
           opts, args = getopt.getopt(sys.argv[1:], "ho:v", ["help", "output="])
       except getopt.GetoptError, err:
           # ヘルプメッセージを出力して終了
           print str(err) # will print something like "option -a not recognized"
           usage()
           sys.exit(2)
       output = None
       verbose = False
       for o, a in opts:
           if o == "-v":
               verbose = True
           elif o in ("-h", "--help"):
               usage()
               sys.exit()
           elif o in ("-o", "--output"):
               output = a
           else:
               assert False, "unhandled option"
       # ...

   if __name__ == "__main__":
       main()

:mod:`argparse` モジュールを使えば、より良いヘルプメッセージとエラーメッセージを
持った同じコマンドラインインタフェースをより少ないコードで実現できます。 ::

   import argparse

   if __name__ == '__main__':
       parser = argparse.ArgumentParser()
       parser.add_argument('-o', '--output')
       parser.add_argument('-v', dest='verbose', action='store_true')
       args = parser.parse_args()
       # ... do something with args.output ...
       # ... do something with args.verbose ..

.. seealso::

   Module :mod:`argparse`
      別のコマンドラインオプションと引数の解析ライブラリ.

