:mod:`plistlib` --- Mac OS X ``.plist`` ファイルの生成と解析
================================================================

.. module:: plistlib
   :synopsis: Mac OS X plist ファイルの生成と解析
.. moduleauthor:: Jack Jansen
.. sectionauthor:: Georg Brandl <georg@python.org>
.. (harvested from docstrings in the original file)

.. versionchanged:: 2.6
   このモジュールは以前は Mac 専用ライブラリだけにありましたが、
   全てのプラットフォームで使えるようにしました。

.. index::
   pair: plist; file
   single: property list

このモジュールは主に Mac OS X で使われる「プロパティーリスト」
XML ファイルを読み書きするインターフェイスを提供します。

プロパティーリスト (``.plist``) ファイル形式は基本的型のオブジェクト、\
たとえば辞書やリスト、数、文字列など、に対する単純な XML による保存形式です。
たいてい、トップレベルのオブジェクトは辞書です。

値は文字列、整数、浮動小数点数、ブール型、タプル、リスト、辞書
(ただし文字列だけがキーになれます)、 :class:`Data` または :class:`datetime.datetime`
のオブジェクトです。
文字列の値は(辞書のキーも含めて)ユニコード文字列であって構いません --
それらは UTF-8 で書き出されます。

``<data>`` plist 型は :class:`Data` クラスを通じてサポートされます。
これは Python の文字列に対する薄いラッパです。
文字列に制御文字を含めなければならない場合は :class:`Data` を使って下さい。

.. seealso::

   `PList manual page <http://developer.apple.com/documentation/Darwin/Reference/ManPages/man5/plist.5.html>`_
      このファイル形式の Apple の文書。


このモジュールは以下の関数を定義しています:

.. function:: readPlist(pathOrFile)

   plist ファイルを読み込みます。
   *pathOrFile* はファイル名でも(読み込み可能な)ファイルオブジェクトでも構いません。
   展開されたルートオブジェクト(たいていは辞書です)を返します。

   XML データは :mod:`xml.parsers.expat` にある Expat パーサを使って解析されます
   -- 間違いのある XML に対して送られる可能性のある例外についてはそちらの文書を\
   参照して下さい。
   未知の要素は plist 解析器から単純に無視されます。


.. function:: writePlist(rootObject, pathOrFile)

    *rootObject* を plist ファイルに書き込みます。
    *pathOrFile* はファイル名でも(書き込み可能な)ファイルオブジェクトでも構いません。

    :exc:`TypeError` が、オブジェクトがサポート外の型のものであったり\
    サポート外の型のオブジェクトを含むコンテナだった場合に、送出されます。


.. function:: readPlistFromString(data)

   文字列から plist を読み取ります。ルートオブジェクトを返します。


.. function:: writePlistToString(rootObject)

   *rootObject* を plist 形式の文字列として返します。


.. function:: readPlistFromResource(path[, restype='plst'[, resid=0]])

    *path* のリソースフォークの中の *restype* タイプのリソースから
    plist を読み込みます。使用可能: Mac OS X。

    .. warning::

       3.x では、この関数は削除されています。


.. function:: writePlistToResource(rootObject, path[, restype='plst'[, resid=0]])

    *rootObject* を *path* のリソースフォークの中に *restype*
    タイプのリソースとして書き込みます。使用可能: Mac OS X。

    .. warning::

       3.x では、この関数は削除されてます。


以下のクラスが使用可能です。

.. class:: Data(data)

   文字列 *data* を包むラッパオブジェクトを返します。
   plist 中に入れられる ``<data>`` 型を表すものとして plist への/からの\
   変換関数で使われます。

   これには :attr:`data` という一つの属性があり、そこに収められた
   Python 文字列を取り出すのに使えます。


例
---

plist を作ります::

    pl = dict(
        aString="Doodah",
        aList=["A", "B", 12, 32.1, [1, 2, 3]],
        aFloat = 0.1,
        anInt = 728,
        aDict=dict(
            anotherString="<hello & hi there!>",
            aUnicodeValue=u'M\xe4ssig, Ma\xdf',
            aTrueValue=True,
            aFalseValue=False,
        ),
        someData = Data("<binary gunk>"),
        someMoreData = Data("<lots of binary gunk>" * 10),
        aDate = datetime.datetime.fromtimestamp(time.mktime(time.gmtime())),
    )
    # unicode keys are possible, but a little awkward to use:
    pl[u'\xc5benraa'] = "That was a unicode key."
    writePlist(pl, fileName)

plist を解析します::

    pl = readPlist(pathOrFile)
    print pl["aKey"]
