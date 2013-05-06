
:mod:`netrc` --- netrc ファイルの処理
=====================================

.. module:: netrc
   :synopsis: .netrc ファイル群の読み出し。
.. moduleauthor:: Eric S. Raymond <esr@snark.thyrsus.com>
.. sectionauthor:: Eric S. Raymond <esr@snark.thyrsus.com>


.. versionadded:: 1.5.2

:class:`netrc` クラスは、Unix :program:`ftp` プログラムや他の FTP クライアントで用いられる netrc
ファイル形式を解析し、カプセル化 (encapsulate) します。


.. class:: netrc([file])

   :class:`netrc` のインスタンスやサブクラスのインスタンスは netrc ファイルのデータをカプセル化します。初期化の際の引数が存在する
   場合、解析対象となるファイルの指定になります。引数がない場合、ユーザのホームディレクトリ下にある :file:`.netrc` が読み出されます。
   解析エラーが発生した場合、ファイル名、行番号、解析を中断したトークンに関する情報の入った :exc:`NetrcParseError` を送出します。


.. exception:: NetrcParseError

   ソースファイルのテキスト中で文法エラーに遭遇した場合に :class:`netrc`  クラスによって送出される例外です。この例外のインスタンスは 3 つの
   インスタンス変数を持っています: :attr:`msg` はテキストによるエラーの説明で、 :attr:`filename` はソースファイルの名前、そして
   :attr:`lineno` はエラーが発見された行番号です。


.. _netrc-objects:

netrc オブジェクト
------------------

:class:`netrc` インスタンスは以下のメソッドを持っています:


.. method:: netrc.authenticators(host)

   *host* の認証情報として、三要素のタプル  ``(login, account, password)`` を返します。与えられた host
   に対するエントリが netrc ファイルにない場合、 'default' エントリに関連付けられたタプルが返されます。 host
   に対応するエントリがなく、default エントリもない場合、 ``None`` を返します。


.. method:: netrc.__repr__()

   クラスの持っているデータを netrc ファイルの書式に従った文字列で出力します。(コメントは無視され、エントリが並べ替えられる可能性があります。)

:class:`netrc` のインスタンスは以下の公開されたインスタンス変数を持っています:


.. attribute:: netrc.hosts

   ホスト名を ``(login, account, password)`` からなるタプルに対応づけている辞書です。'default' エントリがある場合、
   その名前の擬似ホスト名として表現されます。


.. attribute:: netrc.macros

   マクロ名を文字列のリストに対応付けている辞書です。

.. note::

   利用可能なパスワードの文字セットは、ASCIIのサブセットのみです。2.3より前の
   バージョンでは厳しく制限されていましたが、2.3以降ではASCIIの記号を使用することが
   できます。しかし、空白文字と印刷不可文字を使用することはできません。この制限は .netrcファイルの解析方法によるものであり、将来解除されます。

