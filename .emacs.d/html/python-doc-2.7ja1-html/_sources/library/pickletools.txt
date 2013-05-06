
:mod:`pickletools` --- pickle 開発者のためのツール群
====================================================

.. versionadded:: 2.3

.. module:: pickletools
   :synopsis: pickle プロトコルと pickle マシン opcode に関する詳しいコメントと、有用な関数がいくつかが入っています。


このモジュールには、 :mod:`pickle` モジュールの詳細に関わる様々な定数や実装に関する長大なコメント、そして pickle 化されたデータ\
を解析する上で有用な関数をいくつか定義しています。
このモジュールの内容は :mod:`pickle` および :mod:`cPickle` の実装に関わっている
Python コア開発者にとって有用なものです; 普通の :mod:`pickle` 利用者にとっては、
:mod:`pickletools` モジュールはおそらく関係ないものでしょう。


.. function:: dis(pickle[, out=None, memo=None, indentlevel=4])

   pickle をシンボル分解 (symbolic disassembly) した内容をファイル類似オブジェクト
   *out* (デフォルトでは ``sys.stdout``) に出力します。
   *pickle* は文字列にもファイル類似オブジェクトにもできます。
   *memo* は Python 辞書型で、 pickle のメモに使われます。
   同じ pickler の生成した複数の pickle 間にわたってシンボル分解を行う場合に使われます。
   ストリーム中で ``MARK`` opcode で表される継続レベル (successive level) は
   *indentlevel* に指定したスペース分インデントされます。


.. function:: genops(pickle)

   pickle 内の全ての opcode を取り出すイテレータ(:term:`iterator`)を返します。
   このイテレータは ``(opcode, arg, pos)`` の三つ組みからなる配列を返します。
   *opcode* は :class:`OpcodeInfo` クラスのインスタンスのクラスです。
   *arg* は *opcode* の引数としてデコードされた Python オブジェクトの値です。
   *pos* は *opcode* の場所を表す値です。
   *pickle* は文字列でもファイル類似オブジェクトでもかまいません。
