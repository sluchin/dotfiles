
:mod:`codeop` --- Pythonコードをコンパイルする
==============================================

.. module:: codeop
   :synopsis: (完全ではないかもしれない)Pythonコードをコンパイルする。
.. sectionauthor:: Moshe Zadka <moshez@zadka.site.co.il>
.. sectionauthor:: Michael Hudson <mwh@python.net>


:mod:`codeop` モジュールは、 :mod:`code` モジュールで行われているようなPythonの
read-eval-printループをエミュレートするユーティリティを提供します。
そのため、このモジュールを直接利用する場面はあまり無いでしょう。
プログラムにこのようなループを含めたい場合は、 :mod:`code` モジュールの方が便利です。

この仕事には二つの部分があります:

#. 入力の一行がPythonの文として完全であるかどうかを見分けられること:
   簡単に言えば、次が '``>>>``' か、あるいは '``...``' かどうかを見分けます。

#. どのfuture文をユーザが入力したのかを覚えていること。したがって、実質的にそれに続く入力をこれらとともにコンパイルすることができます。

:mod:`codeop` モジュールはこうしたことのそれぞれを行う方法とそれら両方を行う方法を提供します。

前者は実行するには:


.. function:: compile_command(source[, filename[, symbol]])

   Pythonコードの文字列であるべき *source* をコンパイルしてみて、
   *source* が有効なPythonコードの場合はコードオブジェクトを返します。
   このような場合、コードオブジェクトのファイル名属性は、デフォルトで
   ``'<input>'`` である *filename* でしょう。
   *source* が有効なPythonコードでは *ない* が、有効なPythonコードの接頭語である場合には、 ``None`` を返します。

   *source* に問題がある場合は、例外を発生させます。
   無効なPython構文がある場合は、 :exc:`SyntaxError` を発生させます。
   また、無効なリテラルがある場合は、 :exc:`OverflowError` または :exc:`ValueError` を発生させます。

   *symbol* 引数は *source* が文としてコンパイルされるか(``'single'`` 、デフォルト)
   、または式としてコンパイルされたかどうかを決定します(``'eval'``)。
   他のどんな値も :exc:`ValueError` を発生させる原因となります。


   .. note::

      ソースの終わりに達する前に、成功した結果をもってパーサは構文解析を止めることがあります。
      このような場合、後ろに続く記号はエラーとならずに無視されます。
      例えば、バックスラッシュの後ろに改行が2つあって、その後ろにゴミがあるかもしれません。
      パーサのAPIがより良くなればすぐに、この挙動は修正されるでしょう。


.. class:: Compile()

   このクラスのインスタンスは組み込み関数 :func:`compile` とシグネチャが一致する
   :meth:`__call__` メソッドを持っていますが、インスタンスが :mod:`__future__`
   文を含むプログラムテキストをコンパイルする場合は、インスタンスは有効なその文と\
   ともに続くすべてのプログラムテキストを'覚えていて'コンパイルするという違いがあります。


.. class:: CommandCompiler()

   このクラスのインスタンスは :func:`compile_command` とシグネチャが一致する
   :meth:`__call__` メソッドを持っています。
   インスタンスが ``__future__`` 文を含むプログラムテキストをコンパイルする場合に、
   インスタンスは有効なその文とともにそれに続くすべてのプログラムテキストを'覚えていて'コンパイルするという違いがあります。

バージョン間の互換性についての注意: :class:`Compile` と :class:`CommandCompiler` はPython
2.2で導入されました。2.2のfuture-tracking機能を有効にするだけでなく、
2.1とPythonのより以前のバージョンとの互換性も保ちたい場合は、次のように書くことができます ::

   try:
       from codeop import CommandCompiler
       compile_command = CommandCompiler()
       del CommandCompiler
   except ImportError:
       from codeop import compile_command

これは影響の小さい変更ですが、あなたのプログラムにおそらく望まれないグローバル状態を導入します。または、次のように書くこともできます::

   try:
       from codeop import CommandCompiler
   except ImportError:
       def CommandCompiler():
           from codeop import compile_command
           return compile_command

そして、新たなコンパイラオブジェクトが必要となるたびに ``CommandCompiler`` を呼び出します。

