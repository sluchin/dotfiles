.. _ast:

抽象構文木
==========

.. module:: ast
   :synopsis: Abstract Syntax Tree classes and manipulation.
.. sectionauthor:: Martin v. Löwis <martin@v.loewis.de>
.. sectionauthor:: Georg Brandl <georg@python.org>

.. versionadded:: 2.5
   低級モジュール ``_ast`` はノード・クラスだけ含みます。

.. versionadded:: 2.6
   高級モジュール ``ast`` は全てのヘルパーを含みます。


:mod:`ast` モジュールは、Python アプリケーションで Python の抽象構文木を処理しやすくするものです。
抽象構文そのものは、Python のリリースごとに変化する可能性があります。
このモジュールを使用すると、現在の文法をプログラム上で知る助けになるでしょう。

抽象構文木を作成するには、 :data:`ast.PyCF_ONLY_AST` を組み込み関数 :func:`compile` のフラグとして渡すか、あるいはこのモジュールで提供されているヘルパー関数 :func:`parse` を使います。
その結果は、 :class:`ast.AST` を継承したクラスのオブジェクトのツリーとなります。
抽象構文木は組み込み関数 :func:`compile` を使って Python コード・オブジェクトにコンパイルすることができます。


.. seealso::

   最新バージョンの `ast module Python source code
   <http://svn.python.org/view/python/branches/release27-maint/Lib/ast.py?view=markup>`_

Node クラス
--------------

.. class:: AST

   このクラスは全ての AST ノード・クラスの基底です。
   実際のノード・クラスは :ref:`後ほど <abstract-grammar>` 示す
   :file:`Parser/Python.asdl` ファイルから派生したものです。
   これらのクラスは :mod:`_ast` C モジュールで定義され、
   :mod:`ast` にもエクスポートし直されています。

   抽象文法の左辺のシンボル一つずつにそれぞれ一つのクラスがあります
   (たとえば :class:`ast.stmt` や :class:`ast.expr`)。
   それに加えて、右辺のコンストラクタ一つずつにそれぞれ一つのクラスがあり、
   これらのクラスは左辺のツリーのクラスを継承しています。
   たとえば、 :class:`BinOp` は :class:`ast.expr` から継承しています。
   代替を伴った生成規則 (production rules with alternatives) (別名 "sums")
   の場合、左辺は抽象クラスとなり、
   特定のコンストラクタ・ノードのインスタンスのみが作成されます。

   .. production rules with alternatives 「代替を伴った生成規則」で良い?

   .. attribute:: _fields

      各具象クラスは属性 :attr:`_fields` を持っており、すべての子ノードの名前を\
      そこに保持しています。

      具象クラスのインスタンスは、各子ノードに対してそれぞれひとつの属性を持っています。
      この属性は、文法で定義された型となります。
      たとえば :class:`ast.BinOp` のインスタンスは :attr:`left`
      という属性を持っており、その型は :class:`ast.expr` です。

      これらの属性が、文法上 (クエスチョンマークを用いて) オプションであると\
      マークされている場合は、その値が ``None`` となることもあります。
      属性が0個以上の複数の値をとりうる場合 (アスタリスクでマークされている場合) は、
      値は Python のリストで表されます。
      全ての属性は AST を :func:`compile` でコンパイルする際には存在しなければならず、
      そして妥当な値でなければなりません。

   .. attribute:: lineno
                  col_offset

      :class:`ast.expr` や :class:`ast.stmt` のサブクラスのインスタンスにはさらに
      :attr:`lineno` や :class:`col_offset` といった属性があります。
      :attr:`lineno` はソーステキスト上の行番号 (1 から数え始めるので、最初の\
      行の行番号は 1 となります)、そして :attr:`col_offset` はノードが生成した\
      最初のトークンの UTF-8 バイトオフセットとなります。
      UTF-8 オフセットが記録される理由は、パーサが内部で UTF-8 を使用するからです。

   クラス :class:`ast.T` のコンストラクタは引数を次のように解析します:

   * 位置による引数があるとすれば、 :attr:`T._fields` にあるのと同じだけ\
     の個数が無ければなりません。
     これらの引数はそこにある名前を持った属性として割り当てられます。
   * キーワード引数があるとすれば、それらはその名前の属性にその値を割り当てられます。

   たとえば、 :class:`ast.UnaryOp` ノードを生成して属性を埋めるには、
   次のようにすることができます::

      node = ast.UnaryOp()
      node.op = ast.USub()
      node.operand = ast.Num()
      node.operand.n = 5
      node.operand.lineno = 0
      node.operand.col_offset = 0
      node.lineno = 0
      node.col_offset = 0

   もしくはよりコンパクトにも書けます::

      node = ast.UnaryOp(ast.USub(), ast.Num(5, lineno=0, col_offset=0),
                         lineno=0, col_offset=0)

   .. versionadded:: 2.6
      上で説明したコンストラクタが付け加えられました。
      Python 2.5 においてはノードは引数なしのコンストラクタを呼び出して生成した後、
      全ての属性をセットしていかなければなりませんでした。


.. _abstract-grammar:

抽象文法 (Abstract Grammar)
---------------------------

このモジュールでは文字列定数 ``__version__`` を定義しています。これは、以下に示すファイルの Subversion リビジョン番号です。

抽象文法は、現在次のように定義されています。


.. literalinclude:: ../includes/Python.asdl

.. 元の記述位置は ../../Parser/Python.asdl ですがツリー構成の都合上 includes に
   コピーしました。
   *注意!* 今後も(バージョンが上がるたびに)コピーしてこなければなりません。


:mod:`ast` ヘルパー
-------------------

.. versionadded:: 2.6

ノード・クラスの他に、 :mod:`ast` モジュールは以下のような抽象構文木をトラバースするためのユーティリティ関数やクラスも定義しています。

.. function:: parse(source, filename='<unknown>', mode='exec')

   *source* を解析して AST ノードにします。
   ``compile(source, filename, mode, ast.PyCF_ONLY_AST)`` と等価です。


.. function:: literal_eval(node_or_string)

   式ノードまたは Python の式を表す文字列を安全に評価します。
   与えられる文字列またはノードは次のリテラルのみからなるものに限られます:
   文字列, 数, タプル, リスト, 辞書, ブール値, ``None`` 。

   この関数は Python の式を含んだ必ずしも信用できない出どころからの文字列を、
   自身での値の解析を要さずに、安全に評価するのに使えます。


.. function:: get_docstring(node, clean=True)

   与えられた *node* (これは :class:`FunctionDef`, :class:`ClassDef`,
   :class:`Module` のいずれかのノードでなければなりません) のドキュメント文字列\
   を返します。
   もしドキュメント文字列が無ければ ``None`` を返します。
   *clean* が真ならば、ドキュメント文字列のインデントを :func:`inspect.cleandoc`
   を用いて一掃します。


.. function:: fix_missing_locations(node)

   :func:`compile` はノード・ツリーをコンパイルする際、 :attr:`lineno` と
   :attr:`col_offset` 両属性をサポートする全てのノードに対しそれが存在するものと\
   想定します。
   生成されたノードに対しこれらを埋めて回るのはどちらかというと退屈な作業なので、
   このヘルパーが再帰的に二つの属性がセットされていないものに親ノードと同じ値をセット\
   していきます。
   再帰の出発点が *node* です。


.. function:: increment_lineno(node, n=1)

   *node* から始まるツリーの全てのノードの行番号を *n* ずつ増やします。
   これはファイルの中で別の場所に「コードを動かす」ときに便利です。


.. function:: copy_location(new_node, old_node)

   ソースの場所 (:attr:`lineno` と :attr:`col_offset`) を *old_node* から
   *new_node* に可能ならばコピーし、 *new_node* を返します。


.. function:: iter_fields(node)

   *node* にある ``node._fields`` のそれぞれのフィールドを ``(フィールド名, 値)``
   のタプルとして yield します。


.. function:: iter_child_nodes(node)

   *node* の直接の子ノード全てを yield します。
   すなわち、 yield されるのは、ノードであるような全てのフィールドおよびノードの\
   リストであるようなフィールドの全てのアイテムです。

   .. 合ってる?


.. function:: walk(node)

   *node* の全ての子孫ノード(*node* 自体を含む)を再帰的に yield します。
   順番は決められていません。
   この関数はノードをその場で変更するだけで文脈を気にしないような場合に便利です。


.. class:: NodeVisitor()

   抽象構文木を渡り歩いてビジター関数を見つけたノードごとに呼び出す\
   ノード・ビジターの基底クラスです。
   この関数は :meth:`visit` メソッドに送られる値を返してもかまいません。

   このクラスはビジター・メソッドを付け加えたサブクラスを派生させることを意図しています。

   .. method:: visit(node)

      ノードを訪れます。
      デフォルトの実装では :samp:`self.visit_{classname}` というメソッド
      (ここで *classname* はノードのクラス名です) を呼び出すか、
      そのメソッドがなければ :meth:`generic_visit` を呼び出します。

   .. method:: generic_visit(node)

      このビジターはノードの全ての子について :meth:`visit` を呼び出します。

      注意して欲しいのは、専用のビジター・メソッドを具えたノードの子ノードは、
      このビジターが :meth:`generic_visit` を呼び出すかそれ自身で子ノードを\
      訪れない限り訪れられないということです。

   トラバースの途中でノードを変化させたいならば :class:`NodeVisitor` を使ってはいけません。
   そうした目的のために変更を許す特別なビジター (:class:`NodeTransformer`) があります。


.. class:: NodeTransformer()

   :class:`NodeVisitor` のサブクラスで抽象構文木を渡り歩きながらノードを変更することを\
   許すものです。

   :class:`NodeTransformer` は抽象構文木(AST)を渡り歩き、ビジター・メソッドの\
   戻り値を使って古いノードを置き換えたり削除したりします。
   ビジター・メソッドの戻り値が ``None`` ならば、ノードはその場から取り去られ、
   そうでなければ戻り値で置き換えられます。
   置き換えない場合は戻り値が元のノードそのものであってもかまいません。

   それでは例を示しましょう。
   Name (たとえば ``foo``) を見つけるたび全て ``data['foo']`` に書き換える変換器
   (transformer) です::

      class RewriteName(NodeTransformer):

          def visit_Name(self, node):
              return copy_location(Subscript(
                  value=Name(id='data', ctx=Load()),
                  slice=Index(value=Str(s=node.id)),
                  ctx=node.ctx
              ), node)

   操作しようとしているノードが子ノードを持つならば、その子ノードの変形も自分で行うか、
   またはそのノードに対し最初に :meth:`generic_visit` メソッドを呼び出すか、
   それを行うのはあなたの責任だということを肝に銘じましょう。

   文のコレクションであるようなノード (全ての文のノードが当てはまります) に対して、
   このビジターは単独のノードではなくノードのリストを返すかもしれません。

   たいてい、変換器の使い方は次のようになります::

      node = YourTransformer().visit(node)

.. function:: dump(node, annotate_fields=True, include_attributes=False)

   *node* 中のツリーのフォーマットされたダンプを返します。
   主な使い道はデバッグです。
   返される文字列は名前とフィールドの値を表示します。
   これを使うとコードは評価できなくなりますので、
   評価が必要ならば *annotated_fields* に ``False`` をセットしなければなりません。
   行番号や列オフセットのような属性はデフォルトではダンプされません。
   これが欲しければ、 *include_attributes* を ``True`` にセットすることができます。
