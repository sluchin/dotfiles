:mod:`new` --- ランタイム内部オブジェクトの作成
===============================================

.. module:: new
   :synopsis: ランタイム実装オブジェクトの作成のインターフェイス。
   :deprecated:

.. deprecated:: 2.6
   :mod:`new` モジュールは Python 3.0 で削除されました。
   代わりに、 :mod:`types` モジュールのクラスを利用してください。

.. sectionauthor:: Moshe Zadka <moshez@zadka.site.co.il>


:mod:`new` モジュールはインタプリタオブジェクト作成関数へのインターフェイスを与えます。新しいオブジェクトを"魔法を使ったように"作り出す必要がある、通常の作成関数が使えないときに、これは主にマーシャル型関数で使われます。このモジュールはインタプリタへの低レベルインターフェイスを提供します。したがって、このモジュールを使うときには注意しなければなりません。
オブジェクトが利用される時にインタプリタをクラッシュさせるような引数を与えることもできてしまいます。

:mod:`new` モジュールは次の関数を定義しています:


.. function:: instance(class[, dict])

   この関数は :meth:`__init__` コンストラクタを呼び出さずに辞書 *dict* をもつ *class* のインスタンスを作り出します。 *dict* が省略されるか、 ``None`` である場合は、新しいインスタンスのために新しい空の辞書が作られます。オブジェクトがいつもと同じ状態であるという保証はないことに注意してください。


.. function:: instancemethod(function, instance, class)

   この関数は *instance* に束縛されたメソッドオブジェクトか、あるいは *instance* が ``None`` の場合に束縛されていないメソッドオブジェクトを返します。 *function* は呼び出し可能でなければなりません。


.. function:: function(code, globals[, name[, argdefs[, closure]]])

   与えられたコードとグローバル変数をもつ(Python)関数を返します。 *name* を与えるならば、文字列か ``None`` でなければならない。文字列の場合は、関数は与えられた名前をもつ。そうでなければ、関数名は ``code.co_name`` から取られる。 *argdefs* を与える場合はタプルでなければならず、パラメータのデフォルト値を決めるために使われます。
   *closure* を与える場合は ``None`` または名前を ``code.co_freevars`` に束縛するセルオブジェクトのタプルである必要があります。

   )が与えられていると、


.. function:: code(argcount, nlocals, stacksize, flags, codestring, constants, names, varnames, filename, name, firstlineno, lnotab)

   この関数は :c:func:`PyCode_New` というC関数へのインターフェイスです。

   .. XXX This is still undocumented!


.. function:: module(name[, doc])

   この関数は *name* という名前の新しいモジュールオブジェクトを返します。 *name* は文字列でなければならない。省略可能な *doc*
   引数はどんな型でもよい。


.. function:: classobj(name, baseclasses, dict)

   この関数は新しいクラスオブジェクトを返します。そのクラスオブジェクトは(クラスのタプルであるべき) *baseclasses* から派生し、名前空間 *dict* を持ち、 *name* という名前です。

