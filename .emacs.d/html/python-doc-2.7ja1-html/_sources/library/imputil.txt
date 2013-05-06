
:mod:`imputil` --- Import ユーティリティ
=====================================================

.. module:: imputil
   :synopsis: import 処理の管理と増強
   :deprecated:

.. deprecated:: 2.6
   :mod:`imputil` モジュールは Python 3.0 で削除されました。


.. index:: statement: import

このモジュールは手軽で役に立つ :keyword:`import` フックを改造する機構を提供します。
古い :mod:`ihooks` と比較すると、 :mod:`imputil` は
:keyword:`import` 関数を改造するのに劇的に単純で直截なアプローチをとります。


.. class:: ImportManager([fs_imp])

   import 処理を管理します。

   .. method:: ImportManager.install([namespace])

      この ImportManager を指定された名前空間にインストールします。

   .. method:: ImportManager.uninstall()

      以前の import 機構に戻します。

   .. method:: ImportManager.add_suffix(suffix, importFunc)

      内緒です。


.. class:: Importer()

   標準 import 関数を置き換える基底クラス。

   .. method:: Importer.import_top(name)

      トップレベルのモジュールを import します。

   .. method:: Importer.get_code(parent, modname, fqname)

      与えられたモジュールに対するコードを見つけて取ってきます。

      *parent* は import するコンテキストを定義する親モジュールを指定します。
      ``None`` でも構いません。その場合探すべきコンテキストは特にないことを意味します。

      *modname* は親の内部の (ドットの付かない) 単独のモジュールを指定します。

      *fqname* 完全修飾(fully-qualified)モジュール名を指定します。
      これは (潜在的に) ドット付けられたモジュール名前空間の "root"
      から modname までの名前です。

      parent が無ければ、modname==fqname です。

      このメソッドは ``None`` または3要素タプルを返します。

        * モジュールが見つからなければ ``None`` が返されます。

        * 2または3要素のタプルの最初の要素は整数の 0 か 1 で、
          見つかったモジュールがパッケージかそうでないかを指定します。

        * 2番目の要素はモジュールのコードオブジェクトです
          (このコードは新しいモジュールの名前空間の中で実行されます)。
          この要素はまた完全に読み込まれたモジュールオブジェクト
          (たとえば共有ライブラリから読み込まれてものなど) でもありえます。

        * 3番目の要素はコードオブジェクトが実行される前に新しいモジュールに挿入する\
          名前/値ペアの辞書です。この要素はモジュールのコードが特定の値
          (たとえばどこでモジュールが見つかったかといった)
          を予期している場合に供給されます。
          2番目の要素がモジュールオブジェクトであるときは、
          これらの名前/値はモジュールが読み込まれ/初期化された *後で* 挿入されます。


.. class:: BuiltinImporter()

   ビルトインおよび凍結されたモジュール用の import 機構をエミュレートします。
   :class:`Importer` クラスのサブクラスです。

   .. method:: BuiltinImporter.get_code(parent, modname, fqname)

      内緒です。

.. function:: py_suffix_importer(filename, finfo, fqname)

   内緒です。

.. class:: DynLoadSuffixImporter([desc])

   内緒です。

   .. method:: DynLoadSuffixImporter.import_file(filename, finfo, fqname)

      内緒です。

.. _examples-imputil:

Examples
--------

これは階層的モジュール import の再実装です。

このコードは読むためのもので、実行するためのものではありません。
しかしながら、まあ動きます -- 必要なのは "import knee" することだけです。

(名前はこのモジュールの不格好な前身 "ni" との語呂合わせです)

::

   import sys, imp, __builtin__

   # Replacement for __import__()
   def import_hook(name, globals=None, locals=None, fromlist=None):
       parent = determine_parent(globals)
       q, tail = find_head_package(parent, name)
       m = load_tail(q, tail)
       if not fromlist:
           return q
       if hasattr(m, "__path__"):
           ensure_fromlist(m, fromlist)
       return m

   def determine_parent(globals):
       if not globals or  not globals.has_key("__name__"):
           return None
       pname = globals['__name__']
       if globals.has_key("__path__"):
           parent = sys.modules[pname]
           assert globals is parent.__dict__
           return parent
       if '.' in pname:
           i = pname.rfind('.')
           pname = pname[:i]
           parent = sys.modules[pname]
           assert parent.__name__ == pname
           return parent
       return None

   def find_head_package(parent, name):
       if '.' in name:
           i = name.find('.')
           head = name[:i]
           tail = name[i+1:]
       else:
           head = name
           tail = ""
       if parent:
           qname = "%s.%s" % (parent.__name__, head)
       else:
           qname = head
       q = import_module(head, qname, parent)
       if q: return q, tail
       if parent:
           qname = head
           parent = None
           q = import_module(head, qname, parent)
           if q: return q, tail
       raise ImportError("No module named " + qname)

   def load_tail(q, tail):
       m = q
       while tail:
           i = tail.find('.')
           if i < 0: i = len(tail)
           head, tail = tail[:i], tail[i+1:]
           mname = "%s.%s" % (m.__name__, head)
           m = import_module(head, mname, m)
           if not m:
               raise ImportError("No module named " + mname)
       return m

   def ensure_fromlist(m, fromlist, recursive=0):
       for sub in fromlist:
           if sub == "*":
               if not recursive:
                   try:
                       all = m.__all__
                   except AttributeError:
                       pass
                   else:
                       ensure_fromlist(m, all, 1)
               continue
           if sub != "*" and not hasattr(m, sub):
               subname = "%s.%s" % (m.__name__, sub)
               submod = import_module(sub, subname, m)
               if not submod:
                   raise ImportError("No module named " + subname)

   def import_module(partname, fqname, parent):
       try:
           return sys.modules[fqname]
       except KeyError:
           pass
       try:
           fp, pathname, stuff = imp.find_module(partname,
                                                 parent and parent.__path__)
       except ImportError:
           return None
       try:
           m = imp.load_module(fqname, fp, pathname, stuff)
       finally:
           if fp: fp.close()
       if parent:
           setattr(parent, partname, m)
       return m


   # Replacement for reload()
   def reload_hook(module):
       name = module.__name__
       if '.' not in name:
           return import_module(name, name, None)
       i = name.rfind('.')
       pname = name[:i]
       parent = sys.modules[pname]
       return import_module(name[i+1:], name, parent)


   # Save the original hooks
   original_import = __builtin__.__import__
   original_reload = __builtin__.reload

   # Now install our hooks
   __builtin__.__import__ = import_hook
   __builtin__.reload = reload_hook

.. index::
   module: knee

:mod:`importers` モジュール (Python の配布されているソースの
:file:`Demo/imputil/` の中にあります) ももう一つの例として参照して下さい。

