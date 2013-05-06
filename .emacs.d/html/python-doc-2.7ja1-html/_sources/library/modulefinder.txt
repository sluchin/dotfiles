
:mod:`modulefinder` --- スクリプト中で使われているモジュールを検索する
=======================================================================

.. sectionauthor:: A.M. Kuchling <amk@amk.ca>


.. module:: modulefinder
   :synopsis: スクリプト中で使われているモジュールを検索します。


.. versionadded:: 2.3

このモジュールでは、スクリプト中で import されているモジュールセットを調べるために使える :class:`ModuleFinder`
クラスを提供しています。 ``modulefinder.py`` はまた、Python スクリプトのファイル名を引数に指定してスクリプトとして実行し、
import されているモジュールのレポートを出力させることもできます。


.. function:: AddPackagePath(pkg_name, path)

   *pkg_name* という名前のパッケージの在り処が *path* であることを記録します。


.. function:: ReplacePackage(oldname, newname)

   実際にはパッケージ内で *oldname* という名前になっているモジュールを *newname* という名前で指定できるようにします。
   この関数の主な用途は、 :mod:`_xmlplus` パッケージが :mod:`xml` パッケージに置き換わっている場合の処理でしょう。


.. class:: ModuleFinder([path=None, debug=0, excludes=[], replace_paths=[]])

   このクラスでは :meth:`run_script` および :meth:`report`  メソッドを提供しています。これらのメソッドは何らかのスクリプト中で
   import されているモジュールの集合を調べます。 *path* はモジュールを検索する先のディレクトリ名からなるリストです。 *path*
   を指定しない場合、 ``sys.path`` を使います。 *debug* にはデバッグレベルを設定します; 値を大きくすると、
   実行している内容を表すデバッグメッセージを出力します。 *excludes* は検索から除外するモジュール名です。 *replace_paths*
   には、モジュールパス内で置き換えられるパスをタプル ``(oldpath, newpath)`` からなるリストで指定します。


   .. method:: report()

      スクリプトで import しているモジュールと、そのパスからなるリストを列挙した\
      レポートを標準出力に出力します。モジュールを見つけられなかったり、
      モジュールがないように見える場合にも報告します。


   .. method:: run_script(pathname)

      *pathname* に指定したファイルの内容を解析します。
      ファイルには Python コードが入っていなければなりません。

   .. attribute:: modules

      モジュール名をモジュールに結びつける辞書。
      :ref:`modulefinder-example` を参照して下さい。


.. _modulefinder-example:

:class:`ModuleFinder` の使用例
--------------------------------------

解析対象のスクリプトはこれ (bacon.py) です::

   import re, itertools

   try:
       import baconhameggs
   except ImportError:
       pass

   try:
       import guido.python.ham
   except ImportError:
       pass


bacon.py のレポートを出力するスクリプトです::

   from modulefinder import ModuleFinder

   finder = ModuleFinder()
   finder.run_script('bacon.py')

   print 'Loaded modules:'
   for name, mod in finder.modules.iteritems():
       print '%s: ' % name,
       print ','.join(mod.globalnames.keys()[:3])

   print '-'*50
   print 'Modules not imported:'
   print '\n'.join(finder.badmodules.iterkeys())


出力例です (アーキテクチャに依って違ってくるかもしれません)::

    Loaded modules:
    _types:
    copy_reg:  _inverted_registry,_slotnames,__all__
    sre_compile:  isstring,_sre,_optimize_unicode
    _sre:
    sre_constants:  REPEAT_ONE,makedict,AT_END_LINE
    sys:
    re:  __module__,finditer,_expand
    itertools:
    __main__:  re,itertools,baconhameggs
    sre_parse:  __getslice__,_PATTERNENDERS,SRE_FLAG_UNICODE
    array:
    types:  __module__,IntType,TypeType
    ---------------------------------------------------
    Modules not imported:
    guido.python.ham
    baconhameggs

