
:mod:`dl` --- 共有オブジェクトのC関数の呼び出し
===============================================

.. module:: dl
   :platform: Unix
   :synopsis: 共有オブジェクトのC関数の呼び出し
   :deprecated:

.. deprecated:: 2.6
    :mod:`dl` モジュールは Python 3.0 で削除されました。代わりに :mod:`ctypes`
    モジュールを使ってください。

.. sectionauthor:: Moshe Zadka <moshez@zadka.site.co.il>

:mod:`dl` モジュールは :c:func:`dlopen` 関数へのインタフェースを定義します。
これはダイナミックライブラリにハンドルするための
Unix プラットフォーム上の最も一般的なインタフェースです。
そのライブラリの任意の関数を呼ぶプログラムを与えます。

.. warning::

   :mod:`dl` モジュールは Python の型システムとエラー処理をバイパスしています。
   もし間違って使用すれば、セグメンテーションフォルト、
   クラッシュ、その他の不正な動作を起こします。

.. note::

   このモジュールは ``sizeof(int) == sizeof(long) == sizeof(char *)`` でなければ働きません。
   そうでなければimportするときに :exc:`SystemError` が送出されるでしょう。

:mod:`dl` モジュールは次の関数を定義します:


.. function:: open(name[, mode=RTLD_LAZY])

   共有オブジェクトファイルを開いて、ハンドルを返します。
   モードは遅延結合(:const:`RTLD_LAZY`)または即時結合(:const:`RTLD_NOW`) を表します。
   デフォルトは :const:`RTLD_LAZY` です。
   いくつかのシステムは :const:`RTLD_NOW` をサポートしていないことに注意してください。

   返り値は :class:`dlobject` です。

:mod:`dl` モジュールは次の定数を定義します:


.. data:: RTLD_LAZY

   :func:`.open` の引数として使います。


.. data:: RTLD_NOW

   :func:`.open` の引数として使います。
   即時結合をサポートしないシステムでは、この定数がモジュールに現われないことに注意してください。
   最大のポータビリティを求めるならば、システムが即時結合をサポートするかどうかを決定するために :func:`hasattr` を使用してください。

:mod:`dl` モジュールは次の例外を定義します:


.. exception:: error

   動的なロードやリンクルーチンの内部でエラーが生じたときに送出される例外です。

例::

   >>> import dl, time
   >>> a=dl.open('/lib/libc.so.6')
   >>> a.call('time'), time.time()
   (929723914, 929723914.498)

この例はDebian GNU/Linuxシステム上で行なったもので、
このモジュールの使用はたいてい悪い選択肢であるという事実のよい例です。


.. _dl-objects:

Dlオブジェクト
--------------

:func:`.open` によって返されたDlオブジェクトは次のメソッドを持っています:


.. method:: dl.close()

   メモリーを除く全てのリソースを解放します。


.. method:: dl.sym(name)

   *name* という名前の関数が参照された共有オブジェクトに存在する場合、
   そのポインター(整数値)を返します。存在しない場合 ``None`` を返します。
   これは次のように使えます::

      >>> if a.sym('time'):
      ...     a.call('time')
      ... else:
      ...     time.time()

   (0は *NULL* ポインターであるので、この関数は0でない数を返すだろうということに注意してください)


.. method:: dl.call(name[, arg1[, arg2...]])

   参照された共有オブジェクトの *name* という名前の関数を呼出します。
   引数は、Python整数(そのまま渡される)、Python文字列(ポインターが渡される)、
   ``None`` (*NULL* として渡される)
   のどれかでなければいけません。
   Pythonはその文字列が変化させられるのを好まないので、
   文字列は :c:type:`const char*` として関数に渡されるべきであることに注意してください。

   最大で10個の引数が渡すことができ、与えられない引数は ``None`` として扱われます。
   関数の返り値は C :c:type:`long` (Python整数である)です。

