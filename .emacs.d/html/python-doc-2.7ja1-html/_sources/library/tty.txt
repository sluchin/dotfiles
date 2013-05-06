
:mod:`tty` --- 端末制御のための関数群
=====================================

.. module:: tty
   :platform: Unix
   :synopsis: 一般的な端末制御操作のためのユーティリティ関数群。
.. moduleauthor:: Steen Lumholt
.. sectionauthor:: Moshe Zadka <moshez@zadka.site.co.il>


:mod:`tty` モジュールは端末を cbreak および raw モードにする\
ための関数を定義しています。

このモジュールは :mod:`termios` モジュールを必要とするため、
Unix でしか動作しません。

:mod:`tty` モジュールでは、以下の関数を定義しています:


.. function:: setraw(fd[, when])

   ファイル記述子 *fd* のモードを raw モードに変えます。
   *when* を省略すると標準の値は :const:`termios.TCSAFLUSH` に\
   なり、 :func:`termios.tcsetattr` に渡されます。


.. function:: setcbreak(fd[, when])

   ファイル記述子 *fd* のモードを cbreakモードに変えます。
   *when* を省略すると標準の値は :const:`termios.TCSAFLUSH`
   になり、 :func:`termios.tcsetattr` に渡されます。


.. seealso::

   :mod:`termios` モジュール
      低レベル端末制御インタフェース。

