
:mod:`autoGIL` --- イベントループ中のグローバルインタープリタの取り扱い
=======================================================================

.. module:: autoGIL
   :platform: Mac
   :synopsis: イベントループ中のグローバルインタープリタの取り扱い
   :deprecated:
.. moduleauthor:: Just van Rossum <just@letterror.com>


:mod:`autoGIL` モジュールは、自動的にイベントループを実行する場合、
Python のグローバルインタープリタロック(:term:`Global Interpreter Lock`)
をロックしたり、ロックの解除をしたりするための関数 :func:`installAutoGIL`
を提供します。

.. note::

   このモジュールは Python 3.x で削除されました。


.. exception:: AutoGILError

   例えば現在のスレッドがループしていないなど、オブザーバにコールバックが\
   できない場合に発生します。


.. function:: installAutoGIL()

   現在のスレッドのイベントループ(CFRunLoop)中のオブザーバにコールバッ\
   クを行ない、適切な時にグローバルインタープリタロック(GIL)を、イ\
   ベントループが使用されていない間、他の Python スレッドの起動がで\
   きるようにロックしたり、ロックの解除をしたりします。

   有効性：OS X 10.1 以降

