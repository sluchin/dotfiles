
:mod:`mutex` --- 排他制御
=========================

.. module:: mutex
   :synopsis: 排他制御のためのロックとキュー
.. sectionauthor:: Moshe Zadka <moshez@zadka.site.co.il>
   :deprecated:

.. deprecated:: 2.6
   :mod:`mutex` モジュールは Python 3.0 で削除されました。


:mod:`mutex` モジュールでは、ロック (lock) の獲得と解除によって排他制御を可能にするクラスを定義しています。
排他制御は :mod:`threading` やマルチタスクを使う上で便利かもしれませんが、このクラスがそうした機能を必要として(いたり、想定して)いるわけではありません。

:mod:`mutex` モジュールでは以下のクラスを定義しています:


.. class:: mutex()

   新しい (ロックされてない) mutex を作ります。

   mutex には 2 つの状態変数 --- "ロック" ビット (locked bit) とキュー (queue) があります。 mutex
   がロックされていなければ、キューは空です。それ以外の場合、キューは空になっているか、 ``(function, argument)``
   のペアが一つ以上入っています。このペアはロックを獲得しようと待機している関数 (またはメソッド) を表しています。キューが空でないときに mutex
   をロック解除すると、キューの先頭のエントリをキューから除去し、そのエントリのペアに基づいて ``function(argument)`` を呼び出します。
   これによって、先頭にあったエントリが新たなロックを獲得します。

   当然のことながらマルチスレッドの制御には利用できません -- というのも、 :meth:`lock` が、ロックを獲得したら関数を呼び出すという
   変なインタフェースだからです。


.. _mutex-objects:

mutex オブジェクト
------------------

:class:`mutex` には以下のメソッドがあります:


.. method:: mutex.test()

   mutex がロックされているかどうか調べます。


.. method:: mutex.testandset()

   「原子的 (Atomic)」な Test-and-Set 操作です。ロックがセットされていなければ獲得して ``True`` を返します。
   それ以外の場合には ``False`` を返します。


.. method:: mutex.lock(function, argument)

   mutex がロックされていなければ ``function(argument)`` を実行します。 mutex
   がロックされている場合、関数とその引数をキューに置きます。キューに置かれた ``function(argument)`` がいつ実行
   されるかについては :meth:`unlock` を参照してください。


.. method:: mutex.unlock()

   キューが空ならば mutex をロック解除します。そうでなければ、キューの最初の要素を実行します。

