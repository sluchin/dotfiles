
:mod:`aetools` --- OSA クライアントのサポート
=============================================

.. module:: aetools
   :platform: Mac
   :synopsis: Apple Eventを送るための基本的なサポート
   :deprecated:
.. sectionauthor:: Jack Jansen <Jack.Jansen@cwi.nl>
.. moduleauthor:: Jack Jansen

:mod:`aetools` モジュールは Python で AppleScript クライアントとして\
の機能をサポートするアプリケーションを構築するための基本的な機能を含ん\
でいます。さらに、このモジュールは、 :mod:`aetypes` および
:mod:`aepack` モジュールの中核機能をインポートし再\
エクスポートします。 :mod:`gensuitemodule` によって生成されたスタブパッケージは
:mod:`aetools` のかなり適切な部分をインポートするので、通常はそれを\
明示的にインポートする必要はありません。生成されたパッケージ群を使用す\
ることができない場合と、スクリプト対応のためにより低いレベルのアクセス\
を必要としている場合、例外が発生します。

:mod:`aetools` モジュールはそれ自身、
:mod:`Carbon.AE` モジュールによって提供される AppleEvent
サポートを利用します。このモジュールにはウィンドウマネージャへのアクセスを必要とする\
という1つの欠点があります。詳細は :ref:`osx-gui-scripts` を見てください。
この制限は将来のリリースで撤廃されるかもしれません。

.. note::

   このモジュールは Python 3.x で削除されました。



:mod:`aetools` モジュールは下記の関数を定義しています。


.. function:: packevent(ae, parameters, attributes)

   あらかじめ作成された ``Carbon.AE.AEDesc`` オブジェクト中のパラメー\
   ターおよび属性を保存します。
   ``parameters`` と ``attributes`` は Python オブジェクトの4文字の
   OSA パラメータのキーを写像した辞書です。このオブジェクトをパックするには
   ``aepack.pack()`` を使います。


.. function:: unpackevent(ae[, formodulename])

   再帰的に、
   ``Carbon.AE.AEDesc`` イベントを Python オブジェクトへアンパックします。
   関数は引数の辞書および属性の辞書を返します。
   ``formodulename`` 引数は AppleScript クラスをどこに捜しに行くか制御\
   するために、生成されたスタブパッケージにより使用されます。


.. function:: keysubst(arguments, keydict)

   Python キーワード引数辞書 ``arguments`` を、写像による4文字の OSA
   キーとして ``keydict`` の中で指定された Python
   識別子であるキーの交換により ``packevent`` によって要求されるフォーマットへ\
   変換します。生成されたパッケージ群によって使用されます。


.. function:: enumsubst(arguments, key, edict)

   ``arguments`` 辞書が ``key`` へのエントリーを含んでいる場合、辞書
   ``edict`` のエントリーに見合う値に変換します。これは人間に判読可\
   能なように Python 列挙名を OSA 4文字のコードに変換します。生成されたパッ\
   ケージ群によって使用されます。

:mod:`aetools` モジュールは次のクラスを定義しています。


.. class:: TalkTo([signature=None, start=0, timeout=0])

   アプリケーションとの対話に利用する代理の基底クラスです。
   ``signature`` はクラス属性 ``_signature`` (サブクラスによって通
   常設定される)を上書きした、対話するアプリケーションを定義する4文字
   クリエートコードです。 ``start`` にはクラスインスタンス上でアプリケーショ
   ンを実行することを可能にするために、真を設定する事ができます。
   ``timeout`` を明示的に設定する事で、 AppleEvent の返答を待つデフォ
   ルトのタイムアウト時間を変更する事ができます。


.. method:: TalkTo._start()

   アプリケーションが起動していてるか確認し、起動していなければ起動しようとします。


.. method:: TalkTo.send(code, subcode[, parameters, attributes])

   OSA指示子  ``code, subcode`` (いずれも通常4文字の文字列です)を持った変数のために、
   ``parameters`` をパックし、
   ``attributes`` に戻し、目標アプリケーションにそれを送って、返答を待ち、
   ``unpackevent`` を含んだ返答をアンパックし、AppleEvent の返答を返し、
   辞書としてアンパックした値と属性を返して、AppleEvent
   ``Carbon.AE.AEDesc`` を作成します。
