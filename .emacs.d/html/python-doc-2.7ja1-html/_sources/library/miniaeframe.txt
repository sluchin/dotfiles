
:mod:`MiniAEFrame` --- オープンスクリプティングアーキテクチャサーバのサポート
=============================================================================

.. module:: MiniAEFrame
   :platform: Mac
   :synopsis: オープンスクリプティングアーキテクチャ(OSA)サーバ("Apple Events")のサポート。


.. index::
   single: Open Scripting Architecture
   single: AppleEvents
   module: FrameWork

:mod:`MiniAEFrame` モジュールは、アプリケーションにオープンスクリプティング\
アーキテクチャ(OSA)サーバ機能を持たせるためのフレームワークを提供します。
つまり、AppleEvents の受信と処理を行わせます。
:mod:`FrameWork` と連携させても良いし、単独でも使えます。
実例として、このモジュールは :program:`PythonCGISlave` の中で使われています。

:mod:`MiniAEFrame` には以下のクラスが定義されています。


.. class:: AEServer()

   AppleEvent の分岐を処理するクラス。作成するアプリケーションはこのクラスと、
   :class:`MiniApplication` あるいは :class:`FrameWork.Application`
   のサブクラスでなければなりません。サブクラス化したクラスでは :meth:`__init__`
   メソッドで、継承した両方のクラスの :meth:`__init__` メソッドを呼びださ\
   なければなりません。


.. class:: MiniApplication()

   :class:`FrameWork.Application` とある程度互換なクラスですが、機能は少ない\
   です。このクラスのイベントループはアップルメニュー、
   Cmd-.(コマンドキーを押しながらピリオド.を押す)、
   AppleEvent をサポートします。他のイベントは Python インタープリタか
   Sioux（CodeWarrior のコンソールシステム）に渡されます。
   作成するアプリケーションで :class:`AEServer` を使いたいが、
   独自のウィンドウなどを持たない場合に便利です。


.. _aeserver-objects:

AEServer オブジェクト
---------------------


.. method:: AEServer.installaehandler(classe, type, callback)

   AppleEvent ハンドラをインストールします。
   *classe* と *type* は4文字の OSA クラスとタイプの指定子で、
   ワイルドカード ``'****'`` も使えます。対応する AppleEvent
   を受けるとパラメータがデコードされ、与えたコールバックが呼び出されます。


.. method:: AEServer.callback(_object, **kwargs)

   与えたコールバックは、OSAダイレクトオブジェクトを1番目のパラメータとして\
   呼び出されます。他のパラメータは4文字の指定子を名前にしたキーワード引数\
   として渡されます。他に3つのキーワード・パラメータが渡されます。つまり、
   ``_class`` と ``_type`` はクラスとタイプ指定子で、
   ``_attributes`` は AppleEvent 属性を持つ辞書です。

   与えたメソッドの返り値は :func:`aetools.packevent` でパックされ、
   リプライとして送られます。

現在のクラス設計にはいくつか重大な問題があることに注意してください。
引数に名前ではない4文字の指定子を持つ AppleEvent はまだ実装されていないし、
イベントの送信側にエラーを返すこともできません。
この問題は将来のリリースまで先送りにされています。

