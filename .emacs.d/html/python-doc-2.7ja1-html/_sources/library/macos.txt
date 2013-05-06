:mod:`MacOS` --- Mac OS インタプリタ機能へのアクセス
====================================================

.. module:: MacOS
   :platform: Mac
   :synopsis: Mac OS 固有のインタープリタ機能へのアクセス。
   :deprecated:


このモジュールは、Python インタプリタ内の MacOS 固有の機能に対するアク\
セスを提供します。例えば、インタプリタのイベントループ関数などです。十\
分注意して利用してください。

.. note::

   このモジュールは Python 3.x で削除されました。


モジュール名が大文字で始まることに注意してください。これは昔からの約束\
です。


.. data:: runtimemodel

   Python 2.4 以降は常に ``'macho'`` です。それより前のバージョンの
   Python では、古い Mac OS 8 ランタイムモデルの場合は
   ``'ppc'`` 、 Mac OS 9 ランタイムモデルの場合は ``'carbon'`` となります。


.. data:: linkmodel

   インタープリタがどのような方法でリンクされているかを返します。拡張モ\
   ジュールがリンクモデル間で非互換性かもしれない場合、パッケージはより多\
   くの適切なエラーメッセージを伝えるためにこの情報を使用することができま\
   す。値は静的リンクした Python は ``'static'`` 、Mac OS X
   framework  で構築した Python は ``'framework'`` 、標準の Unix
   共有ライブラリ (shared library)で構築された Python は ``'shared'``
   となります。古いバージョンの Python の場合、Mac OS 9 互換の
   Python では ``'cfm'`` となります。


.. exception:: Error

   .. index:: module: macerrors

   MacOS でエラーがあると、このモジュールの関数か、Mac 固有なツールボック\
   スインターフェースモジュールから、この例外が生成されます。引数は、整数\
   エラーコード(:c:data:`OSErr` 値)とテキストで記述されたエラーコードです。
   分かっている全てのエラーコードのシンボル名は、標準モジュール
   :mod:`macerrors` で定義されています。


.. function:: GetErrorString(errno)

   MacOSのエラーコード *errno* のテキスト表現を返します。


.. function:: DebugStr(message [, object])

   Mac OS X上では、文字列を単純に標準出力に送ります (古いバージョンの
   Mac OSでは、より複雑な機能が使用できました)。しかし、低水準のデバッガ
   (:program:`gdb` など) 用にブレークポイントを設定する場所も適切に用意しています。

   .. note::

      64ビットモードでは使用できません。

.. function:: SysBeep()

   ベルを鳴らします。

   .. note::

      64ビットモードでは使用できません。


.. function:: GetTicks()

   システム起動時からのチック数(clock ticks、1/60秒)を得ます。


.. function:: GetCreatorAndType(file)

   2つの4文字の文字列としてファイルクリエータおよびファイルタイプを返しま\
   す。 *file* 引数はパスもしくは、 ``FSSpec`` 、 ``FSRef`` オブジェ\
   クトを与える事ができます。

   .. note::

      ``FSSpec`` は64ビットモードでは使うことができません。


.. function:: SetCreatorAndType(file, creator, type)

   ファイルクリエータおよびファイルタイプを設定します。 *file* 引数は\
   パスもしくは、 ``FSSpec`` 、 ``FSRef`` オブジェクトを与える事ができ\
   ます。 *creator* と *type* は4文字の文字列が必要です。

   .. note::

      ``FSSpec`` は64ビットモードでは使うことができません。

.. function:: openrf(name [, mode])

   ファイルのリソースフォークを開きます。引数は組み込み関数
   :func:`open` と同じです。返されたオブジェクトはファイルのように\
   見えるかもしれませんが、これは Python のファイルオブジェクトではあり\
   ませんので扱いに微妙な違いがあります。


.. function:: WMAvailable()

   現在のプロセスが動作しているウィンドウマネージャにアクセスします。例え\
   ば、Mac OS X サーバー上、あるいは SSH でログインしている、もしくは現在\
   のインタープリタがフルブローンアプリケーションバンドル(fullblown application
   bundle)から起動されていない場合などのような、ウィンドウマネー\
   ジャが存在しない場合は ``False`` を返します。

.. function:: splash([resourceid])

   リソース id でスプラッシュスクリーンを開きます。スプラッシュスクリーンを\
   閉じるには resourceid ``0`` を使います。

   .. note::

      64ビットモードでは使用できません。

