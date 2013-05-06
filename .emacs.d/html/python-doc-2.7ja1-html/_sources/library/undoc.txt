
.. _undoc:

************************************
ドキュメント化されていないモジュール
************************************

現在ドキュメント化されていないが、ドキュメント化すべきモジュールを\
以下にざっと列挙します。どうぞこれらのドキュメントを寄稿してください！
(電子メールで docs@python.org に送ってください)。

この章のアイデアと元の文章内容は Fredrik Lundh のポストによる\
ものです; この章の特定の内容は実際には改訂されてきています。


雑多な有用ユーティリティ
========================

以下のいくつかは非常に古く、かつ／またはあまり頑健ではありません。 "hmm." マーク付きです。

:mod:`ihooks`
   --- import フックのサポートです (:mod:`rexec` のためのものです; 撤廃されるかもしれません)。
   Python 3.0 で削除されました。


プラットフォーム固有のモジュール
================================

これらのモジュールは :mod:`os.path` モジュールを実装するために用いられていますが、
ここで触れる内容を超えてドキュメントされていません。
これらはもう少しドキュメント化する必要があります。

:mod:`ntpath`
   --- Win32、 Win64、 WinCE、および OS/2 プラットフォームにおける :mod:`os.path` 実装です。

:mod:`posixpath`
   --- POSIXにおける :mod:`os.path` 実装です。

:mod:`bsddb185`
   --- まだBerkeleyDB 1.85を使用しているシステムで後方互換性を保つためのモジュール。通常、特定のBSD
   Unixベースのシステムでのみ利用可能。直接使用しないで下さい。


マルチメディア関連
==================

:mod:`audiodev`
   --- 音声データを再生するためのプラットフォーム非依存の API です。
   Python 3.0 で削除されました。

:mod:`linuxaudiodev`
   --- Linux 音声デバイスで音声データを再生します。Python 2.3 では
   :mod:`ossaudiodev` モジュールと置き換えられました。
   Python 3.0 で削除されました。

:mod:`sunaudio`
   --- Sun 音声データヘッダを解釈します (撤廃されるか、ツール/デモに
   なるかもしれません)。
   Python 3.0 で削除されました。

:mod:`toaiff`
   --- "任意の" 音声ファイルを AIFF ファイルに変換します; おそらくツールか
   デモになるはずです。外部プログラム :program:`sox` が必要です。
   Python 3.0 で削除されました。


.. _undoc-mac-modules:

文書化されていない Mac OS モジュール
====================================


:mod:`applesingle` --- AppleSingle デコーダー
---------------------------------------------

.. module:: applesingle
   :platform: Mac
   :synopsis: AppleSingle フォーマットファイル用の基本的なデコーダ
   :deprecated:

.. deprecated:: 2.6


:mod:`buildtools` --- BuildAppletとその仲間のヘルパーモジュール
---------------------------------------------------------------

.. module:: buildtools
   :platform: Mac
   :synopsis: BuildAppletとその仲間のヘルパーモジュール
   :deprecated:


.. deprecated:: 2.4

:mod:`cfmfile` --- コードフラグメントリソースを扱うモジュール
-------------------------------------------------------------

.. module:: cfmfile
   :platform: Mac
   :synopsis: コードフラグメントリソースを扱うモジュール
   :deprecated:


:mod:`cfmfile` は、コードフラグメントと関連する"cfrg"リソースを処\
理するモジュールです。このモジュールでコードフラグメントを分解やマージ\
できて、全てのプラグインモジュールをまとめて、一つの実行可能ファイルに\
するため、BuildApplicationによって利用されます。

.. deprecated:: 2.4

:mod:`icopen` --- :meth:`open` と Internet Config の置き換え
------------------------------------------------------------

.. module:: icopen
   :platform: Mac
   :synopsis: open()と Internet Config の置き換え
   :deprecated:


:mod:`icopen` をインポートすると、組込み :meth:`open` を新しいファイル用に
ファイルタイプおよびクリエーターを設定するために
Internet Configを使用するバージョンに置き換えます。

.. deprecated:: 2.6


:mod:`macerrors` --- MacOSのエラー
----------------------------------

.. module:: macerrors
   :platform: Mac
   :synopsis: 多くの MacOS エラーコード定数定義
   :deprecated:


:mod:`macerrors` は、MacOS エラーコードを意味する定数定義を含みます。

.. deprecated:: 2.6


:mod:`macresource` --- スクリプトのリソースを見つける
-----------------------------------------------------

.. module:: macresource
   :platform: Mac
   :synopsis: スクリプトのリソースを見つける
   :deprecated:


:mod:`macresource` はスクリプトが MacPython 上や MacPython アプレッ\
トおよび OSX Python 上で起動されている時、特別な処理をせずにダイアログ\
やメニューなどのようなリソースを見つけるためのヘルパースクリプトです。

.. deprecated:: 2.6


:mod:`Nav` --- NavServices の呼出し
-----------------------------------

.. module:: Nav
   :platform: Mac
   :synopsis: Navigation Services へのインターフェース
   :deprecated:


Navigation Servicesの低レベルインターフェース。

.. deprecated:: 2.6


:mod:`PixMapWrapper` --- PixMapオブジェクトのラッパー
-----------------------------------------------------

.. module:: PixMapWrapper
   :platform: Mac
   :synopsis: PixMapオブジェクトのラッパー
   :deprecated:


:mod:`PixMapWrapper` はPixMap オブジェクトを Python オブジェクトでラッ\
プしたもので、各フィールドに対し名前でアクセスできるようになります。
:mod:`PIL` 画像との相互の変換をするメソッドも用意されています。

.. deprecated:: 2.6


:mod:`videoreader` --- QuickTime ムービーの読み込み
---------------------------------------------------

.. module:: videoreader
   :platform: Mac
   :synopsis: フレームの継続処理のためのQuickTime ムービーのフレーム読み込み
   :deprecated:


:mod:`videoreader` は QuickTime ムービーを読み込み、デコードし、プロ\
グラムへ渡せます。このモジュールはさらにオーディオトラックをサ\
ポートしています。

.. deprecated:: 2.6


:mod:`W` --- :mod:`FrameWork` 上に作られたウイジェット
------------------------------------------------------

.. module:: W
   :platform: Mac
   :synopsis: FrameWork 上に作られた Mac 用ウイジェット
   :deprecated:


:mod:`W` ウィジェットは、 :program:`IDE` で頻繁に使われています。

.. deprecated:: 2.6


.. _obsolete-modules:

撤廃されたもの
==============

これらのモジュールは通常 import して利用できません; 利用できるようにするには作業を行わなければなりません。

これらの拡張モジュールのうち C で書かれたものは、標準の設定ではビルドされません。Unixでこれらのモジュールを有効にするには、ビルドツリー内の
:file:`Modules/Setup` の適切な行のコメントアウトを外して、モジュールを静的リンクするなら Python をビルドしなおし、
動的にロードされる拡張を使うなら共有オブジェクトをビルドしてインストールする必要があります。

.. (lib-old is empty as of Python 2.5)

   Python で書かれたものは、標準ライブラリの一部としてインストール
   されている \file{lib-old/} ディレクトリの中にインストールされます。
   利用するには、\envvar{PYTHONPATH} を使うなどして、\file{lib-old/}
   ディレクトリを \code{sys.path} に追加しなければなりません。


:mod:`timing`
   --- 高い精度で経過時間を計測します (:func:`time.clock` を使ってください)。
   Python 3.0 で削除されました。


SGI 固有の拡張モジュール
========================

以下は SGI 固有のモジュールで、現在のバージョンの SGI の実情が反映されていないかもしれません。

:mod:`cl`
   --- SGI 圧縮ライブラリへのインタフェースです。

:mod:`sv`
   --- SGI Indigo 上の "simple video" ボード(旧式のハードウェアです) へのインタフェースです。
   Python 3.0 で削除されました。
