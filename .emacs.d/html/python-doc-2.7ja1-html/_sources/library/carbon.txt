
.. _toolbox:

*******************************
Mac OS ツールボックスモジュール
*******************************

各種の Mac OS ツールボックスへのインターフェースを与えるモジュール群が\
あります。対応するモジュールがあるなら、そのモジュールではツールボックス\
で宣言された各種の構造体の Python オブジェクトが定義され、操作は定義され\
たオブジェクトのメソッドとして実装されています。その他の操作はモジュー\
ルの関数として実装されています。 C で可能な操作がすべて Python で可能な\
わけではありませんし(コールバックはよく問題になります)、パラメータが\
Python だと違ってしまうことはよくあります(特に入力バッファや出力バッファ)。
全てのメソッドと関数は :attr:`__doc__` 文字列があるので、引数と返り値\
の説明を得る事ができます。他の情報源としては、 `Inside Macintosh
<http://developer.apple.com/documentation/macos8/mac8.html>`_
などを参照してください。

これらのモジュールは全て :mod:`Carbon` パッケージに含まれています。
この名前にもかかわらずそれら全てが Carbon フレームワークの一部なわけで\
はありません。CF は、CoreFoundation フレームワークの中に実際はあります\
し、Qt は QuickTime フレームワークにあります。ツールボックスモジュール\
は普通以下のようにして利用します。

::

   from Carbon import AE

.. note::

   Carbon モジュール群は Python 3.0 で削除されました。


:mod:`Carbon.AE` --- Apple Events
=================================

.. module:: Carbon.AE
   :platform: Mac
   :synopsis: Apple Eventツールボックスへのインタフェース
   :deprecated:


:mod:`Carbon.AH` --- Apple ヘルプ
=================================

.. module:: Carbon.AH
   :platform: Mac
   :synopsis: Apple ヘルプマネージャへのインタフェース
   :deprecated:


:mod:`Carbon.App` --- アピアランスマネージャ
============================================

.. module:: Carbon.App
   :platform: Mac
   :synopsis: アピアランスマネージャへのインタフェース
   :deprecated:

:mod:`Carbon.Appearance` --- Appearance Manager 定数
=========================================================

.. module:: Carbon.Appearance
   :platform: Mac
   :synopsis: Appearance Manager のインタフェースのための定数
   :deprecated:


:mod:`Carbon.CF` --- Core Foundation
====================================

.. module:: Carbon.CF
   :platform: Mac
   :synopsis: Core Foundationへのインタフェース
   :deprecated:

``CFBase``, ``CFArray``, ``CFData``, ``CFDictionary``, ``CFString`` と
``CFURL`` オブジェクトがいくらか部分的にサポートされています。


:mod:`Carbon.CG` --- Core Graphics
==================================

.. module:: Carbon.CG
   :platform: Mac
   :synopsis: コア・グラフィックスへのインタフェース
   :deprecated:


:mod:`Carbon.CarbonEvt` --- Carbon Event Manager
================================================

.. module:: Carbon.CaronEvt
   :platform: Mac
   :synopsis: Carbon Event Managerへのインタフェース
   :deprecated:

:mod:`Carbon.CarbonEvents` --- Carbon Event Manager 定数
=============================================================

.. module:: Carbon.CarbonEvents
   :platform: Mac
   :synopsis: Carbon Event Manager のための定数
   :deprecated:


:mod:`Carbon.Cm` --- Component Manager
======================================

.. module:: Carbon.Cm
   :platform: Mac
   :synopsis: Component Managerへのインタフェース
   :deprecated:

:mod:`Carbon.Components` --- Component Manager constants
========================================================

.. module:: Carbon.Components
   :platform: Mac
   :synopsis: Constants for the interface to the Component Manager.
   :deprecated:


:mod:`Carbon.ControlAccessor` --- Control Manager accssors
===========================================================

.. module:: Carbon.ControlAccessor
   :platform: Mac
   :synopsis: Accessor functions for the interface to the Control Manager.
   :deprecated:

:mod:`Carbon.Controls` --- Control Manager constants
====================================================

.. module:: Carbon.Controls
   :platform: Mac
   :synopsis: Constants for the interface to the Control Manager.
   :deprecated:

:mod:`Carbon.CoreFounation` --- CoreFounation constants
=======================================================

.. module:: Carbon.CoreFounation
   :platform: Mac
   :synopsis: Constants for the interface to CoreFoundation.
   :deprecated:

:mod:`Carbon.CoreGraphics` --- CoreGraphics constants
=======================================================

.. module:: Carbon.CoreGraphics
   :platform: Mac
   :synopsis: Constants for the interface to CoreGraphics.
   :deprecated:

:mod:`Carbon.Ctl` --- Control Manager
=====================================

.. module:: Carbon.Ctl
   :platform: Mac
   :synopsis: Control Managerへのインタフェース
   :deprecated:

:mod:`Carbon.Dialogs` --- Dialog Manager constants
==================================================

.. module:: Carbon.Dialogs
   :platform: Mac
   :synopsis: Constants for the interface to the Dialog Manager.
   :deprecated:

:mod:`Carbon.Dlg` --- Dialog Manager
====================================

.. module:: Carbon.Dlg
   :platform: Mac
   :synopsis: Dialog Managerへのインタフェース
   :deprecated:

:mod:`Carbon.Drag` --- Drag and Drop Manager
=============================================

.. module:: Carbon.Drag
   :platform: Mac
   :synopsis: Interface to the Drag and Drop Manager.
   :deprecated:

:mod:`Carbon.Dragconst` --- Drag and Drop Manager constants
===========================================================

.. module:: Carbon.Dragconst
   :platform: Mac
   :synopsis: Constants for the interface to the Drag and Drop Manager.
   :deprecated:

:mod:`Carbon.Events` --- Event Manager constants
================================================

.. module:: Carbon.Events
   :platform: Mac
   :synopsis: Constants for the interface to the classic Event Manager.
   :deprecated:

:mod:`Carbon.Evt` --- Event Manager
===================================

.. module:: Carbon.Evt
   :platform: Mac
   :synopsis: Event Managerへのインタフェース
   :deprecated:

:mod:`Carbon.File` --- File Manager
===================================

.. module:: Carbon.File
   :platform: Mac
   :synopsis: Interface to the File Manager.
   :deprecated:

:mod:`Carbon.Files` --- File Manager constants
==============================================

.. module:: Carbon.Files
   :platform: Mac
   :synopsis: Constants for the interface to the File Manager.
   :deprecated:

:mod:`Carbon.Fm` --- Font Manager
=================================

.. module:: Carbon.Fm
   :platform: Mac
   :synopsis: Font Managerへのインタフェース
   :deprecated:


:mod:`Carbon.Folder` --- Folder Manager
=======================================

.. module:: Carbon.Folder
   :platform: Mac
   :synopsis: Folder Managerへのインタフェース
   :deprecated:

:mod:`Carbon.Folders` --- Folder Manager constants
==================================================

.. module:: Carbon.Folders
   :platform: Mac
   :synopsis: Constants for the interface to the Folder Manager.
   :deprecated:


:mod:`Carbon.Fonts` --- Font Manager constants
==================================================

.. module:: Carbon.Fonts
   :platform: Mac
   :synopsis: Constants for the interface to the Font Manager.
   :deprecated:


:mod:`Carbon.Help` --- Help Manager
===================================

.. module:: Carbon.Help
   :platform: Mac
   :synopsis: Carbon Help Managerへのインタフェース
   :deprecated:

:mod:`Carbon.IBCarbon` --- Carbon InterfaceBuilder
==================================================

.. module:: Carbon.IBCarbon
   :platform: Mac
   :synopsis: Interface to the Carbon InterfaceBuilder support libraries.
   :deprecated:

:mod:`Carbon.IBCarbonRuntime` --- Carbon InterfaceBuilder constants
===================================================================

.. module:: Carbon.IBCarbonRuntime
   :platform: Mac
   :synopsis: Constants for the interface to the Carbon InterfaceBuilder support libraries.
   :deprecated:

:mod:`Carbon.Icn` --- Carbon Icon Manager
=========================================

.. module:: Carbon.Icns
   :platform: Mac
   :synopsis: Interface to the Carbon Icon Manager
   :deprecated:

:mod:`Carbon.Icons` --- Carbon Icon Manager constants
=====================================================

.. module:: Carbon.Icons
   :platform: Mac
   :synopsis: Constants for the interface to the Carbon Icon Manager
   :deprecated:

:mod:`Carbon.Launch` --- Carbon Launch Services
===============================================

.. module:: Carbon.Launch
   :platform: Mac
   :synopsis: Interface to the Carbon Launch Services.
   :deprecated:

:mod:`Carbon.LaunchServices` --- Carbon Launch Services constants
=================================================================

.. module:: Carbon.LaunchServices
   :platform: Mac
   :synopsis: Constants for the interface to the Carbon Launch Services.
   :deprecated:

:mod:`Carbon.List` --- List Manager
===================================

.. module:: Carbon.List
   :platform: Mac
   :synopsis: List Managerへのインタフェース
   :deprecated:


:mod:`Carbon.Lists` --- List Manager constants
==============================================

.. module:: Carbon.Lists
   :platform: Mac
   :synopsis: Constants for the interface to the List Manager.
   :deprecated:

:mod:`Carbon.MacHelp` --- Help Manager constants
================================================

.. module:: Carbon.MacHelp
   :platform: Mac
   :synopsis: Constants for the interface to the Carbon Help Manager.
   :deprecated:

:mod:`Carbon.MediaDescr` --- Parsers and generators for Quicktime Media descriptors
===================================================================================

.. module:: Carbon.MediaDescr
   :platform: Mac
   :synopsis: Parsers and generators for Quicktime Media descriptors
   :deprecated:


:mod:`Carbon.Menu` --- Menu Manager
===================================

.. module:: Carbon.Menu
   :platform: Mac
   :synopsis: Menu Managerへのインタフェース
   :deprecated:

:mod:`Carbon.Menus` --- Menu Manager constants
==============================================

.. module:: Carbon.Menus
   :platform: Mac
   :synopsis: Constants for the interface to the Menu Manager.
   :deprecated:

:mod:`Carbon.Mlte` --- MultiLingual Text Editor
===============================================

.. module:: Carbon.Mlte
   :platform: Mac
   :synopsis: MultiLingual Text Editorへのインタフェース
   :deprecated:

:mod:`Carbon.OSA` --- Carbon OSA Interface
==========================================

.. module:: Carbon.OSA
   :platform: Mac
   :synopsis: Interface to the Carbon OSA Library.
   :deprecated:

:mod:`Carbon.OSAconst` --- Carbon OSA Interface constants
=========================================================

.. module:: Carbon.OSAconst
   :platform: Mac
   :synopsis: Constants for the interface to the Carbon OSA Library.
   :deprecated:

:mod:`Carbon.QDOffscreen` --- QuickDraw Offscreen constants
===========================================================

.. module:: Carbon.QDOffscreen
   :platform: Mac
   :synopsis: Constants for the interface to the QuickDraw Offscreen APIs.
   :deprecated:

:mod:`Carbon.Qd` --- QuickDraw
==============================

.. module:: Carbon.Qd
   :platform: Mac
   :synopsis: QuickDrawツールボックスへのインタフェース
   :deprecated:


:mod:`Carbon.Qdoffs` --- QuickDraw Offscreen
============================================

.. module:: Carbon.Qdoffs
   :platform: Mac
   :synopsis: QuickDrawオフスクリーン APIへのインタフェース
   :deprecated:


:mod:`Carbon.Qt` --- QuickTime
==============================

.. module:: Carbon.Qt
   :platform: Mac
   :synopsis: QuickTime ツールボックスへのインタフェース
   :deprecated:

:mod:`Carbon.QuickDraw` --- QuickDraw constants
===============================================

.. module:: Carbon.QuickDraw
   :platform: Mac
   :synopsis: Constants for the interface to the QuickDraw toolbox.
   :deprecated:

:mod:`Carbon.QuickTime` --- QuickTime constants
===============================================

.. module:: Carbon.QuickTime
   :platform: Mac
   :synopsis: Constants for the interface to the QuickTime toolbox.
   :deprecated:

:mod:`Carbon.Res` --- Resource Manager and Handles
==================================================

.. module:: Carbon.Res
   :platform: Mac
   :synopsis: Resource Managerとハンドルへのインタフェース
   :deprecated:

:mod:`Carbon.Resources` --- Resource Manager and Handles constants
==================================================================

.. module:: Carbon.Resources
   :platform: Mac
   :synopsis: Constants for the interface to the Resource Manager and Handles.
   :deprecated:

:mod:`Carbon.Scrap` --- スクラップマネージャ
============================================

.. module:: Carbon.Scrap
   :platform: Mac
   :synopsis: スクラップマネージャはカット & ペーストとクリップボードの操作の基本的\
              なサービスを提供します。
   :deprecated:


このモジュールは Mac OS 9 とそれ以前の OS 上の Classic PPC MacPython
で完全に利用可能です。
Carbon 版の MacPython ではほんの限られた機能だけが利用可能です。

.. index:: single: Scrap Manager

スクラップマネージャは Macintosh 上でのカット & ペースト操作の最も\
シンプルな形式をサポートします。
アプリケーション間とアプリケーション内での両方のクリップボード操作が可能\
です。

:mod:`Scrap` モジュールはスクラップマネージャの関数へのローレベルでのア\
クセスを提供します。
以下の関数が定義されています：


.. function:: InfoScrap()

   スクラップについて現在の情報を返します。
   この情報は ``(size, handle, count, state, path)``
   を含むタプルでエンコードされます。

   +----------+------------------------------------------------------------------+
   | Field    | Meaning                                                          |
   +==========+==================================================================+
   | *size*   | スクラップのサイズをバイト数で示したもの。                       |
   +----------+------------------------------------------------------------------+
   | *handle* | スクラップを表現するリソースオブジェクト。                       |
   +----------+------------------------------------------------------------------+
   | *count*  | スクラップの内容のシリアルナンバー。                             |
   +----------+------------------------------------------------------------------+
   | *state*  | 整数。メモリー内にあるなら正、ディスク上にあるなら ``0`` 、      |
   |          | 初期化されていないなら負。                                       |
   +----------+------------------------------------------------------------------+
   | *path*   | ディスク上に保存されているなら、そのスクラップのファイルネーム。 |
   +----------+------------------------------------------------------------------+


.. seealso::

   `Scrap Manager <http://developer.apple.com/documentation/mac/MoreToolbox/MoreToolbox-109.html>`_
      Appleのスクラップマネージャに関する文書には、アプリケーションでスクラッ\
      プマネージャを使用する上での便利な情報がたくさんあります。



:mod:`Carbon.Snd` --- Sound Manager
===================================

.. module:: Carbon.Snd
   :platform: Mac
   :synopsis: Sound Managerへのインタフェース
   :deprecated:

:mod:`Carbon.Sound` --- Sound Manager constants
===============================================

.. module:: Carbon.Sound
   :platform: Mac
   :synopsis: Constants for the interface to the Sound Manager.
   :deprecated:

:mod:`Carbon.TE` --- TextEdit
=============================

.. module:: Carbon.TE
   :platform: Mac
   :synopsis: TextEditへのインタフェース
   :deprecated:

:mod:`Carbon.TextEdit` --- TextEdit constants
=============================================

.. module:: Carbon.TextEdit
   :platform: Mac
   :synopsis: Constants for the interface to TextEdit.
   :deprecated:


:mod:`Carbon.Win` --- Window Manager
====================================

.. module:: Carbon.Win
   :platform: Mac
   :synopsis: Window Managerへのインタフェース
   :deprecated:

:mod:`Carbon.Windows` --- Window Manager constants
==================================================

.. module:: Carbon.Windows
   :platform: Mac
   :synopsis: Constants for the interface to the Window Manager.
   :deprecated:
