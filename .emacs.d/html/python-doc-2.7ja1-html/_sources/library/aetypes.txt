
:mod:`aetypes` --- AppleEvent オブジェクト
==========================================

.. module:: aetypes
   :platform: Mac
   :synopsis: Apple Event オブジェクトモデルの Python 表現
   :deprecated:
.. sectionauthor:: Vincent Marchetti <vincem@en.com>
.. moduleauthor:: Jack Jansen

:mod:`aetypes` では、 Apple Event データデスクリプタ (data descriptor) や Apple
Eventオブジェクト指定子 (object specifier) を表現するクラスを定義しています。

Apple Event データはデスクリプタに含まれていて、これらのデスクリプタは\
型付けされています。多くのデスクリプタは、単に対応するPython の型で
表現されています。例えば、OSA 中の ``typeText`` は Python 文字\
列型で、 ``typeFloat`` は
浮動小数点型になる、といった具合です。
このモジュールでは、OSA の型のうち、直接的に対応する Python の型がない\
もののためにクラスを定義しています。そのようなクラスのインスタンスに対\
するパックやアンパック操作は、 :mod:`aepack` モジュール自動的に処理\
します。

オブジェクト指定子は、本質的には Apple Event サーバ中に実装されている\
オブジェクトへのアドレスです。Apple Event 指定子は、Apple
Event のオブジェクトそのものとして、あるいはオプションパラメタの引数として\
使われます。 :mod:`aetypes` モジュールには OSA
クラスやプロパティを表現するための\
基底クラスが入っています。これらのクラスは、 :mod:`gensuitemodule`
が生成するパッケージ内で、目的に応じてクラスやプロパティを増やす\
際に使われます。

以前のバージョンとの互換性や、スタブパッケージを生成していないような\
アプリケーションをスクリプトで書く必要がある場合のために、\
このモジュールには ``Document`` 、 ``Window`` 、 ``Character`` 、
といったよく使われる OSA
クラスのいくつかを指定できるオブジェクト指定子も入っています。

.. note::

   このモジュールは Python 3.x で削除されました。



:mod:`AEObjects` モジュールでは、以下のようなクラスを定義して、
Apple Event デスクリプタデータを表現しています:


.. class:: Unknown(type, data)

   :mod:`aepack` や :mod:`aetypes` がサポートしていない OSA の
   デスクリプタデータ、すなわち、このモジュールで扱っている他のクラスや、
   Python の組み込み型の値で表現されていないようなデータを表現するクラスです。


.. class:: Enum(enum)

   列挙値 (enumeration value) を表すクラスです。
   値は 4 文字の文字列型になります。


.. class:: InsertionLoc(of, pos)

   オブジェクト ``of`` の中の ``pos`` の位置を表すクラスです。


.. class:: Boolean(bool)

   ブール値 (真偽値) を表すクラスです。


.. class:: StyledText(style, text)

   スタイル情報 (フォント、タイプフェイスなど) つきのテキストを表す\
   クラスです。


.. class:: AEText(script, style, text)

   スクリプトシステム (script system) およびスタイル情報の入った\
   テキストを表すクラスです。


.. class:: IntlText(script, language, text)

   スクリプトシステムと言語情報 (language information) の入った\
   テキストを表すクラスです。


.. class:: IntlWritingCode(script, language)

   スクリプトシステムと言語情報を表すクラスです。


.. class:: QDPoint(v, h)

   QuickDrawの点を表すクラスです。


.. class:: QDRectangle(v0, h0, v1, h1)

   QuickDrawの矩形を表すクラスです。


.. class:: RGBColor(r, g, b)

   色を表すクラスです。


.. class:: Type(type)

   OSA の型 (type value) を表すクラスです。 4 文字からなる名前を値に持ちます。


.. class:: Keyword(name)

   OSA のキーワードです。 4 文字からなる名前を値に持ちます。


.. class:: Range(start, stop)

   範囲を表すクラスです。


.. class:: Ordinal(abso)

   先頭を表す ``"firs"`` や中央を表す ``"midd"`` のように、
   数値でない絶対位置指定子を表すクラスです。


.. class:: Logical(logc, term)

   演算子 ``logc`` を ``term`` に適用したときの論理式を\
   表すクラスです。


.. class:: Comparison(obj1, relo, obj2)

   ``obj1`` と ``obj2`` の ``relo`` による比較を表すクラスです。

以下のクラスは、生成されたスタブパッケージが、 AppleScript のクラスやプロパティを Python で表現する上で基底クラスとして利用します。


.. class:: ComponentItem(which[, fr])

   OSA クラス用の抽象基底クラスです。サブクラスでは、クラス属性 ``want`` を
   4 文字の OSA クラスコードに設定せねばなりません。
   このクラスのサブクラスのインスタンスは AppleScript オブジェクト指定子\
   と同じになります。インスタンス化を行う最には、
   ``which`` にセレクタを渡さねばなりません。また、任意で親オブジェクトを
   ``fr`` に渡せます。


.. class:: NProperty(fr)

   OSA プロパティ用の抽象基底クラスです。サブクラスでは、クラス属性
   ``want`` と ``which`` を設定して、どのプロパティを表しているかを\
   指定せねばなりません。このクラスのサブクラスのインスタンスは\
   オブジェクト指定子と同じになります。


.. class:: ObjectSpecifier(want, form, seld[, fr])

   ``ComponentItem`` と ``NProperty`` の基底クラスで、汎用の OSA
   オブジェクト指定子を表します。パラメタの説明は Apple
   Open Scripting Architecture のドキュメントを参照してください。
   このクラスは抽象クラスではないので注意してください。
