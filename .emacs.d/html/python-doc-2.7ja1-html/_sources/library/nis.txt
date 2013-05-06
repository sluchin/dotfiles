
:mod:`nis` --- Sun の NIS (Yellow Pages) へのインタフェース
===========================================================

.. module:: nis
   :platform: Unix
   :synopsis: Sun の NIS (Yellow Pages) ライブラリへのインタフェース。
.. moduleauthor:: Fred Gansevles <Fred.Gansevles@cs.utwente.nl>
.. sectionauthor:: Moshe Zadka <moshez@zadka.site.co.il>


:mod:`nis` モジュールは複数のホストを集中管理する上で便利な NIS
ライブラリを薄くラップします。

NIS は Unix システム上にしかないので、このモジュールは Unix でしか利用できません。

:mod:`nis` モジュールでは以下の関数を定義しています:


.. function:: match(key, mapname[, domain=default_domain])

   *mapname* 中で *key* に一致するものを返すか、見つからない\
   場合にはエラー (:exc:`nis.error`) を送出します。
   両方の引数とも文字列で、 *key* は 8 ビットクリーンです。
   返される値は (``NULL`` その他を含む可能性のある) 任意のバイト列です。

   *mapname* は他の名前の別名になっていないか最初にチェックされます。

   .. versionchanged:: 2.5
      *domain* 引数で参照するNISドメインをオーバーライドできます。
      設定されない場合にはデフォルトのNISドメインを参照します。


.. function:: cat(mapname[, domain=default_domain])

   ``match(key, mapname)==value`` となる
   *key* を *value* に対応付ける辞書を返します。
   辞書内のキーと値は共に任意のバイト列なので注意してください。

   *mapname* は他の名前の別名になっていないか最初にチェックされます。

   .. versionchanged:: 2.5
      *domain* 引数で参照するNISドメインをオーバーライドできます。
      設定されない場合にはデフォルトのNISドメインを参照します。


.. function:: maps()

   有効なマップのリストを返します。


.. function:: get_default_domain()

   システムのデフォルトNISドメインを返します。

   .. versionadded:: 2.5

:mod:`nis` モジュールは以下の例外を定義しています:


.. exception:: error

   NIS 関数がエラーコードを返した場合に送出されます。

