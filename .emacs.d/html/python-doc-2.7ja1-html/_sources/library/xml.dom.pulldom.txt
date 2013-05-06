
:mod:`xml.dom.pulldom` --- 部分的な DOM ツリー構築のサポート
============================================================

.. module:: xml.dom.pulldom
   :synopsis: SAX イベントからの部分的な DOM ツリー構築のサポート。
.. moduleauthor:: Paul Prescod <paul@prescod.net>


.. versionadded:: 2.0

:mod:`xml.dom.pulldom` では、SAX イベントから、文書の文書オブジェクトモデル表現の選択された一部分だけを構築できるようにします。


.. class:: PullDOM([documentFactory])

   :class:`xml.sax.handler.ContentHandler` 実装です ...


.. class:: DOMEventStream(stream, parser, bufsize)

   ...


.. class:: SAX2DOM([documentFactory])

   :class:`xml.sax.handler.ContentHandler` 実装です ...


.. function:: parse(stream_or_string[, parser[, bufsize]])

   ...


.. function:: parseString(string[, parser])

   ...


.. data:: default_bufsize

   :func:`parse` の *bufsize* パラメタのデフォルト値です。

   .. versionchanged:: 2.1
      この変数の値は :func:`parse` を呼び出す前に変更することができ、その場合新たな値が効果を持つようになります.


.. _domeventstream-objects:

DOMEventStream オブジェクト
---------------------------


.. method:: DOMEventStream.getEvent()

   ...


.. method:: DOMEventStream.expandNode(node)

   ...


.. method:: DOMEventStream.reset()

   ...

