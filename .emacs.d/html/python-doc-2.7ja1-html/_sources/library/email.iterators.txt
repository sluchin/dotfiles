:mod:`email`: イテレータ
------------------------

.. module:: email.iterators
   :synopsis: メッセージオブジェクトツリーをたどる。


:meth:`Message.walk` メソッドを使うと、簡単にメッセージオブジェクトツリー内\
を次から次へとたどる (iteration) ことができます。
:mod:`email.iterators` モジュールはこのための高水準イテレータをいくつか提供\
します。


.. function:: body_line_iterator(msg[, decode])

   このイテレータは *msg* 中のすべてのサブパートに含まれるペイロードをすべて\
   順にたどっていき、ペイロード内の文字列を 1行ずつ返します。
   サブパートのヘッダはすべて無視され、Python 文字列でないペイロードからなる\
   サブパートも無視されます。これは :meth:`readline` を使って、
   ファイルからメッセージを (ヘッダだけとばして) フラットなテキストとして読む\
   のにいくぶん似ているかもしれません。

   オプション引数 *decode* は、 :meth:`Message.get_payload` にそのまま渡されます。


.. function:: typed_subpart_iterator(msg[, maintype[, subtype]])

   このイテレータは *msg* 中のすべてのサブパートをたどり、それらの中で指定された\
   MIME 形式 *maintype* と *subtype*
   をもつようなパートのみを返します。

   *subtype* は省略可能であることに注意してください。
   これが省略された場合、サブパートの MIME 形式は maintype のみが\
   チェックされます。じつは *maintype* も省略可能で、
   その場合にはデフォルトは :mimetype:`text` です。

   つまり、デフォルトでは :func:`typed_subpart_iterator` は MIME 形式
   :mimetype:`text/*` をもつサブパートを順に返していくというわけです。

以下の関数は役に立つデバッグ用ツールとして追加されたもので、
パッケージとして公式なサポートのあるインターフェイスでは *ありません* 。


.. function:: _structure(msg[, fp[, level]])

   そのメッセージオブジェクト構造の content-type をインデントつきで\
   表示します。たとえば::

      >>> msg = email.message_from_file(somefile)
      >>> _structure(msg)
      multipart/mixed
          text/plain
          text/plain
          multipart/digest
              message/rfc822
                  text/plain
              message/rfc822
                  text/plain
              message/rfc822
                  text/plain
              message/rfc822
                  text/plain
              message/rfc822
                  text/plain
          text/plain

   オプション引数 *fp* は出力を渡すためのストリーム [#]_ オブジェクトです。
   これは Python の拡張 print
   文が対応できるようになっている必要があります。 *level* は内部的に使用されます。

.. rubric:: 注記

.. [#] 訳注: 原文では file-like。

