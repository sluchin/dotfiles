:mod:`email`: エンコーダ
------------------------

.. module:: email.encoders
   :synopsis: 電子メールメッセージのペイロードのためのエンコーダ。


何もないところから :class:`~email.message.Message` を作成するときしばしば必要になるのが、
ペイロードをメールサーバに通すためにエンコードすることです。
これはとくにバイナリデータを含んだ :mimetype:`image/\*` や
:mimetype:`text/\*` タイプのメッセージで必要です。

:mod:`email` パッケージでは、 :mod:`encoders` モジュールにおいて
いくかの便宜的なエンコーディングをサポートしています。
実際にはこれらのエンコーダは :class:`~email.mime.audio.MIMEAudio` および
:class:`~email.mime.audio.MIMEImage` クラスのコンストラクタでデフォルトエンコーダとして使われています。
すべてのエンコーディング関数は、エンコードするメッセージオブジェクト\
ひとつだけを引数にとります。これらはふつうペイロードを取りだし、
それをエンコードして、ペイロードをエンコードされたものにセットしなおします。
これらはまた :mailheader:`Content-Transfer-Encoding` ヘッダを適切な値に\
設定します。

提供されているエンコーディング関数は以下のとおりです:


.. function:: encode_quopri(msg)

   ペイロードを quoted-printable 形式にエンコードし、
   :mailheader:`Content-Transfer-Encoding` ヘッダを
   ``quoted-printable`` [#]_ に設定します。
   これはそのペイロードのほとんどが通常の印刷可能な文字からなっているが、
   印刷不可能な文字がすこしだけあるときのエンコード方法として適しています。


.. function:: encode_base64(msg)

   ペイロードを base64 形式でエンコードし、
   :mailheader:`Content-Transfer-Encoding` ヘッダを ``base64``
   に変更します。これはペイロード中の\
   データのほとんどが印刷不可能な文字である場合に適しています。 quoted-printable
   形式よりも結果としてはコンパクトなサイズになるからです。
   base64 形式の欠点は、これが人間にはまったく読めないテキストに\
   なってしまうことです。


.. function:: encode_7or8bit(msg)

   これは実際にはペイロードを変更はしませんが、ペイロードの形式に応じて
   :mailheader:`Content-Transfer-Encoding` ヘッダを
   ``7bit`` あるいは ``8bit`` に適した形に設定します。


.. function:: encode_noop(msg)

   これは何もしないエンコーダです。
   :mailheader:`Content-Transfer-Encoding` ヘッダを設定さえしません。

.. rubric:: 注記

.. [#] 注意: :meth:`encode_quopri` を\
       使ってエンコードすると、データ中のタブ文字や空白文字も\
       エンコードされます。

