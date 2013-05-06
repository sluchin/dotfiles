.. _email-examples:

:mod:`email`: 使用例
----------------------

ここでは :mod:`email` パッケージを使って電子メールメッセージを\
読む・書く・送信するいくつかの例を紹介します。より複雑な MIME
メッセージについても扱います。

最初に、テキスト形式の単純なメッセージを作成・送信する方法です:

.. literalinclude:: ../includes/email-simple.py


Parser() クラスの parse(filename) か parsestr(message_as_string) メソッドを
使って簡単に RFC822 ヘッダの解析ができます。

.. literalinclude:: ../includes/email-headers.py


つぎに、あるディレクトリ内にある何枚かの家族写真をひとつの MIME
メッセージに収めて送信する例です:

.. literalinclude:: ../includes/email-mime.py


つぎはあるディレクトリに含まれている内容全体をひとつの電子メールメッセージとして\
送信するやり方です:  [1]_

.. literalinclude:: ../includes/email-dir.py


つぎに、上のような MIME メッセージをどうやって展開してひとつのディレクトリ上の\
複数ファイルにするかを示します:

.. literalinclude:: ../includes/email-unpack.py

つぎの例は、HTML メッセージを代替プレーンテキスト版付きで作るやりかたです: [2]_

.. literalinclude:: ../includes/email-alternative.py


.. rubric:: 注記

.. [1] 最初の思いつきと用例は Matthew Dixon Cowles のおかげです。
.. [2] Martin Matejek が教えてくれました。
