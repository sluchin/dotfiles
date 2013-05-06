
:mod:`mailcap` --- mailcap ファイルの操作
=========================================

.. module:: mailcap
   :synopsis: mailcap ファイルの操作



mailcap ファイルは、メールリーダや Web ブラウザのような MIME 対応の\
アプリケーションが、異なる MIME タイプのファイルにどのように反応\
するかを設定するために使われます ("mailcap" の名前は
"mail capability" から取られました)。例えば、ある mailcap
ファイルに ``video/mpeg; xmpeg %s`` のような\
行が入っていたとします。ユーザが email メッセージや Web ドキュメント\
上でその MIME タイプ :mimetype:`video/mpeg` に遭遇すると、 ``%s`` はファイル名
(通常テンポラリファイルに属するものになります) に置き換えられ、ファイルを閲覧するために\
:program:`xmpeg` プログラムが自動的に起動されます。

mailcap の形式は :rfc:`1524`, "A User Agent Configuration Mechanism
For Multimedia Mail Format Information" で文書化されていますが、
この文書はインターネット標準ではありません。しかしながら、 mailcap
ファイルはほとんどの Unix システムでサポートされています。


.. function:: findmatch(caps, MIMEtype[, key[, filename[, plist]]])

   2 要素のタプルを返します; 最初の要素は文字列で、実行すべき\
   コマンド (:func:`os.system` に渡されます) が入っています。
   二つめの要素は与えられた MIME タイプに対する mailcap エントリです。
   一致する MIME タイプが見つからなかった場合、 ``(None, None)`` が\
   返されます。

   *key* は desired フィールドの値で、実行すべき動作のタイプを表現します;
   ほとんどの場合、単に MIME 形式のデータ本体を見たいと思うので、
   標準の値は 'view' になっています。
   与えられた MIME 型をもつ新たなデータ本体を作成した場合や、
   既存のデータ本体を置き換えたい場合には、'view' の他に 'compose'
   および 'edit' を取ることもできます。

   これらフィールドの完全なリストについては :rfc:`1524` を参照してください。

   *filename* はコマンドライン中で ``%s`` に代入されるファイル名\
   です; 標準の値は ``'/dev/null'`` で、たいていこの値を使いたい\
   わけではないはずです。従って、ファイル名を指定してこのフィールドを\
   上書きする必要があるでしょう。

   *plist* は名前付けされたパラメタのリストです; 標準の値は単なる\
   空のリストです。リスト中の各エントリはパラメタ名を含む文字列、等号
   (``'='``)、およびパラメタの値でなければなりません。
   mailcap エントリには ``%{foo}`` といったような名前つき\
   のパラメタを含めることができ、'foo' と名づけられたパラメタの値に\
   置き換えられます。例えば、コマンドライン ``showpartial %{id}
   %{number} %{total}`` が mailcap ファイルにあり、
   *plist* が ``['id=1', 'number=2', 'total=3']`` に設定されていれば、
   コマンドラインは ``'showpartial 1 2 3'`` になります。

   mailcap ファイル中では、オプションの "test" フィールドを\
   使って、(計算機アーキテクチャや、利用しているウィンドウシステムといった)
   何らかの外部条件をテストするよう指定することができます。
   :func:`findmatch` はこれらの条件を自動的にチェックし、
   チェックが失敗したエントリを読み飛ばします。


.. function:: getcaps()

   MIME タイプを mailcap ファイルのエントリに対応付ける辞書を返します。
   この辞書は :func:`findmatch` 関数に渡されるべきものです。
   エントリは辞書のリストとして記憶されますが、この表現形式の\
   詳細について知っておく必要はないでしょう。

   mailcap 情報はシステム上で見つかった全ての mailcap ファイルから\
   導出されます。ユーザ設定の mailcap ファイル
   :file:`$HOME/.mailcap` はシステムの mailcap ファイル :file:`/etc/mailcap` 、
   :file:`/usr/etc/mailcap` 、および :file:`/usr/local/etc/mailcap`
   の内容を上書きします。

以下に使用例を示します::

   >>> import mailcap
   >>> d=mailcap.getcaps()
   >>> mailcap.findmatch(d, 'video/mpeg', filename='/tmp/tmp1223')
   ('xmpeg /tmp/tmp1223', {'view': 'xmpeg %s'})

