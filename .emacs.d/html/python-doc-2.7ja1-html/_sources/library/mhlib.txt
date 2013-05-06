:mod:`mhlib` --- MH のメールボックスへのアクセス機構
====================================================

.. module:: mhlib
   :synopsis: Python から MH のメールボックスを操作します。
   :deprecated:

.. deprecated:: 2.6
    :mod:`mhlib` は Python 3.0 では削除されています。
    代わりに :mod:`mailbox` をお使い下さい。

.. sectionauthor:: Skip Montanaro <skip@pobox.com>

:mod:`mhlib` モジュールは MH フォルダおよびその内容に対する Python インタフェースを提供します。

このモジュールには、あるフォルダの集まりを表現する :class:`MH` 、
単一のフォルダを表現する :class:`Folder` 、
単一のメッセージを表現する :class:`Message` 、の 3 つのクラスが入っています。


.. class:: MH([path[, profile]])

   :class:`MH` は MH フォルダの集まりを表現します。


.. class:: Folder(mh, name)

   :class:`Folder` クラスは単一のフォルダとフォルダ内のメッセージ群を表現します。


.. class:: Message(folder, number[, name])

   :class:`Message` オブジェクトはフォルダ内の個々のメッセージを表現します。
   メッセージクラスは :class:`mimetools.Message` から派生しています。


.. _mh-objects:

MH オブジェクト
---------------

:class:`MH` インスタンスは以下のメソッドを持っています:


.. method:: MH.error(format[, ...])

   エラーメッセージを出力します -- 上書きすることができます。


.. method:: MH.getprofile(key)

   プロファイルエントリ (設定されていなければ ``None``) を返します。


.. method:: MH.getpath()

   メールボックスのパス名を返します。


.. method:: MH.getcontext()

   現在のフォルダ名を返します。


.. method:: MH.setcontext(name)

   現在のフォルダ名を設定します。


.. method:: MH.listfolders()

   トップレベルフォルダのリストを返します。


.. method:: MH.listallfolders()

   全てのフォルダを列挙します。


.. method:: MH.listsubfolders(name)

   指定したフォルダの直下にあるサブフォルダのリストを返します。


.. method:: MH.listallsubfolders(name)

   指定したフォルダの下にある全てのサブフォルダのリストを返します。


.. method:: MH.makefolder(name)

   新しいフォルダを生成します。


.. method:: MH.deletefolder(name)

   フォルダを削除します -- サブフォルダが入っていてはいけません。


.. method:: MH.openfolder(name)

   新たな開かれたフォルダオブジェクトを返します。


.. _mh-folder-objects:

Folder オブジェクト
-------------------

:class:`Folder` インスタンスは開かれたフォルダを表現し、以下のメソッドを持っています:


.. method:: Folder.error(format[, ...])

   エラーメッセージを出力します -- 上書きすることができます。


.. method:: Folder.getfullname()

   フォルダの完全なパス名を返します。


.. method:: Folder.getsequencesfilename()

   フォルダ内のシーケンスファイルの完全なパス名を返します。


.. method:: Folder.getmessagefilename(n)

   フォルダ内のメッセージ *n* の完全なパス名を返します。


.. method:: Folder.listmessages()

   フォルダ内のメッセージの (番号の) リストを返します。


.. method:: Folder.getcurrent()

   現在のメッセージ番号を返します。


.. method:: Folder.setcurrent(n)

   現在のメッセージ番号を *n* に設定します。


.. method:: Folder.parsesequence(seq)

   msgs 文を解釈して、メッセージのリストにします。


.. method:: Folder.getlast()

   最新のメッセージを取得します。メッセージがフォルダにない場合には ``0`` を返します。


.. method:: Folder.setlast(n)

   最新のメッセージを設定します (内部使用のみ)。


.. method:: Folder.getsequences()

   フォルダ内のシーケンスからなる辞書を返します。シーケンス名がキーとして使われ、値はシーケンスに含まれるメッセージ番号のリストになります。


.. method:: Folder.putsequences(dict)

   フォルダ内のシーケンスからなる辞書 name: list を返します。


.. method:: Folder.removemessages(list)

   リスト中のメッセージをフォルダから削除します。


.. method:: Folder.refilemessages(list, tofolder)

   リスト中のメッセージを他のフォルダに移動します。


.. method:: Folder.movemessage(n, tofolder, ton)

   一つのメッセージを他のフォルダの指定先に移動します。


.. method:: Folder.copymessage(n, tofolder, ton)

   一つのメッセージを他のフォルダの指定先にコピーします。


.. _mh-message-objects:

Message オブジェクト
--------------------

:class:`Message` クラスは :class:`mimetools.Message` のメソッドに加え、一つメソッドを持っています:


.. method:: Message.openmessage(n)

   新たな開かれたメッセージオブジェクトを返します (ファイル記述子を一つ消費します)。

