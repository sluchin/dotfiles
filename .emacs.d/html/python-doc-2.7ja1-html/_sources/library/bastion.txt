
:mod:`Bastion` --- オブジェクトに対するアクセスの制限
=====================================================

.. module:: Bastion
   :synopsis: オブジェクトに対するアクセスの制限を提供する。
   :deprecated:

.. deprecated:: 2.6
   :mod:`Bastion` モジュールは Python 3.0 で削除されました。

.. moduleauthor:: Barry Warsaw <bwarsaw@python.org>


.. versionchanged:: 2.3
   Disabled module.

.. note::

   このドキュメントは、Bastion モジュールを使用している古いコードを読む際の
   参照用として残されています。

辞書によると、バスチョン (bastion、要塞) とは、"防衛された領域や地点"、
または "最後の砦と考えられているもの" であり、オブジェクトの特定の属性への
アクセスを禁じる方法を提供するこのモジュールにふさわしい名前です。
制限モード下のプログラムに対して、あるオブジェクトにおける特定の安全な
属性へのアクセスを許可し、かつその他の安全でない属性へのアクセスを拒否するには、
要塞オブジェクトは常に :mod:`rexec` モジュールと共に使われなければなりません。

.. I'm concerned that the word 'bastion' won't be understood by people
.. for whom English is a second language, making the module name
.. somewhat mysterious.  Thus, the brief definition... --amk

.. I've punted on the issue of documenting keyword arguments for now.


.. function:: Bastion(object[, filter[, name[, class]]])

   オブジェクト *object* を保護し、オブジェクトに対する要塞オブジェクトを返します。
   オブジェクトの属性に対するアクセスの試みは全て、 *filter*
   関数によって認可されなければなりません;
   アクセスが拒否された場合 :exc:`AttributeError` 例外が送出されます。

   *filter* が存在する場合、この関数は属性名を含む文字列を受理し、
   その属性に対するアクセスが許可される場合には真を返さなければなりません;
   *filter* が偽を返す場合、アクセスは拒否されます。
   標準のフィルタは、アンダースコア (``'_'``) で始まる全ての
   関数に対するアクセスを拒否します。
   *name* の値が与えられた場合、要塞オブジェクトの文字列表現は
   ``<Bastion for name>`` になります;
   そうでない場合、 ``repr(object)`` が使われます。

   *class* が存在する場合、 :class:`BastionClass` のサブクラスでなくてはなりません;
   詳細は :file:`bastion.py` のコードを参照してください。
   :class:`BastionClass` の標準設定を上書きする必要はほとんどないはずです。


.. class:: BastionClass(getfunc, name)

   実際に要塞オブジェクトを実装しているクラスです。
   このクラスは :func:`Bastion` によって使われる標準のクラスです。
   *getfunc* 引数は関数で、唯一の引数である属性の名前を与えて呼び出した際、
   制限された実行環境に対して、開示すべき属性の値を返します。 *name* は
   :class:`BastionClass` インスタンスの :func:`repr` を構築するために使われます。
