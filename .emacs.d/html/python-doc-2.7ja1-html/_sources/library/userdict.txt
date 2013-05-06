
:mod:`UserDict` --- 辞書オブジェクトのためのクラスラッパー
==========================================================

.. module:: UserDict
   :synopsis: 辞書オブジェクトのためのクラスラッパー。


このモジュールは最小限のマッピングインターフェイスをすでに持っているクラスのために、
すべての辞書メソッドを定義しているmixin、 :class:`DictMixin` を定義しています。これによって、shelveモジュールのような辞書の代わりをする必要があるクラスを書くことが非常に簡単になります。

このモジュールでは :class:`UserDict` クラスを定義しています。これは辞書オブジェクトのラッパーとして
動作します。これは :class:`dict` \ (Python 2.2から利用可能な機能です)によって置き換えられています。
:class:`dict` の導入以前に、 :class:`UserDict` クラスは辞書風のサブクラスをオーバライドや新メソッドの
定義によって作成するために使われていました。

.. seealso::

   最新バージョンの `UserDict Python ソースコード
   <http://svn.python.org/view/python/branches/release27-maint/Lib/UserDict.py?view=markup>`_

:mod:`UserDict` モジュールは :class:`UserDict` クラスと :class:`DictMixin` を定義しています:


.. class:: UserDict([initialdata])

   辞書をシミュレートするクラス。インスタンスの内容は通常の辞書に保存され、 :class:`UserDict` インスタンスの :attr:`data` 属性を通してアクセスできます。 *initialdata* が与えられれば、 :attr:`data` はその内容で初期化されます。他の目的のために使えるように、 *initialdata* への参照が保存されないことがあるということに注意してください。

   .. note::

      後方互換性のために、 :class:`UserDict` のインスタンスはイテレート可能ではありません。


.. class:: IterableUserDict([initialdata])

   :class:`UserDict` のイテレーションをサポートするサブクラス (使用例: ``for key in myDict``).

マッピングのメソッドと演算(節 :ref:`typesmapping` を参照)に加えて、 :class:`UserDict` 、 :class:`IterableUserDict` インスタンスは次の属性を提供します:


.. attribute:: IterableUserDict.data

   :class:`UserDict` クラスの内容を保存するために使われる実際の辞書。


.. class:: DictMixin()

   :meth:`__getitem__` 、 :meth:`__setitem__` 、 :meth:`__delitem__` および :meth:`keys`
   といった最小の辞書インタフェースを既に持っているクラスのために、全ての辞書メソッドを定義する mixin です。

   このmixinはスーパークラスとして使われるべきです。上のそれぞれのメソッドを追加することで、より多くの機能がだんだん追加されます。
   例えば、 :meth:`__delitem__` 以外の全てのメソッドを定義すると、使えないのは :meth:`pop` と :meth:`popitem`
   だけになります。

   4 つの基底メソッドに加えて、 :meth:`__contains__` 、 :meth:`__iter__`
   および :meth:`iteritems` を定義すれば、順次効率化を果たすことができます。

   mixin はサブクラスのコンストラクタについて何も知らないので、 :meth:`__init__` や :meth:`copy` は定義していません。

   ..
      Starting with Python version 2.6, it is recommended to use
      :class:`collections.MutableMapping` instead of :class:`DictMixin`.

   Python 2.6 からは、 :class:`DictMixin` の代わりに、 :class:`collections.MutableMapping`
   を利用することが推奨されています。


:mod:`UserList` --- リストオブジェクトのためのクラスラッパー
============================================================

.. module:: UserList
   :synopsis: リストオブジェクトのためのクラスラッパー。


.. note::

   このモジュールは後方互換性のためだけに残されています。Python
   2.2より前のバージョンのPythonで動作する必要のないコードを書いているのならば、組み込み :class:`list` 型から直接サブクラス化することを検討してください。

このモジュールはリストオブジェクトのラッパーとして働くクラスを定義します。独自のリストに似たクラスのために役に立つ基底クラスで、これを継承し既存のメソッドをオーバーライドしたり、あるいは、新しいものを追加したりすることができます。このような方法で、リストに新しい振る舞いを追加できます。

:mod:`UserList` モジュールは :class:`UserList` クラスを定義しています:


.. class:: UserList([list])

   リストをシミュレートするクラス。インスタンスの内容は通常のリストに保存され、
   :class:`UserList` インスタンスの :attr:`data` 属性を通してアクセスできます。
   インスタンスの内容は最初に *list* のコピーに設定されますが、デフォルトでは空リスト ``[]`` です。
   *list* は何かイテレートできるオブジェクトで、例えば、通常のPythonリストや、
   :class:`UserList` (またはサブクラス)のインスタンスなどを利用できます。

   .. note::
      .. The :class:`UserList` class has been moved to the :mod:`collections`
         module in Python 3.0. The :term:`2to3` tool will automatically adapt
         imports when converting your sources to 3.0.

      :class:`UserList` クラスは Python 3.0 では :mod:`collections` モジュールに移動されました。
      :term:`2to3` ツールが自動的にソースコードの import 文を修正します。

変更可能シーケンスのメソッドと演算(節 :ref:`typesseq` を参照)に加えて、 :class:`UserList` インスタンスは次の属性を提供します:


.. attribute:: UserList.data

   :class:`UserList` クラスの内容を保存するために使われる実際のPythonリストオブジェクト。

**サブクラス化の要件:**
:class:`UserList` のサブクラスは引数なしか、あるいは一つの引数のどちらかとともに呼び出せるコンストラクタを提供することが期待されます。新しいシーケンスを返すリスト演算は現在の実装クラスのインスタンスを作成しようとします。そのために、データ元として使われるシーケンスオブジェクトである一つのパラメータとともにコンストラクタを呼び出せると想定しています。

派生クラスがこの要求に従いたくないならば、このクラスがサポートしているすべての特殊メソッドはオーバーライドされる必要があります。その場合に提供される必要のあるメソッドについての情報は、ソースを参考にしてください。

.. versionchanged:: 2.0
   Pythonバージョン1.5.2と1.6では、コンストラクタが引数なしで呼び出し可能であることと変更可能な :attr:`data` 属性を提供するということも要求されます。Pythonの初期のバージョンでは、派生クラスのインスタンスを作成しようとはしません。


:mod:`UserString` --- 文字列オブジェクトのためのクラスラッパー
==============================================================

.. module:: UserString
   :synopsis: 文字列オブジェクトのためのクラスラッパー。
.. moduleauthor:: Peter Funk <pf@artcom-gmbh.de>
.. sectionauthor:: Peter Funk <pf@artcom-gmbh.de>


.. note::

   このモジュールの :class:`UserString` クラスは後方互換性のためだけに残されています。Python
   2.2より前のバージョンのPythonで動作する必要のないコードを書いているのならば、 :class:`UserString` を使う代わりに組み込み :class:`str` 型から直接サブクラス化することを検討してください(組み込みの :class:`MutableString` と等価なものはありません)。

このモジュールは文字列オブジェクトのラッパーとして働くクラスを定義します。独自の文字列に似たクラスのために役に立つ基底クラスで、これを継承し既存のメソッドをオーバーライドしたり、あるいは、新しいものを追加したりすることができます。このような方法で、文字列に新しい振る舞いを追加できます。

これらのクラスは実際のクラスやユニコードオブジェクトに比べてとても効率が悪いということに注意した方がよいでしょう。これは特に :class:`MutableString` に対して当てはまります。

:mod:`UserString` モジュールは次のクラスを定義しています:


.. class:: UserString([sequence])

   文字列またはユニコード文字列オブジェクトをシミュレートするクラス。インスタンスの内容は通常の文字列またはユニコード文字列オブジェクトに保存され、 :class:`UserString` インスタンスの :attr:`data` 属性を通してアクセスできます。インスタンスの内容は最初に *sequence* のコピーに設定されます。 *sequence* は通常のPython文字列またはユニコード文字列、 :class:`UserString` \
   (またはサブクラス)のインスタンス、あるいは組み込み :func:`str` 関数を使って文字列に変換できる任意のシーケンスのいずれかです。


.. class:: MutableString([sequence])

   このクラスは上の :class:`UserString` から派生し、 *変更可能に* なるように文字列を再定義します。変更可能な文字列は辞書のキーとして使うことができません。なぜなら、辞書はキーとして *変更不能な* オブジェクトを要求するからです。このクラスの主な目的は、辞書のキーとして変更可能なオブジェクトを使うという試みを捕捉するために、継承と :meth:`__hash__` メソッドを取り除く(オーバーライドする)必要があることを示す教育的な例を提供することです。そうしなければ、非常にエラーになりやすく、突き止めることが困難でしょう。

   .. deprecated:: 2.6
      :class:`MutableString` クラスは Python 3.0 では削除されます。

文字列とユニコードオブジェクトのメソッドと演算(節 :ref:`string-methods` を参照)に加えて、 :class:`UserString` インスタンスは次の属性を提供します:


.. attribute:: MutableString.data

   :class:`UserString` クラスの内容を保存するために使われる実際のPython文字列またはユニコードオブジェクト。

