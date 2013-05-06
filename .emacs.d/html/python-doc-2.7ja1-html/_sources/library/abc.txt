:mod:`abc` --- 抽象基底クラス
====================================

.. module:: abc
   :synopsis: PEP 3119 に基づいた抽象基底クラス
.. moduleauthor:: Guido van Rossum
.. sectionauthor:: Georg Brandl
.. much of the content adapted from docstrings

.. versionadded:: 2.6

このモジュールは Python に :pep:`3119` で概要が示された抽象基底クラス
(:term:`abstract base class`, ABC) を定義する基盤を提供します。
なぜこれが Python に付け加えられたかについてはその PEP を参照してください。
(ABC に基づいた数の型階層を扱った :pep:`3141` と :mod:`numbers` モジュールも\
参照してください。)

:mod:`collections` モジュールには ABC から派生した具象クラスがいくつかあります。
これらから、もちろんですが、さらに派生させることもできます。また :mod:`collections`
モジュールにはいくつかの ABC もあって、あるクラスやインスタンスが特定のインタフェース\
を提供しているかどうか、たとえばハッシュ可能かあるいはマッピングか、をテストできます。


このモジュールは以下のクラスを提供します:

.. class:: ABCMeta

   抽象基底クラス(ABC)を定義するためのメタクラス。

   ABC を作るときにこのメタクラスを使います。ABC は直接的にサブクラス化することができ、\
   ミックスイン(mix-in)クラスのように振る舞います。また、無関係な具象クラス(組み込み型\
   でも構いません)と無関係な ABC を "仮想的サブクラス" として登録できます --
   これらとその子孫は組み込み関数 :func:`issubclass` によって登録した ABC の\
   サブクラスと判定されますが、登録した ABC は MRO (Method Resolution Order,
   メソッド解決順)には現れませんし、この ABC のメソッド実装が(:func:`super`
   を通してだけでなく)呼び出し可能になるわけでもありません。 [#]_

   メタクラス :class:`ABCMeta` を使って作られたクラスには以下のメソッドがあります:

   .. method:: register(subclass)

      *subclass* を "仮想的サブクラス" としてこの ABC に登録します。たとえば::

        from abc import ABCMeta

        class MyABC:
            __metaclass__ = ABCMeta

        MyABC.register(tuple)

        assert issubclass(tuple, MyABC)
        assert isinstance((), MyABC)

   また、次のメソッドを抽象基底クラスの中でオーバーライドできます:

   .. method:: __subclasshook__(subclass)

      (クラスメソッドとして定義しなければなりません。)

      *subclass* がこの ABC のサブクラスと見なせるかどうかチェックします。
      これによって ABC のサブクラスと見なしたい全てのクラスについて :meth:`register`
      を呼び出すことなく ``issubclass`` の振る舞いをさらにカスタマイズできます。
      (このクラスメソッドは ABC の :meth:`__subclasscheck__` メソッドから\
      呼び出されます。)

      このメソッドは ``True``, ``False`` または ``NotImplemented``
      を返さなければなりません。 ``True`` を返す場合は、 *subclass* はこの ABC
      のサブクラスと見なされます。 ``False`` を返す場合は、たとえ通常の意味で\
      サブクラスであっても ABC のサブクラスではないと見なされます。
      ``NotImplemented`` の場合、サブクラスチェックは通常のメカニズムに\
      戻ります。

      .. XXX explain the "usual mechanism"


   この概念のデモとして、次の ABC 定義の例を見てください::

      class Foo(object):
          def __getitem__(self, index):
              ...
          def __len__(self):
              ...
          def get_iterator(self):
              return iter(self)

      class MyIterable:
          __metaclass__ = ABCMeta

          @abstractmethod
          def __iter__(self):
              while False:
                  yield None

          def get_iterator(self):
              return self.__iter__()

          @classmethod
          def __subclasshook__(cls, C):
              if cls is MyIterable:
                  if any("__iter__" in B.__dict__ for B in C.__mro__):
                      return True
              return NotImplemented

      MyIterable.register(Foo)

   ABC ``MyIterable`` は標準的なイテラブルのメソッド :meth:`__iter__` を\
   抽象メソッドとして定義します。ここで与えられている実装はサブクラスから呼び\
   出されることがそれでもあり得ます。 :meth:`get_iterator` メソッドも
   ``MyIterable`` 抽象基底クラスの一部ですが、抽象的でない派生クラスでオーバ\
   ーライドされなくても構いません。

   ここで定義されるクラスメソッド :meth:`__subclasshook__` の意味は、\
   :meth:`__iter__` メソッドがクラスの(または :attr:`__mro__`
   でアクセスされる基底クラスの一つの) :attr:`__dict__` にある場合にも\
   そのクラスが ``MyIterable`` だと見なされるということです。

   最後に、一番下の行は ``Foo`` を :meth:`__iter__` メソッドを定義しないにも\
   かかわらず ``MyIterable`` の仮想的サブクラスにします (``Foo`` は古い様式の
   :meth:`__len__` と :meth:`__getitem__` を用いた繰り返しのプロトコルを使っ\
   ています)。これによって ``Foo`` のメソッドとして ``get_iterator`` が手に入\
   るわけではないことに注意してください。それは別に提供されています。


以下のデコレータも提供しています:

.. function:: abstractmethod(function)

   抽象メソッドを示すデコレータです。

   このデコレータを使うにはクラスのメタクラスが :class:`ABCMeta` であるかまたは\
   派生したものであることが求められます。
   :class:`ABCMeta` から派生したメタクラスを持つクラスは全ての抽象メソッドお\
   よびプロパティをオーバーライドしない限りインスタンス化できません。
   抽象メソッドは普通の 'super' 呼び出し機構を使って呼び出すことができます。

   クラスに動的に抽象メソッドを追加する、あるいはメソッドやクラスが作られた後\
   から抽象的かどうかの状態を変更しようと試みることは、サポートされません。
   :func:`abstractmethod` が影響を与えるのは正規の継承により派生したサブクラ\
   スのみで、ABC の :meth:`register` メソッドで登録された "仮想的サブクラス"
   は影響されません。

   使用例::

      class C:
          __metaclass__ = ABCMeta
          @abstractmethod
          def my_abstract_method(self, ...):
              ...

   .. note::

      Java の抽象メソッドと違い、これらの抽象メソッドは実装を持ち得ます。
      この実装は :func:`super` メカニズムを通してそれ\
      をオーバーライドしたクラスから呼び出すことができます。これは協調的多重\
      継承を使ったフレームワークにおいて super 呼び出しの終点として有効です。


.. function:: abstractproperty([fget[, fset[, fdel[, doc]]]])

   組み込みの :func:`property` のサブクラスで、抽象プロパティであることを示します。

   この関数を使うにはクラスのメタクラスが :class:`ABCMeta` であるかまたは\
   派生したものであることが求められます。
   :class:`ABCMeta` から派生したメタクラスを持つクラスは全ての抽象メソッドお\
   よびプロパティをオーバーライドしない限りインスタンス化できません。
   抽象プロパティは普通の 'super' 呼び出し機構を使って呼び出すことができます。

   使用例::

      class C:
          __metaclass__ = ABCMeta
          @abstractproperty
          def my_abstract_property(self):
              ...

   この例は読み取り専用のプロパティを定義しています。読み書きできる抽象\
   プロパティを「長い」形式のプロパティ宣言を使って定義することもできます::

      class C:
          __metaclass__ = ABCMeta
          def getx(self): ...
          def setx(self, value): ...
          x = abstractproperty(getx, setx)


.. rubric:: 注記

.. [#] C++ プログラマは Python の仮想的基底クラスの概念は C++ のものと同じで\
   はないということを銘記すべきです。
