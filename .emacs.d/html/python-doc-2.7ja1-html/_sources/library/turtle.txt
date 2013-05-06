==================================================
:mod:`turtle` --- Tkのためのタートルグラフィックス
==================================================

.. module:: turtle
   :synopsis: Tkのためのタートルグラフィックス
.. sectionauthor:: Gregor Lingl <gregor.lingl@aon.at>

.. testsetup:: default

   from turtle import *
   turtle = Turtle()

はじめに
========

タートルグラフィックスは子供にプログラミングを紹介するのによく使われます。
タートルグラフィックスは Wally Feurzig と Seymore Papert が 1966 年に開発した Logo プログラミング言語の一部でした。

x-y 平面の (0, 0) から動き出すロボット亀を想像してみて下さい。
``turtle.forward(15)`` という命令を出すと、その亀が (スクリーン上で!) 15
ピクセル分顔を向けている方向に動き、動きに沿って線を引きます。
``turtle.left(25)`` という命令を出すと、今度はその場で25度反時計回りに回ります。

  .. clockwise とあるが、左に回るので *反* 時計回り

これらの命令と他の同様な命令を組み合わせることで、複雑な形や絵が簡単に描けます。

:mod:`turtle` モジュールは同じ名前を持った Python 2.5 までのモジュールの拡張された再実装です。

再実装に際しては古い turtle モジュールのメリットをそのままに、
(ほぼ) 100% 互換性を保つようにしました。
すなわち、まず第一に、学習中のプログラマがモジュールを ``-n`` スイッチを付けて走らせている
IDLE の中から全てのコマンド、クラス、メソッドを対話的に使えるようにしました。

turtle モジュールはオブジェクト指向と手続き指向の両方の方法でタートルグラフィックス・プリミティブを提供します。グラフィックスの基礎として :mod:`Tkinter` を使っているために、Tk をサポートした Python のバージョンが必要です。

オブジェクト指向インターフェイスでは、本質的に 2+2 のクラスを使います:

1. :class:`TurtleScreen` クラスはタートルが絵を描きながら走り回る画面を定義します。
   そのコンストラクタには :class:`Tkinter.Canvas` または :class:`ScrolledCanvas`
   を渡す必要があります。
   :mod:`turtle` をアプリケーションの一部として用いたい場合にはこれを使うべきです。

   :func:`Screen` 関数は :class:`TurtleScreen` のサブクラスのシングルトンオブジェクトを返します。
   :mod:`turtle` をグラフィクスを使う一つの独立したツールとして使う場合には、
   この関数を呼び出すべきです。
   シングルトンなので、そのクラスからの継承はできません。

   TurtleScreen/Screen の全てのメソッドは関数としても、すなわち、
   手続き指向インターフェイスの一部としても存在しています。

2. :class:`RawTurtle` (別名: :class:`RawPen`) は :class:`TurtleScreen`
   上に絵を描く Turtle オブジェクトを定義します。
   コンストラクタには Canvas, ScrolledCanvas, TurtleScreen
   のいずれかを引数として渡して RawTurtle オブジェクトがどこに絵を描くかを教えます。

   RawTurtle の派生はサブクラス :class:`Turtle` (別名: :class:`Pen`) で、
   "唯一の" :class:`Screen` (既に与えられているのでなければ自動的に作られたインスタンス)
   に絵を描きます。

   RawTurtle/Turtle の全てのメソッドは関数としても、すなわち、
   手続き指向インターフェイスの一部としても存在しています。

手続き型インターフェイスでは :class:`Screen` および :class:`Turtle`
クラスのメソッドを元にした関数を提供しています。
その名前は対応するメソッドと一緒です。
Screen のメソッドを元にした関数が呼び出されるといつでも screen オブジェクトが自動的に作られます。
Turtle のメソッドを元にした関数が呼び出されるといつでも(名無しの) turtle オブジェクトが自動的に作られます。

複数のタートルを一つのスクリーン上で使いたい場合、オブジェクト指向インターフェイスを使わなければなりません。

   .. an a screen は on a screen だと考えた

.. note::
   以下の文書では関数に対する引数リストが与えられています。
   メソッドでは、勿論、ここでは省略されている *self* が第一引数になります。

Turtle および Screen のメソッド概観
===================================

Turtle のメソッド
-----------------

Turtle の動き
   移動および描画
      | :func:`forward` | :func:`fd`
      | :func:`backward` | :func:`bk` | :func:`back`
      | :func:`right` | :func:`rt`
      | :func:`left` | :func:`lt`
      | :func:`goto` | :func:`setpos` | :func:`setposition`
      | :func:`setx`
      | :func:`sety`
      | :func:`setheading` | :func:`seth`
      | :func:`home`
      | :func:`circle`
      | :func:`dot`
      | :func:`stamp`
      | :func:`clearstamp`
      | :func:`clearstamps`
      | :func:`undo`
      | :func:`speed`

   Turtle の状態を知る
      | :func:`position` | :func:`pos`
      | :func:`towards`
      | :func:`xcor`
      | :func:`ycor`
      | :func:`heading`
      | :func:`distance`

   設定と計測
      | :func:`degrees`
      | :func:`radians`

Pen の制御
   描画状態
      | :func:`pendown` | :func:`pd` | :func:`down`
      | :func:`penup` | :func:`pu` | :func:`up`
      | :func:`pensize` | :func:`width`
      | :func:`pen`
      | :func:`isdown`

   色の制御
      | :func:`color`
      | :func:`pencolor`
      | :func:`fillcolor`

   塗りつぶし
      | :func:`fill`
      | :func:`begin_fill`
      | :func:`end_fill`

   さらなる描画の制御
      | :func:`reset`
      | :func:`clear`
      | :func:`write`

タートルの状態
   可視性
      | :func:`showturtle` | :func:`st`
      | :func:`hideturtle` | :func:`ht`
      | :func:`isvisible`

   見た目
      | :func:`shape`
      | :func:`resizemode`
      | :func:`shapesize` | :func:`turtlesize`
      | :func:`settiltangle`
      | :func:`tiltangle`
      | :func:`tilt`

イベントを利用する
   | :func:`onclick`
   | :func:`onrelease`
   | :func:`ondrag`

特別な Turtle のメソッド
   | :func:`begin_poly`
   | :func:`end_poly`
   | :func:`get_poly`
   | :func:`clone`
   | :func:`getturtle` | :func:`getpen`
   | :func:`getscreen`
   | :func:`setundobuffer`
   | :func:`undobufferentries`
   | :func:`tracer`
   | :func:`window_width`
   | :func:`window_height`

TurtleScreen/Screen のメソッド
------------------------------

ウィンドウの制御
   | :func:`bgcolor`
   | :func:`bgpic`
   | :func:`clear` | :func:`clearscreen`
   | :func:`reset` | :func:`resetscreen`
   | :func:`screensize`
   | :func:`setworldcoordinates`

アニメーションの制御
   | :func:`delay`
   | :func:`tracer`
   | :func:`update`

スクリーンイベントを利用する
   | :func:`listen`
   | :func:`onkey`
   | :func:`onclick` | :func:`onscreenclick`
   | :func:`ontimer`

設定と特殊なメソッド
   | :func:`mode`
   | :func:`colormode`
   | :func:`getcanvas`
   | :func:`getshapes`
   | :func:`register_shape` | :func:`addshape`
   | :func:`turtles`
   | :func:`window_height`
   | :func:`window_width`

Screen 独自のメソッド
   | :func:`bye`
   | :func:`exitonclick`
   | :func:`setup`
   | :func:`title`

RawTurtle/Turtle のメソッドと対応する関数
=========================================

この節のほとんどの例では ``turtle`` という名前の Turtle インスタンスを使います。

Turtle の動き
-------------

.. function:: forward(distance)
              fd(distance)

   :param distance: 数 (整数または浮動小数点数)

   タートルが頭を向けている方へ、タートルを距離 *distance* だけ前進させます。

   .. doctest::

      >>> turtle.position()
      (0.00, 0.00)
      >>> turtle.forward(25)
      >>> turtle.position()
      (25.00,0.00)
      >>> turtle.forward(-75)
      >>> turtle.position()
      (-50.00,0.00)

.. function:: back(distance)
              bk(distance)
              backward(distance)

   :param distance: 数

   タートルが頭を向けている方と反対方向へ、タートルを距離 *distance* だけ後退させます。
   タートルの向きは変えません。

   .. doctest::
      :hide:

      >>> turtle.goto(0, 0)

   .. doctest::

      >>> turtle.position()
      (0.00,0.00)
      >>> turtle.backward(30)
      >>> turtle.position()
      (-30.00,0.00)


.. function:: right(angle)
              rt(angle)

   :param angle: 数 (整数または浮動小数点数)

   タートルを *angle* 単位だけ右に回します。
   (単位のデフォルトは度ですが、 :func:`degrees` と :func:`radians` 関数を使って設定できます。)
   角度の向きはタートルのモードによって意味が変わります。
   :func:`mode` を参照してください。

   .. doctest::
      :hide:

      >>> turtle.setheading(22)

   .. doctest::

      >>> turtle.heading()
      22.0
      >>> turtle.right(45)
      >>> turtle.heading()
      337.0


.. function:: left(angle)
              lt(angle)

   :param angle: 数 (整数または浮動小数点数)

   タートルを *angle* 単位だけ左に回します。
   (単位のデフォルトは度ですが、 :func:`degrees` と :func:`radians` 関数を使って設定できます。)
   角度の向きはタートルのモードによって意味が変わります。
   :func:`mode` を参照してください。

   .. doctest::
      :hide:

      >>> turtle.setheading(22)

   .. doctest::

      >>> turtle.heading()
      22.0
      >>> turtle.left(45)
      >>> turtle.heading()
      67.0


.. function:: goto(x, y=None)
              setpos(x, y=None)
              setposition(x, y=None)

   :param x: 数または数のペア/ベクトル
   :param y: 数または ``None``

   *y* が ``None`` の場合、
   *x* は座標のペアかまたは :class:`Vec2D` (たとえば :func:`pos` で返されます)
   でなければなりません。

   タートルを指定された絶対位置に移動します。
   ペンが下りていれば線を引きます。
   タートルの向きは変わりません。

   .. doctest::
      :hide:

      >>> turtle.goto(0, 0)

   .. doctest::

       >>> tp = turtle.pos()
       >>> tp
       (0.00,0.00)
       >>> turtle.setpos(60,30)
       >>> turtle.pos()
       (60.00,30.00)
       >>> turtle.setpos((20,80))
       >>> turtle.pos()
       (20.00,80.00)
       >>> turtle.setpos(tp)
       >>> turtle.pos()
       (0.00,0.00)


.. function:: setx(x)

   :param x: 数 (整数または浮動小数点数)

   タートルの第一座標を *x* にします。
   第二座標は変わりません。

   .. doctest::
      :hide:

      >>> turtle.goto(0, 240)

   .. doctest::

      >>> turtle.position()
      (0.00,240.00)
      >>> turtle.setx(10)
      >>> turtle.position()
      (10.00,240.00)


.. function:: sety(y)

   :param y: 数 (整数または浮動小数点数)

   タートルの第二座標を *y* にします。
   第一座標は変わりません。

   .. doctest::
      :hide:

      >>> turtle.goto(0, 40)

   .. doctest::

      >>> turtle.position()
      (0.00,40.00)
      >>> turtle.sety(-10)
      >>> turtle.position()
      (0.00,-10.00)


.. function:: setheading(to_angle)
              seth(to_angle)

   :param to_angle: 数 (整数または浮動小数点数)

   タートルの向きを *to_angle* に設定します。
   以下はよく使われる方向を度で表わしたものです:

   =================== ====================
    標準モード           logo モード
   =================== ====================
      0 - 東                  0 - 北
     90 - 北                 90 - 東
    180 - 西                180 - 南
    270 - 南                270 - 西
   =================== ====================

   .. doctest::

      >>> turtle.setheading(90)
      >>> turtle.heading()
      90.0

.. function:: home()

   タートルを原点 -- 座標 (0, 0) -- に移動し、向きを開始方向に設定します
   (開始方向はモードに依って違います。 :func:`mode` を参照してください)。

   .. doctest::
      :hide:

      >>> turtle.setheading(90)
      >>> turtle.goto(0, -10)

   .. doctest::

      >>> turtle.heading()
      90.0
      >>> turtle.position()
      (0.00,-10.00)
      >>> turtle.home()
      >>> turtle.position()
      (0.00,0.00)
      >>> turtle.heading()
      0.0


.. function:: circle(radius, extent=None, steps=None)

   :param radius: 数
   :param extent: 数 (または ``None``)
   :param steps: 整数 (または ``None``)

   半径 *radius* の円を描きます。
   中心はタートルの左 *radius* ユニットの点です。
   *extent* -- 角度です -- は円のどの部分を描くかを決定します。
   *extent* が与えられなければ、デフォルトで完全な円になります。
   *extent* が完全な円でない場合は、弧の一つの端点は、現在のペンの位置です。
   *radius* が正の場合、弧は反時計回りに描かれます。
   そうでなければ、時計回りです。
   最後にタートルの向きが *extent* 分だけ変わります。

   円は内接する正多角形で近似されます。
   *steps* でそのために使うステップ数を決定します。
   この値は与えられなければ自動的に計算されます。
   また、これを正多角形の描画に利用することもできます。

   .. doctest::

      >>> turtle.home()
      >>> turtle.position()
      (0.00,0.00)
      >>> turtle.heading()
      0.0
      >>> turtle.circle(50)
      >>> turtle.position()
      (-0.00,0.00)
      >>> turtle.heading()
      0.0
      >>> turtle.circle(120, 180)  # 半円を描きます
      >>> turtle.position()
      (0.00,240.00)
      >>> turtle.heading()
      180.0


.. function:: dot(size=None, *color)

   :param size: 1 以上の整数 (与えられる場合には)
   :param color: 色を表わす文字列またはタプル

   直径 *size* の丸い点を *color* で指定された色で描きます。
   *size* が与えられなかった場合、pensize+4 と 2*pensize
   の大きい方が使われます。

   .. doctest::

      >>> turtle.home()
      >>> turtle.dot()
      >>> turtle.fd(50); turtle.dot(20, "blue"); turtle.fd(50)
      >>> turtle.position()
      (100.00,-0.00)
      >>> turtle.heading()
      0.0


.. function:: stamp()

   キャンバス上の現在タートルがいる位置にタートルの姿のハンコを押します。
   そのハンコに対して stamp_id が返されますが、
   これを使うと後で ``clearstamp(stamp_id)`` のように呼び出して消すことができます。

   .. doctest::

      >>> turtle.color("blue")
      >>> turtle.stamp()
      11
      >>> turtle.fd(50)


.. function:: clearstamp(stampid)

   :param stampid: 整数で、先立つ :func:`stamp` 呼出しで返された値でなければなりません

   *stampid* に対応するハンコを消します。

   .. doctest::

      >>> turtle.position()
      (150.00,-0.00)
      >>> turtle.color("blue")
      >>> astamp = turtle.stamp()
      >>> turtle.fd(50)
      >>> turtle.position()
      (200.00,-0.00)
      >>> turtle.clearstamp(astamp)
      >>> turtle.position()
      (200.00,-0.00)


.. function:: clearstamps(n=None)

   :param n: 整数 (または ``None``)

   全ての、または最初の/最後の *n* 個のハンコを消します。
   *n* が None の場合、全てのハンコを消します。
   *n* が正の場合には最初の *n* 個、
   *n* が負の場合には最後の *n* 個を消します。

   .. doctest::

      >>> for i in range(8):
      ...     turtle.stamp(); turtle.fd(30)
      13
      14
      15
      16
      17
      18
      19
      20
      >>> turtle.clearstamps(2)
      >>> turtle.clearstamps(-2)
      >>> turtle.clearstamps()


.. function:: undo()

   最後の(繰り返すことにより複数の)タートルの動きを取り消します。
   取り消しできる動きの最大数は undobuffer のサイズによって決まります。

   .. doctest::

      >>> for i in range(4):
      ...     turtle.fd(50); turtle.lt(80)
      ...
      >>> for i in range(8):
      ...     turtle.undo()


.. function:: speed(speed=None)

   :param speed: 0 から 10 までの整数またはスピードを表わす文字列(以下の説明を参照)

   タートルのスピードを 0 から 10 までの範囲の整数に設定します。
   引数が与えられない場合は現在のスピードを返します。

   与えられた数字が 10 より大きかったり 0.5 より小さかったりした場合は、
   スピードは 0 になります。
   スピードを表わす文字列は次のように数字に変換されます:

   * "fastest":  0
   * "fast":  10
   * "normal":  6
   * "slow":  3
   * "slowest":  1

   1 から 10 までのスピードを上げていくにつれて線を描いたりタートルが回ったりするアニメーションがだんだん速くなります。

   注意: *speed* = 0 はアニメーションを無くします。
   forward/backward ではタートルがジャンプし、left/right では瞬時に方向を変えます。

   .. doctest::

      >>> turtle.speed()
      3
      >>> turtle.speed('normal')
      >>> turtle.speed()
      6
      >>> turtle.speed(9)
      >>> turtle.speed()
      9


Turtle の状態を知る
-------------------

.. function:: position()
              pos()

   タートルの現在位置を (:class:`Vec2D` のベクトルとして) 返します。

   .. doctest::

      >>> turtle.pos()
      (440.00,-0.00)


.. function:: towards(x, y=None)

   :param x: 数または数のペア/ベクトルまたはタートルのインスタンス
   :param y: *x* が数ならば数、そうでなければ ``None``

   タートルの位置から指定された (x,y) への直線の角度を返します。
   この値はタートルの開始方向にそして開始方向はモード
   ("standard"/"world" または "logo")
   に依存します。

   .. doctest::

      >>> turtle.goto(10, 10)
      >>> turtle.towards(0,0)
      225.0


.. function:: xcor()

   タートルの x 座標を返します。

   .. doctest::

      >>> turtle.home()
      >>> turtle.left(50)
      >>> turtle.forward(100)
      >>> turtle.pos()
      (64.28,76.60)
      >>> print turtle.xcor()
      64.2787609687


.. function:: ycor()

   タートルの y 座標を返します。

   .. doctest::

      >>> turtle.home()
      >>> turtle.left(60)
      >>> turtle.forward(100)
      >>> print turtle.pos()
      (50.00,86.60)
      >>> print turtle.ycor()
      86.6025403784


.. function:: heading()

   タートルの現在の向きを返します (返される値はタートルのモードに依存します。
   :func:`mode` を参照してください)。

   .. doctest::

      >>> turtle.home()
      >>> turtle.left(67)
      >>> turtle.heading()
      67.0


.. function:: distance(x, y=None)

   :param x: 数または数のペア/ベクトルまたはタートルのインスタンス
   :param y: *x* が数ならば数、そうでなければ ``None``

   タートルから与えられた (x,y) あるいはベクトルあるいは渡されたタートルへの距離を、
   タートルのステップを単位として測った値を返します。

   .. doctest::

      >>> turtle.home()
      >>> turtle.distance(30,40)
      50.0
      >>> turtle.distance((30,40))
      50.0
      >>> joe = Turtle()
      >>> joe.forward(77)
      >>> turtle.distance(joe)
      77.0


設定と計測
----------

.. function:: degrees(fullcircle=360.0)

   :param fullcircle: 数

   角度を計る単位「度」を、円周を何等分するかという値に指定します。
   デフォルトは360等分で通常の意味での度です。

   .. doctest::

      >>> turtle.home()
      >>> turtle.left(90)
      >>> turtle.heading()
      90.0

      角度の単位をグラード(grad. gon, grade, gradian とも呼ばれ、
      90度の 1/100)に変更する
      >>> turtle.degrees(400.0)
      >>> turtle.heading()
      100.0
      >>> turtle.degrees(360)
      >>> turtle.heading()
      90.0


.. function:: radians()

   角度を計る単位をラジアンにします。
   ``degrees(2*math.pi)`` と同じ意味です。

   .. doctest::

      >>> turtle.home()
      >>> turtle.left(90)
      >>> turtle.heading()
      90.0
      >>> turtle.radians()
      >>> turtle.heading()
      1.5707963267948966

   .. doctest::
      :hide:

      >>> turtle.degrees(360)


Pen の制御
-----------

描画状態
~~~~~~~~~~~~~

.. function:: pendown()
              pd()
              down()

   ペンを下ろします -- 動くと線が引かれます。

.. function:: penup()
              pu()
              up()

   ペンを上げます -- 動いても線は引かれません。

.. function:: pensize(width=None)
              width(width=None)

   :param width: 正の数

   線の太さを *width* にするか、または現在の太さを返します。
   resizemode が "auto" でタートルの形が多角形の場合、
   その多角形も同じ太さで描画されます。
   引数が渡されなければ、現在の pensize が返されます。

   .. doctest::

      >>> turtle.pensize()
      1
      >>> turtle.pensize(10)   # これ以降幅 10 の線が描かれます


.. function:: pen(pen=None, **pendict)

   :param pen: 以下にリストされたキーをもった辞書
   :param pendict: 以下にリストされたキーをキーワードとするキーワード引数

   ペンの属性を "pen-dictionary" に以下のキー/値ペアで設定するかまたは返します。

   * "shown": True/False
   * "pendown": True/False
   * "pencolor": 色文字列または色タプル
   * "fillcolor": 色文字列または色タプル
   * "pensize": 正の数
   * "speed": 0 から 10 までの整数
   * "resizemode": "auto" または "user" または "noresize"
   * "stretchfactor": (正の数, 正の数)
   * "outline": 正の数
   * "tilt": 数

   この辞書を以降の :func:`pen` 呼出しに渡して以前のペンの状態に復旧することができます。
   さらに一つ以上の属性をキーワード引数として渡すこともできます。
   一つの文で幾つものペンの属性を設定するのに使えます。

   .. doctest::
      :options: +NORMALIZE_WHITESPACE

      >>> turtle.pen(fillcolor="black", pencolor="red", pensize=10)
      >>> sorted(turtle.pen().items())
      [('fillcolor', 'black'), ('outline', 1), ('pencolor', 'red'),
       ('pendown', True), ('pensize', 10), ('resizemode', 'noresize'),
       ('shown', True), ('speed', 9), ('stretchfactor', (1, 1)), ('tilt', 0)]
      >>> penstate=turtle.pen()
      >>> turtle.color("yellow", "")
      >>> turtle.penup()
      >>> sorted(turtle.pen().items())
      [('fillcolor', ''), ('outline', 1), ('pencolor', 'yellow'),
       ('pendown', False), ('pensize', 10), ('resizemode', 'noresize'),
       ('shown', True), ('speed', 9), ('stretchfactor', (1, 1)), ('tilt', 0)]
      >>> turtle.pen(penstate, fillcolor="green")
      >>> sorted(turtle.pen().items())
      [('fillcolor', 'green'), ('outline', 1), ('pencolor', 'red'),
       ('pendown', True), ('pensize', 10), ('resizemode', 'noresize'),
       ('shown', True), ('speed', 9), ('stretchfactor', (1, 1)), ('tilt', 0)]


.. function:: isdown()

   もしペンが下りていれば ``True`` を、上がっていれば ``False`` を返します。

   .. doctest::

      >>> turtle.penup()
      >>> turtle.isdown()
      False
      >>> turtle.pendown()
      >>> turtle.isdown()
      True


色の制御
~~~~~~~~~~~~~

.. function:: pencolor(*args)

   ペンの色(pencolor)を設定するかまたは返します。

   4種類の入力形式が受け入れ可能です:

   ``pencolor()``
      現在のペンの色を色指定文字列またはタプルで返します
      (例を見て下さい)。
      次の color/pencolor/fillcolor の呼び出しへの入力に使うこともあるでしょう。

   ``pencolor(colorstring)``
      ペンの色を *colorstring* に設定します。
      その値は Tk の色指定文字列で、 ``"red"``, ``"yellow"``, ``"#33cc8c"``
      のような文字列です。

   ``pencolor((r, g, b))``
      ペンの色を *r*, *g*, *b* のタプルで表された RGB の色に設定します。
      各 *r*, *g*, *b* は 0 から colormode の間の値でなければなりません。
      ここで colormode は 1.0 か 255 のどちらかです (:func:`colormode` を参照)。

   ``pencolor(r, g, b)``
      ペンの色を *r*, *g*, *b* で表された RGB の色に設定します。
      各 *r*, *g*, *b* は 0 から colormode の間の値でなければなりません。

   タートルの形(turtleshape)が多角形の場合、多角形の外側が新しく設定された色で描かれます。

   .. doctest::

       >>> colormode()
       1.0
       >>> turtle.pencolor()
       'red'
       >>> turtle.pencolor("brown")
       >>> turtle.pencolor()
       'brown'
       >>> tup = (0.2, 0.8, 0.55)
       >>> turtle.pencolor(tup)
       >>> turtle.pencolor()
       (0.2, 0.8, 0.5490196078431373)
       >>> colormode(255)
       >>> turtle.pencolor()
       (51, 204, 140)
       >>> turtle.pencolor('#32c18f')
       >>> turtle.pencolor()
       (50, 193, 143)


.. function:: fillcolor(*args)

   塗りつぶしの色(fillcolor)を設定するかまたは返します。

   4種類の入力形式が受け入れ可能です:

   ``fillcolor()``
      現在の塗りつぶしの色を色指定文字列またはタプルで返します
      (例を見て下さい)。
      次の color/pencolor/fillcolor の呼び出しへの入力に使うこともあるでしょう。

   ``fillcolor(colorstring)``
      塗りつぶしの色を *colorstring* に設定します。
      その値は Tk の色指定文字列で、 ``"red"``, ``"yellow"``, ``"#33cc8c"``
      のような文字列です。

   ``fillcolor((r, g, b))``
      塗りつぶしの色を *r*, *g*, *b* のタプルで表された RGB の色に設定します。
      各 *r*, *g*, *b* は 0 から colormode の間の値でなければなりません。
      ここで colormode は 1.0 か 255 のどちらかです (:func:`colormode` を参照)。

   ``fillcolor(r, g, b)``
      塗りつぶしの色を *r*, *g*, *b* で表された RGB の色に設定します。
      各 *r*, *g*, *b* は 0 から colormode の間の値でなければなりません。

   タートルの形(turtleshape)が多角形の場合、多角形の内側が新しく設定された色で描かれます。

   .. doctest::

       >>> turtle.fillcolor("violet")
       >>> turtle.fillcolor()
       'violet'
       >>> col = turtle.pencolor()
       >>> col
       (50, 193, 143)
       >>> turtle.fillcolor(col)
       >>> turtle.fillcolor()
       (50, 193, 143)
       >>> turtle.fillcolor('#ffffff')
       >>> turtle.fillcolor()
       (255, 255, 255)


.. function:: color(*args)

   ペンの色(pencolor)と塗りつぶしの色(fillcolor)を設定するかまたは返します。

   いくつかの入力形式が受け入れ可能です。
   形式ごとに 0 から 3 個の引数を以下のように使います:

   ``color()``
      現在のペンの色と塗りつぶしの色を :func:`pencolor` および
      :func:`fillcolor` で返される色指定文字列またはタプルのペアで返します。

   ``color(colorstring)``, ``color((r,g,b))``, ``color(r,g,b)``
      :func:`pencolor` の入力と同じですが、塗りつぶしの色とペンの色、
      両方を与えられた値に設定します。

   ``color(colorstring1, colorstring2)``, ``color((r1,g1,b1), (r2,g2,b2))``
      ``pencolor(colorstring1)`` および ``fillcolor(colorstring2)``
      を呼び出すのと等価です。
      もう一つの入力形式についても同様です。

   タートルの形(turtleshape)が多角形の場合、多角形の内側も外側も新しく設定された色で描かれます。

   .. doctest::

       >>> turtle.color("red", "green")
       >>> turtle.color()
       ('red', 'green')
       >>> color("#285078", "#a0c8f0")
       >>> color()
       ((40, 80, 120), (160, 200, 240))


こちらも参照: スクリーンのメソッド :func:`colormode` 。


塗りつぶし
~~~~~~~~~~

.. doctest::
   :hide:

   >>> turtle.home()

.. function:: fill(flag)

   :param flag: True/False (またはそれぞれ 1/0)

   塗りつぶしたい形を描く前に ``fill(True)`` を呼び出し、それが終わったら
   ``fill(False)`` を呼び出します。
   引数なしで呼び出されたときは、塗りつぶしの状態(fillstate)の値
   (``True`` なら塗りつぶす、 ``False`` なら塗りつぶさない)を返します。

   .. doctest::

      >>> turtle.fill(True)
      >>> for _ in range(3):
      ...    turtle.forward(100)
      ...    turtle.left(120)
      ...
      >>> turtle.fill(False)


.. function:: begin_fill()

   塗りつぶしたい図形を描く直前に呼び出します。
   ``fill(True)`` と等価です。


.. function:: end_fill()

   最後に呼び出された :func:`begin_fill` の後に描かれた図形を塗りつぶします。
   ``fill(False)`` と等価です。

   .. doctest::

      >>> turtle.color("black", "red")
      >>> turtle.begin_fill()
      >>> turtle.circle(80)
      >>> turtle.end_fill()


さらなる描画の制御
~~~~~~~~~~~~~~~~~~~~

.. function:: reset()

   タートルの描いたものをスクリーンから消し、タートルを中心に戻して、
   全ての変数をデフォルト値に設定し直します。

   .. doctest::

      >>> turtle.goto(0,-22)
      >>> turtle.left(100)
      >>> turtle.position()
      (0.00,-22.00)
      >>> turtle.heading()
      100.0
      >>> turtle.reset()
      >>> turtle.position()
      (0.00,0.00)
      >>> turtle.heading()
      0.0


.. function:: clear()

   タートルの描いたものをスクリーンから消します。タートルは動かしません。
   タートルの状態と位置、それに他のタートルたちの描いたものは影響を受けません。


.. function:: write(arg, move=False, align="left", font=("Arial", 8, "normal"))

   :param arg: TurtleScreen に書かれるオブジェクト
   :param move: True/False
   :param align: 文字列 "left", "center", right" のどれか
   :param font: 三つ組み (fontname, fontsize, fonttype)

   文字を書きます—
   *arg* の文字列表現を、現在のタートルの位置に、
   *align* ("left", "center", right" のどれか) に従って、
   与えられたフォントで。
   もし *move* が True ならば、ペンは書いた文の右下隅に移動します。
   デフォルトでは、 *move* は False です。

   >>> turtle.write("Home = ", True, align="center")
   >>> turtle.write((0,0), True)


タートルの状態
--------------

可視性
~~~~~~~~~~

.. function:: hideturtle()
              ht()

   タートルを見えなくします。
   複雑な図を描いている途中、タートルが見えないようにするのは良い考えです。
   というのもタートルを隠すことで描画が目に見えて速くなるからです。

   .. doctest::

      >>> turtle.hideturtle()


.. function:: showturtle()
              st()

   タートルが見えるようにします。

   .. doctest::

      >>> turtle.showturtle()


.. function:: isvisible()

   タートルが見えている状態ならば True を、隠されていれば False を返します。

   >>> turtle.hideturtle()
   >>> turtle.isvisible()
   False
   >>> turtle.showturtle()
   >>> turtle.isvisible()
   True


見た目
~~~~~~~~~~

.. function:: shape(name=None)

   :param name: 形の名前(shapename)として正しい文字列

   タートルの形を与えられた名前(*name*)の形に設定するか、
   もしくは名前が与えられなければ現在の形の名前を返します。
   *name* という名前の形は TurtleScreen の形の辞書に載っていなければなりません。
   最初は次の多角形が載っています:
   "arrow", "turtle", "circle", "square", "triangle", "classic"。
   形についての扱いを学ぶには Screen のメソッド :func:`register_shape`
   を参照して下さい。

   .. doctest::

      >>> turtle.shape()
      'classic'
      >>> turtle.shape("turtle")
      >>> turtle.shape()
      'turtle'


.. function:: resizemode(rmode=None)

   :param rmode: 文字列 "auto", "user", "noresize" のどれか

   サイズ変更のモード(resizemode)を "auto", "user", "noresize" のどれかに設定します。
   もし *rmode* が与えられなければ、現在のサイズ変更モードを返します。
   それぞれのサイズ変更モードは以下の効果を持ちます:

   - "auto": ペンのサイズに対応してタートルの見た目を調整します。
   - "user": 伸長係数(stretchfactor)およびアウトライン幅(outlinewidth)の値に\
     対応してタートルの見た目を調整します。これらの値は :func:`shapesize` で設定します。
   - "noresize": タートルの見た目を調整しません。

   resizemode("user") は :func:`shapesize` に引数を渡したときに呼び出されます。

   .. doctest::

      >>> turtle.resizemode()
      'noresize'
      >>> turtle.resizemode("auto")
      >>> turtle.resizemode()
      'auto'


.. function:: shapesize(stretch_wid=None, stretch_len=None, outline=None)
              turtlesize(stretch_wid=None, stretch_len=None, outline=None)

   :param stretch_wid: 正の数
   :param stretch_len: 正の数
   :param outline: 正の数

   ペンの属性 x/y-伸長係数および/またはアウトラインを返すかまたは設定します。
   サイズ変更のモードは "user" に設定されます。
   サイズ変更のモードが "user" に設定されたときかつそのときに限り、
   タートルは伸長係数(stretchfactor)に従って伸長されて表示されます。
   *stretch_wid* は進行方向に直交する向きの伸長係数で、
   *stretch_len* は進行方向に沿ったの伸長係数、
   *outline* はアウトラインの幅を決めるものです。

   .. doctest::

      >>> turtle.shapesize()
      (1, 1, 1)
      >>> turtle.resizemode("user")
      >>> turtle.shapesize(5, 5, 12)
      >>> turtle.shapesize()
      (5, 5, 12)
      >>> turtle.shapesize(outline=8)
      >>> turtle.shapesize()
      (5, 5, 8)


.. function:: tilt(angle)

   :param angle: 数

   タートルの形(turtleshape)を現在の傾斜角から角度(*angle*)だけ回転します。
   このときタートルの進む方向は *変わりません* 。

   .. doctest::

      >>> turtle.reset()
      >>> turtle.shape("circle")
      >>> turtle.shapesize(5,2)
      >>> turtle.tilt(30)
      >>> turtle.fd(50)
      >>> turtle.tilt(30)
      >>> turtle.fd(50)


.. function:: settiltangle(angle)

   :param angle: 数

   タートルの形(turtleshape)を現在の傾斜角に関わらず、
   指定された角度(*angle*)の向きに回転します。
   タートルの進む方向は *変わりません* 。

   .. doctest::

      >>> turtle.reset()
      >>> turtle.shape("circle")
      >>> turtle.shapesize(5,2)
      >>> turtle.settiltangle(45)
      >>> turtle.fd(50)
      >>> turtle.settiltangle(-45)
      >>> turtle.fd(50)


.. function:: tiltangle()

   現在の傾斜角を返します。
   すなわち、タートルの形が向いている角度と進んでいく方向との間の角度を返します。

   .. doctest::

      >>> turtle.reset()
      >>> turtle.shape("circle")
      >>> turtle.shapesize(5,2)
      >>> turtle.tilt(45)
      >>> turtle.tiltangle()
      45.0


イベントを利用する
------------------

.. function:: onclick(fun, btn=1, add=None)

   :param fun: 2引数の関数でキャンバスのクリックされた点の座標を引数として\
               呼び出されるものです
   :param num: マウスボタンの番号、デフォルトは 1 (左マウスボタン)
   :param add: ``True`` または ``False`` -- ``True`` ならば、
               新しい束縛が追加されますが、そうでなければ、
               以前の束縛を置き換えます。

   *fun* をタートルのマウスクリック(mouse-click)イベントに束縛します。
   *fun* が ``None`` ならば、既存の束縛が取り除かれます。
   無名タートル、つまり手続き的なやり方の例です:

   .. doctest::

      >>> def turn(x, y):
      ...     left(180)
      ...
      >>> onclick(turn)  # タートルをクリックすると回転します
      >>> onclick(None)  # イベント束縛は消去されます


.. function:: onrelease(fun, btn=1, add=None)

   :param fun: 2引数の関数でキャンバスのクリックされた点の座標を引数として\
               呼び出されるものです
   :param num: マウスボタンの番号、デフォルトは 1 (左マウスボタン)
   :param add: ``True`` または ``False`` -- ``True`` ならば、
               新しい束縛が追加されますが、そうでなければ、
               以前の束縛を置き換えます。

   *fun* をタートルのマウスボタンリリース(mouse-button-release)イベントに束縛します。
   *fun* が ``None`` ならば、既存の束縛が取り除かれます。

   .. doctest::

      >>> class MyTurtle(Turtle):
      ...     def glow(self,x,y):
      ...         self.fillcolor("red")
      ...     def unglow(self,x,y):
      ...         self.fillcolor("")
      ...
      >>> turtle = MyTurtle()
      >>> turtle.onclick(turtle.glow)     # タートル上でクリックすると塗りつぶしの色が赤に
      >>> turtle.onrelease(turtle.unglow) # リリース時に透明に


.. function:: ondrag(fun, btn=1, add=None)

   :param fun: 2引数の関数でキャンバスのクリックされた点の座標を引数として\
               呼び出されるものです
   :param num: マウスボタンの番号、デフォルトは 1 (左マウスボタン)
   :param add: ``True`` または ``False`` -- ``True`` ならば、
               新しい束縛が追加されますが、そうでなければ、
               以前の束縛を置き換えます。

   *fun* をタートルのマウスムーブ(mouse-move)イベントに束縛します。
   *fun* が ``None`` ならば、既存の束縛が取り除かれます。

   注意: 全てのマウスムーブイベントのシーケンスに先立ってマウスクリックイベントが\
   起こります。

   .. doctest::

      >>> turtle.ondrag(turtle.goto)

   この後、タートルをクリックしてドラッグするとタートルはスクリーン上を動き
   それによって(ペンが下りていれば)手書きの線ができあがります


特別な Turtle のメソッド
------------------------

.. function:: begin_poly()

   多角形の頂点の記録を開始します。現在のタートル位置が最初の頂点です。


.. function:: end_poly()

   多角形の頂点の記録を停止します。現在のタートル位置が最後の頂点です。
   この頂点が最初の頂点と結ばれます。


.. function:: get_poly()

   最後に記録された多角形を返します。

   .. doctest::

      >>> turtle.home()
      >>> turtle.begin_poly()
      >>> turtle.fd(100)
      >>> turtle.left(20)
      >>> turtle.fd(30)
      >>> turtle.left(60)
      >>> turtle.fd(50)
      >>> turtle.end_poly()
      >>> p = turtle.get_poly()
      >>> register_shape("myFavouriteShape", p)


.. function:: clone()

   位置、向きその他のプロパティがそっくり同じタートルのクローンを作って返します。

   .. doctest::

      >>> mick = Turtle()
      >>> joe = mick.clone()


.. function:: getturtle()
              getpen()

   Turtle オブジェクトそのものを返します。
   唯一の意味のある使い方: 無名タートルを返す関数として使う。

   .. doctest::

      >>> pet = getturtle()
      >>> pet.fd(50)
      >>> pet
      <turtle.Turtle object at 0x...>


.. function:: getscreen()

   タートルが描画中の :class:`TurtleScreen` オブジェクトを返します。
   TurtleScreen のメソッドをそのオブジェクトに対して呼び出すことができます。

   .. doctest::

      >>> ts = turtle.getscreen()
      >>> ts
      <turtle._Screen object at 0x...>
      >>> ts.bgcolor("pink")


.. function:: setundobuffer(size)

   :param size: 整数または ``None``

   アンドゥバッファを設定または無効化します。
   *size* が整数ならばそのサイズの空のアンドゥバッファを用意します。
   *size* の値はタートルのアクションを何度 :func:`undo` メソッド/関数で\
   取り消せるかの最大数を与えます。
   *size* が ``None`` ならば、アンドゥバッファは無効化されます。

   .. doctest::

      >>> turtle.setundobuffer(42)


.. function:: undobufferentries()

   アンドゥバッファのエントリー数を返します。

   .. doctest::

      >>> while undobufferentries():
      ...     undo()


.. function:: tracer(flag=None, delay=None)

   対応する TurtleScreen のメソッドの複製です。

   .. deprecated:: 2.6


.. function:: window_width()
              window_height()

   どちらも対応する TurtleScreen のメソッドの複製です。

   .. deprecated:: 2.6


.. _compoundshapes:

合成形の使用に関する補遺
------------------------

合成されたタートルの形、つまり幾つかの色の違う多角形から成るような形を使うには、
以下のように補助クラス :class:`Shape` を直接使わなければなりません:

1. タイプ "compound" の空の Shape オブジェクトを作ります。
2. :meth:`addcomponent` メソッドを使って、好きなだけここにコンポーネントを追加します。

   例えば:

   .. doctest::

      >>> s = Shape("compound")
      >>> poly1 = ((0,0),(10,-5),(0,10),(-10,-5))
      >>> s.addcomponent(poly1, "red", "blue")
      >>> poly2 = ((0,0),(10,-5),(-10,-5))
      >>> s.addcomponent(poly2, "blue", "red")

3. こうして作った Shape を Screen の形のリスト(shapelist) に追加して使います:

   .. doctest::

      >>> register_shape("myshape", s)
      >>> shape("myshape")


.. note::

   :class:`Shape` クラスは :func:`register_shape` の内部では違った使われ方をします。
   アプリケーションを書く人が Shape クラスを扱わなければならないのは、
   上で示したように合成された形を使うとき *だけ* です。


TurtleScreen/Screen のメソッドと対応する関数
============================================

この節のほとんどの例では ``screen`` という名前の TurtleScreen インスタンスを使います。

.. doctest::
   :hide:

   >>> screen = Screen()

ウィンドウの制御
----------------

.. function:: bgcolor(*args)

   :param args: 色文字列または 0 から colormode の範囲の数3つ、
                またはそれを三つ組みにしたもの

   TurtleScreen の背景色を設定するかまたは返します。

   .. doctest::

      >>> screen.bgcolor("orange")
      >>> screen.bgcolor()
      'orange'
      >>> screen.bgcolor("#800080")
      >>> screen.bgcolor()
      (128, 0, 128)


.. function:: bgpic(picname=None)

   :param picname: 文字列で gif ファイルの名前 ``"nopic"`` 、または ``None``

   背景の画像を設定するかまたは現在の背景画像(backgroundimage)の名前を返します。
   *picname* がファイル名ならば、その画像を背景に設定します。
   *picname* が ``"nopic"`` ならば、(もしあれば)背景画像を削除します。
   *picname* が ``None`` ならば、現在の背景画像のファイル名を返します。 ::

      >>> screen.bgpic()
      "nopic"
      >>> screen.bgpic("landscape.gif")
      >>> screen.bgpic()
      "landscape.gif"


.. function:: clear()
              clearscreen()

   全ての図形と全てのタートルを TurtleScreen から削除します。
   そして空になった TurtleScreen をリセットして初期状態に戻します:
   白い背景、背景画像もイベント束縛もなく、トレーシングはオンです。

   .. note::
      この TurtleScreen メソッドはグローバル関数としては ``clearscreen``
      という名前でだけ使えます。
      グローバル関数 ``clear`` は Turtle メソッドの ``clear``
      から派生した別ものです。


.. function:: reset()
              resetscreen()

   スクリーン上の全てのタートルをリセットしその初期状態に戻します。

   .. note::
      この TurtleScreen メソッドはグローバル関数としては ``resetscreen``
      という名前でだけ使えます。
      グローバル関数 ``reset`` は Turtle メソッドの ``reset``
      から派生した別ものです。


.. function:: screensize(canvwidth=None, canvheight=None, bg=None)

   :param canvwidth: 正の整数でピクセル単位の新しいキャンバス幅(canvaswidth)
   :param canvheight: 正の整数でピクセル単位の新しいキャンバス高さ(canvasheight)
   :param bg: 色文字列または色タプルで新しい背景色

   引数が渡されなければ、現在の (キャンバス幅, キャンバス高さ) を返します。
   そうでなければタートルが描画するキャンバスのサイズを変更します。
   描画ウィンドウには影響しません。
   キャンバスの隠れた部分を見るためにはスクロールバーを使って下さい。
   このメソッドを使うと、以前はキャンバスの外にあったそうした図形の一部を\
   見えるようにすることができます。

      >>> screen.screensize()
      (400, 300)
      >>> turtle.screensize(2000,1500)
      >>> screen.screensize()
      (2000, 1500)

   # 逃げ出してしまったタートルを探すためとかね ;-)


.. function:: setworldcoordinates(llx, lly, urx, ury)

   :param llx: 数でキャンバスの左下隅の x-座標
   :param lly: 数でキャンバスの左下隅の y-座標
   :param urx: 数でキャンバスの右上隅の x-座標
   :param ury: 数でキャンバスの右上隅の y-座標

   ユーザー定義座標系を準備し必要ならばモードを "world" に切り替えます。
   この動作は ``screen.reset()`` を伴います。
   すでに "world" モードになっていた場合、全ての図形は新しい座標に従って再描画されます。

   **重要なお知らせ**: ユーザー定義座標系では角度が歪むかもしれません。

   .. doctest::

      >>> screen.reset()
      >>> screen.setworldcoordinates(-50,-7.5,50,7.5)
      >>> for _ in range(72):
      ...     left(10)
      ...
      >>> for _ in range(8):
      ...     left(45); fd(2)   # 正八角形

   .. doctest::
      :hide:

      >>> screen.reset()
      >>> for t in turtles():
      ...      t.reset()


アニメーションの制御
--------------------

.. function:: delay(delay=None)

   :param delay: 正の整数

   描画の遅延(*delay*)をミリ秒単位で設定するかまたはその値を返します。
   (これは概ね引き続くキャンバス更新の時間間隔です。)
   遅延が大きくなると、アニメーションは遅くなります。

   オプション引数:

   .. doctest::

      >>> screen.delay()
      10
      >>> screen.delay(5)
      >>> screen.delay()
      5


.. function:: tracer(n=None, delay=None)

   :param n: 非負整数
   :param delay: 非負整数

   タートルのアニメーションをオン・オフし、描画更新の遅延を設定します。
   *n* が与えられた場合、通常のスクリーン更新のうち 1/n しか実際に実行されません。
   (複雑なグラフィックスの描画を加速するのに使えます。)
   二つ目の引数は遅延の値を設定します(:func:`delay` も参照)。

   .. doctest::

      >>> screen.tracer(8, 25)
      >>> dist = 2
      >>> for i in range(200):
      ...     fd(dist)
      ...     rt(90)
      ...     dist += 2


.. function:: update()

   TurtleScreen の更新を実行します。
   トレーサーがオフの時に使われます。


RawTurtle/Turtle のメソッド :func:`speed` も参照して下さい。


スクリーンイベントを利用する
----------------------------

.. function:: listen(xdummy=None, ydummy=None)

   TurtleScreen に(キー・イベントを収集するために)フォーカスします。
   ダミー引数は :func:`listen` を onclick メソッドに渡せるようにするためのものです。


.. function:: onkey(fun, key)

   :param fun: 引数なしの関数または ``None``
   :param key: 文字列: キー (例 "a") またはキー・シンボル (例 "space")

   *fun* を指定されたキーのキーリリース(key-release)イベントに束縛します。
   *fun* が ``None`` ならばイベント束縛は除かれます。
   注意: キー・イベントを登録できるようにするためには TurtleScreen
   はフォーカスを持っていないとなりません(:func:`listen` を参照)。

   .. doctest::

      >>> def f():
      ...     fd(50)
      ...     lt(60)
      ...
      >>> screen.onkey(f, "Up")
      >>> screen.listen()


.. function:: onclick(fun, btn=1, add=None)
              onscreenclick(fun, btn=1, add=None)

   :param fun: 2引数の関数でキャンバスのクリックされた点の座標を引数として\
               呼び出されるものです
   :param num: マウスボタンの番号、デフォルトは 1 (左マウスボタン)
   :param add: ``True`` または ``False`` -- ``True`` ならば、
               新しい束縛が追加されますが、そうでなければ、
               以前の束縛を置き換えます。

   *fun* をタートルのマウスクリック(mouse-click)イベントに束縛します。
   *fun* が ``None`` ならば、既存の束縛が取り除かれます。

   Example for a
   ``screen`` という名の TurtleScreen インスタンスと turtle という名前の
   Turtle インスタンスの例:

   .. doctest::

      >>> screen.onclick(turtle.goto) # この後、TurtleScreen をクリックすると
      >>>                             # タートルをクリックされた点に
      >>>                             # 移動させることになります
      >>> screen.onclick(None)  # イベント束縛を取り除きます

   .. note::
      この TurtleScreen メソッドはグローバル関数としては ``onscreenclick``
      という名前でだけ使えます。
      グローバル関数 ``onclick`` は Turtle メソッドの ``onclick``
      から派生した別ものです。


.. function:: ontimer(fun, t=0)

   :param fun: 引数なし関数
   :param t: 数 >= 0

   *t* ミリ秒後に *fun* を呼び出すタイマーを仕掛けます。

   .. doctest::

      >>> running = True
      >>> def f():
      ...     if running:
      ...         fd(50)
      ...         lt(60)
      ...         screen.ontimer(f, 250)
      >>> f()   ### タートルが歩き続けます
      >>> running = False


設定と特殊なメソッド
--------------------

.. function:: mode(mode=None)

   :param mode: 文字列 "standard", "logo", "world" のいずれか

   タートルのモード("standard", "logo", "world" のいずれか)を設定してリセットします。
   モードが渡されなければ現在のモードが返されます。

   モード "standard" は古い :mod:`turtle` 互換です。
   モード "logo" は Logo タートルグラフィックスとほぼ互換です。
   モード "world" はユーザーの定義した「世界座標(world coordinates)」を使います。
   **重要なお知らせ**: このモードでは ``x/y`` 比が 1 でないと角度が歪むかもしれません。

   ============ ========================= ===================
      モード      タートルの向きの初期値      正の角度
   ============ ========================= ===================
    "standard"    右 (東) 向き              反時計回り
      "logo"      上 (北) 向き              時計回り
   ============ ========================= ===================

   .. doctest::

      >>> mode("logo")   # タートルが北を向くようにリセットします
      >>> mode()
      'logo'


.. function:: colormode(cmode=None)

   :param cmode: 1.0 か 255 のどちらかの値

   色モード(colormode)を返すか、または 1.0 か 255 のどちらかの値に設定します。
   設定した後は、色トリプルの *r*, *g*, *b* 値は 0 から *cmode*
   の範囲になければなりません。

   .. doctest::

      >>> screen.colormode(1)
      >>> turtle.pencolor(240, 160, 80)
      Traceback (most recent call last):
           ...
      TurtleGraphicsError: bad color sequence: (240, 160, 80)
      >>> screen.colormode()
      1.0
      >>> screen.colormode(255)
      >>> screen.colormode()
      255
      >>> turtle.pencolor(240,160,80)


.. function:: getcanvas()

   この TurtleScreen の Canvas を返します。
   Tkinter の Canvas を使って何をするか知っている人には有用です。

   .. doctest::

      >>> cv = screen.getcanvas()
      >>> cv
      <turtle.ScrolledCanvas instance at 0x...>


.. function:: getshapes()

   現在使うことのできる全てのタートルの形のリストを返します。

   .. doctest::

      >>> screen.getshapes()
      ['arrow', 'blank', 'circle', ..., 'turtle']


.. function:: register_shape(name, shape=None)
              addshape(name, shape=None)

   この関数を呼び出す三つの異なる方法があります:

   (1) *name* が gif ファイルの名前で *shape* が ``None``:
       対応する画像の形を取り込みます。 ::

       >>> screen.register_shape("turtle.gif")

       .. note::
          画像の形はタートルが向きを変えても *回転しません* ので、
          タートルがどちらを向いているか見ても判りません!

   (2) *name* が任意の文字列で *shape* が座標ペアのタプル:
       対応する多角形を取り込みます。

       .. doctest::

          >>> screen.register_shape("triangle", ((5,-3), (0,5), (-5,-3)))

   (3) *name* が任意の文字列で *shape* が (合成形の) :class:`Shape`
       オブジェクト: 対応する合成形を取り込みます。

   タートルの形を TurtleScreen の形リスト(shapelist)に加えます。
   このように登録された形だけが ``shape(shapename)`` コマンドに使えます。


.. function:: turtles()

   スクリーン上のタートルのリストを返します。

   .. doctest::

      >>> for turtle in screen.turtles():
      ...     turtle.color("red")


.. function:: window_height()

   タートルウィンドウの高さを返します。 ::

      >>> screen.window_height()
      480


.. function:: window_width()

   タートルウィンドウの幅を返します。 ::

      >>> screen.window_width()
      640


.. _screenspecific:

Screen 独自のメソッド、TurtleScreen から継承したもの以外
--------------------------------------------------------

.. function:: bye()

   タートルグラフィックス(turtlegraphics)のウィンドウを閉じます。


.. function:: exitonclick()

   スクリーン上のマウスクリックに bye() メソッドを束縛します。


   設定辞書中の "using_IDLE" の値が ``False`` (デフォルトです) の場合、
   さらにメインループ(mainloop)に入ります。
   注意: もし IDLE が ``-n`` スイッチ(サブプロセスなし)付きで使われているときは、
   この値は :file:`turtle.cfg` の中で ``True`` とされているべきです。
   この場合、IDLE のメインループもクライアントスクリプトから見てアクティブです。

     .. どういうことだか解らずに訳しています


.. function:: setup(width=_CFG["width"], height=_CFG["height"], startx=_CFG["leftright"], starty=_CFG["topbottom"])

   メインウィンドウのサイズとポジションを設定します。
   引数のデフォルト値は設定辞書に収められており、
   :file:`turtle.cfg` ファイルを通じて変更できます。

   :param width: 整数ならばピクセル単位のサイズ、浮動小数点数ならばスクリーンに対する割合
                 (スクリーンの 50% がデフォルト)
   :param height: 整数ならばピクセル単位の高さ、浮動小数点数ならばスクリーンに対する割合
                  (スクリーンの 75% がデフォルト)
   :param startx: 正の数ならばスクリーンの左端からピクセル単位で測った開始位置、
                  負の数ならば右端から、None ならば水平方向に真ん中
   :param startx: 正の数ならばスクリーンの上端からピクセル単位で測った開始位置、
                  負の数ならば下端から、None ならば垂直方向に真ん中

   .. doctest::

      >>> screen.setup (width=200, height=200, startx=0, starty=0)
      >>>              # ウィンドウを 200×200 ピクセルにして, スクリーンの左上に
      >>> screen.setup(width=.75, height=0.5, startx=None, starty=None)
      >>>              # ウィンドウをスクリーンの 75% かける 50% にして, スクリーンの真ん中に


.. function:: title(titlestring)

   :param titlestring: タートルグラフィックスウィンドウのタイトルバーに表示される文字列

   ウインドウのタイトルを *titlestring* に設定します。

   .. doctest::

      >>> screen.title("Welcome to the turtle zoo!")


:mod:`turtle` モジュールのパブリッククラス
==========================================


.. class:: RawTurtle(canvas)
           RawPen(canvas)

   :param canvas: :class:`Tkinter.Canvas`, :class:`ScrolledCanvas`,
                  :class:`TurtleScreen` のいずれか

   タートルを作ります。
   タートルには上の「Turtle/RawTurtle のメソッド」で説明した全てのメソッドがあります。


.. class:: Turtle()

   RawTurtle のサブクラスで同じインターフェイスを持ちますが、
   最初に必要になったとき自動的に作られる :class:`Screen` オブジェクトに描画します。


.. class:: TurtleScreen(cv)

   :param cv: :class:`Tkinter.Canvas`

   上で説明した :func:`setbg` のようなスクリーン向けのメソッドを提供します。

.. class:: Screen()

   TurtleScreen のサブクラスで :ref:`4つのメソッドが加わっています <screenspecific>` 。

.. class:: ScrolledCanvas(master)

   :param master: この ScrolledCanvas すなわちスクロールバーの付いた Tkinter
      canvas を収める Tkinter ウィジェット

   タートルたちが遊び回る場所として自動的に ScrolledCanvas を提供する
   Screen クラスによって使われます

.. class:: Shape(type_, data)

   :param type\_: 文字列 "polygon", "image", "compound" のいずれか

   形をモデル化するデータ構造。
   ペア ``(type_, data)`` は以下の仕様に従わなければなりません。


   =========== ===========
   *type_*     *data*
   =========== ===========
   "polygon"   多角形タプル、すなわち座標ペアのタプル
   "image"     画像  (この形式は内部的にのみ使用されます!)
   "compound"  ``None`` (合成形は :meth:`addcomponent` メソッドを使って\
               作らなければなりません)
   =========== ===========

   .. method:: addcomponent(poly, fill, outline=None)

      :param poly: 多角形、すなわち数のペアのタプル
      :param fill: *poly* を塗りつぶす色
      :param outline: *poly* のアウトラインの色 (与えられた場合)

      例:

      .. doctest::

         >>> poly = ((0,0),(10,-5),(0,10),(-10,-5))
         >>> s = Shape("compound")
         >>> s.addcomponent(poly, "red", "blue")
         >>> # .. もっと成分を増やした後 register_shape() を使います

      :ref:`compoundshapes` を参照。


.. class:: Vec2D(x, y)

   2次元ベクトルのクラスで、タートルグラフィックスを実装するための補助クラス。
   タートルグラフィックスを使ったプログラムでも有用でしょう。
   タプルから派生しているので、ベクターはタプルです!

   以下の演算が使えます (*a*, *b* はベクトル、 *k* は数):

   * ``a + b`` ベクトル和
   * ``a - b`` ベクトル差
   * ``a * b`` 内積
   * ``k * a`` および ``a * k`` スカラー倍
   * ``abs(a)``  a の絶対値
   * ``a.rotate(angle)`` 回転


ヘルプと設定
============

ヘルプの使い方
---------------

Screen と Turtle クラスのパブリックメソッドはドキュメント文字列で網羅的に文書化されていますので、Python のヘルプ機能を通じてオンラインヘルプとして利用できます:

- IDLE を使っているときは、打ち込んだ関数/メソッド呼び出しのシグニチャとドキュメント文字列の一行目がツールチップとして表示されます。

- :func:`help` をメソッドや関数に対して呼び出すとドキュメント文字列が表示されます::

     >>> help(Screen.bgcolor)
     Help on method bgcolor in module turtle:

     bgcolor(self, *args) unbound turtle.Screen method
         Set or return backgroundcolor of the TurtleScreen.

         Arguments (if given): a color string or three numbers
         in the range 0..colormode or a 3-tuple of such numbers.


           >>> screen.bgcolor("orange")
           >>> screen.bgcolor()
           "orange"
           >>> screen.bgcolor(0.5,0,0.5)
           >>> screen.bgcolor()
           "#800080"

     >>> help(Turtle.penup)
     Help on method penup in module turtle:

     penup(self) unbound turtle.Turtle method
         Pull the pen up -- no drawing when moving.

         Aliases: penup | pu | up

         No argument

         >>> turtle.penup()

- メソッドに由来する関数のドキュメント文字列は変更された形をとります::

     >>> help(bgcolor)
     Help on function bgcolor in module turtle:

     bgcolor(*args)
         Set or return backgroundcolor of the TurtleScreen.

         Arguments (if given): a color string or three numbers
         in the range 0..colormode or a 3-tuple of such numbers.

         Example::

           >>> bgcolor("orange")
           >>> bgcolor()
           "orange"
           >>> bgcolor(0.5,0,0.5)
           >>> bgcolor()
           "#800080"

     >>> help(penup)
     Help on function penup in module turtle:

     penup()
         Pull the pen up -- no drawing when moving.

         Aliases: penup | pu | up

         No argument

         Example:
         >>> penup()

これらの変更されたドキュメント文字列はインポート時にメソッドから導出される関数定義と一緒に自動的に作られます。


ドキュメント文字列の翻訳
------------------------

Screen と Turtle クラスのパブリックメソッドについて、
キーがメソッド名で値がドキュメント文字列である辞書を作るユーティリティがあります。

.. function:: write_docstringdict(filename="turtle_docstringdict")

   :param filename: ファイル名として使われる文字列

   ドキュメント文字列辞書(docstring-dictionary)を作って与えられたファイル名の
   Python スクリプトに書き込みます。
   この関数はわざわざ呼び出さなければなりません (タートルグラフィックスのクラスから\
   使われることはありません)。
   ドキュメント文字列辞書は :file:`{filename}.py` という Python
   スクリプトに書き込まれます。
   ドキュメント文字列の異なった言語への翻訳に対するテンプレートとして\
   使われることを意図したものです。


もしあなたが(またはあなたの生徒さんが) :mod:`turtle` を自国語のオンラインヘルプ\
付きで使いたいならば、ドキュメント文字列を翻訳してできあがったファイルをたとえば
:file:`turtle_docstringdict_german.py` という名前で保存しなければなりません。

さらに :file:`turtle.cfg` ファイルで適切な設定をしておけば、
このファイルがインポート時に読み込まれて元の英語のドキュメント文字列を置き換えます。

この文書を書いている時点ではドイツ語とイタリア語のドキュメント文字列辞書が存在します。
( glingl@aon.at にリクエストして下さい。)


Screen および Turtle の設定方法
-------------------------------

初期デフォルト設定では古い turtle の見た目と振る舞いを真似るようにして、
互換性を最大限に保つようにしています。

このモジュールの特性を反映した、あるいは個々人の必要性
(たとえばクラスルームでの使用)に合致した、異なった設定を使いたい場合、
設定ファイル ``turtle.cfg`` を用意してインポート時に読み込ませその設定に\
従わせることができます。

初期設定は以下の turtle.cfg に対応します::

   width = 0.5
   height = 0.75
   leftright = None
   topbottom = None
   canvwidth = 400
   canvheight = 300
   mode = standard
   colormode = 1.0
   delay = 10
   undobuffersize = 1000
   shape = classic
   pencolor = black
   fillcolor = black
   resizemode = noresize
   visible = True
   language = english
   exampleturtle = turtle
   examplescreen = screen
   title = Python Turtle Graphics
   using_IDLE = False

いくつかピックアップしたエントリーの短い説明:

- 最初の4行は :meth:`Screen.setup` メソッドの引数に当たります。
- 5行目6行目は :meth:`Screen.screensize` メソッドの引数に当たります。
- *shape* は最初から用意されている形ならどれでも使えます(arrow, turtle など)。
  詳しくは ``help(shape)`` をお試し下さい。
- 塗りつぶしの色(fillcolor)を使いたくない(つまりタートルを透明にしたい)場合、
  ``fillcolor = ""`` と書かなければなりません。
  (しかし全ての空でない文字列は cfg ファイル中で引用符を付けてはいけません)
- タートルにその状態を反映させるためには ``resizemode = auto`` とします。
- たとえば ``language = italian`` とするとドキュメント文字列辞書(docstringdict)
  として :file:`turtle_docstringdict_italian.py` がインポート時に読み込まれます
  (もしそれがインポートパス、たとえば :mod:`turtle` と同じディレクトリにあれば)。
- *exampleturtle* および *examplescreen* はこれらのオブジェクトの\
  ドキュメント文字列内での呼び名を決めます。
  メソッドのドキュメント文字列から関数のドキュメント文字列に変換する際に、
  これらの名前は取り除かれます。
- *using_IDLE*: IDLE とその -n スイッチ(サブプロセスなし)を常用するならば、
  この値を ``True`` に設定して下さい。
  これにより :func:`exitonclick` がメインループ(mainloop)に入るのを阻止します。

:file:`turtle.cfg` ファイルは :mod:`turtle` の保存されているディレクトリと\
現在の作業ディレクトリに追加的に存在し得ます。
後者が前者の設定をオーバーライドします。

:file:`Demo/turtle` ディレクトリにも :file:`turtle.cfg` ファイルがあります。
デモを実際に(できればデモビュワーからでなく)実行してそこに書かれたものとその効果を\
学びましょう。


デモスクリプト
==============

ソース配布物の :file:`Demo/turtle` ディレクトリにデモスクリプト一式があります。

内容は以下の通りです:

- 新しい :mod:`turtle` モジュールの 15 の異なった特徴を示すデモスクリプト一式
- ソースコードを眺めつつスクリプトを実行できるデモビュワー :file:`turtleDemo.py` 。
  14 個が Examples メニューからアクセスできます。
  もちろんそれらを独立して実行することもできます。
- :file:`turtledemo_two_canvases.py` は同時に二つのキャンバスを使用するデモです。
  これはビュワーからは実行できません。
- :file:`turtle.cfg` ファイルも同じディレクトリにあり、
  設定ファイルの書き方の例としても参考にできます。

デモスクリプトは以下の通りです:

+----------------+------------------------------+-----------------------+
| 名前           | 説明                         | 特徴                  |
+----------------+------------------------------+-----------------------+
| bytedesign     | 複雑な古典的タートル\        | :func:`tracer`, delay,|
|                | グラフィックスパターン       | :func:`update`        |
+----------------+------------------------------+-----------------------+
| chaos          | verhust 力学系のグラフ化,    | 世界座標系            |
|                | コンピュータの計算が常識的   |                       |
|                | な予想に反する場合があること |                       |
|                | を示します。                 |                       |
+----------------+------------------------------+-----------------------+
| clock          | コンピュータの時間を示す\    | タートルが時計の針,   |
|                | アナログ時計                 | ontimer               |
+----------------+------------------------------+-----------------------+
| colormixer     | r, g, b の実験               | :func:`ondrag`        |
+----------------+------------------------------+-----------------------+
| fractalcurves  | Hilbert & Koch 曲線          | 再帰                  |
+----------------+------------------------------+-----------------------+
| lindenmayer    | 民俗的数学                   | L-システム            |
|                | (インド kolams)              |                       |
+----------------+------------------------------+-----------------------+
| minimal_hanoi  | ハノイの塔                   | ハノイ盤として正方形\ |
|                |                              | のタートル            |
|                |                              | (shape, shapesize)    |
+----------------+------------------------------+-----------------------+
| paint          | 超極小主義的描画プログラム   | :func:`onclick`       |
+----------------+------------------------------+-----------------------+
| peace          | 初歩的                       | turtle: 見た目と\     |
|                |                              | アニメーション        |
+----------------+------------------------------+-----------------------+
| penrose        | 凧と矢による非周期的\        | :func:`stamp`         |
|                | タイリング                   |                       |
+----------------+------------------------------+-----------------------+
| planet_and_moon| 重力系のシミュレーション     | 合成形,               |
|                |                              | :class:`Vec2D`        |
+----------------+------------------------------+-----------------------+
| tree           | (図形的) 幅優先木            | :func:`clone`         |
|                | (ジェネレータを使って)       |                       |
+----------------+------------------------------+-----------------------+
| wikipedia      | タートルグラフィックスにつ\  | :func:`clone`,        |
|                | いての wikipedia の記事の例  | :func:`undo`          |
+----------------+------------------------------+-----------------------+
| yingyang       | もう一つの初歩的な例         | :func:`circle`        |
+----------------+------------------------------+-----------------------+

楽しんでね!

.. doctest::
   :hide:

   >>> for turtle in turtles():
   ...      turtle.reset()
   >>> turtle.penup()
   >>> turtle.goto(-200,25)
   >>> turtle.pendown()
   >>> turtle.write("No one expects the Spanish Inquisition!",
   ...      font=("Arial", 20, "normal"))
   >>> turtle.penup()
   >>> turtle.goto(-100,-50)
   >>> turtle.pendown()
   >>> turtle.write("Our two chief Turtles are...",
   ...      font=("Arial", 16, "normal"))
   >>> turtle.penup()
   >>> turtle.goto(-450,-75)
   >>> turtle.write(str(turtles()))
