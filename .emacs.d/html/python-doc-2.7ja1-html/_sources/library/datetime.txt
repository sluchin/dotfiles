:mod:`datetime` --- 基本的な日付型および時間型
==============================================

.. module:: datetime
   :synopsis: 基本的な日付型および時間型。
.. moduleauthor:: Tim Peters <tim@zope.com>
.. sectionauthor:: Tim Peters <tim@zope.com>
.. sectionauthor:: A.M. Kuchling <amk@amk.ca>

.. XXX what order should the types be discussed in?

.. versionadded:: 2.3

:mod:`datetime` モジュールでは、日付や時間データを簡単な方法と複雑な方
法の両方で操作するためのクラスを提供しています。日付や時刻を対象にした
四則演算がサポートされている一方で、このモジュールの実装では出力の書式
化や操作を目的としたデータメンバの効率的な取り出しに焦点を絞っています。
機能については、 :mod:`time` および :mod:`calendar` も参照下さい。

日付および時刻オブジェクトには、 "naive" および "aware" の 2 種類があ
ります。この区別はオブジェクトがタイムゾーンや夏時間、あるいはその他の
アルゴリズム的、政治的な理由による時刻の修正に関する何らかの表記をもつ
かどうかによるものです。
特定の数字がメートルか、マイルか、質量を表すかといったことがプログラム
の問題であるように、 naive な :class:`datetime` オブジェクトが標準世界
時 (UTC: Coordinated Universal time) を表現するか、ローカルの時刻を表
現するか、あるいは他のいずれかのタイムゾーンにおける時刻を表現するかは
純粋にプログラムの問題となります。 naive な :class:`datetime` オブジェ
クトは、現実世界のいくつかの側面を無視するという犠牲のもとに、理解しや
すく、かつ利用しやすくなっています。

より多くの情報を必要とするアプリケーションのために、 :class:`datetime`
および :class:`time` オブジェクトはオプションのタイムゾーン情報メンバ、
:attr:`tzinfo` を持っています。このメンバには抽象クラス
:class:`tzinfo` のサブクラスのインスタンスが入っています。
:class:`tzinfo` オブジェクトは UTC 時刻からのオフセット、タイムゾーン
名、夏時間が有効になっているかどうか、といった情報を記憶しています。
:mod:`datetime` モジュールでは具体的な :class:`tsinfo` クラスを提供し
ていないので注意してください。必要な詳細仕様を備えたタイムゾーン機能
を提供するのはアプリケーションの責任です。世界各国における時刻の修正に
関する法則は合理的というよりも政治的なものであり、全てのアプリケーショ
ンに適した標準というものが存在しないのです。

:mod:`datetime` モジュールでは以下の定数を公開しています:

.. data:: MINYEAR

   :class:`date` や :class:`datetime` オブジェクトで許されている、年を
   表現する最小の数字です。 :const:`MINYEAR` は ``1`` です。


.. data:: MAXYEAR

   :class:`date` や :class:`datetime` オブジェクトで許されている、年を
   表現する最大の数字です。 :const:`MAXYEAR` は ``9999`` です。


.. seealso::

   Module :mod:`calendar`
      汎用のカレンダー関連関数。

   Module :mod:`time`
      時刻へのアクセスと変換。


利用可能なデータ型
------------------

.. class:: date
   :noindex:

   理想化された naive な日付表現で、実質的には、これまでもこれからも現
   在のグレゴリオ暦 (Gregorian calender) であると仮定しています。
   属性: :attr:`year`, :attr:`month`,および :attr:`day` 。


.. class:: time
   :noindex:

   理想化された時刻表現で、あらゆる特定の日における影響から独立してお
   り、毎日厳密に 24\*60\*60 秒であると仮定します ("うるう秒: leap
   seconds" の概念はありません)。属性: :attr:`hour`, :attr:`minute`,
   :attr:`second`, :attr:`microsecond`, および :attr:`tzinfo` 。


.. class:: datetime
   :noindex:

   日付と時刻を組み合わせたもの。属性: :attr:`year`, :attr:`month`,
   :attr:`day`, :attr:`hour`, :attr:`minute`, :attr:`second`,
   :attr:`microsecond`, および :attr:`tzinfo` 。


.. class:: timedelta
   :noindex:

   :class:`date`, :class:`time`,あるいは :class:`datetime` クラス
   の二つのインスタンス間の時間差をマイクロ秒精度で表す経過時間値です。


.. class:: tzinfo

   タイムゾーン情報オブジェクトの抽象基底クラスです。
   :class:`datetime` および :class:`time` クラスで用いられ、カスタマイ
   ズ可能な時刻修正の概念 (たとえばタイムゾーンや夏時間の計算）を提供します。

これらの型のオブジェクトは変更不可能 (immutable) です。

:class:`date` 型のオブジェクトは常に naive です。

:class:`time` や :class:`datetime` 型のオブジェクト *d* は naive にも
aware にもできます。 *d* は ``d.tzinfo`` が ``None`` でなく、かつ
``d.tzinfo.utcoffset(d)`` が ``None`` を返さない場合に aware となりま
す。 ``d.tzinfo`` が ``None`` の場合や、 ``d.tzinfo`` は ``None`` では
ないが ``d.tzinfo.utcoffset(d)`` が ``None`` を返す場合には、 *d* は
naive となります。

naive なオブジェクトと aware なオブジェクトの区別は :class:`timedelta`
オブジェクトにはあてはまりません。

サブクラスの関係は以下のようになります::

   object
       timedelta
       tzinfo
       time
       date
           datetime


.. _datetime-timedelta:

:class:`timedelta` オブジェクト
-------------------------------

:class:`timedelta` オブジェクトは経過時間、すなわち二つの日付や時刻間の差を表します。

.. class:: timedelta([days[, seconds[, microseconds[, milliseconds[, minutes[, hours[, weeks]]]]]]])

   全ての引数がオプションで、デフォルト値は *0* です。引数は整数、長整
   数、浮動小数点数にすることができ、正でも負でもかまいません。

   *days*, *seconds* および *microseconds* のみが内部に記憶されます。
    引数は以下のようにして変換されます:

   * 1 ミリ秒は 1000 マイクロ秒に変換されます。
   * 1 分は 60 秒に変換されます。
   * 1 時間は 3600 秒に変換されます。
   * 1 週間は 7 日に変換されます。

   その後、日、秒、マイクロ秒は値が一意に表されるように、

   * ``0 <= microseconds < 1000000``
   * ``0 <= seconds < 3600*24`` (一日中の秒数)
   * ``-999999999 <= days <= 999999999``

   で正規化されます。

   引数のいずれかが浮動小数点であり、小数のマイクロ秒が存在する場合、
   小数のマイクロ秒は全ての引数から一度取り置かれ、それらの和は最も近
   いマイクロ秒に丸められます。浮動小数点の引数がない場合、値の変換と
   正規化の過程は厳密な (失われる情報がない) ものとなります。

   日の値を正規化した結果、指定された範囲の外側になった場合には、
   :exc:`OverflowError` が送出されます。

   負の値を正規化すると、一見混乱するような値になります。例えば、

      >>> from datetime import timedelta
      >>> d = timedelta(microseconds=-1)
      >>> (d.days, d.seconds, d.microseconds)
      (-1, 86399, 999999)


クラス属性を以下に示します:

.. attribute:: timedelta.min

   最小の値を表す :class:`timedelta` オブジェクトで、
   ``timedelta(-999999999)`` です。


.. attribute:: timedelta.max

   最大の値を表す :class:`timedelta` オブジェクトで、
   ``timedelta(days=999999999, hours=23, minutes=59, seconds=59,
   microseconds=999999)`` です。


.. attribute:: timedelta.resolution

   :class:`timedelta` オブジェクトが等しくならない最小の時間差で、
   ``timedelta(microseconds=1)`` です。

正規化のために、 ``timedelta.max`` > ``-timedelta.min`` となるので注意
してください。 ``-timedelta.max`` は :class:`timedelta` オブジェクトと
して表現することができません。

以下に (読み出し専用の) インスタンス属性を示します:

+------------------+---------------------------------------------+
| 属性             | 値                                          |
+==================+=============================================+
| ``days``         | 両端値を含む -999999999 から 999999999 の間 |
+------------------+---------------------------------------------+
| ``seconds``      | 両端値を含む 0 から 86399 の間              |
+------------------+---------------------------------------------+
| ``microseconds`` | 両端値を含む 0 から 999999 の間             |
+------------------+---------------------------------------------+

サポートされている操作を以下に示します:

.. XXX this table is too wide!

+--------------------------------+-----------------------------------------------------------------+
| 演算                           | 結果                                                            |
+================================+=================================================================+
| ``t1 = t2 + t3``               | *t2* と *t3* を加算します。演算後、  *t1*-*t2* == *t3*          |
|                                | および *t1*-*t3* == *t2* は真になります。 (1)                   |
+--------------------------------+-----------------------------------------------------------------+
| ``t1 = t2 - t3``               | *t2* と *t3* の差分です。演算後、  *t1* == *t2* - *t3*          |
|                                | および *t2* == *t1* + *t3* は真になります。 (1)                 |
+--------------------------------+-----------------------------------------------------------------+
| ``t1 = t2 * i or t1 = i * t2`` | 整数や長整数による乗算です。演算後、  *t1* // i == *t2* は      |
|                                | ``i != 0`` であれば真となります。                               |
+--------------------------------+-----------------------------------------------------------------+
|                                | 一般的に、 *t1* \* i == *t1* \* (i-1) + *t1*                    |
|                                | は真となります。(1)                                             |
+--------------------------------+-----------------------------------------------------------------+
| ``t1 = t2 // i``               | 端数を切り捨てて除算され、剰余 (がある場合) は捨てられます。(3) |
+--------------------------------+-----------------------------------------------------------------+
| ``+t1``                        | 同じ値を持つ :class:`timedelta` オブジェクトを返します。(2)     |
+--------------------------------+-----------------------------------------------------------------+
| ``-t1``                        | :class:`timedelta`\ (-*t1.days*,                                |
|                                | -*t1.seconds*, -*t1.microseconds*)、および *t1*\*               |
|                                | -1 と同じです。 (1)(4)                                          |
+--------------------------------+-----------------------------------------------------------------+
| ``abs(t)``                     | ``t.days >= 0`` のときには +*t*, ``t.days < 0`` の              |
|                                | ときには -*t* となります。(2)                                   |
+--------------------------------+-----------------------------------------------------------------+
| ``str(t)``                     | ``[D day[s], ][H]H:MM:SS[.UUUUUU]`` という形式の文字列          |
|                                | を返します。                                                    |
|                                | ``t`` が負の値の場合は ``D`` は負の値となります。(5)            |
+--------------------------------+-----------------------------------------------------------------+
| ``repr(t)``                    | ``datetime.timedelta(D[, S[, U]])`` という形式の文字列を        |
|                                | 返します。                                                      |
|                                | ``t`` が負の場合は D は負の値となります。(5)                    |
+--------------------------------+-----------------------------------------------------------------+


注釈:

(1)
   この操作は厳密ですが、オーバフローするかもしれません。

(2)
   この操作は厳密であり、オーバフローしないはずです。

(3)
   0 による除算は  :exc:`ZeroDivisionError` を送出します。

(4)
   -*timedelta.max* は :class:`timedelta` オブジェクトで表現することができません。

(5)
   :class:`timedelta` オブジェクトの文字列表現は内部表現に類似した形に正規化されます。
   そのため負の timedelta はいくぶん珍しい結果となります。例えば:

   >>> timedelta(hours=-5)
   datetime.timedelta(-1, 68400)
   >>> print(_)
   -1 day, 19:00:00

上に列挙した操作に加えて、 :class:`timedelta` オブジェクトは
:class:`date` および :class:`datetime` オブジェクトとの間で加減算をサ
ポートしています (下を参照してください)。

:class:`timedelta` オブジェクト間の比較はサポートされており、より小さ
い経過時間を表す :class:`timedelta` オブジェクトがより小さい timedelta
と見なされます。型混合の比較がデフォルトのオブジェクトアドレス比較となっ
てしまうのを抑止するために、 :class:`timedelta` オブジェクトと異なる型
のオブジェクトが比較されると、比較演算子が ``==`` または ``!=`` でない
かぎり :exc:`TypeError` が送出されます。後者の場合、それぞれ
:const:`False` または :const:`True` を返します。

:class:`timedelta` オブジェクトはハッシュ可能(:term:`hashable`) つまり、\
辞書のキーとして利用可能) であり、効率的な pickle 化をサポートします。
また、ブール演算コンテキストでは、 :class:`timedelta` オブジェクトは
``timedelta(0)`` に等しくない場合かつそのときに限り真となります。

インスタンスメソッド:

.. method:: timedelta.total_seconds()

   この期間に含まれるトータルの秒数を返します。
   true division が有効な場合の、 ``(td.microseconds + (td.seconds + 
   td.days * 24 * 3600) * 10**6) / 10**6`` と同じです。

   非常に長い期間 (多くのプラットフォームでは270年以上) については、
   このメソッドはマイクロ秒の精度を失うことがあることに注意してください。

   .. versionadded:: 2.7


使用例:

    >>> from datetime import timedelta
    >>> year = timedelta(days=365)
    >>> another_year = timedelta(weeks=40, days=84, hours=23,
    ...                          minutes=50, seconds=600)  # 365日になるように足し算
    >>> year.total_seconds()
    31536000.0
    >>> year == another_year
    True
    >>> ten_years = 10 * year
    >>> ten_years, ten_years.days // 365
    (datetime.timedelta(3650), 10)
    >>> nine_years = ten_years - year
    >>> nine_years, nine_years.days // 365
    (datetime.timedelta(3285), 9)
    >>> three_years = nine_years // 3;
    >>> three_years, three_years.days // 365
    (datetime.timedelta(1095), 3)
    >>> abs(three_years - ten_years) == 2 * three_years + year
    True


.. _datetime-date:

:class:`date` オブジェクト
--------------------------

:class:`date` オブジェクトは日付 (年、月、および日) を表します。日付は
理想的なカレンダー、すなわち現在のグレゴリオ暦を過去と未来の両方向に無
限に延長したもので表されます。 1 年の 1 月 1 日は日番号 1, 1 年 1 月
2 日は日番号 2,となっていきます。この暦法は、全ての計算における基本
カレンダーである、 Dershowitz と Reingold の書籍 Calendrical
Calculations における"予期的グレゴリオ (proleptic Gregorian)" 暦の定義
に一致します。


.. class:: date(year, month, day)

   全ての引数が必要です。引数は整数でも長整数でもよく、以下の範囲に入
   らなければなりません:

   * ``MINYEAR <= year <= MAXYEAR``
   * ``1 <= month <= 12``
   * ``1 <= day <= 指定された月と年における日数``

   範囲を超えた引数を与えた場合、 :exc:`ValueError` が送出されます。

他のコンストラクタ、および全てのクラスメソッドを以下に示します:

.. classmethod:: date.today()

   現在のローカルな日付を返します。
   ``date.fromtimestamp(time.time())`` と等価です。


.. classmethod:: date.fromtimestamp(timestamp)

   :func:`time.time` が返すような POSIX タイムスタンプに対応する、ロー
   カルな日付を返します。タイムスタンプがプラットフォームにおける C 関
   数 :c:func:`localtime` でサポートされている範囲を超えている場合には
   :exc:`ValueError` を送出します。
   この値はよく 1970 年から 2038 年に制限されていることがあります。う
   るう秒がタイムスタンプの概念に含まれている非 POSIX システムでは、
   :meth:`fromtimestamp` はうるう秒を無視します。


.. classmethod:: date.fromordinal(ordinal)

   予期的グレゴリオ暦順序に対応する日付を表し、 1 年 1 月 1 日が序数 1
   となります。 ``1 <= ordinal <= date.max.toordinal()`` でない場合、
   :exc:`ValueError` が送出されます。任意の日付 *d* に対し、
   ``date.fromordinal(d.toordinal()) ==  d`` となります。


以下にクラス属性を示します:

.. attribute:: date.min

   表現できる最も古い日付で、 ``date(MINYEAR, 1, 1)`` です。


.. attribute:: date.max

   表現できる最も新しい日付で、 ``date(MAXYEAR, 12, 31)`` です。


.. attribute:: date.resolution

   等しくない日付オブジェクト間の最小の差で、 ``timedelta(days=1)`` です。


以下に (読み出し専用の) インスタンス属性を示します:

.. attribute:: date.year

   両端値を含む :const:`MINYEAR` から :const:`MAXYEAR` までの値です。


.. attribute:: date.month

   両端値を含む 1 から 12 までの値です。


.. attribute:: date.day

   1 から与えられた月と年における日数までの値です。


サポートされている操作を以下に示します:

+-------------------------------+-------------------------------------------------------------+
| 演算                          | 結果                                                        |
+===============================+=============================================================+
| ``date2 = date1 + timedelta`` | *date2* はから *date1* から ``timedelta.days`` 日           |
|                               | 移動した日付です。 (1)                                      |
+-------------------------------+-------------------------------------------------------------+
| ``date2 = date1 - timedelta`` | ``date2 + timedelta == date1`` であるような日付             |
|                               | *date2* を計算します。 (2)                                  |
+-------------------------------+-------------------------------------------------------------+
| ``timedelta = date1 - date2`` | \(3)                                                        |
+-------------------------------+-------------------------------------------------------------+
| ``date1 < date2``             | *date1* が時刻として *date2* よりも前を表す場合に、 *date1* |
|                               | は *date2* よりも小さいと見なされます。 (4)                 |
+-------------------------------+-------------------------------------------------------------+

注釈:

(1)
   *date2* は ``timedelta.days > 0`` の場合進む方向に、
   ``timedelta.days < 0`` の場合戻る方向に移動します。
   演算後は、 ``date2 - date1 == timedelta.days`` となります。
   ``timedelta.seconds`` および ``timedelta.microseconds`` は無視され
   ます。 ``date2.year`` が :const:`MINYEAR` になってしまったり、
   :const:`MAXYEAR` より大きくなってしまう場合には
   :exc:`OverflowError` が送出されます。

(2)
   この操作は date1 + (-timedelta) と等価ではありません。なぜならば、
   date1 - timedelta がオーバフローしない場合でも、 -timedelta 単体が
   オーバフローする可能性があるからです。 ``timedelta.seconds`` および
   ``timedelta.microseconds`` は無視されます。

(3)
   この演算は厳密で、オーバフローしません。 timedelta.seconds および
   timedelta.microseconds は 0 で、演算後には date2 + timedelta ==
   date1 となります。

(4)
   別の言い方をすると、 ``date1.toordinal() < date2.toordinal()`` であ
   り、かつそのときに限り ``date1 < date2`` となります。型混合の比較が
   デフォルトのオブジェクトアドレス比較となってしまうのを抑止するため
   に、 :class:`timedelta` オブジェクトと異なる型のオブジェクトが比較
   されると :exc:`TypeError` が送出されます。しかしながら、被比較演算
   子のもう一方が :meth:`timetuple` 属性を持つ場合には
   ``NotImplemented`` が返されます。
   このフックにより、他種の日付オブジェクトに型混合比較を実装するチャ
   ンスを与えています。そうでない場合、 :class:`timedelta` オブジェク
   トと異なる型のオブジェクトが比較されると、比較演算子が ``==`` また
   は ``!=`` でないかぎり :exc:`TypeError` が送出されます。後者の場合、
   それぞれ :const:`False` または :const:`True` を返します。

:class:`date` オブジェクトは辞書のキーとして用いることができます。ブー
ル演算コンテキストでは、全ての :class:`date` オブジェクトは真であると
みなされます。

以下にインスタンスメソッドを示します:

.. method:: date.replace(year, month, day)

   キーワード引数で指定されたデータメンバが置き換えられることを除き、
   同じ値を持つ :class:`date` オブジェクトを返します。例えば、 ``d ==
   date(2002, 12, 31)`` とすると、 ``d.replace(day=26) == date(2002,
   12, 26)`` となります。


.. method:: date.timetuple()

   :func:`time.localtime` が返す形式の :class:`time.struct_time` を返
   します。時間、分、および秒は 0 で、DST フラグは -1 になります。
   ``d.timetuple()`` は次の値と同値です:
   ``time.struct_time((d.year, d.month, d.day, 0, 0, 0, d.weekday(), yday, -1))``
   ただし ``yday = d.toordinal() - date(d.year, 1, 1).toordinal() + 1``
   が 1月1日 に 1 で始まる現在の年の日を表す


.. method:: date.toordinal()

   予測的グレゴリオ暦における日付序数を返します。 1 年の 1 月 1 日が
   序数 1 となります。任意の :class:`date` オブジェクト *d* について、
   ``date.fromordinal(d.toordinal()) == d`` となります。


.. method:: date.weekday()

   月曜日を 0、日曜日を 6 として、曜日を整数で返します。例えば、
   ``date(2002, 12, 4).weekday() == 2`` であり、水曜日を示します。
   :meth:`isoweekday` も参照してください。


.. method:: date.isoweekday()

   月曜日を 1,日曜日を 7 として、曜日を整数で返します。例えば、
   ``date(2002, 12, 4).weekday() == 3`` であり、水曜日を示します。
   :meth:`weekday`, :meth:`isocalendar` も参照してください。


.. method:: date.isocalendar()

   3 要素のタプル (ISO 年、ISO 週番号、ISO 曜日) を返します。

   ISO カレンダーはグレゴリオ暦の変種として広く用いられています。細か
   い説明については
   http://www.phys.uu.nl/~vgent/calendar/isocalendar.htm を参照してくだ
   さい。

   ISO 年は完全な週が 52 または 53 週あり、週は月曜から始まって日曜に
   終わります。 ISO 年でのある年における最初の週は、その年の木曜日を含
   む最初の (グレゴリオ暦での) 週となります。この週は週番号 1 と呼ばれ、
   この木曜日での ISO 年はグレゴリオ暦における年と等しくなります。

   例えば、 2004 年は木曜日から始まるため、ISO 年の最初の週は 2003 年
   12 月 29 日、月曜日から始まり、 2004 年 1 月 4 日、日曜日に終わりま
   す。従って、 ``date(2003, 12, 29).isocalendar() == (2004, 1, 1)``
   であり、かつ ``date(2004, 1, 4).isocalendar() == (2004, 1, 7)`` と
   なります。


.. method:: date.isoformat()

   ISO 8601 形式、 'YYYY-MM-DD' の日付を表す文字列を返します。例えば、
   ``date(2002, 12, 4).isoformat() == '2002-12-04'`` となります。


.. method:: date.__str__()

   :class:`date` オブジェクト *d* において、 ``str(d)`` は
   ``d.isoformat()`` と等価です。


.. method:: date.ctime()

   日付を表す文字列を、例えば ``date(2002, 12, 4).ctime() == 'Wed Dec
   4 00:00:00 2002'`` のようにして返します。ネイティブの C 関数 :c:func:`ctime`
   (:func:`time.ctime` はこの関数を呼び出しますが、 :meth:`date.ctime`
   は呼び出しません) が C 標準に準拠しているプラットフォームでは、
   ``d.ctime()`` は ``time.ctime(time.mktime(d.timetuple()))`` と等価
   です。


.. method:: date.strftime(format)

   明示的な書式化文字列で制御された、日付を表現する文字列を返します。
   時間、分、秒を表す書式化コードは値 0 になります。
   :ref:`strftime-strptime-behavior` も参照下さい。

イベントまでの日数を数える例を示します::

    >>> import time
    >>> from datetime import date
    >>> today = date.today()
    >>> today
    datetime.date(2007, 12, 5)
    >>> today == date.fromtimestamp(time.time())
    True
    >>> my_birthday = date(today.year, 6, 24)
    >>> if my_birthday < today:
    ...     my_birthday = my_birthday.replace(year=today.year + 1)
    >>> my_birthday
    datetime.date(2008, 6, 24)
    >>> time_to_birthday = abs(my_birthday - today)
    >>> time_to_birthday.days
    202

:class:`date` と併用する例を示します:

.. doctest::

    >>> from datetime import date
    >>> d = date.fromordinal(730920) # 西暦1年1月1日を1日目として730920日目
    >>> d
    datetime.date(2002, 3, 11)
    >>> t = d.timetuple()
    >>> for i in t:     # doctest: +SKIP
    ...     print i
    2002                # 年
    3                   # 月
    11                  # 日
    0
    0
    0
    0                   # 曜日 (0 = 月曜日)
    70                  # 一年の中で70日目
    -1
    >>> ic = d.isocalendar()
    >>> for i in ic:    # doctest: +SKIP
    ...     print i
    2002                # ISO 年
    11                  # ISO 週番号
    1                   # ISO 曜日番号 ( 1 = 月曜日 )
    >>> d.isoformat()
    '2002-03-11'
    >>> d.strftime("%d/%m/%y")
    '11/03/02'
    >>> d.strftime("%A %d. %B %Y")
    'Monday 11. March 2002'


.. _datetime-datetime:

:class:`datetime` オブジェクト
------------------------------

:class:`datetime` オブジェクトは :class:`date` オブジェクトおよび
:class:`time` オブジェクトの全ての情報が入っている単一のオブジェクトで
す。 :class:`date` オブジェクトと同様に、 :class:`datetime` は現在のグ
レゴリオ暦が両方向に延長されているものと仮定します; また、
:class:`time` オブジェクトと同様に, :class:`datetime` は毎日が厳密に
3600\*24 秒であると仮定します。

以下にコンストラクタを示します:

.. class:: datetime(year, month, day[, hour[, minute[, second[, microsecond[, tzinfo]]]]])

   年、月、および日の引数は必須です。 *tzinfo* は ``None`` または
   :class:`tzinfo` クラスのサブクラスのインスタンスにすることができま
   す。残りの引数は整数または長整数で、以下のような範囲に入ります:

   * ``MINYEAR <= year <= MAXYEAR``
   * ``1 <= month <= 12``
   * ``1 <= day <= 与えられた年と月における日数``
   * ``0 <= hour < 24``
   * ``0 <= minute < 60``
   * ``0 <= second < 60``
   * ``0 <= microsecond < 1000000``

   引数がこれらの範囲外にある場合、 :exc:`ValueError` が送出されます。

その他のコンストラクタ、およびクラスメソッドを以下に示します:


.. classmethod:: datetime.today()

   現在のローカルな :class:`datetime` を :attr:`tzinfo` が ``None`` で
   あるものとして返します。これは
   ``datetime.fromtimestamp(time.time())`` と等価です。 :meth:`now`,
   :meth:`fromtimestamp` も参照してください。


.. classmethod:: datetime.now([tz])

   現在のローカルな日付および時刻を返します。オプションの引数 *tz* が
   ``None`` であるか指定されていない場合、このメソッドは
   :meth:`today` と同様ですが、可能ならば :func:`time.time` タイムスタ
   ンプを通じて得ることができる、より高い精度で時刻を提供します (例え
   ば、プラットフォームが C  関数 :c:func:`gettimeofday` をサポートする
   場合には可能なことがあります)。

   そうでない場合、 *tz* はクラス :class:`tzinfo` のサブクラスのインス
   タンスでなければならず、現在の日付および時刻は *tz* のタイムゾーン
   に変換されます。この場合、結果は
   ``tz.fromutc(datetime.utcnow().replace(tzinfo=tz))`` と等価になりま
   す。 :meth:`today`, :meth:`utcnow` も参照してください。


.. classmethod:: datetime.utcnow()

   現在の UTC における日付と時刻を、 :attr:`tzinfo` が ``None`` である
   ものとして返します。このメソッドは :meth:`now` に似ていますが、現在
   の UTC における日付と時刻を naive な :class:`datetime` オブジェクト
   として返します。 :meth:`now` も参照してください。


.. classmethod:: datetime.fromtimestamp(timestamp[, tz])

   :func:`time.time` が返すような、 POSIX タイムスタンプに対応するロー
   カルな日付と時刻を返します。オプションの引数 *tz* が ``None`` であ
   るか、指定されていない場合、タイムスタンプはプラットフォームのロー
   カルな日付および時刻に変換され、返される :class:`datetime` オブジェ
   クトは naive なものになります。

   そうでない場合、 *tz* はクラス :class:`tzinfo` のサブクラスのインス
   タンスでなければならず、現在の日付および時刻は *tz* のタイムゾーン
   に変換されます。この場合、結果は
   ``tz.fromutc(datetime.utcfromtimestamp(timestamp).replace(tzinfo=tz))``
   と等価になります。

   タイムスタンプがプラットフォームの C 関数 :c:func:`localtime` や
   :c:func:`gmtime` でサポートされている範囲を超えた場合、
   :meth:`fromtimestamp` は :exc:`ValueError` を送出することがあります。
   この範囲はよく 1970 年から 2038 年に制限されています。うるう秒がタ
   イムスタンプの概念に含まれている非 POSIX システムでは、
   :meth:`fromtimestamp` はうるう秒を無視します。このため、秒の異なる
   二つのタイムスタンプが同一の :class:`datetime` オブジェクトとなるこ
   とが起こり得ます。
   :meth:`utcfromtimestamp` も参照してください。


.. classmethod:: datetime.utcfromtimestamp(timestamp)

   :func:`time.time` が返すような POSIX タイムスタンプに対応する、 UTC
   での :class:`datetime` オブジェクトを返します。タイムスタンプがプラッ
   トフォームにおける C 関数 :c:func:`localtime` でサポートされている範
   囲を超えている場合には :exc:`ValueError` を送出します。この値はよ
   く 1970 年から 2038 年に制限されていることがあります。
   :meth:`fromtimestamp` も参照してください。


.. classmethod:: datetime.fromordinal(ordinal)

   1 年 1 月 1 日を序数 1 とする予測的グレゴリオ暦序数に対応する
   :class:`datetime` オブジェクトを返します。 ``1 <= ordinal <=
   datetime.max.toordinal()`` でないかぎり :exc:`ValueError` が送出さ
   れます。結果として返されるオブジェクトの時間、分、秒、およびマイク
   ロ秒はすべて 0 となり、 :attr:`tzinfo` は ``None`` となります。


.. classmethod:: datetime.combine(date, time)

   与えられた :class:`date` オブジェクトと同じデータメンバを持ち、時刻
   と :attr:`tzinfo` メンバが与えられた :class:`time` オブジェクトと等
   しい、新たな :class:`datetime` オブジェクトを返します。任意の
   :class:`datetime` オブジェクト *d* について、 ``d ==
   datetime.combine(d.date(), d.timetz())`` となります。 *date* が
   :class:`datetime` オブジェクトの場合、その時刻と :attr:`tzinfo` は
   無視されます。


.. classmethod:: datetime.strptime(date_string, format)

   *date_string* に対応した :class:`datetime` をかえします。 *format*
   にしたがって構文解析されます。これは、
   ``datetime(*(time.strptime(date_string, format)[0:6]))`` と等価です。
   date_stringとformatが :func:`time.strptime` で構文解析できない場合
   や、この関数が時刻タプルを返してこない場合には :exc:`ValueError` を
   送出します。セクション :ref:`strftime-strptime-behavior` を
   参照して下さい。

   .. versionadded:: 2.5


以下にクラス属性を示します:

.. attribute:: datetime.min

   表現できる最も古い :class:`datetime` で、 ``datetime(MINYEAR, 1, 1,
   tzinfo=None)`` です。


.. attribute:: datetime.max

   表現できる最も新しい :class:`datetime` で、 ``datetime(MAXYEAR, 12,
   31, 23, 59, 59, 999999, tzinfo=None)`` です。


.. attribute:: datetime.resolution

   等しくない :class:`datetime` オブジェクト間の最小の差で、
   ``timedelta(microseconds=1)`` です。


以下に (読み出し専用の) インスタンス属性を示します:

.. attribute:: datetime.year

   両端値を含む :const:`MINYEAR` から :const:`MAXYEAR` までの値です。


.. attribute:: datetime.month

   両端値を含む 1 から 12 までの値です。


.. attribute:: datetime.day

   1 から与えられた月と年における日数までの値です。


.. attribute:: datetime.hour

   ``range(24)`` 内の値です。


.. attribute:: datetime.minute

   ``range(60)`` 内の値です。


.. attribute:: datetime.second

   ``range(60)`` 内の値です。


.. attribute:: datetime.microsecond

   ``range(1000000)`` 内の値です。


.. attribute:: datetime.tzinfo

   :class:`datetime` コンストラクタに *tzinfo* 引数として与えられたオ
   ブジェクトになり、何も渡されなかった場合には ``None`` になります。


以下にサポートされている演算を示します:

+---------------------------------------+-------------------------------------------------------+
| 演算                                  | 結果                                                  |
+=======================================+=======================================================+
| ``datetime2 = datetime1 + timedelta`` | \(1)                                                  |
+---------------------------------------+-------------------------------------------------------+
| ``datetime2 = datetime1 - timedelta`` | \(2)                                                  |
+---------------------------------------+-------------------------------------------------------+
| ``timedelta = datetime1 - datetime2`` | \(3)                                                  |
+---------------------------------------+-------------------------------------------------------+
| ``datetime1 < datetime2``             | :class:`datetime` を :class:`datetime` と比較します。 |
|                                       | (4)                                                   |
+---------------------------------------+-------------------------------------------------------+

(1)
   datetime2 は datetime1 から時間 timedelta 移動したもので、
   ``timedelta.days > 0`` の場合進む方向に、 ``timedelta.days < 0`` の
   場合戻る方向に移動します。結果は入力の datetime と同じ
   :attr:`tzinfo` を持ち、演算後には datetime2 - datetime1 ==
   timedelta となります。 datetime2.year が :const:`MINYEAR` よりも小
   さいか、 :const:`MAXYEAR` より大きい場合には :exc:`OverflowError`
   が送出されます。入力が aware なオブジェクトの場合でもタイムゾーン修
   正は全く行われません。

(2)
   datetime2 + timedelta == datetime1 となるような datetime2 を計算し
   ます。ちなみに、結果は入力の datetime と同じ :attr:`tzinfo` メンバ
   を持ち、入力が aware でもタイムゾーン修正は全く行われません。この
   操作は date1 + (-timedelta) と等価ではありません。なぜならば、
   date1 - timedeltaがオーバフローしない場合でも、-timedelta 単体がオー
   バフローする可能性があるからです。

(3)
   :class:`datetime` から :class:`datetime` の減算は両方の被演算子が
   naive であるか、両方とも aware である場合にのみ定義されています。片
   方が aware でもう一方が naive の場合、 :exc:`TypeError` が送出され
   ます。

   両方とも naive か、両方とも aware で同じ :attr:`tzinfo` メンバを持
   つ場合、 :attr:`tzinfo` メンバは無視され、結果は ``datetime2 + t ==
   datetime1`` であるような :class:`timedelta` オブジェクト *t* となり
   ます。
   この場合タイムゾーン修正は全く行われません。

   両方が aware で異なる :attr:`tzinfo` メンバを持つ場合、 ``a-b`` は
   *a* および *b* をまず naive な UTC datetime オブジェクトに変換した
   かのようにして行います。演算結果は決してオーバフローを起こさないこ
   とを除き、
   ``(a.replace(tzinfo=None) - a.utcoffset()) - (b.replace(tzinfo=None) -
   b.utcoffset())`` と同じになります。

(4)
   *datetime1* が時刻として *datetime2* よりも前を表す場合に、
   *datetime1* は *datetime2* よりも小さいと見なされます。

   被演算子の片方が naive でもう一方が aware の場合、 :exc:`TypeError`
   が送出されます。両方の被演算子が aware で、同じ :attr:`tzinfo` メン
   バを持つ場合、共通の :attr:`tzinfo` メンバは無視され、基本の
   datetime 間の比較が行われます。
   両方の被演算子が aware で異なる :attr:`tzinfo` メンバを持つ場合、被
   演算子はまず (``self.utcoffset()`` で得られる) UTC オフセットで修正
   されます。

   .. note::

      型混合の比較がデフォルトのオブジェクトアドレス比較となってしまう
      のを抑止するために、被演算子のもう一方が :class:`datatime` オブ
      ジェクトと異なる型のオブジェクトの場合には :exc:`TypeError` が送
      出されます。しかしながら、被比較演算子のもう一方が
      :meth:`timetuple` 属性を持つ場合には ``NotImplemented`` が返され
      ます。このフックにより、他種の日付オブジェクトに型混合比較を実装
      するチャンスを与えています。そうでない場合, :class:`datetime` オ
      ブジェクトと異なる型のオブジェクトが比較されると、比較演算子が
      ``==`` または ``!=`` でないかぎり :exc:`TypeError` が送出されま
      す。後者の場合、それぞれ :const:`False` または :const:`True` を
      返します。


:class:`datetime` オブジェクトは辞書のキーとして用いることができます。
ブール演算コンテキストでは、全ての :class:`datetime` オブジェクトは真
であるとみなされます。

インスタンスメソッドを以下に示します:

.. method:: datetime.date()

   同じ年、月、日の :class:`date` オブジェクトを返します。


.. method:: datetime.time()

   同じ時、分、秒、マイクロ秒を持つ :class:`time` オブジェクトを返しま
   す。 :attr:`tzinfo` は ``None`` です。 :meth:`timetz` も参照してく
   ださい。


.. method:: datetime.timetz()

   同じ時、分、秒、マイクロ秒、および tzinfo メンバを持つ
   :class:`time` オブジェクトを返します。 :meth:`time` メソッドも参照
   してください。


.. method:: datetime.replace([year[, month[, day[, hour[, minute[, second[, microsecond[, tzinfo]]]]]]]])

   キーワード引数で指定したメンバの値を除き、同じ値をもつ datetime  オ
   ブジェクトを返します。メンバに対する変換を行わずに aware な
   datetime オブジェクトから naive な datetime オブジェクトを生成する
   ために、 ``tzinfo=None`` を指定することもできます。


.. method:: datetime.astimezone(tz)

   :class:`datetime` オブジェクトを返します。返されるオブジェクトは新
   たな :attr:`tzinfo` メンバ *tz* を持ちます。 *tz* は日付および時刻
   を調整して、オブジェクトが *self* と同じ UTC 時刻を持つが、 *tz* に
   おけるローカルな時刻を表すようにします。

   *tz* は :class:`tzinfo` のサブクラスのインスタンスでなければならず、
   インスタンスの :meth:`utcoffset` および :meth:`dst` メソッドは
   ``None`` を返してはなりません。 *self* は aware でなくてはなりませ
   ん (``self.tzinfo`` が ``None`` であってはならず、かつ
   ``self.utcoffset()`` は ``None`` を返してはなりません)。

   ``self.tzinfo`` が *tz* の場合、 ``self.astimezone(tz)`` は *self*
   に等しくなります:
   日付および時刻データメンバに対する調整は行われません。そうでない場
   合、結果はタイムゾーン *tz* におけるローカル時刻で、 *self* と同じ
   UTC 時刻を表すようになります: ``astz = dt.astimezone(tz)`` とした後、
   ``astz - astz.utcoffset()`` は通常 ``dt - dt.utcoffset()`` と同じ日
   付および時刻データメンバを持ちます。 :class:`tzinfo` クラスに関する
   議論では、夏時間 (Daylight Saving time) の遷移境界では上の等価性が
   成り立たないことを説明しています (*tz* が標準時と夏時間の両方をモデ
   ル化している場合のみの問題です)。

   単にタイムゾーンオブジェクト *tz* を :class:`datetime` オブジェクト
   *dt* に追加したいだけで、日付や時刻データメンバへの調整を行わないの
   なら、 ``dt.replace(tzinfo=tz)`` を使ってください。単に aware な :class:`datetime`
   オブジェクト *dt* からタイムゾーンオブジェクトを除去したいだけで、
   日付や時刻データメンバの変換を行わないのなら、
   ``dt.replace(tzinfo=None)`` を使ってください。

   デフォルトの :meth:`tzinfo.fromutc` メソッドを :class:`tzinfo`
   のサブクラスで上書きして, :meth:`astimezone` が返す結果に影響を及
   ぼすことができます。エラーの場合を無視すると、
   :meth:`astimezone` は以下のように動作します::

      def astimezone(self, tz):
          if self.tzinfo is tz:
              return self
          # 自身を UTC に変換し、新しいタイムゾーンオブジェクトをアタッチします
          utc = (self - self.utcoffset()).replace(tzinfo=tz)
          # UTC から tz のローカルタイムに変換します
          return tz.fromutc(utc)


.. method:: datetime.utcoffset()

   :attr:`tzinfo` が ``None`` の場合、 ``None`` を返し、そうでない場合には
   ``self.tzinfo.utcoffset(self)`` を返します。後者の式が ``None`` か、1 日以下の大きさを持つ経過時間を表す
   :class:`timedelta` オブジェクトのいずれかを返さない場合には例外を送出します。


.. method:: datetime.dst()

   :attr:`tzinfo` が ``None`` の場合、 ``None`` を返し、そうでない場合には ``self.tzinfo.dst(self)``
   を返します。後者の式が ``None`` か、1 日以下の大きさを持つ経過時間を表す :class:`timedelta` オブジェクトのいずれかを返さない
   場合には例外を送出します。


.. method:: datetime.tzname()

   :attr:`tzinfo` が ``None`` の場合、 ``None`` を返し、そうでない場合には
   ``self.tzinfo.tzname(self)`` を返します。後者の式が ``None`` か文字列オブジェクトのいずれか
   を返さない場合には例外を送出します。


.. method:: datetime.timetuple()

   :func:`time.localtime` が返す形式の :class:`time.struct_time` を返します。
   ``d.timetuple()`` は次の値と等価です:
   ``time.struct_time((d.year, d.month, d.day,
   d.hour, d.minute, d.second, d.weekday(), yday, dst))``, ただし ``yday =
   d.toordinal() - date(d.year, 1, 1).toordinal() + 1`` は 1月1日が ``1``
   で始まるその年の中の日の数.

   返されるタプルの :attr:`tm_isdst` フラグは :meth:`dst` メソッドに従って設定されます:  :attr:`tzinfo` が
   ``None`` か :meth:`dst` が ``None`` を返す場合、 :attr:`tm_isdst` は ``-1`` に設定されます;
   そうでない場合、 :meth:`dst` がゼロでない値を返すと, :attr:`tm_isdst` は ``1`` となります; それ以外の場合には
   ``tm_isdst`` は ``0`` に設定されます。


.. method:: datetime.utctimetuple()

   :class:`datetime` インスタンス *d* が naive の場合、このメソッドは ``d.timetuple()``
   と同じであり、 ``d.dst()`` の返す内容にかかわらず :attr:`tm_isdst` が 0 に強制される点だけが異なります。 DST が UTC
   時刻に影響を及ぼすことは決してありません。

   *d* が aware の場合、 *d* から ``d.utcoffset()`` が差し引かれて UTC 時刻に正規化され、正規化された時刻の
   :class:`time.struct_time` を返します。 :attr:`tm_isdst` は 0 に強制されます。 *d*.year が
   ``MINYEAR`` や ``MAXUEAR`` で、UTC への修正の結果表現可能な年の境界を越えた場合には、戻り値の :attr:`tm_year`
   メンバは :const:`MINYEAR`\ -1 または :const:`MAXYEAR`\ +1 になることがあります。


.. method:: datetime.toordinal()

   予測的グレゴリオ暦における日付序数を返します。 ``self.date().toordinal()`` と同じです。


.. method:: datetime.weekday()

   月曜日を 0、日曜日を 6 として、曜日を整数で返します。 ``self.date().weekday()`` と同じです。
   :meth:`isoweekday` も参照してください。


.. method:: datetime.isoweekday()

   月曜日を 1、日曜日を 7 として、曜日を整数で返します。 ``self.date().isoweekday()`` と等価です。
   :meth:`weekday` 、 :meth:`isocalendar` も参照してください。


.. method:: datetime.isocalendar()

   3 要素のタプル (ISO 年、ISO 週番号、ISO 曜日) を返します。 ``self.date().isocalendar()`` と等価です。


.. method:: datetime.isoformat([sep])

   日付と時刻を ISO 8601 形式、すなわち YYYY-MM-DDTHH:MM:SS.mmmmmm か、 :attr:`microsecond` が 0
   の場合には YYYY-MM-DDTHH:MM:SS で表した文字列を返します。 :meth:`utcoffset` が ``None`` を返さない場合、
   UTC からのオフセットを時間と分を表した (符号付きの) 6 文字からなる  文字列が追加されます: すなわち、 YYYY-MM-
   DDTHH:MM:SS.mmmmmm+HH:MM となるか、 :attr:`microsecond` がゼロの場合には YYYY-MM-
   DDTHH:MM:SS+HH:MM となります。オプションの引数 *sep* (デフォルトでは ``'T'`` です)  は 1
   文字のセパレータで、結果の文字列の日付と時刻の間に置かれます。例えば、 ::

      >>> from datetime import tzinfo, timedelta, datetime
      >>> class TZ(tzinfo):
      ...     def utcoffset(self, dt): return timedelta(minutes=-399)
      ...
      >>> datetime(2002, 12, 25, tzinfo=TZ()).isoformat(' ')
      '2002-12-25 00:00:00-06:39'

   となります。


.. method:: datetime.__str__()

   :class:`datetime` オブジェクト *d* において、 ``str(d)`` は ``d.isoformat(' ')`` と等価です。


.. method:: datetime.ctime()

   日付を表す文字列を、例えば ``datetime(2002, 12, 4, 20, 30, 40).ctime() == 'Wed Dec  4
   20:30:40 2002'`` のようにして返します。ネイティブの C 関数 :c:func:`ctime`  (:func:`time.ctime`
   はこの関数を呼び出しますが、 :meth:`datetime.ctime` は呼び出しません) が C 標準に準拠しているプラットフォームでは、
   ``d.ctime()`` は ``time.ctime(time.mktime(d.timetuple()))`` と等価です。


.. method:: datetime.strftime(format)

   明示的な書式化文字列で制御された、日付を表現する文字列を返します。 :meth:`strftime` のふるまいについてのセクション
   :ref:`strftime-strptime-behavior` を参照してください。


datetime オブジェクトを使う例:

.. doctest::

    >>> from datetime import datetime, date, time
    >>> # Using datetime.combine()
    >>> d = date(2005, 7, 14)
    >>> t = time(12, 30)
    >>> datetime.combine(d, t)
    datetime.datetime(2005, 7, 14, 12, 30)
    >>> # Using datetime.now() or datetime.utcnow()
    >>> datetime.now()   # doctest: +SKIP
    datetime.datetime(2007, 12, 6, 16, 29, 43, 79043)   # GMT +1
    >>> datetime.utcnow()   # doctest: +SKIP
    datetime.datetime(2007, 12, 6, 15, 29, 43, 79060)
    >>> # Using datetime.strptime()
    >>> dt = datetime.strptime("21/11/06 16:30", "%d/%m/%y %H:%M")
    >>> dt
    datetime.datetime(2006, 11, 21, 16, 30)
    >>> # Using datetime.timetuple() to get tuple of all attributes
    >>> tt = dt.timetuple()
    >>> for it in tt:   # doctest: +SKIP
    ...     print it
    ...
    2006    # year
    11      # month
    21      # day
    16      # hour
    30      # minute
    0       # second
    1       # weekday (0 = Monday)
    325     # number of days since 1st January
    -1      # dst - method tzinfo.dst() returned None
    >>> # Date in ISO format
    >>> ic = dt.isocalendar()
    >>> for it in ic:   # doctest: +SKIP
    ...     print it
    ...
    2006    # ISO year
    47      # ISO week
    2       # ISO weekday
    >>> # Formatting datetime
    >>> dt.strftime("%A, %d. %B %Y %I:%M%p")
    'Tuesday, 21. November 2006 04:30PM'

datetime を tzinfo と組み合わせて使う:

    >>> from datetime import timedelta, datetime, tzinfo
    >>> class GMT1(tzinfo):
    ...     def __init__(self):         # DST starts last Sunday in March
    ...         d = datetime(dt.year, 4, 1)   # ends last Sunday in October
    ...         self.dston = d - timedelta(days=d.weekday() + 1)
    ...         d = datetime(dt.year, 11, 1)
    ...         self.dstoff = d - timedelta(days=d.weekday() + 1)
    ...     def utcoffset(self, dt):
    ...         return timedelta(hours=1) + self.dst(dt)
    ...     def dst(self, dt):
    ...         if self.dston <=  dt.replace(tzinfo=None) < self.dstoff:
    ...             return timedelta(hours=1)
    ...         else:
    ...             return timedelta(0)
    ...     def tzname(self,dt):
    ...          return "GMT +1"
    ...
    >>> class GMT2(tzinfo):
    ...     def __init__(self):
    ...         d = datetime(dt.year, 4, 1)
    ...         self.dston = d - timedelta(days=d.weekday() + 1)
    ...         d = datetime(dt.year, 11, 1)
    ...         self.dstoff = d - timedelta(days=d.weekday() + 1)
    ...     def utcoffset(self, dt):
    ...         return timedelta(hours=1) + self.dst(dt)
    ...     def dst(self, dt):
    ...         if self.dston <=  dt.replace(tzinfo=None) < self.dstoff:
    ...             return timedelta(hours=2)
    ...         else:
    ...             return timedelta(0)
    ...     def tzname(self,dt):
    ...         return "GMT +2"
    ...
    >>> gmt1 = GMT1()
    >>> # Daylight Saving Time
    >>> dt1 = datetime(2006, 11, 21, 16, 30, tzinfo=gmt1)
    >>> dt1.dst()
    datetime.timedelta(0)
    >>> dt1.utcoffset()
    datetime.timedelta(0, 3600)
    >>> dt2 = datetime(2006, 6, 14, 13, 0, tzinfo=gmt1)
    >>> dt2.dst()
    datetime.timedelta(0, 3600)
    >>> dt2.utcoffset()
    datetime.timedelta(0, 7200)
    >>> # Convert datetime to another time zone
    >>> dt3 = dt2.astimezone(GMT2())
    >>> dt3     # doctest: +ELLIPSIS
    datetime.datetime(2006, 6, 14, 14, 0, tzinfo=<GMT2 object at 0x...>)
    >>> dt2     # doctest: +ELLIPSIS
    datetime.datetime(2006, 6, 14, 13, 0, tzinfo=<GMT1 object at 0x...>)
    >>> dt2.utctimetuple() == dt3.utctimetuple()
    True



.. _datetime-time:

:class:`time` オブジェクト
--------------------------

:class:`time` オブジェクトは (ローカルの) 日中時刻を表現します。この時刻表現は特定の日の影響を受けず, :class:`tzinfo`
オブジェクトを介した修正の対象となります。

.. class:: time(hour[, minute[, second[, microsecond[, tzinfo]]]])

   全ての引数はオプションです。 *tzinfo* は ``None`` または :class:`tzinfo` クラスのサブクラスのインスタンス
   にすることができます。残りの引数は整数または長整数で、以下のような範囲に入ります:

   * ``0 <= hour < 24``
   * ``0 <= minute < 60``
   * ``0 <= second < 60``
   * ``0 <= microsecond < 1000000``.

   引数がこれらの範囲外にある場合、 :exc:`ValueError` が送出されます。 *tzinfo* のデフォルト値が
   :const:`None` である以外のデフォルト値は *0* です。

以下にクラス属性を示します:


.. attribute:: time.min

   表現できる最も古い :class:`datetime` で、 ``time(0, 0, 0, 0)`` です。 The earliest
   representable :class:`time`, ``time(0, 0, 0, 0)``.


.. attribute:: time.max

   表現できる最も新しい :class:`datetime` で、 ``time(23, 59, 59, 999999, tzinfo=None)`` です。


.. attribute:: time.resolution

   等しくない :class:`datetime` オブジェクト間の最小の差で、  ``timedelta(microseconds=1)``
   ですが, :class:`time` オブジェクト間の四則演算はサポートされていないので注意してください。


以下に (読み出し専用の) インスタンス属性を示します:

.. attribute:: time.hour

   ``range(24)`` 内の値です。


.. attribute:: time.minute

   ``range(60)`` 内の値です。


.. attribute:: time.second

   ``range(60)`` 内の値です。


.. attribute:: time.microsecond

   ``range(1000000)`` 内の値です。


.. attribute:: time.tzinfo

   :class:`time` コンストラクタに *tzinfo* 引数として与えられたオブジェクトになり、何も渡されなかった場合には ``None``
   になります。


以下にサポートされている操作を示します:

* :class:`time` と :class:`time` の比較では、 *a* が時刻として *b* よりも前を表す場合に *a* は *b*
  よりも小さいと見なされます。被演算子の片方が naive でもう一方が aware の場合、 :exc:`TypeError`
  が送出されます。両方の被演算子が aware で、同じ :attr:`tzinfo` メンバを持つ場合、共通の :attr:`tzinfo`
  メンバは無視され、基本の datetime 間の比較が行われます。両方の被演算子が aware で異なる :attr:`tzinfo` メンバを持つ
  場合、被演算子はまず (``self.utcoffset()`` で得られる) UTC  オフセットで修正されます。
  型混合の比較がデフォルトのオブジェクトアドレス比較となってしまうのを抑止するために, :class:`time` オブジェクトが他の型のオブジェクトと
  比較された場合、比較演算子が ``==`` または ``!=`` でないかぎり :exc:`TypeError` が送出されます。後者の場合、それぞれ
  :const:`False` または :const:`True` を返します。

* ハッシュ化、辞書のキーとしての利用

* 効率的な pickle 化

* ブール演算コンテキストでは, :class:`time` オブジェクトは、分に変換し, :meth:`utfoffset` (``None``
  を返した場合には ``0``) を差し引いて変換した後の結果がゼロでない場合、かつそのときに限って真とみなされます。

以下にインスタンスメソッドを示します:

.. method:: time.replace([hour[, minute[, second[, microsecond[, tzinfo]]]]])

   キーワード引数で指定したメンバの値を除き、同じ値をもつ :class:`time` オブジェクトを返します。メンバに対する変換を行わずに aware な
   datetime オブジェクトから  naive な :class:`time` オブジェクトを生成するために、 ``tzinfo=None``
   を指定することもできます。


.. method:: time.isoformat()

   日付と時刻を ISO 8601 形式、すなわち HH:MM:SS.mmmmmm か、 :attr:`microsecond` が 0 の場合には
   HH:MM:SS で表した文字列を返します。 :meth:`utcoffset` が ``None`` を返さない場合、 UTC
   からのオフセットを時間と分を表した (符号付きの) 6 文字からなる  文字列が追加されます: すなわち、 HH:MM:SS.mmmmmm+HH:MM
   となるか、 :attr:`microsecond` が 0 の場合には HH:MM:SS+HH:MM となります。


.. method:: time.__str__()

   :class:`time` オブジェクト *t* において、 ``str(t)`` は ``t.isoformat()`` と等価です。


.. method:: time.strftime(format)

   明示的な書式化文字列で制御された、日付を表現する文字列を返します。 :meth:`strftime` のふるまいについてのセクション
   :ref:`strftime-strptime-behavior` を参照してください。


.. method:: time.utcoffset()

   :attr:`tzinfo` が ``None`` の場合、 ``None`` を返し、そうでない場合
   には ``self.tzinfo.utcoffset(None)`` を返します。後者の式が
   ``None`` か、1 日以下の大きさを持つ経過時間を表す
   :class:`timedelta` オブジェクトのいずれかを返さない場合には例外を送
   出します。


.. method:: time.dst()

   :attr:`tzinfo` が ``None`` の場合、 ``None`` を返し、そうでない場合
   には ``self.tzinfo.dst(None)`` を返します。後者の式が ``None`` か、
   1日以下の大きさを持つ経過時間を表す :class:`timedelta` オブジェクト
   のいずれかを返さない場合には例外を送出します。


.. method:: time.tzname()

   :attr:`tzinfo` が ``None`` の場合、 ``None`` を返し、そうでない場合
   には ``self.tzinfo.tzname(None)`` を返します。後者の式が ``None``
   か文字列オブジェクトのいずれかを返さない場合には例外を送出します。

使用例:

    >>> from datetime import time, tzinfo
    >>> class GMT1(tzinfo):
    ...     def utcoffset(self, dt):
    ...         return timedelta(hours=1)
    ...     def dst(self, dt):
    ...         return timedelta(0)
    ...     def tzname(self,dt):
    ...         return "Europe/Prague"
    ...
    >>> t = time(12, 10, 30, tzinfo=GMT1())
    >>> t                               # doctest: +ELLIPSIS
    datetime.time(12, 10, 30, tzinfo=<GMT1 object at 0x...>)
    >>> gmt = GMT1()
    >>> t.isoformat()
    '12:10:30+01:00'
    >>> t.dst()
    datetime.timedelta(0)
    >>> t.tzname()
    'Europe/Prague'
    >>> t.strftime("%H:%M:%S %Z")
    '12:10:30 Europe/Prague'


.. _datetime-tzinfo:

:class:`tzinfo` オブジェクト
----------------------------

:class:`tzinfo` は抽象基底クラスです。つまり、このクラスは直接インスタ\
ンス化して利用しません。具体的なサブクラスを派生し、 (少なくとも) 利用\
したい :class:`datetime` のメソッドが必要とする :class:`tzinfo` の標準\
メソッドを実装してやる必要があります。
:mod:`datetime` モジュールでは、 :class:`tzinfo` の具体的なサブクラス\
は何ら提供していません。

:class:`tzinfo` (の具体的なサブクラス) のインスタンスは
:class:`datetime` および :class:`time` オブジェクトのコンストラクタに
渡すことができます。後者のオブジェクトでは、データメンバをローカル時
刻におけるものとして見ており、 :class:`tzinfo` オブジェクトはローカル
時刻の UTC からのオフセット、タイムゾーンの名前、 DST オフセットを、
渡された日付および時刻オブジェクトからの相対で示すためのメソッドを提供
します。

pickle 化についての特殊な要求事項: :class:`tzinfo` のサブクラスは引数
なしで呼び出すことのできる :meth:`__init__` メソッドを持たねばなりませ
ん。そうでなければ、 pickle 化することはできますがおそらく unpickle 化
することはできないでしょう。これは技術的な側面からの要求であり、将来緩
和されるかもしれません。

:class:`tzinfo` の具体的なサブクラスでは、以下のメソッドを実装する必要
があります。厳密にどのメソッドが必要なのかは、 aware な
:mod:`datetime` オブジェクトがこのサブクラスのインスタンスをどのように
使うかに依存します。不確かならば、単に全てを実装してください。


.. method:: tzinfo.utcoffset(self, dt)

   ローカル時間の UTC からのオフセットを、 UTC から東向きを正とした分
   で返します。ローカル時間が UTC の西側にある場合、この値は負になりま
   す。
   このメソッドは UTC からのオフセットの総計を返すように意図されている
   ので注意してください; 例えば、 :class:`tzinfo` オブジェクトがタイム
   ゾーンと DST 修正の両方を表現する場合、 :meth:`utcoffset` はそれら
   の合計を返さなければなりません。 UTC オフセットが未知である場合、
   ``None`` を返してください。そうでない場合には、返される値は -1439
   から 1439 の両端を含む値 (1440 = 24\*60 ; つまり、オフセットの大き
   さは 1 日より短くなくてはなりません) が分で指定された
   :class:`timedelta` オブジェクトでなければなりません。
   ほとんどの :meth:`utcoffset` 実装は、おそらく以下の二つのうちの一つ
   に似たものになるでしょう::

      return CONSTANT                 # fixed-offset class
      return CONSTANT + self.dst(dt)  # daylight-aware class

   :meth:`utcoffset` が ``None`` を返さない場合、 :meth:`dst` も
   ``None`` を返してはなりません。

   :meth:`utcoffset` のデフォルトの実装は :exc:`NotImplementedError`
   を送出します。


.. method:: tzinfo.dst(self, dt)

   夏時間 (DST) 修正を、 UTC から東向きを正とした分で返します。 DST 情
   報が未知の場合、 ``None`` が返されます。 DST が有効でない場合には
   ``timedelta(0)`` を返します。 DST が有効の場合、オフセットは
   :class:`timedelta` オブジェクトで返します (詳細は :meth:`utcoffset`
   を参照してください)。 DST オフセットが利用可能な場合、この値は
   :meth:`utcoffset` が返す UTC からのオフセットには既に加算されている
   ため、 DST を個別に取得する必要がない限り :meth:`dst` を使って問い
   合わせる必要はないので注意してください。例えば、
   :meth:`datetime.timetuple` は :attr:`tzinfo` メンバの :meth:`dst`
   メソッドを呼んで :attr:`tm_isdst` フラグがセットされているかどうか
   判断し、 :meth:`tzinfo.fromutc` は :meth:`dst` タイムゾーンを移動す
   る際に DST による変更があるかどうかを調べます。

   標準および夏時間の両方をモデル化している :class:`tzinfo` サブクラス
   のインスタンス *tz* は以下の式:

   ``tz.utcoffset(dt) - tz.dst(dt)``

   が、 ``dt.tzinfo == tz`` 全ての :class:`datetime` オブジェクト *dt*
   について常に同じ結果を返さなければならないという点で、一貫性を持っ
   ていなければなりません。正常に実装された :class:`tzinfo` のサブクラ
   スでは、この式はタイムゾーンにおける "標準オフセット (standard
   offset)" を表し、特定の日や時刻の事情ではなく地理的な位置にのみ依存
   していなくてはなりません。 :meth:`datetime.astimezone` の実装はこの
   事実に依存していますが、違反を検出することができません; 正しく実装
   するのはプログラマの責任です。 :class:`tzinfo` のサブクラスでこれを
   保証することができない場合、 :meth:`tzinfo.fromutc` の実装をオーバ
   ライドして、 :meth:`astimezone` に関わらず正しく動作するようにして
   もかまいません。

   ほとんどの :meth:`dst` 実装は、おそらく以下の二つのうちの一つに似た
   ものになるでしょう::

      def dst(self):
          # a fixed-offset class:  doesn't account for DST
          return timedelta(0)

   or ::

      def dst(self):
          # Code to set dston and dstoff to the time zone's DST
          # transition times based on the input dt.year, and expressed
          # in standard local time.  Then

          if dston <= dt.replace(tzinfo=None) < dstoff:
              return timedelta(hours=1)
          else:
              return timedelta(0)

   デフォルトの :meth:`dst` 実装は :exc:`NotImplementedError` を送出し
   ます。


.. method:: tzinfo.tzname(self, dt)

   :class:`datetime` オブジェクト *dt* に対応するタイムゾーン名を文字
   列で返します。 :mod:`datetime` モジュールでは文字列名について何も定
   義しておらず、特に何かを意味するといった要求仕様もまったくありませ
   ん。例えば、 "GMT","UTC", "-500", "-5:00", "EDT",
   "US/Eastern", "America/New York" は全て有効な応答となります。文字
   列名が未知の場合には ``None`` を返してください。 :class:`tzinfo` の
   サブクラスでは、特に, :class:`tzinfo` クラスが夏時間について記述し
   ている場合のように、渡された *dt* の特定の値によって異なった名前を
   返したい場合があるため、文字列値ではなくメソッドとなっていることに
   注意してください。

   デフォルトの :meth:`tzname` 実装は :exc:`NotImplementedError` を送
   出します。

以下のメソッドは :class:`datetime` や :class:`time` オブジェクトにおい
て、同名のメソッドが呼び出された際に応じて呼び出されます。
:class:`datetime` オブジェクトは自身を引数としてメソッドに渡し、
:class:`time` オブジェクトは引数として ``None`` をメソッドに渡します。
従って、 :class:`tzinfo` のサブクラスにおけるメソッドは引数 *dt* が
``None`` の場合と、 :class:`datetime` の場合を受理するように用意しなけ
ればなりません。

``None`` が渡された場合、最良の応答方法を決めるのはクラス設計者次第で
す。例えば、このクラスが :class:`tzinfo` プロトコルと関係をもたないと
いうことを表明させたければ、 ``None`` が適切です。標準時のオフセットを
見つける他の手段がない場合には、標準 UTC オフセットを返すために
``utcoffset(None)`` を使うともっと便利かもしれません。

:class:`datetime` オブジェクトが :meth:`datetime` メソッドの応答として
返された場合、 ``dt.tzinfo`` は *self* と同じオブジェクトになります。
ユーザが直接 :class:`tzinfo` メソッドを呼び出さないかぎり、
:class:`tzinfo` メソッドは ``dt.tzinfo`` と *self* が同じであることに
依存します。その結果 :class:`tzinfo` メソッドは *dt* がローカル時間で
あると解釈するので、他のタイムゾーンでのオブジェクトの振る舞いについて
心配する必要がありません。


.. method:: tzinfo.fromutc(self, dt)

   デフォルトの :class:`datetime.astimezone()` 実装で呼び出されます。
   :class:`datetime.astimezone()` から呼ばれた場合、 ``dt.tzinfo`` は
   *self* であり、 *dt* の日付および時刻データメンバは UTC 時刻を表し
   ているものとして見えます。 :meth:`fromutc` の目的は、 *self* のロー
   カル時刻に等しい :class:`datetime` オブジェクトを返すことにより日付
   と時刻データメンバを修正することにあります。

   ほとんどの :class:`tzinfo` サブクラスではデフォルトの :meth:`fromutc`
   実装を問題なく継承できます。デフォルトの実装は、固定オフセットのタ
   イムゾーンや、標準時と夏時間の両方について記述しているタイムゾーン、
   そして DST 移行時刻が年によって異なる場合でさえ、扱えるくらい強力な
   ものです。デフォルトの :meth:`fromutc` 実装が全ての場合に対して正し
   く扱うことができないような例は、標準時の (UTCからの) オフセットが引
   数として渡された特定の日や時刻に依存するもので、これは政治的な理由
   によって起きることがあります。デフォルトの :meth:`astimezone` や
   :meth:`fromutc` の実装は、結果が標準時オフセットの変化にまたがる何
   時間かの中にある場合、期待通りの結果を生成しないかもしれません。

   エラーの場合のためのコードを除き、デフォルトの :meth:`fromutc` の
   実装は以下のように動作します::

      def fromutc(self, dt):
          # raise ValueError error if dt.tzinfo is not self
          dtoff = dt.utcoffset()
          dtdst = dt.dst()
          # raise ValueError if dtoff is None or dtdst is None
          delta = dtoff - dtdst  # this is self's standard offset
          if delta:
              dt += delta   # convert to standard local time
              dtdst = dt.dst()
              # raise ValueError if dtdst is None
          if dtdst:
              return dt + dtdst
          else:
              return dt

以下に :class:`tzinfo` クラスの使用例を示します:


.. literalinclude:: ../includes/tzinfo-examples.py

標準時間 (standard time) および夏時間 (daylight time) の両方を記述して
いる :class:`tzinfo` のサブクラスでは、回避不能の難解な問題が年に 2 度
あるので注意してください。具体的な例として、東部アメリカ時刻 (US
Eastern, UTC -5000) を考えます。 EDT は 3 月の第二日曜日の 1:59
(EST) 以後に開始し、11 月の最初の日曜日の 1:59 (EDT) に終了します::

     UTC   3:MM  4:MM  5:MM  6:MM  7:MM  8:MM
     EST  22:MM 23:MM  0:MM  1:MM  2:MM  3:MM
     EDT  23:MM  0:MM  1:MM  2:MM  3:MM  4:MM

   start  22:MM 23:MM  0:MM  1:MM  3:MM  4:MM

     end  23:MM  0:MM  1:MM  1:MM  2:MM  3:MM

DST の開始の際 ("start" の並び) ローカルの壁時計は 1:59 から 3:00 に飛
びます。この日は 2:MM の形式をとる時刻は実際には無意味となります。従っ
て、 ``astimezone(Eastern)`` は DST が開始する日には ``hour == 2`` と
なる結果を返すことはありません。
:meth:`astimezone` がこのことを保証するようにするには、
:meth:`tzinfo.dst` メソッドは "失われた時間" (東部時刻における 2:MM)
が夏時間に存在することを考えなければなりません。

DST が終了する際 ("end" の並び) では、問題はさらに悪化します: 1 時間の
間、ローカルの壁時計ではっきりと時刻をいえなくなります:
それは夏時間の最後の 1 時間です。東部時刻では、その日の UTC での 5:MM
に夏時間は終了します。ローカルの壁時計は 1:59 (夏時間) から 1:00 (標準
時) に再び巻き戻されます。ローカルの時刻における 1:MM はあいまいにな
ります。 :meth:`astimezone` は二つの UTC 時刻を同じローカルの時刻に対
応付けることでローカルの時計の振る舞いをまねます。東部時刻の例では、
5:MM および 6:MM の形式をとる UTC 時刻は両方とも、東部時刻に変換された
際に 1:MM に対応づけられます。 :meth:`astimezone` がこのことを保証する
ようにするには、 :meth:`tzinfo.dst` は "繰り返された時間" が標準時に存
在することを考慮しなければなりません。このことは、例えばタイムゾーンの
標準のローカルな時刻に DST への切り替え時刻を表現することで簡単に設定
することができます。

このようなあいまいさを許容できないアプリケーションは、ハイブリッドな
:class:`tzinfo` サブクラスを使って問題を回避しなければなりません;
UTC や、他のオフセットが固定された :class:`tzinfo` のサブクラス (EST
(-5 時間の固定オフセット) のみを表すクラスや、 EDT (-4 時間の固定オフ
セット) のみを表すクラス) を使う限り、あいまいさは発生しません。


.. _strftime-strptime-behavior:

:meth:`strftime` と :meth:`strptime` の振る舞い
-----------------------------------------------

:class:`date`, :class:`datetime`,および :class:`time` オブジェクト
は全て、明示的な書式化文字列でコントロールして時刻表現文字列を生成する
ための ``strftime(format)`` メソッドをサポートしています。大雑把にいう
と、 ``d.strftime(fmt)`` は :mod:`time` モジュールの
``time.strftime(fmt, d.timetuple())`` のように動作します。ただし全ての
オブジェクトが :meth:`timetuple` メソッドをサポートしているわけではあ
りません。

逆に :meth:`datetime.strptime` クラスメソッドは日付や時刻に対応する
フォーマット文字列から :class:`datetime` オブジェクトを生成します。
``datetime.strptime(date_string, format)`` は
``datetime(*(time.strptime(date_string, format)[0:6]))`` と等価です。

:class:`time` オブジェクトでは、年、月、日の値がないため、それらの書式
化コードを使うことができません。無理矢理使った場合、年は ``1900`` に置
き換えられ、月と日は ``1`` に置き換えられます。

:class:`date` オブジェクトでは、時、分、秒、マイクロ秒の値がないため、
それらの書式化コードを使うことができません。無理矢理使った場合、これら
の値は ``0`` に置き換えられます。

.. versionadded:: 2.6
   :class:`time` および、 :class:`datetime` オブジェクトは、6桁まで0埋め
   されるマイクロ秒まで拡大された ``%f`` 書式をサポートします。

naive オブジェクトでは、書式化コード ``%z`` および ``%Z``  は空文字列
に置き換えられます。

aware オブジェクトでは以下のようになります:

``%z``
   :meth:`utcoffset` は +HHMM あるいは -HHMM の形式をもった 5 文字の文
   字列に変換されます。HH は UTC オフセット時間を与える 2 桁の文字列で、
   MM は UTC オフセット分を与える 2 桁の文字列です。例えば、
   :meth:`utcoffset` が ``timedelta(hours=-3, minutes=-30)`` を返した
   場合、 ``%z`` は文字列 ``'-0330'`` に置き換わります。

``%Z``
   :meth:`tzname` が ``None`` を返した場合、 ``%Z`` は空文字列に置き換
   わります。そうでない場合、 ``%Z`` は返された値に置き換わりますが、
   これは文字列でなければなりません。

Python はプラットフォームの C ライブラリから :func:`strftime`
関数を呼び出し、プラットフォーム間のバリエーションはよくあることなので、
サポートされている書式化コードの全セットはプラットフォーム間で異なりま
す。

以下のリストはC標準(1989年版)が要求する全ての書式化コードで、標準C実装\
があれば全ての環境で動作します。
1999 年版の C 標準では書式化コードが追加されているので注意してください。

:meth:`strftime` が正しく動作する年の厳密な範囲はプラットフォーム間で
異なります。プラットフォームに関わらず、1900 年以前の年は使うことがで
きません。

+-----------+---------------------------------------+-------+
| 指定子    | 意味                                  | 備考  |
+===========+=======================================+=======+
| ``%a``    | ロケールの短縮された曜日名            |       |
|           | を表示します                          |       |
+-----------+---------------------------------------+-------+
| ``%A``    | ロケールの曜日名を表示します          |       |
+-----------+---------------------------------------+-------+
| ``%b``    | ロケールの短縮された月名を            |       |
|           | 表示します                            |       |
+-----------+---------------------------------------+-------+
| ``%B``    | ロケールの月名を表示します            |       |
+-----------+---------------------------------------+-------+
| ``%c``    | ロケールの日時を適切な形式            |       |
|           | で表示します                          |       |
+-----------+---------------------------------------+-------+
| ``%d``    | 月中の日にちを10進表記した            |       |
|           | 文字列 [01,31] を表示します           |       |
+-----------+---------------------------------------+-------+
| ``%f``    | マイクロ秒を10進表記した文字列        | \(1)  |
|           | [000000,999999] を表示します          |       |
|           | (左側から0埋めされます)               |       |
+-----------+---------------------------------------+-------+
| ``%H``    | 時 (24時間表記) を10進表記した        |       |
|           | 文字列 [00,23] を表示します           |       |
+-----------+---------------------------------------+-------+
| ``%I``    | 時 (12時間表記) を10進表記した        |       |
|           | 文字列 [01,12] を表示します           |       |
+-----------+---------------------------------------+-------+
| ``%j``    | 年中の日にちを10進表記した            |       |
|           | 文字列 [001,366] を表示します         |       |
+-----------+---------------------------------------+-------+
| ``%m``    | 月を10進表記した文字列                |       |
|           | [01,12] を表示します                  |       |
+-----------+---------------------------------------+-------+
| ``%M``    | 分を10進表記した文字列                |       |
|           | [00,59] を表示します                  |       |
+-----------+---------------------------------------+-------+
| ``%p``    | ロケールの AM もしくは PM             | \(2)  |
|           | を表示します                          |       |
+-----------+---------------------------------------+-------+
| ``%S``    | 秒を10進表記した文字列                | \(3)  |
|           | [00,61] を表示します                  |       |
+-----------+---------------------------------------+-------+
| ``%U``    | 年中の週番号                          | \(4)  |
|           | (週の始まりは日曜日とする)            |       |
|           | を10進表記した文字列 [00,53]          |       |
|           | を表示します                          |       |
|           | 新年の最初の日曜日に先立つ日は        |       |
|           | 0週に属するとします                   |       |
|           |                                       |       |
+-----------+---------------------------------------+-------+
| ``%w``    | 曜日を10進表記した文字列              |       |
|           | [0(日曜日),6] を表示します            |       |
+-----------+---------------------------------------+-------+
| ``%W``    | 年中の週番号                          | \(4)  |
|           | (週の始まりは月曜日とする)            |       |
|           | を10進表記した文字列                  |       |
|           | [00,53] を表示します                  |       |
|           | 新年の最初の月曜日に先立つ日は        |       |
|           | 0週に属するとします                   |       |
|           |                                       |       |
+-----------+---------------------------------------+-------+
| ``%x``    | ロケールの日付を適切な形式で          |       |
|           | 表示します                            |       |
+-----------+---------------------------------------+-------+
| ``%X``    | ロケールの時間を適切な形式で          |       |
|           | 表示します                            |       |
+-----------+---------------------------------------+-------+
| ``%y``    | 世紀なしの年(下2桁)を10進表記         |       |
|           | した文字列 [00,99] を表示します       |       |
+-----------+---------------------------------------+-------+
| ``%Y``    | 世紀ありの年を10進表記した            |       |
|           | 文字列を表示します                    |       |
+-----------+---------------------------------------+-------+
| ``%z``    | UTCオフセットを +HHMM もしくは        | \(5)  |
|           | -HHMM の形式で表示します              |       |
|           | (オブジェクトがnaiveであれば空文字列) |       |
+-----------+---------------------------------------+-------+
| ``%Z``    | タイムゾーンの名前を表示します        |       |
|           | (オブジェクトがnaiveであれば空文字列) |       |
+-----------+---------------------------------------+-------+
| ``%%``    | 文字 ``'%'`` を表示します             |       |
+-----------+---------------------------------------+-------+

Notes:

(1)
   :meth:`strptime` メソッドと共に使われた場合、 ``%f`` 指定子は 1 桁から
   6 桁の数字を受け付け、右側から 0 埋めされます。
   ``%f`` は C 標準規格の書式セットに拡張されます。

(2)
   :meth:`strptime` メソッドと共に使われた場合、 ``%p`` 指定子は出力の時間
   フィールドのみに影響し、 ``%I`` 指定子が使われたかのように振る舞い
   ます。

(3)
   範囲は ``0`` から ``61`` で正しいです; これはうるう秒と、 (極めて稀
   ですが) 2秒のうるう秒を考慮してのことです。

(4)
   :meth:`strptime` メソッドと共に使われた場合、 ``%U`` と ``%W`` 指定子は、
   年と曜日が指定された場合の計算でのみ使われます。

(5)
   例えば、 :meth:`utcoffset` が ``timedelta(hours=-3, minutes=-30)``
   を返すとしたら、 ``%z`` は文字列、 ``'-0330'`` で置き換えられます。
