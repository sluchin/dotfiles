
:mod:`calendar` --- 一般的なカレンダーに関する関数群
====================================================

.. module:: calendar
   :synopsis: Unix の cal プログラム相当の機能を含んだカレンダーに関する関数群
.. sectionauthor:: Drew Csillag <drew_csillag@geocities.com>


このモジュールは Unix の :program:`cal`
プログラムのようなカレンダー出力を行い、
それに加えてカレンダーに関する有益な関数群を提供します。
標準ではこれらのカレンダーは（ヨーロッパの慣例に従って）月曜日を週の始まりとし、
日曜日を最後の日としています。
:func:`setfirstweekday` を用いることで、
日曜日(6)や他の曜日を週の始まりに設定することができます。
日付を表す引数は整数値で与えます。
関連する機能として、 :mod:`datetime` と :mod:`time` モジュールも参照してください。

このモジュールで提供する関数とクラスのほとんどは :mod:`datetime` に依存しており、
過去も未来も現代のグレゴリオ暦を利用します。
この方式は Dershowitz と Reingold の書籍「Calendrical Calculations」にある
proleptic Gregorian 暦に一致しており、同書では全ての計算の基礎となる暦としています。
[#]_

.. seealso::

   最新バージョンの `calendar module Python source code
   <http://svn.python.org/view/python/branches/release27-maint/Lib/calendar.py?view=markup>`_

.. class:: Calendar([firstweekday])

   :class:`Calendar` オブジェクトを作ります。
   *firstweekday* は整数で週の始まりの曜日を指定するものです。
   ``0`` が月曜(デフォルト)、 ``6`` なら日曜です。

   :class:`Calendar` オブジェクトは整形されるカレンダーのデータを準備するために使える
   いくつかのメソッドを提供しています。しかし整形機能そのものは提供していません。
   それはサブクラスの仕事なのです。

   .. versionadded:: 2.5

   :class:`Calendar` インスタンスには以下のメソッドがあります。


.. method:: iterweekdays(weekday)

      曜日の数字を一週間分生成するイテレータを返します。
      イテレータから得られる最初の数字は :meth:`firstweekday`
      が返す数字と同じになります。

      .. % firstweekday は属性
      .. % getfirstweekday() の謂いか


.. method:: itermonthdates(year, month)

      *year* 年 *month* 月に対するイテレータを返します。
      このイテレータはその月の全ての日(:class:`datetime.date`
      オブジェクトとして) およびその前後の日で週に欠けが無いようにするのに必要な日を返します。


.. method:: itermonthdays2(year, month)

      *year* 年 *month* 月に対する :meth:`itermonthdates` と同じような
      イテレータを返します。生成されるのは日付の数字と曜日を表す数字のタプルです。


.. method:: itermonthdays(year, month)

      *year* 年 *month* 月に対する :meth:`itermonthdates` と同じようなイテレータを返します。
      生成されるのは日付の数字だけです。


.. method:: monthdatescalendar(year, month)

      *year* 年 *month* 月の週のリストを返します。
      週は全て七つの :class:`datetime.date` オブジェクトからなるリストです。


.. method:: monthdays2calendar(year, month)

      *year* 年 *month* 月の週のリストを返します。
      週は全て七つの日付の数字と曜日を表す数字のタプルからなるリストです。


.. method:: monthdayscalendar(year, month)

      *year* 年 *month* 月の週のリストを返します。
      週は全て七つの日付の数字からなるリストです。


.. method:: yeardatescalendar(year[, width])

      指定された年のデータを整形に向く形で返します。
      返される値は月の並びのリストです。
      月の並びは最大で *width* ヶ月(デフォルトは3ヶ月)分です。
      各月は4ないし6週からなり、各週は1ないし7日からなります。
      各日は :class:`datetime.date` オブジェクトです。


.. method:: yeardays2calendar(year[, width])

      指定された年のデータを整形に向く形で返します
      (:meth:`yeardatescalendar` と同様です)。
      週のリストの中が日付の数字と曜日の数字のタプルになります。
      月の範囲外の部分の日付はゼロです。


.. method:: yeardayscalendar(year[, width])

      指定された年のデータを整形に向く形で返します
      (:meth:`yeardatescalendar` と同様です)。
      週のリストの中が日付の数字になります。
      月の範囲外の日付はゼロです。


.. class:: TextCalendar([firstweekday])

   このクラスはプレインテキストのカレンダーを生成するのに使えます。

   .. versionadded:: 2.5

   :class:`TextCalendar` インスタンスには以下のメソッドがあります。


   .. method:: formatmonth(theyear, themonth[, w[, l]])

      ひと月分のカレンダーを複数行の文字列で返します。
      *w* により日の列幅を変えることができ、それらはセンタリングされます。
      *l* により各週の表示される行数を変えることができます。
      :meth:`setfirstweekday` メソッドでセットされた週の最初の曜日に依存します。


   .. method:: prmonth(theyear, themonth[, w[, l]])

      :meth:`formatmonth` で返されるひと月分のカレンダーを出力します。


   .. method:: formatyear(theyear[, w[, l[, c[, m]]]])

      *m* 列からなる一年間のカレンダーを複数行の文字列で返します。
      任意の引数 *w*, *l*, *c* はそれぞれ、日付列の表示幅、各週の行数及び
      月と月の間のスペースの数を変更するためのものです。
      :meth:`setfirstweekday` メソッドでセットされた週の最初の曜日に依存します。
      カレンダーを出力できる最初の年はプラットフォームに依存します。


   .. method:: pryear(theyear[, w[, l[, c[, m]]]])

      :meth:`formatyear` で返される一年間のカレンダーを出力します。


.. class:: HTMLCalendar([firstweekday])

   このクラスは HTML のカレンダーを生成するのに使えます。

   .. versionadded:: 2.5

   :class:`HTMLCalendar` インスタンスには以下のメソッドがあります。


   .. method:: formatmonth(theyear, themonth[, withyear])

      ひと月分のカレンダーを HTML のテーブルとして返します。 *withyear*
      が真であればヘッダには年も含まれます。そうでなければ月の名前だけが使われます。


   .. method:: formatyear(theyear[, width])

      一年分のカレンダーを HTML のテーブルとして返します。
      *width* の値 (デフォルトでは 3 です) は何ヶ月分を一行に収めるかを指定します。


   .. method:: formatyearpage(theyear[, width[, css[, encoding]]])

      一年分のカレンダーを一つの完全な HTML ページとして返します。
      *width* の値(デフォルトでは 3 です) は何ヶ月分を一行に収めるかを指定します。
      *css* は使われるカスケーディングスタイルシートの名前です。
      スタイルシートを使わないようにするために :const:`None` を渡すこともできます。
      *encoding* には出力に使うエンコーディングを指定します (デフォルトではシステムデフォルトのエンコーディングです)。


.. class:: LocaleTextCalendar([firstweekday[, locale]])

   この :class:`TextCalendar` のサブクラスではコンストラクタにロケール名を渡すことができ、
   メソッドの返り値で月や曜日が指定されたロケールのものになります。
   このロケールがエンコーディングを含む場合には、月や曜日の入った文字列はユニコードとして返されます。

   .. versionadded:: 2.5


.. class:: LocaleHTMLCalendar([firstweekday[, locale]])

   この :class:`HTMLCalendar` のサブクラスではコンストラクタにロケール名を渡す
   ことができ、メソッドの返り値で月や曜日が指定されたロケールのものになります。
   このロケールがエンコーディングを含む場合には、月や曜日の入った文字列は
   ユニコードとして返されます。

   .. versionadded:: 2.5

.. note::

   これら2つのクラスの :meth:`formatweekday` と :meth:`formatmonthname` メソッドは、
   一時的に現在の locale を指定された *locale* に変更します。
   現在の locale はプロセス全体に影響するので、これらはスレッドセーフではありません。


単純なテキストのカレンダーに関して、このモジュールには以下のような関数が提供されています。


.. function:: setfirstweekday(weekday)

   週の最初の曜日(``0`` は月曜日, ``6`` は日曜日)を設定します。
   定数 :const:`MONDAY`, :const:`TUESDAY`,
   :const:`WEDNESDAY`, :const:`THURSDAY`, :const:`FRIDAY`,
   :const:`SATURDAY` 及び :const:`SUNDAY` は便宜上提供されています。
   例えば、日曜日を週の開始日に設定するときは::

      import calendar
      calendar.setfirstweekday(calendar.SUNDAY)

   .. versionadded:: 2.0


.. function:: firstweekday()

   現在設定されている週の最初の曜日を返します。

   .. versionadded:: 2.0


.. function:: isleap(year)

   *year* が閏年なら :const:`True` を、そうでなければ :const:`False` を返します。


.. function:: leapdays(y1, y2)

   範囲(*y1* ... *y2*)指定された期間の閏年の回数を返します。
   ここで *y1* や *y2* は年を表します。

   .. versionchanged:: 2.0
      Python 1.5.2では、この関数は世紀をまたがった範囲では動作しません。


.. function:: weekday(year, month, day)

   *year* (``1970``--...), *month* (``1``--``12``), *day* (``1``--``31``)
   で与えられた日の曜日(``0`` は月曜日)を返します。


.. function:: weekheader(n)

   短縮された曜日名を含むヘッダを返します。
   *n* は各曜日を何文字で表すかを指定します。


.. function:: monthrange(year, month)

   *year* と *month* で指定された月の一日の曜日と日数を返します。


.. function:: monthcalendar(year, month)

   月のカレンダーを行列で返します。各行が週を表し、月の範囲外の日は0になります。
   それぞれの週は :func:`setfirstweekday` で設定をしていない限り月曜日から始まります。


.. function:: prmonth(theyear, themonth[, w[, l]])

   :func:`month` 関数によって返される月のカレンダーを出力します。


.. function:: month(theyear, themonth[, w[, l]])

   :class:`TextCalendar` の :meth:`formatmonth` メソッドを利用して、
   ひと月分のカレンダーを複数行の文字列で返します。

   .. versionadded:: 2.0


.. function:: prcal(year[, w[, l[c]]])

   :func:`calendar` 関数で返される一年間のカレンダーを出力します。


.. function:: calendar(year[, w[, l[c]]])

   :class:`TextCalendar` の :meth:`formatyear` メソッドを利用して、
   3列からなる一年間のカレンダーを複数行の文字列で返します。

   .. versionadded:: 2.0


.. function:: timegm(tuple)

   関連はありませんが便利な関数で、 :mod:`time` モジュールの :func:`gmtime`
   関数の戻値のような時間のタプルを受け取り、
   1970年を起点とし、POSIX規格のエンコードによるUnixのタイムスタンプに相当する
   値を返します。実際、 :func:`time.gmtime` と :func:`timegm` は反対の動作をします。

   .. versionadded:: 2.0

:mod:`calendar` モジュールの以下のデータ属性を利用することができます:


.. data:: day_name

   現在のロケールでの曜日を表す配列です。


.. data:: day_abbr

   現在のロケールでの短縮された曜日を表す配列です。


.. data:: month_name

   現在のロケールでの月の名を表す配列です。この配列は通常の約束事に従って、
   1月を数字の 1 で表しますので、長さが 13 ある代わりに
   ``month_name[0]`` が空文字列になります。


.. data:: month_abbr

   現在のロケールでの短縮された月の名を表す配列です。
   この配列は通常の約束事に従って、1月を数字の 1 で表しますので、長さが 13 ある代わりに
   ``month_name[0]`` が空文字列になります。


.. seealso::

   Module :mod:`datetime`
      :mod:`time` モジュールと似た機能を持った日付と時間用のオブジェクト指向インタフェース。

   Module :mod:`time`
      低レベルの時間に関連した関数群。

.. rubric:: 注記

.. [#] 訳注: proleptic Gregorian 暦とはグレゴリオ暦制定(1582年)以前についても\
   グレゴリオ暦で言い表す暦の方式のことで ISO 8601 などでも採用されています
