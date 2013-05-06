
:mod:`difflib` --- 差分の計算を助ける
=====================================

.. module:: difflib
   :synopsis: オブジェクト同士の違いを計算する
.. moduleauthor:: Tim Peters <tim_one@users.sourceforge.net>
.. sectionauthor:: Tim Peters <tim_one@users.sourceforge.net>
.. Markup by Fred L. Drake, Jr. <fdrake@acm.org>

.. testsetup::

   import sys
   from difflib import *

.. versionadded:: 2.1

このモジュールは、シーケンスを比較するためのクラスや関数を提供しています。
例えば、ファイルの差分を計算して、それを HTML や context diff, unified diff
などいろいろなフォーマットで出力するために、このモジュールを利用することができます。
ディレクトリやファイル群を比較するためには、 :mod:`filecmp` モジュールも参照してください。


.. class:: SequenceMatcher

   柔軟性のあるクラスで、二つのシーケンスの要素は、
   ハッシュ化できる(:term:`hashable`)型である限り何でも比較可能です。
   基本的なアルゴリズムは、1980年代の後半に発表された
   Ratcliff と Obershelp による"ゲシュタルトパターンマッチング"と大げさに名づけられた
   アルゴリズム以前から知られている、やや凝ったアルゴリズムです。
   その考え方は、"junk" 要素を含まない最も長い互いに隣接したマッチ列を探すことです
   (Ratcliff と Obershelp のアルゴリズムでは junk を示しません)。
   このアイデアは、マッチ列から左または右に伸びる断片に対して再帰的に
   あてはめられます。この方法では編集を最小にする列は取り出されませんが、
   人間の目からみて「正しい感じ」にマッチする傾向があります。

   **実行時間:** 基本的な Ratcliff-Obershelp アルゴリズムは、
   最悪の場合3乗、期待値で2乗となります。
   :class:`SequenceMatcher` オブジェクトでは、最悪のケースで2乗、期待値は比較される
   シーケンス中に共通に現れる要素数に非常にややこしく依存しています。
   最良の場合は線形時間になります。

   **自動 junk ヒューリスティック:** :class:`SequenceMatcher` は、シーケンスの
   特定の要素を自動的に junk として扱うヒューリスティックをサポートしています。
   このヒューリスティックは、各個要素がシーケンス内に何回現れるかを数えます。
   ある要素の重複数が (最初のものは除いて) 合計でシーケンスの 1% 以上になり、
   そのシーケンスが 200 要素以上なら、その要素は "popular" であるものとして
   マークされ、シーケンスのマッチングの目的からは junk として扱われます。
   このヒューリスティックは、 :class:`SequenceMatcher` の作成時に ``autojunk``
   パラメタを ``False`` に設定することで無効化できます。

   .. versionadded:: 2.7.1
      *autojunk* パラメタ。

.. class:: Differ

   テキスト行からなるシーケンスを比較するクラスです。
   人が読むことのできる差分を作成します。
   Differ クラスは :class:`SequenceMatcher`
   クラスを利用して、行からなるシーケンスを比較したり、(ほぼ)同一の行内の文字を比較したりします。

   :class:`Differ` クラスによる差分の各行は、2文字のコードで始まります。

   +----------+------------------------------------------+
   | コード   | 意味                                     |
   +==========+==========================================+
   | ``'- '`` | 行はシーケンス1にのみ存在する            |
   +----------+------------------------------------------+
   | ``'+ '`` | 行はシーケンス2にのみ存在する            |
   +----------+------------------------------------------+
   | ``'  '`` | 行は両方のシーケンスで同一               |
   +----------+------------------------------------------+
   | ``'? '`` | 行は入力シーケンスのどちらにも存在しない |
   +----------+------------------------------------------+

   '? 'で始まる行は、行内のどこに差異が存在するかに注意を向けようとします。
   その行は、入力されたシーケンスのどちらにも存在しません。シーケンスが
   タブ文字を含むとき、これらの行は判別しづらいものになることがあります。


.. class:: HtmlDiff

   このクラスは、二つのテキストを左右に並べて比較表示し、行間あるいは行内の変更点を強調表示するような HTML テーブル (またはテーブルの入った完全な
   HTML ファイル) を生成するために使います。テーブルは完全差分モード、コンテキスト差分モードのいずれでも生成できます。

   このクラスのコンストラクタは以下のようになっています:


   .. function:: __init__([tabsize][, wrapcolumn][, linejunk][, charjunk])

      :class:`HtmlDiff` のインスタンスを初期化します。

      *tabsize* はオプションのキーワード引数で、タブストップ幅を指定します。デフォルトは ``8`` です。

      *wrapcolumn* はオプションのキーワード引数で、テキストを折り返すカラム幅を指定します。デフォルトは ``None`` で折り返しを行いません。

      *linejunk* および *charjunk* はオプションのキーワード引数で、 ``ndiff()`` (:class:`HtmlDiff`
      はこの関数を使って左右のテキストの差分を HTML で生成します) に渡されます。それぞれの引数のデフォルト値および説明は ``ndiff()``
      のドキュメントを参照してください。

   以下のメソッドが public になっています:


   .. function:: make_file(fromlines, tolines [, fromdesc][, todesc][, context][, numlines])

      *fromlines* と *tolines* (いずれも文字列のリスト) を比較し、行間または行内の変更点が強調表示された行差分の入った表を持つ完全な
      HTML  ファイルを文字列で返します。

      *fromdesc* および *todesc* はオプションのキーワード引数で、差分表示テーブルにおけるそれぞれ差分元、差分先ファイルのカラムの
      ヘッダになる文字列を指定します (いずれもデフォルト値は空文字列です)。

      *context* および *numlines* はともにオプションのキーワード引数です。
      *context* を ``True`` にするとコンテキスト差分を表示し、
      デフォルトの ``False`` にすると完全なファイル差分を表示します。
      *numlines* のデフォルト値は ``5`` で、
      *context* が ``True`` の場合、
      *numlines* は強調部分の前後にあるコンテキスト行の数を制御します。
      *context* が ``False`` の場合、
      *numlines* は "next" と書かれたハイパーリンクをたどった時に到達する場所が
      次の変更部分より何行前にあるかを制御します
      (値をゼロにした場合、"next" ハイパーリンクを辿ると変更部分の強調表示が
      ブラウザの最上部に表示されるようになります)。


   .. function:: make_table(fromlines, tolines [, fromdesc][, todesc][, context][, numlines])

      *fromlines* と *tolines* (いずれも文字列のリスト) を比較し、行間または行内の変更点が強調表示された行差分の入った完全な HTML
      テーブルを文字列で返します。

      このメソッドの引数は、 :meth:`make_file` メソッドの引数と同じです。

   :file:`Tools/scripts/diff.py` はこのクラスへのコマンドラインフロントエンドで、使い方を学ぶ上で格好の例題が入っています。

   .. versionadded:: 2.4


.. function:: context_diff(a, b[, fromfile][, tofile][, fromfiledate][, tofiledate][, n][, lineterm])

   *a* と *b* (文字列のリスト) を比較し、差分
   (差分形式の行を生成するジェネレータ(:term:`generator`)) を、
   context diff のフォーマット(以下「コンテクスト形式」)で返します。

   コンテクスト形式は、変更があった行に前後数行を加えてある、コンパクトな表現方法です。
   変更箇所は、変更前/変更後に分けて表します。コンテクスト (変更箇所前後の行)
   の行数は *n* で指定し、デフォルト値は 3 です。

   デフォルトでは、diff の制御行 (``***`` や ``---`` を含む行) の最後には、
   改行文字が付加されます。この場合、入出力とも、行末に改行文字を持つので、
   :func:`file.readlines` で得た入力から生成した差分を、
   :func:`file.writelines` に渡す場合に便利です。

   行末に改行文字を持たない入力に対しては、出力でも改行文字を付加しないように
   *lineterm* 引数に ``""`` を渡してください。

   コンテクスト形式は、通常、ヘッダにファイル名と変更時刻を持っています。この情報は、文字列
   *fromfile*, *tofile*, *fromfiledate*, *tofiledate* で指定できます。
   変更時刻の書式は、通常、ISO 8601 フォーマットで表されます。
   指定しなかった場合のデフォルト値は、空文字列です。

      >>> s1 = ['bacon\n', 'eggs\n', 'ham\n', 'guido\n']
      >>> s2 = ['python\n', 'eggy\n', 'hamster\n', 'guido\n']
      >>> for line in context_diff(s1, s2, fromfile='before.py', tofile='after.py'):
      ...     sys.stdout.write(line)  # doctest: +NORMALIZE_WHITESPACE
      *** before.py
      --- after.py
      ***************
      *** 1,4 ****
      ! bacon
      ! eggs
      ! ham
        guido
      --- 1,4 ----
      ! python
      ! eggy
      ! hamster
        guido

   より詳細な例は、 :ref:`difflib-interface` を参照してください。



   .. versionadded:: 2.3


.. function:: get_close_matches(word, possibilities[, n][, cutoff])

   「十分」なマッチの上位のリストを返します。
   *word* はマッチさせたいシーケンス (大概は文字列) です。
   *possibilities* は *word* にマッチさせるシーケンスのリスト (大概は文字列のリスト) です。

   オプションの引数 *n* (デフォルトでは ``3``)はメソッドの返すマッチの最大数です。
   *n* は ``0`` より大きくなければなりません。

   オプションの引数 *cutoff*  (デフォルトでは ``0.6``)は、 
   区間 [0, 1] に入る小数の値です。
   *word* との一致率がそれ未満の *possibilities* の要素は無視されます。

   *possibilities* の要素でマッチした上位(多くても *n* 個)は、
   類似度のスコアに応じて(一番似たものを先頭に)ソートされたリストとして返されます。

      >>> get_close_matches('appel', ['ape', 'apple', 'peach', 'puppy'])
      ['apple', 'ape']
      >>> import keyword
      >>> get_close_matches('wheel', keyword.kwlist)
      ['while']
      >>> get_close_matches('apple', keyword.kwlist)
      []
      >>> get_close_matches('accept', keyword.kwlist)
      ['except']


.. function:: ndiff(a, b[, linejunk[, charjunk]])

   *a* と *b* (文字列のリスト) を比較し、差分
   (差分形式の行を生成するジェネレータ(:term:`generator`)) を、
   :class:`Differ` のスタイルで返します。

   オプションのキーワードパラメータ *linejunk* と *charjunk* は、
   フィルタ関数を渡します (使わないときは ``None``)。

   *linejunk*: 文字列型の引数ひとつを受け取る関数で、文字列が junk ならば真を、
   違うときには偽を返します。
   Python 2.3 以降、デフォルトでは(``None``)になります。
   それまでは、モジュールレべルの関数 :func:`IS_LINE_JUNK` であり、それは
   高々ひとつのシャープ記号(``'#'``)を除いて可視のキャラクタを含まない行
   をフィルタリングするものです。
   Python 2.3 から、下位にある :class:`SequenceMatcher` クラスが、
   雑音となるくらい頻繁に登場する行であるか否かを、動的に分析します。
   これは、バージョン 2.3 以前のデフォルト値よりたいていうまく動作します。

   *charjunk*: 文字(長さ1の文字列)を受け取る関数です。
   デフォルトでは、モジュールレべルの関数 :func:`IS_CHARACTER_JUNK` であり、
   これは空白文字類 (空白またはタブ、注：改行文字をこれに含めるのは悪いアイデア！)
   をフィルタリングします。

   :file:`Tools/scripts/ndiff.py` は、この関数のコマンドラインのフロントエンド（インターフェイス）です。

      >>> diff = ndiff('one\ntwo\nthree\n'.splitlines(1),
      ...              'ore\ntree\nemu\n'.splitlines(1))
      >>> print ''.join(diff),
      - one
      ?  ^
      + ore
      ?  ^
      - two
      - three
      ?  -
      + tree
      + emu


.. function:: restore(sequence, which)

   差分を生成した元の二つのシーケンスのうち一つを返します。

   :meth:`Differ.compare` または :meth:`ndiff` によって生成された *sequence* 
   を与えられると、行頭のプレフィクスを取りのぞいて
   ファイル 1 または 2 (引数 *which* で指定される) に由来する行を復元します。

   例:

      >>> diff = ndiff('one\ntwo\nthree\n'.splitlines(1),
      ...              'ore\ntree\nemu\n'.splitlines(1))
      >>> diff = list(diff) # materialize the generated delta into a list
      >>> print ''.join(restore(diff, 1)),
      one
      two
      three
      >>> print ''.join(restore(diff, 2)),
      ore
      tree
      emu


.. function:: unified_diff(a, b[, fromfile][, tofile][, fromfiledate][, tofiledate][, n][, lineterm])

   *a* と *b* (文字列のリスト) を比較し、差分
   (差分形式の行を生成するジェネレータ(:term:`generator`)) を、
   unified diff フォーマット(以下「ユニファイド形式」)で返します。

   ユニファイド形式は変更があった行に前後数行を加えた、コンパクトな表現方法です。
   変更箇所は (変更前/変更後を分離したブロックではなく) インライン・スタイルで表されます。
   コンテクスト（変更箇所前後の行）の行数は、 *n* で指定し、デフォルト値は 3 です。

   デフォルトでは、diff の制御行 (``---``, ``+++``, ``@@`` を含む行)
   は行末の改行を含めて生成されます。
   このようにしてあると、入出力とも行末に改行文字を持つので、
   :func:`file.readlines` で得た入力を処理して生成した差分を、
   :func:`file.writelines` に渡す場合に便利です。

   行末に改行文字を持たない入力には、出力も同じように改行なしになるように、
   *lineterm* 引数を ``""`` にセットしてください

   ユニファイド形式は、通常、ヘッダにファイル名と変更時刻を持っています。
   この情報は、文字列 *fromfile*, *tofile*, *fromfiledate*, *tofiledate*
   で指定できます。変更時刻の書式は、通常、ISO 8601 フォーマットで表されます。
   指定しなかった場合のデフォルト値は、空文字列です。

      >>> s1 = ['bacon\n', 'eggs\n', 'ham\n', 'guido\n']
      >>> s2 = ['python\n', 'eggy\n', 'hamster\n', 'guido\n']
      >>> for line in unified_diff(s1, s2, fromfile='before.py', tofile='after.py'):
      ...     sys.stdout.write(line)   # doctest: +NORMALIZE_WHITESPACE
      --- before.py
      +++ after.py
      @@ -1,4 +1,4 @@
      -bacon
      -eggs
      -ham
      +python
      +eggy
      +hamster
       guido

   もっと詳細な例は、 :ref:`difflib-interface` を参照してください。

   .. versionadded:: 2.3


.. function:: IS_LINE_JUNK(line)

   無視できる行のとき真を返します。
   行 *line* は空白、または  ``'#'`` ひとつのときに無視できます。
   それ以外のときには無視できません。
   Python 2.3 以前は :func:`ndiff` の引数 *linkjunk* にデフォルトで使用されました。


.. function:: IS_CHARACTER_JUNK(ch)

   無視できる文字のとき真を返します。
   文字 *ch* が空白、またはタブ文字のときには無視できます。
   それ以外の時には無視できません。
   :func:`ndiff` の引数 *charjunk* としてデフォルトで使用されます。


.. seealso::

   `Pattern Matching: The Gestalt Approach （パターンマッチング: 全体アプローチ） <http://www.ddj.com/184407970?pgno=5>`_
      John W. Ratcliff と  D. E. Metzener による類似のアルゴリズムに関する議論。
      `Dr. Dobb's Journal
      <http://www.ddj.com/>`_  1988年7月号掲載。


.. _sequence-matcher:

SequenceMatcherオブジェクト
---------------------------

:class:`SequenceMatcher` クラスには、以下のようなコンストラクタがあります。


.. class:: SequenceMatcher([isjunk[, a[, b[, autojunk=True]]]])

   オプションの引数 *isjunk* は、 ``None`` (デフォルトの値です) にするか、
   単一の引数をとる関数にせねばなりません。後者の場合、関数は
   シーケンスの要素を受け取り、要素が junk であり、無視すべきである場合に限り
   真をかえすようにせねばなりません。 *isjunk* に ``None`` を渡すと、
   ``lambda x: 0`` を渡したのと同じになります; すなわち、いかなる要素も無視しなくなります。
   例えば以下のような引数を渡すと、空白とタブ文字を無視して文字のシーケンスを比較します。 ::

      lambda x: x in " \t"

   オプションの引数 *a* と *b* は、比較される文字列で、デフォルトでは空の文字列です。
   両方のシーケンスの要素は、ハッシュ化可能(:term:`hashable`)である必要があります。

   オプションの引数 *autojunk* は、自動 junk ヒューリスティックを
   無効にするために使えます。

   .. versionadded:: 2.7.1
      *autojunk* パラメタ。

   :class:`SequenceMatcher` オブジェクトは以下のメソッドを持ちます。


   .. method:: set_seqs(a, b)

      比較される2つの文字列を設定します。

   :class:`SequenceMatcher` オブジェクトは、2つ目のシーケンスについての詳細な情報を
   計算し、キャッシュします。
   1つのシーケンスをいくつものシーケンスと比較する場合、まず :meth:`set_seq2`
   を使って文字列を設定しておき、別の文字列を1つずつ比較するために、繰り返し :meth:`set_seq1` を呼び出します。


   .. method:: set_seq1(a)

      比較を行う1つ目のシーケンスを設定します。比較される2つ目のシーケンスは変更されません。


   .. method:: set_seq2(b)

      比較を行う2つ目のシーケンスを設定します。比較される1つ目のシーケンスは変更されません。


   .. method:: find_longest_match(alo, ahi, blo, bhi)

      ``a[alo:ahi]`` と ``b[blo: bhi]`` の中から、最長のマッチ列を探します。

      *isjunk* が省略されたか ``None`` の時、 :meth:`get_longest_match` は
      ``a[i:i+k]`` が ``b[j:j+k]`` と等しいような ``(i, j, k)`` を返します。
      その値は ``alo <= i <= i+k <=  ahi`` かつ ``blo <= j <= j+k <=  bhi``
      となります。 ``(i', j', k')`` でも、同じようになります。
      さらに ``k >= k', i <= i'`` が ``i == i', j <= j'``
      でも同様です。言い換えると、いくつものマッチ列すべてのうち、
      *a* 内で最初に始まるものを返します。そしてその *a* 内で最初のマッチ列すべてのうち
      *b* 内で最初に始まるものを返します。

         >>> s = SequenceMatcher(None, " abcd", "abcd abcd")
         >>> s.find_longest_match(0, 5, 0, 9)
         Match(a=0, b=4, size=5)

      引数 *isjunk* が与えられている場合、上記の通り、はじめに最長のマッチ列を判定します。ブロック内に junk 要素が見当たらないような
      追加条件の際はこれに該当しません。次にそのマッチ列を、その両側の junk 要素にマッチするよう、できる限り広げていきます。そのため結果
      となる列は、探している列のたまたま直前にあった同一の junk 以外の junk にはマッチしません。

      以下は前と同じサンプルですが、空白を junk とみなしています。これは ``' abcd'`` が2つ目の列の末尾にある ``' abcd'`` にマッチしない
      ようにしています。代わりに ``'abcd'`` にはマッチします。そして 2つ目の文字列中、一番左の ``'abcd'`` にマッチします。

         >>> s = SequenceMatcher(lambda x: x==" ", " abcd", "abcd abcd")
         >>> s.find_longest_match(0, 5, 0, 9)
         Match(1, 0, 4)

      どんな列にもマッチしない時は、 ``(alo, blo, 0)`` を返します。

      .. versionchanged:: 2.6
         このメソッドは、名前付きタプル(:term:`named tuple`)で ``Match(a, b, size)`` を返すようになりました。

   .. method:: get_matching_blocks()

      マッチしたシーケンス中で個別にマッチしたシーケンスをあらわす、 3つの値のリストを返します。それぞれの値は
      ``(i, j, n)`` という形式であらわされ、 ``a[i:i+n] == b[j:j+n]`` という関係を意味します。
      3つの値は *i* と *j* の間で単調に増加します。

      最後のタプルはダミーで、 ``(len(a), len(b), 0)`` という値を持ちます。これは ``n==0`` である唯一のタプルです。

      もし ``(i, j, n)`` と ``(i', j', n')`` がリストで並んでいる3つ組で、 2つ目が最後の3つ組でなければ、 ``i+n != i'``
      または ``j+n != j'`` です。言い換えると並んでいる3つ組は常に隣接していない同じブロックを表しています。

      .. XXX Explain why a dummy is used!

      .. versionchanged:: 2.5
         隣接する3つ組は常に隣接しないブロックを表すと保証するようになりました.

      .. doctest::

         >>> s = SequenceMatcher(None, "abxcd", "abcd")
         >>> s.get_matching_blocks()
         [Match(a=0, b=0, size=2), Match(a=3, b=2, size=2), Match(a=5, b=4, size=0)]

   .. method:: get_opcodes()

      *a* を *b* にするための方法を記述する5つのタプルを返します。
      それぞれのタプルは ``(tag, i1, i2, j1, j2)`` という形式であらわされます。
      最初のタプルは ``i1 == j1 == 0`` であり、 
      *i1* はその前にあるタプルの *i2* と同じ値です。
      同様に *j1* は前の *j2* と同じ値になります。

      *tag* の値は文字列であり、次のような意味です。

      +---------------+-----------------------------------------------------------+
      | 値            | 意味                                                      |
      +===============+===========================================================+
      | ``'replace'`` | ``a[i1:i2]`` は ``b[ j1:j2]`` に置き換えられる            |
      +---------------+-----------------------------------------------------------+
      | ``'delete'``  | ``a[i1:i2]`` は削除される。この時、 ``j1 == j2`` である   |
      +---------------+-----------------------------------------------------------+
      | ``'insert'``  | ``b[j1:j2]`` が ``a [i1:i1]`` に挿入される。この時        |
      |               | ``i1 == i2`` である。                                     |
      +---------------+-----------------------------------------------------------+
      | ``'equal'``   | ``a[i1:i2] == b[j1:j2]`` (この部分シーケンスは同値)       |
      +---------------+-----------------------------------------------------------+

      例)

         >>> a = "qabxcd"
         >>> b = "abycdf"
         >>> s = SequenceMatcher(None, a, b)
         >>> for tag, i1, i2, j1, j2 in s.get_opcodes():
         ...    print ("%7s a[%d:%d] (%s) b[%d:%d] (%s)" %
         ...           (tag, i1, i2, a[i1:i2], j1, j2, b[j1:j2]))
          delete a[0:1] (q) b[0:0] ()
           equal a[1:3] (ab) b[0:2] (ab)
         replace a[3:4] (x) b[2:3] (y)
           equal a[4:6] (cd) b[3:5] (cd)
          insert a[6:6] () b[5:6] (f)

   .. method:: get_grouped_opcodes([n])

      最大 *n* 行までのコンテクストを含むグループを生成するような、ジェネレータ(:term:`generator`)を返します。

      このメソッドは、 :meth:`get_opcodes` で返されるグループの中から、似たような差異のかたまりに分け、間に挟まっている変更の無い部分を省きます。

      グループは :meth:`get_opcodes` と同じ書式で返されます。

      .. versionadded:: 2.3


   .. method:: ratio()

      [0, 1] の範囲の浮動小数点数で、シーケンスの類似度を測る値を返します。

      T が2つのシーケンスの要素数の総計だと仮定し、M をマッチした数とすると、
      この値は 2.0\*M / T であらわされます。もしシーケンスがまったく
      同じ場合、値は ``1.0`` となり、まったく異なる場合には ``0.0`` となります。

      このメソッドは :meth:`get_matching_blocks` または :meth:`get_opcodes`
      がまだ呼び出されていない場合には非常にコストが高いです。
      この場合、上限を素早く計算するために、 :meth:`quick_ratio` もしくは
      :meth:`real_quick_ratio` を最初に試してみる方がいいかもしれません。

   .. method:: quick_ratio()

      :meth:`ratio` の上界を、より高速に計算します。

   .. method:: real_quick_ratio()

      :meth:`ratio` の上界を、非常に高速に計算します。

この文字列全体のマッチ率を返す3つのメソッドは、精度の異なる近似値を返します。
:meth:`quick_ratio` と :meth:`real_quick_ratio` は、常に :meth:`ratio`
以上の値を返します。

   >>> s = SequenceMatcher(None, "abcd", "bcde")
   >>> s.ratio()
   0.75
   >>> s.quick_ratio()
   0.75
   >>> s.real_quick_ratio()
   1.0


.. _sequencematcher-examples:

SequenceMatcher の例
--------------------

この例は2つの文字列を比較します。空白を junk とします。

   >>> s = SequenceMatcher(lambda x: x == " ",
   ...                     "private Thread currentThread;",
   ...                     "private volatile Thread currentThread;")

:meth:`ratio` は、[0, 1] の範囲の値を返し、シーケンスの類似度を測ります。
経験によると、 :meth:`ratio`
の値が0.6を超えると、シーケンスがよく似ていることを示します。

   >>> print round(s.ratio(), 3)
   0.866

シーケンスのどこがマッチしているかにだけ興味のある時には  :meth:`get_matching_blocks` が手軽でしょう。

   >>> for block in s.get_matching_blocks():
   ...     print "a[%d] and b[%d] match for %d elements" % block
   a[0] and b[0] match for 8 elements
   a[8] and b[17] match for 21 elements
   a[29] and b[38] match for 0 elements

:meth:`get_matching_blocks` が返す最後のタプルが常にダミーであることに注目してください。
このダミーは ``(len(a), len(b), 0)``  であり、これはタプルの最後の要素（マッチする要素の数）がゼロとなる唯一のケースです。

はじめのシーケンスがどのようにして2番目のものになるのかを知るには、 :meth:`get_opcodes` を使います。

   >>> for opcode in s.get_opcodes():
   ...     print "%6s a[%d:%d] b[%d:%d]" % opcode
    equal a[0:8] b[0:8]
   insert a[8:8] b[8:17]
    equal a[8:29] b[17:38]

.. seealso::

   * :class:`SequenceMatcher` を使った、シンプルで使えるコードを知るには、
     このモジュールの関数 :func:`get_close_matches` を参照してください。

   * `Simple version control recipe
     <http://code.activestate.com/recipes/576729/>`_ 
     :class:`SequenceMatcher` で作った小規模アプリケーション。

.. _differ-objects:

Differ オブジェクト
-------------------

:class:`Differ` オブジェクトによって生成された差分が **最小** であるなどとは言いません。
むしろ、最小の差分はしばしば直観に反しています。
その理由は、どこでもできるとなれば一致を見いだしてしまうからで、
ときには思いがけなく100ページも離れたマッチになってしまうのです。
一致点を互いに隣接したマッチに制限することで、
場合によって長めの差分を出力するというコストを掛けることにはなっても、
ある種の局所性を保つことができるのです。

:class:`Differ` は、以下のようなコンストラクタを持ちます。


.. class:: Differ([linejunk[, charjunk]])

   オプションのパラメータ *linejunk* と *charjunk* はフィルタ関数の指定用です
   (もしくは ``None`` を指定)。

   *linejunk*: ひとつの文字列引数を受け取る関数です。
   文字列が junk のときに真を返します。
   デフォルトでは、 ``None`` であり、どんな行であっても junk とは見なされません。

   *charjunk*: この関数は文字(長さ1の文字列)を引数として受け取り、文字が
   junk であるときに真を返します。
   デフォルトは ``None`` であり、どんな文字も junk とは見なされません。

   :class:`Differ` オブジェクトは、以下の1つのメソッドを通して利用されます。（差分を生成します）。


   .. method:: compare(a, b)

       文字列からなる2つのシーケンスを比較し、差分（を表す文字列からなるシーケンス）を生成します。

       ..   Each sequence must contain individual single-line strings ending with newlines.
       ..   Such sequences can be obtained from the :meth:`readlines` method of file-like
       ..   objects.  The delta generated also consists of newline-terminated strings, ready
       ..   to be printed as-is via the :meth:`writelines` method of a file-like object.

       それぞれのシーケンスは、改行文字によって終了する、独立したひと連なりの文字列でなければなりません。そのようなシーケンスは、ファイル形式オブジェクトの
       :meth:`readlines` メソッドによって得ることができます。（得られる）差分は
       改行文字で終了する文字列として得られ、ファイル形式オブジェクトの 
       :meth:`writelines` メソッドによって出力できる形になっています。


.. _differ-examples:

Differ の例
-----------

この例では2つのテキストを比較します。
初めに、改行文字で終了する独立した1行の連続した
(ファイル形式オブジェクトの :meth:`readlines` メソッドによって得られるような)
テキストを用意します。

   >>> text1 = '''  1. Beautiful is better than ugly.
   ...   2. Explicit is better than implicit.
   ...   3. Simple is better than complex.
   ...   4. Complex is better than complicated.
   ... '''.splitlines(1)
   >>> len(text1)
   4
   >>> text1[0][-1]
   '\n'
   >>> text2 = '''  1. Beautiful is better than ugly.
   ...   3.   Simple is better than complex.
   ...   4. Complicated is better than complex.
   ...   5. Flat is better than nested.
   ... '''.splitlines(1)

次に Differ オブジェクトをインスタンス化します。

   >>> d = Differ()

注意： :class:`Differ` オブジェクトをインスタンス化するとき、
行 junk と文字 junk をフィルタリングする関数を渡すことができます。
詳細は :meth:`Differ` コンストラクタを参照してください。

最後に、2つを比較します。

   >>> result = list(d.compare(text1, text2))

``result`` は文字列のリストなので、pretty-printしてみましょう。

   >>> from pprint import pprint
   >>> pprint(result)
   ['    1. Beautiful is better than ugly.\n',
    '-   2. Explicit is better than implicit.\n',
    '-   3. Simple is better than complex.\n',
    '+   3.   Simple is better than complex.\n',
    '?     ++\n',
    '-   4. Complex is better than complicated.\n',
    '?            ^                     ---- ^\n',
    '+   4. Complicated is better than complex.\n',
    '?           ++++ ^                      ^\n',
    '+   5. Flat is better than nested.\n']

これは、複数行の文字列として、次のように出力されます。

   >>> import sys
   >>> sys.stdout.writelines(result)
       1. Beautiful is better than ugly.
   -   2. Explicit is better than implicit.
   -   3. Simple is better than complex.
   +   3.   Simple is better than complex.
   ?     ++
   -   4. Complex is better than complicated.
   ?            ^                     ---- ^
   +   4. Complicated is better than complex.
   ?           ++++ ^                      ^
   +   5. Flat is better than nested.


.. A command-line interface to difflib

.. _difflib-interface:

difflib のコマンドラインインタフェース
---------------------------------------

.. This example shows how to use difflib to create a ``diff``-like utility.
.. It is also contained in the Python source distribution, as
.. :file:`Tools/scripts/diff.py`.

この例は、 difflib を使って ``diff`` に似たユーティリティーを作成する方法を示します。
これは、 Python のソース配布物にも、 :file:`Tools/scripts/diff.py` として含まれています。

.. testcode::

   """ Command line interface to difflib.py providing diffs in four formats:

   * ndiff:    lists every line and highlights interline changes.
   * context:  highlights clusters of changes in a before/after format.
   * unified:  highlights clusters of changes in an inline format.
   * html:     generates side by side comparison with change highlights.

   """

   import sys, os, time, difflib, optparse

   def main():
        # Configure the option parser
       usage = "usage: %prog [options] fromfile tofile"
       parser = optparse.OptionParser(usage)
       parser.add_option("-c", action="store_true", default=False,
                         help='Produce a context format diff (default)')
       parser.add_option("-u", action="store_true", default=False,
                         help='Produce a unified format diff')
       hlp = 'Produce HTML side by side diff (can use -c and -l in conjunction)'
       parser.add_option("-m", action="store_true", default=False, help=hlp)
       parser.add_option("-n", action="store_true", default=False,
                         help='Produce a ndiff format diff')
       parser.add_option("-l", "--lines", type="int", default=3,
                         help='Set number of context lines (default 3)')
       (options, args) = parser.parse_args()

       if len(args) == 0:
           parser.print_help()
           sys.exit(1)
       if len(args) != 2:
           parser.error("need to specify both a fromfile and tofile")

       n = options.lines
       fromfile, tofile = args # as specified in the usage string

       # we're passing these as arguments to the diff function
       fromdate = time.ctime(os.stat(fromfile).st_mtime)
       todate = time.ctime(os.stat(tofile).st_mtime)
       fromlines = open(fromfile, 'U').readlines()
       tolines = open(tofile, 'U').readlines()

       if options.u:
           diff = difflib.unified_diff(fromlines, tolines, fromfile, tofile,
                                       fromdate, todate, n=n)
       elif options.n:
           diff = difflib.ndiff(fromlines, tolines)
       elif options.m:
           diff = difflib.HtmlDiff().make_file(fromlines, tolines, fromfile,
                                               tofile, context=options.c,
                                               numlines=n)
       else:
           diff = difflib.context_diff(fromlines, tolines, fromfile, tofile,
                                       fromdate, todate, n=n)

       # we're using writelines because diff is a generator
       sys.stdout.writelines(diff)

   if __name__ == '__main__':
       main()
