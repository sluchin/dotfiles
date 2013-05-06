
:mod:`textwrap` --- テキストの折り返しと詰め込み
================================================

.. module:: textwrap
   :synopsis: テキストの折り返しと詰め込み
.. moduleauthor:: Greg Ward <gward@python.net>
.. sectionauthor:: Greg Ward <gward@python.net>


.. versionadded:: 2.3

:mod:`textwrap` モジュールでは、二つの簡易関数 :func:`wrap` と
:func:`fill` 、そして作業のすべてを行うクラス :class:`TextWrapper` とユーティリティ関数 :func:`dedent`
を提供しています。単に一つや二つのテキスト文字列の折り返しまたは詰め込みを行っているならば、簡易関数で十分間に合います。そうでなければ、
効率のために :class:`TextWrapper` のインスタンスを使った方が良いでしょう。

.. seealso::

   最新バージョンの `textwrap モジュールの Python ソースコード
   <http://svn.python.org/view/python/branches/release27-maint/Lib/textwrap.py?view=markup>`_

.. function:: wrap(text[, width[, ...]])

   *text* (文字列)内の段落を一つだけ折り返しを行います。したがって、すべての行が高々 *width* 文字の長さになります。最後に改行が付かない出力行のリストを返します。

   オプションのキーワード引数は、以下で説明する :class:`TextWrapper` のインスタンス属性に対応しています。 *width* はデフォルトで ``70`` です。


.. function:: fill(text[, width[, ...]])

   *text* 内の段落を一つだけ折り返しを行い、折り返しが行われた段落を含む一つの文字列を返します。 :func:`fill` は  ::

      "\n".join(wrap(text, ...))

   の省略表現です。

   特に、 :func:`fill` は :func:`wrap` とまったく同じ名前のキーワード引数を受け取ります。

:func:`wrap` と :func:`fill` の両方ともが :class:`TextWrapper` インスタンスを作成し、その一つのメソッドを呼び出すことで機能します。
そのインスタンスは再利用されません。
したがって、たくさんのテキスト文字列を折り返し/詰め込みを行うアプリケーションのためには、あなた自身の :class:`TextWrapper` オブジェクトを作成することでさらに効率が良くなるでしょう。

.. Text is preferably wrapped on whitespaces and right after the hyphens in
   hyphenated words; only then will long words be broken if necessary, unless
   :attr:`TextWrapper.break_long_words` is set to false.

テキストはなるべく空白か、ハイフンを含む語のハイフンの直後で折り返されます。
:attr:`TextWrapper.break_long_words` が偽に設定されていなければ、必要な場合に長い語が分解されます。

追加のユーティリティ関数である :func:`dedent` は、不要な空白をテキストの左側に持つ文字列からインデントを取り去ります。


.. function:: dedent(text)

   *text* の各行に対し、共通して現れる先頭の空白を削除します。

   この関数は通常、三重引用符で囲われた文字列をスクリーン/その他の左端にそろえ、なおかつソースコード中ではインデントされた形式を
   損なわないようにするために使われます。

   タブとスペースはともにホワイトスペースとして扱われますが、同じではないことに注意してください:  ``"  hello"`` という行と
   ``"\thello"`` 　は、同じ先頭の空白文字をもっていないとみなされます。(このふるまいは Python 2.5で導入されました。古いバージョ
   ンではこのモジュールは不正にタブを展開して共通の先頭空白文字列を探していました）

   以下に例を示します::

      def test():
          # end first line with \ to avoid the empty line!
          s = '''\
          hello
            world
          '''
          print repr(s)          # prints '    hello\n      world\n    '
          print repr(dedent(s))  # prints 'hello\n  world\n'


.. class:: TextWrapper(...)

   :class:`TextWrapper` コンストラクタはたくさんのオプションのキーワード引数を受け取ります。それぞれの引数は一つのインスタンス属性に対応します。したがって、例えば、
   ::

      wrapper = TextWrapper(initial_indent="* ")

   は  ::

      wrapper = TextWrapper()
      wrapper.initial_indent = "* "

   と同じです。

   あなたは同じ :class:`TextWrapper` オブジェクトを何回も再利用できます。また、使用中にインスタンス属性へ代入することでそのオプションのどれでも変更できます。

   :class:`TextWrapper` インスタンス属性(とコンストラクタのキーワード引数)は以下の通りです:


   .. attribute:: width

      (デフォルト: ``70``)
      折り返しが行われる行の最大の長さ。
      入力行に :attr:`width` より長い単一の語が無い限り、 :class:`TextWrapper` は
      :attr:`width` 文字より長い出力行が無いことを保証します。


   .. attribute:: expand_tabs

      (デフォルト: ``True``)
      もし真ならば、そのときは *text* 内のすべてのタブ文字は *text* の :meth:`expand_tabs`
      メソッドを用いて空白に展開されます。


   .. attribute:: replace_whitespace

      (デフォルト: ``True``)
      もし真ならば、タブ展開の後に残る(``string.whitespace``
      に定義された)空白文字のそれぞれが一つの空白と置き換えられます。

      .. note::

         :attr:`expand_tabs` が偽で :attr:`replace_whitespace` が真ならば、\
         各タブ文字は1つの空白に置き換えられます。それはタブ展開と同じでは *ありません* 。

      .. note::

         :attr:`replace_whitespace` が偽の場合、改行が行の途中で現れることで
         出力がおかしくなることがあります。
         このため、テキストを(:meth:`str.splitlines` などを使って)段落ごとに
         分けて別々に wrap する必要があります。

   .. attribute:: drop_whitespace

      .. (default: ``True``) If true, whitespace that, after wrapping, happens to
         end up at the beginning or end of a line is dropped (leading whitespace in
         the first line is always preserved, though).

      (デフォルト: ``True``) 真の場合、ラップ後に行末や行頭にあるスペースが削除されます。
      (最初の行の先頭の空白は残ります)

      .. versionadded:: 2.6
         過去のバージョンでは、空白は常に削除されていました。

         .. Whitespace was always dropped in earlier versions.


   .. attribute:: initial_indent

      (デフォルト: ``''``)
      折り返しが行われる出力の一行目の先頭に付けられる文字列。
      一行目の折り返しまでの長さにカウントされます。


   .. attribute:: subsequent_indent

      (デフォルト: ``''``)
      一行目以外の折り返しが行われる出力のすべての行の先頭に付けられる文字列。
      一行目以外の各行の折り返しまでの長さにカウントされます。


   .. attribute:: TextWrapper.fix_sentence_endings

      (デフォルト: ``False``)
      もし真ならば、 :class:`TextWrapper` は文の終わりを見つけようとし、確実に\
      文がちょうど二つの空白で常に区切られているようにします。
      これは一般的に固定スペースフォントのテキストに対して望ましいです。
      しかし、文の検出アルゴリズムは完全ではありません:
      文の終わりには、後ろに空白がある ``'.'``, ``'!'`` または ``'?'``
      の中の一つ、ことによると ``'"'`` あるいは ``'''`` が付随する小文字が\
      あると仮定しています。これに伴う一つの問題は ::

         [...] Dr. Frankenstein's monster [...]

      の"Dr."と ::

         [...] See Spot. See Spot run [...]

      の"Spot."の間の差異を検出できないアルゴリズムです。

      :attr:`fix_sentence_endings` はデフォルトで偽です。

      文検出アルゴリズムは"小文字"の定義のために ``string.lowercase`` に依存し、\
      同一行の文を区切るためにピリオドの後に二つの空白を使う慣習に依存しているため、\
      英文テキストに限定されたものです。


   .. attribute:: break_long_words

      (デフォルト: ``True``)
      もし真ならば、そのとき :attr:`width` より長い行が確実にないようにするために、
      :attr:`width` より長い語は切られます。
      偽ならば、長い語は切られないでしょう。そして、 :attr:`width` より長い行があるかもしれません。
      (:attr:`width` を超える分を最小にするために、長い語は単独で一行に置かれるでしょう。)

   .. attribute:: break_on_hyphens

      .. (default: ``True``) If true, wrapping will occur preferably on whitespaces
         and right after hyphens in compound words, as it is customary in English.
         If false, only whitespaces will be considered as potentially good places
         for line breaks, but you need to set :attr:`break_long_words` to false if
         you want truly insecable words.  Default behaviour in previous versions
         was to always allow breaking hyphenated words.

      (デフォルト: ``True``)
      真の場合、英語で一般的なように、ラップ処理は空白か合成語に含まれるハイフンの直後で行われます。
      偽の場合、空白だけが改行に適した位置として判断されます。ただし、本当に語の途中で\
      改行が行われないようにするためには、 :attr:`break_long_words` 属性を真に設定する必要があります。
      過去のバージョンでのデフォルトの振る舞いは、常にハイフンの直後での改行を許していました。

      .. versionadded:: 2.6


   :class:`TextWrapper` はモジュールレベルの簡易関数に類似した二つの公開メソッドも提供します:

   .. method:: wrap(text)

      *text* (文字列)内の段落を一つだけ折り返しを行います。したがって、すべての行は高々
      :attr:`width` 文字です。すべてのラッピングオプションは :class:`TextWrapper`
      インスタンスのインスタンス属性から取られています。最後に改行の無い出力された行のリストを返します。


   .. method:: TextWrapper.fill(text)

      *text* 内の段落を一つだけ折り返しを行い、折り返しが行われた段落を含む一つの文字列を返します。

