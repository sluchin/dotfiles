
:mod:`mmap` --- メモリマップファイル
====================================

.. module:: mmap
   :synopsis: Unix と Windows のメモリマップファイルへのインターフェース


メモリにマップされたファイルオブジェクトは、文字列とファイルオブジェクトの両方のように振舞います。
しかし通常の文字列オブジェクトとは異なり、これらは可変です。
文字列が期待されるほとんどの場所で mmap オブジェクトを利用できます。
例えば、メモリマップファイルを探索するために :mod:`re`
モジュールを使うことができます。
それらは可変なので、 ``obj[index] = 'a'``
のように文字を変換できますし、スライスを使うことで  ``obj[i1:i2] = '...'``
のように部分文字列を変換することができます。
現在のファイル位置をデータの始めとする読込みや書込み、
ファイルの異なる位置へ :meth:`seek` することもできます。

メモリマップファイルは Unix 上と Windows 上とでは異なる
:class:`mmap` コンストラクタによって作られます。
いずれの場合も、開いたファイルのファイル記述子を、更新のために提供しなければなりません。
すでに存在する Python ファイルオブジェクトをマップしたい場合は、
*fileno* パラメータに渡す値を手に入れるために、 :meth:`fileno`
メソッドを使用して下さい。
そうでなければ、ファイル記述子を直接返す :func:`os.open`
関数 (呼び出すときにはまだファイルが閉じている必要があります) を使って、
ファイルを開くことができます。

Unix バージョンと Windows バージョンのどちらのコンストラクタについても、
オプションのキーワード・パラメータとして *access* を指定することになるかもしれません。
*access* は3つの値の内の1つを受け入れます。
:const:`ACCESS_READ` は読み込み専用、 :const:`ACCESS_WRITE` は書き込み可能、
:const:`ACCESS_COPY` はコピーした上での書き込みです。
*access* は Unix と Windows の両方で使用することができます。
*access* が指定されない場合、 Windows の mmap は書き込み可能マップを返します。
3つのアクセス型すべてに対する初期メモリ値は、指定されたファイルから得られます。
:const:`ACCESS_READ` 型のメモリマップに対して書き込むと
:exc:`TypeError` 例外を送出します。
:const:`ACCESS_WRITE` 型のメモリマップへの書き込みはメモリと元のファイルの両方に影響を与えます。
:const:`ACCESS_COPY` 型のメモリマップへの書き込みはメモリに影響を与えますが、元のファイルを更新することはありません。

.. versionchanged:: 2.5
   無名メモリ(anonymous memory)にマップするためには fileno として -1
   を渡し、length を与えてください。

.. versionchanged:: 2.6
   mmap.mmap はこれまで mmap オブジェクトを生成するファクトリ関数でした。
   これからは mmap.mmap がクラスそのものになります。

.. class:: mmap(fileno, length[, tagname[, access[, offset]]])

   **(Windows バージョン)** ファイルハンドル *fileno*
   によって指定されたファイルから *length* バイトをマップして、
   mmap オブジェクトを生成します。
   *length* が現在のファイルサイズより大きな場合、
   ファイルサイズは *length* を含む大きさにまで拡張されます。
   *length* が ``0`` の場合、マップの最大の長さは現在のファイルサイズになります。
   ただし、ファイル自体が空のときは Windows が例外を送出します
   (Windows では空のマップを作成することができません)。

   *tagname* は、 ``None`` 以外で指定された場合、
   マップのタグ名を与える文字列となります。
   Windows は同じファイルに対する様々なマップを持つことを可能にします。
   既存のタグの名前を指定すればそのタグがオープンされ、
   そうでなければこの名前の新しいタグが作成されます。
   もしこのパラメータを省略したり ``None`` を与えたりしたならば、
   マップは名前なしで作成されます。
   タグ・パラメータの使用の回避は、あなたのコードを Unix と Windows
   の間で移植可能にしておくのを助けてくれるでしょう。

   *offset* は非負整数のオフセットとして指定できます。
   mmap の参照はファイルの先頭からのオフセットに相対的になります。
   *offset* のデフォルトは 0 です。
   *offset* は ALLOCATIONGRANULARITY の倍数でなければなりません。

.. class:: mmap(fileno, length[, flags[, prot[, access[, offset]]]])
   :noindex:

   **(Unix バージョン)** ファイル記述子 *fileno*
   によって指定されたファイルから *length* バイトをマップし、
   mmap オブジェクトを返します。
   *length* が ``0`` の場合、マップの最大長は
   :class:`mmap` が呼び出された時点のファイルサイズになります。

   *flags* はマップの種類を指定します。 :const:`MAP_PRIVATE`
   はプライベートな copy-on-write(書込み時コピー)のマップを作成します。
   従って、mmap オブジェクトの内容への変更はこのプロセス内にのみ有効です。
   :const:`MAP_SHARED` はファイルの同じ領域をマップする他のすべてのプロセス\
   と共有されたマップを作成します。デフォルトは :const:`MAP_SHARED` です。

   *prot* が指定された場合、希望のメモリ保護を与えます。
   2つの最も有用な値は、 :const:`PROT_READ` と :const:`PROT_WRITE` です。
   これは、読込み可能または書込み可能を指定するものです。
   *prot* のデフォルトは :const:`PROT_READ \| PROT_WRITE` です。

   *access* はオプションのキーワード・パラメータとして、
   *flags* と *prot* の代わりに指定してもかまいません。
   *flags*, *prot* と *access* の両方を指定することは間違っています。
   このパラメーターを使用法についての情報は、
   先に述べた *access* の記述を参照してください。

   *offset* は非負整数のオフセットとして指定できます。
   mmap の参照はファイルの先頭からのオフセットに相対的になります。
   *offset* のデフォルトは 0 です。
   *offset* は PAGESIZE または ALLOCATIONGRANULARITY の倍数でなければなりません。

   Mac OS X と OpenVMS において、作成された memory mapping の正当性を確実にするために
   *fileno* で指定されたファイルディスクリプタは内部で自動的に物理的な
   ストレージ (physical backing store) と同期されます。

   以下の例は :class:`mmap` の簡単な使い方です::

      import mmap

      # write a simple example file
      with open("hello.txt", "wb") as f:
          f.write("Hello Python!\n")

      with open("hello.txt", "r+b") as f:
          # memory-map the file, size 0 means whole file
          map = mmap.mmap(f.fileno(), 0)
          # read content via standard file methods
          print map.readline()  # prints "Hello Python!"
          # read content via slice notation
          print map[:5]  # prints "Hello"
          # update content using slice notation;
          # note that new content must have same size
          map[6:] = " world!\n"
          # ... and read again using standard file methods
          map.seek(0)
          print map.readline()  # prints "Hello  world!"
          # close the map
          map.close()


   次の例では無名マップを作り親プロセスと子プロセスの間でデータのやりとりをしてみせます::

      import mmap
      import os

      map = mmap.mmap(-1, 13)
      map.write("Hello world!")

      pid = os.fork()

      if pid == 0: # In a child process
          map.seek(0)
          print map.readline()

          map.close()


   メモリマップファイルオブジェクトは以下のメソッドをサポートしています:


   .. method:: close()

      ファイルを閉じます。この呼出しの後にオブジェクトの他のメソッドの呼出すことは、
      例外の送出を引き起こすでしょう。


   .. method:: find(string[, start[, end]])

      オブジェクト内の [*start*, *end*] の範囲に含まれている部分文字列
      *string* が見つかった場所の最も小さいインデックスを返します。
      オプションの引数 *start* と *end* はスライスに使われるときのように解釈されます。
      失敗したときには ``-1`` を返します。


   .. method:: flush([offset, size])

      ファイルのメモリコピー内での変更をディスクへフラッシュします。
      この呼出しを使わなかった場合、オブジェクトが破壊される前に変更が書き込まれる保証はありません。
      もし *offset* と *size* が指定された場合、与えられたバイトの範囲の変更だけがディスクにフラッシュされます。
      指定されない場合、マップ全体がフラッシュされます。

      **(Windows バージョン)** ゼロ以外の値が返されたら成功を、ゼロは失敗を意味します。

      **(Unix バージョン)** ゼロの値が返されたら成功を意味します。
      呼び出しが失敗すると例外が送出されます。

   .. method:: move(dest, src, count)

      オフセット *src* から始まる *count* バイトをインデックス *dest*
      の位置へコピーします。もし mmap が :const:`ACCESS_READ` で作成されていた場合、
      :exc:`TypeError` 例外を発生させます。


   .. method:: read(num)

      現在のファイル位置から最大で *num* バイト分の文字列を返します。
      ファイル位置は返したバイトの分だけ後ろの位置へ更新されます。


   .. method:: read_byte()

      現在のファイル位置から長さ1の文字列を返します。ファイル位置は1だけ進みます。


   .. method:: readline()

      現在のファイル位置から次の改行までの、1行を返します。


   .. method:: resize(newsize)

      マップと元ファイル(がもしあれば)のサイズを変更します。
      もし mmap が :const:`ACCESS_READ` または :const:`ACCESS_COPY`
      で作成されたならば、マップサイズの変更は :exc:`TypeError` 例外を発生させます。


   .. method:: rfind(string[, start[, end]])

      オブジェクト内の [*start*, *end*] の範囲に含まれている部分文字列
      *string* が見つかった場所の最も大きいインデックスを返します。
      オプションの引数 *start* と *end* はスライスに使われるときのように解釈されます。
      失敗したときには ``-1`` を返します。

   .. method:: seek(pos[, whence])

      ファイルの現在位置をセットします。 *whence* 引数はオプションであり、
      デフォルトは ``os.SEEK_SET`` つまり ``0`` (絶対位置)です。
      その他の値として、
      ``os.SEEK_CUR`` つまり ``1`` (現在位置からの相対位置)と
      ``os.SEEK_END`` つまり ``2`` (ファイルの終わりからの相対位置)があります。


   .. method:: size()

      ファイルの長さを返します。メモリマップ領域のサイズより大きいかもしれません。


   .. method:: tell()

      ファイル・ポインタの現在位置を返します。


   .. method:: write(string)

      メモリ内のファイル・ポインタの現在位置に *string* のバイト列を書き込みます。
      ファイル位置はバイト列が書き込まれた後の位置へ更新されます。
      もし mmap が :const:`ACCESS_READ` で作成されていた場合、
      書き込み時に :exc:`TypeError` 例外を発生させるでしょう。


   .. method:: write_byte(byte)

      メモリ内のファイル・ポインタの現在位置に単一文字の文字列 *byte* を書き込みます。
      ファイル位置は ``1`` だけ進みます。
      もし mmap が :const:`ACCESS_READ` で作成されていた場合、
      書き込み時に :exc:`TypeError` 例外を発生させるでしょう。
