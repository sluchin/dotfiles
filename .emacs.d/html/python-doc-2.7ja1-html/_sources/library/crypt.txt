
:mod:`crypt` --- Unix パスワードをチェックするための関数
========================================================

.. module:: crypt
   :platform: Unix
   :synopsis: Unix パスワードをチェックするための関数 crypt()。
.. moduleauthor:: Steven D. Majewski <sdm7g@virginia.edu>
.. sectionauthor:: Steven D. Majewski <sdm7g@virginia.edu>
.. sectionauthor:: Peter Funk <pf@artcom-gmbh.de>


.. index::
   pair: cipher; DES
   single: crypt(3)

このモジュールは DES アルゴリズムに基づいた一方向ハッシュ関数である
:manpage:`crypt(3)`  ルーチンを実装しています。
詳細については Unix マニュアルページを参照してください。
このモジュールは、Python スクリプトがユーザから入力されたパスワードを\
受理できるようにしたり、
Unix パスワードに (脆弱性検査のための) 辞書攻撃を試みるのに使えます。

.. index:: single: crypt(3)

このモジュールは実行環境の :manpage:`crypt(3)` の実装に依存しています。
そのため、現在の実装で利用可能な拡張を、このモジュールでもそのまま利用できます。


.. function:: crypt(word, salt)

   *word* は通常はユーザのパスワードで、プロンプトやグラフィカル\
   インタフェースからタイプ入力されます。
   *salt* は通常ランダムな 2 文字からなる文字列で、DES アルゴリズムに
   4096 通りのうち 1 つの方法で外乱を与えるために使われます。
   *salt* に使う文字は集合 ``[./a-zA-Z0-9]`` の要素でなければなりません。
   ハッシュされたパスワードを文字列として返します。パスワード文字列は *salt*
   と同じ文字集合に含まれる文字からなります (最初の 2 文字は *salt* 自体です).

   .. index:: single: crypt(3)

   いくつかの拡張された :manpage:`crypt(3)` は異なる値と *salt*
   の長さを許しているので、パスワードをチェックする際には crypt
   されたパスワード文字列全体を *salt* として渡すよう勧めます。

典型的な使用例のサンプルコード::

   import crypt, getpass, pwd

   def login():
       username = raw_input('Python login:')
       cryptedpasswd = pwd.getpwnam(username)[1]
       if cryptedpasswd:
           if cryptedpasswd == 'x' or cryptedpasswd == '*':
               raise NotImplementedError(
                   "Sorry, currently no support for shadow passwords")
           cleartext = getpass.getpass()
           return crypt.crypt(cleartext, cryptedpasswd) == cryptedpasswd
       else:
           return 1

